use std::collections::HashMap;
use std::convert::TryFrom;
use std::f64::consts::TAU;
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::time::Instant;

use anyhow::anyhow;
use clap::{Parser, ValueEnum};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, FromSample, Sample, SampleFormat, SizedSample, Stream, StreamConfig};
use midir::MidiInput;
use thiserror::Error;
use wmidi::MidiMessage;

#[derive(Error, Debug)]
#[error("Requested or default output device not available")]
struct OutputDeviceError;

#[derive(Error, Debug)]
#[error("Requested midi input port not available")]
struct InputPortError;

/// A simple midi synthesizer
#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct Opts {
    #[clap(short, long, default_value_t = 0)]
    input_port: usize,
    #[clap(short, long)]
    output_device: Option<usize>,
    #[clap(short = 'l', long)]
    list_input_ports: bool,
    #[clap(short = 'L', long)]
    list_output_devices: bool,
    #[cfg(debug_assertions)]
    #[clap(short = 'D', long)]
    debug: bool,
    #[clap(flatten)]
    synth_opts: SynthOpts,
}

#[derive(Parser, Debug)]
struct SynthOpts {
    #[clap(short, long, value_enum, default_value_t = Waveform::Tri)]
    waveform: Waveform,
    #[clap(short, long, default_value_t = 0.5)]
    pulse_width: f64,
    #[clap(short, long, default_value_t = 0.05)]
    attack: f64,
    #[clap(short, long, default_value_t = 0.2)]
    decay: f64,
    #[clap(short, long, default_value_t = 0.8)]
    sustain: f64,
    #[clap(short, long, default_value_t = 0.5)]
    release: f64,
    #[clap(short = 'A', long, default_value_t = 0.5)]
    amplitude: f64,
}

#[derive(ValueEnum, Copy, Clone, Debug)]
enum Waveform {
    Sine,
    Square,
    Saw,
    Tri,
    Pulse,
}

impl Into<fn(f64, &WaveProps) -> f64> for Waveform {
    fn into(self) -> fn(f64, &WaveProps) -> f64 {
        match self {
            Waveform::Sine => waveform_sine,
            Waveform::Square => waveform_square,
            Waveform::Saw => waveform_saw,
            Waveform::Tri => waveform_tri,
            Waveform::Pulse => waveform_pulse,
        }
    }
}

fn waveform_sine(t: f64, _: &WaveProps) -> f64 {
    (TAU * t).sin()
}

fn waveform_square(t: f64, _: &WaveProps) -> f64 {
    1. - 2. * (2. * t % 2.).floor()
}

fn waveform_saw(t: f64, _: &WaveProps) -> f64 {
    (t + 0.5) % 1. * 2. - 1.
}

fn waveform_tri(t: f64, _: &WaveProps) -> f64 {
    ((4. * t + 3.) % 4. - 2.).abs() - 1.
}

fn waveform_pulse(t: f64, w: &WaveProps) -> f64 {
    if t % 1. < w.pulse_width { -1. } else { 1. }
}

struct WaveProps {
    waveform: fn(f64, &Self) -> f64,
    pulse_width: f64,
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    amplitude: f64,
}

struct Note {
    frequency: f64,
    velocity: f64,
    phase: f64,
    on_time: f64,
    off_time: Option<f64>,
}

struct MidiChannelState {
    notes: HashMap<wmidi::Note, Note>,
    pitch_bend: f64,
    program: u8,
}

impl Default for MidiChannelState {
    fn default() -> Self {
        MidiChannelState {
            notes: HashMap::new(),
            pitch_bend: 1.,
            program: 0,
        }
    }
}

struct MidiSynth {
    audio_channels: usize,
    sample_rate: usize,
    sample_index: usize,
    start_time: Option<Instant>,
    wave_props: WaveProps,
    receiver: Receiver<(MidiMessage<'static>, Instant)>,
    midi_channel_states: [MidiChannelState; 16],
}

const DELAY: f64 = 0.1;

impl MidiSynth {
    pub fn new(
        opts: SynthOpts,
        config: &StreamConfig,
        receiver: Receiver<(MidiMessage<'static>, Instant)>,
    ) -> MidiSynth {
        MidiSynth {
            audio_channels: config.channels as usize,
            sample_rate: config.sample_rate.0 as usize,
            sample_index: 0,
            start_time: None,
            wave_props: WaveProps {
                waveform: opts.waveform.into(),
                pulse_width: opts.pulse_width,
                attack: opts.attack,
                decay: opts.decay,
                sustain: opts.sustain,
                release: opts.release,
                amplitude: opts.amplitude,
            },
            receiver,
            midi_channel_states: Default::default(),
        }
    }

    pub fn make_stream<T: SizedSample + FromSample<f32>>(
        mut self,
        device: &Device,
        config: &StreamConfig,
    ) -> Result<Stream, cpal::BuildStreamError> {
        device.build_output_stream(
            config,
            move |data, info| self.output_callback::<T>(data, info),
            Self::error_callback,
            None,
        )
    }

    fn error_callback(err: cpal::StreamError) {
        eprintln!("Error: audio output stream: {}", err);
    }

    fn output_callback<T: Sample + FromSample<f32>>(
        &mut self,
        data: &mut [T],
        _info: &cpal::OutputCallbackInfo,
    ) {
        self.process_messages();
        self.flush_notes();

        for frame in data.chunks_exact_mut(self.audio_channels) {
            let y = self.synthesize(self.sample_time()) as f32;
            frame.fill(T::from_sample(y));
            self.next_sample();
        }
    }

    fn sample_time(&self) -> f64 {
        self.sample_index as f64 / self.sample_rate as f64
    }

    fn process_messages(&mut self) {
        let start_time = *self.start_time.get_or_insert_with(Instant::now);

        while let Some((msg, time)) = match self.receiver.try_recv() {
            Err(TryRecvError::Empty) => None,
            result => Some(result.unwrap()),
        } {
            let time = time.saturating_duration_since(start_time).as_secs_f64() + DELAY;
            let channels = &mut self.midi_channel_states;
            match msg {
                MidiMessage::NoteOn(ch, note, velocity) => {
                    channels[ch as usize].notes.insert(
                        note,
                        Note {
                            frequency: note.to_freq_f64(),
                            velocity: u8::from(velocity) as f64 / 127.,
                            phase: 0.,
                            on_time: time,
                            off_time: None,
                        },
                    );
                }
                MidiMessage::NoteOff(ch, note, _) => {
                    // channels[ch as usize].notes.remove(&note);
                    if let Some(note) = channels[ch as usize].notes.get_mut(&note) {
                        note.off_time = Some(time);
                    }
                }
                MidiMessage::PitchBendChange(ch, pitch_bend) => {
                    let pitch_bend: u16 = pitch_bend.into();
                    let pitch_bend_norm = pitch_bend as f64 / 8192.0 - 1.0;
                    let pitch_bend_mult = (pitch_bend_norm / 6.).exp2();
                    channels[ch as usize].pitch_bend = pitch_bend_mult;
                }
                MidiMessage::ProgramChange(ch, prog) => {
                    channels[ch as usize].program = prog.into();
                }
                MidiMessage::ControlChange(_ch, _ctrl, _val) => {}
                _ => {}
            }
        }
    }

    fn flush_notes(&mut self) {
        let t = self.sample_time();
        let release = self.wave_props.release;
        for ch in self.midi_channel_states.iter_mut() {
            ch.notes
                .retain(|_, n| n.off_time.map_or(true, |off_t| t - off_t < release));
        }
    }

    fn synthesize(&self, t: f64) -> f64 {
        let w = &self.wave_props;
        let mut y = 0.;
        for channel_state in self.midi_channel_states.iter() {
            for note in channel_state.notes.values() {
                let amp = note.velocity * w.amplitude;
                let t_on = t - note.on_time;
                let t_off = t - note.off_time.unwrap_or(f64::INFINITY);
                let env = self.envelope(t_on, t_off);
                y += amp * env * (w.waveform)(note.phase, w);
            }
        }
        y
    }

    fn next_sample(&mut self) {
        self.sample_index += 1;
        let delta_t = 1. / self.sample_rate as f64;
        for channel_state in self.midi_channel_states.iter_mut() {
            for note in channel_state.notes.values_mut() {
                let freq = note.frequency * channel_state.pitch_bend;
                note.phase += freq * delta_t;
            }
        }
    }

    fn envelope(&self, t_on: f64, t_off: f64) -> f64 {
        let w = &self.wave_props;
        if t_on < 0.0 {
            0.0
        } else if t_on < w.attack {
            t_on / w.attack
        } else if t_on < w.attack + w.decay {
            1. - (1. - w.sustain) * (t_on - w.attack) / w.decay
        } else if t_off < 0.0 {
            w.sustain
        } else if t_off < w.release {
            w.sustain * (1. - t_off / w.release)
        } else {
            0.0
        }
    }
}

fn wait_for_ctrlc() -> anyhow::Result<()> {
    let (tx, rx) = mpsc::channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })?;
    rx.recv()?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    #[cfg(debug_assertions)]
    let debug = opts.debug;

    if opts.list_output_devices {
        let host = cpal::default_host();
        let def_dev_name = host.default_output_device().and_then(|d| d.name().ok());
        for dev in host.output_devices()? {
            let isdef = Some(dev.name()?) == def_dev_name;
            let c = if isdef { '*' } else { ' ' };
            println!("{} {}", c, dev.name()?);
        }
        return Ok(());
    }

    if opts.list_input_ports {
        let midi_in = MidiInput::new("midi_synth")?;
        for port in midi_in.ports() {
            println!("{}", midi_in.port_name(&port)?);
        }
        return Ok(());
    }

    let host = cpal::default_host();
    let device = match opts.output_device {
        None => host.default_output_device(),
        Some(index) => host.output_devices()?.nth(index),
    }
    .ok_or(OutputDeviceError)?;
    let supported_config = device.default_output_config()?;
    let config = supported_config.config();

    let (sender, receiver) = mpsc::channel();

    let synth = MidiSynth::new(opts.synth_opts, &config, receiver);
    let stream = match supported_config.sample_format() {
        SampleFormat::F32 => synth.make_stream::<f32>(&device, &config)?,
        SampleFormat::I16 => synth.make_stream::<i16>(&device, &config)?,
        SampleFormat::U16 => synth.make_stream::<u16>(&device, &config)?,
        format => Err(anyhow!("Unsupported sample format '{format}'"))?,
    };
    stream.play()?;

    let mut midi_in = MidiInput::new("midi_synth")?;
    midi_in.ignore(midir::Ignore::All);
    let ports = midi_in.ports();
    let port = ports.get(opts.input_port).ok_or(InputPortError)?;
    let input_callback = move |_timestamp: u64, message: &[u8], _: &mut ()| {
        let time = Instant::now();
        let message = MidiMessage::try_from(message).unwrap();
        sender.send((message.to_owned(), time)).unwrap();
        #[cfg(debug_assertions)]
        if debug {
            println!("{} {:?}", _timestamp, message);
        }
    };
    let _midi_conn = midi_in.connect(&port, "midi_synth", input_callback, ());

    println!("Receiving midi messages... Press Ctr+C to exit");
    wait_for_ctrlc()?;
    println!("Exiting");

    Ok(())
}
