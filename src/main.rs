use std::collections::HashMap;
use std::convert::TryFrom;
use std::f64::consts::TAU;
use std::sync::mpsc::{self, Receiver, TryRecvError};
use std::time::Instant;

use clap::arg_enum;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, Sample, SampleFormat, Stream, StreamConfig};
use midir::MidiInput;
use structopt::StructOpt;
use thiserror::Error;
use wmidi::MidiMessage;

#[derive(Error, Debug)]
#[error("Requested or default output device not available")]
struct OutputDeviceError;

#[derive(Error, Debug)]
#[error("Requested midi input port not available")]
struct InputPortError;

#[derive(StructOpt, Debug)]
struct Opts {
    #[structopt(short, long, default_value = "0")]
    input_port: usize,
    #[structopt(short, long)]
    output_device: Option<usize>,
    #[structopt(short = "l", long)]
    list_input_ports: bool,
    #[structopt(short = "L", long)]
    list_output_devices: bool,
    #[cfg(debug_assertions)]
    #[structopt(short = "D", long)]
    debug: bool,
    #[structopt(flatten)]
    synth_opts: SynthOpts,
}

#[derive(StructOpt, Debug)]
struct SynthOpts {
    #[structopt(short, long, possible_values = &Waveform::variants(),
                case_insensitive = true, default_value = "Tri")]
    waveform: Waveform,
    #[structopt(short, long, default_value = "0.5")]
    pulse_width: f64,
    #[structopt(short, long, default_value = "0.05")]
    attack: f64,
    #[structopt(short, long, default_value = "0.2")]
    decay: f64,
    #[structopt(short, long, default_value = "0.8")]
    sustain: f64,
    #[structopt(short, long, default_value = "0.5")]
    release: f64,
    #[structopt(short = "A", long, default_value = "0.5")]
    amplitude: f64,
}

arg_enum! {
    #[derive(Debug)]
    enum Waveform {
        Sine,
        Square,
        Saw,
        Tri,
        Pulse,
    }
}

struct Adsr {
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
}

struct Note {
    frequency: f64,
    velocity: f64,
    on_time: f64,
    off_time: Option<f64>,
}

#[derive(Default)]
struct MidiChannelState {
    notes: HashMap<wmidi::Note, Note>,
    pitch_bend: f64,
    program: u8,
}

struct MidiSynth {
    audio_channels: usize,
    sample_rate: usize,
    audio_index: usize,
    start_time: Option<Instant>,
    waveform: fn(&Self, f64) -> f64,
    pulse_width: f64,
    adsr: Adsr,
    amplitude: f64,
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
        let waveform = match opts.waveform {
            Waveform::Sine => Self::waveform_sine,
            Waveform::Square => Self::waveform_square,
            Waveform::Saw => Self::waveform_saw,
            Waveform::Tri => Self::waveform_tri,
            Waveform::Pulse => Self::waveform_pulse,
        };
        MidiSynth {
            audio_channels: config.channels as usize,
            sample_rate: config.sample_rate.0 as usize,
            audio_index: 0,
            start_time: None,
            waveform,
            pulse_width: opts.pulse_width,
            adsr: Adsr {
                attack: opts.attack,
                decay: opts.decay,
                sustain: opts.sustain,
                release: opts.release,
            },
            amplitude: opts.amplitude,
            receiver,
            midi_channel_states: Default::default(),
        }
    }

    pub fn make_stream<T: Sample>(
        mut self,
        device: &Device,
        config: &StreamConfig,
    ) -> Result<Stream, cpal::BuildStreamError> {
        device.build_output_stream(
            &config,
            move |data, info| self.output_callback::<T>(data, info),
            Self::error_callback,
        )
    }

    fn error_callback(err: cpal::StreamError) {
        eprintln!("Error: audio output stream: {}", err);
    }

    fn output_callback<T: Sample>(&mut self, data: &mut [T], _info: &cpal::OutputCallbackInfo) {
        self.process_messages();
        self.flush_notes();

        for frame in data.chunks_exact_mut(self.audio_channels) {
            let t = self.audio_index as f64 / self.sample_rate as f64;
            let y = self.synthesize(t) as f32;
            frame.fill(Sample::from(&y));
            self.audio_index += 1;
        }
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
                    channels[ch as usize].pitch_bend = pitch_bend as f64 / 8192.0 - 1.0;
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
        let t = self.audio_index as f64 / self.sample_rate as f64;
        let release = self.adsr.release;
        for ch in self.midi_channel_states.iter_mut() {
            ch.notes
                .retain(|_, n| n.off_time.map_or(true, |off_t| t - off_t < release));
        }
    }

    fn synthesize(&self, t: f64) -> f64 {
        let mut y = 0.;
        for channel_state in self.midi_channel_states.iter() {
            for note in channel_state.notes.values() {
                let freq = note.frequency * 2_f64.powf(channel_state.pitch_bend / 6.);
                let amp = note.velocity * self.amplitude;
                let t_on = t - note.on_time;
                let t_off = t - note.off_time.unwrap_or(f64::INFINITY);
                let env = self.envelope(t_on, t_off);
                y += amp * env * (self.waveform)(self, freq * t_on);
            }
        }
        y
    }

    fn envelope(&self, t_on: f64, t_off: f64) -> f64 {
        let adsr = &self.adsr;
        if t_on < 0.0 {
            0.0
        } else if t_on < adsr.attack {
            t_on / adsr.attack
        } else if t_on < adsr.attack + adsr.decay {
            1. - (1. - adsr.sustain) * (t_on - adsr.attack) / adsr.decay
        } else if t_off < 0.0 {
            adsr.sustain
        } else if t_off < adsr.release {
            adsr.sustain * (1. - t_off / adsr.release)
        } else {
            0.0
        }
    }

    fn waveform_sine(&self, t: f64) -> f64 {
        (TAU * t).sin()
    }

    fn waveform_square(&self, t: f64) -> f64 {
        1. - 2. * (2. * t % 2.).floor()
    }

    fn waveform_saw(&self, t: f64) -> f64 {
        (t + 0.5) % 1. * 2. - 1.
    }

    fn waveform_tri(&self, t: f64) -> f64 {
        ((4. * t + 3.) % 4. - 2.).abs() - 1.
    }

    fn waveform_pulse(&self, t: f64) -> f64 {
        if t % 1. < self.pulse_width { -1. } else { 1. }
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
    let opts = Opts::from_args();
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
    let sample_format = supported_config.sample_format();
    let config = supported_config.config();

    let (sender, receiver) = mpsc::channel();

    let synth = MidiSynth::new(opts.synth_opts, &config, receiver);
    let stream = match sample_format {
        SampleFormat::F32 => synth.make_stream::<f32>(&device, &config),
        SampleFormat::I16 => synth.make_stream::<i16>(&device, &config),
        SampleFormat::U16 => synth.make_stream::<u16>(&device, &config),
    }?;
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
            println!("{:?}", message);
        }
    };
    let _midi_conn = midi_in.connect(&port, "midi_synth", input_callback, ());

    println!("Receiving midi messages... Press Ctr+C to exit");
    wait_for_ctrlc()?;
    println!("Exiting");

    Ok(())
}
