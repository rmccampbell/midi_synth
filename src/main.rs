use std::collections::HashMap;
use std::convert::TryFrom;
use std::f64::consts::TAU;
use std::sync::mpsc::{self, Receiver};
use std::time::{Duration, Instant};

use anyhow::anyhow;
use clap::{Parser, ValueEnum};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, FromSample, Sample, SampleFormat, SizedSample, Stream, SupportedStreamConfig};
use midir::MidiInput;
use wmidi::MidiMessage;

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

type WaveformFunc = fn(f64, &WaveProps) -> f64;

impl Into<WaveformFunc> for Waveform {
    fn into(self) -> WaveformFunc {
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
    if t % 1. < w.pulse_width {
        1.
    } else {
        -1.
    }
}

struct WaveProps {
    waveform: WaveformFunc,
    pulse_width: f64,
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    amplitude: f64,
}

struct NoteState {
    frequency: f64,
    velocity: f64,
    phase: f64,
    on_time: f64,
    off_time: Option<f64>,
}

struct MidiChannelState {
    notes: HashMap<wmidi::Note, Vec<NoteState>>,
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
    stream_config: SupportedStreamConfig,
    wave_props: WaveProps,
    sample_index: usize,
    start_time: Option<Instant>,
    receiver: Receiver<(MidiMessage<'static>, Instant)>,
    midi_channel_states: [MidiChannelState; 16],
}

const DELAY: f64 = 0.1;

impl MidiSynth {
    pub fn new(
        opts: SynthOpts,
        stream_config: SupportedStreamConfig,
        receiver: Receiver<(MidiMessage<'static>, Instant)>,
    ) -> MidiSynth {
        MidiSynth {
            stream_config,
            wave_props: WaveProps {
                waveform: opts.waveform.into(),
                pulse_width: opts.pulse_width,
                attack: opts.attack,
                decay: opts.decay,
                sustain: opts.sustain,
                release: opts.release,
                amplitude: opts.amplitude,
            },
            sample_index: 0,
            start_time: None,
            receiver,
            midi_channel_states: Default::default(),
        }
    }

    fn audio_channels(&self) -> usize {
        self.stream_config.channels().into()
    }

    fn sample_rate(&self) -> f64 {
        self.stream_config.sample_rate().0.into()
    }

    fn sample_time(&self) -> f64 {
        self.sample_index as f64 / self.sample_rate()
    }

    pub fn make_stream(self, device: &Device) -> anyhow::Result<Stream> {
        Ok(match self.stream_config.sample_format() {
            SampleFormat::I8 => self.make_stream_fmt::<i8>(device)?,
            SampleFormat::I16 => self.make_stream_fmt::<i16>(device)?,
            SampleFormat::I32 => self.make_stream_fmt::<i32>(device)?,
            SampleFormat::I64 => self.make_stream_fmt::<i64>(device)?,
            SampleFormat::U8 => self.make_stream_fmt::<u8>(device)?,
            SampleFormat::U16 => self.make_stream_fmt::<u16>(device)?,
            SampleFormat::U32 => self.make_stream_fmt::<u32>(device)?,
            SampleFormat::U64 => self.make_stream_fmt::<u64>(device)?,
            SampleFormat::F32 => self.make_stream_fmt::<f32>(device)?,
            SampleFormat::F64 => self.make_stream_fmt::<f64>(device)?,
            format => Err(anyhow!("Unsupported sample format '{format}'"))?,
        })
    }

    pub fn make_stream_fmt<T: SizedSample + FromSample<f32>>(
        mut self,
        device: &Device,
    ) -> Result<Stream, cpal::BuildStreamError> {
        device.build_output_stream(
            &self.stream_config.config(),
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

        for frame in data.chunks_exact_mut(self.audio_channels()) {
            let y = self.synthesize(self.sample_time()) as f32;
            frame.fill(T::from_sample(y));
            self.next_sample();
        }
    }

    fn process_messages(&mut self) {
        let start_time = *self.start_time.get_or_insert_with(Instant::now);

        for (msg, time) in self.receiver.try_iter() {
            let time = time.saturating_duration_since(start_time).as_secs_f64() + DELAY;
            let Some(chan) = msg.channel() else { continue };
            let chan_state = &mut self.midi_channel_states[chan as usize];
            match msg {
                MidiMessage::NoteOn(_ch, note, velocity) => {
                    let notes = chan_state.notes.entry(note).or_default();
                    notes.push(NoteState {
                        frequency: note.to_freq_f64(),
                        velocity: u8::from(velocity) as f64 / 127.,
                        phase: 0.,
                        on_time: time,
                        off_time: None,
                    });
                }
                MidiMessage::NoteOff(_ch, note, _) => {
                    if let Some(notes) = chan_state.notes.get_mut(&note) {
                        notes
                            .iter_mut()
                            .find(|n| n.off_time.is_none())
                            .map(|n| n.off_time = Some(time));
                    }
                }
                MidiMessage::PitchBendChange(_ch, pitch_bend) => {
                    let pitch_bend: u16 = pitch_bend.into();
                    let pitch_bend_norm = pitch_bend as f64 / 8192.0 - 1.0;
                    let pitch_bend_mult = (pitch_bend_norm / 6.).exp2();
                    chan_state.pitch_bend = pitch_bend_mult;
                }
                MidiMessage::ProgramChange(_ch, prog) => {
                    chan_state.program = prog.into();
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
            ch.notes.retain(|_, notes| {
                notes.retain(|n| n.off_time.map_or(true, |off_t| t - off_t < release));
                !notes.is_empty()
            });
        }
    }

    fn synthesize(&self, t: f64) -> f64 {
        let w = &self.wave_props;
        let mut y = 0.;
        for channel_state in self.midi_channel_states.iter() {
            for note in channel_state.notes.values().flatten() {
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
        let delta_t = 1. / self.sample_rate();
        for channel_state in self.midi_channel_states.iter_mut() {
            for note in channel_state.notes.values_mut().flatten() {
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
        println!("Host: {:?}", host.id());
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
    .ok_or(anyhow!("Requested or default output device not available"))?;
    let config = device.default_output_config()?;
    let (sender, receiver) = mpsc::channel();

    let synth = MidiSynth::new(opts.synth_opts, config, receiver);
    let stream = synth.make_stream(&device)?;
    stream.play()?;

    let mut midi_in = MidiInput::new("midi_synth")?;
    midi_in.ignore(midir::Ignore::All);
    let ports = midi_in.ports();
    let port = ports
        .get(opts.input_port)
        .ok_or(anyhow!("Requested midi input port not available"))?;

    let mut ts_start: Option<Instant> = None;
    let input_callback = move |timestamp_us: u64, message: &[u8], _: &mut ()| {
        let timestamp = Duration::from_micros(timestamp_us);
        let ts_start = *ts_start.get_or_insert_with(|| Instant::now() - timestamp);
        let time = ts_start + timestamp;

        let message = MidiMessage::try_from(message).unwrap();
        sender.send((message.to_owned(), time)).unwrap();
        #[cfg(debug_assertions)]
        if debug {
            println!("{:?} {:?}", time, message);
        }
    };
    let _midi_conn = midi_in.connect(port, "midi_synth", input_callback, ())?;

    println!("Receiving midi messages... Press Ctr+C to exit");
    wait_for_ctrlc()?;
    println!("Exiting");

    Ok(())
}
