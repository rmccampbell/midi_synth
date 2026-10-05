use std::cell::RefCell;
use std::collections::HashMap;
use std::f64::consts::TAU;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::sync::mpsc::{self, Receiver, Sender};
use std::time::{Duration, Instant};

use anyhow::anyhow;
use clap::{Args, Parser, ValueEnum};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Device, FromSample, Sample, SampleFormat, SizedSample, Stream, SupportedStreamConfig};
#[cfg(unix)]
use midir::os::unix::VirtualInput;
use midir::{MidiInput, MidiInputConnection};
use midly::{live::LiveEvent, num::u7, MidiMessage, Smf};

mod play;

struct SignedDuration {
    dur: Duration,
    neg: bool,
}

impl Display for SignedDuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{:?}", if self.neg { "-" } else { "" }, self.dur)
    }
}

fn signed_secs_f64(secs: f64) -> SignedDuration {
    SignedDuration {
        dur: Duration::from_secs_f64(secs.abs()),
        neg: secs < 0.,
    }
}

fn signed_time_diff(x: Instant, y: Instant) -> SignedDuration {
    if x >= y {
        SignedDuration { dur: x - y, neg: false }
    } else {
        SignedDuration { dur: y - x, neg: true }
    }
}

/// A simple midi synthesizer
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Opts {
    #[arg(short, long, default_value_t = 0)]
    input_port: usize,
    #[cfg(unix)]
    #[arg(short, long, num_args=0..=1, default_missing_value="midi_synth")]
    virtual_port: Option<String>,
    #[arg(short, long)]
    output_device: Option<usize>,
    #[arg(short = 'l', long)]
    list_input_ports: bool,
    #[arg(short = 'L', long)]
    list_output_devices: bool,
    #[arg(short, long, value_name = "MIDI_FILE")]
    play: Option<PathBuf>,
    #[command(flatten)]
    synth_opts: SynthOpts,
}

#[derive(Args, Debug, Clone, Copy)]
struct SynthOpts {
    #[arg(short, long, value_enum, default_value_t = Waveform::Tri)]
    waveform: Waveform,
    #[arg(short = 'P', long, default_value_t = 0.5)]
    pulse_width: f64,
    #[arg(short, long, default_value_t = 0.05)]
    attack: f64,
    #[arg(short, long, default_value_t = 0.2)]
    decay: f64,
    #[arg(short, long, default_value_t = 0.8)]
    sustain: f64,
    #[arg(short, long, default_value_t = 0.5)]
    release: f64,
    #[arg(short = 'A', long, default_value_t = 0.5)]
    amplitude: f64,
    #[arg(short = 'W', long, default_value_t = false)]
    freq_weighting: bool,
    #[cfg_attr(feature = "debug", arg(short = 'D', long, default_value_t = 0))]
    #[cfg_attr(not(feature = "debug"), arg(skip))]
    debug: u32,
}

#[derive(ValueEnum, Copy, Clone, Debug)]
enum Waveform {
    Sine,
    Square,
    #[value(alias = "sawtooth")]
    Saw,
    #[value(alias = "triangle")]
    Tri,
    Pulse,
}

type WaveformFunc = fn(f64, &WaveOpts) -> f64;

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

fn waveform_sine(t: f64, _: &WaveOpts) -> f64 {
    (TAU * t).sin()
}

fn waveform_square(t: f64, _: &WaveOpts) -> f64 {
    1. - 2. * (2. * t % 2.).floor()
}

fn waveform_saw(t: f64, _: &WaveOpts) -> f64 {
    (t + 0.5) % 1. * 2. - 1.
}

fn waveform_tri(t: f64, _: &WaveOpts) -> f64 {
    ((4. * t + 3.) % 4. - 2.).abs() - 1.
}

fn waveform_pulse(t: f64, w: &WaveOpts) -> f64 {
    if t % 1. < w.pulse_width {
        1.
    } else {
        -1.
    }
}

struct WaveOpts {
    waveform: WaveformFunc,
    pulse_width: f64,
    attack: f64,
    decay: f64,
    sustain: f64,
    release: f64,
    amplitude: f64,
    freq_weighting: bool,
}

struct NoteState {
    frequency: f64,
    velocity: f64,
    phase: f64,
    on_time: f64,
    off_time: Option<f64>,
}

struct MidiChannelState {
    notes: HashMap<u7, Vec<NoteState>>,
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
    wave_opts: WaveOpts,
    sample_index: usize,
    start_time: Option<Instant>,
    receiver: Receiver<(LiveEvent<'static>, Instant)>,
    midi_channel_states: [MidiChannelState; 16],
    debug: u32,
    buffer: RefCell<Vec<f64>>,
}

impl MidiSynth {
    const DELAY: f64 = 0.05;

    pub fn new(
        opts: SynthOpts,
        stream_config: SupportedStreamConfig,
        receiver: Receiver<(LiveEvent<'static>, Instant)>,
    ) -> MidiSynth {
        MidiSynth {
            stream_config,
            wave_opts: WaveOpts {
                waveform: opts.waveform.into(),
                pulse_width: opts.pulse_width,
                attack: opts.attack,
                decay: opts.decay,
                sustain: opts.sustain,
                release: opts.release,
                amplitude: opts.amplitude,
                freq_weighting: opts.freq_weighting,
            },
            sample_index: 0,
            start_time: None,
            receiver,
            midi_channel_states: Default::default(),
            debug: opts.debug,
            buffer: RefCell::new(Vec::new()),
        }
    }

    fn audio_channels(&self) -> usize {
        self.stream_config.channels().into()
    }

    fn sample_rate(&self) -> f64 {
        self.stream_config.sample_rate().into()
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
            format => {
                eprintln!("Warning: Unsupported sample format {format}, falling back to f32.");
                self.make_stream_fmt::<f32>(device)?
            }
        })
    }

    fn make_stream_fmt<T: SizedSample + FromSample<f64>>(
        mut self,
        device: &Device,
    ) -> Result<Stream, cpal::Error> {
        device.build_output_stream(
            self.stream_config.config(),
            move |data, info| self.output_callback::<T>(data, info),
            Self::error_callback,
            None,
        )
    }

    fn error_callback(err: cpal::Error) {
        eprintln!("Error: audio output stream: {}", err);
    }

    fn output_callback<T: Sample + FromSample<f64>>(
        &mut self,
        data: &mut [T],
        info: &cpal::OutputCallbackInfo,
    ) {
        let start_time = *self.start_time.get_or_insert_with(Instant::now);
        let fn_start_time = Instant::now();
        let samp_time = self.sample_time();
        let samp_index = self.sample_index;

        let any_messages = self.process_messages(start_time);
        self.flush_notes();

        self.synthesize(data);

        if cfg!(feature = "debug") && self.debug >= 2 {
            let nframes = data.len() / self.audio_channels();
            let seq_num = samp_index / nframes;
            if any_messages || seq_num % 10 == 0 {
                let real_time = (fn_start_time - start_time).as_secs_f64();
                println!(
                    "Output {seq_num}: sample time: {samp_time:.3}, nframes: {nframes}, \
                    latency: {:?}, compute time: {:?}, time drift: {}",
                    info.timestamp().playback - info.timestamp().callback,
                    fn_start_time.elapsed(),
                    signed_secs_f64(samp_time - real_time)
                );
            }
        }
    }

    fn process_messages(&mut self, start_time: Instant) -> bool {
        let mut any_messages = false;
        for (event, time) in self.receiver.try_iter() {
            any_messages = true;
            let rel_time = time.saturating_duration_since(start_time).as_secs_f64();
            let sched_time = rel_time + Self::DELAY;
            if cfg!(feature = "debug") && self.debug >= 2 {
                println!(
                    "Process message: {event:?}, time: {rel_time}, sample time delta: {}",
                    signed_secs_f64(rel_time - self.sample_time())
                );
            }

            let LiveEvent::Midi { channel, message } = event else {
                continue;
            };
            let chan_state = &mut self.midi_channel_states[channel.as_int() as usize];

            match message {
                MidiMessage::NoteOff { key, vel } | MidiMessage::NoteOn { key, vel } => {
                    if matches!(message, MidiMessage::NoteOff { .. }) || vel.as_int() == 0 {
                        // Note off
                        if let Some(notes) = chan_state.notes.get_mut(&key) {
                            notes
                                .iter_mut()
                                .find(|n| n.off_time.is_none())
                                .map(|n| n.off_time = Some(sched_time));
                        }
                    } else {
                        // Note on
                        let notes = chan_state.notes.entry(key).or_default();
                        notes.push(NoteState {
                            frequency: Self::note_to_freq(key),
                            velocity: vel.as_int() as f64 / 127.,
                            phase: 0.,
                            on_time: sched_time,
                            off_time: None,
                        });
                    }
                }
                MidiMessage::PitchBend { bend } => {
                    chan_state.pitch_bend = (bend.as_f64() / 6.).exp2();
                }
                MidiMessage::ProgramChange { program } => {
                    chan_state.program = program.into();
                }
                MidiMessage::Controller { controller, value } => {
                    Self::control_change(chan_state, controller.into(), value.into());
                }
                _ => {}
            }
        }
        any_messages
    }

    fn control_change(chan_state: &mut MidiChannelState, controller: u8, _value: u8) {
        match controller {
            // All sound off
            120 => {
                chan_state.notes.clear();
            }
            _ => {}
        }
    }

    fn flush_notes(&mut self) {
        let t = self.sample_time();
        let release = self.wave_opts.release;
        for ch in self.midi_channel_states.iter_mut() {
            ch.notes.retain(|_, notes| {
                notes.retain(|n| n.off_time.map_or(true, |off_t| t - off_t < release));
                !notes.is_empty()
            });
        }
    }

    fn synthesize<T: Sample + FromSample<f64>>(&mut self, data: &mut [T]) {
        let nframes = data.len() / self.audio_channels();
        {
            let mut buffer = self.buffer.borrow_mut();
            buffer.clear();
            buffer.resize(nframes, 0.);

            for channel_state in self.midi_channel_states.iter() {
                for note in channel_state.notes.values().flatten() {
                    self.synthesize_note(note, channel_state, &mut buffer);
                }
            }

            let frames = data.chunks_exact_mut(self.audio_channels());
            for (frame, sample) in frames.zip(&*buffer) {
                frame.fill(T::from_sample(*sample));
            }
        }
        self.advance(nframes);
    }

    fn synthesize_note(
        &self,
        note: &NoteState,
        channel_state: &MidiChannelState,
        buffer: &mut [f64],
    ) {
        let w = &self.wave_opts;
        let freq = note.frequency * channel_state.pitch_bend;
        let mut amp = note.velocity * w.amplitude;
        if w.freq_weighting {
            amp *= Self::inv_a_weighting(freq);
        }
        let (on, off) = (note.on_time, note.off_time.unwrap_or(f64::INFINITY));
        let t0 = self.sample_index as f64 / self.sample_rate();
        for (i, frame) in buffer.iter_mut().enumerate() {
            let delta_t = i as f64 / self.sample_rate();
            let t = t0 + delta_t;
            let env = Self::envelope(w, t - on, t - off);
            let y = amp * env * (w.waveform)(note.phase + freq * delta_t, w);
            *frame += y;
        }
    }

    fn advance(&mut self, nframes: usize) {
        self.sample_index += nframes;
        let delta_t = nframes as f64 / self.sample_rate();
        for channel_state in self.midi_channel_states.iter_mut() {
            for note in channel_state.notes.values_mut().flatten() {
                let freq = note.frequency * channel_state.pitch_bend;
                note.phase += freq * delta_t;
            }
        }
    }

    fn envelope(w: &WaveOpts, t_on: f64, t_off: f64) -> f64 {
        let ads = if t_on < 0. {
            0.
        } else if t_on < w.attack {
            t_on / w.attack
        } else if t_on < w.attack + w.decay {
            1. - (1. - w.sustain) * (t_on - w.attack) / w.decay
        } else {
            w.sustain
        };
        let r = if t_off < 0. {
            1.
        } else if t_off < w.release {
            1. - t_off / w.release
        } else {
            0.
        };
        ads * r
    }

    fn note_to_freq(key: u7) -> f64 {
        440. * 2_f64.powf((key.as_int() as f64 - 69.) / 12.)
    }

    fn inv_a_weighting(f: f64) -> f64 {
        let c1 = 20.598997_f64.powi(2);
        let c2 = 107.65265_f64.powi(2);
        let c3 = 737.86223_f64.powi(2);
        let c4 = 12194.217_f64.powi(2);

        let f2 = f * f;
        let denom = c4 * f2 * f2;
        let num = (f2 + c1) * (f2 + c4) * ((f2 + c2) * (f2 + c3)).sqrt();

        return 0.7943597 * num / denom;
    }
}

fn make_midi_connection(
    sender: Sender<(LiveEvent<'static>, Instant)>,
    opts: &Opts,
) -> anyhow::Result<MidiInputConnection<()>> {
    let debug = opts.synth_opts.debug;

    let mut ts_start: Option<Instant> = None;
    let input_callback = move |timestamp_us: u64, message: &[u8], _: &mut ()| {
        let timestamp = Duration::from_micros(timestamp_us);
        let ts_start = *ts_start.get_or_insert_with(|| Instant::now() - timestamp);
        let time = ts_start + timestamp;

        let message = LiveEvent::parse(message).unwrap().to_static();
        sender.send((message, time)).unwrap();

        if cfg!(feature = "debug") && debug >= 1 {
            println!(
                "Midi callback: {message:?}, time: {timestamp:?}, time drift: {}",
                signed_time_diff(time, Instant::now()),
            );
        }
    };

    let mut midi_in = MidiInput::new("midi_synth")?;
    midi_in.ignore(midir::Ignore::All);

    #[cfg(unix)]
    if let Some(name) = &opts.virtual_port {
        return Ok(midi_in
            .create_virtual(name, input_callback, ())
            .map_err(|e| midir::ConnectError::new(e.kind(), ()))?);
    }

    let ports = midi_in.ports();
    let port = ports
        .get(opts.input_port)
        .ok_or(anyhow!("Requested midi input port not available"))?;
    Ok(midi_in
        .connect(port, "midi_synth", input_callback, ())
        .map_err(|e| midir::ConnectError::new(e.kind(), ()))?)
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

    if opts.list_output_devices {
        let host = cpal::default_host();
        println!("Host: {:?}", host.id());
        let def_dev_id = host.default_output_device().and_then(|d| d.id().ok());
        for dev in host.output_devices()? {
            let isdef = Some(dev.id()?) == def_dev_id;
            let c = if isdef { '*' } else { ' ' };
            println!("{} {}", c, dev.description()?.name());
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

    let (sender, receiver) = mpsc::channel();

    let config = device.default_output_config()?;
    let synth = MidiSynth::new(opts.synth_opts, config, receiver);
    let stream = synth.make_stream(&device)?;
    stream.play()?;

    if let Some(midi_file) = opts.play {
        println!("Playing MIDI file: {}", midi_file.to_string_lossy());
        let midi_data = std::fs::read(&midi_file)?;
        let smf = Smf::parse(&midi_data)?;
        play::play_midi_file(&smf, sender, true)?;
    } else {
        let _midi_conn = make_midi_connection(sender, &opts)?;
        println!("Receiving midi messages... Press Ctr+C to exit");
        wait_for_ctrlc()?;
        println!("Exiting");
    }

    Ok(())
}
