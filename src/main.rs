#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]
use std::collections::HashMap;
use std::convert::TryFrom;
use std::f32::consts::TAU;
use std::sync::mpsc::{self, Receiver, Sender};
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::{Duration, Instant};

use anyhow;
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{Sample, SampleFormat, StreamConfig};
use ctrlc;
use midir::MidiInput;
use structopt::StructOpt;
use thiserror::Error;
use wmidi::MidiMessage;

#[derive(StructOpt, Debug)]
struct Opt {
    #[structopt(short, long, default_value = "0")]
    input_port: usize,
    #[structopt(short, long)]
    output_device: Option<usize>,
    #[structopt(short = "l", long)]
    list_input_ports: bool,
    #[structopt(short = "L", long)]
    list_output_devices: bool,
}

#[derive(Error, Debug)]
#[error("Requested or default output device not available")]
struct OutputDeviceError;

#[derive(Error, Debug)]
#[error("Requested midi input port not available")]
struct InputPortError;

struct Synth {
    num_channels: usize,
    sample_rate: usize,
    index: usize,
    start_time: Option<Instant>,
    receiver: Receiver<(MidiMessage<'static>, Instant)>,
    channel_states: [MidiChannelState; 16],
}

#[derive(Default)]
struct MidiChannelState {
    notes: HashMap<u8, Note>,
}

#[derive(Clone)]
struct Note {
    frequency: f32,
    velocity: u8,
    on_time: f32,
    off_time: Option<f32>,
}

impl Synth {
    pub fn new(
        config: &StreamConfig,
        receiver: Receiver<(MidiMessage<'static>, Instant)>,
    ) -> Synth {
        Synth {
            num_channels: config.channels as usize,
            sample_rate: config.sample_rate.0 as usize,
            index: 0,
            start_time: None,
            receiver,
            channel_states: Default::default(),
        }
    }

    fn process_messages(&mut self) {
        let start_time = *self.start_time.get_or_insert_with(|| Instant::now());

        while let Some((msg, time)) = match self.receiver.try_recv() {
            Ok(pair) => Some(pair),
            Err(mpsc::TryRecvError::Empty) => None,
            Err(e) => panic!("{}", e),
        } {
            let time = (time - start_time).as_secs_f32();
            if let Some(ch) = msg.channel() {
                let channel_state = &mut self.channel_states[ch as usize];
                match msg {
                    MidiMessage::NoteOn(ch, note, velocity) => {
                        channel_state.notes.insert(
                            note.into(),
                            Note {
                                frequency: note.to_freq_f32(),
                                velocity: velocity.into(),
                                on_time: time,
                                off_time: None,
                            },
                        );
                    }
                    MidiMessage::NoteOff(ch, note, _) => {
                        channel_state.notes.remove(&note.into());
                        // if let Some(note) = channel_state.notes.get_mut(&note.into()) {
                        //     note.off_time = Some(time);
                        // }
                    }
                    MidiMessage::PitchBendChange(ch, pitchbend) => {}
                    MidiMessage::ProgramChange(ch, prog) => {}
                    _ => {}
                }
            }
        }
    }

    pub fn output_callback<T: Sample>(&mut self, data: &mut [T], _info: &cpal::OutputCallbackInfo) {
        self.process_messages();

        let nsamples = data.len() / self.num_channels;
        for (i, frame) in data.chunks_exact_mut(self.num_channels).enumerate() {
            let t = (self.index + i) as f32 / self.sample_rate as f32;
            let y = self.synthesize(t);
            let out = Sample::from(&y);
            frame.copy_from_slice(&vec![out; self.num_channels]);
        }
        self.index += nsamples;
    }

    fn synthesize(&self, t: f32) -> f32 {
        let mut y = 0.;
        for channel_state in self.channel_states.iter() {
            for note in channel_state.notes.values() {
                let freq = note.frequency;
                let amp = note.velocity as f32 / 127.;
                y += (TAU * freq * t).sin() * amp;
            }
        }
        y
    }
}

fn wait_for_ctrlc() {
    let (tx, rx) = mpsc::channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })
    .unwrap();
    rx.recv().unwrap();
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let host = cpal::default_host();
    if opt.list_output_devices {
        let def_dev_name = host.default_output_device().and_then(|d| d.name().ok());
        for dev in host.output_devices()? {
            let isdef = Some(dev.name()?) == def_dev_name;
            let c = if isdef { '*' } else { ' ' };
            println!("{} {}", c, dev.name()?);
        }
        return Ok(());
    }

    let mut midi_in = MidiInput::new("midi_synth")?;
    midi_in.ignore(midir::Ignore::All);
    let ports = midi_in.ports();
    if opt.list_input_ports {
        for port in ports {
            println!("{}", midi_in.port_name(&port)?);
        }
        return Ok(());
    }

    let device = match opt.output_device {
        None => host.default_output_device(),
        Some(index) => host.output_devices()?.nth(index),
    }
    .ok_or(OutputDeviceError)?;
    let supported_config = device.default_output_config()?;
    // let sample_format = supported_config.sample_format();
    let config = supported_config.config();

    let (sender, receiver) = mpsc::channel();

    let port = ports.get(opt.input_port).ok_or(InputPortError)?;
    let input_callback = move |timestamp: u64, message: &[u8], _: &mut ()| {
        let time = Instant::now();
        let message = MidiMessage::try_from(message).unwrap().to_owned();
        // println!("{:?}", message);
        sender.send((message, time)).unwrap();
    };
    let midi_conn = midi_in.connect(&port, "midi_synth", input_callback, ());

    let mut synth = Synth::new(&config, receiver);
    let err_fn = |err| eprintln!("An error occurred in the output audio stream: {}", err);
    let stream = device.build_output_stream(
        &config,
        move |data, info| synth.output_callback::<f32>(data, info),
        err_fn,
    )?;
    stream.play()?;

    println!("Waiting for Ctrl-C...");
    wait_for_ctrlc();
    println!("Exiting");

    Ok(())
}
