#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]
use std::convert::TryFrom;
use std::f32::consts::TAU;
use std::sync::mpsc::{channel, Receiver, Sender};
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
enum MidiSynthError {
    #[error("No output device available")]
    OutputDeviceError,
    #[error("No output device available")]
    InputPortError,
}

use MidiSynthError::*;

struct Synth {
    num_channels: usize,
    sample_rate: usize,
    index: usize,
    start_time: Option<Instant>,
    receiver: Receiver<MidiMessage<'static>>,
}

impl Synth {
    pub fn new(config: &StreamConfig, receiver: Receiver<MidiMessage<'static>>) -> Synth {
        Synth {
            num_channels: config.channels as usize,
            sample_rate: config.sample_rate.0 as usize,
            index: 0,
            start_time: None,
            receiver,
        }
    }

    // pub fn output_callback<T: Sample>(&mut self, data: &mut [T], _info: &cpal::OutputCallbackInfo) {
    //     let freq = 440.0 * 2_f32.powf(-9. / 12.); // C4
    //     let amp = 0.5;
    //     let nsamples = data.len() / self.num_channels;
    //     let i = Array::range(0.0, nsamples as f32, 1.0);
    //     let t = (i + self.index as f32) / self.sample_rate as f32;
    //     let y = t.mapv(|t| (TAU * freq * t).sin() * amp);
    //     let out = y.map(Sample::from).insert_axis(Axis(1));
    //     let out = out.broadcast((nsamples, self.num_channels)).unwrap();
    //     data.copy_from_slice(out.as_standard_layout().as_slice().unwrap());
    //     self.index += nsamples;
    // }

    pub fn output_callback<T: Sample>(&mut self, data: &mut [T], _info: &cpal::OutputCallbackInfo) {
        let start_time = *self.start_time.get_or_insert_with(|| Instant::now());
        // let t0 = self.index as f32 / self.sample_rate as f32;
        // let t = start_time - start_time;
        // t.as_secs_f32();

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
        let freq = 440.0 * 2_f32.powf(-9. / 12.); // C4
        let amp = 0.5;
        return (TAU * freq * t).sin() * amp;
    }
}

pub fn input_callback(timestamp: u64, message: &[u8], sender: &mut Sender<MidiMessage<'static>>) {
    let message = MidiMessage::try_from(message).unwrap();
    sender.send(message.to_owned()).unwrap();
}

fn wait_for_ctrlc() {
    let (tx, rx) = channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })
    .unwrap();
    rx.recv().unwrap();
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let host = cpal::default_host();
    let device = host.default_output_device().ok_or(OutputDeviceError)?;
    let supported_config = device.default_output_config()?;
    // let sample_format = supported_config.sample_format();
    let config = supported_config.config();

    let (sender, reciever) = channel();
    let mut synth = Synth::new(&config, reciever);

    let mut midi_in = MidiInput::new("midi_synth")?;
    midi_in.ignore(midir::Ignore::All);
    let ports = midi_in.ports();
    let port = ports.get(opt.input_port).ok_or(InputPortError)?;
    let input_callback = move |timestamp: u64, message: &[u8], _: &mut ()| {
        let message = MidiMessage::try_from(message).unwrap();
        println!("{:?}", message);
        sender.send(message.to_owned()).unwrap();
    };
    let midi_conn = midi_in.connect(&port, "midi_synth", input_callback, ());

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
