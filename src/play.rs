use std::io::{self, Write};
use std::sync::mpsc::Sender;
use std::thread::sleep;
use std::time::{Duration, Instant};

use midly::{live::LiveEvent, Smf};
use midly::{TrackEvent, TrackEventKind};

// Default tempo in microseconds per beat, aka 120 BPM
const DEFAULT_TEMPO: u32 = 500_000;

struct AbsEvent<'a> {
    pub tick: u32,
    pub event: TrackEventKind<'a>,
}

struct ScheduledEvent<'a> {
    pub time: Duration,
    pub event: TrackEventKind<'a>,
}

fn to_abs_events<'a>(track_events: &[TrackEvent<'a>]) -> Vec<AbsEvent<'a>> {
    let mut tick = 0;
    let mut abs_events = Vec::new();
    for event in track_events {
        tick += event.delta.as_int();
        abs_events.push(AbsEvent { tick, event: event.kind });
    }
    abs_events
}

fn merged_events<'a>(tracks: &[Vec<TrackEvent<'a>>]) -> Vec<AbsEvent<'a>> {
    let mut merged = Vec::new();
    for track in tracks {
        merged.extend(to_abs_events(&track));
    }
    merged.sort_by_key(|e| e.tick);
    merged
}

fn tick_duration(timing: midly::Timing, tempo: u32) -> Duration {
    match timing {
        midly::Timing::Metrical(ticks_per_beat) => {
            // Tempo is microseconds per beat
            Duration::from_micros(tempo as u64 / ticks_per_beat.as_int() as u64)
        }
        midly::Timing::Timecode(fps, ticks_per_frame) => {
            Duration::from_secs_f32(1. / (fps.as_f32() * ticks_per_frame as f32))
        }
    }
}

fn schedule_events<'a>(smf: &Smf<'a>) -> Vec<ScheduledEvent<'a>> {
    let mut tempo = DEFAULT_TEMPO;
    let mut tick_dur = tick_duration(smf.header.timing, tempo);
    let abs_events = merged_events(&smf.tracks);
    let mut scheduled = Vec::new();
    let mut last_tick = 0;
    let mut last_timestamp = Duration::ZERO;
    for AbsEvent { tick, event } in abs_events {
        let timestamp = last_timestamp + (tick - last_tick) * tick_dur;
        last_tick = tick;
        last_timestamp = timestamp;
        scheduled.push(ScheduledEvent { time: timestamp, event });

        if let TrackEventKind::Meta(meta) = event {
            if let midly::MetaMessage::Tempo(new_tempo) = meta {
                tempo = new_tempo.into();
                tick_dur = tick_duration(smf.header.timing, tempo);
            }
        }
    }
    scheduled
}

fn fmt_time(duration: Duration) -> String {
    let total_seconds = duration.as_secs();
    let minutes = total_seconds / 60;
    let seconds = total_seconds % 60;
    format!("{}:{:02}", minutes, seconds)
}

pub fn play_midi_file(
    smf: &Smf,
    sender: Sender<(LiveEvent<'static>, Instant)>,
    print_progress: bool,
) -> anyhow::Result<()> {
    let events = schedule_events(smf);
    let tot_dur = events.last().map_or(Duration::ZERO, |e| e.time);
    let tot_dur_fmt = fmt_time(tot_dur);
    let mut last_offset = Duration::ZERO;
    let start_time = Instant::now();
    for event in events {
        if print_progress && event.time > last_offset {
            // Print only when we are about to wait to hide the delay
            print!("\r{}/{tot_dur_fmt}", fmt_time(last_offset));
            io::stdout().flush().unwrap();
        }
        last_offset = event.time;
        let event_time = start_time + event.time;
        sleep(event_time - Instant::now());

        if let Some(live_event) = event.event.as_live_event() {
            sender.send((live_event.to_static(), event_time)).unwrap();
        }
    }
    if print_progress {
        println!("\r{}/{tot_dur_fmt}", fmt_time(last_offset));
    }
    Ok(())
}
