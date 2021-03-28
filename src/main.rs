mod reader;
mod writer;

use bumpalo::Bump;
use crossbeam_queue::ArrayQueue;
use crossbeam_utils::thread::scope;
use crossterm::{tty::IsTty, Result};
use std::fs::File;
use std::path::PathBuf;
use std::sync::{atomic, Arc};

static RUN: atomic::AtomicBool = atomic::AtomicBool::new(true);

struct Args {
    path: Option<PathBuf>,
}

impl Args {
    pub fn parse() -> Option<Self> {
        let mut args = pico_args::Arguments::from_env();

        if args.contains(["-h", "--help"]) {
            println!("rp 0.1.0");
            println!("USAGE: `<command> | rp` or `rp <path>`");
            return None;
        }

        Some(Self {
            path: args.free_from_str().ok(),
        })
    }
}

fn get_input(args: &crate::Args) -> Result<File> {
    if !std::io::stdin().is_tty() {
        unsafe {
            use std::os::unix::prelude::FromRawFd;
            let stdin = File::from_raw_fd(libc::STDIN_FILENO);
            Ok(stdin)
        }
    } else {
        Ok(File::open(args.path.as_deref().expect("No given path"))?)
    }
}

fn main() -> Result<()> {
    #[cfg(feature = "logging")]
    {
        use simplelog::*;
        WriteLogger::init(
            LevelFilter::Trace,
            ConfigBuilder::new().build(),
            File::create("rp.log")?,
        )
        .unwrap();
        log_panics::init();
    }

    ctrlc::set_handler(|| {
        crate::RUN.store(false, atomic::Ordering::Release);
    })
    .expect("Set ctrlc handler");

    let args = match Args::parse() {
        Some(args) => args,
        None => return Ok(()),
    };
    let stdin = get_input(&args)?;
    let rx = Arc::new(ArrayQueue::new(1024 * 16));
    let mut b = Bump::with_capacity(1024 * 1024);

    scope(|s| {
        let tx = rx.clone();
        s.builder()
            .name("stdin".into())
            .spawn(|_| reader::read_from_stdin(stdin, &mut b, tx))?;

        writer::UiContext::new(rx)?.run()?;

        Ok(())
    })
    .unwrap()
}
