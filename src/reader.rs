use crate::shared::{Buffer, RpLine};
use bumpalo::Bump;
use crossbeam_queue::ArrayQueue;
use crossterm::Result;
use std::{
    fs::File,
    io::{ErrorKind, Read},
    sync::{atomic::Ordering, Arc},
};

pub fn read_from_stdin<'b>(
    mut stdin: File,
    b: &'b mut Bump,
    tx: Arc<ArrayQueue<RpLine<'b>>>,
) -> Result<()> {
    let mut parser = vte::Parser::new();
    let mut buffer = Buffer::new(b, &tx);
    let mut stdin_buf = [0; 8196];

    loop {
        let buf = match stdin.read(&mut stdin_buf) {
            Ok(l) => &stdin_buf[..l],
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(From::from(e)),
        };

        if buf.is_empty() {
            #[cfg(feature = "logging")]
            log::info!("EOF");
            if !buffer.is_empty() {
                buffer.flush();
            }
            break Ok(());
        }

        if !crate::RUN.load(Ordering::Acquire) {
            break Ok(());
        }

        buf.iter().for_each(|b| parser.advance(&mut buffer, *b));

        if buffer.is_full() {
            #[cfg(feature = "logging")]
            log::error!("Too long");
            buffer.flush();
            break Ok(());
        }
    }
}
