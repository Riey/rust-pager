use bumpalo::Bump;
use crossbeam_queue::ArrayQueue;
use crossterm::Result;
use std::{
    fs::File,
    io::{ErrorKind, Read},
    sync::Arc,
    time::Duration,
};

fn push_newline<'b>(b: &'b Bump, tx: &ArrayQueue<&'b [u8]>, line: &[u8]) {
    let line = b.alloc_slice_copy(line);

    while tx.push(line).is_err() {
        std::thread::sleep(Duration::from_millis(50));
    }
}

pub fn read_from_stdin<'b>(
    mut stdin: File,
    b: &'b mut Bump,
    tx: Arc<ArrayQueue<&'b [u8]>>,
) -> Result<()> {
    let mut stdin_buf = [0; 8196];
    let mut start_len = 0;

    loop {
        let mut buf = match stdin.read(&mut stdin_buf[start_len..]) {
            Ok(l) => &stdin_buf[..start_len + l],
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(From::from(e)),
        };

        #[cfg(feature = "logging")]
        log::debug!("Read {} bytes", buf.len() - start_len);

        if start_len == buf.len() {
            #[cfg(feature = "logging")]
            log::info!("EOF");
            if !buf.is_empty() {
                push_newline(b, &tx, buf);
            }
            break Ok(());
        }

        start_len = buf.len();

        loop {
            match memchr::memchr(b'\n', buf) {
                Some(pos) => {
                    let (line, new_buf) = buf.split_at(pos);
                    buf = new_buf.split_first().expect("Must success").1;
                    push_newline(b, &tx, line);
                }
                None => {
                    break;
                }
            }
        }

        let end_len = buf.len();

        if stdin_buf.len() == end_len {
            #[cfg(feature = "logging")]
            log::info!("Too many bytes");
            // send incomplete single line
            push_newline(b, &tx, &buf);
            break Ok(());
        } else if start_len == end_len {
            // no bytes processed
        } else {
            stdin_buf.copy_within((start_len - end_len)..start_len, 0);
            start_len = end_len;
        }
    }
}
