// use std::io::{Error, ErrorKind};

struct Doc {
    head: Option<Head>,
}

struct Head {
    title: Option<String>,
}

struct DocReport {
    title: String,
    ok: bool,
}

// #[derive(Debug)]
// struct Error(String)
// impl std::fmt::Display for Error {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(f, "{:?}", self)
//     }
// }
// impl std::error::Error for Error {}

fn read_doc(url: &str) -> Result<Doc, String> {
    match () {
        _ if url.contains("fail") => Err("Failed to read document".into()),
        _ if url.contains("headless") => Ok(Doc { head: None }),
        _ if url.contains("empty") => Ok(Doc {
            head: Some(Head { title: None }),
        }),
        _ => Ok(Doc {
            head: Some(Head {
                title: Some("Something".into()),
            }),
        }),
    }
}

fn main() {}
