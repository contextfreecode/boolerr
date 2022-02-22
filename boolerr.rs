use std::str::Chars;

enum Part {
    Letter(char),
    Repeat(char),
}

type Pattern = Vec<Part>;

struct PartSource<'a> {
    part: Part,
    source: Chars<'a>,
}

#[derive(Debug)]
struct ParseError {}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for ParseError {}

fn parse_letter(mut source: Chars) -> Result<PartSource, ParseError> {
    match source.next() {
        Some(letter @ 'a'..='z') => Ok(PartSource {
            part: Part::Letter(letter),
            source,
        }),
        _ => Err(ParseError {}),
    }
}

fn main() {
    //
}
