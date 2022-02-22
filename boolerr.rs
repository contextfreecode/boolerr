use std::str::Chars;

enum Part {
    Letter(char),
    Repeat(char),
}

type Pattern = Vec<Part>;

#[derive(Debug)]
struct ParseError {}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for ParseError {}

fn parse_letter(source: &mut Chars) -> Result<Part, ParseError> {
    match source.next() {
        Some(letter @ 'a'..='z') => Ok(Part::Letter(letter)),
        _ => Err(ParseError {}),
    }
}

fn main() {
    //
}
