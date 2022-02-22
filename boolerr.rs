use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug)]
struct Letter(char);
#[derive(Debug)]
struct Repeat(Letter);
#[derive(Debug)]
enum Part {
    Letter(Letter),
    Repeat(Repeat),
}
type Pattern = Vec<Part>;

type Source<'a> = Peekable<Chars<'a>>;

#[derive(Debug)]
struct ParseError {}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
impl std::error::Error for ParseError {}

fn parse_letter(source: &mut Source) -> Result<Letter, ParseError> {
    match source.peek() {
        Some(&c @ 'a'..='z') => {
            source.next();
            Ok(Letter(c))
        }
        _ => Err(ParseError {}),
    }
}

fn parse_repeat(source: &mut Source) -> Result<Part, ParseError> {
    let sub = parse_letter(source)?;
    match source.peek() {
        Some('*') => {
            source.next();
            Ok(Part::Repeat(Repeat(sub)))
        }
        _ => Ok(Part::Letter(sub)),
    }
}

fn parse_pattern(source: &mut Source) -> Result<Pattern, ParseError> {
    let mut pattern = vec![];
    while source.peek().is_some() {
        let next = parse_repeat(source)?;
        pattern.push(next);
    }
    Ok(pattern)
}

fn main() {
    let pattern = parse_pattern(&mut "a*b".chars().peekable());
    println!("{:?}", pattern);
}
