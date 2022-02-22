type Letter = char;
type Repeat = Letter;
#[derive(Debug)]
enum Part {
    Letter(Letter),
    Repeat(Repeat),
}
type Pattern = Vec<Part>;

type Source<'a> = std::iter::Peekable<std::str::Chars<'a>>;

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
        Some(&letter @ 'a'..='z') => {
            source.next();
            Ok(letter)
        }
        _ => Err(ParseError {}),
    }
}

fn parse_repeat(source: &mut Source) -> Result<Part, ParseError> {
    let sub = parse_letter(source)?;
    Ok(match source.peek() {
        Some('*') => {
            source.next();
            Part::Repeat(sub)
        }
        _ => Part::Letter(sub),
    })
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
    for source in vec!["", "a*b", "a*b*", "a**"] {
        let pattern = parse_pattern(&mut source.chars().peekable());
        println!("{:?}", pattern);
    }
}
