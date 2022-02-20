type Letter = string;
type Repeat = { repeat: Letter };
type Part = Letter | Repeat;
type Pattern = Part[];

type PartSource<SpecificPart extends Part = Part> = {
  part: SpecificPart;
  source: string;
};

function parseLetter(source: string): PartSource<Letter> | Error {
  if (source.match(/^[a-z]/)) {
    return { part: source[0], source: source.slice(1) };
  }
  return Error(`bad letter: '${source[0]}'`);
}

function parseRepeat(source: string): PartSource | Error {
  const sub = parseLetter(source);
  if (sub instanceof Error) return sub;
  return sub.source[0] == "*"
    ? { part: { repeat: sub.part }, source: sub.source.slice(1) }
    : sub;
}

function parsePattern(source: string): Pattern | Error {
  const pattern: Pattern = [];
  while (source) {
    const next = parseRepeat(source);
    if (next instanceof Error) return next;
    source = next.source;
    pattern.push(next.part);
  }
  return pattern;
}

function evalLetter(letter: string, subject: string): string | undefined {
  return letter == subject[0] ? subject.slice(1) : undefined;
}

function evalRepeat({ repeat }: Repeat, subject: string): string | undefined {
  while (subject) {
    const result = evalLetter(repeat, subject);
    if (result == undefined) return subject;
    subject = result;
  }
  return subject; // == ""
}

function evalPattern(pattern: Pattern, subject: string): string | undefined {
  for (const part of pattern) {
    const next = typeof part == "string"
      ? evalLetter(part, subject)
      : evalRepeat(part, subject);
    if (next == undefined) return undefined;
    subject = next;
  }
  return subject && undefined;
}

function parseEvalPattern(
  source: string,
  subject: string | undefined,
): boolean | undefined | Error { // true | false
  if (subject == undefined) return undefined;
  // Both "" and undefined are falsy in JS, so check explicitly.
  const pattern = parsePattern(source);
  if (pattern instanceof Error) return pattern;
  return evalPattern(pattern, subject) !== undefined;
}

function main() {
  const subjects = ["", "b", "aaab", "aaa", "aaabbb", "abba", undefined];
  for (const source of ["", "a*b", "a*b*", "a**"]) {
    console.log(`"${source}":`);
    // console.log(parsePattern(source));
    for (const subject of subjects) {
      console.log(`  ${subject}: ${parseEvalPattern(source, subject)}`);
    }
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const returning = undefined;
