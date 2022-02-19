type Letter = string;
type Repeat = { repeat: Letter };
type Part = Letter | Repeat;
type Pattern = Part[];

type PartSource<SpecificPart extends Part> = {
  part: SpecificPart;
  source: string;
};

function parseLetter(source: string): PartSource<Letter> {
  if (source.match(/^[a-z]/)) {
    return { part: source[0], source: source.slice(1) };
  }
  throw Error(`bad letter: ${source[0]}`);
}

function parseRepeat(source: string): PartSource<Part> {
  const sub = parseLetter(source);
  return sub.source[0] == "*"
    ? { part: { repeat: sub.part }, source: sub.source.slice(1) }
    : sub;
}

function parsePattern(source: string): Pattern {
  const pattern: Pattern = [];
  while (source) {
    const next = parseRepeat(source);
    source = next.source;
    pattern.push(next.part);
  }
  return pattern;
}

function parseBool(text: string): boolean {
  const result = { true: true, false: false }[text];
  if (result == null) {
    throw Error("bad bool");
  }
  return result;
}

function parseBoolResult(text: string): boolean | Error {
  const result = { true: true, false: false }[text];
  if (result == null) {
    return Error("bad bool");
  }
  return result;
}

function process(text: string): boolean | undefined {
  return text ? parseBool(text) : undefined;
}

// function processResult(text: string): undefined | true | Error | false {
function processResult(text: string): boolean | Error | undefined {
  return text ? parseBoolResult(text) : undefined;
}

function main() {
  // for (let text of ["true", "false", "", "bad"]) {
  //   const processed = processResult(text);
  //   const truthy = processed ? "✓" : "✗";
  //   console.log(`${truthy} "${text}" is ${processed}`);
  // }
  for (const source of ["", "a*b", "a*b*", "a**"]) {
    console.log(`"${source}" ->`, parsePattern(source));
  }
}

main();
