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
  throw Error("bad letter");
}

function parseRepeat(source: string): PartSource<Part> {
  const next = parseLetter(source);
  return next.source[0] == "*"
    ? { part: { repeat: next.part }, source: next.source.slice(1) }
    : next;
}

function loop<State>(start: State, update: (state: State) => State): State {
  let state = start;
  while (state) {
    state = update(state);
  }
  return state;
}

function parsePatternFun(source: string): Pattern {
  const pattern: Pattern = [];
  loop(source, (state) => {
    const next = parseRepeat(state);
    pattern.push(next.part);
    return next.source;
  });
  // while (source) {
  //   const next = parseRepeat(source);
  //   source = next.source;
  //   pattern.push(next.part);
  // }
  return pattern;
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
  const pattern = parsePatternFun("a*b");
  console.log(pattern);
}

main();
