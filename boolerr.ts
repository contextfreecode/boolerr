type Letter = string;
type Repeat = { repeat: Letter };
type Part = Letter | Repeat;
type Pattern = Part[];

function parsePattern(text: string): Pattern {
  let part: Part | undefined;
  let pattern: Pattern = [];
  let commit = () => {
    if (part !== undefined) {
      pattern.push(part as Part);
      part = undefined;
    }
  }
  for (const token of text) {
    if (token.match(/[A-Za-z]/)) {
      if (part) {
        commit();
      }
      part = token;
    } else if (token == "*") {
      if (typeof part == "string") {
        part = { repeat: part };
        commit();
      } else {
        throw Error(`bad pattern: ${part}`)
      }
    } else {
      throw Error("bad token")
    }
  }
  commit();
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
  const pattern = parsePattern("a*b");
  console.log(pattern);
}

main();
