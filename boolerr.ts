type Char = string;
type Repeat = { repeat: Char };
type Part = Char | Repeat;
type Pattern = Part[];

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
  for (let text of ["true", "false", "", "bad"]) {
    const processed = processResult(text);
    const truthy = processed ? "✓" : "✗";
    console.log(`${truthy} "${text}" is ${processed}`);
  }
}

main();
