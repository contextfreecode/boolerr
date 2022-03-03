type Doc = { head?: Head };
type Head = { title?: string };
type Summary = { title: string | undefined; ok: boolean };

function throwError(message: string): never {
  throw Error(message);
}

function readDoc(url: string): Doc {
  // deno-fmt-ignore
  return (
    url.match("fail") ? throwError("Failed to read document") :
    url.match("head-missing") ? {} :
    url.match("title-missing") ? { head: { } } :
    url.match("title-empty") ? { head: { title: "" } } :
    { head: { title: `Title of ${url}` } }
  );
}

function buildSummary(doc: Doc): Summary {
  // return { title: doc.head && doc.head.title, ok: true };
  return { title: doc.head?.title, ok: true };
}

function readAndBuildSummary(url: string): Summary {
  try {
    return buildSummary(readDoc(url));
  } catch {
    return { title: undefined, ok: false };
  }
}

function titledSummary(summary: Summary): Summary {
  return { ...summary, title: summary.title || "" };
}

function isTitleNonEmpty(doc: Doc): boolean | undefined {
  const title = doc.head?.title;
  // return title === undefined ? undefined : !!title;
  return title === undefined ? undefined : Boolean(title);
}

function readWhetherTitleNonEmpty(url: string): boolean | undefined {
  return isTitleNonEmpty(readDoc(url));
}

function main() {
  const urls = ["good", "title-empty", "title-missing", "head-missing", "fail"];
  for (const url of urls) {
    console.log(`Checking "https://${url}/":`);
    const summary = readAndBuildSummary(url);
    console.log("  Summary:", summary);
    console.log("  Titled: ", titledSummary(summary));
    try {
      const hasTitle = readWhetherTitleNonEmpty(url);
      console.log(`  Has title: ${hasTitle} vs ${hasTitle || false}`);
    } catch (error) {
      console.log(`  Has title: ${error}`);
    }
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const throwing = undefined;
