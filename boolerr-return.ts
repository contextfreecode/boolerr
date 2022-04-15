type Doc = { head?: Head };
type Head = { title?: string };
type Summary = { title: string | undefined; ok: boolean };

function readDoc(url: string): Doc | Error {
  // deno-fmt-ignore
  return (
    url.match("fail") ? Error(`Bad read of ${url}`) :
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
  const docOrError = readDoc(url);
  if (docOrError instanceof Error) {
    return { title: undefined, ok: false };
  } else {
    return buildSummary(docOrError);
  }
}

function isTitleNonEmpty(doc: Doc): boolean | undefined {
  const title = doc.head?.title;
  // return title === undefined ? undefined : !!title;
  return title === undefined ? undefined : Boolean(title);
}

function readWhetherTitleNonEmpty(url: string): boolean | undefined | Error {
  const docOrError = readDoc(url);
  if (docOrError instanceof Error) return docOrError;
  return isTitleNonEmpty(docOrError);
}

function main() {
  const urls = ["good", "title-empty", "title-missing", "head-missing", "fail"];
  for (const url of urls) {
    console.log(`Checking "https://${url}/":`);
    // Summary.
    const summary = readAndBuildSummary(url);
    console.log("  Summary:", summary);
    console.log("  Title:", summary.title ?? "");
    // Has title.
    const hasTitle = readWhetherTitleNonEmpty(url);
    const hasTitleSure = hasTitle instanceof Error ? false : hasTitle || false;
    console.log(`  Has title: ${hasTitle} vs ${hasTitleSure}`);
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const returning = undefined;
