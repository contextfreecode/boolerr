type Doc = { head?: Head };
type Head = { title?: string };
type DocReport = { title: string | undefined; ok: boolean };

function readDoc(url: string): Doc | Error {
  // deno-fmt-ignore
  return (
    url.match("fail") ? Error("Failed to read document") :
    url.match("head-missing") ? {} :
    url.match("title-missing") ? { head: { } } :
    url.match("title-empty") ? { head: { title: "" } } :
    { head: { title: `Title of ${url}` } }
  );
}

function buildDocReport(doc: Doc): DocReport {
  // return { title: doc.head && doc.head.title, ok: true };
  return { title: doc.head?.title, ok: true };
}

function readAndBuildDocReport(url: string): DocReport {
  const docOrError = readDoc(url);
  if (docOrError instanceof Error) {
    return { title: undefined, ok: false };
  } else {
    return buildDocReport(docOrError);
  }
}

function titledReport(report: DocReport): DocReport {
  return { ...report, title: report.title || "" };
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
    const report = readAndBuildDocReport(url);
    console.log("  Report:", report);
    console.log("  Titled:", titledReport(report));
    const hasTitle = readWhetherTitleNonEmpty(url);
    console.log(`  Has title: ${hasTitle} vs ${hasTitle || false}`);
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const throwing = undefined;
