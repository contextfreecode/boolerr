type Doc = { head?: Head };
type Head = { title?: string };
type DocReport = { title: string | undefined; ok: boolean };

function readDoc(url: string): Doc | Error {
  // deno-fmt-ignore
  return (
    url.match("fail") ? Error("Failed to read document") :
    url.match("headless") ? {} :
    url.match("empty") ? { head: { title: "" } } :
    { head: { title: "Something" } }
  );
}

function buildDocReport(doc: Doc): DocReport {
  // return { title: doc.head && doc.head.title, ok: true }
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

function isTitleNonEmpty(doc: Doc): boolean | undefined {
  // return !!doc.head?.title;
  return Boolean(doc.head?.title);
}

function readIfTitleNonEmpty(url: string): boolean | undefined | Error {
  const docOrError = readDoc(url);
  if (docOrError instanceof Error) return docOrError;
  return isTitleNonEmpty(docOrError);
}

function main() {
  for (const url of ["good", "empty", "headless", "fail"]) {
    console.log(`Checking "https://${url}/":`);
    console.log("  Report:", readAndBuildDocReport(url));
    console.log("  Has title:", readIfTitleNonEmpty(url) || false);
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const throwing = undefined;
