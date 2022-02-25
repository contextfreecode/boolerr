type Doc = { head?: Head };
type Head = { title?: string };
type DocReport = { title: string | undefined, ok: boolean };

function readDoc(url: string): Doc {
  if (url.match("fail")) {
    throw Error("Failed to read document");
  } else {
    // deno-fmt-ignore
    return (
      url.match("headless") ? {} :
      url.match("empty") ? { head: { title: "" } } :
      { head: { title: "Something" } }
    );
  }
}

function buildDocReport(doc: Doc): DocReport {
  // return { title: doc.head && doc.head.title, ok: true }
  return { title: doc.head?.title, ok: true }
}

function readAndBuildDocReport(url: string): DocReport {
  try {
    return buildDocReport(readDoc(url));
  } catch {
    return { title: undefined, ok: false }
  }
}

function isTitleNonEmpty(doc: Doc): boolean | undefined {
  return !!doc.head?.title;
}

function readIfTitleNonEmpty(url: string): boolean | undefined {
  return isTitleNonEmpty(readDoc(url));
}

function main() {
  for (const url of ["good", "empty", "headless", "fail"]) {
    console.log(`Checking "https://${url}/":`)
    console.log("  Report:", readAndBuildDocReport(url));
    try {
      console.log("  Has title:", readIfTitleNonEmpty(url) || false);
    } catch (error) {
      console.log(`  Has title: ${error}`)
    }
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const throwing = undefined;
