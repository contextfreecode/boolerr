type Doc = { head?: Head };
type Head = { title?: string };
type Summary = { title: string | undefined; ok: boolean };

function throwError(message: string): never {
  throw Error(message);
}

function readDoc(url: string): Doc {
  // deno-fmt-ignore
  return (
    url.match("fail") ? throwError(`Bad read of ${url}`) :
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
    // Summary.
    const summary = readAndBuildSummary(url);
    console.log("  Summary:", summary);
    console.log("  Title:", summary.title || ""); // or ??
    // Has title.
    try {
      const hasTitle = readWhetherTitleNonEmpty(url);
      console.log(`  Has title: ${hasTitle} vs ${hasTitle ?? false}`); // or ||
    } catch (error) {
      console.log(`  Has title: ${error} vs ${false}`);
    }
  }
}

main();

// Junk to prevent vscode from whining about duplicate ids.
export const throwing = undefined;
