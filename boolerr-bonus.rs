#[derive(Clone, Copy, Debug)]
struct Unit(());
const UNIT: Unit = Unit(());

type Bool = Result<Unit, Unit>;
type Opt<T> = Result<T, Unit>;

fn bool_of(condition: bool) -> Bool {
    if condition {
        Ok(UNIT)
    } else {
        Err(UNIT)
    }
}

const TRUE: Bool = Bool::Ok(UNIT);
const FALSE: Bool = Bool::Err(UNIT);
fn none<T>() -> Opt<T> {
    Err(UNIT)
}

impl From<&Unit> for Unit {
    fn from(_: &Unit) -> Unit {
        UNIT
    }
}

struct Doc {
    // TODO Should these all be optional pointers across languages???
    head: Opt<Head>,
}

struct Head {
    title: Opt<String>,
}

#[allow(dead_code)] // not really dead because debug printing
#[derive(Debug)]
struct DocReport {
    title: Opt<String>,
    ok: Bool,
}

fn read_doc(url: &str) -> Result<Doc, String> {
    bool_of(url.contains("fail"))
        .map(|_| Err::<Doc, String>("Failed to read document".into()))
        .or_else(|_| bool_of(url.contains("head-missing")).map(|_| Ok(Doc { head: none() })))
        .or_else(|_| {
            bool_of(url.contains("title-missing")).map(|_| {
                Ok(Doc {
                    head: Ok(Head { title: none() }),
                })
            })
        })
        .or_else(|_| {
            bool_of(url.contains("title-empty")).map(|_| {
                Ok(Doc {
                    head: Ok(Head {
                        title: Ok("".into()),
                    }),
                })
            })
        })
        .unwrap_or_else(|_| {
            Ok(Doc {
                head: Ok(Head {
                    title: Ok(format!("Title of {url}")),
                }),
            })
        })
}

fn build_doc_report(doc: Doc) -> DocReport {
    // return { title: doc.head && doc.head.title, ok: true };
    DocReport {
        title: doc.head.and_then(|it| it.title),
        ok: TRUE,
    }
}

fn read_and_build_doc_report(url: &str) -> DocReport {
    match read_doc(url) {
        Ok(doc) => build_doc_report(doc),
        Err(_) => DocReport {
            title: none(),
            ok: FALSE,
        },
    }
}

fn is_title_non_empty(doc: &Doc) -> Opt<Bool> {
    // TODO How to say `not`?
    Ok(bool_of(doc.head.as_ref()?.title.as_ref()?.is_empty()))
}

fn read_whether_title_non_empty(url: &str) -> Result<Opt<Bool>, String> {
    Ok(is_title_non_empty(&read_doc(url)?))
}

fn main() {
    let urls = [
        "good",
        "title-empty",
        "title-missing",
        "head-missing",
        "fail",
    ];
    for url in urls {
        println!(r#"Checking "https://{}/":"#, url);
        println!("  Report: {:?}", read_and_build_doc_report(url));
        let has_title = read_whether_title_non_empty(url);
        println!(
            "  Has title: {:?} vs {:?}",
            &has_title,
            has_title
                .as_ref()
                // .map(|it| it.unwrap_or(FALSE))
                // .unwrap_or(FALSE),
                .unwrap_or(&Ok(FALSE))
                // .unwrap_or_else(|_| &Ok(FALSE))
                .unwrap_or(FALSE),
        );
    }
}
