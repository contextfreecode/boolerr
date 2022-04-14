#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Unit(());
const UNIT: Unit = Unit(());
impl From<&Unit> for Unit {
    fn from(_: &Unit) -> Unit {
        UNIT
    }
}

type Bool = Result<Unit, Unit>;
const TRUE: Bool = Bool::Ok(UNIT);
const FALSE: Bool = Bool::Err(UNIT);
fn bool_of(condition: bool) -> Bool {
    match condition {
        true => TRUE,
        false => FALSE,
    }
}
fn not(condition: Bool) -> Bool {
    match condition {
        TRUE => FALSE,
        FALSE => TRUE,
    }
}

type Opt<T> = Result<T, Unit>;
fn none<T>() -> Opt<T> {
    Err(UNIT)
}

#[derive(Debug)]
struct Error(String);

struct Doc {
    head: Opt<Box<Head>>,
}

struct Head {
    title: Opt<String>,
}

#[allow(dead_code)] // not really dead because debug printing
#[derive(Debug)]
struct Summary {
    title: Opt<String>,
    ok: Bool,
}

fn read_doc(url: &str) -> Result<Doc, Error> {
    bool_of(url.contains("fail"))
        .map(|_| Err(Error(format!("Bad read of {url}"))))
        .or_else(|_| bool_of(url.contains("head-missing")).map(|_| Ok(Doc { head: none() })))
        .or_else(|_| {
            bool_of(url.contains("title-missing")).map(|_| {
                Ok(Doc {
                    head: Ok(Box::new(Head { title: none() })),
                })
            })
        })
        .or_else(|_| {
            bool_of(url.contains("title-empty")).map(|_| {
                Ok(Doc {
                    head: Ok(Box::new(Head {
                        title: Ok("".into()),
                    })),
                })
            })
        })
        .unwrap_or_else(|_| {
            Ok(Doc {
                head: Ok(Box::new(Head {
                    title: Ok(format!("Title of {url}")),
                })),
            })
        })
}

fn build_summary(doc: Doc) -> Summary {
    Summary {
        title: doc.head.and_then(|it| it.title),
        ok: TRUE,
    }
}

fn read_and_build_summary(url: &str) -> Summary {
    match read_doc(url) {
        Ok(doc) => build_summary(doc),
        Err(_) => Summary {
            title: none(),
            ok: FALSE,
        },
    }
}

fn is_title_non_empty(doc: &Doc) -> Opt<Bool> {
    Ok(not(bool_of(doc.head.as_ref()?.title.as_ref()?.is_empty())))
}

fn read_whether_title_non_empty(url: &str) -> Result<Opt<Bool>, Error> {
    // read_doc(url).map(|doc| is_title_non_empty(&doc))
    Ok(is_title_non_empty(&read_doc(url)?))
}

fn opt_str(text: &str) -> Opt<&str> {
    not(bool_of(text.is_empty()))?; // see also Try trait
    Ok(text)
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
        // Summary.
        let summary = read_and_build_summary(url);
        println!("  Summary: {summary:?}");
        let title_sure = summary.title.unwrap_or_else(|_| "".into());
        println!("  Title: {title_sure:?}");
        // Has title.
        let has_title = read_whether_title_non_empty(url);
        // let has_title_sure = has_title
        //     .as_ref()
        //     .map(|it| it.unwrap_or(FALSE))
        //     .unwrap_or(FALSE);
        let has_title_sure = has_title.as_ref().unwrap_or(&none()).unwrap_or(FALSE);
        println!("  Has title: {has_title:?} vs {has_title_sure:?}");
    }
    println!("Opt: {:?}, {:?}", opt_str(""), opt_str("Bye, y'all."));
}
