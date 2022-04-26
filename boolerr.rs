#[derive(Debug)]
struct Error(String);

struct Doc {
    head: Option<Box<Head>>,
}

struct Head {
    title: Option<String>,
}

#[allow(dead_code)] // not really dead because debug printing
#[derive(Debug)]
struct Summary {
    title: Option<String>,
    ok: bool,
}

fn read_doc(url: &str) -> Result<Doc, Error> {
    match () {
        _ if url.contains("fail") => Err(Error(format!("Bad read of {url}"))),
        _ => Ok(match () {
            _ if url.contains("head-missing") => Doc { head: None },
            _ if url.contains("title-missing") => Doc {
                head: Some(Box::new(Head { title: None })),
            },
            _ if url.contains("title-empty") => Doc {
                head: Some(Box::new(Head {
                    title: Some("".into()),
                })),
            },
            _ => Doc {
                head: Some(Box::new(Head {
                    title: Some(format!("Title of {url}")),
                })),
            },
        }),
    }
}

fn build_summary(doc: Doc) -> Summary {
    Summary {
        title: doc.head.and_then(|it| it.title),
        ok: true,
    }
}

fn read_and_build_summary(url: &str) -> Summary {
    match read_doc(url) {
        Ok(doc) => build_summary(doc),
        Err(_) => Summary {
            title: None,
            ok: false,
        },
    }
}

fn is_title_non_empty(doc: &Doc) -> Option<bool> {
    // doc.head
    //     .as_ref()
    //     .and_then(|head| head.title.as_ref())
    //     .map(|title| !title.is_empty())
    Some(!doc.head.as_ref()?.title.as_ref()?.is_empty())
}

fn read_whether_title_non_empty(url: &str) -> Result<Option<bool>, Error> {
    // read_doc(url).map(|doc| is_title_non_empty(&doc))
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
        // Summary.
        let summary = read_and_build_summary(url);
        println!("  Summary: {summary:?}");
        let title_sure = summary.title.unwrap_or_else(|| "".into());
        println!("  Title: {title_sure:?}");
        // Has title.
        let has_title = read_whether_title_non_empty(url);
        // let has_title_sure = has_title
        //     .as_ref()
        //     .map(|it| it.unwrap_or(false))
        //     .unwrap_or(false);
        let has_title_sure = has_title.as_ref().unwrap_or(&None).unwrap_or(false);
        println!("  Has title: {has_title:?} vs {has_title_sure:?}");
    }
}
