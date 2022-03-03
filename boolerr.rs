struct Doc {
    // TODO Should these all be optional pointers across languages???
    head: Option<Head>,
}

struct Head {
    title: Option<String>,
}

#[allow(dead_code)] // not really dead because debug printing
#[derive(Debug)]
struct DocReport {
    title: Option<String>,
    ok: bool,
}

fn read_doc(url: &str) -> Result<Doc, String> {
    match () {
        _ if url.contains("fail") => Err("Failed to read document".into()),
        _ => Ok(match () {
            _ if url.contains("head-missing") => Doc { head: None },
            _ if url.contains("title-missing") => Doc {
                head: Some(Head { title: None }),
            },
            _ if url.contains("title-empty") => Doc {
                head: Some(Head {
                    title: Some("".into()),
                }),
            },
            _ => Doc {
                head: Some(Head {
                    title: Some(format!("Title of {url}")),
                }),
            },
        }),
    }
}

fn build_doc_report(doc: Doc) -> DocReport {
    // return { title: doc.head && doc.head.title, ok: true };
    DocReport {
        title: doc.head.and_then(|it| it.title),
        ok: true,
    }
}

fn read_and_build_doc_report(url: &str) -> DocReport {
    match read_doc(url) {
        Ok(doc) => build_doc_report(doc),
        Err(_) => DocReport {
            title: None,
            ok: false,
        },
    }
}

fn is_title_non_empty(doc: &Doc) -> Option<bool> {
    Some(!doc.head.as_ref()?.title.as_ref()?.is_empty())
}

fn read_whether_title_non_empty(url: &str) -> Result<Option<bool>, String> {
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
                // .map(|it| it.unwrap_or(false))
                // .unwrap_or(false),
                .unwrap_or(&Some(false))
                // .unwrap_or_else(|_| &Some(false))
                .unwrap_or(false),
        );
    }
}
