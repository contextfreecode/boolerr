struct Doc {
    head: Option<Head>,
}

struct Head {
    title: Option<String>,
}

#[derive(Debug)]
struct DocReport {
    title: Option<String>,
    ok: bool,
}

fn read_doc(url: &str) -> Result<Doc, String> {
    match () {
        _ if url.contains("fail") => Err("Failed to read document".into()),
        _ if url.contains("headless") => Ok(Doc { head: None }),
        _ if url.contains("empty") => Ok(Doc {
            head: Some(Head { title: None }),
        }),
        _ => Ok(Doc {
            head: Some(Head {
                title: Some("Something".into()),
            }),
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

fn read_if_title_non_empty(url: &str) -> Result<Option<bool>, String> {
    Ok(is_title_non_empty(&read_doc(url)?))
}

fn main() {
    for url in ["good", "empty", "headless", "fail"] {
        println!(r#"Checking "https://{}/":"#, url);
        println!("  Report: {:?}", read_and_build_doc_report(url));
        let has_title = read_if_title_non_empty(url);
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
