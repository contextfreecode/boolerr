package main

import "core:fmt"
import "core:mem"
import "core:mem/virtual"
import "core:strings"

Doc :: struct {
	head: Maybe(Head),
}

Head :: struct {
    title: Maybe(string),
}

Doc_Report :: struct {
    title: Maybe(string),
    ok: bool,
}

Error :: enum {
    None,
    FailedRead,
}

read_doc :: proc(url: string) -> (result: Doc, err: Error) {
    if strings.contains(url, "fail") {
        err = .FailedRead
        return
    }
    result =
        Doc{} if strings.contains(url, "head-missing") else
        Doc{head = Head{}} if strings.contains(url, "title-missing") else
        Doc{head = Head{title = fmt.aprint("")}} if
            strings.contains(url, "title-empty") else
        Doc{head = Head{title = fmt.aprint("Title of", url)}}
    return
}

build_doc_report :: proc(doc: Doc) -> Doc_Report {
    return Doc_Report{
        title = doc.head.?.title if doc.head != nil else nil,
        ok = true,
    };
}

read_and_build_doc_report :: proc(url: string) -> Doc_Report {
    if doc, err := read_doc(url); err != nil {
        return Doc_Report{}
    } else {
        return build_doc_report(doc)
    }
}

is_title_non_empty :: proc(doc: Doc) -> Maybe(bool) {
    if doc.head == nil || doc.head.?.title == nil {
        return nil
    }
    // return len(doc.head.(Head).title.(string)) > 0
    return len(doc.head.?.title.?) > 0
}

read_whether_title_non_empty ::
proc(url: string) -> (result: Maybe(bool), err: Error) {
    result = is_title_non_empty(read_doc(url) or_return)
    return
}

main :: proc() {
    // Prep arena.
    arena: virtual.Growing_Arena
    defer virtual.growing_arena_destroy(&arena)
    context.allocator = virtual.growing_arena_allocator(&arena)
    // Loop.
    urls := []string{"good", "title-empty", "title-missing", "head-missing", "fail"}
    for url in urls {
        // Reset arena.
        temp := virtual.growing_arena_temp_begin(&arena)
        defer virtual.growing_arena_temp_end(temp)
        // Scrape.
        fmt.printf("Checking \"https://%v/\":\n", url)
        fmt.println("  Report:", read_and_build_doc_report(url))
        if has_title, err := read_whether_title_non_empty(url); err != nil {
            fmt.println("  Has title:", err)
        } else {
            fmt.println("  Has title:", has_title, "vs", has_title.? or_else false)
        }
    }
}
