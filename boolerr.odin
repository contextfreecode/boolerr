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

Summary :: struct {
    title: Maybe(string),
    ok: bool,
}

Err :: struct {message: string}
Error :: Maybe(Err)

read_doc :: proc(url: string) -> (result: Doc, err: Error) {
    if strings.contains(url, "fail") {
        err = Err{message = fmt.aprint("Bad read of", url)}
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

build_summary :: proc(doc: Doc) -> Summary {
    return Summary{
        title = doc.head.?.title if doc.head != nil else nil,
        ok = true,
    }
}

read_and_build_summary :: proc(url: string) -> Summary {
    if doc, err := read_doc(url); err != nil {
        return Summary{}
    } else {
        return build_summary(doc)
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
        // Summary.
        fmt.printf("Checking \"https://%v/\":\n", url)
        summary := read_and_build_summary(url)
        fmt.println("  Summary:", summary)
        fmt.println("  Title:", summary.title.? or_else "")
        // Has title.
        if has_title, err := read_whether_title_non_empty(url); err != nil {
            fmt.println("  Has title:", err, "vs", false)
        } else {
            fmt.println("  Has title:", has_title, "vs", has_title.? or_else false)
        }
    }
}
