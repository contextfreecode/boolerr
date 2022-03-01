package main

import "core:fmt"
import "core:mem"
import "core:strings"

Doc :: struct {
	head: Maybe(Head),
}

Head :: struct {
    title: Maybe(string),
}

DocReport :: struct {
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

is_title_non_empty :: proc (doc: Doc) -> Maybe(bool) {
    return len(doc.head.? or_return.title.? or_return) > 0
}

read_whether_title_non_empty ::
proc (url: string) -> (result: Maybe(bool), err: Error) {
    result = is_title_non_empty(read_doc(url) or_return)
    return
}

main :: proc() {
    tracking_allocator: mem.Tracking_Allocator
    mem.tracking_allocator_init(&tracking_allocator, context.allocator)
    defer mem.tracking_allocator_destroy(&tracking_allocator)
    context.allocator = mem.tracking_allocator(&tracking_allocator)
    doc, err := read_doc("good")
    fmt.println("Doc:", doc)
}
