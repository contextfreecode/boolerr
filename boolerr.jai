#import "Tagged_Union";

// Recommendation from ctp: Option :: struct (T: Type) { value: T = ---; exists: bool }
Maybe :: struct (Value: Type) {
    using Tagged_Union(Value, void);
}
// Maybe :: Tagged_Union($Value, void);

Doc :: struct { head: *Head; }
Head :: struct { title: Maybe(string); }
Summary :: struct {
    title: Maybe(string);
    ok: bool;
}

Err :: struct { message: string; }
Error :: Maybe(Err);

read_doc :: (url: string) -> result: Doc = .{}, err: Error = .{} {
    if strings.contains(url, "fail") {
        err: Err;
        err.message = tprint("Bad read of", url);
        return err = err;
    }
    result: Doc;
    if strings.contains(url, "head-missing") {
        // Nothing more to add.
    } else if strings.contains(url, "title-missing") {
        result.head = New(Head, allocator=__temporary_allocator);
    }
    // result =
    //     ifx strings.contains(url, "head-missing") then
    //         Doc{}
    //     else ifx strings.contains(url, "title-missing") then
    //         Doc{head = new_clone(Head{})}
    //     else ifx strings.contains(url, "title-empty") then
    //         Doc{head = new_clone(Head{title = fmt.aprint("")})}
    //     else
    //         Doc{head = new_clone(Head{title = fmt.aprint("Title of", url)})};
    return result;
}

build_summary :: (doc: Doc) -> Summary {
    result := Summary.{ok = true};
    if (doc.head != null) {
        result.title = doc.head.title;
        // set(*result.title, doc.head.title);
    } else {
        set(*result.title, void);
    }
    return result;
}

read_and_build_summary :: (url: string) -> Summary {
    doc, err := read_doc(url);
    if err != null {
        return .{};
    } else {
        return build_summary(doc);
    };
}

is_title_non_empty :: (doc: Doc) -> Maybe(bool) {
    if doc.head == null || isa(doc.head.title, void) {
        return null;
    }
    // return len(doc.head.title.(string)) > 0;
    return len(doc.head.title) > 0;
}

read_whether_title_non_empty ::
(url: string) -> result: Maybe(bool), err: Error = .{} {
    result: Maybe(bool);
    set(cast(*Tagged_Union(bool, void)) *result, void);
    doc, err := read_doc(url);
    if (isa(err, void)) {
        non_empty := is_title_non_empty(doc);
        if (non_empty != null) {
            set(*result, is_title_non_empty(doc));
        }
    }
    return result = result, err = err;
}

main :: () {
    // Loop.
    urls := string.["good", "title-empty", "title-missing", "head-missing", "fail"];
    for url: urls {
        // Reset arena on each pass.
        defer reset_temporary_storage();
        // Summary.
        print("Checking \"https://%/\":\n", url);
        summary := read_and_build_summary(url);
        print("  Summary: %\n", summary);
        print("  Title: %\n", ifx summary.title summary.title else "");
        // Has title.
        has_title, err := read_whether_title_non_empty(url);
        has_title_text := ifx err != null aprint(err) else aprint(has_title);
        has_title_bool :=
            ifx err != null false else ifx has_title has_title else false;
        print("  Has title:", has_title_text, "vs", has_title_bool);
    }
}
