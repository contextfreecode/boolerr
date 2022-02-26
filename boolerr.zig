const std = @import("std");
const Allocator = std.mem.Allocator;

const Doc = struct { head: ?Head };
const Head = struct { title: ?[]u8 };
const DocReport = struct {
    title: ?[]u8,
    ok: bool,
};

const Error = error{FailedRead};

fn contains(comptime T: type, haystack: []const T, needle: []const T) bool {
    return std.mem.indexOf(u8, haystack, needle) != null;
}

fn readDoc(allocator: Allocator, url: []const u8) !Doc {
    return if (contains(u8, url, "fail"))
        error.FailedRead
    else if (contains(u8, url, "headless"))
        Doc{ .head = null }
    else if (contains(u8, url, "empty"))
        Doc{ .head = Head{ .title = null } }
    else
        Doc{ .head = Head{ .title = try std.fmt.allocPrint(allocator, "Something", .{}) } };
}

fn buildDocReport(doc: Doc) DocReport {
    return DocReport{
        .title = if (doc.head) |head| head.title else null,
        .ok = true,
    };
}

fn readAndBuildDocReport(allocator: Allocator, url: []const u8) DocReport {
    const doc = readDoc(allocator, url) catch {
        return DocReport{ .title = null, .ok = false };
    };
    return buildDocReport(doc);
}

pub fn main() !void {
    const print = std.io.getStdOut().writer().print;
    const urls = [_][]const u8{ "good", "empty", "headless", "fail" };
    for (urls) |url| {
        // Treat each scrape independently for memory.
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();
        // Scrape.
        try print("Checking \"https://{s}/\":\n", .{url});
        try print("  Report: {}\n", .{readAndBuildDocReport(allocator, url)});
    }
}
