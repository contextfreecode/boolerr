const std = @import("std");
const Allocator = std.mem.Allocator;

const Doc = struct { head: ?Head };
const Head = struct { title: ?[]u8 };
const DocReport = struct {
    title: ?[]u8,
    ok: bool,
    pub fn format(
        self: DocReport,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        // More general purpose option here by InKryption: https://zigbin.io/be9a52
        try writer.writeAll("DocReport{ .title = ");
        try printOptionalString(writer, self.title);
        try writer.print(", .ok = {} }}", .{self.ok});
    }
};

fn printOptionalString(writer: anytype, text: ?[]u8) !void {
    if (text != null) try writer.writeByte('"');
    try writer.print("{s}", .{text});
    if (text != null) try writer.writeByte('"');
}

const Error = error{FailedRead};

fn contains(comptime T: type, haystack: []const T, needle: []const T) bool {
    return std.mem.indexOf(u8, haystack, needle) != null;
}

fn readDoc(allocator: Allocator, url: []const u8) !Doc {
    return if (contains(u8, url, "fail"))
        error.FailedRead
    else if (contains(u8, url, "headless"))
        Doc{ .head = null }
    else if (contains(u8, url, "untitled")) // allocPrint is proxy for real work
        Doc{ .head = Head{ .title = try std.fmt.allocPrint(allocator, "", .{}) } }
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
    const urls = [_][]const u8{ "good", "untitled", "headless", "fail" };
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
