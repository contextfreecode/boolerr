const std = @import("std");

const Doc = struct { head: ?Head };

const Head = struct { title: ?[]u8 };

const Error = error{FailedRead};

fn contains(comptime T: type, haystack: []const T, needle: []const T) bool {
    return std.mem.indexOf(u8, haystack, needle) != null;
}

fn readDoc(url: []const u8) !Doc {
    return if (contains(u8, url, "fail"))
        error.FailedRead
    else if (contains(u8, url, "headless"))
        Doc{ .head = null }
    else if (contains(u8, url, "empty"))
        Doc{ .head = Head{ .title = null } }
    else
        Doc{ .head = null };
}

pub fn main() void {
    const urls = [_][]const u8{ "good", "empty", "headless", "fail" };
    for (urls) |url| {
        _ = readDoc(url) catch void;
    }
}
