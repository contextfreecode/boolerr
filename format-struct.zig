// By InKryption
// Copied from https://zigbin.io/be9a52

const std = @import("std");
const builtin = @import("builtin");

pub fn main() !void {
    const T = struct {
        str1: ?[]const u8,
        str2: [:0]const u8,
    };

    std.debug.print("{}\n", .{fmtStructWithStringFields(T{ .str1 = "foo", .str2 = "bar" })});
    std.debug.print("{}\n", .{T{ .str1 = "foo", .str2 = "bar" }});
}

fn fmtStructWithStringFields(value: anytype) std.fmt.Formatter(FormatStructWithStringFieldsImpl(@TypeOf(value)).formatStructWithStringFields) {
    return .{ .data = value };
}

fn FormatStructWithStringFieldsImpl(comptime T: type) type {
    return struct {
        fn formatStructWithStringFields(
            value: T,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;

            std.debug.assert(std.meta.trait.is(.Struct)(T));
            const field_names = comptime std.meta.fieldNames(T);

            try writer.writeAll("{");
            inline for (field_names) |field_name, i| {
                const field_value = @field(value, field_name);
                const FieldType = @TypeOf(field_value);
                if (comptime std.meta.trait.isZigString(FieldType) or
                    (std.meta.trait.is(.Optional)(FieldType) and std.meta.trait.isZigString(std.meta.Child(FieldType))))
                {
                    try writer.print("." ++ field_name ++ " = {s}", .{field_value});
                } else {
                    try writer.print("." ++ field_name ++ " = {}", .{field_value});
                }

                if ((i + 1) != field_names.len) {
                    try writer.writeAll(", ");
                } else {
                    try writer.writeAll(" }");
                }
            }
        }
    };
}
