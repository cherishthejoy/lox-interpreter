const std = @import("std");
const Literal = @import("Parser.zig").Literal;
const Token = @import("Token.zig").Token;

const runtimeError = @import("main.zig").runtimeError;

pub const Environment = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    values: std.StringHashMap(Literal),

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .values = std.StringHashMap(Literal).init(allocator),
        };
    }

    pub fn define(self: *Self, name: []const u8, value: Literal) void {
        self.values.put(name, value);
    }

    pub fn get(self: *Self, name: Token) Literal {
        const message = std.fmt.allocPrint(
            self.allocator,
            "Undefined variable '{s}.'",
            .{name.lexeme},
        );
        return self.values.get(name.lexeme) orelse {
            runtimeError(name, message);
        };
    }
};
