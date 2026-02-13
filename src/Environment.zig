const std = @import("std");
const Literal = @import("Parser.zig").Literal;
const Token = @import("Token.zig").Token;

const runtimeError = @import("main.zig").runtimeError;

const RuntimeError = @import("Interpreter.zig").RuntimeError;

pub const Environment = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    values: std.StringHashMap(Literal),

    enclosing: ?*Environment,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .values = std.StringHashMap(Literal).init(allocator),
            .enclosing = null,
        };
    }

    pub fn initEnclosing(allocator: std.mem.Allocator, enclosing: *Environment) Self {
        return .{
            .allocator = allocator,
            .values = std.StringHashMap(Literal).init(allocator),
            .enclosing = enclosing,
        };
    }

    pub fn define(self: *Self, name: []const u8, value: Literal) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Self, name: Token) !Literal {
        return self.values.get(name.lexeme) orelse {
            const message = try std.fmt.allocPrint(
                self.allocator,
                "Undefined variable '{s}.'",
                .{name.lexeme},
            );

            if (self.enclosing != null) return try self.enclosing.?.get(name);

            runtimeError(name, message);
            return RuntimeError.UndefinedVariable;
        };
    }

    pub fn assign(self: *Self, name: Token, value: Literal) !void {
        if (self.values.contains(name.lexeme)) {
            try self.values.put(name.lexeme, value);
            return;
        } else {
            const message = try std.fmt.allocPrint(
                self.allocator,
                "Undefined variable '{s}.'",
                .{name.lexeme},
            );

            if (self.enclosing != null) {
                try self.enclosing.?.assign(name, value);
                return;
            }
            runtimeError(name, message);
            return RuntimeError.OutOfMemory;
        }
    }
};
