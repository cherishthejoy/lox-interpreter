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

    pub fn init(allocator: std.mem.Allocator) !*Self {
        const self = try allocator.create(Environment);
        self.* = .{
            .allocator = allocator,
            .values = std.StringHashMap(Literal).init(allocator),
            .enclosing = null,
        };
        return self;
    }

    pub fn initEnclosing(allocator: std.mem.Allocator, enclosing: *Environment) !*Self {
        const self = try allocator.create(Environment);
        self.* = .{
            .allocator = allocator,
            .values = std.StringHashMap(Literal).init(allocator),
            .enclosing = enclosing,
        };
        return self;
    }

    pub fn define(self: *Self, name: []const u8, value: Literal) !void {
        try self.values.put(name, value);
    }

    pub fn get(self: *Self, name: Token) !Literal {
        if (self.values.get(name.lexeme)) |value| return value;
        if (self.enclosing) |enclosing| return try enclosing.get(name);
        const message = try std.fmt.allocPrint(
            self.allocator,
            "Undefined variable '{s}'.",
            .{name.lexeme},
        );
        runtimeError(name, message);
        return RuntimeError.UndefinedVariable;
    }

    pub fn getAt(self: *Self, distance: usize, name: []const u8) !Literal {
        return self.ancestor(distance).values.get(name).?;
    }

    fn ancestor(self: *Self, distance: usize) *Environment {
        var environment = self;
        for (0..distance) |_| {
            environment = environment.enclosing.?;
        }
        return environment;
    }

    pub fn assign(self: *Self, name: Token, value: Literal) !void {
        if (self.values.contains(name.lexeme)) {
            try self.values.put(name.lexeme, value);
            return;
        }
        if (self.enclosing) |enclosing| {
            try enclosing.assign(name, value);
            return;
        }
        const message = try std.fmt.allocPrint(
            self.allocator,
            "Undefined variable '{s}'.",
            .{name.lexeme},
        );
        runtimeError(name, message);
        return RuntimeError.UndefinedVariable;
    }

    pub fn assignAt(self: *Self, distance: usize, name: Token, value: Literal) !void {
        try self.ancestor(distance).values.put(name.lexeme, value);
    }
};
