const std = @import("std");

const Interpreter = @import("Interpreter.zig").Interpreter;
const LoxInstance = @import("LoxInstance.zig").LoxInstance;
const Parser = @import("Parser.zig");
const Literal = Parser.Literal;

pub const LoxClass = struct {
    const Self = @This();
    name: []const u8,

    pub fn init(name: []const u8) Self {
        return .{ .name = name };
    }

    pub fn call(self: *Self, interpreter: *Interpreter, arguments: []Literal) !Literal {
        const new_instance = try interpreter.allocator.create(LoxInstance);
        _ = arguments;
        new_instance.* = LoxInstance.init(self);
        return Literal{ .instance = new_instance };
    }

    pub fn arity(self: *Self) usize {
        _ = self;
        return 0;
    }

    pub fn toString(self: *Self) []const u8 {
        return self.name;
    }
};
