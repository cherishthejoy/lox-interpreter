const std = @import("std");

const Interpreter = @import("Interpreter.zig").Interpreter;
const LoxInstance = @import("LoxInstance.zig").LoxInstance;
const LoxFunction = @import("LoxFunction.zig").LoxFunction;
const Parser = @import("Parser.zig");
const Literal = Parser.Literal;

pub const LoxClass = struct {
    const Self = @This();
    name: []const u8,
    methods: std.StringHashMap(*LoxFunction),

    pub fn init(name: []const u8, methods: std.StringHashMap(*LoxFunction)) Self {
        return .{ .name = name, .methods = methods };
    }

    pub fn call(self: *Self, interpreter: *Interpreter, arguments: []Literal) !Literal {
        const new_instance = try interpreter.allocator.create(LoxInstance);
        new_instance.* = LoxInstance.init(self, interpreter.allocator);

        if (self.findMethod("init")) |initializer| {
            const bound = try initializer.bind(new_instance);
            _ = try bound.call(interpreter, arguments);
        }

        return Literal{ .instance = new_instance };
    }

    pub fn arity(self: *Self) usize {
        if (self.findMethod("init")) |initializer| {
            initializer.arity();
        }
        return 0;
    }

    pub fn findMethod(self: *Self, name: []const u8) ?*LoxFunction {
        if (self.methods.contains(name)) {
            return self.methods.get(name).?;
        }
        return null;
    }

    pub fn toString(self: *Self) []const u8 {
        return self.name;
    }
};
