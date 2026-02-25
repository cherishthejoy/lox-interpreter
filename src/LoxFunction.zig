const std = @import("std");
const Parser = @import("Parser.zig");
const Interpreter = @import("Interpreter.zig").Interpreter;
const Environment = @import("Environment.zig").Environment;

const Expr = Parser.Expr;
const Stmt = Parser.Stmt;
const Literal = Parser.Literal;
const FunctionDecl = Parser.FunctionDecl;
const RuntimeError = @import("Interpreter.zig").RuntimeError;

const LoxInstance = @import("LoxInstance.zig").LoxInstance;

pub const LoxFunction = struct {
    const Self = @This();
    declaration: *FunctionDecl,
    closure: *Environment,
    isInitializer: bool,

    pub fn init(declaration: *FunctionDecl, closure: *Environment, isInitializer: bool) Self {
        return .{
            .declaration = declaration,
            .closure = closure,
            .isInitializer = isInitializer,
        };
    }

    pub fn call(self: *Self, interpreter: *Interpreter, arguments: []Literal) !Literal {
        const environment = try Environment.initEnclosing(
            interpreter.allocator,
            self.closure,
        );

        for (0..self.declaration.params.len) |i| {
            try environment.define(self.declaration.params[i].lexeme, arguments[i]);
        }

        interpreter.executeBlock(self.declaration.body, environment) catch |err| switch (err) {
            RuntimeError.Return => {
                if (self.isInitializer) return try self.closure.getAt(0, "this");
                const val = interpreter.return_value orelse .none;
                interpreter.return_value = null;
                return val;
            },
            else => return err,
        };
        if (self.isInitializer) return try self.closure.getAt(0, "this");
        return .none;
    }

    pub fn bind(self: *Self, instance: *LoxInstance) !*LoxFunction {
        const environment = try Environment.initEnclosing(
            self.closure.allocator,
            self.closure,
        );

        const wrap = Literal{ .instance = instance };
        try environment.define("this", wrap);

        const new_func = try self.closure.allocator.create(LoxFunction);
        new_func.* = LoxFunction.init(self.declaration, environment, self.isInitializer);
        return new_func;
    }

    pub fn arity(self: *Self) usize {
        return self.declaration.params.len;
    }

    pub fn toString(self: *Self) []const u8 {
        return self.declaration.name.lexeme;
    }
};
