const std = @import("std");
const Parser = @import("Parser.zig");
const Interpreter = @import("Interpreter.zig").Interpreter;
const Environment = @import("Environment.zig").Environment;

const Expr = Parser.Expr;
const Stmt = Parser.Stmt;
const Literal = Parser.Literal;
const FunctionDecl = Parser.FunctionDecl;
const RuntimeError = @import("Interpreter.zig").RuntimeError;

pub const LoxFunction = struct {
    const Self = @This();
    declaration: *FunctionDecl,
    closure: *Environment,

    pub fn init(declaration: *FunctionDecl, closure: *Environment) Self {
        return .{
            .declaration = declaration,
            .closure = closure,
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
                const val = interpreter.return_value orelse .none;
                interpreter.return_value = null;
                return val;
            },
            else => return err,
        };
        return .none;
    }

    pub fn arity(self: *Self) usize {
        return self.declaration.params.len;
    }

    pub fn toString(self: *Self) []const u8 {
        return self.declaration.name.lexeme;
    }
};
