const std = @import("std");
const Parser = @import("Parser.zig");
const Interpreter = @import("Interpreter.zig").Interpreter;
const Environment = @import("Environment.zig").Environment;

const Expr = Parser.Expr;
const Stmt = Parser.Stmt;
const Literal = Parser.Literal;
const FunctionDecl = Parser.FunctionDecl;

pub const LoxFunction = struct {
    const Self = @This();
    declaration: *FunctionDecl,

    pub fn init(declaration: *FunctionDecl) Self {
        return .{ .declaration = declaration };
    }

    pub fn call(self: *Self, interpreter: *Interpreter, arguments: []Literal) !Literal {
        const environment = try Environment.initEnclosing(
            interpreter.allocator,
            interpreter.globals,
        );

        for (0..self.declaration.params.len) |i| {
            try environment.define(self.declaration.params[i].lexeme, arguments[i]);
        }
        try interpreter.executeBlock(self.declaration.body, environment);
        return .none;
    }

    pub fn arity(self: *Self) usize {
        return self.declaration.params.len;
    }
};
