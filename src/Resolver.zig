const std = @import("std");
const Interpreter = @import("Interpreter.zig").Interpreter;

const Parser = @import("Parser.zig");
const Token = @import("Token.zig").Token;
const LoxFunction = @import("LoxFunction.zig").LoxFunction;

const Stmt = Parser.Stmt;
const Expr = Parser.Expr;
const FunctionDecl = Parser.FunctionDecl;

const parseError = @import("main.zig").parseError;
const runtimeError = @import("main.zig").runtimeError;

const FunctionType = enum {
    NONE,
    FUNCTION,
    INITIALIZER,
    METHOD,
};

const ClassType = enum {
    NONE,
    CLASS,
};

pub const Resolver = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    interpreter: *Interpreter,
    scopes: std.ArrayList(std.StringHashMap(bool)),
    current_function: FunctionType = .NONE,
    current_class: ClassType = .NONE,

    pub fn init(interpreter: *Interpreter, allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .scopes = std.ArrayList(std.StringHashMap(bool)).empty,
        };
    }

    pub fn resolveStmts(self: *Self, statements: []*Stmt) !void {
        for (statements) |stmt| {
            try self.resolveStmt(stmt);
        }
    }

    fn resolveStmt(self: *Self, statement: *Stmt) anyerror!void {
        switch (statement.*) {
            .block => {
                try self.beginScope();
                try self.resolveStmts(statement.block);
                self.endScope();
            },
            .class => {
                const enclosing_class = self.current_class;
                self.current_class = .CLASS;

                try self.declare(statement.class.name);
                try self.define(statement.class.name);

                try self.beginScope();
                try self.scopes.items[self.scopes.items.len - 1].put("this", true);

                for (statement.class.methods) |method| {
                    var declaration: FunctionType = .METHOD;
                    if (std.mem.eql(u8, method.name.lexeme, "init")) declaration = .INITIALIZER;
                    try self.resolveFunction(method, declaration);
                }
                self.endScope();

                self.current_class = enclosing_class;
            },
            .expression => {
                try self.resolveExpr(statement.expression);
            },
            .variable => {
                const v = &statement.variable;
                try self.declare(v.name);
                if (v.initializer) |ini| try self.resolveExpr(ini);
                try self.define(v.name);
            },
            .while_stmt => {
                try self.resolveExpr(statement.while_stmt.condition);
                try self.resolveStmt(statement.while_stmt.body);
            },
            .return_stmt => {
                if (self.current_function == .NONE) {
                    parseError(
                        statement.return_stmt.keyword,
                        "Can't return from top-level code.",
                    );
                }
                if (statement.return_stmt.value) |expr| {
                    if (self.current_function == .INITIALIZER) {
                        runtimeError(
                            statement.return_stmt.keyword,
                            "Can't return a value from an initializer.",
                        );
                    }
                    try self.resolveExpr(expr);
                }
                return;
            },
            .function => {
                try self.declare(statement.function.name);
                try self.define(statement.function.name);
                try self.resolveFunction(statement.function, .FUNCTION);
            },
            .if_stmt => {
                const i = &statement.if_stmt;
                try self.resolveExpr(i.condition);
                try self.resolveStmt(i.then_branch);
                if (i.else_branch) |stmt| {
                    try self.resolveStmt(stmt);
                }
            },
            .print => {
                try self.resolveExpr(statement.print);
            },
        }
    }

    fn resolveExpr(self: *Self, expr: *const Expr) !void {
        switch (expr.*) {
            .binary => {
                try self.resolveExpr(expr.binary.left);
                try self.resolveExpr(expr.binary.right);
            },
            .variable => {
                const e = &expr.variable;

                if (self.scopes.items.len != 0 and
                    self.scopes.items[self.scopes.items.len - 1].get(e.name.lexeme) == false)
                {
                    parseError(e.name, "Can't read local variable in its own initializer.");
                }
                try self.resolveLocal(expr, e.name);
            },
            .assign => {
                const a = &expr.assign;
                try self.resolveExpr(a.value);
                try self.resolveLocal(expr, a.name);
            },
            .caller => {
                try self.resolveExpr(expr.caller.callee);

                for (expr.caller.arguments) |arg| {
                    try self.resolveExpr(arg);
                }
            },
            .this => {
                if (self.current_class == .NONE) runtimeError(
                    expr.this.keyword,
                    "Can't use 'this' outside of a class",
                );
                try self.resolveLocal(expr, expr.this.keyword);
            },
            .get => {
                try self.resolveExpr(expr.get.object);
            },
            .set => {
                try self.resolveExpr(expr.set.value);
                try self.resolveExpr(expr.set.object);
            },
            .grouping => {
                try self.resolveExpr(expr.grouping.expression);
            },
            .literal => {},
            .logical => {
                try self.resolveExpr(expr.logical.left);
                try self.resolveExpr(expr.logical.right);
            },
            .unary => {
                try self.resolveExpr(expr.unary.right);
            },
        }
    }

    fn beginScope(self: *Self) !void {
        try self.scopes.append(
            self.allocator,
            std.StringHashMap(bool).init(self.allocator),
        );
    }

    fn endScope(self: *Self) void {
        _ = self.scopes.pop();
    }

    fn declare(self: *Self, name: Token) !void {
        if (self.scopes.items.len == 0) return;
        var scope = &self.scopes.items[self.scopes.items.len - 1];

        if (scope.contains(name.lexeme)) {
            parseError(name, "Already a variable with this name in this scope.");
        }
        try scope.put(name.lexeme, false);
    }

    fn define(self: *Self, name: Token) !void {
        if (self.scopes.items.len == 0) return;
        try self.scopes.items[self.scopes.items.len - 1].put(name.lexeme, true);
    }

    fn resolveLocal(self: *Self, expr: *const Expr, name: Token) !void {
        var i = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            if (self.scopes.items[i].contains(name.lexeme)) {
                try self.interpreter.resolve(expr, self.scopes.items.len - 1 - i);
                return;
            }
        }
    }

    fn resolveFunction(self: *Self, function: FunctionDecl, f_type: FunctionType) !void {
        const enclosing_function = self.current_function;
        self.current_function = f_type;
        try self.beginScope();
        for (function.params) |param| {
            try self.declare(param);
            try self.define(param);
        }
        try self.resolveStmts(function.body);
        self.endScope();
        self.current_function = enclosing_function;
    }
};
