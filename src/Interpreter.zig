const std = @import("std");
const Parser = @import("Parser.zig");
const Expr = Parser.Expr;
const Literal = Parser.Literal;
const Token = @import("Token.zig").Token;
const Stmt = Parser.Stmt;
const LoxCallable = @import("LoxCallable.zig").LoxCallable;
const LoxFunction = @import("LoxFunction.zig").LoxFunction;
const LoxClass = @import("LoxClass.zig").LoxClass;

const Environment = @import("Environment.zig").Environment;

const runtimeError = @import("main.zig").runtimeError;

pub const Interpreter = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    runtime_error: ?RuntimeErrorInfo = null,
    return_value: ?Literal = null,

    locals: std.AutoHashMap(*const Expr, usize),

    pub fn init(allocator: std.mem.Allocator) !Self {
        const globals = try Environment.init(allocator);
        var self = Self{
            .allocator = allocator,
            .globals = globals,
            .environment = globals,
            .locals = std.AutoHashMap(*const Expr, usize).init(allocator),
        };

        const clock = LoxCallable{
            .native = .{
                .arity = 0,
                .call = &clockNative,
            },
        };

        try self.globals.define("clock", Literal{ .callable = clock });

        return self;
    }

    pub fn interpret(self: *Self, stmt: std.ArrayList(*Stmt)) RuntimeError!void {
        for (stmt.items) |statement| {
            try self.execute(statement);
        }
    }

    fn execute(self: *Self, stmt: *Stmt) RuntimeError!void {
        switch (stmt.*) {
            .print => {
                const value = self.evaluate(stmt.print) catch |err| {
                    runtimeError(self.runtime_error.?.token, @errorName(err));
                    return;
                };
                std.debug.print("> {s}\n", .{try self.stringify(value)});
            },
            .if_stmt => {
                if (isTruthy(try self.evaluate(stmt.if_stmt.condition))) {
                    try self.execute(stmt.if_stmt.then_branch);
                } else if (stmt.*.if_stmt.else_branch == null) {
                    if (stmt.if_stmt.else_branch) |else_branch| {
                        try self.execute(else_branch);
                    }
                }
                return;
            },
            .while_stmt => {
                while (isTruthy(try self.evaluate(stmt.while_stmt.condition))) {
                    try self.execute(stmt.while_stmt.body);
                }
                return;
            },
            .expression => {
                _ = self.evaluate(stmt.expression) catch |err| {
                    runtimeError(self.runtime_error.?.token, @errorName(err));
                };
            },
            .variable => {
                const value = if (stmt.variable.initializer) |ini|
                    try self.evaluate(ini)
                else
                    null;
                try self.environment.define(stmt.variable.name.lexeme, value orelse .none);
                return;
            },
            .block => {
                const env = try Environment.initEnclosing(
                    self.allocator,
                    self.environment,
                );
                try self.executeBlock(stmt.block, env);
                return;
            },
            .class => {
                try self.environment.define(stmt.class.name.lexeme, .none);
                const new_class = try self.allocator.create(LoxClass);
                new_class.* = LoxClass.init(stmt.class.name.lexeme);
                try self.environment.assign(stmt.class.name, Literal{ .callable = .{ .class = new_class } });
            },
            .function => {
                const new_function = try self.allocator.create(LoxFunction);
                new_function.* = LoxFunction.init(&stmt.function, self.environment);
                try self.environment.define(
                    stmt.function.name.lexeme,
                    Literal{
                        .callable = .{ .function = new_function },
                    },
                );
                return;
            },
            .return_stmt => {
                if (stmt.return_stmt.value != null) {
                    self.return_value = try self.evaluate(stmt.return_stmt.value.?);
                }
                return RuntimeError.Return;
            },
        }
    }

    pub fn executeBlock(self: *Self, statements: []*Stmt, environment: *Environment) !void {
        const previous = self.environment;
        self.environment = environment;
        defer self.environment = previous;
        for (statements) |stmt| {
            try self.execute(stmt);
        }
    }

    fn stringify(self: *Self, literal: Literal) RuntimeError![]const u8 {
        return switch (literal) {
            .none => "nil",
            .number => try std.fmt.allocPrint(self.allocator, "{d}", .{literal.number}),
            .string => literal.string,
            .boolean => if (literal.boolean) "true" else "false",
            .callable => {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "<fn {s}>",
                    .{
                        literal.callable.function.toString(),
                    },
                );
            },
            .class => literal.class.toString(),
            .instance => {
                return try std.fmt.allocPrint(
                    self.allocator,
                    "{s} instance",
                    .{
                        literal.instance.class.toString(),
                    },
                );
            },
        };
    }

    /// Pretty sure that there's a refactoring needed.
    pub fn evaluate(self: *Self, expr: *const Expr) RuntimeError!Literal {
        return switch (expr.*) {
            .literal => return expr.*.literal,
            .binary => {
                const left = try self.evaluate(expr.binary.left);
                const right = try self.evaluate(expr.binary.right);

                return switch (expr.binary.operator.token_type) {
                    .MINUS => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .number = left.number - right.number };
                    },
                    .SLASH => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .number = left.number / right.number };
                    },
                    .STAR => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .number = left.number * right.number };
                    },
                    .PLUS => {
                        return switch (left) {
                            .number => switch (right) {
                                .number => Literal{ .number = left.number + right.number },
                                else => {
                                    self.runtime_error = .{
                                        .token = expr.binary.operator,
                                        .message = "Operands must be two numbers",
                                    };
                                    return RuntimeError.OperandsMustBeNumbersOrStrings;
                                },
                            },
                            .string => switch (right) {
                                .string => Literal{ .string = try std.mem.concat(
                                    self.allocator,
                                    u8,
                                    &.{ left.string, right.string },
                                ) },
                                else => {
                                    self.runtime_error = .{
                                        .token = expr.binary.operator,
                                        .message = "Operands must be two strings",
                                    };
                                    return RuntimeError.OperandsMustBeNumbersOrStrings;
                                },
                            },
                            else => unreachable,
                        };
                    },
                    .GREATER => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .boolean = left.number > right.number };
                    },
                    .GREATER_EQUAL => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .boolean = left.number >= right.number };
                    },
                    .LESS => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .boolean = left.number < right.number };
                    },
                    .LESS_EQUAL => {
                        try self.checkNumberOperands(expr.binary.operator, left, right);
                        return Literal{ .boolean = left.number <= right.number };
                    },
                    .BANG_EQUAL => Literal{ .boolean = !std.meta.eql(left, right) },
                    .EQUAL_EQUAL => Literal{ .boolean = std.meta.eql(left, right) },
                    else => unreachable,
                };
            },
            .caller => {
                const callee = try self.evaluate(expr.caller.callee);
                var arguments = std.ArrayList(Literal).empty;

                for (expr.caller.arguments) |arg| {
                    try arguments.append(self.allocator, try self.evaluate(arg));
                }

                switch (callee) {
                    .callable => |callable| {
                        switch (callable) {
                            .native => |n| {
                                if (arguments.items.len != n.arity) {
                                    const message = try std.fmt.allocPrint(
                                        self.allocator,
                                        "Expected {d} arguments but got {d}",
                                        .{ n.arity, arguments.items.len },
                                    );
                                    runtimeError(expr.caller.paren, message);
                                }
                                return n.call(self, arguments.items);
                            },
                            .function => |f| {
                                if (arguments.items.len != f.arity()) {
                                    const message = try std.fmt.allocPrint(
                                        self.allocator,
                                        "Expected {d} arguments but got {d}",
                                        .{ f.arity(), arguments.items.len },
                                    );
                                    runtimeError(expr.caller.paren, message);
                                }
                                return try f.call(self, arguments.items);
                            },
                            .class => |cls| {
                                return try cls.call(self, arguments.items);
                            },
                        }
                    },
                    else => {
                        runtimeError(expr.caller.paren, "Can only call functions and classes");
                        return RuntimeError.UndefinedVariable;
                    },
                }
            },
            .unary => {
                const right = try self.evaluate(expr.unary.right);

                switch (expr.unary.operator.token_type) {
                    .BANG => return Literal{ .boolean = isTruthy(right) },
                    .MINUS => return Literal{ .number = -right.number },
                    else => unreachable,
                }
            },
            .logical => {
                const left = try self.evaluate(expr.logical.left);

                if (expr.logical.operator.token_type == .OR) {
                    if (isTruthy(left)) return left;
                } else {
                    if (!isTruthy(left)) return left;
                }
                return try self.evaluate(expr.logical.right);
            },
            .grouping => try self.evaluate(expr.grouping.expression),
            // .variable => try self.environment.get(expr.variable.name),
            .variable => try self.lookupVariable(expr.variable.name, expr),
            .assign => {
                const value = try self.evaluate(expr.assign.value);
                // try self.environment.assign(expr.assign.name, value);

                const distance = self.locals.get(expr);
                if (distance) |d| {
                    try self.environment.assignAt(d, expr.assign.name, value);
                } else {
                    try self.globals.assign(expr.assign.name, value);
                }

                return value;
            },
        };
    }

    fn checkNumberOperands(self: *Interpreter, operator: Token, left: Literal, right: Literal) RuntimeError!void {
        if (left == .number and right == .number) return;

        self.runtime_error = .{
            .token = operator,
            .message = "Operands Must be numbers",
        };
        return RuntimeError.OperandsMustBeNumbers;
    }

    pub fn resolve(self: *Interpreter, expr: *const Expr, depth: usize) !void {
        try self.locals.put(expr, depth);
    }

    fn lookupVariable(self: *Self, name: Token, expr: *const Expr) !Literal {
        const distance = self.locals.get(expr);
        if (distance) |d| {
            return self.environment.getAt(d, name.lexeme);
        } else {
            return try self.globals.get(name);
        }
    }
};

pub const RuntimeError = error{
    OperandsMustBeNumbers,
    OperandsMustBeNumbersOrStrings,
    OutOfMemory,
    UndefinedVariable,
    Return,
};

pub const RuntimeErrorInfo = struct {
    token: Token,
    message: []const u8,
};

fn isTruthy(literal: Literal) bool {
    return switch (literal) {
        .none => false,
        .boolean => literal.boolean,
        .number => literal.number != 0,
        .string => literal.string.len != 0,
        else => unreachable,
    };
}

fn clockNative(interpreter: *Interpreter, args: []Literal) Literal {
    _ = interpreter;
    _ = args;
    const ms: f64 = @floatFromInt(std.time.milliTimestamp());
    return Literal{ .number = ms / 1000.0 };
}
