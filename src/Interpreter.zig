const std = @import("std");
const Parser = @import("Parser.zig");
const Expr = Parser.Expr;
const Literal = Parser.Literal;
const Token = @import("Token.zig").Token;
const Stmt = Parser.Stmt;

const Environment = @import("Environment.zig").Environment;

const runtimeError = @import("main.zig").runtimeError;

pub const Interpreter = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    environment: Environment,
    runtime_error: ?RuntimeErrorInfo = null,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .environment = Environment.init(allocator),
        };
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
                    try self.execute(stmt.if_stmt.else_branch.?);
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
                try self.environment.define(stmt.variable.name.lexeme, value.?);
                return;
            },
            .block => {
                const env = Environment.initEnclosing(
                    self.allocator,
                    &self.environment,
                );
                try self.executeBlock(stmt.block, env);
                return;
            },
        }
    }

    fn executeBlock(self: *Self, statements: []*Stmt, environment: Environment) !void {
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
                    .BANG => Literal{ .boolean = isTruthy(right) },
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
                    .EQUAL_EQUAL => Literal{ .boolean = !std.meta.eql(left, right) },
                    else => unreachable,
                };
            },
            .unary => {
                const right = try self.evaluate(expr.unary.right);

                switch (expr.unary.operator.token_type) {
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
            .variable => try self.environment.get(expr.variable.name),
            .assign => {
                const value = try self.evaluate(expr.assign.value);
                try self.environment.assign(expr.assign.name, value);
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
};

pub const RuntimeError = error{
    OperandsMustBeNumbers,
    OperandsMustBeNumbersOrStrings,
    OutOfMemory,
    UndefinedVariable,
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
    };
}
