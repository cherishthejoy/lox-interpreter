const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const parseError = @import("main.zig").parseError;

pub const Stmt = union(enum) {
    expression: *Expr,
    if_stmt: struct { condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt },
    print: *Expr,
    variable: struct { name: Token, initializer: ?*Expr },
    block: []*Stmt,
};

/// Expression
///   -> Equality (!=, ==)
///     -> Comparisson (>, >=, <, <=)
///       -> Term (-, +)
///         -> Factor (/, *)
///           -> Unary (!, -)
///             -> Primary (Number, string, true, false, nil)
///
/// Parser has 2 jobs.
/// Given a sequence of tokens, produce a corresponding syntax tree.
/// Given an invalid sequence of tokens, detect any errors and tell
/// the user about their mistaes
pub const Parser = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),
    current: usize = 0,

    pub fn init(tokens: std.ArrayList(Token), allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .tokens = tokens,
        };
    }

    fn deinitStmt(self: *Parser, stmt: *Stmt) void {
        switch (stmt.*) {
            .block => |stmts| self.allocator.free(stmts),
            else => {},
        }
    }

    pub fn parse(self: *Self) !std.ArrayList(*Stmt) {
        var stmt = try std.ArrayList(*Stmt).initCapacity(self.allocator, 10);
        while (!self.isAtEnd()) {
            try stmt.append(self.allocator, try self.declaration());
        }
        return stmt;
    }

    fn declaration(self: *Self) !*Stmt {
        if (self.match(&[_]TokenType{.VAR})) return self.varDeclaration() catch |err| {
            self.synchronize();
            // parseError(err);
            return err;
        };
        return try self.statement();
    }

    fn varDeclaration(self: *Self) !*Stmt {
        const name = try self.consume(.IDENTIFIER, "Expect variable name.");

        const ini = if (self.match(&[_]TokenType{.EQUAL}))
            try self.expression()
        else
            null;
        _ = try self.consume(.SEMICOLON, "Expect ';' after variable declaration.");
        const new_stmt = try self.allocator.create(Stmt);
        new_stmt.* = Stmt{
            .variable = .{
                .name = name,
                .initializer = ini,
            },
        };

        return new_stmt;
    }

    fn assignment(self: *Self) !*Expr {
        const expr = try self.myOr();

        if (self.match(&[_]TokenType{.EQUAL})) {
            const equals = self.previous();
            const value = try self.assignment();

            switch (expr.*) {
                .variable => {
                    const name = expr.variable.name;
                    const new_expr = try self.allocator.create(Expr);
                    new_expr.* = Expr{
                        .assign = .{
                            .name = name,
                            .value = value,
                        },
                    };
                    return new_expr;
                },
                else => unreachable,
            }

            returnError(equals, "Invalid assignment target.");
        }
        return expr;
    }

    fn statement(self: *Self) !*Stmt {
        if (self.match(&[_]TokenType{.IF})) return try self.ifStatement();
        if (self.match(&[_]TokenType{.PRINT})) return try self.printStatement();
        if (self.match(&[_]TokenType{.LEFT_BRACE})) {
            const body = try self.block();
            const new_stmt = try self.allocator.create(Stmt);
            new_stmt.* = Stmt{ .block = body };
            return new_stmt;
        }
        return try self.expressionStatement();
    }

    fn ifStatement(self: *Self) ParseError!*Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after if condition.");

        const then_branch = try self.statement();
        var else_branch: ?*Stmt = null;
        if (self.match(&[_]TokenType{.ELSE})) {
            else_branch = try self.statement();
        }
        const new_stmt = try self.allocator.create(Stmt);

        new_stmt.* = Stmt{
            .if_stmt = .{
                .condition = condition,
                .then_branch = then_branch,
                .else_branch = else_branch,
            },
        };

        return new_stmt;
    }

    fn myOr(self: *Self) !*Expr {
        var expr = try self.myAnd();

        while (self.match(&[_]TokenType{.OR})) {
            const operator = self.previous();
            const right = try self.myAnd();
            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{
                .logical = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn myAnd(self: *Self) !*Expr {
        var expr = try self.equality();

        while (self.match(&[_]TokenType{.AND})) {
            const operator = self.previous();
            const right = try self.equality();
            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{
                .logical = .{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn printStatement(self: *Self) !*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after value.");
        const new_stmt = try self.allocator.create(Stmt);
        new_stmt.* = Stmt{ .print = expr };
        return new_stmt;
    }

    fn expressionStatement(self: *Self) !*Stmt {
        const expr = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after value.");
        const new_stmt = try self.allocator.create(Stmt);
        new_stmt.* = Stmt{ .expression = expr };
        return new_stmt;
    }

    fn block(self: *Self) ParseError![]*Stmt {
        var statements = try std.ArrayList(*Stmt).initCapacity(self.allocator, 10);
        while (!self.check(.RIGHT_BRACE) and !self.isAtEnd()) {
            try statements.append(self.allocator, try self.declaration());
        }
        _ = try self.consume(.RIGHT_BRACE, "Expect '}' after block");
        return try statements.toOwnedSlice(self.allocator);
    }

    fn expression(self: *Self) ParseError!*Expr {
        return try self.assignment();
    }

    fn equality(self: *Self) ParseError!*Expr {
        var expr = try self.comparison();

        while (self.match(&[_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            const operator = self.previous();
            const right = try self.comparison();
            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{
                .binary = Binary{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn comparison(self: *Self) ParseError!*Expr {
        var expr = try self.term();
        while (self.match(&[_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            const operator = self.previous();
            const right = try self.term();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{
                .binary = Binary{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }

        return expr;
    }

    /// Check if any of the given token types matches the
    /// current token type. Believe it or not. This thing
    /// consumes!
    fn match(self: *Self, types: []const TokenType) bool {
        for (types) |ttype| {
            if (self.check(ttype)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    /// Check if the current token type is the same as the
    /// argument token type.
    fn check(self: *Self, ttype: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().token_type == ttype;
    }

    /// Advance the current index.
    /// Otherwise return the previous.
    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return self.previous();
    }

    fn isAtEnd(self: *Self) bool {
        return self.peek().token_type == .EOF;
    }

    fn peek(self: *Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: *Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn term(self: *Self) ParseError!*Expr {
        var expr = try self.factor();

        while (self.match(&[_]TokenType{ .MINUS, .PLUS })) {
            const operator = self.previous();
            const right = try self.factor();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{
                .binary = Binary{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn factor(self: *Self) ParseError!*Expr {
        var expr = try self.unary();

        while (self.match(&[_]TokenType{ .SLASH, .STAR })) {
            const operator = self.previous();
            const right = try self.factor();
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{
                .binary = Binary{
                    .left = expr,
                    .operator = operator,
                    .right = right,
                },
            };
            expr = new_expr;
        }
        return expr;
    }

    fn unary(self: *Self) ParseError!*Expr {
        if (self.match(&[_]TokenType{ .BANG, .MINUS })) {
            const operator = self.previous();
            const right = try self.unary();
            const new_expr = try self.allocator.create(Expr);

            new_expr.* = Expr{
                .unary = Unary{ .operator = operator, .right = right },
            };
            return new_expr;
        }
        return self.primary();
    }

    fn primary(self: *Self) ParseError!*Expr {
        if (self.match(&[_]TokenType{.FALSE})) return self.literal(Literal{ .boolean = false });
        if (self.match(&[_]TokenType{.TRUE})) return self.literal(Literal{ .boolean = true });
        if (self.match(&[_]TokenType{.NIL})) return self.literal(Literal.none);

        if (self.match(&[_]TokenType{ .NUMBER, .STRING })) {
            return self.literal(self.previous().literal);
        }

        if (self.match(&[_]TokenType{.IDENTIFIER})) {
            const new_expr = try self.allocator.create(Expr);
            new_expr.* = Expr{ .variable = .{ .name = self.previous() } };
            return new_expr;
        }

        if (self.match(&[_]TokenType{.LEFT_PAREN})) {
            var expr = try self.expression();
            const new_expr = try self.allocator.create(Expr);

            _ = try self.consume(.RIGHT_PAREN, "Expect ')' after expression");
            new_expr.* = Expr{ .grouping = Grouping{ .expression = expr } };
            expr = new_expr;
            return expr;
        }
        return returnError(self.peek(), "Expect expression.");
    }

    fn consume(self: *Self, ttype: TokenType, message: []const u8) ParseError!Token {
        if (self.check(ttype)) return self.advance();
        return returnError(self.peek(), message);
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().token_type == .SEMICOLON) return;

            switch (self.peek().token_type) {
                .CLASS => {},
                .FUN => {},
                .VAR => {},
                .FOR => {},
                .IF => {},
                .WHILE => {},
                .PRINT => {},
                .RETURN => return,
                else => return,
            }
            _ = self.advance();
        }
    }

    // Returns the pointer of a literal expression.
    fn literal(self: *Self, l: Literal) *Expr {
        const e = self.allocator.create(Expr) catch unreachable;
        e.* = Expr{ .literal = l };
        return e;
    }
};

pub const Expr = union(enum) {
    binary: Binary,
    literal: Literal,
    logical: Logical,
    grouping: Grouping,
    unary: Unary,
    variable: Variable,
    assign: Assign,

    pub fn print(self: *const Expr) void {
        switch (self.*) {
            .binary => |b| {
                parenthesize(b.operator.lexeme, &[_]*const Expr{ b.left, b.right });
            },
            .grouping => |g| {
                parenthesize("group", &[_]*const Expr{g.expression});
            },
            .literal => |l| {
                switch (l) {
                    .number => std.debug.print("{d}", .{l.number}),
                    .boolean => std.debug.print("{}", .{l.boolean}),
                    .string => std.debug.print("{s}", .{l.string}),
                    .none => std.debug.print("nil", .{}),
                }
            },
            .unary => |u| {
                parenthesize(u.operator.lexeme, &[_]*const Expr{u.right});
            },
        }
    }
};

const Assign = struct {
    name: Token,
    value: *const Expr,
};

const Binary = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

pub const Literal = union(enum) {
    none,
    number: f64,
    string: []const u8,
    boolean: bool,
};

const Logical = struct {
    left: *const Expr,
    operator: Token,
    right: *const Expr,
};

const Grouping = struct {
    expression: *const Expr,
};

const Unary = struct {
    operator: Token,
    right: *const Expr,
};

const Variable = struct {
    name: Token,
};

const ParseError = error{
    ParseError,
    OutOfMemory,
};

pub fn unary(operator: Token, right: *const Expr) Expr {
    return Expr{ .unary = Unary{ .operator = operator, .right = right } };
}

fn parenthesize(name: []const u8, exprs: []const *const Expr) void {
    std.debug.print("{s}", .{"("});
    std.debug.print("{s}", .{name});

    for (exprs) |expr| {
        std.debug.print(" ", .{});
        expr.print();
    }
    std.debug.print("{s}", .{")"});
}

pub fn returnError(token: Token, message: []const u8) ParseError {
    parseError(token, message);
    return ParseError.ParseError;
}
