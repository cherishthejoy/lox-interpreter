const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const parseError = @import("main.zig").parseError;
const LoxCallable = @import("LoxCallable.zig").LoxCallable;

pub const Stmt = union(enum) {
    expression: *Expr,
    function: FunctionDecl,
    if_stmt: struct { condition: *Expr, then_branch: *Stmt, else_branch: ?*Stmt },
    print: *Expr,
    return_stmt: struct { keyword: Token, value: ?*Expr },
    variable: struct { name: Token, initializer: ?*Expr },
    while_stmt: struct { condition: *Expr, body: *Stmt },
    block: []*Stmt,
};

pub const FunctionDecl = struct {
    name: Token,
    params: []Token,
    body: []*Stmt,
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
        if (self.match(&[_]TokenType{.FUN})) return try self.function("function");
        if (self.match(&[_]TokenType{.VAR})) return self.varDeclaration() catch |err| {
            self.synchronize();
            // parseError(err);
            return err;
        };
        return try self.statement();
    }

    fn function(self: *Self, kind: []const u8) ParseError!*Stmt {
        var message = try std.fmt.allocPrint(self.allocator, "Expect {s} name.", .{kind});
        const name = try self.consume(.IDENTIFIER, message);

        message = try std.fmt.allocPrint(self.allocator, "Expect '(' after {s}", .{kind});
        _ = try self.consume(.LEFT_PAREN, message);
        var parameters = std.ArrayList(Token).empty;
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                if (parameters.items.len >= 255) {
                    return returnError(self.peek(), "Can't have more than 255 arguments.");
                }
                try parameters.append(
                    self.allocator,
                    try self.consume(
                        .IDENTIFIER,
                        "Expect paramter name",
                    ),
                );
                if (!self.match(&[_]TokenType{.COMMA})) break;
            }
        }
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after parameters.");
        message = try std.fmt.allocPrint(self.allocator, "Expect '{{' before {s} body.", .{kind});
        _ = try self.consume(.LEFT_BRACE, message);
        const body = try self.block();

        const new_function = try self.allocator.create(Stmt);

        new_function.* = Stmt{
            .function = FunctionDecl{
                .name = name,
                .params = parameters.items,
                .body = body,
            },
        };

        return new_function;
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
                else => {
                    return returnError(equals, "Invalid assignment target.");
                },
            }

            returnError(equals, "Invalid assignment target.");
        }
        return expr;
    }

    fn statement(self: *Self) !*Stmt {
        if (self.match(&[_]TokenType{.FOR})) return try self.forStatement();
        if (self.match(&[_]TokenType{.IF})) return try self.ifStatement();
        if (self.match(&[_]TokenType{.PRINT})) return try self.printStatement();
        if (self.match(&[_]TokenType{.RETURN})) return try self.returnStatement();
        if (self.match(&[_]TokenType{.WHILE})) return try self.whileStatement();
        if (self.match(&[_]TokenType{.LEFT_BRACE})) {
            const body = try self.block();
            const new_stmt = try self.allocator.create(Stmt);
            new_stmt.* = Stmt{ .block = body };
            return new_stmt;
        }
        return try self.expressionStatement();
    }

    fn returnStatement(self: *Self) ParseError!*Stmt {
        const keyword = self.previous();
        var value: ?*Expr = null;

        if (!self.check(.SEMICOLON)) value = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after return value.");

        const new_stmt = try self.allocator.create(Stmt);
        new_stmt.* = Stmt{
            .return_stmt = .{
                .keyword = keyword,
                .value = value.?,
            },
        };

        return new_stmt;
    }

    fn forStatement(self: *Self) ParseError!*Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");

        var initializer: ?*Stmt = null;

        if (self.match(&[_]TokenType{.SEMICOLON})) {
            initializer = null;
        } else if (self.match(&[_]TokenType{.VAR})) {
            initializer = try self.varDeclaration();
        } else {
            initializer = try self.expressionStatement();
        }

        var condition: ?*Expr = null;
        if (!self.check(.SEMICOLON)) {
            condition = try self.expression();
        }
        _ = try self.consume(.SEMICOLON, "Expect ';' after loop condition.");

        var increment: ?*Expr = null;
        if (!self.check(.RIGHT_PAREN)) {
            increment = try self.expression();
        }
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after for clauses.");

        var body = try self.statement();
        const new_stmt = try self.allocator.create(Stmt);

        if (increment != null) {
            const new_inner_stmt = try self.allocator.create(Stmt);
            new_inner_stmt.* = Stmt{ .expression = increment.? };

            const stmt_array: [2]*Stmt = .{ body, new_inner_stmt };
            const stmt_slice = try self.allocator.dupe(*Stmt, &stmt_array);
            new_stmt.* = Stmt{ .block = stmt_slice };
            body = new_stmt;
        }

        const new_expr = try self.allocator.create(Expr);
        new_expr.* = Expr{ .literal = .{ .boolean = true } };
        if (condition == null) condition = new_expr;
        const new_while_stmt = try self.allocator.create(Stmt);
        new_while_stmt.* = Stmt{
            .while_stmt = .{ .condition = condition.?, .body = body },
        };
        body = new_while_stmt;

        if (initializer != null) {
            const new_inner_stmt = try self.allocator.create(Stmt);
            const stmt_array: [2]*Stmt = .{ initializer.?, body };
            const stmt_slice = try self.allocator.dupe(*Stmt, &stmt_array);
            new_inner_stmt.* = Stmt{ .block = stmt_slice };
            body = new_inner_stmt;
        }

        return body;
    }

    fn whileStatement(self: *Self) ParseError!*Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'while'.");
        const condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after condition.");

        const body = try self.statement();
        const new_stmt = try self.allocator.create(Stmt);
        new_stmt.* = Stmt{
            .while_stmt = .{ .condition = condition, .body = body },
        };

        return new_stmt;
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

    fn call(self: *Self) !*Expr {
        var expr = try self.primary();

        while (true) {
            if (self.match(&[_]TokenType{.LEFT_PAREN}))
                expr = try self.finishCall(expr)
            else
                break;
        }
        return expr;
    }

    fn finishCall(self: *Self, callee: *Expr) !*Expr {
        var arguments = std.ArrayList(*Expr).empty;
        if (!self.check(.RIGHT_PAREN)) {
            while (true) {
                if (arguments.items.len >= 255) {
                    parseError(self.peek(), "Can't have more than 255 arguements.");
                }
                try arguments.append(self.allocator, try self.expression());
                if (!self.match(&[_]TokenType{.COMMA})) break;
            }
        }
        const paren = try self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");
        const new_expr = try self.allocator.create(Expr);
        new_expr.* = Expr{
            .caller = .{
                .callee = callee,
                .paren = paren,
                .arguments = arguments.items,
            },
        };

        return new_expr;
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
        // return self.primary();
        return self.call();
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
    caller: Caller,
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

const Caller = struct {
    callee: *const Expr,
    paren: Token,
    arguments: []*Expr,
};

pub const Literal = union(enum) {
    none,
    number: f64,
    string: []const u8,
    boolean: bool,
    callable: LoxCallable,
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
