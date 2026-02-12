const std = @import("std");
const Token = @import("Token.zig").Token;
const Literal = @import("Parser.zig").Literal;
const TokenType = @import("TokenType.zig").TokenType;
const scanError = @import("main.zig").scanError;

const keywords = std.StaticStringMap(TokenType).initComptime(
    .{
        .{ "and", .AND },
        .{ "class", .CLASS },
        .{ "else", .ELSE },
        .{ "false", .FALSE },
        .{ "for", .FOR },
        .{ "fun", .FUN },
        .{ "if", .IF },
        .{ "nil", .NIL },
        .{ "or", .OR },
        .{ "print", .PRINT },
        .{ "return", .RETURN },
        .{ "super", .SUPER },
        .{ "this", .THIS },
        .{ "true", .TRUE },
        .{ "var", .VAR },
        .{ "while", .WHILE },
    },
);

pub const Scanner = struct {
    const Self = @This();
    source: []const u8,
    allocator: std.mem.Allocator,
    tokens: std.ArrayList(Token),
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,

    pub fn init(source: []const u8, allocator: std.mem.Allocator) !Scanner {
        return .{
            .allocator = allocator,
            .source = source,
            .tokens = std.ArrayList(Token).empty,
        };
    }

    pub fn deinit(self: *Self) void {
        self.tokens.deinit(self.allocator);
    }

    /// Scans the passed string from prompt and returns a list of tokens.
    pub fn scanTokens(self: *Scanner) std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            self.scanToken();
        }

        // After scanning tokens mark the end of the line
        self.tokens.append(self.allocator, Token.init(
            .EOF,
            "",
            .none,
            self.line,
        )) catch |err| {
            std.log.err("Error: {}\n", .{err});
            return self.tokens;
        };

        return self.tokens;
    }

    /// Whether the current index as the end of the source string or not.
    fn isAtEnd(self: *Scanner) bool {
        return self.current >= self.source.len;
    }

    /// Advances through the string, tokenize each characters.
    /// Decides if a character is either a symbol, a number or
    /// a literal.
    fn scanToken(self: *Scanner) void {
        const char = self.advance();
        switch (char) {
            '(' => self.addToken(.LEFT_PAREN, .none),
            ')' => self.addToken(.RIGHT_PAREN, .none),
            '{' => self.addToken(.LEFT_BRACE, .none),
            '}' => self.addToken(.RIGHT_BRACE, .none),
            ',' => self.addToken(.COMMA, .none),
            '.' => self.addToken(.DOT, .none),
            '-' => self.addToken(.MINUS, .none),
            '+' => self.addToken(.PLUS, .none),
            ';' => self.addToken(.SEMICOLON, .none),
            '*' => self.addToken(.STAR, .none),
            '!' => self.addToken(if (self.match('=')) .BANG_EQUAL else .BANG, .none),
            '=' => self.addToken(if (self.match('=')) .EQUAL_EQUAL else .EQUAL, .none),
            '<' => self.addToken(if (self.match('=')) .LESS_EQUAL else .LESS, .none),
            '>' => self.addToken(if (self.match('=')) .GREATER_EQUAL else .GREATER, .none),
            '/' => if (self.match('/')) {
                while (self.peek() != '\n' and !self.isAtEnd()) {
                    _ = self.advance();
                }
            } else {
                self.addToken(.SLASH, .none);
            },
            else => if (std.ascii.isDigit(char)) {
                self.number();
            } else if (std.ascii.isAlphabetic(char)) {
                self.identifier();
            } else {
                scanError(self.line, "Unexpected character");
            },
            ' ', '\r', '\t' => {},
            '\n' => self.line += 1,
            '"' => self.string(),
            'o' => if (self.match('r')) self.addToken(.OR, .none),
        }
    }

    /// Advances the current index to the next. Otherwise, the while loop
    /// doesn't make sense.
    fn advance(self: *Scanner) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    /// Add values to the token list.
    fn addToken(self: *Scanner, token_type: TokenType, literal: Literal) void {
        const text = self.source[self.start..self.current];
        self.tokens.append(self.allocator, Token.init(
            token_type,
            text,
            literal,
            self.line,
        )) catch |err| {
            std.log.err("Error: {}\n", .{err});
            return;
        };
    }

    /// Used for matching the next character.
    fn match(self: *Scanner, comptime expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;

        self.current += 1;
        return true;
    }

    /// Peek the current indexed character, does not iterate.
    fn peek(self: *Scanner) u8 {
        if (self.isAtEnd()) return '\x00';
        return self.source[self.current];
    }

    /// Peek the next indexed character, does not iterate.
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return '\x00';
        return self.source[self.current + 1];
    }

    /// If the current character happens to be a quote this
    /// function is called. First while loop checks the content
    /// inside the quote until the closing quote is checked.
    /// If it's a new line keep advancing.
    fn string(self: *Scanner) void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            scanError(self.line, "Unterminated string.");
            return;
        }

        _ = self.advance();

        const value = self.source[self.start + 1 .. self.current - 1];
        self.addToken(.STRING, .{ .string = value });
    }

    /// This function is called if a characters happens to be a
    /// numeric value.
    /// First while loop is for natural numbers.
    /// Second while loop is for floating points.
    /// Parse the characters to a literal number.
    fn number(self: *Scanner) void {
        while (std.ascii.isDigit(self.peek())) {
            _ = self.advance();
        }

        if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
            _ = self.advance();

            while (std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        const text = self.source[self.start..self.current];
        const parsed = std.fmt.parseFloat(f64, text) catch |err| {
            std.log.err("Error: {}\n", .{err});
            return;
        };

        self.addToken(.NUMBER, .{ .number = parsed });
    }

    /// This function is called if a character happens to be a
    /// alphabetic value.
    /// If an identifier hashed string is matched mark it is one.
    /// Advances until the end of the string.
    fn identifier(self: *Scanner) void {
        while (std.ascii.isAlphanumeric(self.peek())) {
            _ = self.advance();
        }

        const text = self.source[self.start..self.current];
        const token_type = keywords.get(text) orelse .IDENTIFIER;
        self.addToken(token_type, .none);
    }
};
