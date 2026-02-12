const std = @import("std");

const TokenType = @import("TokenType.zig").TokenType;
const Literal = @import("Parser.zig").Literal;

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: Literal,
    line: usize,

    pub fn init(token_type: TokenType, lexeme: []const u8, literal: Literal, line: usize) Token {
        return .{
            .token_type = token_type,
            .lexeme = lexeme,
            .literal = literal,
            .line = line,
        };
    }
};
