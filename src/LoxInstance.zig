const std = @import("std");

const Token = @import("Token.zig").Token;
const Literal = @import("Parser.zig").Literal;
const LoxClass = @import("LoxClass.zig").LoxClass;

const runtimeError = @import("main.zig").runtimeError;
const RuntimeError = @import("Interpreter.zig").RuntimeError;

pub const LoxInstance = struct {
    const Self = @This();
    class: *LoxClass,
    fields: std.StringHashMap(Literal),

    pub fn init(class: *LoxClass, allocator: std.mem.Allocator) Self {
        return .{
            .class = class,
            .fields = std.StringHashMap(Literal).init(allocator),
        };
    }

    pub fn get(self: *Self, name: Token) !Literal {
        if (self.fields.get(name.lexeme)) |found| {
            return found;
        }
        if (self.class.findMethod(name.lexeme)) |method| {
            return Literal{
                .callable = .{
                    .function = method,
                },
            };
        }

        runtimeError(
            name,
            "Undefined property + name",
        );
        return RuntimeError.UndefinedProperty;
    }

    pub fn set(self: *Self, name: Token, value: Literal) !void {
        try self.fields.put(name.lexeme, value);
    }

    pub fn toString(self: *Self) []const u8 {
        return self.class.name;
    }
};
