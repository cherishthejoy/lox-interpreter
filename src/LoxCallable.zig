const std = @import("std");
const Interpreter = @import("Interpreter.zig").Interpreter;
const Literal = @import("Parser.zig").Literal;
const LoxFunction = @import("LoxFunction.zig").LoxFunction;
const LoxClass = @import("LoxClass.zig").LoxClass;

pub const LoxCallable = union(enum) {
    native: NativeFn,
    function: *LoxFunction,
    class: *LoxClass,
};

const NativeFn = struct {
    arity: usize,
    call: *const fn (interpreter: *Interpreter, args: []Literal) Literal,
};
