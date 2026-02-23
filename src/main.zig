const std = @import("std");
const lox = @import("lox");

const Allocator = std.mem.Allocator;

const Token = @import("Token.zig").Token;
const Scanner = @import("Scanner.zig").Scanner;
const Parser = @import("Parser.zig").Parser;
const Interpreter = @import("Interpreter.zig").Interpreter;
const Resolver = @import("Resolver.zig").Resolver;

var input_buffer: [50]u8 = undefined;
var stdin_reader = std.fs.File.stdin().reader(&input_buffer);
const stdin = &stdin_reader.interface;

var output_buffer: [50]u8 = undefined;
var stdout_writer = std.fs.File.stdout().writer(&output_buffer);
const stdout = &stdout_writer.interface;

// Lost variables in the void
var had_error: bool = false;
var had_runtime_error: bool = false;

pub fn main() !void {
    const argv = std.os.argv;

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = arena.deinit();
    const allocator = arena.allocator();

    if (argv.len > 2) {
        std.debug.print("Usage: lox [script]\n", .{});
        std.process.exit(64);
    } else if (argv.len == 2) {
        try runFile(argv[1], allocator);
    } else try runPrompt(stdin, stdout, allocator);
}

fn runFile(arg: [*:0]const u8, allocator: Allocator) !void {
    var buffer: [256]u8 = undefined;
    // Convert the null terminated bytes to slice
    const slice = std.mem.span(arg);
    const file = std.fs.cwd().readFile(slice, &buffer) catch |err| {
        std.log.err("Failed to open file: {s}", .{@errorName(err)});
        return;
    };
    try run(file, allocator);
    if (had_error) std.process.exit(65);
    if (had_runtime_error) std.process.exit(70);
}

fn runPrompt(reader: *std.Io.Reader, writer: *std.Io.Writer, allocator: Allocator) !void {
    try writer.print("> ", .{});
    try writer.flush();

    const line = try reader.takeDelimiter('\n');
    try run(line.?, allocator);
    had_error = false;
}

fn run(string: []const u8, allocator: Allocator) !void {
    var scanner = try Scanner.init(string, allocator);
    defer scanner.deinit();
    const token = scanner.scanTokens();

    var parser = Parser.init(token, allocator);
    const statements = parser.parse() catch |err| {
        std.debug.print("Error: {s}\n", .{@errorName(err)});
        return;
    };

    if (had_error) return;

    var interpreter = try Interpreter.init(allocator);
    var resolver = Resolver.init(&interpreter, allocator);

    try resolver.resolveStmts(statements.items);

    if (had_error) return;

    try interpreter.interpret(statements);
}

pub fn parseError(token: Token, message: []const u8) void {
    if (token.token_type == .EOF) {
        std.debug.print("At line: {d} | ParseError: {s}, {s}\n", .{ token.line, "", message });
        had_error = true;
    } else {
        std.debug.print("At line: {d} | ParseError: {s}, {s}\n", .{ token.line, token.lexeme, message });
        had_error = true;
    }
}

pub fn scanError(line: usize, message: []const u8) void {
    std.debug.print("At line: {d} | ScanError: {s}, {s}\n", .{ line, "", message });
}

pub fn runtimeError(token: Token, message: []const u8) void {
    std.debug.print("At line: {d} | RuntimeError: {s}\n", .{ token.line, message });
    had_runtime_error = true;
}
