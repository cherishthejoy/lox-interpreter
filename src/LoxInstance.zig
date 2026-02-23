const std = @import("std");

const LoxClass = @import("LoxClass.zig").LoxClass;

pub const LoxInstance = struct {
    const Self = @This();
    class: *LoxClass,

    pub fn init(class: *LoxClass) Self {
        return .{ .class = class };
    }

    pub fn toString(self: *Self) []const u8 {
        return self.class.name;
    }
};
