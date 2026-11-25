import { describe, expect, it } from "vitest";
import { levelApplyShape } from "./level-apply-shape.js";

describe("levelApplyShape", () => {
  it.each([
    ["a source that applies", "const a = levelWindow.apply(level, untilMs);", { applies: true, handRolls: false }],
    ["a source that opens by hand", "levelWindow.open(level, lastingMs);", { applies: false, handRolls: true }],
    ["a page that closes by hand", "levelWindow.apply(level, untilMs); levelWindow.close();", { applies: true, handRolls: true }],
    ["a webhost that closes by hand", "env.level.close();", { applies: false, handRolls: true }],
    ["a call named only in a comment", "// levelWindow.apply(level, untilMs)\n/* env.level.close() */", { applies: false, handRolls: false }],
  ] as const)("reads %s", (_name, source, want) => {
    // Act
    const got = levelApplyShape(source);

    // Assert
    expect(got).toEqual(want);
  });
});
