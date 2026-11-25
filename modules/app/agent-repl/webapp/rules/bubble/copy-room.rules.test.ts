/**
 * EVERY BUBBLE COPY CONTROL LEAVES ITS ROOM (src/bubble/copy-room.ts): a rule
 * over the sources that draw one.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "../files-under.js";

describe("every bubble copy control's draw", () => {
  /** Every source file under src/, by its path from src/. */
  function sources(): { file: string; code: string }[] {
    const src = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src");
    return filesUnder(src).map((file) => ({ file: path.relative(src, file), code: readFileSync(file, "utf8") }));
  }

  it("leaves its room through reserveCopyRoom, wherever a copy control goes in a bubble's corner", () => {
    // Arrange: every file that draws a bubble's copy control into a bubble.
    const drawing = sources().filter(
      ({ file, code }) =>
        file !== "bubble/copy-room.ts" &&
        file !== "feed/rows/blocks.ts" &&
        /\b(?:bubbleCopyControl|drawPromptCopyControl)\(/.test(code),
    );
    // Act
    const without = drawing.filter(({ code }) => !/\breserveCopyRoom\(/.test(code)).map(({ file }) => file);
    // Assert
    expect([drawing.length > 0, without]).toEqual([true, []]);
  });

  it("is never a hand-rolled copy control in a bubble's corner", () => {
    // Act: every other file that builds a bound copy control.
    const rolled = sources()
      .filter(({ file, code }) => file !== "copy-control.ts" && file !== "bubble/copy-room.ts" && /\bboundCopyControl\(/.test(code))
      .map(({ file }) => file);
    // Assert
    expect(rolled).toEqual([]);
  });
});
