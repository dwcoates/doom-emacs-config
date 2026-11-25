/** Every page module waits for a frame through nextFrame (src/frame.ts), but those reading the scheduler off a view. */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

describe("who waits for a frame", () => {
  it("leaves every page module to nextFrame, but those reading the scheduler off a view", () => {
    // Arrange: the modules that own a view's missing scheduler (src/frame.ts).
    const ownView = new Set(["frame.ts", "type-out.ts", path.join("bubble", "body.ts")]);

    // Act
    const rolled = [...codeUnder(SRC)]
      .filter(([file, code]) => !ownView.has(file) && /requestAnimationFrame/.test(code))
      .map(([file]) => file);

    // Assert
    expect(rolled).toEqual([]);
  });
});
