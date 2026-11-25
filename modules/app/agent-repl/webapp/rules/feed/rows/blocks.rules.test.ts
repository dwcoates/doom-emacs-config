/**
 * The prompt blocks' rules: the attached image's own rule in the stylesheet
 * (src/styles.css), and both prompt kinds handing their body's blocks to
 * drawPromptCopyControl (src/feed/rows/).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import stylesheet from "../../../src/styles.css?raw";
import imageThumbnail from "../../../../proto/vocab/image-thumbnail.json";

describe("the attached image's own rule in the stylesheet", () => {
  it("caps an attached image at the width of the bubble it sits in", () => {
    // Arrange: the rule as the stylesheet declares it. An unconstrained <img>
    // of a screenshot is a couple of thousand pixels wide and would push the
    // bubble, and with it the feed's column, off the panel.
    const rule = stylesheet.match(/\.prompt-block-image\s*\{[^}]*\}/)?.[0] ?? "";

    // Act / Assert.
    expect(rule).toContain("max-width: 100%");
  });

  it.each([
    ["draws every image at the shared thumbnail height, large or small", `height: ${imageThumbnail.height_px}px`],
    ["keeps the aspect ratio when the width cap bites", "object-fit: contain"],
  ])("%s", (_name, declaration) => {
    // Arrange.
    const rule = stylesheet.match(/\.prompt-block-image\s*\{[^}]*\}/)?.[0] ?? "";

    // Act / Assert.
    expect(rule).toContain(declaration);
  });
});

describe("the prompt kinds' copy controls", () => {
  it("both hand their body's blocks to drawPromptCopyControl unwrapped by nothing", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));
    const sources = ["user-prompt.ts", "agent-prompt.ts"].map((file) =>
      readFileSync(path.join(here, "../../../src/feed/rows", file), "utf8"),
    );
    // Act
    const calls = sources.map((code) => /drawPromptCopyControl\(body\.blocks,/.test(code));
    // Assert
    expect(calls).toEqual([true, true]);
  });
});
