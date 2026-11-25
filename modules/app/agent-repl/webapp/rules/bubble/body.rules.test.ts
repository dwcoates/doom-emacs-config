/**
 * THE BUBBLE BODY'S RULES over its source (src/bubble/body.ts): every
 * slot-only setting goes through the one guard.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("requireMarkdownSlot", () => {
  it("is the one guard every slot-only setting goes through", () => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/bubble/body.ts"), "utf8");
    // Act
    const handRolled = source.match(/if \(!isMarkdownSlot\(/g) ?? [];
    // Assert: only requireMarkdownSlot itself tests a slot this way.
    expect(handRolled).toHaveLength(1);
  });
});
