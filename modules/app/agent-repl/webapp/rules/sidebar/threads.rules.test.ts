/**
 * EVERY THREAD OUTLINE LABEL IS DRAWN THROUGH `labelOutline`: the threads
 * source read for the call and for a hand-drawn label.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("the current thread's outline", () => {
  it("draws every outline label through labelOutline", () => {
    // Arrange
    const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar/threads.ts"), "utf8");

    // Assert: no label is drawn from its text by hand beside labelOutline.
    expect(source.match(/labelOutline\(/g)?.length).toBe(3);
    expect(source).not.toMatch(/\.label, `\$\{at\}\.label`\)\.text/);
  });
});
