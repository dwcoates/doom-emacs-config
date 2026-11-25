/**
 * The tool call card reads a wire diff line's kind at one site
 * (src/feed/cards/tool-call.ts, classicDiffLines).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("classicDiffLines", () => {
  it("is the one site that reads a wire line's kind", () => {
    const src = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src/feed/cards/tool-call.ts");
    const calls = readFileSync(src, "utf8").match(/diffLineKind\(/g) ?? [];
    // The definition and the one call inside classicDiffLines.
    expect(calls.length).toBe(2);
  });
});
