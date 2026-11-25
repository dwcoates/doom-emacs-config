/**
 * EVERY CARD THAT NOTES A REFUSAL BESIDE ITS CONTROLS USES THE SHARED HELPERS
 * (`src/rpc/refuse.ts`'s `drawRefusalAfter` and `clearRefusalsAfter`).
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../test/source-text.js";

describe("every card that notes a refusal beside its controls uses the shared helpers", () => {
  it.each(["held/held-prompt.ts", "feed/merge/queue-offer.ts"])("%s", (file) => {
    const code = codeOf(readFileSync(join(process.cwd(), "src", file), "utf8"));
    expect([
      /\bdrawRefusalAfter\(/.test(code),
      /\bclearRefusalsAfter\(/.test(code),
      /\.after\(\s*refusal\s*\)/.test(code),
    ]).toEqual([true, true, false]);
  });
});
