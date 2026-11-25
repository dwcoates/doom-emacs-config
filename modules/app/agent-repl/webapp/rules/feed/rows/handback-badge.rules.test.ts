/**
 * Every handback badge row draws its badge through drawHandbackBadge
 * (src/feed/rows/handback-badge.ts), never a hand-built element.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeOf } from "../../../test/source-text.js";

describe("drawHandbackBadge", () => {
  it("is what every handback badge row draws through", () => {
    for (const file of ["subagent-handback.ts", "merge-handback.ts"]) {
      const source = codeOf(readFileSync(join(process.cwd(), "src/feed/rows", file), "utf8"));
      expect(source.includes("drawHandbackBadge("), file).toBe(true);
      expect(source.includes("createElement("), file).toBe(false);
    }
  });
});
