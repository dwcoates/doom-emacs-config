/**
 * THE SECTIONS' CLASSES LIVE IN A MODULE THAT IMPORTS NOTHING: its source read
 * for an import.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("the sections' classes", () => {
  it("live in a module that imports nothing, so no import cycle can leave them undefined", () => {
    const source = readFileSync(join(process.cwd(), "src/sidebar/section-classes.ts"), "utf8");

    expect(source).not.toMatch(/^\s*import\b/m);
  });
});
