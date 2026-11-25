/** The name prefix's drawer spells no label, and every surface draws the prefix through it (src/name-prefix.ts). */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the priority labels", () => {
  it("never spells a label in the drawer itself", () => {
    const source = readFileSync(path.join(here, "../src/name-prefix.ts"), "utf8");
    expect(source).not.toContain('"P0.5"');
  });
});

describe("the prefix's drawn order", () => {
  // EVERY SURFACE DRAWS THE PREFIX THROUGH THIS ONE HELPER: a surface that
  // built its own mark element would draw it apart from the others.
  it.each(["src/sidebar/row.ts", "src/topbar/strip.ts"])("is the one drawing %s uses", (file) => {
    const source = readFileSync(path.join(here, "..", file), "utf8");
    expect([source.includes("drawNamePrefix("), source.includes('"name-emoji-ack"'), source.includes('"name-priority"')]).toEqual([
      true,
      false,
      false,
    ]);
  });
});
