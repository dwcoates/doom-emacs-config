/**
 * The webapp reads and places its CSS animations through src/animation-delays.ts:
 * no source asks WebKit for an animation or parses a CSS time of its own, and
 * every one-shot is placed through placeOneShot.
 */
import path from "node:path";
import { fileURLToPath } from "node:url";
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { codeUnder } from "./files-under.js";
import { codeOf } from "../test/source-text.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the call sites", () => {
  const code = codeUnder(path.join(here, "../src"));

  it("ask WebKit for no animation anywhere in the webapp", () => {
    // Arrange / Act
    const asking = [...code].filter(([, text]) => text.includes("getAnimations(")).map(([file]) => file);

    // Assert
    expect(asking).toEqual([]);
  });

  it.each(["sidebar/wall-phase.ts", "host/unshown-animations.ts", "page-census.ts"])(
    "%s reads animations through the shared helpers, with no time parsing of its own",
    (file) => {
      // Arrange
      const text = code.get(file) ?? "";

      // Act
      const shape = [text.includes('from "../animation-delays.js"') || text.includes('from "./animation-delays.js"'), /function cssTimeMs|function cssList/.test(text)];

      // Assert
      expect(shape).toEqual([true, false]);
    },
  );
});

describe("placeOneShot", () => {
  // EVERY ONE-SHOT IS PLACED BY THE ONE HELPER: a site that spells its own
  // length and negative delay would drift from the others' format.
  it.each(["sidebar/fold-fade.ts", "sidebar/selection-fade.ts"])(
    "places %s's one-shot through placeOneShot",
    (file) => {
      // Arrange
      const code = codeOf(readFileSync(path.join(here, "../src", file), "utf8"));

      // Act / Assert
      expect([code.includes("placeOneShot("), /setProperty\([^)]*`\$\{-|setProperty\([^)]*`-\$\{/.test(code)]).toEqual([true, false]);
    },
  );

});
