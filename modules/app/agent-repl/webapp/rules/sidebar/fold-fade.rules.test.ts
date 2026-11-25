/**
 * THE FOLD FADE'S STYLING, read off the stylesheet: what a fading row and a
 * covered block divider fade, and the fade's keyframes.
 */
import { describe, expect, it } from "vitest";
import stylesheet from "../../src/styles.css?raw";
import { keyframesOf, rulesOf } from "../stylesheet-text.js";

describe("the fold fade's styling", () => {
  it("fades a fading row's own line, a covered row's only", () => {
    expect(stylesheet).toMatch(/#ws-sidebar \.ws\.covered\.fold-fading > :not\(\.kids\)/);
  });

  it("fades a block divider heading only covered, fading rows with them", () => {
    expect(stylesheet).toMatch(
      /#ws-sidebar \.sb-repo-block:not\(:has\(\.ws:not\(\.covered\)\)\):not\(:has\(\.surfaced:not\(\.fold-fading\)\)\) > \.sb-repo-divider \{\s*animation: ws-fold-fade var\(--fold-fade-duration/,
    );
  });

  it("fades from full ink to none", () => {
    expect(rulesOf(keyframesOf("ws-fold-fade")).map((rule) => [rule.selectors, rule.declarations.trim()])).toEqual([
      [["from"], "opacity: 1;"],
      [["to"], "opacity: 0;"],
    ]);
  });
});
