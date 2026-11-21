// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { createRowRenderers } from "../../src/feed/row-renderers.js";
import type { RowRenderers } from "../../src/feed/renderers.js";
import { carriesCardQuestion } from "../../src/feed/response-question.js";
import { harness } from "./harness.js";

/**
 * THE REGISTRY IS COMPLETE.
 *
 * The type-level half is the annotation on this list: a key added to
 * `RowRenderers` and missing from it does not compile, and a key here that the
 * interface does not declare does not compile either. The runtime half is the
 * sweep below, which catches the one thing the type cannot — a key declared,
 * typed, and left `undefined` by the assembler.
 */
const REGISTRY_KEYS: readonly (keyof RowRenderers)[] = [
  "response",
  "simpleToolCall",
  "hook",
  "skill",
  "artifact",
  "plan",
  "findings",
  "chessBoard",
  "subagentResult",
  "fixGate",
  "hotReload",
  "shell",
  "shellHead",
  "permission",
  "question",
  "coldGate",
  "mergeHead",
  "subagentHead",
  "detachedSubagentHead",
  "commandPanel",
  "commandRefused",
  "mergeBody",
];

/** The one seam that draws no card: a merge bubble's body lays out its sub-feed's rows. */
const NOT_A_CARD: readonly (keyof RowRenderers)[] = ["mergeBody"];

describe("createRowRenderers", () => {
  for (const key of REGISTRY_KEYS) {
    it(`fills the ${key} seam`, () => {
      const h = harness();
      expect(typeof createRowRenderers(h.ctx)[key]).toBe("function");
    });
  }

  it("fills no seam the interface does not declare", () => {
    const h = harness();
    expect(Object.keys(createRowRenderers(h.ctx)).sort()).toEqual([...REGISTRY_KEYS].sort());
  });

  // Owner request, 2026-11-09: every card carries the question control, through
  // one helper, so a card kind added without it fails here.
  it("draws every card through the question control's one helper", () => {
    // Arrange
    const h = harness();
    const renderers = createRowRenderers(h.ctx);

    // Act
    const unwrapped = Object.entries(renderers)
      .filter(([key, draw]) => !NOT_A_CARD.includes(key as keyof RowRenderers) && !carriesCardQuestion(draw))
      .map(([key]) => key);

    // Assert
    expect(unwrapped).toEqual([]);
  });

  it("leaves the merge body, which draws no card, unwrapped", () => {
    // Arrange
    const h = harness();

    // Act
    const got = carriesCardQuestion(createRowRenderers(h.ctx).mergeBody);

    // Assert
    expect(got).toBe(false);
  });
});
