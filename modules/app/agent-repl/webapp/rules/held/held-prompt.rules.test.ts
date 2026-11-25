// @vitest-environment jsdom
/**
 * The held prompt card's rules: it composes no badge sentence of its own
 * (src/held/held-prompt.ts), no webapp source draws the retired "Release"
 * label, and the parts a folded or collapsed card shows are none the
 * stylesheet hides or clamps (src/styles.css).
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { HELD_NOTE_CLASS } from "../../src/held/held-prompt.js";
import heldPromptSource from "../../src/held/held-prompt.ts?raw";
import { codeOf } from "../../test/source-text.js";
import { mountedFull, withTallHeld } from "../../test/held/held-fixtures.js";
import { filesUnder } from "../files-under.js";
import { selectorOf } from "../stylesheet-text.js";

/** The stylesheet's selector for an expand-only element the toggle has not opened. */
const HIDDEN_WHILE_COLLAPSED = selectorOf(
  /([^{}]*\.bubble-expand-only)\s*\{\s*display:\s*none;\s*\}/,
  "hidden expand-only region",
);

/** The ellipsis clamp on a collapsed body, as the stylesheet writes it. */
const CLAMPED_BODY = selectorOf(
  /([^{}]*\.bubble-body)\s*\{[^{}]*-webkit-line-clamp/,
  "ellipsis clamp",
);

/** The parts a collapsed card shows. */
const ALWAYS_SHOWN: Array<[string, string]> = [
  ["the status badge", ".held-badge"],
  ["a further fact's note", `.${HELD_NOTE_CLASS}`],
  ["the queued age", "[data-queued]"],
  ["the Send now button", '[data-held-action="release"]'],
  ["the Meta button", '[data-held-action="update-classifier"]'],
  ["the Cancel button", '[data-held-action="drop"]'],
];

describe("the daemon's badge words", () => {
  it("composes no status sentence of its own", () => {
    // Arrange: every sentence the card used to compose for a badge.
    const composed = [
      '"queued — classifying"',
      '"interjects"',
      '"after this turn"',
      '"waits for "',
      '" to finish"',
      '"unclassified"',
      '"confirmed"',
      "held for the scheduled restart",
      "held until the session reconnects",
      "held for the build refresh",
    ];
    // Act
    const source = codeOf(heldPromptSource);
    // Assert
    expect(composed.filter((sentence) => source.includes(sentence))).toEqual(
      [],
    );
  });
});

describe("a held prompt collapsed", () => {
  it.each(ALWAYS_SHOWN)("shows %s while collapsed", (_name, selector) => {
    // Arrange / Act
    const part = mountedFull().querySelector(selector);
    // Assert
    expect([part === null, part?.closest(HIDDEN_WHILE_COLLAPSED) ?? null]).toEqual([false, null]);
  });
});

describe("a held prompt folded", () => {
  it("clamps nothing, so every line shows", () => {
    // Arrange / Act / Assert
    withTallHeld((_card, box) => expect(box.querySelector(".bubble-body")?.matches(CLAMPED_BODY)).toBe(false));
  });

  it("keeps its badges shown while folded", () => {
    // Arrange / Act / Assert
    withTallHeld((card) => expect(card.querySelector(".held-badge")?.closest(HIDDEN_WHILE_COLLAPSED)).toBeNull());
  });
});

describe("the retired Release label", () => {
  /** The files among PATHS holding a string literal that is exactly "Release". */
  function holdingTheLabel(paths: string[]): string[] {
    return paths.filter((path) =>
      /(["'`])Release\1/.test(readFileSync(path, "utf8")),
    );
  }

  it("is drawn by no webapp source", () => {
    // Arrange
    const sources = filesUnder(join(process.cwd(), "src"));
    // Act, Assert
    expect(holdingTheLabel(sources)).toEqual([]);
  });
});
