// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import pageHold from "../../../proto/vocab/page-hold.json";
import {
  NAME_FADE_DELAY_PROPERTY,
  NAME_FADE_MS,
  NAME_FADE_PROPERTY,
  NAME_FADING_CLASS,
  NAME_STEADY_CLASS,
  applyNameTint,
  nameTint,
} from "../../src/sidebar/name-tint.js";

describe("nameTint", () => {
  it.each([
    { name: "holds a running turn's name at its tone", shown: "turnRunning", ended: undefined, tone: "tone-red", want: { kind: "steady" } },
    { name: "fades an ended turn's name from the time it ended", shown: "noTurnRunning", ended: 9_000, tone: "tone-green", want: { kind: "fading", elapsedMs: 1_000 } },
    { name: "never reads a turn that ends in the future as negative time", shown: "noTurnRunning", ended: 11_000, tone: "tone-green", want: { kind: "fading", elapsedMs: 0 } },
    { name: "tints nothing when no turn has ended", shown: "noTurnRunning", ended: undefined, tone: "tone-green", want: null },
    { name: "tints nothing on a merged row", shown: "merged", ended: undefined, tone: "tone-green", want: null },
    { name: "tints nothing on a row with no tone", shown: "turnRunning", ended: undefined, tone: "tone-none", want: null },
  ])("$name", ({ shown, ended, tone, want }) => {
    // Arrange / Act
    const got = nameTint(shown, ended, tone, false, 10_000);

    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "stops a viewed row's ended-turn fade so it draws at rest", shown: "noTurnRunning", ended: 9_000, tone: "tone-green", want: null },
    { name: "tints nothing on a viewed row with no ended turn", shown: "noTurnRunning", ended: undefined, tone: "tone-green", want: null },
  ])("$name", ({ shown, ended, tone, want }) => {
    // Arrange / Act
    const got = nameTint(shown, ended, tone, true, 10_000);

    // Assert
    expect(got).toEqual(want);
  });
  it.each([
    ["holds a sync merge's name at the merge's purple", "tone-purple", false],
    ["holds a sync merge's own turn's name at the merge's purple over the row's tone", "tone-red", false],
    ["holds a viewed sync merge's name at the merge's purple", "tone-purple", true],
    ["holds a toneless sync merge's name at the merge's purple", "tone-none", false],
  ] as const)("%s", (_name, tone, viewed) => {
    // Arrange / Act
    const got = nameTint("syncMerging", undefined, tone, viewed, 10_000);

    // Assert
    expect(got).toEqual({ kind: "syncMerging", tone: "tone-purple" });
  });
});

describe("applyNameTint", () => {
  it("marks a steady name", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "steady" });

    // Assert
    expect([label.classList.contains(NAME_STEADY_CLASS), label.classList.contains(NAME_FADING_CLASS)]).toEqual([true, false]);
  });

  it("marks a fading name and sets the delay to minus the elapsed time", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "fading", elapsedMs: 120_000 });

    // Assert
    expect([label.classList.contains(NAME_FADING_CLASS), label.style.getPropertyValue(NAME_FADE_DELAY_PROPERTY)]).toEqual([true, "-120000ms"]);
  });

  it("fades a name over the page hold", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "fading", elapsedMs: 0 });

    // Assert
    expect([NAME_FADE_MS, label.style.getPropertyValue(NAME_FADE_PROPERTY)]).toEqual([
      pageHold.hold_seconds * 1000,
      `${pageHold.hold_seconds * 1000}ms`,
    ]);
  });

  it("gives a steady name no fade length", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "steady" });

    // Assert
    expect(label.style.getPropertyValue(NAME_FADE_PROPERTY)).toBe("");
  });

  it("marks a sync-merging name steady in its tone", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "syncMerging", tone: "tone-purple" });

    // Assert
    expect([label.classList.contains(NAME_STEADY_CLASS), label.classList.contains("tone-purple"), label.classList.contains(NAME_FADING_CLASS)]).toEqual([true, true, false]);
  });

  it("leaves an untinted name alone", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, null);

    // Assert
    expect([label.className, label.getAttribute("style")]).toEqual(["", null]);
  });
});
