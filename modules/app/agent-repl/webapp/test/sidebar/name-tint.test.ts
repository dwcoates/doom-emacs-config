// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  NAME_FADE_DELAY_PROPERTY,
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

  it("leaves an untinted name alone", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, null);

    // Assert
    expect([label.className, label.getAttribute("style")]).toEqual(["", null]);
  });
});
