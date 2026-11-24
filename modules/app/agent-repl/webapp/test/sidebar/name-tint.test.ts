// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { NAME_STEADY_CLASS, applyNameTint, nameTint } from "../../src/sidebar/name-tint.js";

describe("nameTint", () => {
  it.each([
    { name: "holds a running turn's name at its tone", shown: "turnRunning", tone: "tone-red", want: { kind: "steady" } },
    { name: "holds an ended turn's name at its tone, with no fade", shown: "noTurnRunning", tone: "tone-green", want: { kind: "steady" } },
    { name: "holds a row that never ran a turn at its tone", shown: "noTurnRunning", tone: "tone-green", want: { kind: "steady" } },
    { name: "holds a merged row's name at its tone", shown: "merged", tone: "tone-green", want: { kind: "steady" } },
    { name: "tints nothing on a row with no tone", shown: "turnRunning", tone: "tone-none", want: null },
  ])("$name", ({ shown, tone, want }) => {
    // Arrange / Act
    const got = nameTint(shown, tone, false);

    // Assert
    expect(got).toEqual(want);
  });

  it.each([
    { name: "tints nothing on a viewed running turn", shown: "turnRunning", tone: "tone-red" },
    { name: "tints nothing on a viewed ended turn", shown: "noTurnRunning", tone: "tone-green" },
  ])("$name", ({ shown, tone }) => {
    // Arrange / Act
    const got = nameTint(shown, tone, true);

    // Assert
    expect(got).toBeNull();
  });

  it.each([
    ["holds a sync merge's name at the merge's purple", "tone-purple", false],
    ["holds a sync merge's own turn's name at the merge's purple over the row's tone", "tone-red", false],
    ["holds a viewed sync merge's name at the merge's purple", "tone-purple", true],
    ["holds a toneless sync merge's name at the merge's purple", "tone-none", false],
  ] as const)("%s", (_name, tone, viewed) => {
    // Arrange / Act
    const got = nameTint("syncMerging", tone, viewed);

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
    expect([label.className, label.getAttribute("style")]).toEqual([NAME_STEADY_CLASS, null]);
  });

  it("marks a sync-merging name steady in its tone", () => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, { kind: "syncMerging", tone: "tone-purple" });

    // Assert
    expect([label.classList.contains(NAME_STEADY_CLASS), label.classList.contains("tone-purple")]).toEqual([true, true]);
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
