// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { NAME_STEADY_CLASS, NAME_VIEWED_CLASS, applyNameTint, nameTint, type NameFacts, type NameTint } from "../../src/sidebar/name-tint.js";

/** An idle, unviewed, done row with no sync merge and no background. */
const IDLE: NameFacts = {
  shown: "noTurnRunning",
  arm: "done",
  tone: "tone-green",
  viewed: false,
  unreadTurnEnd: undefined,
};

describe("nameTint", () => {
  it.each<{ name: string; facts: Partial<NameFacts>; want: NameTint }>([
    // Idle rows: their tone until viewed.
    { name: "holds an unviewed idle row's name at its tone", facts: {}, want: { kind: "steady" } },
    { name: "greys a viewed idle row's name", facts: { viewed: true }, want: { kind: "viewed" } },
    { name: "holds an unviewed failed turn's name at its tone", facts: { arm: "turnFailed", tone: "tone-turquoise" }, want: { kind: "steady" } },
    { name: "greys a viewed gate's name", facts: { arm: "permission", viewed: true }, want: { kind: "viewed" } },
    { name: "draws a toneless row's name at rest", facts: { arm: "inactive", tone: "tone-none" }, want: { kind: "rest" } },
    { name: "greys a viewed toneless row's name", facts: { arm: "inactive", tone: "tone-none", viewed: true }, want: { kind: "viewed" } },
    // Working rows: red, viewed or not.
    { name: "holds a running turn's name at its tone", facts: { shown: "turnRunning", arm: "thinking", tone: "tone-red" }, want: { kind: "steady" } },
    { name: "holds a viewed running turn's name at its tone", facts: { shown: "turnRunning", arm: "thinking", tone: "tone-red", viewed: true }, want: { kind: "steady" } },
    { name: "holds a viewed compaction's name at its tone", facts: { shown: "turnRunning", arm: "compacting", tone: "tone-red", viewed: true }, want: { kind: "steady" } },
    // The row's own merge: purple, viewed or not.
    { name: "holds a viewed merging row's name at its tone", facts: { arm: "merging", tone: "tone-purple", viewed: true }, want: { kind: "steady" } },
    // Sync merges: the merge's purple over any arm.
    { name: "holds a sync merge's name at the merge's purple", facts: { shown: "syncMerging", arm: "merging", tone: "tone-purple" }, want: { kind: "toned", tone: "tone-purple" } },
    { name: "holds a sync merge's own turn at the merge's purple", facts: { shown: "syncMerging", arm: "thinking", tone: "tone-red" }, want: { kind: "toned", tone: "tone-purple" } },
    { name: "holds a viewed sync merge's name at the merge's purple", facts: { shown: "syncMerging", arm: "mergeQueued", tone: "tone-purple", viewed: true }, want: { kind: "toned", tone: "tone-purple" } },
    { name: "holds a toneless sync merge's name at the merge's purple", facts: { shown: "syncMerging", tone: "tone-none" }, want: { kind: "toned", tone: "tone-purple" } },
    // Background rows: the unread turn end's color, else rest; viewed grey.
    { name: "draws a background row's done turn end in green", facts: { arm: "idleAsync", tone: "tone-yellow", unreadTurnEnd: "done" }, want: { kind: "toned", tone: "tone-green" } },
    { name: "draws a background row's interrupted turn end in green", facts: { arm: "idleAsync", tone: "tone-yellow", unreadTurnEnd: "interrupted" }, want: { kind: "toned", tone: "tone-green" } },
    { name: "draws a background row's failed turn end in turquoise", facts: { arm: "idleAsync", tone: "tone-yellow", unreadTurnEnd: "turnFailed" }, want: { kind: "toned", tone: "tone-turquoise" } },
    { name: "draws a background row with no unread turn end at rest", facts: { arm: "idleAsync", tone: "tone-yellow" }, want: { kind: "rest" } },
    { name: "greys a viewed background row's name", facts: { arm: "idleAsync", tone: "tone-yellow", viewed: true }, want: { kind: "viewed" } },
  ])("$name", ({ facts, want }) => {
    // Arrange
    const f = { ...IDLE, ...facts };

    // Act
    const got = nameTint(f);

    // Assert
    expect(got).toEqual(want);
  });
});

describe("applyNameTint", () => {
  it.each<{ name: string; tint: NameTint; want: string[] }>([
    { name: "marks a steady name", tint: { kind: "steady" }, want: [NAME_STEADY_CLASS] },
    { name: "marks a toned name steady in its own tone", tint: { kind: "toned", tone: "tone-purple" }, want: [NAME_STEADY_CLASS, "tone-purple"] },
    { name: "marks a viewed name grey", tint: { kind: "viewed" }, want: [NAME_VIEWED_CLASS] },
    { name: "leaves a resting name alone", tint: { kind: "rest" }, want: [] },
  ])("$name", ({ tint, want }) => {
    // Arrange
    const label = document.createElement("span");

    // Act
    applyNameTint(label, tint);

    // Assert
    expect([[...label.classList], label.getAttribute("style")]).toEqual([want, null]);
  });
});
