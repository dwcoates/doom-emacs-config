// @vitest-environment jsdom
// Where a dragged waiting merge goes. See src/merge-queue-move.ts.
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { moveToAction, placeBeside } from "../src/merge-queue-move.js";

/** Waiting merges named NAMES, each a line with its workspace ref. */
function waitingLines(names: string[]) {
  const lines = names.map(() => document.createElement("div"));
  const refs = new Map(lines.map((line, i) => [line, { id: names[i], dir: `/w/${names[i]}` }] as const));
  return { lines, refs };
}

describe("placeBeside", () => {
  it.each([
    { name: "ahead of the merge it was dropped before", before: 0, want: { case: "aheadOf", id: "w1" } },
    { name: "behind the last merge when dropped at the end", before: null, want: { case: "behind", id: "w3" } },
    { name: "behind the last other merge when the dropped merge was the last", item: 2, before: null, want: { case: "behind", id: "w2" } },
  ])("places a dropped merge $name", ({ item = 1, before, want }) => {
    // Arrange
    const { lines, refs } = waitingLines(["w1", "w2", "w3"]);

    // Act
    const place = placeBeside(lines, lines[item], before === null ? null : lines[before], refs as never);

    // Assert
    expect([place.case, place.value?.id]).toEqual([want.case, want.id]);
  });

  it("refuses an anchor whose workspace was never named", () => {
    // Arrange
    const { lines, refs } = waitingLines(["w1", "w2"]);
    const stranger = document.createElement("div");

    // Act / Assert
    expect(() => placeBeside(lines, lines[1], stranger, refs as never)).toThrow(/invariant/);
  });

  it("refuses a drop at the end of a line holding only the dropped merge", () => {
    // Arrange
    const { lines, refs } = waitingLines(["w1"]);

    // Act / Assert
    expect(() => placeBeside(lines, lines[0], null, refs as never)).toThrow(/invariant/);
  });
});

describe("moveToAction", () => {
  it("moves the workspace's merge to the place, verbatim", () => {
    // Arrange
    const workspace = { id: "w2", dir: "/w/w2" };
    const place = { case: "aheadOf" as const, value: { id: "w1", dir: "/w/w1" } };

    // Act
    const action = moveToAction(workspace as never, place as never);

    // Assert
    expect(action).toEqual({ case: "moveTo", value: { workspace, place } });
  });
});

describe("every queue a reader drags places its drop through this module", () => {
  /** Every TypeScript source under src/. */
  function sources(dir: string): string[] {
    return readdirSync(dir).flatMap((name) => {
      const at = path.join(dir, name);
      return statSync(at).isDirectory() ? sources(at) : at.endsWith(".ts") ? [at] : [];
    });
  }
  const src = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

  it("builds a moveTo action nowhere else", () => {
    // Act
    const builders = sources(src).filter((file) => /case:\s*"moveTo"/.test(readFileSync(file, "utf8")));

    // Assert
    expect(builders.map((file) => path.relative(src, file))).toEqual(["merge-queue-move.ts"]);
  });

  it("is the placing of the sidebar's Merge Queue", () => {
    // Act
    const users = ["sidebar/merge-queue-view.ts"].map((file) =>
      /import \{[^}]*\bplaceBeside\b[^}]*\} from "[./]+merge-queue-move\.js"/.test(readFileSync(path.join(src, file), "utf8")),
    );

    // Assert
    expect(users).toEqual([true]);
  });
});
