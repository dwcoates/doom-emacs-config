/**
 * THE FEED CONTROLLER'S RULES over src/feed/feed-view.ts and the sources that
 * call it: one place tells an open's answer from a resume's, tears a row down,
 * places a new row, and looks a held row up.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import feedViewSource from "../../src/feed/feed-view.ts?raw";
import { codeOf } from "../../test/source-text.js";
import { filesUnder } from "../files-under.js";

describe("createFeedController: resuming", () => {
  it("is the one place an open's answer is told apart from a resume's", () => {
    // Arrange
    const sources = filesUnder(join(process.cwd(), "src"));
    // Act
    const offenders = sources.filter(
      (file) => !file.endsWith("feed-view.ts") && /\.applyResumed\(/.test(readFileSync(file, "utf8")),
    );
    // Assert
    expect(offenders).toEqual([]);
  });
});

describe("discardRow", () => {
  it("is the only place feed-view.ts tears a row down", () => {
    // Arrange -- a hand-rolled drop would repeat the bubble's disposal.
    const disposals = feedViewSource.match(/\.bubble\?\.dispose\(\)/g) ?? [];
    // Act + Assert
    expect(disposals).toHaveLength(1);
  });
});

describe("createFeedController: every row is placed by its order key", () => {
  it("never places a new row by arrival order (source scan)", () => {
    // Arrange — feed-view.ts, comments stripped.
    const source = codeOf(
      readFileSync(join(process.cwd(), "src/feed/feed-view.ts"), "utf8"),
    );
    // Act — every insertion into the order, and every arrival-order index.
    const insertions = [
      ...source.matchAll(/\border\.splice\(([^,)]*),\s*0\b/g),
    ].map((m) => m[1].trim());
    const arrival = [
      ...source.matchAll(/\border\.(?:push|unshift)\(/g),
      ...source.matchAll(/\(\s*[^()]*order\.length[^()]*,\s*0\s*,/g),
      ...source.matchAll(
        /(?:adopt|insertAt|adoptPageRow)\([^;]*order\.length/g,
      ),
    ].map((m) => m[0]);
    // Assert — one insertion, at the index the key's binary search found.
    expect({ insertions, arrival }).toEqual({
      insertions: ["index"],
      arrival: [],
    });
  });
});

describe("createFeedController's held-row lookup", () => {
  it("is the one place a row in the order is looked up as held", () => {
    // Arrange
    const source = readFileSync(join(__dirname, "../../src/feed/feed-view.ts"), "utf8");

    // Act
    const throws = source.match(/is ordered but not held/g) ?? [];

    // Assert
    expect(throws).toHaveLength(1);
  });
});
