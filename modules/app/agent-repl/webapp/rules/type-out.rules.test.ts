/** typeOut (src/type-out.ts) is the loop every typed-out surface runs, none hand-rolling its own. */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

describe("the type-out loop", () => {
  it("is the loop every typed-out surface runs, none hand-rolling its own", () => {
    // Arrange: the pacing step is the loop's alone.
    const sources = ["../src/feed/cards/response.ts", "../src/feed/title-reveal.ts"].map((path) =>
      readFileSync(new URL(path, import.meta.url), "utf8"),
    );
    // Act
    const shapes = sources.map((source) => [source.includes("typeOut("), source.includes("advanceReveal(")]);
    // Assert
    expect(shapes).toEqual([
      [true, false],
      [true, false],
    ]);
  });
});
