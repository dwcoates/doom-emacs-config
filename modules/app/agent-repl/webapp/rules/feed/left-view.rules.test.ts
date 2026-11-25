import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

/**
 * THE ONE DETECTOR: both "left the view" consumers go through it, and neither
 * builds an observer of its own, so the two cannot disagree about what "left"
 * means.
 */
describe("left-view's call sites", () => {
  /** The source of the src module at PATH. */
  const source = (path: string): string => readFileSync(join(process.cwd(), path), "utf8");

  it.each([["src/feed/selection-visibility.ts"], ["src/feed/out-of-view-collapse.ts"]])(
    "%s detects departures through createLeftViewWatch and no observer of its own",
    (path) => {
      // Arrange
      const text = source(path);
      // Act
      const uses = [text.includes("createLeftViewWatch("), /IntersectionObserver\s*\(|new Ctor\(/.test(text)];
      // Assert
      expect(uses).toEqual([true, false]);
    },
  );
});
