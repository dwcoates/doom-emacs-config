import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { MalformedView } from "../src/rpc/malformed.js";
import { tokenHeatColor } from "../src/token-heat.js";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("tokenHeatColor", () => {
  it.each([
    [0, 0, 1, 0],
    [1 / 6, 0, 1, 50],
    [1 / 3, 1, 2, 0],
    [0.5, 1, 2, 50],
    [2 / 3, 2, 3, 0],
    [5 / 6, 2, 3, 50],
    [1, 2, 3, 100],
  ])("places %d between heat color %d and %d at %d%%", (position, lower, upper, share) => {
    expect(tokenHeatColor(position, "p")).toBe(
      `color-mix(in oklab, var(--token-heat-${String(lower)}), var(--token-heat-${String(upper)}) ${String(share)}%)`,
    );
  });

  it.each([[-0.01], [1.01], [Number.NaN], [Number.POSITIVE_INFINITY]])("refuses %d", (position) => {
    expect(() => tokenHeatColor(position, "p")).toThrow(MalformedView);
  });

  it.each(["src/footer/strip.ts"])(
    "%s colors its token figure through the shared helper",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Assert
      expect(source).toContain("tokenHeatColor(");
      expect(source).not.toContain("--token-heat-0");
    },
  );
});
