import { describe, expect, it } from "vitest";
import { MalformedView } from "../src/rpc/malformed.js";
import { heatColor } from "../src/heat.js";

describe("heatColor", () => {
  it.each([
    [0, 0, 1, 0],
    [1 / 6, 0, 1, 50],
    [1 / 3, 1, 2, 0],
    [0.5, 1, 2, 50],
    [2 / 3, 2, 3, 0],
    [5 / 6, 2, 3, 50],
    [1, 2, 3, 100],
  ])("places %d between heat color %d and %d at %d%%", (position, lower, upper, share) => {
    expect(heatColor(position, "p")).toBe(
      `color-mix(in oklab, var(--token-heat-${String(lower)}), var(--token-heat-${String(upper)}) ${String(share)}%)`,
    );
  });

  it.each([[-0.01], [1.01], [Number.NaN], [Number.POSITIVE_INFINITY]])("refuses %d", (position) => {
    expect(() => heatColor(position, "p")).toThrow(MalformedView);
  });
});
