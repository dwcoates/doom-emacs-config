import { describe, expect, it } from "vitest";
import { createPaintReporter } from "../../src/feed/painted.js";

describe("createPaintReporter", () => {
  it("answers paintedAt from the controller's own record", () => {
    const reporter = createPaintReporter((id) => (id === "r1" ? 42 : null));
    expect(reporter.watch.paintedAt("r1")).toBe(42);
    expect(reporter.watch.paintedAt("r2")).toBeNull();
  });

  it("tells every subscriber each painted row with its instant", () => {
    const reporter = createPaintReporter(() => null);
    const seen: [string, number][] = [];
    reporter.watch.onPainted((id, at) => seen.push([id, at]));

    reporter.report(["a", "b"], 7);

    expect(seen).toEqual([
      ["a", 7],
      ["b", 7],
    ]);
  });

  it("tells an unsubscribed listener nothing", () => {
    const reporter = createPaintReporter(() => null);
    const seen: string[] = [];
    const off = reporter.watch.onPainted((id) => seen.push(id));
    off();

    reporter.report(["a"], 1);

    expect(seen).toEqual([]);
  });
});
