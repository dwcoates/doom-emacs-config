// @vitest-environment jsdom
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker } from "../../src/clock.js";
import { footerClockSpan } from "../../src/footer/clock-span.js";

const here = path.dirname(fileURLToPath(import.meta.url));

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("footerClockSpan", () => {
  it("marks a countdown as one", () => {
    const span = footerClockSpan(createTicker(1000), "countdown", undefined, () => undefined);
    expect([span.hasAttribute("data-countdown"), span.hasAttribute("data-age")]).toEqual([true, false]);
  });

  it("marks an age as one", () => {
    const span = footerClockSpan(createTicker(1000), "age", undefined, () => undefined);
    expect([span.hasAttribute("data-age"), span.hasAttribute("data-countdown")]).toEqual([true, false]);
  });

  it("wears the class it is given", () => {
    expect(footerClockSpan(createTicker(1000), "age", "footer-rate-age", () => undefined).className).toBe("footer-rate-age");
  });

  it("wears no class when none is given", () => {
    expect(footerClockSpan(createTicker(1000), "age", undefined, () => undefined).hasAttribute("class")).toBe(false);
  });

  it("paints its reading at once", () => {
    const span = footerClockSpan(createTicker(1000), "countdown", undefined, (s, nowMs) => {
      s.textContent = String(nowMs);
    });
    expect(span.textContent).toBe("1000000");
  });

  it("repaints on the shared clock's tick", () => {
    const span = footerClockSpan(createTicker(1000), "countdown", undefined, (s, nowMs) => {
      s.textContent = String(nowMs);
    });
    vi.advanceTimersByTime(1000);
    expect(span.textContent).toBe("1001000");
  });
});

// EVERY FOOTER CLOCK IS BUILT BY footerClockSpan: a span marked by hand
// elsewhere is a second spelling of the same shape, which drifts.
describe("the footer's clock spans share one builder", () => {
  it("marks no clock span by hand outside clock-span.ts", () => {
    const dir = path.join(here, "..", "..", "src", "footer");
    const offenders = readdirSync(dir)
      .filter((file) => file.endsWith(".ts") && file !== "clock-span.ts")
      .filter((file) => /setAttribute\("data-(countdown|age)"/.test(readFileSync(path.join(dir, file), "utf8")));
    expect(offenders).toEqual([]);
  });
});
