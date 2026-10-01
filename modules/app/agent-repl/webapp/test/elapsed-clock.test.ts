// @vitest-environment jsdom
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker, type Ticker } from "../src/clock.js";
import { liveElapsedClock, settledElapsedClock } from "../src/elapsed-clock.js";
import { stopTicking } from "../src/feed/ticking.js";

const here = path.dirname(fileURLToPath(import.meta.url));

let ticker: Ticker;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
  ticker = createTicker(1000);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("liveElapsedClock", () => {
  it("wears the class it is given", () => {
    expect(liveElapsedClock(ticker, "merge-clock", 1_000_000).className).toBe("merge-clock");
  });

  it("paints the time elapsed since its start at once", () => {
    expect(liveElapsedClock(ticker, "c", 1_000_000 - 65_000).textContent).toBe("1m 5s");
  });

  it("repaints on the shared clock's tick", () => {
    const el = liveElapsedClock(ticker, "c", 1_000_000);
    vi.advanceTimersByTime(3_000);
    expect(el.textContent).toBe("3s");
  });

  it("stops repainting once it is discarded", () => {
    const el = liveElapsedClock(ticker, "c", 1_000_000);
    stopTicking(el);
    vi.advanceTimersByTime(3_000);
    expect(el.textContent).toBe("0s");
  });
});

describe("settledElapsedClock", () => {
  it("wears the class it is given", () => {
    expect(settledElapsedClock("shell-clock", 5_000).className).toBe("shell-clock");
  });

  it("shows the finished span", () => {
    expect(settledElapsedClock("c", 90_000).textContent).toBe("1m 30s");
  });

  it("never ticks", () => {
    const el = settledElapsedClock("c", 90_000);
    vi.advanceTimersByTime(5_000);
    expect([el.textContent, el.hasAttribute("data-ticking")]).toEqual(["1m 30s", false]);
  });
});

/** Every .ts file under DIR, recursively. */
function sources(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return sources(full);
    return name.endsWith(".ts") ? [full] : [];
  });
}

// EVERY ELAPSED CLOCK IS BUILT HERE: a span painted with a bare elapsed reading
// anywhere else is a second spelling of the same shape, which drifts.
describe("the page's elapsed clocks share one builder", () => {
  it("paints no bare elapsed reading by hand outside elapsed-clock.ts", () => {
    const src = path.join(here, "..", "src");
    const offenders = sources(src)
      .filter((file) => path.basename(file) !== "elapsed-clock.ts")
      .filter((file) => /\.textContent = format(Ticked)?Elapsed\(/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
