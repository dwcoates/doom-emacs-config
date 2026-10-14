// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { createTicker, type Ticker } from "../src/clock.js";
import {
  CLOCK_RESERVE_PROPERTY,
  ELAPSED_RESERVE_LABELS,
  RESERVED_CLOCK_CLASS,
  liveElapsedClock,
  liveElapsedReading,
  reservedLiveElapsedClock,
  reservedQuietClock,
  reservedSettledElapsedClock,
  reservedTickingClock,
  settledElapsedClock,
} from "../src/elapsed-clock.js";
import { stopTicking } from "../src/feed/ticking.js";
import { fitsReserve, reserveLabelsCss } from "../src/width-reserve.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { filesUnder } from "./files-under.js";

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

describe("liveElapsedReading", () => {
  it.each([
    { name: "zero at its start", started: 1_000_000, now: 1_000_000, want: "0s" },
    { name: "seconds", started: 1_000_000, now: 1_007_000, want: "7s" },
    { name: "minutes and seconds", started: 1_000_000, now: 1_065_000, want: "1m 5s" },
    { name: "nothing before its start", started: 1_000_000, now: 999_000, want: "0s" },
  ])("reads $name", ({ started, now, want }) => {
    expect(liveElapsedReading(started, now)).toBe(want);
  });

  it("is what a live elapsed clock paints", () => {
    // Arrange
    const started = 1_000_000 - 65_432;
    // Act
    const el = liveElapsedClock(ticker, "c", started);
    vi.advanceTimersByTime(4_000);
    // Assert
    expect(el.textContent).toBe(liveElapsedReading(started, Date.now()));
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

describe("ELAPSED_RESERVE_LABELS", () => {
  it.each(["0s", "59s", "5m", "59m 59s", "1h", "23h 59m", "999h 59m"])("reserves room for %s", (reading) => {
    // Arrange / Act / Assert
    expect(fitsReserve(reading, ELAPSED_RESERVE_LABELS)).toBe(true);
  });
});

describe("reservedLiveElapsedClock", () => {
  it("wears its class and the reserved-clock class", () => {
    // Arrange / Act
    const el = reservedLiveElapsedClock(ticker, "merge-clock", 1_000_000);
    // Assert
    expect([...el.classList]).toEqual(["merge-clock", RESERVED_CLOCK_CLASS]);
  });

  it("hands its ghost the elapsed reserve", () => {
    // Arrange / Act
    const el = reservedLiveElapsedClock(ticker, "c", 1_000_000);
    // Assert
    expect(el.style.getPropertyValue(CLOCK_RESERVE_PROPERTY)).toBe(reserveLabelsCss(ELAPSED_RESERVE_LABELS));
  });

  it("paints and repaints the elapsed reading", () => {
    // Arrange
    const el = reservedLiveElapsedClock(ticker, "c", 1_000_000 - 65_000);
    // Act
    vi.advanceTimersByTime(3_000);
    // Assert
    expect(el.textContent).toBe("1m 8s");
  });
});

describe("reservedSettledElapsedClock", () => {
  it("shows the finished span in the elapsed reserve, ticking nothing", () => {
    // Arrange / Act
    const el = reservedSettledElapsedClock("shell-clock", 90_000);
    // Assert
    expect([
      el.textContent,
      el.classList.contains(RESERVED_CLOCK_CLASS),
      el.style.getPropertyValue(CLOCK_RESERVE_PROPERTY),
      el.hasAttribute("data-ticking"),
    ]).toEqual(["1m 30s", true, reserveLabelsCss(ELAPSED_RESERVE_LABELS), false]);
  });
});

describe("reservedQuietClock", () => {
  it("paints the quiet reading", () => {
    // Arrange / Act
    const el = reservedQuietClock(ticker, "shell-quiet", 1_000_000 - 12_000);
    // Assert
    expect(el.textContent).toBe("quiet for 12s");
  });

  it("reserves room for the quiet reading at its widest", () => {
    // Arrange / Act
    const el = reservedQuietClock(ticker, "shell-quiet", 1_000_000);
    // Assert
    expect(el.style.getPropertyValue(CLOCK_RESERVE_PROPERTY)).toContain("quiet for 00m 00s");
  });
});

describe("reservedTickingClock", () => {
  it("reports a reading wider than its reserve once, through the canonical log", async () => {
    // Arrange
    const capture = captureLogRecords("info");
    // Act
    const el = reservedTickingClock(ticker, "c", ["00s"], () => "1000h 0m");
    vi.advanceTimersByTime(2_000);
    // Assert
    const record = await forwardedRecord(capture, "clock.reserve-exceeded");
    expect([
      record.level.case,
      record.context,
      capture.sent.filter((r) => r.operation === "clock.reserve-exceeded").length,
      el.textContent,
    ]).toMatchObject(["error", { class_name: "c", reading: "1000h 0m", reserve: ["00s"] }, 1, "1000h 0m"]);
  });

  it("reports nothing while its readings fit", () => {
    // Arrange
    const capture = captureLogRecords("info");
    // Act
    reservedTickingClock(ticker, "c", ["00s"], () => "5s");
    vi.advanceTimersByTime(2_000);
    capture.logger.flush();
    // Assert
    expect(capture.sent.filter((r) => r.operation === "clock.reserve-exceeded")).toEqual([]);
  });
});

describe("the clocks share one ticking builder", () => {
  it("subscribes the shared clock in exactly one place in elapsed-clock.ts", () => {
    // Arrange -- a hand-rolled clock would take a second subscription of its own.
    const text = readFileSync(path.join(here, "..", "src", "elapsed-clock.ts"), "utf8");
    // Act
    const subscriptions = text.match(/\btick\(el, ticker,/g) ?? [];
    // Assert
    expect(subscriptions).toHaveLength(1);
  });
});

// EVERY CARD HEAD'S CLOCK IS RESERVED: a head that built its clock with the
// unreserved builders would slide its neighbors on every tick again.
describe("the card heads' clocks share the reserved builders", () => {
  it.each(["feed/rows/subagent.ts", "feed/cards/shell.ts", "feed/merge/merge.ts", "feed/merge/tab-strip.ts"])(
    "builds no unreserved clock in %s",
    (file) => {
      // Arrange
      const text = readFileSync(path.join(here, "..", "src", file), "utf8");
      // Act
      const unreserved = text.match(/\b(live|settled)ElapsedClock\(/g) ?? [];
      // Assert
      expect(unreserved).toEqual([]);
    },
  );
});


// EVERY ELAPSED CLOCK IS BUILT HERE: a span painted with a bare elapsed reading
// anywhere else is a second spelling of the same shape, which drifts.
describe("the page's elapsed clocks share one builder", () => {
  it("paints no bare elapsed reading by hand outside elapsed-clock.ts", () => {
    const src = path.join(here, "..", "src");
    const offenders = filesUnder(src)
      .filter((file) => path.basename(file) !== "elapsed-clock.ts")
      .filter((file) => /\.textContent = format(Ticked)?Elapsed\(/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
