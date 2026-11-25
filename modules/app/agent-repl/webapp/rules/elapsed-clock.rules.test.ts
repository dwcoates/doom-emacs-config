/**
 * The page's elapsed clocks share src/elapsed-clock.ts's builders: one ticking
 * subscription, reserved clocks in every card head, and no bare elapsed
 * reading painted by hand.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

const here = path.dirname(fileURLToPath(import.meta.url));

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
