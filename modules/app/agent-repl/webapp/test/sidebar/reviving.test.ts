// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import {
  REVIVE_SHIMMER_PERIOD_MS,
  ReviveShimmer,
  markReviving,
} from "../../src/sidebar/reviving.js";

describe("the reviving shimmer's phase", () => {
  it.each([
    { name: "the first draw starts the pass", draws: [1000], want: 0 },
    { name: "a later draw continues the pass", draws: [1000, 1500], want: 500 },
    {
      name: "a draw past one period wraps into the next pass",
      draws: [1000, 1000 + REVIVE_SHIMMER_PERIOD_MS + 300],
      want: 300,
    },
    { name: "a clock that went backwards never seeks forward", draws: [1000, 400], want: 0 },
  ])("$name", ({ draws, want }) => {
    // Arrange.
    const shimmer = new ReviveShimmer();

    // Act.
    const delays = draws.map((now) => shimmer.delayMs(now));

    // Assert.
    expect(delays[delays.length - 1]).toBe(want);
  });
});

describe("marking a row reviving", () => {
  it("adds the class the shimmer keys on to the name", () => {
    // Arrange.
    const row = document.createElement("div");
    const name = document.createElement("span");

    // Act.
    markReviving(row, name, new ReviveShimmer(), 0);

    // Assert.
    expect(name.classList.contains("reviving")).toBe(true);
  });

  it("seeks a redrawn name to where the pass already is", () => {
    // Arrange: the pass started 700ms before this redraw.
    const shimmer = new ReviveShimmer();
    shimmer.delayMs(5000);
    const name = document.createElement("span");

    // Act.
    markReviving(document.createElement("div"), name, shimmer, 5700);

    // Assert.
    expect(name.style.animationDelay).toBe("-700ms");
  });

  it("stamps the row's hook attribute", () => {
    // Arrange.
    const row = document.createElement("div");

    // Act.
    markReviving(row, document.createElement("span"), new ReviveShimmer(), 0);

    // Assert.
    expect(row.getAttribute("data-reviving")).toBe("true");
  });
});
