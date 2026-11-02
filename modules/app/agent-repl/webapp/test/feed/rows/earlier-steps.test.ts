// @vitest-environment jsdom
// A turn's earlier-steps indicator: a face of the feed's walk, no fold.
// See src/feed/rows/earlier-steps.ts.
import { describe, expect, it } from "vitest";
import type { Control } from "../../../src/control.js";
import { drawFeedEarlierSteps, EARLIER_STEPS_ATTRIBUTE, type EarlierStepsWalk } from "../../../src/feed/rows/earlier-steps.js";
import { stopTicking } from "../../../src/feed/ticking.js";
import { WALK_SPINNER_CLASS } from "../../../src/feed/walk-control.js";

/** A walk recording what the row asked of it. */
function recordingWalk() {
  const ran: Control[] = [];
  const attached: Control[] = [];
  let detached = 0;
  const walk: EarlierStepsWalk = {
    run: (indicator) => ran.push(indicator),
    attach: (indicator) => {
      attached.push(indicator);
      return () => (detached += 1);
    },
  };
  return { walk, ran, attached, detached: () => detached };
}

/** The row's indicator. */
function indicatorOf(row: HTMLElement): Control {
  const found = row.querySelector<HTMLElement>(`[${EARLIER_STEPS_ATTRIBUTE}]`);
  if (found === null) throw new Error("no earlier-steps indicator drawn");
  return found as Control;
}

describe("drawFeedEarlierSteps", () => {
  it("draws one spinner and no text, band or label", () => {
    // Arrange / Act
    const row = drawFeedEarlierSteps(recordingWalk().walk);

    // Assert
    expect({
      text: row.textContent,
      className: row.className,
      spinners: row.querySelectorAll(`.${WALK_SPINNER_CLASS}`).length,
      indicatorClass: indicatorOf(row).className,
    }).toEqual({ text: "", className: "feed-earlier-steps", spinners: 1, indicatorClass: "feed-load-more" });
  });

  it("makes its indicator a face of the feed's walk as it is drawn", () => {
    // Arrange
    const { walk, attached } = recordingWalk();

    // Act
    const row = drawFeedEarlierSteps(walk);

    // Assert
    expect(attached).toEqual([indicatorOf(row)]);
  });

  it("runs the walk from its indicator when the indicator is clicked", () => {
    // Arrange
    const { walk, ran } = recordingWalk();
    const row = drawFeedEarlierSteps(walk);

    // Act
    indicatorOf(row).click();

    // Assert
    expect(ran).toEqual([indicatorOf(row)]);
  });

  it("leaves the walk when its row is discarded", () => {
    // Arrange
    const recording = recordingWalk();
    const row = drawFeedEarlierSteps(recording.walk);

    // Act
    stopTicking(row);

    // Assert
    expect(recording.detached()).toBe(1);
  });
});
