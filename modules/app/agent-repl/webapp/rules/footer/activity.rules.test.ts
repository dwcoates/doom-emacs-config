/**
 * The footer activity's one-helper rules: the allowances' divider is drawn
 * through drawFooterRateDivider (src/footer/activity.ts), and every labeled
 * countdown through footerCountdownSpan.
 */
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import ACTIVITY_SOURCE from "../../src/footer/activity.ts?raw";

describe("the enduring line", () => {
  it("draws the divider between the allowances through the one helper", () => {
    expect(ACTIVITY_SOURCE).not.toMatch(
      /append\([^)]*drawFooterRateSeparator\(\)/,
    );
  });
});

// EVERY LABELED FOOTER COUNTDOWN IS BUILT BY footerCountdownSpan: a countdown
// painting `label + remainingLabel(...)` by hand is a second spelling of the
// same shape, which drifts (one once counted up past its instant).
describe("the footer's countdowns share one builder", () => {
  it("paints label-plus-remaining only inside footerCountdownSpan", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "..", "src", "footer");
    const handPainted = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .flatMap((file) =>
        (readFileSync(path.join(dir, file), "utf8").match(/textContent = (`[^`]*\$\{)?remainingLabel\(/g) ?? []).map(
          (site) => `${file}: ${site}`,
        ),
      );
    expect(handPainted).toEqual(["activity.ts: textContent = `${label}${remainingLabel("]);
  });
});
