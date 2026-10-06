// @vitest-environment jsdom
/**
 * THE SAME CODE ON BOTH SURFACES (owner, 2026-10-01): the footer's allowance
 * percentage and the topbar's context figure are both colored through
 * `pressurePercentColor`. A surface that grew its own gradient would pass every
 * color assertion it wrote for itself, so this asserts the CALL as well as the
 * color.
 *
 * A SOURCE SCAN, not a module mock: the scheduled runner shares one module
 * graph across a worker's files, so a `vi.mock` here would observe whichever
 * copy an earlier file already imported.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { TokenBreakdownViewSchema, TopbarContextChipSchema } from "../../proto/gen/ts/frontend/v1/topbar_pb";
import { drawFooterPercent } from "../src/footer/activity.js";
import { pressurePercentColor } from "../src/pressure-color.js";
import { drawTopbarContextChip } from "../src/topbar/context-chip.js";
import { topbarContext } from "./topbar/fixtures.js";

const here = path.dirname(fileURLToPath(import.meta.url));

/** The color PERCENT paints, as the DOM normalizes it. */
function painted(percent: number): string {
  const probe = document.createElement("span");
  probe.style.color = pressurePercentColor(percent);
  return probe.style.color;
}

describe("the pressure color's call sites", () => {
  it.each(["src/footer/activity.ts", "src/topbar/context-chip.ts"])(
    "%s colors through pressurePercentColor and interpolates nothing itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("pressurePercentColor("), source.includes("percentGradientColor(")]).toEqual([
        true,
        false,
      ]);
    },
  );

  it.each(["src/footer/activity.ts", "src/feed/asks/cold-gate.ts"])(
    "%s colors the cold gate's figure through coldGateFigureColor and interpolates nothing itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("coldGateFigureColor("), source.includes("percentGradientColor(")]).toEqual([
        true,
        false,
      ]);
    },
  );

  it("paints the footer's 42% and the topbar's 42%-full window alike", () => {
    // Arrange
    const { tc } = topbarContext();
    const chip = create(TopbarContextChipSchema, {
      text: "420k",
      breakdown: create(TokenBreakdownViewSchema, { sections: [] }),
      windowFill: 0.42,
    });
    // Act
    const footer = drawFooterPercent(0.42).style.color;
    const topbar = drawTopbarContextChip(chip, tc).querySelector<HTMLElement>(".topbar-context-figure")?.style.color;
    // Assert
    expect([footer, topbar]).toEqual([painted(42), painted(42)]);
  });
});
