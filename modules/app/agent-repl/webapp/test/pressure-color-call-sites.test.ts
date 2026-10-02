// @vitest-environment jsdom
/**
 * THE SAME CODE ON BOTH SURFACES (owner, 2026-10-01): the footer's allowance
 * percentage and the topbar's context figure are both colored through
 * `pressurePercentColor`. A surface that grew its own gradient would pass every
 * color assertion it wrote for itself, so this asserts the CALL, not the color.
 */
import { create } from "@bufbuild/protobuf";
import { beforeEach, describe, expect, it, vi } from "vitest";
import {
  TokenBreakdownViewSchema,
  TopbarContextChipSchema,
} from "../../proto/gen/ts/frontend/v1/topbar_pb";
import { pressurePercentColor } from "../src/pressure-color.js";
import { drawFooterPercent } from "../src/footer/activity.js";
import { drawTopbarContextChip } from "../src/topbar/context-chip.js";
import { topbarContext } from "./topbar/fixtures.js";

vi.mock("../src/pressure-color.js", async (importOriginal) => {
  const original = await importOriginal<typeof import("../src/pressure-color.js")>();
  return { pressurePercentColor: vi.fn(original.pressurePercentColor) };
});

beforeEach(() => {
  vi.mocked(pressurePercentColor).mockClear();
});

describe("the pressure color's call sites", () => {
  it("colors the footer's percentage through the shared helper", () => {
    // Arrange, Act.
    drawFooterPercent(0.42);

    // Assert.
    expect(vi.mocked(pressurePercentColor)).toHaveBeenCalledWith(42);
  });

  it("colors the topbar's context figure through the same helper", () => {
    // Arrange.
    const { tc } = topbarContext();
    const chip = create(TopbarContextChipSchema, {
      text: "420k",
      breakdown: create(TokenBreakdownViewSchema, { sections: [] }),
      windowFill: 0.42,
    });

    // Act.
    drawTopbarContextChip(chip, tc);

    // Assert.
    expect(vi.mocked(pressurePercentColor)).toHaveBeenCalledWith(42);
  });
});
