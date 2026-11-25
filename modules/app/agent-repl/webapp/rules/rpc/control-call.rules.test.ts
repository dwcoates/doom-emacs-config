/**
 * THE TRANSPORT-FAILURE-TO-CHIP SHAPE IS `controlCall`'S ALONE
 * (`src/rpc/control-call.ts`): no other source hand-rolls it. Every source
 * that files a transport failure on the chip asks `isPlannedRefusal` first, so
 * a deploy handover's quiet window is never filed as a fault.
 */
import path from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder, srcDir as SRC } from "../files-under.js";

describe("the control call's call sites", () => {
  it("leave the transport-failure-to-chip shape to controlCall", () => {
    // Arrange
    const offenders: string[] = [];
    // Act
    for (const [file, code] of codeUnder(SRC)) {
      if (file === path.join("rpc", "control-call.ts")) continue;
      if (/controlPlaneFailed\([^)]*callFailure\(err\)\.text/.test(code)) offenders.push(file);
    }
    // Assert
    expect(offenders).toEqual([]);
  });

  it("ask isPlannedRefusal before filing a transport failure on the chip", () => {
    // Arrange: a source that files callFailure's text on the chip is one that
    // reads a transport failure, which may be a planned refusal.
    const offenders: string[] = [];
    // Act
    for (const [file, code] of codeUnder(SRC)) {
      if (!/controlPlaneFailed\(/.test(code) || !/callFailure\(/.test(code)) continue;
      if (!/isPlannedRefusal\(err\)/.test(code)) offenders.push(file);
    }
    // Assert
    expect(offenders).toEqual([]);
  });
});
