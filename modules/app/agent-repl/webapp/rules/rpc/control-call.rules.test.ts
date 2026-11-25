/**
 * THE TRANSPORT-FAILURE-TO-CHIP SHAPE IS `controlCall`'S ALONE
 * (`src/rpc/control-call.ts`): no other source hand-rolls it.
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
});
