/**
 * The engine suites' fakes, where a fake carries behavior of its own.
 */
import { describe, expect, it } from "vitest";
import { ScriptedQuery } from "./fakes.js";

describe("ScriptedQuery.untilPlanUsageReads", () => {
  it("resolves at once when the reads were already made", async () => {
    // Arrange
    const query = new ScriptedQuery();
    await query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET({ skipBehaviors: true });

    // Act / Assert
    await expect(query.untilPlanUsageReads(1)).resolves.toBeUndefined();
  });

  it("resolves at the read that makes the count, not before", async () => {
    // Arrange
    const query = new ScriptedQuery();
    let done = false;
    const waiting = query.untilPlanUsageReads(2).then(() => {
      done = true;
    });

    // Act
    await query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET({ skipBehaviors: true });
    await Promise.resolve();
    const afterOne = done;
    await query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET({ skipBehaviors: true });
    await waiting;

    // Assert
    expect([afterOne, done]).toEqual([false, true]);
  });
});
