/**
 * The task-board and SendMessage family.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, toolUseResults, toolUses } from "../harness.js";

describe("the task board", () => {
  it("creates two tasks and links one to the other, so an edge exists at all", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!task-create"]);
    const names = toolUses(driven).map((t) => t.name);

    // Assert
    expect(names).toEqual(["TaskCreate", "TaskCreate", "TaskUpdate"]);
  });

  it("states the DAG edge as blockedBy on the second task", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!task-create"]);
    const link = toolUses(driven)[2] as { input: { blockedBy: string[] } };

    // Assert
    expect(link.input.blockedBy).toEqual(["1"]);
  });

  it("reports a status change as a from/to pair, not a bare new value", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!task-change"]);
    const changed = toolUseResults(driven.transcript())[0] as {
      statusChange: { from: string; to: string };
    };

    // Assert
    expect(changed.statusChange).toEqual({ from: "pending", to: "in_progress" });
  });

  it("reports a rejection as success:false with an error and no status change", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!task-reject"]);
    const rejected = toolUseResults(driven.transcript())[0] as Record<string, unknown>;

    // Assert
    expect({ success: rejected.success, change: rejected.statusChange }).toEqual({
      success: false,
      change: undefined,
    });
  });
});

describe("SendMessage", () => {
  it("omits resumedAgentId when the recipient was already live", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!send-message"]);
    const sent = toolUseResults(driven.transcript())[0] as Record<string, unknown>;

    // Assert. Its ABSENCE is the queued-to-live discriminator.
    expect({ success: sent.success, resumed: sent.resumedAgentId }).toEqual({
      success: true,
      resumed: undefined,
    });
  });

  it("carries resumedAgentId when the vendor resumed an idle recipient", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!send-message-resumed"]);
    const sent = toolUseResults(driven.transcript())[0] as { resumedAgentId: string };

    // Assert
    expect(sent.resumedAgentId).toMatch(/^a[a-f0-9]{16}$/);
  });

  it("names the resumed agent's output file, which the sidecar then tails", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!send-message-resumed"]);
    const sent = toolUseResults(driven.transcript())[0] as { message: string };

    // Assert
    expect(sent.message).toContain(".output");
  });

  it("reports a stopped recipient as a failure rather than a silent no-op", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!send-message-refused"]);
    const sent = toolUseResults(driven.transcript())[0] as { success: boolean };

    // Assert
    expect(sent.success).toBe(false);
  });
});
