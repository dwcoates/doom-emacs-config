/**
 * The hook family, on both planes: the stream says a hook RAN, the attachment
 * says what it did to the tool.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, recordsOfType, theResult, toolUses } from "../harness.js";

const attachmentTypes = (driven: Awaited<ReturnType<typeof driveScenario>>): string[] =>
  recordsOfType(driven.transcript(), "attachment").map((l) => (l.attachment as { type: string }).type);

describe("every hook outcome", () => {
  it("pairs a hook_started with a hook_response carrying the same id", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-success"]);
    const started = ofType(driven, "system", "hook_started")[0];
    const response = ofType(driven, "system", "hook_response")[0];

    // Assert
    expect(response?.hook_id).toBe(started?.hook_id);
  });

  it("records a success as a hook_success attachment", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-success"]);

    // Assert
    expect(attachmentTypes(driven)).toEqual(["hook_success"]);
  });

  it("joins the success attachment to the tool call it guarded", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-success"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      toolUseID: string;
    };

    // Assert
    expect(attachment.toolUseID).toBe((toolUses(driven)[0] as { id: string }).id);
  });

  it("records a block as a hook_blocking_error carrying the blocking text", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-blocked"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
      blockingError: { blockingError: string };
    };

    // Assert
    expect({ type: attachment.type, hasText: attachment.blockingError.blockingError.length > 0 }).toEqual({
      type: "hook_blocking_error",
      hasText: true,
    });
  });

  it("still SUCCEEDS the turn when a hook blocks a tool", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-blocked"]);

    // Assert. The shim never synthesizes a turn terminal from hook activity; a
    // blocked TOOL is not a stopped TURN.
    expect(theResult(driven).subtype).toBe("success");
  });

  it("records a non-blocking failure with its stderr and exit code", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-failed"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as {
      type: string;
      exitCode: number;
    };

    // Assert
    expect({ type: attachment.type, exit: attachment.exitCode }).toEqual({
      type: "hook_non_blocking_error",
      exit: 1,
    });
  });

  it("records a cancellation with FOUR fields and no exit code", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-cancelled"]);
    const attachment = recordsOfType(driven.transcript(), "attachment")[0]?.attachment as Record<
      string,
      unknown
    >;

    // Assert. A cancelled hook produced no stderr, no exit code, no duration.
    expect(Object.keys(attachment).sort()).toEqual(
      ["hookEvent", "hookName", "toolUseID", "type"].sort(),
    );
  });

  it("lets the guarded edit stand when the hook was cancelled", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!hook-cancelled"]);
    const block = (
      driven.transcript().find((l) => l.toolUseResult !== undefined)?.message as {
        content: { is_error: boolean }[];
      }
    ).content[0];

    // Assert
    expect(block?.is_error).toBe(false);
  });

  it("reports the outcome on the hook_response for each family", async () => {
    // Arrange + Act
    const outcomes: string[] = [];
    for (const prompt of ["!hook-success", "!hook-blocked", "!hook-failed", "!hook-cancelled"]) {
      const driven = await driveScenario([prompt]);
      outcomes.push(String(ofType(driven, "system", "hook_response")[0]?.outcome));
    }

    // Assert
    expect(outcomes).toEqual(["success", "error", "error", "cancelled"]);
  });
});
