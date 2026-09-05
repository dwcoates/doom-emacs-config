/**
 * The permission family. Each test drives a DIFFERENT `canUseTool`, because the
 * scenario scripts the question and the gate scripts the answer — that split is
 * the point, and a suite that stubbed the answer inside the scenario would be
 * testing the mock against itself.
 */
import { describe, expect, it } from "vitest";

import type { CanUseToolLike } from "../../../src/sdk/types.js";
import { driveScenario, ofType, theResult, toolUseResults } from "../harness.js";

const allowOnce: CanUseToolLike = async (_n, input) =>
  ({ behavior: "allow", updatedInput: input });

const allowStanding: CanUseToolLike = async (_n, input, options) =>
  ({
    behavior: "allow",
    updatedInput: input,
    // The gate echoes the ask's suggestions back. That round trip IS the
    // standing arm; an ask with no suggestions could only ever produce a once.
    updatedPermissions: (options.suggestions ?? []),
  });

const deny: CanUseToolLike = async () =>
  ({ behavior: "deny", message: "the user declined this command" });

describe("the ask itself", () => {
  it("carries the tool_use id as the question's identity", async () => {
    // Arrange
    const seen: string[] = [];
    const spy: CanUseToolLike = async (_n, input, options) => {
      seen.push(options.toolUseID);
      return { behavior: "allow", updatedInput: input };
    };

    // Act
    const driven = await driveScenario(["!perm-allow-once"], { canUseTool: spy });
    // THE tool_use LINE, not merely the first assistant line: the vendor's first
    // API response of a tool turn is `[thinking, tool_use]`, one assistant line
    // per block, so the reasoning line comes first.
    const toolUse = (driven.messages as unknown as Record<string, unknown>[]).find(
      (m) =>
        m.type === "assistant" &&
        (m.message as { content?: { type?: string }[] }).content?.[0]?.type === "tool_use",
    );
    const blockId = ((toolUse?.message as { content: { id?: string }[] }).content[0] ?? {}).id;

    // Assert. AgentPermissionId IS the gated call's tool_use_id, verbatim.
    expect(seen).toEqual([blockId]);
  });

  it("offers suggestions on every ask, so the standing arm is reachable", async () => {
    // Arrange
    let offered = 0;
    const spy: CanUseToolLike = async (_n, input, options) => {
      offered = (options.suggestions ?? []).length;
      return { behavior: "allow", updatedInput: input };
    };

    // Act
    await driveScenario(["!perm-allow-once"], { canUseTool: spy });

    // Assert
    expect(offered).toBeGreaterThan(0);
  });

  it("carries the rendered prompt sentence and the compact display name", async () => {
    // Arrange
    let context: { title?: string; displayName?: string } = {};
    const spy: CanUseToolLike = async (_n, input, options) => {
      context = { title: options.title, displayName: options.displayName };
      return { behavior: "allow", updatedInput: input };
    };

    // Act
    await driveScenario(["!perm-allow-once"], { canUseTool: spy });

    // Assert
    expect(context).toEqual({ title: "Claude wants to run Bash", displayName: "Bash" });
  });
});

describe("allow", () => {
  it("runs the command when the gate allows it once", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-once"], { canUseTool: allowOnce });

    // Assert
    expect((toolUseResults(driven.transcript())[0] as { stdout: string }).stdout).toBe("clean\n");
  });

  it("reports the standing rules the gate echoed back", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-standing"], { canUseTool: allowStanding });

    // Assert
    expect(theResult(driven).result).toBe("Ran the command with 1 standing rule(s).");
  });

  it("reports NO standing rules when the gate allowed only once", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-standing"], { canUseTool: allowOnce });

    // Assert
    expect(theResult(driven).result).toBe("Ran the command with 0 standing rule(s).");
  });

  it("leaves permission_denials empty on an allowed turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-once"], { canUseTool: allowOnce });

    // Assert
    expect(theResult(driven).permission_denials).toEqual([]);
  });
});

describe("deny by user", () => {
  it("turns the gate's message into the tool_result the model sees", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-deny-user"], { canUseTool: deny });

    // Assert
    expect(toolUseResults(driven.transcript())[0]).toBe("Error: the user declined this command");
  });

  it("stamps the record with the denial kind", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-deny-user"], { canUseTool: deny });
    const record = driven.transcript().find((l) => l.toolDenialKind !== undefined);

    // Assert
    expect(record?.toolDenialKind).toBe("user");
  });

  it("lists the refused call under the result's permission_denials", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-deny-user"], { canUseTool: deny });

    // Assert
    expect(theResult(driven).permission_denials).toMatchObject([{ tool_name: "Bash" }]);
  });
});

describe("deny by policy", () => {
  it("never reaches canUseTool at all", async () => {
    // Arrange
    let asked = 0;
    const spy: CanUseToolLike = async (_n, input) => {
      asked++;
      return { behavior: "allow", updatedInput: input };
    };

    // Act
    await driveScenario(["!perm-deny-policy"], { canUseTool: spy });

    // Assert. A rule decided BEFORE an ask would have been made; asking anyway
    // would make the policy arm indistinguishable from a user denial.
    expect(asked).toBe(0);
  });

  it("announces the refusal with a rule decision reason", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-deny-policy"]);

    // Assert
    expect(ofType(driven, "system", "permission_denied")[0]).toMatchObject({
      decision_reason_type: "rule",
    });
  });

  it("stamps the record with the permission-rule denial kind", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-deny-policy"]);
    const record = driven.transcript().find((l) => l.toolDenialKind !== undefined);

    // Assert
    expect(record?.toolDenialKind).toBe("permission-rule");
  });
});

describe("undecidable", () => {
  it("announces the refusal with a CLASSIFIER decision reason, not a rule", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-undecidable"]);

    // Assert. The classifier reason is the closest the SDK offers; the arm is
    // known-open and this is the nearest producer.
    expect(ofType(driven, "system", "permission_denied")[0]).toMatchObject({
      decision_reason_type: "classifier",
    });
  });

  it("never reaches canUseTool either", async () => {
    // Arrange
    let asked = 0;
    const spy: CanUseToolLike = async (_n, input) => {
      asked++;
      return { behavior: "allow", updatedInput: input };
    };

    // Act
    await driveScenario(["!perm-undecidable"], { canUseTool: spy });

    // Assert
    expect(asked).toBe(0);
  });
});

describe("a standing that carries a mode change", () => {
  it("runs the command and reports the echoed standing, mode change included", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-standing-mode"], { canUseTool: allowStanding });

    // Assert
    expect(theResult(driven).result).toBe("Ran the command with 2 standing rule(s).");
  });

  it("reports zero standing rules when the gate allows only once", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-allow-standing-mode"], { canUseTool: allowOnce });

    // Assert
    expect(theResult(driven).result).toBe("Ran the command with 0 standing rule(s).");
  });
});

describe("an ask that offers no standing at all", () => {
  it("runs the command once-allowed, the only shape it can ever produce", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-no-standing"], { canUseTool: allowOnce });

    // Assert
    expect((toolUseResults(driven.transcript())[0] as { stdout: string }).stdout).toBe("one commit\n");
    expect(theResult(driven).result).toBe("Ran the command.");
  });

  it("denies the command when the gate declines, offering nothing to echo", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!perm-no-standing"], { canUseTool: deny });

    // Assert
    expect(theResult(driven).result).toBe("The user declined the command.");
  });
});

describe("a permission ask that PARKS until an interrupt", () => {
  it("opens the ask and never concludes on its own", async () => {
    // !perm-hold's run() never awaits its own askPermission call and parks on
    // ctx.awaitInterrupt() -- a bare driveScenario() would hang the suite
    // forever, which is exactly why it had never been driven at the fake
    // level. Driving the interrupt through `during` releases it in-process.
    const driven = await driveScenario(["!perm-hold"], {
      canUseTool: allowOnce,
      during: async (query, _prompts, messages) => {
        for (let i = 0; i < 4; i++) await new Promise((r) => setImmediate(r));
        expect(messages.some((m) => (m as { type: string }).type === "assistant")).toBe(true);
        await query.interrupt();
      },
    });

    const results = ofType(driven, "result");
    expect(results).toHaveLength(1);
    expect(results[0]?.terminal_reason).toBe("aborted_streaming");
  });
});
