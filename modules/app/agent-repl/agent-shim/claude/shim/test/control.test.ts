import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ControlDispatch,
  SdkControlTarget,
  ToolPermissionResult,
} from "../src/uds/control.js";
import {
  AckSchema,
  InterruptOutcome,
  InterruptSchema,
  NackSchema,
  PermissionDecision,
  PermissionRequest,
  PermissionResponseSchema,
  SubmitPromptSchema,
} from "../src/uds/proto.js";

interface Recorder {
  target: SdkControlTarget;
  prompts: Array<{ requestId: string; text: string; origin: string; permissionMode?: string }>;
  interrupts: Array<{ requestId: string; hard: boolean }>;
  throwOnPrompt?: string;
}

/**
 * @param outcome what the target reports for an interrupt; defaults to
 *   INTERRUPTED (a live turn was signalled).
 * @param throwOnInterrupt makes the target throw SYNCHRONOUSLY, which is the
 *   Nack path rather than an outcome.
 */
function recorder(
  throwOnPrompt?: string,
  outcome: InterruptOutcome = InterruptOutcome.INTERRUPTED,
  throwOnInterrupt?: string,
): Recorder {
  const prompts: Recorder["prompts"] = [];
  const interrupts: Recorder["interrupts"] = [];
  return {
    prompts,
    interrupts,
    throwOnPrompt,
    target: {
      submitPrompt: (input) => {
        if (throwOnPrompt) throw new Error(throwOnPrompt);
        prompts.push(input);
      },
      interrupt: (input) => {
        if (throwOnInterrupt) throw new Error(throwOnInterrupt);
        interrupts.push(input);
        return outcome;
      },
    },
  };
}

function dispatch(rec: Recorder, sent: PermissionRequest[], ids: string[] = []): ControlDispatch {
  let i = 0;
  return new ControlDispatch(
    rec.target,
    (req) => sent.push(req),
    { newRequestId: () => ids[i++] ?? `auto-${i}` },
  );
}

describe("ControlDispatch.handleSubmitPrompt", () => {
  it("pushes the prompt into the SDK target and Acks", () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r1", text: "hi", origin: "human" }));
    // Assert
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(rec.prompts).toEqual([{ requestId: "r1", text: "hi", origin: "human" }]);
  });

  it("forwards a permission-mode override when present", () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r", text: "x", origin: "human", permissionMode: "acceptEdits" }));
    // Assert
    expect(rec.prompts[0]!.permissionMode).toBe("acceptEdits");
  });

  it("Nacks with the error reason when the target throws", () => {
    // Arrange
    const rec = recorder("boom");
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r2", text: "hi" }));
    // Assert
    expect(receipt.$typeName).toBe(NackSchema.typeName);
    expect((receipt as { reason: string }).reason).toBe("boom");
  });
});

describe("ControlDispatch.handleInterrupt", () => {
  it("forwards the hard flag and Acks", () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1", hard: true }));
    // Assert
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(rec.interrupts).toEqual([{ requestId: "i1", hard: true }]);
  });

  // --- the three-valued outcome ------------------------------------------
  //
  // A consumer watching for the turn's `aborted` result sees the stop and the
  // turn end as two unordered events, so it cannot tell a stop that FAILED
  // from a turn that had ALREADY ENDED. Carrying the verdict on the Ack is
  // what stops every consumer re-deriving it wrongly.

  it("acks INTERRUPTED when a live turn was signalled", () => {
    // Arrange
    const rec = recorder(undefined, InterruptOutcome.INTERRUPTED);
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1", hard: true }));
    // Assert
    expect((receipt as { interruptOutcome: InterruptOutcome }).interruptOutcome)
      .toBe(InterruptOutcome.INTERRUPTED);
  });

  it("acks ALREADY_COMPLETE when no turn was in flight", () => {
    // Arrange
    const rec = recorder(undefined, InterruptOutcome.ALREADY_COMPLETE);
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1", hard: false }));
    // Assert — a SUCCESS: the user asked for the turn to be over, and it is.
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect((receipt as { interruptOutcome: InterruptOutcome }).interruptOutcome)
      .toBe(InterruptOutcome.ALREADY_COMPLETE);
  });

  it("acks FAILED when the stop provably cannot be delivered", () => {
    // Arrange
    const rec = recorder(undefined, InterruptOutcome.FAILED);
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1", hard: true }));
    // Assert
    expect((receipt as { interruptOutcome: InterruptOutcome }).interruptOutcome)
      .toBe(InterruptOutcome.FAILED);
  });

  it("keeps the Nack for a synchronous throw rather than a FAILED ack", () => {
    // Arrange — the target throws before it can decide anything.
    const rec = recorder(undefined, InterruptOutcome.INTERRUPTED, "sdk exploded");
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1", hard: true }));
    // Assert — a Nack is the stronger, established failure signal on this
    // wire; downgrading it to a successful receipt with a sad field would
    // weaken error coverage the daemon already acts on.
    expect(receipt.$typeName).toBe(NackSchema.typeName);
    expect((receipt as { reason: string }).reason).toBe("sdk exploded");
  });

  it("carries the request id alongside the outcome", () => {
    // Arrange
    const rec = recorder(undefined, InterruptOutcome.ALREADY_COMPLETE);
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i9", hard: false }));
    // Assert
    expect((receipt as { requestId: string }).requestId).toBe("i9");
  });
});

describe("ControlDispatch.requestPermission round-trip", () => {
  it("emits a PermissionRequest carrying tool name and input", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    // Act
    void d.requestPermission("Bash", { command: "ls" });
    // Assert
    expect(sent).toHaveLength(1);
    expect(sent[0]!.requestId).toBe("req-1");
    expect(sent[0]!.toolName).toBe("Bash");
    expect(sent[0]!.input).toEqual({ command: "ls" });
  });

  it("blocks until the matching PermissionResponse arrives", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    let settled = false;
    const p = d.requestPermission("Bash", { command: "ls" }).then((r) => {
      settled = true;
      return r;
    });
    // Act / Assert: still pending before a response
    await new Promise<void>((resolve) => setImmediate(resolve));
    expect(settled).toBe(false);
    expect(d.pendingCount()).toBe(1);
    // Now answer
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "req-1", decision: PermissionDecision.ALLOW }));
    const result = await p;
    expect(result.behavior).toBe("allow");
    expect(d.pendingCount()).toBe(0);
  });

  it("resolves ALLOW with the original input when no updated_input is given", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    const p = d.requestPermission("Bash", { command: "ls" });
    // Act
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "req-1", decision: PermissionDecision.ALLOW }));
    // Assert
    const r = (await p) as Extract<ToolPermissionResult, { behavior: "allow" }>;
    expect(r.updatedInput).toEqual({ command: "ls" });
  });

  it("resolves ALLOW-with-edits by replacing the input", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    const p = d.requestPermission("Bash", { command: "ls" });
    // Act
    d.handlePermissionResponse(create(PermissionResponseSchema, {
      requestId: "req-1",
      decision: PermissionDecision.ALLOW,
      updatedInput: { command: "ls -la" },
    }));
    // Assert
    const r = (await p) as Extract<ToolPermissionResult, { behavior: "allow" }>;
    expect(r.updatedInput).toEqual({ command: "ls -la" });
  });

  it("resolves DENY with the deny message", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    const p = d.requestPermission("Bash", { command: "rm -rf /" });
    // Act
    d.handlePermissionResponse(create(PermissionResponseSchema, {
      requestId: "req-1",
      decision: PermissionDecision.DENY,
      denyMessage: "nope",
    }));
    // Assert
    expect(await p).toEqual({ behavior: "deny", message: "nope" });
  });

  it("resolves DENY with a default message when none is given", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    const p = d.requestPermission("Bash", {});
    // Act
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "req-1", decision: PermissionDecision.DENY }));
    // Assert
    expect(await p).toEqual({ behavior: "deny", message: "permission denied" });
  });

  it("ignores a PermissionResponse for an unknown request_id", () => {
    // Arrange
    const d = dispatch(recorder(), []);
    // Act / Assert: no throw, nothing pending
    expect(() =>
      d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "ghost", decision: PermissionDecision.ALLOW })),
    ).not.toThrow();
  });

  it("correlates two concurrent requests independently by request_id", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["a", "b"]);
    const pa = d.requestPermission("Bash", { command: "1" });
    const pb = d.requestPermission("Bash", { command: "2" });
    // Act: answer b first, then a
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "b", decision: PermissionDecision.DENY, denyMessage: "no-b" }));
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "a", decision: PermissionDecision.ALLOW }));
    // Assert
    expect(await pb).toEqual({ behavior: "deny", message: "no-b" });
    expect((await pa).behavior).toBe("allow");
  });

  it("cancelAll resolves every pending request as a deny", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["a", "b"]);
    const pa = d.requestPermission("Bash", {});
    const pb = d.requestPermission("Bash", {});
    // Act
    d.cancelAll("interrupted");
    // Assert
    expect(await pa).toEqual({ behavior: "deny", message: "interrupted" });
    expect(await pb).toEqual({ behavior: "deny", message: "interrupted" });
    expect(d.pendingCount()).toBe(0);
  });
});
