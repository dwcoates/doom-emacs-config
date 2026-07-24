import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ControlDispatch,
  SdkControlTarget,
  ToolPermissionResult,
} from "../src/uds/control.js";
import {
  AckSchema,
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

function recorder(throwOnPrompt?: string): Recorder {
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
        interrupts.push(input);
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
