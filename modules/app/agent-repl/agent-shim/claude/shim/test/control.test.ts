import { describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";
import { create } from "@bufbuild/protobuf";
import {
  ControlDispatch,
  SdkControlTarget,
  ToolPermissionResult,
} from "../src/uds/control.js";
import {
  Ack,
  AckSchema,
  CancelDetachedAgentsSchema,
  DetachedAgentsCancelledSchema,
  DetachedCancelOutcome,
  DetachedCancelOutcomeSchema,
  DetachedCancelUnsupportedSchema,
  NoDetachedAgentsRunningSchema,
  InterruptOutcome,
  InterruptSchema,
  Nack,
  NackSchema,
  PermissionDecision,
  PermissionRequest,
  PermissionResponseSchema,
  PromptOrigin,
  QueryLiveTasksSchema,
  QuerySelectedModelSchema,
  SetModelSchema,
  SubmitPromptSchema,
} from "../src/uds/proto.js";
import { SYNTHETIC_MODEL } from "../src/model.js";

interface Recorder {
  target: SdkControlTarget;
  prompts: Array<{ requestId: string; text: string; origin: string; promptOrigin: PromptOrigin; permissionMode?: string }>;
  interrupts: Array<{ requestId: string }>;
  cancels: Array<{ requestId: string }>;
  models: Array<{ requestId: string; model: string }>;
  /**
   * What the target answers a detached-agent cancel with, and the rejection a
   * test can force instead. Mutable rather than another positional argument to
   * `recorder`, exactly as `selected` is: the list is long enough already.
   */
  detached: { outcome: DetachedCancelOutcome; throws?: string };
  throwOnPrompt?: string;
  /**
   * What the target reports as its live selection, and the only way a test can
   * drive the two answers a read has: a real model, or the empty "this session
   * has never observed one" that must NACK rather than Ack emptily.
   */
  selected: { value: string; throws?: string };
  /**
   * What the target reports as its live background-task set. Mutable so one
   * test can drive the empty answer (which must ACK, since it is the answer
   * that retires a phantom) and another a populated one.
   */
  liveTasks: { ids: string[]; throws?: string };
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
  const cancels: Recorder["cancels"] = [];
  const models: Recorder["models"] = [];
  const selected: Recorder["selected"] = { value: "claude-sonnet-5" };
  const liveTasks: Recorder["liveTasks"] = { ids: [] };
  const detached: Recorder["detached"] = {
    outcome: create(DetachedCancelOutcomeSchema, {
      outcome: { case: "cancelled", value: create(DetachedAgentsCancelledSchema, { taskIds: ["task-a", "task-b"] }) },
    }),
  };
  return {
    prompts,
    interrupts,
    cancels,
    models,
    throwOnPrompt,
    selected,
    liveTasks,
    detached,
    target: {
      submitPrompt: async (input) => {
        if (throwOnPrompt) throw new Error(throwOnPrompt);
        prompts.push(input);
      },
      interrupt: (input) => {
        if (throwOnInterrupt) throw new Error(throwOnInterrupt);
        interrupts.push(input);
        return outcome;
      },
      cancelDetachedAgents: async (input) => {
        if (detached.throws !== undefined) throw new Error(detached.throws);
        cancels.push(input);
        return detached.outcome;
      },
      setModel: async (input) => {
        models.push(input);
        return input.model;
      },
      selectedModel: () => {
        if (selected.throws !== undefined) throw new Error(selected.throws);
        return selected.value;
      },
      liveTaskIds: () => {
        if (liveTasks.throws !== undefined) throw new Error(liveTasks.throws);
        return liveTasks.ids;
      },
    },
  };
}

interface DispatchOpts {
  /** Delivery verdict the injected sender reports; defaults to attached. */
  delivered?: () => boolean;
  /** Turn-liveness predicate; omitted, the dispatch has no turn owner bound. */
  isTurnLive?: () => boolean;
}

function dispatch(rec: Recorder, sent: PermissionRequest[], ids: string[] = [], opts: DispatchOpts = {}): ControlDispatch {
  let i = 0;
  const delivered = opts.delivered ?? ((): boolean => true);
  return new ControlDispatch(
    rec.target,
    (req) => {
      sent.push(req);
      return delivered();
    },
    {
      newRequestId: () => ids[i++] ?? `auto-${i}`,
      ...(opts.isTurnLive !== undefined ? { isTurnLive: opts.isTurnLive } : {}),
    },
  );
}

function persistedLogs(): Array<Record<string, unknown>> {
  const calls = vi.mocked(writeSync).mock.calls as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
  );
}

describe("ControlDispatch.handleSubmitPrompt", () => {
  it.each([
    ["unspecified", PromptOrigin.UNSPECIFIED],
    ["unknown", 999 as PromptOrigin],
  ])("Nacks a %s prompt origin before calling the SDK target", async (_label, promptOrigin) => {
    const rec = recorder();
    const receipt = await dispatch(rec, []).handleSubmitPrompt(
      create(SubmitPromptSchema, { requestId: "bad-origin", text: "hi", promptOrigin }),
    );

    expect(receipt.$typeName).toBe(NackSchema.typeName);
    if (receipt.$typeName !== NackSchema.typeName) throw new Error("expected Nack");
    expect(receipt.reason).toContain("invalid prompt_origin");
    expect(rec.prompts).toEqual([]);
    expect(persistedLogs()).toEqual(expect.arrayContaining([
      expect.objectContaining({
        level: "error",
        operation: "shim.control.dispatch",
        request_id: "bad-origin",
        context: expect.objectContaining({ prompt_origin: promptOrigin }),
      }),
    ]));
  });

  it("pushes the prompt into the SDK target and Acks", async () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r1", text: "hi", origin: "human", promptOrigin: PromptOrigin.USER_SENT }));
    // Assert
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(rec.prompts).toEqual([{ requestId: "r1", text: "hi", origin: "human", promptOrigin: PromptOrigin.USER_SENT }]);
  });

  it("Nacks blank request ids before calling the SDK target", async () => {
    for (const requestId of ["", " \t"]) {
      const rec = recorder();
      const receipt = await dispatch(rec, []).handleSubmitPrompt(create(SubmitPromptSchema, { requestId, text: "hi" }));
      expect(receipt.$typeName).toBe(NackSchema.typeName);
      expect((receipt as { reason: string }).reason).toBe("SubmitPrompt requires a non-empty request_id");
      expect(rec.prompts).toEqual([]);
    }
  });

  it("forwards a permission-mode override when present", async () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    await d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r", text: "x", origin: "human", promptOrigin: PromptOrigin.USER_SENT, permissionMode: "acceptEdits" }));
    // Assert
    expect(rec.prompts[0]!.permissionMode).toBe("acceptEdits");
  });

  it("Nacks with the error reason when the target throws", async () => {
    // Arrange
    const rec = recorder("boom");
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "r2", text: "hi", promptOrigin: PromptOrigin.USER_SENT }));
    // Assert
    expect(receipt.$typeName).toBe(NackSchema.typeName);
    expect((receipt as { reason: string }).reason).toBe("boom");
  });

  it("records canonical accepted and rejected prompt dispatches with request context", async () => {
    vi.mocked(writeSync).mockClear();
    const accepted = dispatch(recorder(), []);
    await accepted.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "accepted-1", text: "hello", origin: "human", promptOrigin: PromptOrigin.USER_SENT }));
    const rejected = dispatch(recorder("target failed"), []);
    await rejected.handleSubmitPrompt(create(SubmitPromptSchema, { requestId: "rejected-1", text: "bye", origin: "human", promptOrigin: PromptOrigin.USER_SENT }));

    expect(persistedLogs()).toEqual(expect.arrayContaining([
      expect.objectContaining({ operation: "shim.control.dispatch", request_id: "accepted-1", message: "SubmitPrompt accepted by SDK session" }),
      expect.objectContaining({ level: "error", operation: "shim.control.dispatch", request_id: "rejected-1", message: expect.stringContaining("submit-prompt failed") }),
    ]));
  });
});

describe("ControlDispatch.handleSetModel", () => {
  it("forwards a real model and acknowledges only the SDK-confirmed selection", async () => {
    const rec = recorder();
    const receipt = await dispatch(rec, []).handleSetModel(
      create(SetModelSchema, { requestId: "model-1", model: "opus" }),
    );
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(receipt.selectedModel).toBe("opus");
    expect(rec.models).toEqual([{ requestId: "model-1", model: "opus" }]);
  });

  it("rejects <synthetic> without calling the SDK", async () => {
    const rec = recorder();
    const receipt = await dispatch(rec, []).handleSetModel(
      create(SetModelSchema, { requestId: "model-synthetic", model: " <synthetic> " }),
    );
    expect(receipt.$typeName).toBe(NackSchema.typeName);
    if (receipt.$typeName !== NackSchema.typeName) throw new Error("expected a Nack");
    expect(receipt.reason).toContain("not selectable");
    expect(rec.models).toEqual([]);
  });
});

describe("ControlDispatch.handleInterrupt", () => {
  it("forwards the request and Acks", () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1" }));
    // Assert
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(rec.interrupts).toEqual([{ requestId: "i1" }]);
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
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1" }));
    // Assert
    expect((receipt as { interruptOutcome: InterruptOutcome }).interruptOutcome)
      .toBe(InterruptOutcome.INTERRUPTED);
  });

  it("acks ALREADY_COMPLETE when no turn was in flight", () => {
    // Arrange
    const rec = recorder(undefined, InterruptOutcome.ALREADY_COMPLETE);
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1" }));
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
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1" }));
    // Assert
    expect((receipt as { interruptOutcome: InterruptOutcome }).interruptOutcome)
      .toBe(InterruptOutcome.FAILED);
  });

  it("keeps the Nack for a synchronous throw rather than a FAILED ack", () => {
    // Arrange — the target throws before it can decide anything.
    const rec = recorder(undefined, InterruptOutcome.INTERRUPTED, "sdk exploded");
    const d = dispatch(rec, []);
    // Act
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i1" }));
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
    const receipt = d.handleInterrupt(create(InterruptSchema, { requestId: "i9" }));
    // Assert
    expect((receipt as { requestId: string }).requestId).toBe("i9");
  });
});

describe("ControlDispatch.handleCancelDetachedAgents", () => {
  it("forwards the request and Acks the cancelled arm", async () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c1" }));
    // Assert
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect(rec.cancels).toEqual([{ requestId: "c1" }]);
    expect((receipt as Ack).detachedCancelOutcome?.outcome.case).toBe("cancelled");
  });

  it("relays the stopped task ids verbatim", async () => {
    // Arrange — the ids are what the daemon settles bubbles by, so a receipt
    // that renamed or counted them would leave it guessing.
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c1" }));
    // Assert
    const outcome = (receipt as Ack).detachedCancelOutcome?.outcome;
    expect(outcome?.case === "cancelled" ? outcome.value.taskIds : null).toEqual(["task-a", "task-b"]);
  });

  it("acks the nothing_running arm when the session had nothing detached", async () => {
    // Arrange
    const rec = recorder();
    rec.detached.outcome = create(DetachedCancelOutcomeSchema, {
      outcome: { case: "nothingRunning", value: create(NoDetachedAgentsRunningSchema, {}) },
    });
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c1" }));
    // Assert — an ANSWER at this layer, not a wire failure: the session was
    // asked what it had running and said nothing. Whether that reads as a
    // refusal to the user is the daemon's ruling.
    expect(receipt.$typeName).toBe(AckSchema.typeName);
    expect((receipt as Ack).detachedCancelOutcome?.outcome.case).toBe("nothingRunning");
  });

  it("acks the unsupported arm when the stop could not be attempted", async () => {
    // Arrange
    const rec = recorder();
    rec.detached.outcome = create(DetachedCancelOutcomeSchema, {
      outcome: { case: "unsupported", value: create(DetachedCancelUnsupportedSchema, { detail: "no SDK query" }) },
    });
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c1" }));
    // Assert — distinct from nothing_running: a session that could not be
    // asked, not one that answered "nothing".
    const outcome = (receipt as Ack).detachedCancelOutcome?.outcome;
    expect(outcome?.case === "unsupported" ? outcome.value.detail : null).toBe("no SDK query");
  });

  it("keeps the Nack for a rejecting target rather than an unsupported ack", async () => {
    // Arrange
    const rec = recorder();
    rec.detached.throws = "stop_task exploded";
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c1" }));
    // Assert — same discipline handleInterrupt keeps: a Nack is the stronger
    // failure signal, and an ack carrying a sad arm would weaken coverage the
    // daemon acts on.
    expect(receipt.$typeName).toBe(NackSchema.typeName);
    expect((receipt as Nack).reason).toBe("stop_task exploded");
  });

  it("carries the request id alongside the outcome", async () => {
    // Arrange
    const rec = recorder();
    const d = dispatch(rec, []);
    // Act
    const receipt = await d.handleCancelDetachedAgents(create(CancelDetachedAgentsSchema, { requestId: "c9" }));
    // Assert
    expect(receipt.requestId).toBe("c9");
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

  it("rejects and removes pending state when sending the request throws synchronously", async () => {
    vi.mocked(writeSync).mockClear();
    const failure = new Error("daemon send failed");
    const d = new ControlDispatch(
      recorder().target,
      () => {
        throw failure;
      },
      { newRequestId: () => "req-send-failure" },
    );

    await expect(d.requestPermission("Bash", { command: "ls" })).rejects.toBe(failure);
    expect(d.pendingCount()).toBe(0);
    const errors = persistedLogs().filter((record) =>
      record["level"] === "error" &&
      record["operation"] === "shim.control.dispatch",
    );
    expect(errors).toHaveLength(1);
    expect(errors[0]).toMatchObject({
      request_id: "req-send-failure",
      message: "permission request send failed: daemon send failed",
      context: expect.objectContaining({
        tool_name: "Bash",
        pending_count: 0,
      }),
    });
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

  it("warns when cancelAll force-denies asks the user never saw, naming them", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["a", "b"]);
    void d.requestPermission("Bash", {});
    void d.requestPermission("Bash", {});
    vi.mocked(writeSync).mockClear();
    // Act
    d.cancelAll("interrupted");
    // Assert
    const cancelled = persistedLogs().filter((r) => String(r.message).includes("force-denied"));
    expect(cancelled).toHaveLength(1);
    expect(cancelled[0]).toMatchObject({
      level: "warn",
      context: { pending_count: 2, reason: "interrupted", request_ids: ["a", "b"] },
    });
  });
});

describe("ControlDispatch.resendPending", () => {
  it("keeps an undeliverable request pending instead of failing the tool call", async () => {
    // Arrange: no daemon attached, so the sender reports non-delivery.
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"], { delivered: () => false });
    // Act
    let settled = false;
    void d.requestPermission("Bash", { command: "ls" }).then(() => {
      settled = true;
    });
    await new Promise<void>((resolve) => setImmediate(resolve));
    // Assert
    expect(settled).toBe(false);
    expect(d.pendingCount()).toBe(1);
  });

  it("logs an undeliverable request as an error naming the re-send recovery", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"], { delivered: () => false });
    vi.mocked(writeSync).mockClear();
    // Act
    void d.requestPermission("Bash", { command: "ls" });
    // Assert
    const errors = persistedLogs().filter((r) => r["level"] === "error");
    expect(errors).toHaveLength(1);
    expect(String(errors[0]!["message"])).toContain("re-sent on reattach");
  });

  it("re-sends the identical frame for an unanswered request", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    void d.requestPermission("Bash", { command: "ls" });
    // Act
    d.resendPending("daemon reattach");
    // Assert: same identity, same tool, same input.
    expect(sent).toHaveLength(2);
    expect(sent[1]).toEqual(sent[0]);
  });

  it("re-sends multiple pending requests in their original order", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["a", "b", "c"]);
    void d.requestPermission("Bash", { command: "1" });
    void d.requestPermission("Read", { file_path: "/x" });
    void d.requestPermission("Write", { file_path: "/y" });
    sent.length = 0;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(sent.map((req) => req.requestId)).toEqual(["a", "b", "c"]);
  });

  it("does not re-send a request the user answered before the reattach", () => {
    // Arrange: the answer lands first, so the request is no longer open.
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    void d.requestPermission("Bash", { command: "ls" });
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "req-1", decision: PermissionDecision.ALLOW }));
    sent.length = 0;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(sent).toEqual([]);
  });

  it("resolves a request answered immediately after its re-send exactly once", async () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    const resolutions: ToolPermissionResult[] = [];
    void d.requestPermission("Bash", { command: "ls" }).then((r) => resolutions.push(r));
    // Act: the reattach races the answer; the answer arrives just after.
    d.resendPending("daemon reattach");
    d.handlePermissionResponse(create(PermissionResponseSchema, { requestId: "req-1", decision: PermissionDecision.ALLOW }));
    await new Promise<void>((resolve) => setImmediate(resolve));
    // Assert
    expect(resolutions).toHaveLength(1);
    expect(d.pendingCount()).toBe(0);
  });

  it("re-sends the same frame again across two quick reconnects", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"]);
    void d.requestPermission("Bash", { command: "ls" });
    // Act
    d.resendPending("first reattach");
    d.resendPending("second reattach");
    // Assert: three identical frames, one open question.
    expect(sent).toHaveLength(3);
    expect(new Set(sent.map((req) => req.requestId))).toEqual(new Set(["req-1"]));
    expect(d.pendingCount()).toBe(1);
  });

  it("cancels rather than re-asks when no turn is in flight", async () => {
    // Arrange: the turn that raised the ask was interrupted meanwhile.
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"], { isTurnLive: () => false });
    const p = d.requestPermission("Bash", { command: "ls" });
    sent.length = 0;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(sent).toEqual([]);
    expect((await p).behavior).toBe("deny");
  });

  it("re-sends when a turn is in flight", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, ["req-1"], { isTurnLive: () => true });
    void d.requestPermission("Bash", { command: "ls" });
    sent.length = 0;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(sent.map((req) => req.requestId)).toEqual(["req-1"]);
  });

  it("sends nothing when no request is pending", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    const d = dispatch(recorder(), sent, []);
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(sent).toEqual([]);
  });

  it("keeps a request pending when its re-send finds no daemon attached", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    let attached = true;
    const d = dispatch(recorder(), sent, ["req-1"], { delivered: () => attached });
    void d.requestPermission("Bash", { command: "ls" });
    attached = false;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(d.pendingCount()).toBe(1);
  });

  it("keeps a request pending when its re-send throws", () => {
    // Arrange
    const sent: PermissionRequest[] = [];
    let boom = false;
    const d = new ControlDispatch(
      recorder().target,
      (req) => {
        if (boom) throw new Error("socket gone");
        sent.push(req);
        return true;
      },
      { newRequestId: () => "req-1" },
    );
    void d.requestPermission("Bash", { command: "ls" });
    boom = true;
    // Act
    d.resendPending("daemon reattach");
    // Assert
    expect(d.pendingCount()).toBe(1);
  });

  it("logs the re-send failure as an error naming the request", () => {
    // Arrange: the first send succeeds, so a request is open; the re-send throws.
    const sent: PermissionRequest[] = [];
    let attempts = 0;
    const d = new ControlDispatch(
      recorder().target,
      (req) => {
        attempts += 1;
        if (attempts > 1) throw new Error("socket gone");
        sent.push(req);
        return true;
      },
      { newRequestId: () => "req-1" },
    );
    void d.requestPermission("Bash", { command: "ls" });
    vi.mocked(writeSync).mockClear();
    // Act
    d.resendPending("daemon reattach");
    // Assert
    const errors = persistedLogs().filter((r) => r["level"] === "error");
    expect(errors).toHaveLength(1);
    expect(errors[0]).toMatchObject({
      request_id: "req-1",
      message: "permission request re-send failed: socket gone",
    });
  });
});

describe("ControlDispatch.handleQueryLiveTasks", () => {
  it("answers with the live task ids, sorted", () => {
    // Arrange — the daemon's catalog is derived, so this list is the only
    // authority it can close a catalog entry against.
    const rec = recorder();
    rec.liveTasks.ids = ["task-b", "task-a"];

    // Act.
    const receipt = dispatch(rec, []).handleQueryLiveTasks(
      create(QueryLiveTasksSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Ack");
    expect((receipt as Ack).liveTaskSet?.taskIds).toEqual(["task-a", "task-b"]);
  });

  it("ACKs an EMPTY live set rather than nacking it", () => {
    // Arrange — "nothing is running" is the answer that retires a phantom
    // catalog entry. A Nack here would leave the footer claiming work forever.
    const rec = recorder();
    rec.liveTasks.ids = [];

    // Act.
    const receipt = dispatch(rec, []).handleQueryLiveTasks(
      create(QueryLiveTasksSchema, { requestId: "q1" }),
    );

    // Assert — the SET IS PRESENT and empty, which is what distinguishes it
    // from a peer that never answered.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Ack");
    expect((receipt as Ack).liveTaskSet).toBeDefined();
    expect((receipt as Ack).liveTaskSet?.taskIds).toEqual([]);
  });

  it("NACKs when the target throws instead of answering", () => {
    // Arrange — a read that cannot be served is a failure the daemon surfaces,
    // never an assumed-empty set it would then close live tasks against.
    const rec = recorder();
    rec.liveTasks.throws = "the SDK query does not exist yet";

    // Act.
    const receipt = dispatch(rec, []).handleQueryLiveTasks(
      create(QueryLiveTasksSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Nack");
    expect((receipt as Nack).reason).toMatch(/does not exist yet/);
  });
});

describe("ControlDispatch.handleQuerySelectedModel", () => {
  it("answers with the model the session is running", () => {
    // Arrange — the read exists so the daemon does not have to wait for the
    // next submit's system:init to learn what a bare `/model` settled on.
    const rec = recorder();
    rec.selected.value = "claude-opus-5";

    // Act.
    const receipt = dispatch(rec, []).handleQuerySelectedModel(
      create(QuerySelectedModelSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Ack");
    expect((receipt as Ack).selectedModel).toBe("claude-opus-5");
  });

  it("normalizes the synthetic marker away and then refuses", () => {
    // Arrange — the marker is not a model, so reporting it would hand the
    // daemon a value nothing can be spawned under.
    const rec = recorder();
    rec.selected.value = SYNTHETIC_MODEL;

    // Act.
    const receipt = dispatch(rec, []).handleQuerySelectedModel(
      create(QuerySelectedModelSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Nack");
    expect((receipt as Nack).reason).toMatch(/has not observed a real model/);
  });

  it("NACKs rather than acking an empty selection", () => {
    // Arrange — an Ack carrying no model is indistinguishable from a confirmed
    // absence, and the daemon would have to guess which it was. The whole
    // point of the read is that what it commits was verified.
    const rec = recorder();
    rec.selected.value = "";

    // Act.
    const receipt = dispatch(rec, []).handleQuerySelectedModel(
      create(QuerySelectedModelSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Nack");
  });

  it("NACKs when the target throws instead of answering", () => {
    // Arrange — a read that cannot be served is a failure the daemon surfaces,
    // never a picker left naming a model nobody verified.
    const rec = recorder();
    rec.selected.throws = "the SDK query does not exist yet";

    // Act.
    const receipt = dispatch(rec, []).handleQuerySelectedModel(
      create(QuerySelectedModelSchema, { requestId: "q1" }),
    );

    // Assert.
    expect(receipt.$typeName).toBe("agentshim.core.v1.Nack");
    expect((receipt as Nack).reason).toMatch(/does not exist yet/);
  });
});
