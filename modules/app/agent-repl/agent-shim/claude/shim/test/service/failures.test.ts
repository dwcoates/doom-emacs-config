/**
 * Every refusal the shim can mint, asserted arm by arm.
 *
 * WHAT THESE GUARD. A failure message whose oneof is UNSET is illegal on the
 * wire and unactionable at the daemon, and it is exactly what an inline
 * `create(FailureSchema, { detail })` produces when someone forgets the arm.
 * One row per arm is what proves the closed union and the proto oneof still
 * line up: a proto arm that is renamed or removed fails to compile here, and
 * an arm nobody constructs shows up as a missing row.
 */
import { create } from "@bufbuild/protobuf";
import { containing } from "../expect-shapes.js";
import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";
import { conversationv1, shimv1 } from "../../src/proto.js";
import * as failures from "../../src/service/failures.js";
import { TRANSCRIPT_QUIET_AFTER_MS, type TranscriptSummary } from "../../src/engine/transcripts.js";

describe("startSessionFailure", () => {
  it.each([
    ["unknownSession"],
    ["alreadyStarted"],
    ["conversationOwned"],
  ] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.startSessionFailure({ kind }, "why");

    // Assert.
    expect(failure.cause.case).toBe(kind);
  });

  it("states the vendorStartFailed arm", () => {
    // Arrange, Act.
    const failure = failures.startSessionFailure({ kind: "vendorStartFailed", retry: "retryable", cause: "vendor" }, "why");

    // Assert.
    expect(failure.cause.case).toBe("vendorStartFailed");
  });

  it.each([["network"], ["vendor"]] as const)(
    "sets the %s cause on a retryable vendorStartFailed, never leaving the oneof unset",
    (cause) => {
      // Arrange, Act.
      const failure = failures.startSessionFailure({ kind: "vendorStartFailed", retry: "retryable", cause }, "why");

      // Assert.
      const retry = failure.cause.case === "vendorStartFailed" ? failure.cause.value.retry : undefined;
      expect(retry?.case === "retryable" ? retry.value.cause.case : undefined).toBe(cause);
    },
  );

  it.each([["retryable"], ["rejected"]] as const)(
    "sets the %s retry label on vendorStartFailed, never leaving the oneof unset",
    (retry) => {
      // Arrange, Act.
      const failure = failures.startSessionFailure({ kind: "vendorStartFailed", retry, cause: "vendor" }, "why");

      // Assert.
      expect(failure.cause.case === "vendorStartFailed" ? failure.cause.value.retry.case : undefined).toBe(retry);
    },
  );

  it("carries the cold evidence verbatim so the daemon can offer a remediation", () => {
    // Arrange.
    const cold = create(conversationv1.SessionColdSchema, { contextTokens: 40_000n });

    // Act.
    const failure = failures.startSessionFailure({ kind: "cold", cold }, "too cold");

    // Assert.
    expect(failure.cause).toEqual({ case: "cold", value: cold });
  });

  it("carries the failed lock holder's binary", () => {
    // Arrange, Act.
    const failure = failures.startSessionFailure(
      { kind: "lockHolderUnavailable", binary: "/bin/shim-lock", how: { kind: "spawnFailed", osError: "spawn ENOENT" } },
      "helper would not start",
    );

    // Assert.
    expect(failure.cause.case === "lockHolderUnavailable" ? failure.cause.value.failure?.binary : undefined).toBe(
      "/bin/shim-lock",
    );
  });

  it.each([
    ["a spawn failure", { kind: "spawnFailed", osError: "spawn ENOENT" } as const, { case: "spawnFailed", value: containing({ osError: "spawn ENOENT" }) }],
    ["an exit", { kind: "exited", code: 1, stderr: "EACCES" } as const, { case: "exited", value: containing({ code: 1, stderr: "EACCES" }) }],
    ["a signal", { kind: "signaled", signal: "SIGSEGV", stderr: "" } as const, { case: "signaled", value: containing({ signal: "SIGSEGV", stderr: "" }) }],
    ["a wrong line", { kind: "misanswered", line: "ok" } as const, { case: "misanswered", value: containing({ line: "ok" }) }],
    ["no answer", { kind: "silent", timeoutMs: 5000 } as const, { case: "silent", value: containing({ timeoutMs: 5000 }) }],
  ])("carries %s as its own LockHolderFailure arm", (_name, how, want) => {
    // Arrange, Act.
    const failure = failures.lockHolderFailure("/bin/shim-lock", how);

    // Assert.
    expect(failure.how).toEqual(want);
  });

  it("carries the human detail alongside the machine-readable arm", () => {
    // Arrange, Act.
    const failure = failures.startSessionFailure({ kind: "alreadyStarted" }, "already up");

    // Assert.
    expect(failure.detail).toBe("already up");
  });
});

describe("startSessionRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.startSessionRefused({ kind: "unknownSession" }, "gone");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("startSessionStarted", () => {
  it("wraps the session's opening facts as the response's success arm", () => {
    // Arrange.
    const session = create(conversationv1.SessionStartedSchema, { vendorSessionId: "v-1" });

    // Act.
    const response = failures.startSessionStarted(session);

    // Assert.
    expect(response.result).toEqual({
      case: "success",
      value: containing({ session }),
    });
  });
});

describe("setSessionModelFailure", () => {
  it.each([["modelNotInCatalog"], ["noSession"], ["vendorRefused"]] as const)(
    "states the %s arm",
    (kind) => {
      // Arrange, Act.
      const failure = failures.setSessionModelFailure({ kind }, "why");

      // Assert.
      expect(failure.cause.case).toBe(kind);
    },
  );

  it("carries the cold evidence when the switch itself would go cold", () => {
    // Arrange.
    const cold = create(conversationv1.SessionColdSchema, { contextTokens: 1n });

    // Act.
    const failure = failures.setSessionModelFailure({ kind: "cold", cold }, "cold");

    // Assert.
    expect(failure.cause).toEqual({ case: "cold", value: cold });
  });
});

describe("setSessionModelRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.setSessionModelRefused({ kind: "noSession" }, "no session");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("setSessionPermissionModeFailure", () => {
  it.each([["noSession"], ["vendorRefused"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.setSessionPermissionModeFailure({ kind }, "why");

    // Assert.
    expect(failure.kind.case).toBe(kind);
  });
});

describe("setSessionPermissionModeRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.setSessionPermissionModeRefused({ kind: "vendorRefused" }, "no");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("setSessionEffortFailure", () => {
  it.each([["notSupported"], ["noSession"], ["vendorRefused"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.setSessionEffortFailure({ kind }, "why");

    // Assert.
    expect(failure.cause.case).toBe(kind);
  });

  it("carries the detail", () => {
    // Arrange, Act.
    const failure = failures.setSessionEffortFailure({ kind: "noSession" }, "why");

    // Assert.
    expect(failure.detail).toBe("why");
  });
});

describe("setSessionEffortRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.setSessionEffortRefused({ kind: "vendorRefused" }, "no");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("hibernateError", () => {
  it.each([["turnInFlight"], ["noSession"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const error = failures.hibernateError({ kind });

    // Assert.
    expect(error.kind.case).toBe(kind);
  });
});

describe("hibernateRefused", () => {
  it("wraps the error as the response's error arm", () => {
    // Arrange, Act.
    const response = failures.hibernateRefused({ kind: "turnInFlight" });

    // Assert.
    expect(response.result.case).toBe("error");
  });
});

describe("hibernateAcked", () => {
  it("acks so the daemon may stand the shim down", () => {
    // Arrange, Act.
    const response = failures.hibernateAcked();

    // Assert.
    expect(response.result.case).toBe("success");
  });
});

describe("killSessionFailure", () => {
  it.each([["noSession"], ["queryRefusedToEnd"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.killSessionFailure({ kind }, "why");

    // Assert.
    expect(failure.cause.case).toBe(kind);
  });

  it("NAMES what is live so the daemon can say what forcing would destroy", () => {
    // Arrange.
    const live = create(conversationv1.SessionLiveSchema, {
      turnInFlight: create(conversationv1.TurnIdSchema, { value: "t-1" }),
    });

    // Act.
    const failure = failures.killSessionFailure({ kind: "live", live }, "busy");

    // Assert.
    expect(failure.cause).toEqual({ case: "live", value: live });
  });
});

describe("killSessionRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.killSessionRefused({ kind: "noSession" }, "none");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("killSessionClosed", () => {
  it("says HOW the session ended", () => {
    // Arrange.
    const closed = create(conversationv1.SessionKilledSchema, {
      how: { case: "idle", value: create(conversationv1.SessionKilledIdleSchema, {}) },
    });

    // Act.
    const response = failures.killSessionClosed(closed);

    // Assert.
    expect(response.result).toEqual({
      case: "success",
      value: containing({ closed }),
    });
  });
});

describe("startTurnFailure", () => {
  it.each([["noSession"], ["vendorRefused"], ["queryDead"]] as const)(
    "states the %s arm",
    (kind) => {
      // Arrange, Act.
      const failure = failures.startTurnFailure({ kind }, "why");

      // Assert.
      expect(failure.kind.case).toBe(kind);
    },
  );

  it("states the turnAlreadyOpen arm", () => {
    // Arrange, Act.
    const failure = failures.startTurnFailure({ kind: "turnAlreadyOpen" }, "why");

    // Assert.
    expect(failure.kind.case).toBe("turnAlreadyOpen");
  });

  it("states no keep-alive fact on turnAlreadyOpen: the arm carries no field at all", () => {
    // Arrange, Act: the retired `keepalive` flag (tag 1, reserved) is gone, so
    // a keep-alive can never reach the daemon through this refusal.
    const fields = shimv1.StartTurnTurnAlreadyOpenSchema.fields.map((field) => field.name);

    // Assert.
    expect(fields).toEqual([]);
  });
});

describe("startTurnRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.startTurnRefused({ kind: "queryDead" }, "dead");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("startTurnAccepted", () => {
  it("returns the delivered prompt as the record the daemon persists", () => {
    // Arrange.
    const prompt = create(conversationv1.AgentPromptSchema, {
      id: create(conversationv1.TurnIdSchema, { value: "t-7" }),
    });

    // Act.
    const response = failures.startTurnAccepted(prompt, failures.emptyOpeningPage());

    // Assert.
    expect(response.result).toEqual({
      case: "success",
      value: containing({ prompt }),
    });
  });

  it("always carries a page, because an absent one and an empty one look alike", () => {
    // Arrange.
    const prompt = create(conversationv1.AgentPromptSchema, {
      id: create(conversationv1.TurnIdSchema, { value: "t-7" }),
    });

    // Act.
    const response = failures.startTurnAccepted(prompt, failures.emptyOpeningPage());

    // Assert.
    const success = response.result.value as shimv1.StartTurnSuccess;
    expect(success.page).toBeDefined();
    expect(success.page?.boundary.case).toBe("floor");
  });
});

describe("updateAgentFailure", () => {
  it.each([
    ["unknownAgent"],
    ["noOpenAsk"],
    ["answerMismatch"],
    ["nothingRunning"],
    ["noSession"],
    ["notDeliverable"],
    ["agentBusy"],
  ] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.updateAgentFailure({ kind }, "why");

    // Assert.
    expect(failure.kind.case).toBe(kind);
  });
});

describe("updateAgentRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.updateAgentRefused({ kind: "answerMismatch" }, "echo mismatch");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("updateAgentDelivered", () => {
  it("says only that it was delivered; the effects arrive on the agent's stream", () => {
    // Arrange, Act.
    const response = failures.updateAgentDelivered();

    // Assert.
    expect(response.result.case).toBe("success");
  });
});

describe("killTurnFailure", () => {
  it.each([["notTheOpenTurn"], ["noTurnOpen"], ["noSession"]] as const)(
    "states the %s arm",
    (kind) => {
      // Arrange, Act.
      const failure = failures.killTurnFailure({ kind }, "why");

      // Assert.
      expect(failure.cause.case).toBe(kind);
    },
  );

  it("names the transitive refusal set when the turn still has live work", () => {
    // Arrange.
    const live = create(conversationv1.TurnLiveSchema, {
      liveWork: [create(conversationv1.DetachedWorkIdSchema, { value: "b1" })],
    });

    // Act.
    const failure = failures.killTurnFailure({ kind: "live", live }, "busy");

    // Assert.
    expect(failure.cause).toEqual({ case: "live", value: live });
  });
});

describe("rollBackSessionFailure", () => {
  it.each([["noSession"], ["promptNotRecorded"], ["firstPrompt"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.rollBackSessionFailure({ kind }, "why");

    // Assert.
    expect(failure.cause.case).toBe(kind);
  });

  it("names the first unseen prompt's vendor uuid", () => {
    // Arrange, Act.
    const failure = failures.rollBackSessionFailure({ kind: "unseenPrompt", vendorPromptUuid: "u-9" }, "why");

    // Assert.
    expect(failure.cause.case === "unseenPrompt" ? failure.cause.value.vendorPromptUuid : "").toBe("u-9");
  });

  it("carries the vendor's refusal of the cut verbatim", () => {
    // Arrange, Act.
    const failure = failures.rollBackSessionFailure({ kind: "vendorRefused", vendorMessage: "Resume rejected" }, "why");

    // Assert.
    expect(failure.cause.case === "vendorRefused" ? failure.cause.value.vendorMessage : "").toBe("Resume rejected");
  });

  it("carries the vendor's reason the files cannot be restored verbatim", () => {
    // Arrange, Act.
    const failure = failures.rollBackSessionFailure({ kind: "filesNotRestorable", vendorMessage: "no checkpoint" }, "why");

    // Assert.
    expect(failure.cause.case === "filesNotRestorable" ? failure.cause.value.vendorMessage : "").toBe("no checkpoint");
  });

  it("carries the shim's account in the detail", () => {
    // Arrange, Act.
    const failure = failures.rollBackSessionFailure({ kind: "firstPrompt" }, "the prompt opens it");

    // Assert.
    expect(failure.detail).toBe("the prompt opens it");
  });
});

describe("rollBackSessionRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.rollBackSessionRefused({ kind: "noSession" }, "idle");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("rollBackSessionSucceeded", () => {
  it("states no restored files when the files were kept", () => {
    // Arrange, Act.
    const response = failures.rollBackSessionSucceeded(undefined);

    // Assert.
    expect(response.result.case === "success" ? response.result.value.filesRestored : "not success").toBeUndefined();
  });

  it("names every restored path when the files were restored", () => {
    // Arrange, Act.
    const response = failures.rollBackSessionSucceeded(["/ws/a.ts", "/ws/b.ts"]);

    // Assert.
    expect(response.result.case === "success" ? response.result.value.filesRestored?.paths : []).toEqual([
      "/ws/a.ts",
      "/ws/b.ts",
    ]);
  });
});

describe("killTurnRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.killTurnRefused({ kind: "noTurnOpen" }, "idle");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("killTurnKilled", () => {
  it("says whether anything died with the turn", () => {
    // Arrange.
    const killed = create(conversationv1.TurnKilledSchema, {
      how: { case: "agentOnly", value: create(conversationv1.TurnKilledAgentOnlySchema, {}) },
    });

    // Act.
    const response = failures.killTurnKilled(killed);

    // Assert.
    expect(response.result).toEqual({
      case: "success",
      value: containing({ killed }),
    });
  });
});

describe("stopBashFailure", () => {
  it.each([["unknownWork"], ["alreadyEnded"]] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.stopBashFailure({ kind }, "why");

    // Assert.
    expect(failure.kind.case).toBe(kind);
  });
});

describe("stopBashRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.stopBashRefused({ kind: "alreadyEnded" }, "over");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("stopBashStopped", () => {
  it("acks the stop; the run's terminal arrives on its own stream", () => {
    // Arrange, Act.
    const response = failures.stopBashStopped();

    // Assert.
    expect(response.result.case).toBe("success");
  });
});

describe("detachForegroundFailure", () => {
  it.each([
    ["unknownUnit"],
    ["alreadyConcluded"],
    ["notDetachable"],
    ["noSession"],
    ["notInForeground"],
  ] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const failure = failures.detachForegroundFailure({ kind }, "why");

    // Assert.
    expect(failure.kind.case).toBe(kind);
  });
});

describe("detachForegroundRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.detachForegroundRefused({ kind: "notDetachable" }, "no");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("detachForegroundDetached", () => {
  it("acks the detach; the turn announces the work itself", () => {
    // Arrange, Act.
    const response = failures.detachForegroundDetached();

    // Assert.
    expect(response.result.case).toBe("success");
  });
});

describe("readHistoryFailure", () => {
  it.each([["unknownAgent"], ["stalePointer"], ["storeUnavailable"]] as const)(
    "states the %s arm",
    (kind) => {
      // Arrange, Act.
      const failure = failures.readHistoryFailure({ kind }, "why");

      // Assert.
      expect(failure.kind.case).toBe(kind);
    },
  );
});

describe("readHistoryRefused", () => {
  it("wraps the failure as the response's failure arm", () => {
    // Arrange, Act.
    const response = failures.readHistoryRefused({ kind: "storeUnavailable" }, "store down");

    // Assert.
    expect(response.result.case).toBe("failure");
  });
});

describe("readHistoryPage", () => {
  it("returns the page as the response's success arm", () => {
    // Arrange.
    const page = create(conversationv1.HistoryPageSchema, {
      boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
    });

    // Act.
    const response = failures.readHistoryPage(page);

    // Assert.
    expect(response.result).toEqual({
      case: "success",
      value: containing({ page }),
    });
  });
});

describe("sessionFault", () => {
  it.each([
    ["storeUnreachable"],
    ["converterDefect"],
    ["logSinkPoisoned"],
    ["keepaliveFailed"],
    ["vendorQueryFailed"],
    ["networkUnreachable"],
  ] as const)("states the %s arm", (kind) => {
    // Arrange, Act.
    const fault = failures.sessionFault({ kind }, "store-client", "why");

    // Assert.
    expect(fault.kind.case).toBe(kind);
  });

  it("names the component that broke, not just the kind", () => {
    // Arrange, Act.
    const fault = failures.sessionFault({ kind: "storeUnreachable" }, "store-client", "ECONNREFUSED");

    // Assert.
    expect({ component: fault.component, detail: fault.detail }).toEqual({
      component: "store-client",
      detail: "ECONNREFUSED",
    });
  });
});

describe("invalidArgument", () => {
  it("refuses a malformed request at the transport rather than answering it", () => {
    // Arrange, Act.
    const error = failures.invalidArgument("turn is unset");

    // Assert.
    expect(ConnectError.from(error).code).toBe(Code.InvalidArgument);
  });
});

describe("notFound", () => {
  it("closes a refused stream open at the transport, where a stream can say it", () => {
    // Arrange, Act.
    const error = failures.notFound("no such work");

    // Assert.
    expect(ConnectError.from(error).code).toBe(Code.NotFound);
  });
});

describe("unimplemented", () => {
  it("answers Unimplemented rather than an empty success the caller cannot read", () => {
    // Arrange, Act.
    const error = failures.unimplemented("GetWorkflow");

    // Assert.
    expect(ConnectError.from(error).code).toBe(Code.Unimplemented);
  });

  it("names the rpc so a log says which verb was refused", () => {
    // Arrange, Act.
    const error = failures.unimplemented("StopWorkflow");

    // Assert.
    expect(error.message).toContain("shim.v1.StopWorkflow");
  });
});

describe("internalFromUnknown", () => {
  it("carries the detail of a thrown value that is not an Error at all", () => {
    // A `throw "..."` loses its detail entirely if only Error is read.
    // Arrange, Act.
    const failure = failures.internalFromUnknown("StartTurn", "the fold came apart");

    // Assert.
    expect(failure.rawMessage).toBe("shim.v1.StartTurn: the fold came apart");
  });

  it("codes a non-Error throw Internal, because it is a defect and not a refusal", () => {
    // Arrange, Act.
    const failure = failures.internalFromUnknown("StartTurn", 7);

    // Assert.
    expect(failure.code).toBe(Code.Internal);
  });
});

describe("transcriptsRead", () => {
  /** A summary with everything stated, which each case narrows. */
  const FULL: TranscriptSummary = {
    vendorSessionId: "s-1",
    mtimeMs: 1_700_000_000_000,
    bound: false,
    facts: {
      contextTokens: 4_242,
      sawUsage: true,
      lastRequestAtMs: 1_699_999_000_000,
      cacheTtlMs: 300_000,
      lastModel: "claude-opus-5",
      opening: "explain hash tables",
      prompts: 3,
    },
  };

  /** The one transcript in the response, or a thrown explanation. */
  function only(summary: TranscriptSummary): shimv1.Transcript {
    const response = failures.transcriptsRead([summary]);
    if (response.result.case !== "success") throw new Error("expected a success");
    const [transcript] = response.result.value.transcripts;
    if (transcript === undefined) throw new Error("expected one transcript");
    return transcript;
  }

  it("answers an empty list as a SUCCESS", () => {
    // Arrange, Act.
    const response = failures.transcriptsRead([]);

    // Assert: a directory with no transcripts is an answer, not a failure.
    expect(response.result.case === "success" ? response.result.value.transcripts : undefined).toEqual([]);
  });

  it("carries the transcript's own figures", () => {
    // Arrange, Act.
    const transcript = only(FULL);

    // Assert.
    expect([
      transcript.vendorSessionId,
      transcript.contextTokens,
      transcript.lastRequestAtMs,
      transcript.lastModel?.name,
      transcript.opening,
      transcript.prompts,
    ]).toEqual(["s-1", 4_242n, 1_699_999_000_000n, "claude-opus-5", "explain hash tables", 3]);
  });

  it("LEAVES context_tokens UNSET when the transcript stated no usage", () => {
    // Arrange: a conversation that never reached the model.
    const transcript = only({ ...FULL, facts: { ...FULL.facts, sawUsage: false, contextTokens: 0 } });

    // Assert: a zero that cannot be told from an absence ranks it cheapest.
    expect(transcript.contextTokens).toBeUndefined();
  });

  it("LEAVES last_request_at_ms UNSET when the transcript stated no request", () => {
    // Arrange.
    const transcript = only({ ...FULL, facts: { ...FULL.facts, lastRequestAtMs: 0 } });

    // Assert.
    expect(transcript.lastRequestAtMs).toBeUndefined();
  });

  it("sets the bound marker for this shim's own conversation", () => {
    // Arrange, Act.
    const transcript = only({ ...FULL, bound: true });

    // Assert: presence is the fact.
    expect(transcript.bound).toBeDefined();
  });

  it("leaves the bound marker absent for every other conversation", () => {
    // Arrange, Act.
    const transcript = only(FULL);

    // Assert.
    expect(transcript.bound).toBeUndefined();
  });

  it("states the clear's instant when the last boundary was a clear", () => {
    // Arrange, Act.
    const transcript = only({ ...FULL, facts: { ...FULL.facts, clearedAtMs: 1_699_000_000_000 } });

    // Assert.
    expect(transcript.cleared?.atMs).toBe(1_699_000_000_000n);
  });

  it("states the write instant AND the quiet rule for an active transcript", () => {
    // Arrange, Act.
    const transcript = only({ ...FULL, activeAtMs: 1_700_000_000_000 });

    // Assert: the daemon's refusal names the rule it applied, not a verdict.
    expect([transcript.active?.atMs, transcript.active?.quietAfterMs]).toEqual([
      1_700_000_000_000n,
      BigInt(TRANSCRIPT_QUIET_AFTER_MS),
    ]);
  });
});

describe("transcriptsRefused", () => {
  it("states the no_project_dir arm with the path it searched", () => {
    // Arrange, Act.
    const response = failures.transcriptsRefused({ kind: "no_project_dir", searchedPath: "/p" });

    // Assert.
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    expect([failure?.cause.case, failure?.cause.case === "noProjectDir" ? failure.cause.value.searchedPath : undefined]).toEqual([
      "noProjectDir",
      "/p",
    ]);
  });

  it("states the unreadable arm with the read's own account", () => {
    // Arrange, Act.
    const response = failures.transcriptsRefused({
      kind: "unreadable",
      searchedPath: "/p",
      detail: "EACCES",
    });

    // Assert.
    const failure = response.result.case === "failure" ? response.result.value : undefined;
    expect([failure?.cause.case, failure?.cause.case === "unreadable" ? failure.cause.value.detail : undefined]).toEqual([
      "unreadable",
      "EACCES",
    ]);
  });
});
