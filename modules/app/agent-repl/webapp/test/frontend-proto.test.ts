/**
 * frontend-proto — decode + loud validation of agentshim.frontend.v1 protojson
 * frames (S9 typed ConversationItem envelope + ContentDelta typing +
 * SessionInitView). One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  UNSUPPORTED_SHAPES,
  decodeFrontendFrame,
  decodeFailureCardView,
  decodeFailureKind,
  isVisuallySupportedFrame,
} from "../src/frontend-proto.js";

/** Wrap a plain object as a protojson string and decode it. */
function decode(obj: unknown): ReturnType<typeof decodeFrontendFrame> {
  return decodeFrontendFrame(JSON.stringify(obj));
}

// Minimal-but-valid sample payloads for each frame variant.
const WS_STATE = {
  workspace: "ws",
  sessionId: "s1",
  state: "RENDER_STATE_IDLE",
  connectivity: "SESSION_CONNECTIVITY_OPERATIONAL",
  status: "SESSION_STATUS_READY",
  controllerGenerationId: "g1",
  activeFaults: [],
};
const SESSION_VIEW = {
  workspace: "ws",
  sessionId: "s1",
  model: "claude",
  totalTokens: "1200",
  totalCostUsd: 0.5,
  contextWindow: "200000",
  modelOptions: [],
};
const TYPING = { workspace: "ws", fence: "s1", delta: { uuid: "u1", blockIndex: 0, text: "hi" } };
const TASK_CATALOG = {
  workspace: "ws",
  fence: "s1",
  tasks: [{ taskId: "t1", agent: {}, description: "d", running: {} }],
};
const CONV_DELTA = {
  workspace: "ws",
  fence: "s1",
  throughSeq: "5",
  items: [{ uuid: "u1", tsMs: "1700000000000", assistantMessage: { content: [{ text: { text: "hi" } }] } }],
};
const SESSION_INIT = { workspace: "ws", fence: "s1", init: { model: "claude", cwd: "/w" } };
const COMMAND_ACK = { requestId: "r1", ok: true };
const DAEMON_VIEW = {
  bootId: "b_abc",
  protocolVersion: "1",
  daemonBinaryMtimeMs: "1700000000000",
  daemonVersion: "v9",
};
const HEARTBEAT = {
  workspace: "ws",
  fence: "s1",
  progress: { toolUseId: "tu1", toolName: "Bash", elapsedSeconds: 12.5 },
};
const QUEUE = {
  workspace: "ws",
  fence: "s1",
  entries: [
    {
      id: "q1",
      text: "run this later",
      queuedAtMs: "1700000000000",
      holdForTurnEnd: { rationale: "independent" },
    },
  ],
};
const SNAPSHOT = { workspaces: [WS_STATE], sessions: [SESSION_VIEW], catalogs: [TASK_CATALOG] };
const WORKSPACE_AVAILABLE = {
  jobId: "job-1",
  finalName: "new-workspace",
  worktreePath: "/worktrees/new-workspace",
  sessionId: "session-1",
};

const TOKEN_UTILIZATION = {
  agentReplSessionId: "session-1", claudeSessionId: "claude-1", rootTurnId: "turn-1", apiRequestId: "request-1", apiMessageId: "msg-1", model: "claude-opus", mainAgent: {},
  usage: { inputTokens: "10", outputTokens: "20", cacheReadInputTokens: "30", cacheCreationInputTokens: "40", cacheCreation: { ephemeral5mInputTokens: "4", ephemeral1hInputTokens: "36" }, serverToolUse: { webSearchRequests: "2", webFetchRequests: "3" }, serviceTier: "priority", speed: "fast", inferenceGeo: "us", outputDetails: { thinkingTokens: "5" }, iterations: [{ sampling: { inputTokens: "1", outputTokens: "2", cacheReadInputTokens: "3", cacheCreationInputTokens: "4", cacheCreation: { ephemeral5mInputTokens: "1", ephemeral1hInputTokens: "3" }, model: "claude-opus" } }, { compaction: { inputTokens: "5", outputTokens: "6", cacheReadInputTokens: "7", cacheCreationInputTokens: "8", cacheCreation: { ephemeral5mInputTokens: "2", ephemeral1hInputTokens: "6" } } }, { advisor: { inputTokens: "9", outputTokens: "10", cacheReadInputTokens: "11", cacheCreationInputTokens: "12", cacheCreation: { ephemeral5mInputTokens: "3", ephemeral1hInputTokens: "9" }, model: "claude-haiku" } }, { fallback: { inputTokens: "13", outputTokens: "14", cacheReadInputTokens: "15", cacheCreationInputTokens: "16", cacheCreation: { ephemeral5mInputTokens: "4", ephemeral1hInputTokens: "12" }, model: "claude-sonnet" } }], cacheDiagnostic: { modelChanged: { cacheMissedInputTokens: "17" } }, cacheRates: { totalPromptInputTokens: "80", cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 }, fallbackCredit: { applied: true }, unmodeledUsage: { vendorField: { preserved: "exactly" } }, rawUsage: { inputTokens: "10", outputTokens: "20", cacheReadInputTokens: "30", cacheCreationInputTokens: "40", cacheCreation: { ephemeral_5m_input_tokens: 4, retainedNestedField: { exact: ["value"] } }, serverToolUse: { web_search_requests: 2 }, iterations: [{ model: "claude-opus", output_tokens: 20, details: { exact: true } }], fallbackCredit: { applied: true }, outputTokensDetails: { thinking_tokens: 5 }, unmodeledUsage: { vendor_field: { preserved: "exactly" } }, cacheDiagnostic: { cache_miss_reason: "cold" } } },
  responseTiming: { timeToFirstTokenMs: "50", outputGenerationDurationMs: "100" },
};

describe("FailureKind: the closed failure vocabulary", () => {
  it("adopts an arm with its own typed evidence", () => {
    // Arrange / Act — each arm carries the evidence that arm actually has,
    // rather than a shared free-text field every producer packs differently.
    const got = decodeFailureKind({ shimRejected: { requestId: "r1", reason: "busy" } }, "k");
    // Assert
    expect(got.kind).toEqual({
      case: "shimRejected",
      value: expect.objectContaining({ requestId: "r1", reason: "busy" }),
    });
  });

  it("THROWS on an unset kind rather than rendering a generic error", () => {
    // Arrange / Act / Assert — errors.proto says so outright: an unset
    // FailureKind is a malformed frame.
    expect(() => decodeFailureKind({}, "k")).toThrow(/sets no failure kind/);
  });

  it("THROWS on a double-set kind rather than picking an arm", () => {
    // Arrange / Act / Assert
    expect(() => decodeFailureKind({ shimRejected: {}, apiOverloaded: {} }, "k")).toThrow(
      /FailureKind contract/,
    );
  });

  it("THROWS on an arm the wire does not have", () => {
    // Arrange / Act / Assert — a consumer meeting an unfamiliar failure fails
    // to match it instead of silently rendering it as something else.
    expect(() => decodeFailureKind({ shimExploded: {} }, "k")).toThrow(/FailureKind contract/);
  });

  it("preserves exact query-termination evidence on its arm", () => {
    // Arrange / Act
    const got = decodeFailureKind(
      {
        queryTermination: {
          detail: {
            queryInstanceId: "query-1",
            vendorSessionId: "claude-1",
            observedAtMs: "1700000000000",
            iteratorFailure: { cause: "child exited 137" },
          },
        },
      },
      "k",
    );
    // Assert
    if (got.kind.case !== "queryTermination") throw new Error("wrong arm");
    expect(got.kind.value.detail?.reason.case).toBe("iteratorFailure");
  });

  it("preserves exact resume-continuity evidence on its arm", () => {
    // Arrange / Act
    const got = decodeFailureKind(
      {
        sessionResumeFailed: {
          detail: {
            claudeSessionId: "claude-1",
            cwd: "/repo",
            resolvedConfigDir: "/config",
            automaticRestore: {},
            bringUpFailure: { cause: "shim readiness timed out" },
          },
        },
      },
      "k",
    );
    // Assert
    if (got.kind.case !== "sessionResumeFailed") throw new Error("wrong arm");
    expect(got.kind.value.detail?.cause).toEqual({
      case: "bringUpFailure",
      value: expect.objectContaining({ cause: "shim readiness timed out" }),
    });
  });
});

// --- the invariants the two evidence-carrying arms are held to --------------
//
// `queryTermination` and `sessionResumeFailed` carry whole machine-readable
// lifecycle records that renderers read field by field. The generated
// descriptor accepts an absent detail, an empty oneof and a blank string
// alike, so the record's own invariants are enforced in the decoder and each
// violation is refused LOUDLY — never rendered as "missing …" prose describing
// evidence nobody supplied.

describe("FailureKind: query-termination evidence invariants", () => {
  /** A complete, valid query-termination record, with one field overridden. */
  function queryTermination(over: Record<string, unknown> = {}): unknown {
    return {
      queryTermination: {
        detail: {
          queryInstanceId: "query-1",
          vendorSessionId: "claude-1",
          observedAtMs: "1700000000000",
          iteratorFailure: { cause: "child exited 137" },
          ...over,
        },
      },
    };
  }

  it("THROWS when the arm carries no detail record at all", () => {
    // Arrange / Act / Assert — the arm exists to carry exact evidence, so an
    // empty one claims a query death it can say nothing about.
    expect(() => decodeFailureKind({ queryTermination: {} }, "k")).toThrow(
      /requires query-termination evidence/,
    );
  });

  it("THROWS on a blank query_instance_id", () => {
    // Arrange / Act / Assert — a record naming no query() invocation
    // corroborates nothing.
    expect(() => decodeFailureKind(queryTermination({ queryInstanceId: "  " }), "k")).toThrow(
      /nonblank `query_instance_id`/,
    );
  });

  it("THROWS on a non-positive observed_at_ms", () => {
    // Arrange / Act / Assert — an unset instant is not "the epoch", it is a
    // record that cannot say when the query died.
    expect(() => decodeFailureKind(queryTermination({ observedAtMs: "0" }), "k")).toThrow(
      /positive `observed_at_ms`/,
    );
  });

  it("THROWS when neither vendor-identity arm is set", () => {
    // Arrange / Act / Assert — the proto offers an explicit "identity was never
    // exposed" arm, so silence is a malformed frame rather than that statement.
    const noVendor = {
      queryTermination: {
        detail: {
          queryInstanceId: "query-1",
          observedAtMs: "1700000000000",
          iteratorFailure: { cause: "child exited 137" },
        },
      },
    };
    expect(() => decodeFailureKind(noVendor, "k")).toThrow(/explicit vendor identity evidence/);
  });

  it("THROWS on a blank vendor_session_id", () => {
    // Arrange / Act / Assert — the arm asserts an authoritative conversation
    // UUID; blank asserts one and supplies none.
    expect(() => decodeFailureKind(queryTermination({ vendorSessionId: "" }), "k")).toThrow(
      /vendorSessionId must be nonblank/,
    );
  });

  it("THROWS when no termination reason is set", () => {
    // Arrange / Act / Assert — the reason is the whole point of the record;
    // without it the card would read "missing termination reason".
    const noReason = {
      queryTermination: {
        detail: {
          queryInstanceId: "query-1",
          vendorSessionId: "claude-1",
          observedAtMs: "1700000000000",
        },
      },
    };
    expect(() => decodeFailureKind(noReason, "k")).toThrow(/unexpected termination reason/);
  });
});

describe("FailureKind: resume-continuity evidence invariants", () => {
  it("THROWS when the arm carries no detail record at all", () => {
    // Arrange / Act / Assert
    expect(() => decodeFailureKind({ sessionResumeFailed: {} }, "k")).toThrow(
      /requires resume-continuity evidence/,
    );
  });

  it("THROWS when no attempt arm is set", () => {
    // Arrange / Act / Assert — the attempt names WHICH operation's continuity
    // requirement could not be met.
    const noAttempt = {
      sessionResumeFailed: {
        detail: {
          claudeSessionId: "claude-1",
          cwd: "/repo",
          resolvedConfigDir: "/config",
          bringUpFailure: { cause: "shim readiness timed out" },
        },
      },
    };
    expect(() => decodeFailureKind(noAttempt, "k")).toThrow(/requires exactly one attempt/);
  });

  it("THROWS when no cause arm is set", () => {
    // Arrange / Act / Assert — this is exactly the frame that rendered as
    // "missing resume-continuity cause" prose instead of being refused.
    const noCause = {
      sessionResumeFailed: {
        detail: {
          claudeSessionId: "claude-1",
          cwd: "/repo",
          resolvedConfigDir: "/config",
          automaticRestore: {},
        },
      },
    };
    expect(() => decodeFailureKind(noCause, "k")).toThrow(/requires exactly one cause/);
  });

  it("THROWS on a query-death cause that violates the query record's invariants", () => {
    // Arrange / Act / Assert — a nested record is held to the same evidence
    // rules as a top-level one; the card renders it with the same code.
    const badNested = {
      sessionResumeFailed: {
        detail: {
          claudeSessionId: "claude-1",
          cwd: "/repo",
          resolvedConfigDir: "/config",
          automaticRestore: {},
          queryTermination: {
            queryInstanceId: "",
            vendorSessionId: "claude-1",
            observedAtMs: "1700000000000",
            unexpectedEof: {},
          },
        },
      },
    };
    expect(() => decodeFailureKind(badNested, "k")).toThrow(/nonblank `query_instance_id`/);
  });
});

describe("FailureCardView: the feed's resolved failure card", () => {
  const KIND = { apiOverloaded: { httpStatus: 529, attempts: 10 } };

  it("carries the daemon's sentence verbatim", () => {
    // Arrange / Act
    const got = decodeFailureCardView(
      { kind: KIND, message: "the API is overloaded", detail: "status=529", open: {} },
      "card",
    );
    // Assert
    expect(got.message).toBe("the API is overloaded");
  });

  it("accepts an EMPTY detail, which the proto allows deliberately", () => {
    // Arrange / Act
    const got = decodeFailureCardView({ kind: KIND, message: "m", terminal: {} }, "card");
    // Assert
    expect(got.detail).toBe("");
  });

  it("reads the OPEN lifecycle arm", () => {
    // Arrange / Act
    const got = decodeFailureCardView({ kind: KIND, message: "m", open: {} }, "card");
    // Assert
    expect(got.lifecycle).toEqual({ case: "open" });
  });

  it("reads the RESOLVED arm with its closing stamp", () => {
    // Arrange / Act
    const got = decodeFailureCardView(
      { kind: KIND, message: "m", resolved: { resolvedAtMs: "1700000000000" } },
      "card",
    );
    // Assert
    expect(got.lifecycle).toEqual({ case: "resolved", resolvedAtMs: 1700000000000 });
  });

  it("reads the TERMINAL arm, which is distinct from open", () => {
    // Arrange / Act — an open card invites waiting and this one does not.
    const got = decodeFailureCardView({ kind: KIND, message: "m", terminal: {} }, "card");
    // Assert
    expect(got.lifecycle).toEqual({ case: "terminal" });
  });

  it("THROWS when no lifecycle arm is set", () => {
    // Arrange / Act / Assert
    expect(() => decodeFailureCardView({ kind: KIND, message: "m" }, "card")).toThrow(
      /exactly one lifecycle arm/,
    );
  });

  it("THROWS when two lifecycle arms are set", () => {
    // Arrange / Act / Assert
    expect(() =>
      decodeFailureCardView({ kind: KIND, message: "m", open: {}, terminal: {} }, "card"),
    ).toThrow(/exactly one lifecycle arm/);
  });

  it("THROWS when the kind is missing entirely", () => {
    // Arrange / Act / Assert
    expect(() => decodeFailureCardView({ message: "m", open: {} }, "card")).toThrow(
      /requires `kind`/,
    );
  });

  it("rejects an unrecognized field on the card", () => {
    // Arrange / Act / Assert
    expect(() =>
      decodeFailureCardView({ kind: KIND, message: "m", open: {}, errorType: "x" }, "card"),
    ).toThrow(/unrecognized field\(s\): errorType/);
  });
});

describe("ConversationItem token utilization", () => {
  it("preserves every modeled response usage field and raw payload", () => {
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [TOKEN_UTILIZATION] }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].tokenUtilization[0]).toEqual({ agentReplSessionId: "session-1", claudeSessionId: "claude-1", rootTurnId: "turn-1", apiRequestId: "request-1", apiMessageId: "msg-1", model: "claude-opus", actor: "mainAgent", usage: { inputTokens: 10, outputTokens: 20, cacheReadInputTokens: 30, cacheCreationInputTokens: 40, cacheCreation: { ephemeral5mInputTokens: 4, ephemeral1hInputTokens: 36 }, serverToolUse: { webSearchRequests: 2, webFetchRequests: 3 }, serviceTier: "priority", speed: "fast", inferenceGeo: "us", outputDetails: { thinkingTokens: 5 }, iterations: [{ kind: "sampling", inputTokens: 1, outputTokens: 2, cacheReadInputTokens: 3, cacheCreationInputTokens: 4, cacheCreation: { ephemeral5mInputTokens: 1, ephemeral1hInputTokens: 3 }, model: "claude-opus" }, { kind: "compaction", inputTokens: 5, outputTokens: 6, cacheReadInputTokens: 7, cacheCreationInputTokens: 8, cacheCreation: { ephemeral5mInputTokens: 2, ephemeral1hInputTokens: 6 } }, { kind: "advisor", inputTokens: 9, outputTokens: 10, cacheReadInputTokens: 11, cacheCreationInputTokens: 12, cacheCreation: { ephemeral5mInputTokens: 3, ephemeral1hInputTokens: 9 }, model: "claude-haiku" }, { kind: "fallback", inputTokens: 13, outputTokens: 14, cacheReadInputTokens: 15, cacheCreationInputTokens: 16, cacheCreation: { ephemeral5mInputTokens: 4, ephemeral1hInputTokens: 12 }, model: "claude-sonnet" }], cacheDiagnostic: { kind: "modelChanged", cacheMissedInputTokens: 17 }, cacheRates: { totalPromptInputTokens: 80, cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 }, fallbackCredit: { applied: true }, unmodeledUsage: { vendorField: { preserved: "exactly" } }, rawUsage: TOKEN_UTILIZATION.usage.rawUsage }, responseTiming: { timeToFirstTokenMs: 50, outputGenerationDurationMs: 100 } });
  });

  it.each(["agentReplSessionId", "claudeSessionId", "rootTurnId", "apiMessageId"] as const)("rejects blank required correlation field %s", (field) => {
    const utilization = { ...TOKEN_UTILIZATION, [field]: "" };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } })).toThrow(new RegExp(`${field} must be nonblank`));
  });

  it.each(["", " \t\n"])("rejects blank response model identity %j", (model) => {
    const utilization = { ...TOKEN_UTILIZATION, model };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } })).toThrow(/model must be nonblank/);
  });

  it("distinguishes an absent API request identifier from an invalid empty present value", () => {
    const utilization = { ...TOKEN_UTILIZATION, apiRequestId: "" };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } })).toThrow(/apiRequestId must be absent or nonblank/);
    const decoded = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, apiRequestId: undefined }] }] } });
    if (decoded.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(decoded.frame.value.items[0].tokenUtilization[0].apiRequestId).toBeUndefined();
  });

  it("preserves every nested subagent lineage field", () => {
    const subagent = { agentId: "agent-child", parentToolUseId: "tool-parent", parentAgentId: "agent-parent", subagentType: "research", taskDescription: "inspect cache evidence" };
    const utilization = { ...TOKEN_UTILIZATION, mainAgent: undefined, subagent };
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].tokenUtilization[0].subagent).toEqual(subagent);
  });

  it("preserves every cache diagnostic oneof arm", () => {
    const cases = [
      [{ pending: {} }, { kind: "pending" }],
      [{ modelChanged: { cacheMissedInputTokens: "1" } }, { kind: "modelChanged", cacheMissedInputTokens: 1 }],
      [{ systemChanged: { cacheMissedInputTokens: "2" } }, { kind: "systemChanged", cacheMissedInputTokens: 2 }],
      [{ toolsChanged: { cacheMissedInputTokens: "3" } }, { kind: "toolsChanged", cacheMissedInputTokens: 3 }],
      [{ messagesChanged: { cacheMissedInputTokens: "4" } }, { kind: "messagesChanged", cacheMissedInputTokens: 4 }],
      [{ previousMessageUnavailable: {} }, { kind: "previousMessageUnavailable" }],
      [{ diagnosticsUnavailable: {} }, { kind: "diagnosticsUnavailable" }],
    ] as const;
    for (const [cacheDiagnostic, expected] of cases) {
      const utilization = { ...TOKEN_UTILIZATION, usage: { ...TOKEN_UTILIZATION.usage, cacheDiagnostic } };
      const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } });
      if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
      expect(frame.frame.value.items[0].tokenUtilization[0].usage.cacheDiagnostic).toEqual(expected);
    }
  });

  it("carries the reasoning emission's stated origin beside its block", () => {
    // Arrange: `AgentThinking` states the message it was stripped from and the
    // index it held there, alongside the `ThinkingBlock` payload itself.
    const item = {
      uuid: "env-1#thinking:0",
      tsMs: "1700000000000",
      agent: { thinking: { body: { thinking: "hmm", signature: "sig" }, apiMessageId: "msg_01ABC", blockIndex: 2 } },
    };
    // Act
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [item] } });
    // Assert
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].thinkingOrigin).toEqual({ apiMessageId: "msg_01ABC", blockIndex: 2 });
  });

  it("keeps the reasoning emission's payload the block alone", () => {
    // The origin is a statement ABOUT the block, not part of the verbatim
    // durable evidence, so it must not leak into the payload.
    // Arrange
    const item = {
      uuid: "env-1#thinking:0",
      tsMs: "1700000000000",
      agent: { thinking: { body: { thinking: "hmm" }, apiMessageId: "msg_01ABC", blockIndex: 2 } },
    };
    // Act
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [item] } });
    // Assert
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].payload).toEqual({ thinking: "hmm" });
  });

  it("states a proto3-default origin for a reasoning emission that omits it", () => {
    // Arrange
    const item = {
      uuid: "env-1#thinking:0",
      tsMs: "1700000000000",
      agent: { thinking: { body: { thinking: "hmm" } } },
    };
    // Act
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [item] } });
    // Assert
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].thinkingOrigin).toEqual({ apiMessageId: "", blockIndex: 0 });
  });

  it("rejects a reasoning emission whose stated origin is not a string", () => {
    // Arrange
    const item = {
      uuid: "env-1#thinking:0",
      tsMs: "1700000000000",
      agent: { thinking: { body: { thinking: "hmm" }, apiMessageId: 7 } },
    };
    // Act / Assert
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [item] } })).toThrow(/apiMessageId must be a string/);
  });

  it("rejects malformed response actor and unknown usage fields", () => {
    const item = { ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, subagent: {} }] };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [item] } })).toThrow(/oneof .*actor.*set multiple times/);
    const usage = { ...TOKEN_UTILIZATION.usage, unrecognized: 1 };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage }] }] } })).toThrow(/key "unrecognized" is unknown/);
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, rawUsage: TOKEN_UTILIZATION.usage.rawUsage }] }] } })).toThrow(/key "rawUsage" is unknown/);
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage: { ...TOKEN_UTILIZATION.usage, rawUsage: undefined } }] }] } })).toThrow(/usage\.rawUsage is required/);
    const malformedDiagnostic = { ...TOKEN_UTILIZATION.usage, cacheDiagnostic: { pending: { unrecognized: true } } };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage: malformedDiagnostic }] }] } })).toThrow(/key "unrecognized" is unknown/);
    const subagent = { agentId: "a", parentToolUseId: "t", parentAgentId: "p", subagentType: "research", taskDescription: "inspect", unrecognized: true };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, mainAgent: undefined, subagent }] }] } })).toThrow(/key "unrecognized" is unknown/);
  });

  it("rejects scalar and nested oneof violations through the generated contract", () => {
    const wrongScalar = { ...TOKEN_UTILIZATION.usage, inputTokens: true };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage: wrongScalar }] }] } })).toThrow(/generated TokenUtilization contract/);
    const competingDiagnostic = { ...TOKEN_UTILIZATION.usage, cacheDiagnostic: { pending: {}, modelChanged: { cacheMissedInputTokens: "1" } } };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage: competingDiagnostic }] }] } })).toThrow(/oneof .*reason.*set multiple times/);
  });

  it("preserves raw SDK nulls, zeroes, absence, and unknown nested values through generated decode", () => {
    const rawSdkUsage = { cache_read_input_tokens: null, cache_creation_input_tokens: 0, nested_vendor_evidence: { unknown: [null, 0, { exact: "value" }] } };
    const rawUsage = { ...TOKEN_UTILIZATION.usage.rawUsage, rawSdkUsage };
    const utilization = { ...TOKEN_UTILIZATION, usage: { ...TOKEN_UTILIZATION.usage, rawUsage } };
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [utilization] }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    const preserved = frame.frame.value.items[0].tokenUtilization[0].usage.rawUsage.rawSdkUsage as Record<string, unknown>;
    expect(preserved).toEqual(rawSdkUsage);
    expect(preserved.cache_read_input_tokens).toBeNull();
    expect(preserved.cache_creation_input_tokens).toBe(0);
    expect(Object.hasOwn(preserved, "absent_vendor_field")).toBe(false);
  });
});

describe("Session token utilization", () => {
  it("preserves subagent lineage and complete per-agent and session model usage", () => {
    const totals = { inputTokens: "10", outputTokens: "20", cacheReadInputTokens: "30", cacheCreationInputTokens: "40", cacheCreation: { ephemeral5mInputTokens: "4", ephemeral1hInputTokens: "36" }, serverToolUse: { webSearchRequests: "2", webFetchRequests: "3" }, outputDetails: { thinkingTokens: "5" }, cacheRates: { totalPromptInputTokens: "80", cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 }, timing: { outputTokensWithGenerationDuration: "20", outputGenerationDurationMs: "100", responsesWithGenerationDuration: "1", responsesWithoutGenerationDuration: "0", totalTimeToFirstTokenMs: "50", responsesWithTimeToFirstToken: "1", responsesWithoutTimeToFirstToken: "0" } };
    const model = { model: "opus", canonicalModel: "claude-opus-4", provider: "anthropic", totals, contextWindow: "200000", maxOutputTokens: "32000", costUsd: 1.25 };
    const tokenUtilization = { allAgents: totals, mainAgent: totals, subagents: [{ agent: { agentId: "agent-child", parentToolUseId: "tool-parent", parentAgentId: "agent-parent", subagentType: "research", taskDescription: "inspect cache evidence" }, totals, models: [model] }], models: [model] };
    const frame = decode({ sessionView: { ...SESSION_VIEW, tokenUtilization } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong frame");
    const aggregate = frame.frame.value.tokenUtilization;
    expect(aggregate?.allAgents?.inputTokens).toBe(10n);
    expect(aggregate?.allAgents?.cacheCreation?.ephemeral1hInputTokens).toBe(36n);
    expect(aggregate?.allAgents?.serverToolUse?.webFetchRequests).toBe(3n);
    expect(aggregate?.allAgents?.outputDetails?.thinkingTokens).toBe(5n);
    expect(aggregate?.allAgents?.timing?.outputGenerationDurationMs).toBe(100n);
    expect(aggregate?.subagents[0].agent).toMatchObject({ agentId: "agent-child", parentToolUseId: "tool-parent", parentAgentId: "agent-parent", subagentType: "research", taskDescription: "inspect cache evidence" });
    expect(aggregate?.subagents[0].models[0]).toMatchObject({ model: "opus", canonicalModel: "claude-opus-4", provider: "anthropic", contextWindow: 200000n, maxOutputTokens: 32000n, costUsd: 1.25 });
    expect(aggregate?.models[0].totals?.cacheRates).toMatchObject({ totalPromptInputTokens: 80n, cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 });
  });

  it("preserves every ungrouped response separately by API message identity and lineage", () => {
    const totals = { inputTokens: "0", outputTokens: "0", cacheReadInputTokens: "0", cacheCreationInputTokens: "0" };
    const lineage = { agentId: "", parentToolUseId: "", parentAgentId: "parent-agent", subagentType: "research", taskDescription: "inspect evidence" };
    const first = { ...TOKEN_UTILIZATION, apiMessageId: "msg-ungrouped-1", mainAgent: undefined, subagent: lineage, usage: { ...TOKEN_UTILIZATION.usage, inputTokens: "11" } };
    const second = { ...TOKEN_UTILIZATION, apiMessageId: "msg-ungrouped-2", mainAgent: undefined, subagent: lineage, usage: { ...TOKEN_UTILIZATION.usage, inputTokens: "22" } };
    const sessionFrame = decode({ sessionView: { ...SESSION_VIEW, tokenUtilization: { allAgents: totals, mainAgent: totals, subagents: [], models: [], ungroupedSubagentResponses: [first, second] } } });
    const responseFrame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [first, second] }] } });
    if (sessionFrame.frame.case !== "sessionView" || responseFrame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    const preserved = sessionFrame.frame.value.tokenUtilization?.ungroupedSubagentResponses;
    expect(preserved?.map((response) => response.apiMessageId)).toEqual(responseFrame.frame.value.items[0].tokenUtilization.map((response) => response.apiMessageId));
    expect(preserved?.map((response) => [response.apiMessageId, response.usage?.inputTokens, response.actor.case === "subagent" ? { agentId: response.actor.value.agentId, parentToolUseId: response.actor.value.parentToolUseId, parentAgentId: response.actor.value.parentAgentId, subagentType: response.actor.value.subagentType, taskDescription: response.actor.value.taskDescription } : undefined])).toEqual([["msg-ungrouped-1", 11n, lineage], ["msg-ungrouped-2", 22n, lineage]]);
  });

  it.each(["", " \t\n"])("rejects blank durable aggregate model identity %j in session and snapshot frames", (model) => {
    const totals = { inputTokens: "1", outputTokens: "0", cacheReadInputTokens: "0", cacheCreationInputTokens: "0" };
    const tokenUtilization = { allAgents: totals, mainAgent: totals, models: [{ model, totals }] };
    expect(() => decode({ sessionView: { ...SESSION_VIEW, tokenUtilization } })).toThrow(/models\[0\] requires model identity and totals/);
    expect(() => decode({ snapshot: { ...SNAPSHOT, sessions: [{ ...SESSION_VIEW, tokenUtilization }] } })).toThrow(/models\[0\] requires model identity and totals/);
  });

  it("rejects blank nested subagent model identity", () => {
    const totals = { inputTokens: "1", outputTokens: "0", cacheReadInputTokens: "0", cacheCreationInputTokens: "0" };
    const tokenUtilization = { allAgents: totals, mainAgent: totals, subagents: [{ agent: { agentId: "agent" }, totals, models: [{ model: " ", totals }] }], models: [{ model: "valid", totals }] };
    expect(() => decode({ sessionView: { ...SESSION_VIEW, tokenUtilization } })).toThrow(/subagents\[0\]\.models\[0\] requires model identity and totals/);
  });

  it("rejects grouped, main-agent, and duplicate-message records in the ungrouped set", () => {
    const totals = { inputTokens: "0", outputTokens: "0", cacheReadInputTokens: "0", cacheCreationInputTokens: "0" };
    const base = { allAgents: totals, mainAgent: totals, subagents: [], models: [] };
    const ungrouped = { ...TOKEN_UTILIZATION, apiMessageId: "msg-ungrouped", mainAgent: undefined, subagent: { agentId: "", parentToolUseId: "", parentAgentId: "parent", subagentType: "research", taskDescription: "inspect" } };
    expect(() => decode({ sessionView: { ...SESSION_VIEW, tokenUtilization: { ...base, ungroupedSubagentResponses: [{ ...ungrouped, subagent: { ...ungrouped.subagent, agentId: "stable-agent" } }] } } })).toThrow(/stable invocation identity/);
    expect(() => decode({ sessionView: { ...SESSION_VIEW, tokenUtilization: { ...base, ungroupedSubagentResponses: [TOKEN_UTILIZATION] } } })).toThrow(/must be a subagent response/);
    expect(() => decode({ sessionView: { ...SESSION_VIEW, tokenUtilization: { ...base, ungroupedSubagentResponses: [ungrouped, ungrouped] } } })).toThrow(/repeated apiMessageId msg-ungrouped/);
  });
});

describe("ConversationItem turn accounting", () => {
  it("decodes invalid problems before accepting explicitly absent partial evidence", () => {
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ uuid: "result-1", tsMs: "1700000000000", result: {}, turnAccounting: { turnId: "turn-1", queryInstanceId: "query-1", timing: { promptAdmittedAtMs: "1", resultReceivedAtMs: "2", accountingSettledAtMs: "3", promptToResultMs: "1", resultToSettlementMs: "1" }, responses: [TOKEN_UTILIZATION], invalid: { problems: [{ missingUsageBoundary: { turnStart: {} } }] } } }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].turnAccounting).toMatchObject({ turnId: "turn-1", queryInstanceId: "query-1", timing: { promptAdmittedAtMs: 1, resultReceivedAtMs: 2, accountingSettledAtMs: 3, promptToResultMs: 1, resultToSettlementMs: 1 }, responses: [{ usage: { rawUsage: TOKEN_UTILIZATION.usage.rawUsage } }], verdict: { kind: "invalid", problems: [{ kind: "missingUsageBoundary", boundary: "turnStart" }] } });
  });

  it("uses generated proto presence for complete accounting evidence", () => {
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ uuid: "result-1", tsMs: "1700000000000", result: {}, turnAccounting: { turnId: "turn-1", queryInstanceId: "query-1", reconciliation: { responseRecordCount: "0", responseModels: [{ model: "metadata-absent" }, { model: "explicit-zero", canonicalModel: "", provider: "", contextWindow: "0", maxOutputTokens: "0", costUsd: 0 }], resultModels: [], apiMessageIds: [] }, complete: {} } }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    const accounting = frame.frame.value.items[0].turnAccounting;
    expect(accounting).toMatchObject({ turnId: "turn-1", queryInstanceId: "query-1", responses: [], verdict: { kind: "complete" } });
    if (accounting?.reconciliation === undefined) throw new Error("missing reconciliation");
    expect(accounting.reconciliation.responseAllAgents).toBeUndefined();
    expect(accounting.reconciliation.responseModels).toEqual([
      { model: "metadata-absent" },
      { model: "explicit-zero", canonicalModel: "", provider: "", contextWindow: 0, maxOutputTokens: 0, costUsd: 0 },
    ]);
  });

  it("rejects malformed account-usage outcomes instead of inventing unavailable reasons", () => {
    const accounting = (usageAtStart: unknown) => ({ conversationDelta: { ...CONV_DELTA, items: [{ uuid: "result-1", tsMs: "1700000000000", result: {}, turnAccounting: { turnId: "turn-1", queryInstanceId: "query-1", usageAtStart, responses: [], complete: {} } }] } });
    expect(() => decode(accounting({ turnId: "turn-1", turnStart: {}, available: {} }))).toThrow(/available requires fiveHour/);
    expect(() => decode(accounting({ turnId: "turn-1", turnStart: {}, unavailable: {} }))).toThrow(/unavailable requires a reason/);
  });

  it("preserves explicit zero utilization and legal unavailable reasons", () => {
    const accounting = (usageAtStart: unknown) => decode({ conversationDelta: { ...CONV_DELTA, items: [{ uuid: "result-1", tsMs: "1700000000000", result: {}, turnAccounting: { turnId: "turn-1", queryInstanceId: "query-1", usageAtStart, responses: [], complete: {} } }] } });
    const available = accounting({ turnId: "turn-1", turnStart: {}, available: { fiveHour: { utilizationPercent: 0, resetsAtMs: "100" } } });
    const unavailable = accounting({ turnId: "turn-1", turnStart: {}, unavailable: { windowUnavailable: {} } });
    if (available.frame.case !== "conversationDelta" || unavailable.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(available.frame.value.items[0].turnAccounting?.usageAtStart?.outcome).toEqual({ kind: "available", utilizationPercent: 0, resetsAtMs: 100 });
    expect(unavailable.frame.value.items[0].turnAccounting?.usageAtStart?.outcome).toEqual({ kind: "unavailable", reason: "windowUnavailable" });
  });
});
const HOST_ACTION = { actionId: "action-1", setRepositoryFold: { repoKey: "repo", folded: false } };

describe("decodeFrontendFrame — every frame variant decodes", () => {
  const cases: Array<[string, unknown]> = [
    ["snapshot", { snapshot: SNAPSHOT }],
    ["workspaceState", { workspaceState: WS_STATE }],
    ["sessionView", { sessionView: SESSION_VIEW }],
    ["conversationDelta", { conversationDelta: CONV_DELTA }],
    ["typingDelta", { typingDelta: TYPING }],
    ["taskCatalog", { taskCatalog: TASK_CATALOG }],
    ["commandAck", { commandAck: COMMAND_ACK }],
    ["daemonView", { daemonView: DAEMON_VIEW }],
    ["sessionInit", { sessionInit: SESSION_INIT }],
    ["heartbeat", { heartbeat: HEARTBEAT }],
    ["queue", { queue: QUEUE }],
    ["workspaceAvailable", { workspaceAvailable: WORKSPACE_AVAILABLE }],
    ["hostAction", { hostAction: HOST_ACTION }],
  ];
  for (const [name, obj] of cases) {
    it(`decodes ${name}`, () => {
      const frame = decode(obj);
      expect(frame.frame.case).toBe(name);
    });
  }
});

describe("decodeFrontendFrame — durable host work", () => {
  it("decodes host-only snapshot collections", () => {
    const frame = decode({ snapshot: { ...SNAPSHOT, workspaceAvailable: [WORKSPACE_AVAILABLE], hostActions: [HOST_ACTION] } });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.workspaceAvailable[0]?.jobId).toBe("job-1");
    expect(frame.frame.value.hostActions[0]?.action.case).toBe("setRepositoryFold");
  });

  it("rejects a host action with no selected action arm", () => {
    expect(() => decode({ hostAction: { actionId: "action-1" } })).toThrow(/exactly one action arm/);
  });
});

describe("decodeFrontendFrame — protojson field coercion", () => {
  it("decodes an enum by its proto name", () => {
    const frame = decode({ workspaceState: { ...WS_STATE, state: "RENDER_STATE_THINKING" } });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.state).toBe(4);
  });

  it("decodes an int64 field from its protojson string", () => {
    const frame = decode({ workspaceState: { ...WS_STATE, liveTaskCount: "3" } });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.liveTaskCount).toBe(3);
  });

  it("decodes composite connectivity, status, generation, and active faults", () => {
    const frame = decode({
      workspaceState: {
        ...WS_STATE,
        connectivity: "SESSION_CONNECTIVITY_DEGRADED",
        status: "SESSION_STATUS_THINKING",
        activeFaults: [
          {
            component: "store-client",
            faultType: "subscription",
            impact: "connectivity",
            causeKind: "store_subscription_lost",
            openedAtMs: "42",
          },
        ],
      },
    });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.connectivity).toBe(4);
    expect(frame.frame.value.status).toBe(2);
    expect(frame.frame.value.controllerGenerationId).toBe("g1");
    expect(frame.frame.value.activeFaults[0]).toMatchObject({
      component: "store-client",
      faultType: "subscription",
      impact: "connectivity",
      openedAtMs: 42,
    });
  });

  it("rejects the retired flat merge-queue place as an unknown field", () => {
    // The flat trio is RESERVED on the wire. A daemon that stamped it again
    // would be a version skew, and skew is an error here rather than a field
    // the decoder quietly ignores.
    expect(() =>
      decode({ workspaceState: { ...WS_STATE, mergeQueuePosition: 2, mergeQueueDepth: 3 } }),
    ).toThrow(/unrecognized field/);
  });

  it("rejects the retired flat merge phase as an unknown field", () => {
    expect(() => decode({ workspaceState: { ...WS_STATE, mergePhase: "merging" } })).toThrow(
      /unrecognized field/,
    );
  });

  it("decodes the merge lease", () => {
    const frame = decode({ workspaceState: { ...WS_STATE, mergeLeaseHeld: true } });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.mergeLeaseHeld).toBe(true);
  });

  it("decodes the instant a merge landed", () => {
    // The daemon stamps this on every frame for a merged workspace. It was
    // absent from the decoder's field set, so the frames that mattered most
    // were the ones it threw on.
    const frame = decode({ workspaceState: { ...WS_STATE, mergedAtMs: "1700000000000" } });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.mergedAtMs).toBe(1700000000000);
  });

  it("reads an ABSENT merged instant as never merged", () => {
    const frame = decode({ workspaceState: WS_STATE });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.mergedAtMs).toBe(0);
  });

  it("reads an ABSENT merge lease as not held, the proto3 default protojson omits", () => {
    const frame = decode({ workspaceState: WS_STATE });
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.mergeLeaseHeld).toBe(false);
  });

  it("rejects unspecified session connectivity", () => {
    expect(() =>
      decode({ workspaceState: { ...WS_STATE, connectivity: "SESSION_CONNECTIVITY_UNSPECIFIED" } }),
    ).toThrow(/UNSPECIFIED session connectivity/);
  });

  it("rejects current connectivity without complete controller identity", () => {
    expect(() =>
      decode({ workspaceState: { ...WS_STATE, controllerGenerationId: "" } }),
    ).toThrow(/without complete session-controller identity/);
  });

  it("rejects an active fault without its scoped identity", () => {
    expect(() =>
      decode({ workspaceState: { ...WS_STATE, activeFaults: [{ component: "store-client" }] } }),
    ).toThrow(/missing component, faultType, or impact/);
  });
});

describe("decodeFrontendFrame — SessionView resume keys + config dir", () => {
  it("decodes claudeSessionId + cwd (protojson camelCase)", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, claudeSessionId: "cli-uuid", cwd: "/work" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.claudeSessionId).toBe("cli-uuid");
    expect(frame.frame.value.cwd).toBe("/work");
  });

  it("decodes the S8 configDir account identity", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, configDir: "/home/u/.claude" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.configDir).toBe("/home/u/.claude");
  });

  it("defaults the resume keys + configDir to empty strings when absent", () => {
    const frame = decode({ sessionView: SESSION_VIEW });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.claudeSessionId).toBe("");
    expect(frame.frame.value.cwd).toBe("");
    expect(frame.frame.value.configDir).toBe("");
  });
});

describe("decodeFrontendFrame — SessionView S7 parity fields", () => {
  it("decodes terminal", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, terminal: true } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.terminal).toBe(true);
  });

  it("rejects the retired deathReason field (step 11)", () => {
    expect(() =>
      decode({ sessionView: { ...SESSION_VIEW, terminal: true, deathReason: "delete session" } }),
    ).toThrow(/unrecognized field/);
  });

  it("decodes pendingPermissions from its protojson int64 string", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, pendingPermissions: "2" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.pendingPermissions).toBe(2);
  });
});

describe("decodeFrontendFrame — ConversationItem envelope", () => {
  function itemOf(item: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({ conversationDelta: { fence: "s1", items: [item] } });
  }

  it("decodes the envelope + selected arm", () => {
    const frame = itemOf({ uuid: "u1", tsMs: "1700000000000", requestId: "r7", toolUse: { id: "tu1", name: "Bash" } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong variant");
    const item = frame.frame.value.items[0];
    expect(item.uuid).toBe("u1");
    expect(item.tsMs).toBe(1700000000000);
    expect(item.requestId).toBe("r7");
    expect(item.arm).toBe("toolUse");
    expect(item.payload).toEqual({ id: "tu1", name: "Bash" });
  });

  it("decodes the item's provenance by its proto name", () => {
    const frame = itemOf({ uuid: "u1", source: "CONVERSATION_SOURCE_MERGE", toolUse: { id: "tu1" } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong variant");
    expect(frame.frame.value.items[0].source).toBe(2);
  });

  it("ADOPTS an absent provenance as UNSPECIFIED rather than throwing", () => {
    // The conversation layer owns that error: rejecting the whole frame here
    // would lose the correlated context that makes the bad item findable.
    const frame = itemOf({ uuid: "u1", toolUse: { id: "tu1" } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong variant");
    expect(frame.frame.value.items[0].source).toBe(0);
  });

  it("rejects an unrecognized provenance name", () => {
    expect(() => itemOf({ uuid: "u1", source: "CONVERSATION_SOURCE_ROBOT", toolUse: {} })).toThrow(
      /unknown enum value/,
    );
  });

  it("rejects an item with no arm (empty oneof)", () => {
    expect(() => itemOf({ uuid: "u1" })).toThrow(/carries no item variant/);
  });

  it("rejects an item that sets multiple arms", () => {
    expect(() => itemOf({ uuid: "u1", toolUse: {}, result: {} })).toThrow(/sets multiple item variants/);
  });

  it("rejects an item with an unrecognized field", () => {
    expect(() => itemOf({ uuid: "u1", toolUse: {}, bogus: 1 })).toThrow(/unrecognized field/);
  });

  it("adopts the typed payload by shape (does not reject its inner fields)", () => {
    const frame = itemOf({ uuid: "u1", permission: { request: { requestId: "u1" }, brandNewField: 9 } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong variant");
    expect(frame.frame.value.items[0].payload).toHaveProperty("brandNewField", 9);
  });
});

describe("decodeFrontendFrame — TypingDelta embeds ContentDelta", () => {
  it("normalizes the inputJson arm to the input_json kind", () => {
    const frame = decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", blockIndex: 2, inputJson: '{"a":' } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("input_json");
    expect(frame.frame.value.blockIndex).toBe(2);
    expect(frame.frame.value.delta).toBe('{"a":');
  });

  it("carries the optional input_json tool-use identity", () => {
    const frame = decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", blockIndex: 2, inputJson: "{", toolUseId: "toolu_1" } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value).toMatchObject({ kind: "input_json", toolUseId: "toolu_1" });
  });

  it("rejects a tool-use identity on a non-input delta", () => {
    expect(() => decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", text: "x", toolUseId: "toolu_1" } } })).toThrow(
      /toolUseId is only valid for input_json/,
    );
  });

  it("decodes a thinking delta with its estimatedTokens int64 string", () => {
    const frame = decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", thinking: "hmm", estimatedTokens: "12" } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("thinking");
    expect(frame.frame.value.estimatedTokens).toBe(12);
  });

  it("decodes a signature delta", () => {
    const frame = decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", signature: "sig" } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("signature");
  });

  it("rejects a TypingDelta with no delta", () => {
    expect(() => decode({ typingDelta: { fence: "s1" } })).toThrow(/TypingDelta missing required `delta`/);
  });

  it("rejects a ContentDelta with no content arm (empty oneof)", () => {
    expect(() => decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", blockIndex: 0 } } })).toThrow(
      /carries no content delta/,
    );
  });

  it("rejects a ContentDelta that sets two content arms", () => {
    expect(() =>
      decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", text: "a", thinking: "b" } } }),
    ).toThrow(/sets multiple content deltas/);
  });

  it("rejects a ContentDelta with an unrecognized field", () => {
    expect(() =>
      decode({ typingDelta: { fence: "s1", delta: { uuid: "u1", text: "a", bogus: 1 } } }),
    ).toThrow(/unrecognized field/);
  });

  it("rejects a ContentDelta without a uuid", () => {
    expect(() => decode({ typingDelta: { fence: "s1", delta: { text: "a" } } })).toThrow(
      /TypingDelta.delta missing required `uuid`/,
    );
  });
});

describe("decodeFrontendFrame — SessionInitView (S9)", () => {
  it("adopts the SystemInit init by shape", () => {
    const frame = decode({ sessionInit: SESSION_INIT });
    if (frame.frame.case !== "sessionInit") throw new Error("wrong variant");
    expect(frame.frame.value.init).toEqual({ model: "claude", cwd: "/w" });
  });

  it("defaults an absent init to an empty object", () => {
    const frame = decode({ sessionInit: { fence: "s1" } });
    if (frame.frame.case !== "sessionInit") throw new Error("wrong variant");
    expect(frame.frame.value.init).toEqual({});
  });

  it("rejects an unrecognized SessionInitView field loudly", () => {
    expect(() => decode({ sessionInit: { ...SESSION_INIT, bogus: 1 } })).toThrow(/unrecognized field/);
  });

  it("rejects a SessionInitView without a fence", () => {
    // The push is fenced since the figma-idl reshape: `session_id` is reserved,
    // and an unfenced push cannot be tested for staleness at all.
    expect(() => decode({ sessionInit: { workspace: "ws", init: {} } })).toThrow(
      /SessionInitView missing required `fence`/,
    );
  });

  it("decodes snapshot.inits", () => {
    const frame = decode({ snapshot: { ...SNAPSHOT, inits: [SESSION_INIT] } });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.inits).toHaveLength(1);
    expect(frame.frame.value.inits[0].init).toEqual({ model: "claude", cwd: "/w" });
  });

  it("defaults snapshot.inits to an empty array when absent", () => {
    const frame = decode({ snapshot: SNAPSHOT });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.inits).toEqual([]);
  });
});

describe("decodeFrontendFrame — DaemonView", () => {
  it("decodes the daemonView frame fields (mtime int64 string coerced)", () => {
    const frame = decode({ daemonView: DAEMON_VIEW });
    if (frame.frame.case !== "daemonView") throw new Error("wrong variant");
    expect(frame.frame.value.bootId).toBe("b_abc");
    expect(frame.frame.value.daemonBinaryMtimeMs).toBe(1700000000000);
  });

  it("decodes the optional daemon member on a snapshot", () => {
    const frame = decode({ snapshot: { ...SNAPSHOT, daemon: DAEMON_VIEW } });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.daemon?.bootId).toBe("b_abc");
  });

  it("leaves snapshot.daemon undefined when absent (pre-S7 daemon)", () => {
    const frame = decode({ snapshot: SNAPSHOT });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.daemon).toBeUndefined();
  });

  it("rejects an unrecognized DaemonView field loudly", () => {
    expect(() => decode({ daemonView: { ...DAEMON_VIEW, bogus: 1 } })).toThrow(
      /DaemonView has unrecognized field/,
    );
  });
});

describe("decodeFrontendFrame — unknown / empty variants hard-error", () => {
  it("throws on an unknown frame variant key", () => {
    expect(() => decode({ bogusVariant: {} })).toThrow(/unrecognized field/);
  });

  it("throws on an empty oneof", () => {
    expect(() => decode({})).toThrow(/no known frame variant/);
  });
});

describe("decodeFrontendFrame — structural failures hard-error", () => {
  it("throws on invalid JSON", () => {
    expect(() => decodeFrontendFrame("{not json")).toThrow(/not valid JSON/);
  });

  it("throws on an unknown nested field", () => {
    expect(() => decode({ workspaceState: { ...WS_STATE, bogus: 1 } })).toThrow(/unrecognized field/);
  });

  it("throws on an unknown enum value name", () => {
    expect(() => decode({ workspaceState: { ...WS_STATE, state: "RENDER_STATE_NOPE" } })).toThrow(
      /unknown enum value/,
    );
  });
});

describe("decodeFrontendFrame — required-field validation is loud", () => {
  it("rejects a WorkspaceState without a workspace", () => {
    expect(() => decode({ workspaceState: { sessionId: "s1", state: "RENDER_STATE_IDLE" } })).toThrow(
      /missing required `workspace`/,
    );
  });

  it("rejects a WorkspaceState with UNSPECIFIED state", () => {
    expect(() => decode({ workspaceState: { workspace: "ws", sessionId: "s1" } })).toThrow(
      /UNSPECIFIED render state/,
    );
  });

  it("rejects a SessionView without a session id", () => {
    expect(() => decode({ sessionView: { workspace: "ws", model: "claude", modelOptions: [] } })).toThrow(
      /SessionView missing required `session_id`/,
    );
  });

  it("rejects a SessionView that omits the model catalog", () => {
    expect(() => decode({ sessionView: { workspace: "ws", sessionId: "s1", model: "claude" } })).toThrow(
      /SessionView missing required `modelOptions`/,
    );
  });

  it("rejects a ConversationDelta without a fence", () => {
    // The push is fenced since the figma-idl reshape: `session_id` is reserved,
    // and an unfenced push cannot be tested for staleness at all.
    expect(() => decode({ conversationDelta: { workspace: "ws", items: [] } })).toThrow(
      /ConversationDelta missing required `fence`/,
    );
  });

  it("rejects a TaskEntry with no kind arm", () => {
    // The kind is a oneof since the figma-idl reshape; an unset arm is the only
    // spelling of "no kind", and drawing the task under a guessed one would be
    // a claim nothing made.
    expect(() =>
      decode({ taskCatalog: { fence: "s1", tasks: [{ taskId: "t1", running: {} }] } }),
    ).toThrow(/sets no kind arm/);
  });

  it("rejects a TaskEntry with an unrecognized kind arm", () => {
    expect(() =>
      decode({ taskCatalog: { fence: "s1", tasks: [{ taskId: "t1", wat: {}, running: {} }] } }),
    ).toThrow(/unrecognized field\(s\): wat/);
  });

  it("rejects a TaskEntry with two kind arms, which have no single detail", () => {
    expect(() =>
      decode({ taskCatalog: { fence: "s1", tasks: [{ taskId: "t1", agent: {}, shell: {}, running: {} }] } }),
    ).toThrow(/sets multiple kind arms/);
  });

  it("rejects a TaskEntry with no status arm", () => {
    expect(() =>
      decode({ taskCatalog: { fence: "s1", tasks: [{ taskId: "t1", agent: {} }] } }),
    ).toThrow(/sets no status arm/);
  });

  it("rejects a TaskEntry with two status arms, which have no single verdict", () => {
    expect(() =>
      decode({ taskCatalog: { fence: "s1", tasks: [{ taskId: "t1", agent: {}, running: {}, done: {} }] } }),
    ).toThrow(/sets multiple status arms/);
  });

  it("rejects a CommandAck without a request id", () => {
    expect(() => decode({ commandAck: { ok: true } })).toThrow(/CommandAck missing required `request_id`/);
  });

  it("decodes the interrupt confirmation challenge's live task count", () => {
    // Arrange / Act — int64 arrives as a protojson numeric string.
    const got = decode({
      commandAck: { requestId: "r1", ok: false, interruptConfirmRequired: { liveTasks: "3" } },
    });
    // Assert
    if (got.frame.case !== "commandAck") throw new Error("wrong variant");
    expect(got.frame.value.interruptConfirmRequired).toEqual({ liveTasks: 3 });
  });

  it("leaves the challenge absent on an ordinary refusal", () => {
    // Arrange / Act — a real refusal carries no challenge, so a reader can
    // tell the question apart from the failure by presence alone.
    const got = decode({ commandAck: { requestId: "r1", ok: false, error: "no session" } });
    // Assert
    if (got.frame.case !== "commandAck") throw new Error("wrong variant");
    expect(got.frame.value.interruptConfirmRequired).toBeUndefined();
  });

  it("rejects the retired degradedNotice frame arm (step 11)", () => {
    expect(() => decode({ degradedNotice: { component: "shim-store", reason: "down" } })).toThrow(
      /unrecognized field/,
    );
  });

  it("validates nested snapshot members", () => {
    expect(() =>
      decode({ snapshot: { workspaces: [{ sessionId: "s1", state: "RENDER_STATE_IDLE" }] } }),
    ).toThrow(/missing required `workspace`/);
  });
});

describe("decodeFrontendFrame — HeartbeatView (E4)", () => {
  it("flattens the embedded progress onto the view", () => {
    // Arrange / Act
    const frame = decode({ heartbeat: HEARTBEAT });
    // Assert
    if (frame.frame.case !== "heartbeat") throw new Error("wrong variant");
    expect(frame.frame.value).toEqual({
      workspace: "ws",
      fence: "s1",
      toolUseId: "tu1",
      toolName: "Bash",
      parentToolUseId: "",
      elapsedSeconds: 12.5,
    });
  });

  it("carries the subagent attribution when present", () => {
    // Arrange / Act
    const frame = decode({
      heartbeat: { ...HEARTBEAT, progress: { ...HEARTBEAT.progress, parentToolUseId: "tu0" } },
    });
    // Assert
    if (frame.frame.case !== "heartbeat") throw new Error("wrong variant");
    expect(frame.frame.value.parentToolUseId).toBe("tu0");
  });

  it("rejects a heartbeat with no progress", () => {
    // Arrange / Act / Assert
    expect(() => decode({ heartbeat: { workspace: "ws", fence: "s1" } })).toThrow(
      /missing required `progress`/,
    );
  });

  it("rejects a progress with no toolUseId, which nothing could attribute", () => {
    // Arrange / Act / Assert
    expect(() => decode({ heartbeat: { ...HEARTBEAT, progress: { elapsedSeconds: 1 } } })).toThrow(
      /missing required `toolUseId`/,
    );
  });

  it("rejects an unrecognized field inside progress", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({ heartbeat: { ...HEARTBEAT, progress: { toolUseId: "tu1", bogus: 1 } } }),
    ).toThrow(/HeartbeatView.progress has unrecognized field\(s\): bogus/);
  });

  it("reports heartbeat as visually supported (it feeds the running tool chip)", () => {
    // Arrange / Act / Assert
    expect(isVisuallySupportedFrame("heartbeat")).toBe(true);
  });
});

describe("decodeFrontendFrame — QueueView (E4)", () => {
  it("decodes an entry with its classification keyword", () => {
    // Arrange / Act
    const frame = decode({ queue: QUEUE });
    // Assert
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    expect(frame.frame.value.entries[0]).toEqual({
      id: "q1",
      text: "run this later",
      queuedAtMs: 1700000000000,
      classification: "hold",
      rationale: "independent",
      accepted: false,
    });
  });

  it("decodes an empty queue as an empty entries list", () => {
    // Arrange / Act — "the queue is empty" is a value, not an absence.
    const frame = decode({ queue: { workspace: "ws", fence: "s1" } });
    // Assert
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    expect(frame.frame.value.entries).toEqual([]);
  });

  it("rejects an unset classification arm rather than defaulting it to pending", () => {
    // Arrange / Act / Assert — the verdict is a oneof since the figma-idl
    // reshape, so an unset arm is the only spelling of "nothing decided".
    // Reading it as `pending` would invent the claim the wire declined to make.
    expect(() =>
      decode({ queue: { fence: "s1", entries: [{ id: "q1", text: "x" }] } }),
    ).toThrow(/no classification arm/);
  });

  it("rejects two classification arms at once, which have no single badge", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({
        queue: {
          fence: "s1",
          entries: [{ id: "q1", pending: {}, interject: { rationale: "now" } }],
        },
      }),
    ).toThrow(/sets multiple classifications/);
  });

  it("decodes each real classification the daemon sends", () => {
    // Arrange
    const cases: Array<[Record<string, unknown>, string]> = [
      [{ pending: {} }, "pending"],
      [{ interject: { rationale: "now" } }, "interject"],
      [{ holdForTurnEnd: { rationale: "after" } }, "hold"],
      [{ error: { detail: "classifier died" } }, "error"],
    ];
    for (const [arm, want] of cases) {
      // Act
      const frame = decode({ queue: { fence: "s1", entries: [{ id: "q1", ...arm }] } });
      // Assert
      if (frame.frame.case !== "queue") throw new Error("wrong variant");
      expect(frame.frame.value.entries[0].classification).toBe(want);
    }
  });

  it("rejects an unrecognized classification arm rather than defaulting it", () => {
    // Arrange / Act / Assert — rendering an unknown verdict as `pending` would
    // tell the user their prompt is being judged when it is not. An arm this
    // build does not know is an unrecognized FIELD on the entry.
    expect(() =>
      decode({
        queue: { fence: "s1", entries: [{ id: "q1", pending: {}, xxVerdict: {} }] },
      }),
    ).toThrow(/unrecognized field/);
  });

  it("rejects an entry with no id, whose controls would all be dead", () => {
    // Arrange / Act / Assert — a real classification, so the missing id is
    // what fails rather than the classification check upstream of it.
    expect(() =>
      decode({
        queue: {
          fence: "s1",
          entries: [{ text: "x", pending: {} }],
        },
      }),
    ).toThrow(/missing required `id`/);
  });

  it("rejects an unrecognized field on an entry", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({ queue: { fence: "s1", entries: [{ id: "q1", bogus: 1 }] } }),
    ).toThrow(/unrecognized field/);
  });

  it("carries the queue through a StateSnapshot", () => {
    // Arrange / Act — a reconnecting frontend gets the queue in its snapshot.
    const frame = decode({ snapshot: { ...SNAPSHOT, queues: [QUEUE] } });
    // Assert
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.queues).toHaveLength(1);
  });

  it("decodes a snapshot with no queues as an empty list", () => {
    // Arrange / Act
    const frame = decode({ snapshot: SNAPSHOT });
    // Assert
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.queues).toEqual([]);
  });

  it("reports queue as visually supported", () => {
    // Arrange / Act / Assert
    expect(isVisuallySupportedFrame("queue")).toBe(true);
  });
});

describe("UNSUPPORTED_SHAPES registry", () => {
  it("lists non-rendered control and host-only frontend.v1 frames", () => {
    expect([...UNSUPPORTED_SHAPES.keys()]).toEqual([
      "commandAck",
      "daemonView",
      "workspaceAvailable",
      "hostAction",
    ]);
  });

  it("reports workspaceRoster as visually supported (it IS the sidebar rail)", () => {
    expect(isVisuallySupportedFrame("workspaceRoster")).toBe(true);
  });

  it("reports commandAck as visually unsupported", () => {
    expect(isVisuallySupportedFrame("commandAck")).toBe(false);
  });

  it("reports daemonView as visually unsupported", () => {
    expect(isVisuallySupportedFrame("daemonView")).toBe(false);
  });

  it("reports sessionInit as visually supported (feeds the /status panel)", () => {
    expect(isVisuallySupportedFrame("sessionInit")).toBe(true);
  });

  it("reports a mapped variant as visually supported", () => {
    expect(isVisuallySupportedFrame("workspaceState")).toBe(true);
  });

  it("reports progress as visually supported (it IS the footer)", () => {
    expect(isVisuallySupportedFrame("progress")).toBe(true);
  });
});

describe("SessionView.backfill decoding (F2)", () => {
  /** A SessionView frame body carrying an optional backfill value. */
  function sv(over: Record<string, unknown> = {}): string {
    return JSON.stringify({ sessionView: { sessionId: "s1", workspace: "/w", modelOptions: [], ...over } });
  }

  function backfillOf(json: string) {
    const got = decodeFrontendFrame(json);
    if (got.frame.case !== "sessionView") throw new Error("wrong variant");
    return got.frame.value.backfill;
  }

  it("reads a pre-F2 daemon's absent field as unspecified", () => {
    // Arrange / Act / Assert — same "nothing to backfill" a fresh ws has.
    expect(backfillOf(sv())).toBe("unspecified");
  });

  it("decodes pending", () => {
    expect(backfillOf(sv({ backfill: "BACKFILL_STATE_PENDING" }))).toBe("pending");
  });

  it("decodes done", () => {
    expect(backfillOf(sv({ backfill: "BACKFILL_STATE_DONE" }))).toBe("done");
  });

  it("decodes failed", () => {
    expect(backfillOf(sv({ backfill: "BACKFILL_STATE_FAILED" }))).toBe("failed");
  });

  it("rejects an unrecognized state rather than guessing", () => {
    // Arrange / Act / Assert — reading an unknown state as `done` would leave
    // a workspace blue with nothing retrying it.
    expect(() => backfillOf(sv({ backfill: "BACKFILL_STATE_TELEPORTING" }))).toThrow(
      /unrecognized value/,
    );
  });
});

describe("SessionView.death decoding (F4)", () => {
  /** A SessionView frame body, optionally carrying a classified death. */
  function sv(over: Record<string, unknown> = {}): string {
    return JSON.stringify({ sessionView: { sessionId: "s1", workspace: "/w", modelOptions: [], ...over } });
  }

  function deathOf(json: string) {
    const got = decodeFrontendFrame(json);
    if (got.frame.case !== "sessionView") throw new Error("wrong variant");
    return got.frame.value.death;
  }

  it("reads a live session's absent death as undefined", () => {
    // Arrange / Act / Assert — absence is the normal alive case, not an error.
    expect(deathOf(sv())).toBeUndefined();
  });

  it("decodes a terminal push's resolved death card instead of throwing", () => {
    // Arrange — the strict decoder once lacked this key, so every terminal
    // SessionView (delete, supersede, shim death) threw in the live frame
    // path; this pins the fix, now against the resolved card shape.
    const death = {
      kind: { sessionShimDied: {} },
      message: "shim process exited",
      terminal: {},
    };
    // Act
    const got = deathOf(sv({ death }));
    // Assert
    expect(got?.kind.kind.case).toBe("sessionShimDied");
  });

  it("rejects a death whose kind names no arm rather than guessing", () => {
    // Arrange / Act / Assert — the arm decides the card's side and therefore
    // its color; guessing one would paint the failure wrong, quietly.
    expect(() => deathOf(sv({ death: { kind: {}, message: "y", terminal: {} } }))).toThrow(
      /sets no failure kind/,
    );
  });
});

describe("ProgressView decoding (F1)", () => {
  /** A minimal well-formed ProgressView frame body. */
  function pv(over: Record<string, unknown> = {}): string {
    return JSON.stringify({
      progress: { workspace: "/w", fence: "s1", state: "RENDER_STATE_THINKING", ...over },
    });
  }

  it("decodes int64 fields from their protojson numeric strings", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(pv({ inputTokens: "41200", turnStartedAtMs: "1700000000000" }));
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.inputTokens).toBe(41200);
  });

  it("decodes an activity window", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(pv({ hook: { active: true, sinceMs: "5", detail: "PreToolUse" } }));
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.hook).toEqual({ active: true, sinceMs: 5, detail: "PreToolUse" });
  });

  it("leaves an absent window undefined rather than defaulting it open", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(pv());
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.compacting).toBeUndefined();
  });

  it("rejects a view with no workspace", () => {
    // Arrange / Act / Assert — it would address no session.
    expect(() =>
      decodeFrontendFrame(JSON.stringify({ progress: { fence: "s1", state: "RENDER_STATE_IDLE" } })),
    ).toThrow(/missing required `workspace`/);
  });

  it("accepts a view carrying no phase at all", () => {
    // Arrange / Act / Assert — the phase moved to WorkspaceState (F5), so a
    // ProgressView without one is the NORMAL shape rather than a malformed
    // frame. The daemon stopped populating the field, and protojson omits it.
    expect(() => decodeFrontendFrame(pv())).not.toThrow();
  });

  it("tolerates the deprecated phase mirror an older daemon still sends", () => {
    // Arrange / Act / Assert — the field stays on the wire until the
    // approval-gated removal pass, so it must not trip the strict decoder's
    // unrecognized-field rejection. It is accepted and not read.
    expect(() => decodeFrontendFrame(pv({ state: "RENDER_STATE_THINKING" }))).not.toThrow();
  });

  it("rejects an unrecognized field", () => {
    // Arrange / Act / Assert — strict decoding, never a silent drop.
    expect(() => decodeFrontendFrame(pv({ outputTokens: "9000" }))).toThrow(/unrecognized/);
  });

  it("decodes the session allowance's rate-limit window", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      pv({ rateLimited: { active: true, resetsAt: "1700000900", utilization: 0.91, status: "allowed_warning" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.rateLimited).toEqual({
      active: true,
      resetsAt: 1700000900,
      utilization: 0.91,
      status: "allowed_warning",
    });
  });

  it("decodes the WEEKLY allowance into its own field", () => {
    // Arrange / Act — the two allowances are separate facts on the wire.
    const got = decodeFrontendFrame(
      pv({ rateLimitedWeekly: { active: true, resetsAt: "1700500000", utilization: 0.5, status: "allowed_warning" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.rateLimitedWeekly?.resetsAt).toBe(1700500000);
  });

  it("names the weekly allowance in its decode errors, not the session's", () => {
    // Arrange / Act / Assert — a shared context string would send a reader
    // debugging one allowance to the other one's field.
    expect(() => decodeFrontendFrame(pv({ rateLimitedWeekly: { utilization: "lots" } }))).toThrow(
      /ProgressView\.rateLimitedWeekly/,
    );
  });

  it("leaves an unreported allowance undefined rather than defaulting it to zero", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(pv());
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.rateLimitedWeekly).toBeUndefined();
  });

  it("decodes the interrupt window's active, sinceMs and outcome together", () => {
    // Arrange / Act — all three fields ride on one message (I1).
    const got = decodeFrontendFrame(
      pv({ interrupt: { active: true, sinceMs: "42", outcome: "INTERRUPT_OUTCOME_INTERRUPTED" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.interrupt).toEqual({ active: true, sinceMs: 42, outcome: "interrupted" });
  });

  it("keeps ALREADY_COMPLETE distinct from INTERRUPTED", () => {
    // Arrange / Act — collapsing the two is the very confusion the outcome
    // enum exists to end.
    const got = decodeFrontendFrame(
      pv({ interrupt: { active: true, sinceMs: "1", outcome: "INTERRUPT_OUTCOME_ALREADY_COMPLETE" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.interrupt?.outcome).toBe("already_complete");
  });

  it("decodes the FAILED outcome", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      pv({ interrupt: { active: true, sinceMs: "1", outcome: "INTERRUPT_OUTCOME_FAILED" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.interrupt?.outcome).toBe("failed");
  });

  it("leaves a CLOSED window's outcome null rather than inventing one", () => {
    // Arrange / Act — a closed window has no outcome to carry.
    const got = decodeFrontendFrame(pv({ interrupt: { active: false, sinceMs: "0" } }));
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.interrupt).toEqual({ active: false, sinceMs: 0, outcome: null });
  });

  it("rejects an OPEN interrupt window carrying no outcome", () => {
    // Arrange / Act / Assert — the outcome is decided atomically on the ack
    // that opens the window, and protojson omits the UNSPECIFIED zero: an open
    // window with no outcome is a malformed frame, and picking one of the
    // three anyway would invent the claim the wire declined to make.
    expect(() => decodeFrontendFrame(pv({ interrupt: { active: true, sinceMs: "3" } }))).toThrow(
      /open with no outcome/,
    );
  });

  it("rejects an unrecognized interrupt outcome", () => {
    // Arrange / Act / Assert
    expect(() =>
      decodeFrontendFrame(pv({ interrupt: { active: true, outcome: "INTERRUPT_OUTCOME_WAT" } })),
    ).toThrow(/unrecognized value 'INTERRUPT_OUTCOME_WAT'/);
  });

  it("decodes a populated accounting cell rather than rejecting it as unknown", () => {
    // Arrange / Act — THE REGRESSION. The daemon populates `accounting`, and
    // an allowlist without it threw on every connect snapshot, so the page
    // never adopted and looped reconnect/reload forever.
    const got = decodeFrontendFrame(
      pv({ accounting: { summary: "12.3s · 41.2k in", complete: {} } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.accounting).toEqual({
      summary: "12.3s · 41.2k in",
      verdict: { kind: "complete" },
    });
  });

  it("decodes the INCOMPLETE verdict with the phrases it names", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      pv({ accounting: { summary: "partial", incomplete: { missing: ["turn start was never sampled"] } } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.accounting?.verdict).toEqual({
      kind: "incomplete",
      missing: ["turn start was never sampled"],
    });
  });

  it("decodes the INVALID verdict with the contradictions it names", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      pv({ accounting: { summary: "bad", invalid: { problems: ["per-model totals disagree"] } } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.accounting?.verdict).toEqual({
      kind: "invalid",
      problems: ["per-model totals disagree"],
    });
  });

  it("rejects an accounting cell with no summary", () => {
    // Arrange / Act / Assert — the summary IS the cell's text; an empty one
    // would draw a blank cell the daemon never asked for.
    expect(() => decodeFrontendFrame(pv({ accounting: { complete: {} } }))).toThrow(
      /ProgressView\.accounting requires a non-empty `summary`/,
    );
  });

  it("rejects an INCOMPLETE verdict naming nothing", () => {
    // Arrange / Act / Assert — an incompleteness with nothing to name is a
    // daemon fault, not a renderable state.
    expect(() =>
      decodeFrontendFrame(pv({ accounting: { summary: "s", incomplete: { missing: [] } } })),
    ).toThrow(/ProgressView\.accounting\.incomplete\.missing must not be empty/);
  });

  it("rejects an INVALID verdict naming nothing", () => {
    // Arrange / Act / Assert — the arm exists precisely because the evidence
    // is present; an empty list contradicts the arm.
    expect(() =>
      decodeFrontendFrame(pv({ accounting: { summary: "s", invalid: { problems: [] } } })),
    ).toThrow(/ProgressView\.accounting\.invalid\.problems must not be empty/);
  });

  it("rejects an accounting cell carrying no verdict arm", () => {
    // Arrange / Act / Assert — the arm decides the cell's class, so a cell
    // without one would be drawn as reconciled on this side's own authority.
    expect(() => decodeFrontendFrame(pv({ accounting: { summary: "s" } }))).toThrow(
      /ProgressView\.accounting requires a verdict oneof/,
    );
  });

  it("rejects an unrecognized field inside the accounting cell", () => {
    // Arrange / Act / Assert — strict decoding all the way down.
    expect(() =>
      decodeFrontendFrame(pv({ accounting: { summary: "s", complete: {}, bogus: 1 } })),
    ).toThrow(/ProgressView\.accounting has unrecognized field\(s\): bogus/);
  });

  it("decodes the footer phase cell's word, tone and breathing together", () => {
    // Arrange / Act — resolved display props, rendered verbatim.
    const got = decodeFrontendFrame(
      pv({ phase: { word: "thinking", tone: "purple", breathing: true } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.phase).toEqual({ word: "thinking", tone: "purple", breathing: true });
  });

  it("rejects a PRESENT phase cell with no word", () => {
    // Arrange / Act / Assert — absence of the message is "no phase cell", so a
    // present cell with nothing to say is a daemon fault.
    expect(() => decodeFrontendFrame(pv({ phase: { tone: "purple" } }))).toThrow(
      /ProgressView\.phase requires a non-empty `word`/,
    );
  });

  it("decodes the merge chip's text and tooltip", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      pv({ mergeChip: { text: "merge queued 2/3", title: "2 of 3 published" } }),
    );
    // Assert
    if (got.frame.case !== "progress") throw new Error("wrong variant");
    expect(got.frame.value.mergeChip).toEqual({
      text: "merge queued 2/3",
      title: "2 of 3 published",
    });
  });

  it("rejects a PRESENT merge chip with no text", () => {
    // Arrange / Act / Assert — absence of the message is "no chip".
    expect(() => decodeFrontendFrame(pv({ mergeChip: { title: "t" } }))).toThrow(
      /ProgressView\.mergeChip requires a non-empty `text`/,
    );
  });

  it("decodes a snapshot's progress list", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      JSON.stringify({
        snapshot: { progress: [{ workspace: "/w", fence: "s1", state: "RENDER_STATE_IDLE" }] },
      }),
    );
    // Assert
    if (got.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(got.frame.value.progress).toHaveLength(1);
  });

  it("leaves a pre-F1 daemon's snapshot progress list empty", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(JSON.stringify({ snapshot: {} }));
    // Assert
    if (got.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(got.frame.value.progress).toEqual([]);
  });
});

describe("WorkspaceState.mergeStatus decoding", () => {
  /** A merge status carrying `phase`, wrapped as a workspaceState frame. */
  function withStatus(phase: Record<string, unknown>): ReturnType<typeof decodeFrontendFrame> {
    return decode({
      workspaceState: {
        ...WS_STATE,
        mergeStatus: {
          runId: "run-1",
          phaseStartedAtMs: "1700000000000",
          updatedAtMs: "1700000000500",
          ...phase,
        },
      },
    });
  }

  /** The decoded status, or a thrown error when the frame carried none. */
  function statusOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    const status = frame.frame.value.mergeStatus;
    if (status === undefined) throw new Error("no merge status decoded");
    return status;
  }

  it("reads a workspace with no merge as an ABSENT status", () => {
    // Arrange / Act
    const frame = decode({ workspaceState: WS_STATE });
    // Assert
    if (frame.frame.case !== "workspaceState") throw new Error("wrong variant");
    expect(frame.frame.value.mergeStatus).toBeUndefined();
  });

  it("decodes the run identity every phase carries", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ enqueued: { position: 1, depth: 2 } }));
    // Assert
    expect(status.runId).toBe("run-1");
  });

  it("decodes the instant the current phase was entered", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ enqueued: { position: 1, depth: 2 } }));
    // Assert
    expect(status.phaseStartedAtMs).toBe(1700000000000);
  });

  it("decodes the within-phase tick instant separately from the phase's", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ enqueued: { position: 1, depth: 2 } }));
    // Assert
    expect(status.updatedAtMs).toBe(1700000000500);
  });

  it("decodes the enqueued phase's place in the queue", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ enqueued: { position: 2, depth: 3 } }));
    // Assert
    if (status.phase.case !== "enqueued") throw new Error("wrong phase");
    expect(status.phase.value).toEqual({ position: 2, depth: 3 });
  });

  it("rejects an enqueued place beyond the depth it indexes into", () => {
    // MOVED from the retired flat merge_queue_position / merge_queue_depth
    // pair, which used to carry this check. A 1-based place can never exceed
    // the queue holding it; rendering "3/2" would hide a daemon-side
    // accounting bug behind a plausible chip.
    expect(() => withStatus({ enqueued: { position: 3, depth: 2 } })).toThrow(/beyond depth/);
  });

  it("rejects a negative enqueued figure", () => {
    // MOVED from the retired flat pair for the same reason.
    expect(() => withStatus({ enqueued: { position: -1, depth: 3 } })).toThrow(
      /negative merge-queue figure/,
    );
  });

  it("decodes the before-action phase's prompt", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ beforeAction: { prompt: "run the linter" } }));
    // Assert
    if (status.phase.case !== "beforeAction") throw new Error("wrong phase");
    expect(status.phase.value.prompt).toBe("run the linter");
  });

  it("decodes the cherry-picking phase's commit walk", () => {
    // Arrange / Act
    const status = statusOf(
      withStatus({
        cherryPicking: {
          commitsTotal: 4,
          commitsLanded: 1,
          currentSha: "abc1234",
          currentSubject: "fix the thing",
        },
      }),
    );
    // Assert
    if (status.phase.case !== "cherryPicking") throw new Error("wrong phase");
    expect(status.phase.value).toEqual({
      commitsTotal: 4,
      commitsLanded: 1,
      currentSha: "abc1234",
      currentSubject: "fix the thing",
    });
  });

  it("decodes the testing phase's commit under test", () => {
    // Arrange / Act
    const status = statusOf(
      withStatus({
        testing: {
          commitsTotal: 4,
          commitsLanded: 2,
          currentSha: "def5678",
          currentSubject: "cover the thing",
        },
      }),
    );
    // Assert
    if (status.phase.case !== "testing") throw new Error("wrong phase");
    expect(status.phase.value.currentSha).toBe("def5678");
  });

  it("decodes the conflict phase's conflicted commit", () => {
    // Arrange / Act
    const status = statusOf(
      withStatus({
        conflict: {
          conflictedSha: "aaa1111",
          conflictedSubject: "touch shared.txt",
          commitsTotal: 3,
          commitsLanded: 1,
        },
      }),
    );
    // Assert
    if (status.phase.case !== "conflict") throw new Error("wrong phase");
    expect(status.phase.value.conflictedSubject).toBe("touch shared.txt");
  });

  it("decodes the after-action phase's prompt", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ afterAction: { prompt: "announce the merge" } }));
    // Assert
    if (status.phase.case !== "afterAction") throw new Error("wrong phase");
    expect(status.phase.value.prompt).toBe("announce the merge");
  });

  it("decodes the merged phase's landed commit count", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ merged: { commitsTotal: 3 } }));
    // Assert
    if (status.phase.case !== "merged") throw new Error("wrong phase");
    expect(status.phase.value.commitsTotal).toBe(3);
  });

  it("reads a merged run's absent after-action error as empty", () => {
    // An after action that succeeded, or one that never ran, is protojson's
    // omitted default rather than a shape of its own.
    // Arrange / Act
    const status = statusOf(withStatus({ merged: { commitsTotal: 3 } }));
    // Assert
    if (status.phase.case !== "merged") throw new Error("wrong phase");
    expect(status.phase.value.afterActionError).toBe("");
  });

  it("decodes the failed phase's cause", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ failed: { cause: "merge enqueue refused" } }));
    // Assert
    if (status.phase.case !== "failed") throw new Error("wrong phase");
    expect(status.phase.value.cause).toBe("merge enqueue refused");
  });

  it("decodes the failed phase's whole-record JSON", () => {
    // The daemon serializes the failed arm with proto3's JSON mapping so a
    // frontend can report the WHOLE record; the decoder carries it verbatim
    // rather than re-deriving one from the fields beside it.
    // Arrange / Act
    const record = '{"cause":"merge enqueue refused","commitsTotal":3}';
    const status = statusOf(
      withStatus({ failed: { cause: "merge enqueue refused", failedJson: record } }),
    );
    // Assert
    if (status.phase.case !== "failed") throw new Error("wrong phase");
    expect(status.phase.value.failedJson).toBe(record);
  });

  it("decodes a failed run that never finished planning as zero commits", () => {
    // Arrange / Act
    const status = statusOf(withStatus({ failed: { cause: "geometry unresolvable" } }));
    // Assert
    if (status.phase.case !== "failed") throw new Error("wrong phase");
    expect(status.phase.value.commitsTotal).toBe(0);
  });

  it("rejects a status that names no phase", () => {
    // WHICH member is set IS the phase, so a status naming none says nothing a
    // renderer could paint.
    expect(() =>
      decode({
        workspaceState: { ...WS_STATE, mergeStatus: { runId: "run-1", updatedAtMs: "1" } },
      }),
    ).toThrow(/sets no phase/);
  });

  it("rejects a status that names two phases", () => {
    expect(() =>
      withStatus({ enqueued: { position: 1, depth: 1 }, merged: { commitsTotal: 1 } }),
    ).toThrow(/sets multiple phases/);
  });

  it("rejects a status with no run id", () => {
    expect(() =>
      decode({
        workspaceState: { ...WS_STATE, mergeStatus: { merged: { commitsTotal: 1 } } },
      }),
    ).toThrow(/missing required `runId`/);
  });

  it("rejects a phase this build has never heard of", () => {
    expect(() => withStatus({ rebasing: { commitsTotal: 1 } })).toThrow(
      /MergeStatus has unrecognized field\(s\): rebasing/,
    );
  });

  it("rejects an unrecognized field inside a phase", () => {
    expect(() => withStatus({ enqueued: { position: 1, depth: 1, eta: 5 } })).toThrow(
      /MergeStatusEnqueued has unrecognized field\(s\): eta/,
    );
  });
});

// --- the scheduled-shutdown drain lease -------------------------------------

const DRAIN_HOLD = { workspace: "/w/app", sessionId: "s1", turn: { turnId: "t-1" } };
const DRAINING = {
  scheduleId: "sched-1",
  scheduledAtMs: "1700000000000",
  cause: "merge of ws-7 rebuilt the daemon",
  stopShims: false,
  holds: [DRAIN_HOLD],
};

/** Decode a shutdownSchedule frame carrying VIEW. */
function schedule(view: unknown): ReturnType<typeof decodeFrontendFrame> {
  return decode({ shutdownSchedule: view });
}

describe("decodeFrontendFrame — ShutdownScheduleView", () => {
  it("decodes the idle arm", () => {
    // Arrange / Act — a cancel or a completed drain broadcasts idle.
    const frame = schedule({ idle: {} });
    // Assert
    if (frame.frame.case !== "shutdownSchedule") throw new Error("wrong variant");
    expect(frame.frame.value.state.case).toBe("idle");
  });

  it("decodes the draining arm", () => {
    const frame = schedule({ draining: DRAINING });
    if (frame.frame.case !== "shutdownSchedule") throw new Error("wrong variant");
    expect(frame.frame.value.state.case).toBe("draining");
  });

  it("carries the schedule id the cancel command must name", () => {
    const frame = schedule({ draining: DRAINING });
    if (frame.frame.case !== "shutdownSchedule") throw new Error("wrong variant");
    if (frame.frame.value.state.case !== "draining") throw new Error("wrong arm");
    expect(frame.frame.value.state.value.scheduleId).toBe("sched-1");
  });

  it("rejects a view that sets no arm, since idle is a real value", () => {
    expect(() => schedule({})).toThrow(/ShutdownScheduleView sets no state/);
  });

  it("rejects a view that sets both arms", () => {
    expect(() => schedule({ idle: {}, draining: DRAINING })).toThrow(
      /sets multiple states: idle, draining/,
    );
  });

  it("rejects an arm this build has never heard of", () => {
    expect(() => schedule({ paused: {} })).toThrow(
      /ShutdownScheduleView has unrecognized field\(s\): paused/,
    );
  });

  it("rejects an idle arm carrying fields", () => {
    expect(() => schedule({ idle: { scheduleId: "x" } })).toThrow(
      /ShutdownScheduleIdle has unrecognized field\(s\): scheduleId/,
    );
  });

  it("rejects a draining lease with no schedule id", () => {
    expect(() => schedule({ draining: { ...DRAINING, scheduleId: "" } })).toThrow(
      /ShutdownScheduleDraining missing required `scheduleId`/,
    );
  });

  it("rejects a draining lease with no stamp to count elapsed from", () => {
    expect(() => schedule({ draining: { ...DRAINING, scheduledAtMs: "0" } })).toThrow(
      /non-positive scheduledAtMs/,
    );
  });

  it("rejects a draining lease whose holds list is empty", () => {
    // A drained lease is EXECUTED, never broadcast.
    expect(() => schedule({ draining: { ...DRAINING, holds: [] } })).toThrow(
      /carries an empty holds list/,
    );
  });

  it("rejects a draining lease with no holds field at all", () => {
    const { holds: _holds, ...noHolds } = DRAINING;
    expect(() => schedule({ draining: noHolds })).toThrow(/carries an empty holds list/);
  });

  it("rejects an unrecognized field on the draining arm", () => {
    expect(() => schedule({ draining: { ...DRAINING, deadlineMs: "1" } })).toThrow(
      /ShutdownScheduleDraining has unrecognized field\(s\): deadlineMs/,
    );
  });
});

describe("decodeFrontendFrame — ShutdownHold", () => {
  /** Decode a draining lease whose single hold is HOLD. */
  function withHold(hold: unknown): ReturnType<typeof decodeFrontendFrame> {
    return schedule({ draining: { ...DRAINING, holds: [hold] } });
  }

  /** The decoded single hold of a draining lease. */
  function holdOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "shutdownSchedule") throw new Error("wrong variant");
    if (frame.frame.value.state.case !== "draining") throw new Error("wrong arm");
    return frame.frame.value.state.value.holds[0];
  }

  it("decodes a turn-only hold", () => {
    expect(holdOf(withHold(DRAIN_HOLD)).turn?.turnId).toBe("t-1");
  });

  it("decodes a tasks-only hold", () => {
    const hold = { workspace: "/w/app", sessionId: "s1", tasks: { count: 3 } };
    expect(holdOf(withHold(hold)).tasks?.count).toBe(3);
  });

  it("decodes a hold carrying a turn AND tasks, which co-occur", () => {
    const hold = { ...DRAIN_HOLD, tasks: { count: 2 } };
    const decoded = holdOf(withHold(hold));
    expect([decoded.turn?.turnId, decoded.tasks?.count]).toEqual(["t-1", 2]);
  });

  it("leaves the tasks arm absent on a turn-only hold", () => {
    expect(holdOf(withHold(DRAIN_HOLD)).tasks).toBeUndefined();
  });

  it("rejects a hold that names neither a turn nor tasks", () => {
    expect(() => withHold({ workspace: "/w/app", sessionId: "s1" })).toThrow(
      /names neither a turn nor tasks/,
    );
  });

  it("rejects a hold with no workspace to attribute it to", () => {
    expect(() => withHold({ ...DRAIN_HOLD, workspace: "" })).toThrow(
      /ShutdownHold missing `workspace` or `sessionId`/,
    );
  });

  it("rejects a hold with no session id, which could be pinned on a successor", () => {
    expect(() => withHold({ ...DRAIN_HOLD, sessionId: "" })).toThrow(
      /ShutdownHold missing `workspace` or `sessionId`/,
    );
  });

  it("rejects a turn hold that names no turn", () => {
    expect(() => withHold({ ...DRAIN_HOLD, turn: {} })).toThrow(
      /ShutdownHoldTurn missing required `turnId`/,
    );
  });

  it("rejects a tasks hold whose count denies the tasks it claims", () => {
    const hold = { workspace: "/w/app", sessionId: "s1", tasks: { count: 0 } };
    expect(() => withHold(hold)).toThrow(/non-positive count/);
  });

  it("rejects an unrecognized field on a hold", () => {
    expect(() => withHold({ ...DRAIN_HOLD, permission: {} })).toThrow(
      /ShutdownHold has unrecognized field\(s\): permission/,
    );
  });

  it("rejects a holds list that is not an array", () => {
    expect(() => schedule({ draining: { ...DRAINING, holds: DRAIN_HOLD } })).toThrow(
      /ShutdownScheduleDraining.holds must be a JSON array/,
    );
  });
});

describe("decodeFrontendFrame — the lease in a StateSnapshot", () => {
  it("seeds the lease from a connect snapshot", () => {
    const frame = decode({ snapshot: { ...SNAPSHOT, shutdownSchedule: { draining: DRAINING } } });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.shutdownSchedule?.state.case).toBe("draining");
  });

  it("leaves the lease absent when the snapshot does not carry it", () => {
    // Absence is the absence of INFORMATION, never a claim of idle.
    const frame = decode({ snapshot: SNAPSHOT });
    if (frame.frame.case !== "snapshot") throw new Error("wrong variant");
    expect(frame.frame.value.shutdownSchedule).toBeUndefined();
  });

  it("rejects a malformed lease inside an otherwise valid snapshot", () => {
    expect(() => decode({ snapshot: { ...SNAPSHOT, shutdownSchedule: {} } })).toThrow(
      /ShutdownScheduleView sets no state/,
    );
  });
});

describe("decodeFrontendFrame — QueueEntry.shutdownHold", () => {
  /** Decode a queue whose single entry carries HOLD. */
  function entryWith(hold: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({
      queue: {
        fence: "s1",
        entries: [
          {
            id: "q1",
            text: "later",
            pending: {},
            shutdown: hold,
          },
        ],
      },
    });
  }

  /** The decoded single entry of a queue frame. */
  function entryOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    return frame.frame.value.entries[0];
  }

  it("decodes the schedule holding a parked prompt", () => {
    expect(entryOf(entryWith({ scheduleId: "sched-1" })).shutdownHold?.scheduleId).toBe("sched-1");
  });

  it("leaves the hold absent on an ordinary classifier-held entry", () => {
    expect(entryOf(decode({ queue: QUEUE })).shutdownHold).toBeUndefined();
  });

  it("rejects a hold that names no schedule", () => {
    expect(() => entryWith({})).toThrow(/QueueEntryShutdownHold missing required `scheduleId`/);
  });

  it("rejects an unrecognized field on the hold", () => {
    expect(() => entryWith({ scheduleId: "sched-1", cause: "x" })).toThrow(
      /QueueEntryShutdownHold has unrecognized field\(s\): cause/,
    );
  });
});

describe("decodeFrontendFrame — SessionView.hibernation", () => {
  /** Decode a SessionView carrying the given hibernation detail. */
  function svWith(hibernation: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({ sessionView: { ...SESSION_VIEW, hibernation } });
  }

  /** The decoded SessionView of a sessionView frame. */
  function viewOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    return frame.frame.value;
  }

  it("decodes the idle-cutoff cause with the cutoff that tripped", () => {
    // Arrange / Act
    const got = viewOf(svWith({ sinceMs: "1700000000000", idleCutoff: { cutoffMs: "3600000" } }));
    // Assert
    expect(got.hibernation?.cause).toEqual({ case: "idleCutoff", value: { cutoffMs: 3600000 } });
  });

  it("decodes the forced cause, whose empty arm IS the whole claim", () => {
    // Arrange / Act
    const got = viewOf(svWith({ sinceMs: "1700000000000", forced: {} }));
    // Assert
    expect(got.hibernation?.cause).toEqual({ case: "forced", value: {} });
  });

  it("decodes the cache-expired cause with both the elapsed time and the TTL", () => {
    // Arrange / Act
    const got = viewOf(svWith({ sinceMs: "1", cacheExpired: { elapsedMs: "900000", ttlMs: "300000" } }));
    // Assert
    expect(got.hibernation?.cause).toEqual({
      case: "cacheExpired",
      value: { elapsedMs: 900000, ttlMs: 300000 },
    });
  });

  it("carries the since stamp the gate ages from", () => {
    // Arrange / Act
    const got = viewOf(svWith({ sinceMs: "1700000000000", forced: {} }));
    // Assert
    expect(got.hibernation?.sinceMs).toBe(1700000000000);
  });

  it("leaves the detail absent on an awake session", () => {
    // Arrange / Act
    const got = viewOf(decode({ sessionView: SESSION_VIEW }));
    // Assert
    expect(got.hibernation).toBeUndefined();
  });

  it("rejects a detail that names no cause, since the gate has nothing honest to say", () => {
    // Arrange / Act / Assert
    expect(() => svWith({ sinceMs: "1" })).toThrow(/SessionView.hibernation sets no cause/);
  });

  it("rejects a detail setting two causes at once", () => {
    // Arrange / Act / Assert
    expect(() => svWith({ sinceMs: "1", forced: {}, idleCutoff: { cutoffMs: "1" } })).toThrow(
      /SessionView.hibernation sets multiple causes/,
    );
  });

  it("rejects an unrecognized field on the detail", () => {
    // Arrange / Act / Assert
    expect(() => svWith({ sinceMs: "1", forced: {}, why: "x" })).toThrow(
      /SessionView.hibernation has unrecognized field\(s\): why/,
    );
  });

  it("rejects an unrecognized field inside the empty forced arm", () => {
    // Arrange / Act / Assert
    expect(() => svWith({ sinceMs: "1", forced: { by: "me" } })).toThrow(
      /SessionView.hibernation.forced has unrecognized field\(s\): by/,
    );
  });
});

describe("decodeFrontendFrame — QueueEntry.keepAliveHold", () => {
  /** Decode a queue whose single entry carries a keep-alive hold. */
  function entryWith(hold: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({
      queue: {
        fence: "s1",
        entries: [
          { id: "q1", text: "later", pending: {}, keepAlive: hold },
        ],
      },
    });
  }

  /** The decoded single entry of a queue frame. */
  function entryOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    return frame.frame.value.entries[0];
  }

  it("decodes the keep-alive turn holding a parked prompt", () => {
    // Arrange / Act / Assert
    expect(entryOf(entryWith({ turnId: "turn-9" })).keepAliveHold?.turnId).toBe("turn-9");
  });

  it("leaves the hold absent on an ordinary classifier-held entry", () => {
    // Arrange / Act / Assert
    expect(entryOf(decode({ queue: QUEUE })).keepAliveHold).toBeUndefined();
  });

  it("rejects a hold that names no turn", () => {
    // Arrange / Act / Assert
    expect(() => entryWith({})).toThrow(/QueueEntryKeepAliveHold missing required `turnId`/);
  });

  it("rejects an unrecognized field on the hold", () => {
    // Arrange / Act / Assert
    expect(() => entryWith({ turnId: "t1", scheduleId: "s" })).toThrow(
      /QueueEntryKeepAliveHold has unrecognized field\(s\): scheduleId/,
    );
  });
});

describe("decodeFrontendFrame — QueueEntry.revivalHold", () => {
  /** Decode a queue whose single entry carries a revival hold. */
  function entryWith(hold: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({
      queue: {
        fence: "s1",
        entries: [
          { id: "q1", text: "later", pending: {}, revival: hold },
        ],
      },
    });
  }

  /** The decoded single entry of a queue frame. */
  function entryOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    return frame.frame.value.entries[0];
  }

  it("decodes the bare marker that parks a prompt behind a revival", () => {
    // Arrange / Act / Assert — the hold names nothing after the figma-idl
    // reshape; its PRESENCE is the whole claim.
    expect(entryOf(entryWith({})).revivalHold).toEqual({});
  });

  it("leaves the hold absent on an ordinary classifier-held entry", () => {
    // Arrange / Act / Assert
    expect(entryOf(decode({ queue: QUEUE })).revivalHold).toBeUndefined();
  });

  it("rejects the retired session id the hold used to carry", () => {
    // Arrange / Act / Assert — a producer still stamping it is out of contract,
    // and dropping it silently would hide that.
    expect(() => entryWith({ sessionId: "sess-4" })).toThrow(
      /QueueEntryRevivalHold has unrecognized field\(s\): sessionId/,
    );
  });

  it("rejects an unrecognized field on the hold", () => {
    // Arrange / Act / Assert
    expect(() => entryWith({ turnId: "t" })).toThrow(
      /QueueEntryRevivalHold has unrecognized field\(s\): turnId/,
    );
  });
});

describe("decodeFrontendFrame — ProgressView.expensiveTurn", () => {
  const ALERT = {
    turnId: "turn-3",
    uncachedInputTokens: "48000",
    thresholdTokens: "20000",
    atMs: "1700000000000",
    promptOrigin: "PROMPT_ORIGIN_WEBAPP_USER_SENT",
  };

  /** Decode a ProgressView carrying the given context-cost alert. */
  function pvWith(expensiveTurn: unknown): ReturnType<typeof decodeFrontendFrame> {
    return decode({ progress: { workspace: "/w", fence: "s1", expensiveTurn } });
  }

  /** The decoded ProgressView of a progress frame. */
  function progressOf(frame: ReturnType<typeof decodeFrontendFrame>) {
    if (frame.frame.case !== "progress") throw new Error("wrong variant");
    return frame.frame.value;
  }

  it("decodes the alert's int64 token figures from their protojson strings", () => {
    // Arrange / Act
    const got = progressOf(pvWith(ALERT));
    // Assert
    expect(got.expensiveTurn?.uncachedInputTokens).toBe(48000);
  });

  it("carries the prompt origin verbatim so the cold-ping reading stays separable", () => {
    // Arrange / Act
    const got = progressOf(pvWith({ ...ALERT, promptOrigin: "PROMPT_ORIGIN_CACHE_KEEP_ALIVE" }));
    // Assert
    expect(got.expensiveTurn?.promptOrigin).toBe("PROMPT_ORIGIN_CACHE_KEEP_ALIVE");
  });

  it("leaves the alert absent after a cache-efficient turn", () => {
    // Arrange / Act
    const got = progressOf(decode({ progress: { workspace: "/w", fence: "s1" } }));
    // Assert
    expect(got.expensiveTurn).toBeUndefined();
  });

  it("rejects an alert naming no turn", () => {
    // Arrange / Act / Assert
    expect(() => pvWith({ ...ALERT, turnId: "" })).toThrow(
      /ProgressView.expensiveTurn missing required `turnId`/,
    );
  });

  it("rejects an origin that is not a canonical PromptOrigin name", () => {
    // Arrange / Act / Assert
    expect(() => pvWith({ ...ALERT, promptOrigin: "keep-alive" })).toThrow(
      /promptOrigin has unrecognized value 'keep-alive'/,
    );
  });

  it("rejects an UNSPECIFIED origin, which cannot be told from a cold keep-alive", () => {
    // Arrange / Act / Assert
    expect(() => pvWith({ ...ALERT, promptOrigin: "PROMPT_ORIGIN_UNSPECIFIED" })).toThrow(
      /promptOrigin is UNSPECIFIED/,
    );
  });

  it("rejects an unrecognized field on the alert", () => {
    // Arrange / Act / Assert
    expect(() => pvWith({ ...ALERT, modelName: "opus" })).toThrow(
      /ProgressView.expensiveTurn has unrecognized field\(s\): modelName/,
    );
  });
});


// --- the resolved component views (frames 21/22/23, snapshot 12/13/14) -------

describe("TopbarView decoding", () => {
  const TOPBAR = {
    workspace: "/ws",
    title: "agent-repl · main",
    sessionLine: "session since 09:00",
    modelDisplay: "opus-5",
    modelOptions: [{ value: "opus-5", displayName: "Opus 5", description: "top" }],
    accountingLine: "12s · 4k in",
    fence: "f1",
  };

  function topbarOf(over: Record<string, unknown> = {}) {
    const got = decode({ topbar: { ...TOPBAR, ...over } });
    if (got.frame.case !== "topbar") throw new Error("wrong variant");
    return got.frame.value;
  }

  it("decodes the composed title verbatim", () => {
    // Arrange / Act / Assert — the client never re-composes it.
    expect(topbarOf().title).toBe("agent-repl · main");
  });

  it("keeps the daemon's model-option order", () => {
    // Arrange / Act
    const got = topbarOf({
      modelOptions: [
        { value: "a", displayName: "A", description: "" },
        { value: "b", displayName: "B", description: "" },
      ],
    });
    // Assert
    expect(got.modelOptions.map((m) => m.value)).toEqual(["a", "b"]);
  });

  it("leaves an unpublished connectivity ABSENT rather than defaulting one", () => {
    // Arrange / Act / Assert — a placeholder glyph would be a claim the daemon
    // did not make.
    expect(topbarOf().connectivity).toBeUndefined();
  });

  it("carries a published connectivity's resolved tone", () => {
    // Arrange / Act
    const got = topbarOf({ connectivity: { tone: "ok", glyph: "●", title: "connected" } });
    // Assert
    expect(got.connectivity?.tone).toBe("ok");
  });

  it("REJECTS a view with no fence, which nothing could be compared against", () => {
    // Arrange / Act / Assert
    expect(() => topbarOf({ fence: "" })).toThrow(/TopbarView missing required `fence`/);
  });

  it("rejects an unrecognized field", () => {
    // Arrange / Act / Assert — the key set is anchored to the generated
    // manifest, so drift fails the build; this is its runtime half.
    expect(() => topbarOf({ sessionId: "s1" })).toThrow(/unrecognized field\(s\): sessionId/);
  });
});

describe("TokenBreakdownView decoding", () => {
  const ROW = {
    label: "cache read",
    tokens: "12000",
    sharePermille: 812,
    emphasized: true,
    depth: 0,
  };
  const VIEW = {
    workspace: "/ws",
    fence: "f1",
    sections: [{ label: "this turn", rows: [ROW] }],
  };

  function breakdownOf(over: Record<string, unknown> = {}) {
    const got = decode({ tokenBreakdown: { ...VIEW, ...over } });
    if (got.frame.case !== "tokenBreakdown") throw new Error("wrong variant");
    return got.frame.value;
  }

  it("carries the row's count as the daemon resolved it", () => {
    // Arrange / Act / Assert
    expect(breakdownOf().sections[0].rows[0].tokens).toBe(12000);
  });

  it("carries the precomputed share without recomputing it", () => {
    // Arrange / Act / Assert
    expect(breakdownOf().sections[0].rows[0].sharePermille).toBe(812);
  });

  it("preserves the -1 sentinel that means no share applies", () => {
    // Arrange / Act — collapsing it to 0 would print a false zero percent.
    const got = breakdownOf({
      sections: [{ label: "s", rows: [{ ...ROW, sharePermille: -1 }] }],
    });
    // Assert
    expect(got.sections[0].rows[0].sharePermille).toBe(-1);
  });

  it("carries the daemon's layout facts", () => {
    // Arrange / Act / Assert
    expect(breakdownOf().sections[0].rows[0].emphasized).toBe(true);
  });

  it("REJECTS a view with no fence", () => {
    // Arrange / Act / Assert
    expect(() => breakdownOf({ fence: "" })).toThrow(
      /TokenBreakdownView missing required `fence`/,
    );
  });

  it("rejects the retired sessionId, which the fence replaced", () => {
    // Arrange / Act / Assert
    expect(() => breakdownOf({ sessionId: "s1" })).toThrow(
      /unrecognized field\(s\): sessionId/,
    );
  });
});

describe("WorkspaceGateView decoding", () => {
  function gateOf(over: Record<string, unknown>) {
    const got = decode({ workspaceGate: { workspace: "/ws", fence: "f1", ...over } });
    if (got.frame.case !== "workspaceGate") throw new Error("wrong variant");
    return got.frame.value;
  }

  it("reads the OPEN arm", () => {
    // Arrange / Act / Assert
    expect(gateOf({ open: {} }).gate).toEqual({ case: "open" });
  });

  it("reads the HIBERNATED arm with the cause the card renders", () => {
    // Arrange / Act
    const got = gateOf({ hibernated: { detail: { sinceMs: "1700000000000", forced: {} } } });
    // Assert
    expect(got.gate).toEqual({
      case: "hibernated",
      detail: { sinceMs: 1700000000000, cause: { case: "forced", value: {} } },
    });
  });

  it("THROWS when no gate arm is set rather than assuming open", () => {
    // Arrange / Act / Assert — defaulting open would let a prompt go to a
    // sleeping session; defaulting closed would lock a live composer.
    expect(() => gateOf({})).toThrow(/requires exactly one gate arm/);
  });

  it("THROWS when BOTH gate arms are set", () => {
    // Arrange / Act / Assert
    expect(() =>
      gateOf({ open: {}, hibernated: { detail: { sinceMs: "1", forced: {} } } }),
    ).toThrow(/requires exactly one gate arm/);
  });

  it("THROWS when a hibernated gate carries no detail to render", () => {
    // Arrange / Act / Assert — a closed gate always arrives with its account.
    expect(() => gateOf({ hibernated: {} })).toThrow(/requires `detail`/);
  });

  it("REJECTS a view with no fence", () => {
    // Arrange / Act / Assert — adopting half of a stale gate is how a composer
    // ends up locked against a workspace that is already awake.
    expect(() => decode({ workspaceGate: { workspace: "/ws", fence: "", open: {} } })).toThrow(
      /WorkspaceGateView missing required `fence`/,
    );
  });
});

describe("the connect snapshot's resolved views", () => {
  it("carries the topbars the snapshot seeds", () => {
    // Arrange / Act — a connecting client draws its chrome without waiting for
    // each view's first push.
    const got = decode({
      snapshot: {
        topbars: [
          {
            workspace: "/ws",
            title: "t",
            sessionLine: "",
            modelDisplay: "",
            modelOptions: [],
            accountingLine: "",
            fence: "f1",
          },
        ],
      },
    });
    if (got.frame.case !== "snapshot") throw new Error("wrong variant");
    // Assert
    expect(got.frame.value.topbars).toHaveLength(1);
  });

  it("reads an absent view list as EMPTY, never as a stand-in", () => {
    // Arrange / Act — a workspace with no entry has nothing published, and the
    // client renders that absence.
    const got = decode({ snapshot: {} });
    if (got.frame.case !== "snapshot") throw new Error("wrong variant");
    // Assert
    expect(got.frame.value.workspaceGates).toEqual([]);
  });
});

describe("WorkspaceState.fence", () => {
  it("decodes the fence every fenced push is measured against", () => {
    // Arrange / Act — the decoder REJECTED this field outright before, which
    // took down every frame from a daemon that sends it.
    const got = decode({ workspaceState: { ...WS_STATE, fence: "f1" } });
    if (got.frame.case !== "workspaceState") throw new Error("wrong variant");
    // Assert
    expect(got.frame.value.fence).toBe("f1");
  });
});

describe("AgentResponse.usage_stamp", () => {
  function itemWith(response: Record<string, unknown>) {
    const got = decode({
      conversationDelta: {
        workspace: "/ws",
        fence: "f1",
        throughSeq: "1",
        items: [{ uuid: "m1", source: "CONVERSATION_SOURCE_USER", agent: { response } }],
      },
    });
    if (got.frame.case !== "conversationDelta") throw new Error("wrong variant");
    return got.frame.value.items[0];
  }

  it("carries the resolved stamp beside the response body", () => {
    // Arrange / Act — the body is verbatim durable evidence and must not grow a
    // field the vendor never sent.
    const item = itemWith({
      body: { id: "msg-1", content: [] },
      usageStamp: {
        expensiveInputTokens: "4200",
        cacheReadTokens: "118000",
        outputTokens: "900",
        model: "claude-opus-5",
      },
    });
    // Assert
    expect(item.usageStamp?.expensiveInputTokens).toBe(4200);
  });

  it("leaves an ABSENT stamp absent rather than synthesizing zeros", () => {
    // Arrange / Act — zeros would read as a response that cost nothing.
    const item = itemWith({ body: { id: "msg-1", content: [] } });
    // Assert
    expect(item.usageStamp).toBeUndefined();
  });

  it("rejects an unrecognized field on the stamp", () => {
    // Arrange / Act / Assert
    expect(() =>
      itemWith({ body: { id: "m", content: [] }, usageStamp: { totalTokens: "1" } }),
    ).toThrow(/unrecognized field\(s\): totalTokens/);
  });
});

// --- async bubbles: the three places the wire carries detached work ---------

describe("async-bubble decode entry points", () => {
  const BUBBLE = { id: "b1", liveness: { live: {} }, agent: {} };

  it("decodes the asyncBubbleDelta frame variant", () => {
    // Arrange / Act
    const frame = decode({ asyncBubbleDelta: { workspace: "/w", fence: "f1", opened: [BUBBLE] } });

    // Assert
    expect(frame.frame.case === "asyncBubbleDelta" && frame.frame.value.opened[0].id).toBe("b1");
  });

  it("decodes StateSnapshot.asyncBubbles, the reconnect resume set", () => {
    // Arrange / Act
    const frame = decode({ snapshot: { asyncBubbles: [BUBBLE] } });

    // Assert
    expect(frame.frame.case === "snapshot" && frame.frame.value.asyncBubbles.map((b) => b.id)).toEqual(["b1"]);
  });

  it("decodes ConversationItem.asyncBubble, the feed-anchored opening state", () => {
    // Arrange / Act
    const frame = decode({
      conversationDelta: {
        workspace: "ws",
        fence: "s1",
        throughSeq: "1",
        items: [{ uuid: "u1", tsMs: "1", source: "CONVERSATION_SOURCE_USER", asyncBubble: BUBBLE }],
      },
    });

    // Assert
    expect(frame.frame.case === "conversationDelta" && frame.frame.value.items[0].asyncBubble?.id).toBe("b1");
  });

  it("rejects a feed-anchored bubble with no kind arm, loudly, at decode", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({
        conversationDelta: {
          workspace: "ws",
          fence: "s1",
          throughSeq: "1",
          items: [
            {
              uuid: "u1",
              tsMs: "1",
              source: "CONVERSATION_SOURCE_USER",
              asyncBubble: { id: "b1", liveness: { live: {} } },
            },
          ],
        },
      }),
    ).toThrow(/requires exactly one of agent, journal, shell, unclassified/);
  });

  it("carries a tool call's spawned_bubble_id through as the classification verdict", () => {
    // Arrange / Act
    const frame = decode({
      conversationDelta: {
        workspace: "ws",
        fence: "s1",
        throughSeq: "1",
        items: [
          {
            uuid: "u1",
            tsMs: "1",
            source: "CONVERSATION_SOURCE_USER",
            agent: { toolCall: { call: { id: "tu1", name: "Task" }, spawnedBubbleId: "b1" } },
          },
        ],
      },
    });

    // Assert
    expect(frame.frame.case === "conversationDelta" && frame.frame.value.items[0].spawnedBubbleId).toBe("b1");
  });

  it("leaves spawnedBubbleId ABSENT when the call detached nothing", () => {
    // Arrange / Act
    const frame = decode({
      conversationDelta: {
        workspace: "ws",
        fence: "s1",
        throughSeq: "1",
        items: [
          {
            uuid: "u1",
            tsMs: "1",
            source: "CONVERSATION_SOURCE_USER",
            agent: { toolCall: { call: { id: "tu1", name: "Read" } } },
          },
        ],
      },
    });

    // Assert
    expect(frame.frame.case === "conversationDelta" && "spawnedBubbleId" in frame.frame.value.items[0]).toBe(false);
  });
});

describe("the build-refresh queue hold", () => {
  const ENTRY = {
    id: "q1",
    text: "hi",
    queuedAtMs: "1",
    holdForTurnEnd: { rationale: "later" },
  };

  it("decodes the hold as its own arm", () => {
    // Arrange / Act
    const frame = decode({
      queue: { workspace: "ws", fence: "s1", entries: [{ ...ENTRY, buildRefresh: {} }] },
    });

    // Assert
    expect(frame.frame.case === "queue" && frame.frame.value.entries[0].buildRefreshHold).toEqual(
      {},
    );
  });

  it("leaves the hold ABSENT on an ordinary classifier-held entry", () => {
    // Arrange / Act
    const frame = decode({ queue: { workspace: "ws", fence: "s1", entries: [ENTRY] } });

    // Assert
    expect(
      frame.frame.case === "queue" && "buildRefreshHold" in frame.frame.value.entries[0],
    ).toBe(false);
  });

  it("refuses an entry that sets the build refresh beside another hold", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({
        queue: {
          workspace: "ws",
          fence: "s1",
          entries: [{ ...ENTRY, buildRefresh: {}, revival: {} }],
        },
      }),
    ).toThrow(/sets multiple holds/);
  });

  it("refuses a field on the bare build-refresh marker", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({
        queue: {
          workspace: "ws",
          fence: "s1",
          entries: [{ ...ENTRY, buildRefresh: { sessionId: "s1" } }],
        },
      }),
    ).toThrow(/QueueEntryBuildRefreshHold/);
  });
});
