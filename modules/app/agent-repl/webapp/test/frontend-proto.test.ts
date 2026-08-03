/**
 * frontend-proto — decode + loud validation of agentshim.frontend.v1 protojson
 * frames (S9 typed ConversationItem envelope + ContentDelta typing +
 * SessionInitView). One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import {
  UNSUPPORTED_SHAPES,
  decodeFrontendFrame,
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
const TYPING = { workspace: "ws", sessionId: "s1", delta: { uuid: "u1", blockIndex: 0, text: "hi" } };
const TASK_CATALOG = {
  workspace: "ws",
  sessionId: "s1",
  tasks: [{ taskId: "t1", kind: "agent", description: "d", status: "running" }],
};
const CONV_DELTA = {
  workspace: "ws",
  sessionId: "s1",
  throughSeq: "5",
  items: [{ uuid: "u1", tsMs: "1700000000000", assistantMessage: { content: [{ text: { text: "hi" } }] } }],
};
const SESSION_INIT = { workspace: "ws", sessionId: "s1", init: { model: "claude", cwd: "/w" } };
const COMMAND_ACK = { requestId: "r1", ok: true };
const DAEMON_VIEW = {
  bootId: "b_abc",
  protocolVersion: "1",
  daemonBinaryMtimeMs: "1700000000000",
  daemonVersion: "v9",
};
const HEARTBEAT = {
  workspace: "ws",
  sessionId: "s1",
  progress: { toolUseId: "tu1", toolName: "Bash", elapsedSeconds: 12.5 },
};
const QUEUE = {
  workspace: "ws",
  sessionId: "s1",
  entries: [
    {
      id: "q1",
      text: "run this later",
      queuedAtMs: "1700000000000",
      classification: "QUEUE_CLASSIFICATION_HOLD",
      rationale: "independent",
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
  apiMessageId: "msg-1", model: "claude-opus", mainAgent: {},
  usage: { inputTokens: "10", outputTokens: "20", cacheReadInputTokens: "30", cacheCreationInputTokens: "40", cacheCreation: { ephemeral5mInputTokens: "4", ephemeral1hInputTokens: "36" }, serviceTier: "priority", speed: "fast", inferenceGeo: "us", cacheRates: { totalPromptInputTokens: "80", cacheHitRate: 0.375, cacheWriteRate: 0.5, uncachedInputRate: 0.125 } },
  responseTiming: { timeToFirstTokenMs: "50", outputGenerationDurationMs: "100" },
};

describe("ConversationItem token utilization", () => {
  it("strictly decodes response timing and complete cache accounting", () => {
    const frame = decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [TOKEN_UTILIZATION] }] } });
    if (frame.frame.case !== "conversationDelta") throw new Error("wrong frame");
    expect(frame.frame.value.items[0].tokenUtilization[0]).toMatchObject({ model: "claude-opus", usage: { cacheCreation5m: 4, cacheCreation1h: 36, cacheHitRate: 0.375 }, responseTiming: { timeToFirstTokenMs: 50, outputGenerationDurationMs: 100 } });
  });

  it("rejects malformed response actor and unknown usage fields", () => {
    const item = { ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, subagent: {} }] };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [item] } })).toThrow(/exactly one actor/);
    const usage = { ...TOKEN_UTILIZATION.usage, unrecognized: 1 };
    expect(() => decode({ conversationDelta: { ...CONV_DELTA, items: [{ ...CONV_DELTA.items[0], tokenUtilization: [{ ...TOKEN_UTILIZATION, usage }] }] } })).toThrow(/unrecognized field/);
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
    return decode({ conversationDelta: { sessionId: "s1", items: [item] } });
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
    const frame = decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", blockIndex: 2, inputJson: '{"a":' } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("input_json");
    expect(frame.frame.value.blockIndex).toBe(2);
    expect(frame.frame.value.delta).toBe('{"a":');
  });

  it("decodes a thinking delta with its estimatedTokens int64 string", () => {
    const frame = decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", thinking: "hmm", estimatedTokens: "12" } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("thinking");
    expect(frame.frame.value.estimatedTokens).toBe(12);
  });

  it("decodes a signature delta", () => {
    const frame = decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", signature: "sig" } } });
    if (frame.frame.case !== "typingDelta") throw new Error("wrong variant");
    expect(frame.frame.value.kind).toBe("signature");
  });

  it("rejects a TypingDelta with no delta", () => {
    expect(() => decode({ typingDelta: { sessionId: "s1" } })).toThrow(/TypingDelta missing required `delta`/);
  });

  it("rejects a ContentDelta with no content arm (empty oneof)", () => {
    expect(() => decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", blockIndex: 0 } } })).toThrow(
      /carries no content delta/,
    );
  });

  it("rejects a ContentDelta that sets two content arms", () => {
    expect(() =>
      decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", text: "a", thinking: "b" } } }),
    ).toThrow(/sets multiple content deltas/);
  });

  it("rejects a ContentDelta with an unrecognized field", () => {
    expect(() =>
      decode({ typingDelta: { sessionId: "s1", delta: { uuid: "u1", text: "a", bogus: 1 } } }),
    ).toThrow(/unrecognized field/);
  });

  it("rejects a ContentDelta without a uuid", () => {
    expect(() => decode({ typingDelta: { sessionId: "s1", delta: { text: "a" } } })).toThrow(
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
    const frame = decode({ sessionInit: { sessionId: "s1" } });
    if (frame.frame.case !== "sessionInit") throw new Error("wrong variant");
    expect(frame.frame.value.init).toEqual({});
  });

  it("rejects an unrecognized SessionInitView field loudly", () => {
    expect(() => decode({ sessionInit: { ...SESSION_INIT, bogus: 1 } })).toThrow(/unrecognized field/);
  });

  it("rejects a SessionInitView without a session id", () => {
    expect(() => decode({ sessionInit: { workspace: "ws", init: {} } })).toThrow(
      /SessionInitView missing required `session_id`/,
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

  it("rejects a ConversationDelta without a session id", () => {
    expect(() => decode({ conversationDelta: { workspace: "ws", items: [] } })).toThrow(
      /ConversationDelta missing required `session_id`/,
    );
  });

  it("rejects a TaskEntry with an unknown kind", () => {
    expect(() =>
      decode({ taskCatalog: { sessionId: "s1", tasks: [{ taskId: "t1", kind: "wat", status: "running" }] } }),
    ).toThrow(/unknown kind 'wat'/);
  });

  it("rejects a TaskEntry with an unknown status", () => {
    expect(() =>
      decode({ taskCatalog: { sessionId: "s1", tasks: [{ taskId: "t1", kind: "agent", status: "wat" }] } }),
    ).toThrow(/unknown status 'wat'/);
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
      sessionId: "s1",
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
    expect(() => decode({ heartbeat: { workspace: "ws", sessionId: "s1" } })).toThrow(
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
    const frame = decode({ queue: { workspace: "ws", sessionId: "s1" } });
    // Assert
    if (frame.frame.case !== "queue") throw new Error("wrong variant");
    expect(frame.frame.value.entries).toEqual([]);
  });

  it("rejects a missing classification rather than defaulting it to pending", () => {
    // Arrange / Act / Assert — protojson omits an enum at its zero value, and
    // the zero is now UNSPECIFIED, which the daemon never sends. Reading it as
    // `pending` would invent the very claim the wire declined to make.
    expect(() =>
      decode({ queue: { sessionId: "s1", entries: [{ id: "q1", text: "x" }] } }),
    ).toThrow(/no classification/);
  });

  it("rejects an explicit UNSPECIFIED classification", () => {
    // Arrange / Act / Assert — the spelled-out zero is the same wire fact as
    // an absent field and gets the same loud rejection.
    expect(() =>
      decode({
        queue: {
          sessionId: "s1",
          entries: [{ id: "q1", classification: "QUEUE_CLASSIFICATION_UNSPECIFIED" }],
        },
      }),
    ).toThrow(/UNSPECIFIED/);
  });

  it("decodes each real classification the daemon sends", () => {
    // Arrange
    const cases: Array<[string, string]> = [
      ["QUEUE_CLASSIFICATION_PENDING", "pending"],
      ["QUEUE_CLASSIFICATION_INTERJECT", "interject"],
      ["QUEUE_CLASSIFICATION_HOLD", "hold"],
      ["QUEUE_CLASSIFICATION_ERROR", "error"],
    ];
    for (const [wire, want] of cases) {
      // Act
      const frame = decode({ queue: { sessionId: "s1", entries: [{ id: "q1", classification: wire }] } });
      // Assert
      if (frame.frame.case !== "queue") throw new Error("wrong variant");
      expect(frame.frame.value.entries[0].classification).toBe(want);
    }
  });

  it("rejects an unrecognized classification rather than defaulting it", () => {
    // Arrange / Act / Assert — rendering an unknown verdict as `pending` would
    // tell the user their prompt is being judged when it is not.
    expect(() =>
      decode({
        queue: { sessionId: "s1", entries: [{ id: "q1", classification: "QUEUE_CLASSIFICATION_XX" }] },
      }),
    ).toThrow(/unrecognized classification/);
  });

  it("rejects an entry with no id, whose controls would all be dead", () => {
    // Arrange / Act / Assert — a real classification, so the missing id is
    // what fails rather than the classification check upstream of it.
    expect(() =>
      decode({
        queue: {
          sessionId: "s1",
          entries: [{ text: "x", classification: "QUEUE_CLASSIFICATION_PENDING" }],
        },
      }),
    ).toThrow(/missing required `id`/);
  });

  it("rejects an unrecognized field on an entry", () => {
    // Arrange / Act / Assert
    expect(() =>
      decode({ queue: { sessionId: "s1", entries: [{ id: "q1", bogus: 1 }] } }),
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

  it("decodes a terminal push's classified death instead of throwing", () => {
    // Arrange — the strict decoder once lacked this key, so every terminal
    // SessionView (delete, supersede, shim death) threw in the live frame
    // path; this pins the fix.
    const death = {
      errorClass: "ERROR_CLASS_INTERNAL",
      errorType: "internal.shim_died",
      message: "shim process exited",
    };
    // Act
    const got = deathOf(sv({ death }));
    // Assert
    expect(got).toEqual(
      expect.objectContaining({ errorClass: "INTERNAL", errorType: "internal.shim_died" }),
    );
  });

  it("rejects a death with an unrecognized class rather than guessing", () => {
    // Arrange / Act / Assert — the class decides the card color; guessing one
    // would paint the failure the wrong color quietly.
    expect(() =>
      deathOf(sv({ death: { errorClass: "ERROR_CLASS_MYSTERY", errorType: "x", message: "y" } })),
    ).toThrow(/unrecognized value/);
  });
});

describe("ProgressView decoding (F1)", () => {
  /** A minimal well-formed ProgressView frame body. */
  function pv(over: Record<string, unknown> = {}): string {
    return JSON.stringify({
      progress: { workspace: "/w", sessionId: "s1", state: "RENDER_STATE_THINKING", ...over },
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
      decodeFrontendFrame(JSON.stringify({ progress: { sessionId: "s1", state: "RENDER_STATE_IDLE" } })),
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

  it("decodes a snapshot's progress list", () => {
    // Arrange / Act
    const got = decodeFrontendFrame(
      JSON.stringify({
        snapshot: { progress: [{ workspace: "/w", sessionId: "s1", state: "RENDER_STATE_IDLE" }] },
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
        sessionId: "s1",
        entries: [
          {
            id: "q1",
            text: "later",
            classification: "QUEUE_CLASSIFICATION_PENDING",
            shutdownHold: hold,
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
