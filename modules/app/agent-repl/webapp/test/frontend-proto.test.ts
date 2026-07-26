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
const WS_STATE = { workspace: "ws", sessionId: "s1", state: "RENDER_STATE_IDLE" };
const SESSION_VIEW = {
  workspace: "ws",
  sessionId: "s1",
  model: "claude",
  totalTokens: "1200",
  totalCostUsd: 0.5,
  contextWindow: "200000",
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
  ];
  for (const [name, obj] of cases) {
    it(`decodes ${name}`, () => {
      const frame = decode(obj);
      expect(frame.frame.case).toBe(name);
    });
  }
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
    expect(() => decode({ sessionView: { workspace: "ws", model: "claude" } })).toThrow(
      /SessionView missing required `session_id`/,
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
  it("lists commandAck and daemonView as the unsupported frontend.v1 frames", () => {
    expect([...UNSUPPORTED_SHAPES.keys()]).toEqual(["commandAck", "daemonView"]);
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
    return JSON.stringify({ sessionView: { sessionId: "s1", workspace: "/w", ...over } });
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
    return JSON.stringify({ sessionView: { sessionId: "s1", workspace: "/w", ...over } });
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
