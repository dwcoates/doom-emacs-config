/**
 * frontend-proto — decode + loud validation of agentshim.frontend.v1
 * protojson frames. One edge per test (AAA).
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
const TYPING = { workspace: "ws", sessionId: "s1", uuid: "u1", blockIndex: 0, kind: "text", delta: "hi" };
const TASK_CATALOG = {
  workspace: "ws",
  sessionId: "s1",
  tasks: [{ taskId: "t1", kind: "agent", description: "d", status: "running" }],
};
const CONV_DELTA = {
  workspace: "ws",
  sessionId: "s1",
  throughSeq: "5",
  items: [{ kind: "system", subtype: "init" }],
};
const COMMAND_ACK = { requestId: "r1", ok: true };
const DEGRADED = { component: "shim-store", reason: "down" };
const DAEMON_VIEW = {
  bootId: "b_abc",
  protocolVersion: "1",
  daemonBinaryMtimeMs: "1700000000000",
  daemonVersion: "v9",
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
    ["degradedNotice", { degradedNotice: DEGRADED }],
    ["daemonView", { daemonView: DAEMON_VIEW }],
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

describe("decodeFrontendFrame — SessionView resume keys", () => {
  it("decodes claudeSessionId + cwd (protojson camelCase)", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, claudeSessionId: "cli-uuid", cwd: "/work" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.claudeSessionId).toBe("cli-uuid");
    expect(frame.frame.value.cwd).toBe("/work");
  });

  it("defaults the resume keys to empty strings when absent (optional)", () => {
    const frame = decode({ sessionView: SESSION_VIEW });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.claudeSessionId).toBe("");
    expect(frame.frame.value.cwd).toBe("");
  });
});

describe("decodeFrontendFrame — SessionView S7 parity fields", () => {
  it("decodes terminal + deathReason", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, terminal: true, deathReason: "delete session" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.terminal).toBe(true);
    expect(frame.frame.value.deathReason).toBe("delete session");
  });

  it("decodes pendingPermissions from its protojson int64 string", () => {
    const frame = decode({ sessionView: { ...SESSION_VIEW, pendingPermissions: "2" } });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.pendingPermissions).toBe(2);
  });

  it("defaults the parity fields when absent (pre-S7 daemon)", () => {
    const frame = decode({ sessionView: SESSION_VIEW });
    if (frame.frame.case !== "sessionView") throw new Error("wrong variant");
    expect(frame.frame.value.terminal).toBe(false);
    expect(frame.frame.value.deathReason).toBe("");
    expect(frame.frame.value.rehydratable).toBe(false);
    expect(frame.frame.value.hibernated).toBe(false);
    expect(frame.frame.value.pendingPermissions).toBe(0);
  });
});

describe("decodeFrontendFrame — DaemonView", () => {
  it("decodes the daemonView frame fields (mtime int64 string coerced)", () => {
    const frame = decode({ daemonView: DAEMON_VIEW });
    if (frame.frame.case !== "daemonView") throw new Error("wrong variant");
    expect(frame.frame.value.bootId).toBe("b_abc");
    expect(frame.frame.value.protocolVersion).toBe("1");
    expect(frame.frame.value.daemonBinaryMtimeMs).toBe(1700000000000);
    expect(frame.frame.value.daemonVersion).toBe("v9");
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

  it("rejects a ConversationDelta whose item lacks a kind", () => {
    expect(() =>
      decode({ conversationDelta: { sessionId: "s1", items: [{ text: "x" }] } }),
    ).toThrow(/missing string `kind`/);
  });

  it("rejects a TypingDelta with an unknown kind", () => {
    expect(() => decode({ typingDelta: { ...TYPING, kind: "colour" } })).toThrow(/unknown kind 'colour'/);
  });

  it("rejects a TypingDelta without a uuid", () => {
    expect(() => decode({ typingDelta: { sessionId: "s1", kind: "text", delta: "x" } })).toThrow(
      /TypingDelta missing required `uuid`/,
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

  it("rejects a DegradedNotice without a component", () => {
    expect(() => decode({ degradedNotice: { reason: "down" } })).toThrow(
      /DegradedNotice missing required `component`/,
    );
  });

  it("validates nested snapshot members", () => {
    expect(() =>
      decode({ snapshot: { workspaces: [{ sessionId: "s1", state: "RENDER_STATE_IDLE" }] } }),
    ).toThrow(/missing required `workspace`/);
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

  it("reports a mapped variant as visually supported", () => {
    expect(isVisuallySupportedFrame("workspaceState")).toBe(true);
  });
});
