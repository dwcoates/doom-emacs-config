/**
 * frontend-command — encode agentshim.frontend.v1.FrontendCommand frames as
 * canonical protojson. One edge per test (AAA).
 */
import { describe, expect, it } from "vitest";
import { encodeFrontendCommand, type FrontendCommand } from "../src/frontend-command.js";

/** Encode and parse back, so assertions read the wire object, not a string. */
function wire(cmd: FrontendCommand): Record<string, unknown> {
  return JSON.parse(encodeFrontendCommand(cmd)) as Record<string, unknown>;
}

describe("encodeFrontendCommand — envelope", () => {
  it("carries requestId and workspace on every frame", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws-a",
      body: { case: "interrupt", hard: true },
    });
    expect(w.requestId).toBe("r1");
    expect(w.workspace).toBe("ws-a");
  });

  it("nests the command under its lowerCamelCase oneof arm key", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "submitPrompt", text: "hi", permissionMode: "" },
    });
    expect(Object.keys(w).sort()).toEqual(["requestId", "submitPrompt", "workspace"]);
  });
});

describe("encodeFrontendCommand — submitPrompt", () => {
  it("encodes text and permissionMode", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "submitPrompt", text: "run the tests", permissionMode: "plan" },
    });
    expect(w.submitPrompt).toEqual({ text: "run the tests", permissionMode: "plan" });
  });
});

describe("encodeFrontendCommand — interrupt", () => {
  it("encodes the hard flag", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "interrupt", hard: false } });
    expect(w.interrupt).toEqual({ hard: false });
  });
});

describe("encodeFrontendCommand — permissionAnswer", () => {
  it("encodes an allow with no updatedInput (Struct omitted)", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "permissionAnswer", permissionRequestId: "pr1", allow: true, denyMessage: "" },
    });
    expect(w.permissionAnswer).toEqual({ permissionRequestId: "pr1", allow: true, denyMessage: "" });
  });

  it("encodes an allow-with-edits Struct as a plain JSON object", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: {
        case: "permissionAnswer",
        permissionRequestId: "pr1",
        allow: true,
        updatedInput: { command: "ls -la" },
        denyMessage: "",
      },
    });
    expect(w.permissionAnswer).toEqual({
      permissionRequestId: "pr1",
      allow: true,
      denyMessage: "",
      updatedInput: { command: "ls -la" },
    });
  });

  it("encodes a denial with its denyMessage", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "permissionAnswer", permissionRequestId: "pr1", allow: false, denyMessage: "no" },
    });
    expect(w.permissionAnswer).toEqual({ permissionRequestId: "pr1", allow: false, denyMessage: "no" });
  });
});

describe("encodeFrontendCommand — createSession", () => {
  it("encodes every session-creation field", () => {
    const w = wire({
      requestId: "r1",
      workspace: "",
      body: {
        case: "createSession",
        cwd: "/work/ws",
        model: "opus",
        permissionMode: "default",
        configDir: "/home/u/.claude",
        resumeClaudeSessionId: "cli-uuid",
        fake: true,
      },
    });
    expect(w.createSession).toEqual({
      cwd: "/work/ws",
      model: "opus",
      permissionMode: "default",
      configDir: "/home/u/.claude",
      resumeClaudeSessionId: "cli-uuid",
      fake: true,
    });
  });

  it("never encodes the ungated-session consent the daemon gates on", () => {
    // Arrange / Act — the daemon refuses a bypassPermissions create without
    // allow_ungated, and the webapp has no business consenting from a browser
    // tab, so the field is unrepresentable here rather than merely unset.
    const w = wire({
      requestId: "r1",
      workspace: "",
      body: {
        case: "createSession",
        cwd: "/work/ws",
        model: "opus",
        permissionMode: "bypassPermissions",
        configDir: "",
        resumeClaudeSessionId: "",
        fake: false,
      },
    });
    // Assert
    expect(w.createSession).not.toHaveProperty("allowUngated");
  });
});

describe("encodeFrontendCommand — deleteSession", () => {
  it("encodes the session id", () => {
    const w = wire({ requestId: "r1", workspace: "", body: { case: "deleteSession", sessionId: "s9" } });
    expect(w.deleteSession).toEqual({ sessionId: "s9" });
  });
});

describe("encodeFrontendCommand — resync", () => {
  it("renders the uint64 fromSeq as a JSON string", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "resync", fromSeq: 42 } });
    expect(w.resync).toEqual({ fromSeq: "42" });
  });
});

describe("encodeFrontendCommand — clientLog (E4)", () => {
  it("renders the level as its canonical protojson enum name", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "warn", message: "seq gap" },
    });
    expect(w.clientLog).toEqual({ level: "CLIENT_LOG_LEVEL_WARN", message: "seq gap" });
  });

  it("renders the error level", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "error", message: "boom" },
    });
    expect((w.clientLog as Record<string, unknown>).level).toBe("CLIENT_LOG_LEVEL_ERROR");
  });

  it("carries a structured context as a plain JSON object", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "info", message: "m", context: { seq: 42 } },
    });
    expect((w.clientLog as Record<string, unknown>).context).toEqual({ seq: 42 });
  });

  it("omits an absent context rather than fabricating an empty Struct", () => {
    const w = wire({
      requestId: "r1",
      workspace: "ws",
      body: { case: "clientLog", level: "info", message: "m" },
    });
    expect("context" in (w.clientLog as Record<string, unknown>)).toBe(false);
  });
});

describe("encodeFrontendCommand — queue controls (E4)", () => {
  it("encodes a force by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueForce", entryId: "q1" } });
    expect(w.queueForce).toEqual({ entryId: "q1" });
  });

  it("encodes an accept by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueAccept", entryId: "q1" } });
    expect(w.queueAccept).toEqual({ entryId: "q1" });
  });

  it("encodes a cancel by entry id", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "queueCancel", entryId: "q1" } });
    expect(w.queueCancel).toEqual({ entryId: "q1" });
  });
});

describe("encodeFrontendCommand — paintAck", () => {
  it("encodes the painted seq as a protojson uint64 string", () => {
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "paintAck", throughSeq: 42 } });
    expect(w.paintAck).toEqual({ throughSeq: "42" });
  });

  it("encodes an empty-history attestation at seq 0", () => {
    // Seq 0 is a REAL attestation — "there was nothing to paint and I
    // painted it" — which is what lets a never-prompted session read ready.
    const w = wire({ requestId: "r1", workspace: "ws", body: { case: "paintAck", throughSeq: 0 } });
    expect(w.paintAck).toEqual({ throughSeq: "0" });
  });

  it("carries the workspace the attestation is about", () => {
    const w = wire({ requestId: "r1", workspace: "ws-7", body: { case: "paintAck", throughSeq: 3 } });
    expect(w.workspace).toBe("ws-7");
  });
});
