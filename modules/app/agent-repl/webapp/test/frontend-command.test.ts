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
