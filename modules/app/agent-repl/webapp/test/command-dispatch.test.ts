/**
 * command-dispatch — the webapp's FrontendCommand plane: ack-correlated
 * commands and the SessionView-correlated createSession. One edge per test.
 */
import { describe, expect, it } from "vitest";
import { CommandDispatcher, type CreateSessionArgs } from "../src/command-dispatch.js";
import { decodeFrontendFrame, type FrontendFrame } from "../src/frontend-proto.js";

function newDispatcher(sendReturns = true) {
  const sent: string[] = [];
  const logs: Array<[string, string]> = [];
  let n = 0;
  const dispatcher = new CommandDispatcher({
    send: (raw) => {
      sent.push(raw);
      return sendReturns;
    },
    newRequestId: () => `r${++n}`,
    log: (level, message) => logs.push([level, message]),
  });
  return { dispatcher, sent, logs };
}

function ackFrame(requestId: string, ok: boolean, error = ""): FrontendFrame {
  return decodeFrontendFrame(JSON.stringify({ commandAck: { requestId, ok, ...(error !== "" ? { error } : {}) } }));
}

function sessionViewFrame(over: Record<string, unknown>): FrontendFrame {
  return decodeFrontendFrame(JSON.stringify({ sessionView: { sessionId: "s1", ...over } }));
}

const CREATE_ARGS: CreateSessionArgs = {
  cwd: "",
  model: "",
  permissionMode: "",
  configDir: "",
  resumeClaudeSessionId: "",
  fake: true,
};

describe("ack-correlated commands", () => {
  it("submitPrompt sends a FrontendCommand frame and resolves on an ok ack", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.submitPrompt("/w", "hi", "plan");
    expect(JSON.parse(sent[0])).toEqual({
      requestId: "r1",
      workspace: "/w",
      submitPrompt: { text: "hi", permissionMode: "plan" },
    });
    dispatcher.observe(ackFrame("r1", true));
    await expect(p).resolves.toBeUndefined();
  });

  it("interrupt rejects on an error ack, naming the command and reason", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.interrupt("/w", true);
    expect(JSON.parse(sent[0]).interrupt).toEqual({ hard: true });
    dispatcher.observe(ackFrame("r1", false, "no turn"));
    await expect(p).rejects.toThrow(/interrupt rejected: no turn/);
  });

  it("permissionAnswer carries the allow-with-edits Struct", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.permissionAnswer("/w", {
      permissionRequestId: "pr1",
      allow: true,
      updatedInput: { command: "ls" },
      denyMessage: "",
    });
    expect(JSON.parse(sent[0]).permissionAnswer).toEqual({
      permissionRequestId: "pr1",
      allow: true,
      denyMessage: "",
      updatedInput: { command: "ls" },
    });
    dispatcher.observe(ackFrame("r1", true));
    await expect(p).resolves.toBeUndefined();
  });

  it("resync renders the uint64 fromSeq as a string", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.resync("/w", 42);
    expect(JSON.parse(sent[0]).resync).toEqual({ fromSeq: "42" });
    dispatcher.observe(ackFrame("r1", true));
    await expect(p).resolves.toBeUndefined();
  });

  it("rejects when the socket refuses the frame", async () => {
    const { dispatcher } = newDispatcher(false);
    await expect(dispatcher.interrupt("/w")).rejects.toThrow(/socket not open/);
  });

  it("logs a commandAck for an unknown request rather than throwing", () => {
    const { dispatcher, logs } = newDispatcher();
    dispatcher.observe(ackFrame("ghost", true));
    expect(logs).toEqual([["warn", "commandAck for unknown request 'ghost'"]]);
  });
});

describe("createSession — SessionView correlation", () => {
  it("resolves with the id of a newly pushed non-terminal SessionView", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.createSession(CREATE_ARGS);
    expect(JSON.parse(sent[0]).createSession).toMatchObject({ fake: true });
    dispatcher.observe(sessionViewFrame({ sessionId: "s-new", workspace: "/w" }));
    await expect(p).resolves.toBe("s-new");
  });

  it("does not resolve on the bare success ack alone (it carries no id)", async () => {
    const { dispatcher } = newDispatcher();
    let resolved = false;
    void dispatcher.createSession(CREATE_ARGS).then(() => {
      resolved = true;
    });
    dispatcher.observe(ackFrame("r1", true));
    await Promise.resolve();
    expect(resolved).toBe(false);
  });

  it("rejects the create on an error ack", async () => {
    const { dispatcher } = newDispatcher();
    const p = dispatcher.createSession(CREATE_ARGS);
    dispatcher.observe(ackFrame("r1", false, "boom"));
    await expect(p).rejects.toThrow(/createSession rejected: boom/);
  });

  it("ignores a previously-known session and a terminal view", async () => {
    const { dispatcher } = newDispatcher();
    dispatcher.observe(sessionViewFrame({ sessionId: "s-old", workspace: "/w" }));
    let resolved: string | null = null;
    const p = dispatcher.createSession(CREATE_ARGS);
    void p.then((id) => {
      resolved = id;
    });
    dispatcher.observe(sessionViewFrame({ sessionId: "s-old", workspace: "/w" }));
    dispatcher.observe(sessionViewFrame({ sessionId: "s-dead", workspace: "/w", terminal: true }));
    await Promise.resolve();
    expect(resolved).toBeNull();
    dispatcher.observe(sessionViewFrame({ sessionId: "s-new", workspace: "/w" }));
    await expect(p).resolves.toBe("s-new");
  });

  it("with a cwd, ignores a new session for a different cwd", async () => {
    const { dispatcher } = newDispatcher();
    const p = dispatcher.createSession({ ...CREATE_ARGS, cwd: "/want" });
    let resolved: string | null = null;
    void p.then((id) => {
      resolved = id;
    });
    dispatcher.observe(sessionViewFrame({ sessionId: "s-other", workspace: "/other" }));
    await Promise.resolve();
    expect(resolved).toBeNull();
    dispatcher.observe(sessionViewFrame({ sessionId: "s-want", workspace: "/want" }));
    await expect(p).resolves.toBe("s-want");
  });

  it("rejects the create when the socket refuses the frame", async () => {
    const { dispatcher } = newDispatcher(false);
    await expect(dispatcher.createSession(CREATE_ARGS)).rejects.toThrow(/socket not open/);
  });
});

describe("clientLog (E4)", () => {
  it("sends a clientLog FrontendCommand frame", () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    dispatcher.clientLog("/w", "warn", "seq gap");
    // Assert
    expect(JSON.parse(sent[0])).toEqual({
      requestId: "r1",
      workspace: "/w",
      clientLog: { level: "CLIENT_LOG_LEVEL_WARN", message: "seq gap" },
    });
  });

  it("reports delivery from the socket, which is what the logger records", () => {
    // Arrange
    const { dispatcher } = newDispatcher();
    // Act / Assert
    expect(dispatcher.clientLog("/w", "info", "m")).toBe(true);
  });

  it("reports non-delivery on a closed socket", () => {
    // Arrange
    const { dispatcher } = newDispatcher(false);
    // Act / Assert
    expect(dispatcher.clientLog("/w", "info", "m")).toBe(false);
  });

  it("carries a structured context when one is supplied", () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    dispatcher.clientLog("/w", "info", "m", { seq: 7 });
    // Assert
    expect(JSON.parse(sent[0]).clientLog.context).toEqual({ seq: 7 });
  });

  it("does not report its own ack as an unknown request", () => {
    // Arrange — the ack MUST be recognized: reporting it would log, which
    // forwards another clientLog, which acks, which logs…
    const { dispatcher, logs } = newDispatcher();
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert
    expect(logs).toEqual([]);
  });

  it("does not route a rejected clientLog ack through the forwarding logger", () => {
    // Arrange — routing it there would re-send a clientLog and loop.
    const { dispatcher, logs } = newDispatcher();
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", false, "no message"));
    // Assert
    expect(logs).toEqual([]);
  });

  it("reports a rejected clientLog ack on the local-only sink", () => {
    // Arrange
    const local: string[] = [];
    const dispatcher = new CommandDispatcher({
      send: () => true,
      newRequestId: () => "r1",
      logLocal: (message) => local.push(message),
    });
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", false, "no message"));
    // Assert — surfaced somewhere, just not back down the forwarding path.
    expect(local).toEqual(["clientLog rejected: no message"]);
  });

  it("still reports a genuinely unknown ack as an anomaly", () => {
    // Arrange — the clientLog carve-out must not blanket-silence onAck.
    const { dispatcher, logs } = newDispatcher();
    // Act
    dispatcher.observe(ackFrame("never-sent", true));
    // Assert
    expect(logs[0][1]).toContain("unknown request");
  });

  it("tracks no ack id for a frame the socket refused", () => {
    // Arrange — an undelivered log can never be acked, so remembering its id
    // would leak it.
    const { dispatcher, logs } = newDispatcher(false);
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert — the id was never tracked, so this reads as an unknown ack.
    expect(logs[0][1]).toContain("unknown request");
  });
});
