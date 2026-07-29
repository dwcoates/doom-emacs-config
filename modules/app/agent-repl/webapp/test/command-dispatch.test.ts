/**
 * command-dispatch — the webapp's FrontendCommand plane: ack-correlated
 * commands and the SessionView-correlated createSession. One edge per test.
 */
import { afterEach, describe, expect, it } from "vitest";
import {
  CommandDispatcher,
  InterruptConfirmRequiredError,
  type CreateSessionArgs,
} from "../src/command-dispatch.js";
import { decodeFrontendFrame, type FrontendFrame } from "../src/frontend-proto.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";

function installLogging(): Array<Record<string, unknown>> {
  const records: Array<Record<string, unknown>> = [];
  setLogger(new ForwardingLogger((cmd) => {
    records.push(cmd.context as Record<string, unknown>);
    return true;
  }, () => {}));
  return records;
}

afterEach(() => resetLoggingForTests());

function newDispatcher(sendReturns = true) {
  const sent: string[] = [];
  const records = installLogging();
  let n = 0;
  const dispatcher = new CommandDispatcher({
    send: (raw) => {
      sent.push(raw);
      return sendReturns;
    },
    newRequestId: () => `r${++n}`,
    logLocal: (message) => records.push({ local_only: message }),
  });
  return { dispatcher, sent, records };
}

function ackFrame(requestId: string, ok: boolean, error = ""): FrontendFrame {
  return decodeFrontendFrame(JSON.stringify({ commandAck: { requestId, ok, ...(error !== "" ? { error } : {}) } }));
}

/** The interrupt confirmation CHALLENGE: ok=false, no failure, live tasks. */
function challengeFrame(requestId: string, liveTasks: number): FrontendFrame {
  return decodeFrontendFrame(
    JSON.stringify({
      commandAck: { requestId, ok: false, interruptConfirmRequired: { liveTasks: String(liveTasks) } },
    }),
  );
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
    const { dispatcher, sent, records } = newDispatcher();
    const p = dispatcher.interrupt("/w", true);
    expect(JSON.parse(sent[0]).interrupt).toEqual({ confirmAgents: true });
    dispatcher.observe(ackFrame("r1", false, "no turn"));
    await expect(p).rejects.toThrow(/interrupt rejected: no turn/);
    expect(records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.ack-rejected",
      request_id: "r1",
      context: expect.objectContaining({ command: "interrupt", error: "no turn", has_classified_failure: false }),
    }));
  });

  it("interrupt rejects a confirmation challenge as its own typed error", async () => {
    // Arrange — the daemon understood the command and deliberately did not
    // perform it; a generic "rejected" string could not be told from a real
    // refusal without parsing prose.
    const { dispatcher } = newDispatcher();
    const p = dispatcher.interrupt("/w");
    // Act
    dispatcher.observe(challengeFrame("r1", 3));
    // Assert
    await expect(p).rejects.toBeInstanceOf(InterruptConfirmRequiredError);
  });

  it("carries the challenge's live task count to whoever asks the question", async () => {
    // Arrange
    const { dispatcher } = newDispatcher();
    const p = dispatcher.interrupt("/w");
    // Act
    dispatcher.observe(challengeFrame("r1", 3));
    // Assert — enough to ask "interrupt 3 running subagents?" concretely.
    await expect(p).rejects.toMatchObject({ liveTasks: 3 });
  });

  it("keeps a genuinely failed ack a plain error, not a challenge", async () => {
    // Arrange — the challenge arm must not weaken the refusal path.
    const { dispatcher } = newDispatcher();
    const p = dispatcher.interrupt("/w");
    // Act
    dispatcher.observe(ackFrame("r1", false, "no session"));
    // Assert
    await expect(p).rejects.not.toBeInstanceOf(InterruptConfirmRequiredError);
  });

  it("does not route a challenge to the classified-failure sink", async () => {
    // Arrange — a challenge is a question, and a failure card would answer it
    // with an alarm the user never earned.
    const failures: unknown[] = [];
    const records = installLogging();
    const dispatcher = new CommandDispatcher({
      send: () => true,
      newRequestId: () => "r1",
      logLocal: (message) => records.push({ local_only: message }),
      onFailure: (f) => failures.push(f),
    });
    const p = dispatcher.interrupt("/w");
    // Act
    dispatcher.observe(challengeFrame("r1", 2));
    await p.catch(() => {});
    // Assert
    expect(failures).toEqual([]);
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
    const { dispatcher, records } = newDispatcher(false);
    await expect(dispatcher.interrupt("/w")).rejects.toThrow(/socket not open/);
    expect(records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.dispatch-rejected",
      context: expect.objectContaining({ command: "interrupt", workspace: "/w", cause: "socket not open" }),
    }));
  });

  it("logs structured context for a commandAck for an unknown request", () => {
    const { dispatcher, records } = newDispatcher();
    dispatcher.observe(ackFrame("ghost", true));
    expect(records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.ack-unknown-request",
      request_id: "ghost",
      context: expect.objectContaining({ ok: true, pending_count: 0 }),
    }));
  });
});

describe("createSession — SessionView correlation", () => {
  it("logs snapshot selection without mutating a create waiter", () => {
    const { dispatcher, records } = newDispatcher();

    dispatcher.observe(decodeFrontendFrame(JSON.stringify({ snapshot: { sessions: [] } })));

    expect(records).toContainEqual(expect.objectContaining({ operation: "command-dispatch.observe-snapshot" }));
  });

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

  it("logs the bounded client-log acknowledgement eviction", () => {
    const { dispatcher, records } = newDispatcher();
    for (let i = 0; i < 257; i++) dispatcher.clientLog("/w", "info", `line ${i}`);

    expect(dispatcher.trackedClientLogCount()).toBe(256);
    expect(records).toContainEqual(expect.objectContaining({
      local_only: expect.stringContaining("evicted request r1"),
    }));
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
    const { dispatcher, records } = newDispatcher();
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert
    expect(records.some((record) => record.operation === "command-dispatch.ack-unknown-request")).toBe(false);
  });

  it("does not route a rejected clientLog ack through the forwarding logger", () => {
    // Arrange — routing it there would re-send a clientLog and loop.
    const { dispatcher, records } = newDispatcher();
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", false, "no message"));
    // Assert
    expect(records.some((record) => record.operation === "command-dispatch.ack-unknown-request")).toBe(false);
  });

  it("reports a rejected clientLog ack on the local-only sink", () => {
    // Arrange
    const local: string[] = [];
    installLogging();
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
    const { dispatcher, records } = newDispatcher();
    // Act
    dispatcher.observe(ackFrame("never-sent", true));
    // Assert
    expect(records).toContainEqual(expect.objectContaining({ operation: "command-dispatch.ack-unknown-request" }));
  });

  it("tracks no ack id for a frame the socket refused", () => {
    // Arrange — an undelivered log can never be acked, so remembering its id
    // would leak it.
    const { dispatcher, records } = newDispatcher(false);
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert — the id was never tracked, so this reads as an unknown ack.
    expect(records).toContainEqual(expect.objectContaining({ operation: "command-dispatch.ack-unknown-request" }));
  });
});

describe("queue controls (E4)", () => {
  it("sends a queueForce frame carrying the entry id", () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    void dispatcher.queueForce("/w", "q1");
    // Assert
    expect(JSON.parse(sent[0])).toEqual({
      requestId: "r1",
      workspace: "/w",
      queueForce: { entryId: "q1" },
    });
  });

  it("resolves a queueCancel on an ok ack", async () => {
    // Arrange
    const { dispatcher } = newDispatcher();
    const p = dispatcher.queueCancel("/w", "q1");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert
    await expect(p).resolves.toBeUndefined();
  });

  it("rejects a queueForce whose entry is already gone", async () => {
    // Arrange — unlike a clientLog, a queue control is an OPERATION the user
    // asked for, so its rejection must reach the caller.
    const { dispatcher } = newDispatcher();
    const p = dispatcher.queueForce("/w", "q1");
    // Act
    dispatcher.observe(ackFrame("r1", false, "no queued prompt \"q1\""));
    // Assert
    await expect(p).rejects.toThrow(/no queued prompt/);
  });

  it("rejects a queueAccept when the socket is closed", async () => {
    // Arrange
    const { dispatcher } = newDispatcher(false);
    // Act / Assert
    await expect(dispatcher.queueAccept("/w", "q1")).rejects.toThrow(/socket not open/);
  });
});
