/**
 * command-dispatch — the webapp's FrontendCommand plane: ack-correlated
 * commands and the SessionView-correlated createSession. One edge per test.
 */
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  CommandDispatcher,
  InterruptConfirmRequiredError,
  ModelSelectionRejectedError,
  type CreateSessionArgs,
  commandRefusal,
  surfaceRefusal,
  type CommandRefusal,
} from "../src/command-dispatch.js";
import { create } from "@bufbuild/protobuf";
import { FailureKindSchema } from "../../proto/gen/ts/agentshim/frontend/v1/errors_pb";
import { failureKindName } from "../src/failure-card.js";
import type { FailureCardItem } from "../src/store.js";
import {
  decodeFrontendFrame,
  type FrontendFrame,
} from "../src/frontend-proto.js";
import { ForwardingLogger, resetLoggingForTests, setLogger } from "../src/wslog.js";
import { PromptOrigin } from "../src/frontend-command.js";

function installLogging(): {
  records: Array<Record<string, unknown>>;
  consoleRecords: Array<Record<string, unknown>>;
} {
  const records: Array<Record<string, unknown>> = [];
  const consoleRecords: Array<Record<string, unknown>> = [];
  setLogger(new ForwardingLogger((cmd) => {
    records.push(cmd.context as Record<string, unknown>);
    return true;
  }, (_level, line) => {
    consoleRecords.push(JSON.parse(line) as Record<string, unknown>);
  }));
  return { records, consoleRecords };
}

afterEach(() => resetLoggingForTests());

function newDispatcher(sendReturns = true) {
  const sent: string[] = [];
  const { records, consoleRecords } = installLogging();
  let n = 0;
  const dispatcher = new CommandDispatcher({
    send: (raw) => {
      sent.push(raw);
      return sendReturns;
    },
    newRequestId: () => `r${++n}`,
    logLocal: (message) => records.push({ local_only: message }),
  });
  return { dispatcher, sent, records, consoleRecords };
}

/** A dispatcher whose classified-refusal sink is captured, ids `r1`, `r2`, … */
function newFailureDispatcher(sendReturns = true) {
  const failures: CommandRefusal[] = [];
  const { records } = installLogging();
  let n = 0;
  const dispatcher = new CommandDispatcher({
    send: () => sendReturns,
    newRequestId: () => `r${++n}`,
    logLocal: (message) => records.push({ local_only: message }),
    onFailure: (f) => failures.push(f),
  });
  return { dispatcher, failures };
}

/**
 * The card arm of a refusal.
 *
 * Asserting through it rather than casting keeps a test that expected a card
 * and got a REVEAL failing loudly: the two are genuinely different dispositions
 * and a test that silently accepted either would assert nothing.
 */
function cardOf(refusal: CommandRefusal | undefined): FailureCardItem {
  if (refusal === undefined || refusal.kind !== "card") {
    throw new Error(`expected a card refusal, got ${JSON.stringify(refusal)}`);
  }
  return refusal.card;
}

function ackFrame(requestId: string, ok: boolean, error = "", selectedModel = ""): FrontendFrame {
  return decodeFrontendFrame(JSON.stringify({ commandAck: {
    requestId, ok, ...(error !== "" ? { error } : {}), ...(selectedModel !== "" ? { selectedModel } : {}),
  } }));
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
  return decodeFrontendFrame(JSON.stringify({ sessionView: { sessionId: "s1", modelOptions: [], ...over } }));
}

const CREATE_ARGS: CreateSessionArgs = {
  cwd: "",
  permissionMode: "",
  configDir: "",
  resumeMode: "RESUME_MODE_CONTINUE",
  fake: true,
};

describe("ack-correlated commands", () => {
  it("submitPrompt sends a FrontendCommand frame and resolves on an ok ack", async () => {
    const { dispatcher, sent } = newDispatcher();
    const { ack } = dispatcher.submitPrompt("/w", "hi", PromptOrigin.WEBAPP_USER_SENT, "plan");
    expect(JSON.parse(sent[0])).toEqual({
      requestId: "r1",
      workspace: "/w",
      submitPrompt: { text: "hi", permissionMode: "plan", promptOrigin: "PROMPT_ORIGIN_WEBAPP_USER_SENT" },
    });
    dispatcher.observe(ackFrame("r1", true));
    await expect(ack).resolves.toBeUndefined();
  });

  // The id is what the local prompt bubble is filed under, and the daemon's
  // receipt comes back carrying it — so a submit that reported a DIFFERENT id
  // than the frame it sent would file a bubble nothing could ever supersede.
  it("submitPrompt reports the request id its frame went out under", () => {
    const { dispatcher, sent } = newDispatcher();
    const { requestId } = dispatcher.submitPrompt("/w", "hi", PromptOrigin.WEBAPP_USER_SENT);
    expect(requestId).toBe(JSON.parse(sent[0]).requestId);
  });

  it("submitPrompt reports no request id when the origin is unstated", async () => {
    const { dispatcher, sent } = newDispatcher();
    const { requestId, ack } = dispatcher.submitPrompt("/w", "hi", "" as PromptOrigin);
    expect(requestId).toBe("");
    expect(sent).toEqual([]);
    await expect(ack).rejects.toThrow(/explicit prompt origin/);
  });

  it("rejects a model switch with the shim-confirmed selected model", async () => {
    const { dispatcher } = newDispatcher();
    const p = dispatcher.setModel("/w", "opus");
    dispatcher.observe(ackFrame("r1", false, "model unavailable", "sonnet"));
    await expect(p).rejects.toBeInstanceOf(ModelSelectionRejectedError);
    await expect(p).rejects.toMatchObject({ selectedModel: "sonnet" });
  });

  it("rejects a SetModel receipt that omits its authoritative selection", async () => {
    const { dispatcher } = newDispatcher();
    const p = dispatcher.setModel("/w", "opus");
    dispatcher.observe(ackFrame("r1", true));
    await expect(p).rejects.toThrow(/selectedModel is absent/);
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
    const { records } = installLogging();
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

  it("resync renders the snapshot fence and uint64 fromSeq", async () => {
    const { dispatcher, sent, records } = newDispatcher();
    const p = dispatcher.resync("/w", { fromSeq: 42, fence: "f7" });
    expect(JSON.parse(sent[0]).resync).toEqual({ fromSeq: "42", fence: "f7" });
    expect(records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.resync",
      context: expect.objectContaining({
        workspace: "/w",
        from_seq: 42,
        fence: "f7",
        decision: "dispatch",
      }),
    }));
    dispatcher.observe(ackFrame("r1", true));
    await expect(p).resolves.toBeUndefined();
  });

  it("sends a model change only as an explicit request", async () => {
    const { dispatcher, sent } = newDispatcher();
    const p = dispatcher.setModel("/w", "opus");
    expect(JSON.parse(sent[0])).toEqual({ requestId: "r1", workspace: "/w", setModel: { model: "opus" } });
    dispatcher.observe(ackFrame("r1", true, "", "opus"));
    await expect(p).resolves.toBe("opus");
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
    const { dispatcher, consoleRecords } = newDispatcher();
    dispatcher.observe(ackFrame("ghost", true));
    expect(consoleRecords).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.ack-unknown-request",
      request_id: "ghost",
      context: expect.objectContaining({ ok: true, pending_count: 0 }),
    }));
  });

  it("never forwards the unknown-ack anomaly to the daemon", () => {
    // Arrange — a forwarded unknown-ack warn is itself a clientLog whose ack
    // can come back unknown, which is the self-sustaining command flood.
    const { dispatcher, records } = newDispatcher();
    // Act
    dispatcher.observe(ackFrame("ghost", true));
    // Assert
    expect(records).not.toContainEqual(
      expect.objectContaining({ operation: "command-dispatch.ack-unknown-request" }),
    );
  });
});

describe("minted request ids", () => {
  it("refuses to construct at all when crypto.getRandomValues is unavailable", () => {
    // The pre-entropy implementation could have fallen back to Math.random;
    // ids are durable turn-claim ledger keys now, so a weak-id fallback must
    // be impossible — the loud constructor throw is that guarantee, and this
    // test is what keeps a refactor from quietly reintroducing the fallback.
    installLogging();
    vi.stubGlobal("crypto", undefined);
    try {
      expect(() => new CommandDispatcher({ send: () => true, logLocal: () => {} })).toThrowError(
        /crypto\.getRandomValues is unavailable/,
      );
    } finally {
      vi.unstubAllGlobals();
    }
  });


  /** A dispatcher with the real id minter, plus the ids it puts on the wire. */
  function newMintingDispatcher() {
    installLogging();
    const ids: string[] = [];
    const dispatcher = new CommandDispatcher({
      send: (raw) => {
        ids.push(JSON.parse(raw).requestId as string);
        return true;
      },
      logLocal: () => {},
    });
    return { dispatcher, ids };
  }

  /** Mint `count` ids from a fresh dispatcher, standing in for one page load. */
  function mintIds(count: number): string[] {
    const { dispatcher, ids } = newMintingDispatcher();
    for (let i = 0; i < count; i++) void dispatcher.submitPrompt("/w", "hi", PromptOrigin.WEBAPP_USER_SENT, "");
    return ids;
  }

  it("mints fe-<load nonce>-<counter>", () => {
    // Arrange
    const { dispatcher, ids } = newMintingDispatcher();
    // Act
    void dispatcher.submitPrompt("/w", "hi", PromptOrigin.WEBAPP_USER_SENT, "");
    // Assert
    expect(ids[0]).toMatch(/^fe-[0-9a-f]{16}-1$/);
  });

  it("does not collide across page loads, because ids are durable ledger keys", () => {
    // Arrange — two dispatcher instances stand in for two page loads, whose
    // counters both restart at 1.
    const first = mintIds(50);
    // Act
    const second = mintIds(50);
    // Assert
    expect(new Set([...first, ...second]).size).toBe(100);
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
    // Arrange — the clientLog carve-out must not blanket-silence onAck. The
    // report is console-only: forwarding it is the ack/clientLog loop.
    const { dispatcher, consoleRecords } = newDispatcher();
    // Act
    dispatcher.observe(ackFrame("never-sent", true));
    // Assert
    expect(consoleRecords).toContainEqual(
      expect.objectContaining({ operation: "command-dispatch.ack-unknown-request" }),
    );
  });

  it("tracks no ack id for a frame the socket refused", () => {
    // Arrange — an undelivered log can never be acked, so remembering its id
    // would leak it.
    const { dispatcher, consoleRecords } = newDispatcher(false);
    dispatcher.clientLog("/w", "info", "m");
    // Act
    dispatcher.observe(ackFrame("r1", true));
    // Assert — the id was never tracked, so this reads as an unknown ack,
    // reported console-only (see the no-reentry boundary in onAck).
    expect(consoleRecords).toContainEqual(
      expect.objectContaining({ operation: "command-dispatch.ack-unknown-request" }),
    );
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

describe("clientLog rejection circuit breaker", () => {
  /**
   * A rejected client log is never transient in practice: the daemon refuses
   * one whose session attribution disagrees with its registry, and that does
   * not fix itself. Unbounded, every line the page emits floods the shared
   * daemon log — 143,057 records from four idle workspaces in one incident.
   */

  /** Send one client log and reject its ack, returning the dispatcher. */
  const rejectRun = (h: ReturnType<typeof newDispatcher>, count: number, from = 0): void => {
    for (let i = 0; i < count; i += 1) {
      h.dispatcher.clientLog("/w", "info", `line ${from + i}`);
      h.dispatcher.observe(ackFrame(`r${from + i + 1}`, false, "attribution disagrees"));
    }
  };

  it("keeps forwarding while rejections stay below the bound", () => {
    // Arrange
    const h = newDispatcher();
    // Act — one short of the bound.
    rejectRun(h, 19);
    // Assert
    expect(h.dispatcher.clientLog("/w", "info", "still forwarding")).toBe(true);
  });

  it("trips forwarding off after a run of consecutive rejections", () => {
    // Arrange
    const h = newDispatcher();
    // Act
    rejectRun(h, 20);
    // Assert — the next record is not put on the wire at all.
    const before = h.sent.length;
    expect(h.dispatcher.clientLog("/w", "info", "flood")).toBe(false);
    expect(h.sent).toHaveLength(before);
  });

  it("reports the trip once, naming the reason", () => {
    // Arrange
    const h = newDispatcher();
    // Act — well past the bound.
    rejectRun(h, 25);
    // Assert
    const trips = h.records.filter(
      (r) => typeof r.local_only === "string" && r.local_only.includes("forwarding DISABLED"),
    );
    expect(trips).toHaveLength(1);
    expect(trips[0].local_only).toContain("attribution disagrees");
  });

  it("an accepted log clears the run so a recovered page keeps forwarding", () => {
    // Arrange — 19 rejections, then one that lands.
    const h = newDispatcher();
    rejectRun(h, 19);
    h.dispatcher.clientLog("/w", "info", "accepted");
    h.dispatcher.observe(ackFrame("r20", true));
    // Act — another 19 rejections must not trip it.
    rejectRun(h, 19, 20);
    // Assert
    expect(h.dispatcher.clientLog("/w", "info", "still forwarding")).toBe(true);
  });
});

describe("hibernate and revive dispatch", () => {
  it("sends HibernateWorkspaceCmd against the named workspace", async () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    const p = dispatcher.hibernateWorkspace("/w");
    dispatcher.observe(ackFrame("r1", true));
    await p;
    // Assert
    expect(JSON.parse(sent[0])).toMatchObject({ workspace: "/w", hibernateWorkspace: {} });
  });

  it("surfaces the daemon's hibernate refusal through the classified-failure sink", async () => {
    // Arrange — the daemon refuses a hibernate while a turn is live or the
    // merge lease is held, and that nack is the ONLY thing that tells the user
    // why the workspace they asked to sleep is still awake.
    const failures: unknown[] = [];
    const { records } = installLogging();
    const dispatcher = new CommandDispatcher({
      send: () => true,
      newRequestId: () => "r1",
      logLocal: (message) => records.push({ local_only: message }),
      onFailure: (f) => failures.push(f),
    });
    const p = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(
      decodeFrontendFrame(
        JSON.stringify({
          commandAck: {
            requestId: "r1",
            ok: false,
            error: "workspace is not settled",
            failure: { sessionHibernated: { sinceMs: "1700000000000" } },
          },
        }),
      ),
    );
    await p.catch(() => {});
    // Assert
    expect(failures).toHaveLength(1);
  });

  it("rejects the hibernate promise on a refusal, so no caller reads it as done", async () => {
    // Arrange
    const { dispatcher } = newDispatcher();
    const p = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(ackFrame("r1", false, "merge lease held"));
    // Assert
    await expect(p).rejects.toThrow(/hibernateWorkspace rejected: merge lease held/);
  });

  it("sends the compact-first revival decision", async () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    const p = dispatcher.reviveSession("/w", "compactAll");
    dispatcher.observe(ackFrame("r1", true));
    await p;
    // Assert
    expect(JSON.parse(sent[0]).reviveSession).toEqual({ compactFirst: { scope: "COMPACTION_SCOPE_ALL" } });
  });

  it("sends the clear revival decision", async () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    const p = dispatcher.reviveSession("/w", "clear");
    dispatcher.observe(ackFrame("r1", true));
    await p;
    // Assert
    expect(JSON.parse(sent[0]).reviveSession).toEqual({ clear: {} });
  });

  it("sends the direct revival decision", async () => {
    // Arrange
    const { dispatcher, sent } = newDispatcher();
    // Act
    const p = dispatcher.reviveSession("/w", "direct");
    dispatcher.observe(ackFrame("r1", true));
    await p;
    // Assert
    expect(JSON.parse(sent[0]).reviveSession).toEqual({ direct: {} });
  });

  it("surfaces a hibernate nack that carries only an error string", async () => {
    // Arrange — a legacy-shaped nack (no classified failure) used to be
    // log-only, which is exactly the disposition the sleep verb cannot afford.
    const { dispatcher, failures } = newFailureDispatcher();
    const p = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(ackFrame("r1", false, "merge lease held"));
    await p.catch(() => {});
    // Assert
    expect(failures).toHaveLength(1);
  });

  it("carries the daemon's own words on an unclassified nack", async () => {
    // Arrange — the daemon decided the refusal; this end only names it.
    const { dispatcher, failures } = newFailureDispatcher();
    const p = dispatcher.reviveSession("/w", "direct");
    // Act
    dispatcher.observe(ackFrame("r1", false, "session is not hibernated"));
    await p.catch(() => {});
    // Assert
    expect(cardOf(failures[0]).view.message).toBe("session is not hibernated");
  });

  it("names an unclassified nack with the frontend's own arm", async () => {
    // Arrange — the classification is this end's, so the arm says so.
    const { dispatcher, failures } = newFailureDispatcher();
    const p = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(ackFrame("r1", false, "merge lease held"));
    await p.catch(() => {});
    // Assert
    expect(failureKindName(cardOf(failures[0]).view.kind)).toBe(
      "commandRejectionUnclassified",
    );
  });

  it("reconciles repeated refusals of one command onto a single card", async () => {
    // Arrange — a per-refusal card would bury the feed under the same fact.
    const { dispatcher, failures } = newFailureDispatcher();
    const first = dispatcher.hibernateWorkspace("/w");
    dispatcher.observe(ackFrame("r1", false, "merge lease held"));
    await first.catch(() => {});
    const second = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(ackFrame("r2", false, "merge lease held"));
    await second.catch(() => {});
    // Assert
    expect(cardOf(failures[0]).uuid).toBe(cardOf(failures[1]).uuid);
  });

  it("keeps two different refused commands as two cards", async () => {
    // Arrange — a refused hibernate must not overwrite a refused revive.
    const { dispatcher, failures } = newFailureDispatcher();
    const hibernate = dispatcher.hibernateWorkspace("/w");
    dispatcher.observe(ackFrame("r1", false, "merge lease held"));
    await hibernate.catch(() => {});
    const revive = dispatcher.reviveSession("/w", "direct");
    // Act
    dispatcher.observe(ackFrame("r2", false, "not hibernated"));
    await revive.catch(() => {});
    // Assert
    expect(cardOf(failures[0]).uuid).not.toBe(cardOf(failures[1]).uuid);
  });

  it("prefers the daemon's classified failure over the locally-named one", async () => {
    // Arrange — when the daemon DID classify, this end adds nothing.
    const { dispatcher, failures } = newFailureDispatcher();
    const p = dispatcher.hibernateWorkspace("/w");
    // Act
    dispatcher.observe(
      decodeFrontendFrame(
        JSON.stringify({
          commandAck: {
            requestId: "r1",
            ok: false,
            error: "workspace is not settled",
            failure: { sessionHibernated: { sinceMs: "1700000000000" } },
          },
        }),
      ),
    );
    await p.catch(() => {});
    // Assert
    expect(failureKindName(cardOf(failures[0]).view.kind)).toBe("sessionHibernated");
  });

  it("surfaces a revive the socket refused to send", async () => {
    // Arrange — no ack will ever arrive for a frame that never left the page,
    // so this rejection shape has no other route to a human.
    const { dispatcher, failures } = newFailureDispatcher(false);
    // Act
    await dispatcher.reviveSession("/w", "compactAll").catch(() => {});
    // Assert
    expect(failureKindName(cardOf(failures[0]).view.kind)).toBe("commandUnsent");
  });

  it("says the connection is down on a refused send, not that the daemon refused", async () => {
    // Arrange — nothing was decided; the operation is retryable.
    const { dispatcher, failures } = newFailureDispatcher(false);
    // Act
    await dispatcher.hibernateWorkspace("/w").catch(() => {});
    // Assert
    expect(cardOf(failures[0]).view.message).toContain("connection to the daemon is down");
  });

  it("rejects a revive the daemon refused, so the gate can offer the choice again", async () => {
    // Arrange
    const { dispatcher } = newDispatcher();
    const p = dispatcher.reviveSession("/w", "direct");
    // Act
    dispatcher.observe(ackFrame("r1", false, "session is not hibernated"));
    // Assert
    await expect(p).rejects.toThrow(/reviveSession rejected: session is not hibernated/);
  });
});

/**
 * Dial-on-demand: a refused send establishes the connection and retries once
 * before the command is reported unsent. A hidden Emacs webview's timers are
 * suspended, so the socket's own scheduled reconnect can still be pending when
 * the user switches to the workspace and clicks.
 */
describe("dispatch dial-on-demand", () => {
  /**
   * A dispatcher whose transport starts closed. `open()` makes further sends
   * succeed; `ensureConnected` records the dial and, when `dialOpens`, opens
   * the transport and answers the currentness wait.
   */
  function newDeferringDispatcher(opts: { dialOpens: boolean }) {
    const sent: string[] = [];
    const failures: CommandRefusal[] = [];
    const { records } = installLogging();
    let open = false;
    let dials = 0;
    let n = 0;
    const dispatcher = new CommandDispatcher({
      send: (raw) => {
        if (!open) return false;
        sent.push(raw);
        return true;
      },
      newRequestId: () => `r${++n}`,
      logLocal: (message) => records.push({ local_only: message }),
      onFailure: (f) => failures.push(f),
      ensureConnected: () => {
        dials += 1;
        if (opts.dialOpens) open = true;
      },
      whenCurrent: () => Promise.resolve(open),
    });
    return {
      dispatcher,
      sent,
      failures,
      records,
      dials: () => dials,
      pendingCount: () => dispatcher.pendingCount(),
    };
  }

  it("dials and sends the command once the connection becomes current", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: true });
    // Act
    const p = h.dispatcher.reviveSession("/w", "compactAll");
    await vi.waitFor(() => expect(h.sent).toHaveLength(1));
    h.dispatcher.observe(ackFrame("r1", true));
    // Assert
    await expect(p).resolves.toBeUndefined();
    expect(h.dials()).toBe(1);
  });

  it("records the deferral before it dials", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: true });
    // Act
    const p = h.dispatcher.reviveSession("/w", "compactAll");
    await vi.waitFor(() => expect(h.sent).toHaveLength(1));
    h.dispatcher.observe(ackFrame("r1", true));
    await p;
    // Assert
    expect(h.records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.dispatch-deferred",
      context: expect.objectContaining({ command: "reviveSession", workspace: "/w", wait_budget_ms: 10_000 }),
    }));
  });

  it("correlates the ack of a command that was sent only after the dial", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: true });
    const p = h.dispatcher.hibernateWorkspace("/w");
    await vi.waitFor(() => expect(h.sent).toHaveLength(1));
    // Act — the daemon refuses the deferred command.
    h.dispatcher.observe(ackFrame("r1", false, "a turn is live"));
    // Assert — the refusal reaches the caller, so correlation survived.
    await expect(p).rejects.toThrow(/hibernateWorkspace rejected: a turn is live/);
  });

  it("reports the command unsent when currentness never arrives", async () => {
    // Arrange — the dial never opens the transport.
    const h = newDeferringDispatcher({ dialOpens: false });
    // Act / Assert — exactly today's refusal.
    await expect(h.dispatcher.reviveSession("/w", "compactAll")).rejects.toThrow(/socket not open/);
    expect(failureKindName(cardOf(h.failures[0]).view.kind)).toBe("commandUnsent");
  });

  it("logs the original rejection record when the dial does not help", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: false });
    // Act
    await h.dispatcher.reviveSession("/w", "compactAll").catch(() => {});
    // Assert
    expect(h.records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.dispatch-rejected",
      context: expect.objectContaining({ command: "reviveSession", cause: "socket not open" }),
    }));
  });

  it("leaves no pending entry behind when a deferred command ends unsent", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: false });
    // Act
    await h.dispatcher.reviveSession("/w", "compactAll").catch(() => {});
    // Assert
    expect(h.pendingCount()).toBe(0);
  });

  it("surfaces a dial that throws instead of swallowing it", async () => {
    // Arrange
    const { records } = installLogging();
    const dispatcher = new CommandDispatcher({
      send: () => false,
      newRequestId: () => "r1",
      logLocal: () => undefined,
      ensureConnected: () => {
        throw new Error("no transport");
      },
      whenCurrent: () => Promise.resolve(true),
    });
    // Act
    await dispatcher.reviveSession("/w", "direct").catch(() => {});
    // Assert
    expect(records).toContainEqual(expect.objectContaining({
      operation: "command-dispatch.dispatch-connect-failed",
      context: expect.objectContaining({ command: "reviveSession", cause: "no transport" }),
    }));
  });

  it("defers each of two concurrent commands on its own request id", async () => {
    // Arrange
    const h = newDeferringDispatcher({ dialOpens: true });
    // Act
    const first = h.dispatcher.hibernateWorkspace("/w");
    const second = h.dispatcher.reviveSession("/w", "direct");
    await vi.waitFor(() => expect(h.sent).toHaveLength(2));
    h.dispatcher.observe(ackFrame("r2", true));
    h.dispatcher.observe(ackFrame("r1", true));
    // Assert — acks correlate by id, in either order.
    await expect(Promise.all([first, second])).resolves.toEqual([undefined, undefined]);
  });

  it("refuses immediately when no dial hook is wired", async () => {
    // Arrange — the bootstrap dispatcher's shape: no ensureConnected.
    const { dispatcher, records } = newDispatcher(false);
    // Act / Assert
    await expect(dispatcher.reviveSession("/w", "direct")).rejects.toThrow(/socket not open/);
    expect(records).not.toContainEqual(expect.objectContaining({
      operation: "command-dispatch.dispatch-deferred",
    }));
  });
});


// --- the refusal disposition: reveal a filed card, or file one --------------
//
// `CommandAck.failure_card` exists so a refusal that ALREADY produced a feed
// card is pointed at rather than restated. Getting this wrong puts one failure
// on screen twice, worded two different ways.

describe("commandRefusal", () => {
  function ack(over: Record<string, unknown> = {}) {
    const frame = decodeFrontendFrame(
      JSON.stringify({ commandAck: { requestId: "r1", ok: false, ...over } }),
    );
    if (frame.frame.case !== "commandAck") throw new Error("wrong variant");
    return frame.frame.value;
  }

  it("reveals the card the daemon filed the refusal under", () => {
    // Arrange / Act
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
      failureCard: { cardUuid: "failure:e9" },
    }));
    // Assert
    expect(refusal).toEqual({
      kind: "reveal",
      cardUuid: "failure:e9",
      failure: expect.anything(),
      // The refusal's own words ride along, so a reveal that finds nothing can
      // still state the refusal instead of dropping it.
      command: "hibernateWorkspace",
      error: "",
    });
  });

  it("files a card when the ref is EMPTY, which means no card was produced", () => {
    // Arrange / Act — an empty ref is why the field is a message wrapping a
    // string rather than a bare string that would make "" ambiguous.
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
      failureCard: { cardUuid: "" },
      error: "already asleep",
    }));
    // Assert
    expect(refusal.kind).toBe("card");
  });

  it("carries the DAEMON's kind verbatim onto a filed card", () => {
    // Arrange / Act — this end adds the sentence and nothing else.
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
    }));
    // Assert
    expect(failureKindName(cardOf(refusal).view.kind)).toBe("sessionHibernated");
  });

  it("classifies a refusal the daemon carried no kind for", () => {
    // Arrange / Act — somebody has to name it, or the refusal reaches the user
    // through nothing at all.
    const refusal = commandRefusal("hibernateWorkspace", ack({ error: "merge lease held" }));
    // Assert
    expect(failureKindName(cardOf(refusal).view.kind)).toBe("commandRejectionUnclassified");
  });

  it("files a card when a card ref arrives with NO classified failure beside it", () => {
    // Arrange / Act — a ref alone names a card whose account this end never
    // received; restating the refusal is better than revealing on faith.
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failureCard: { cardUuid: "failure:e9" },
      error: "merge lease held",
    }));
    // Assert
    expect(refusal.kind).toBe("card");
  });

  it("leads a filed card with the daemon's error text", () => {
    // Arrange / Act
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
      error: "already asleep",
    }));
    // Assert
    expect(cardOf(refusal).view.message).toBe("already asleep");
  });

  it("names the command when the daemon sent no error text at all", () => {
    // Arrange / Act
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
    }));
    // Assert
    expect(cardOf(refusal).view.message).toBe("hibernateWorkspace was refused");
  });

  it("makes a filed refusal card TERMINAL, since a refusal has no closing edge", () => {
    // Arrange / Act
    const refusal = commandRefusal("hibernateWorkspace", ack({
      failure: { sessionHibernated: { sinceMs: "1" } },
    }));
    // Assert
    expect(cardOf(refusal).view.lifecycle).toEqual({ case: "terminal" });
  });
});

// --- surfacing: every branch ends in a card the user can see -----------------
//
// A reveal is an ADDRESS, and an address can be unreachable: the feed this page
// holds may never have received the card the daemon filed. Logging that and
// returning left a nacked command reaching the user through nothing at all.

describe("surfaceRefusal", () => {
  /** The three surfaces, recording what each was asked to do. */
  function surfaces(revealFinds: boolean) {
    const revealed: string[] = [];
    const filed: FailureCardItem[] = [];
    const logs: string[] = [];
    return {
      revealed,
      filed,
      logs,
      out: {
        reveal: (uuid: string) => {
          revealed.push(uuid);
          return revealFinds;
        },
        file: (card: FailureCardItem) => filed.push(card),
        log: (message: string) => logs.push(message),
      },
    };
  }

  function revealRefusal(): CommandRefusal {
    const frame = decodeFrontendFrame(
      JSON.stringify({
        commandAck: {
          requestId: "r1",
          ok: false,
          error: "already asleep",
          failure: { sessionHibernated: { sinceMs: "1" } },
          failureCard: { cardUuid: "failure:e9" },
        },
      }),
    );
    if (frame.frame.case !== "commandAck") throw new Error("wrong variant");
    return commandRefusal("hibernateWorkspace", frame.frame.value);
  }

  it("reveals the daemon's card and files NOTHING beside it", () => {
    // Arrange
    const s = surfaces(true);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert — restating the account inline would put one failure on screen
    // twice, worded two different ways.
    expect([s.revealed, s.filed]).toEqual([["failure:e9"], []]);
  });

  it("FILES the refusal when the reveal finds nothing", () => {
    // Arrange
    const s = surfaces(false);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert
    expect(s.filed).toHaveLength(1);
  });

  it("leaves exactly ONE log record for a missed reveal", () => {
    // Arrange
    const s = surfaces(false);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert — the broken reveal contract is worth knowing once, not twice.
    expect(s.logs).toEqual(["refusal card failure:e9 is not in this feed"]);
  });

  it("carries the DAEMON's kind verbatim onto the fallback card", () => {
    // Arrange
    const s = surfaces(false);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert
    expect(failureKindName(s.filed[0].view.kind)).toBe("sessionHibernated");
  });

  it("gives the fallback card the DAEMON's uuid, so a late delivery reconciles", () => {
    // Arrange
    const s = surfaces(false);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert — the missed card arriving later must land on this one rather
    // than stand beside it as a second account of one refusal.
    expect(s.filed[0].uuid).toBe("failure:e9");
  });

  it("leads the fallback card with the daemon's own refusal text", () => {
    // Arrange
    const s = surfaces(false);
    // Act
    surfaceRefusal(revealRefusal(), s.out);
    // Assert
    expect(s.filed[0].view.message).toBe("already asleep");
  });

  it("files a `card` refusal without attempting any reveal", () => {
    // Arrange
    const s = surfaces(true);
    const card: FailureCardItem = {
      kind: "failure",
      uuid: "local:x",
      view: {
        kind: create(FailureKindSchema, { kind: { case: "shimNotSpawned", value: {} } }),
        message: "m",
        detail: "",
        lifecycle: { case: "terminal" },
      },
    };
    // Act
    surfaceRefusal({ kind: "card", card }, s.out);
    // Assert
    expect([s.revealed, s.filed]).toEqual([[], [card]]);
  });
});
