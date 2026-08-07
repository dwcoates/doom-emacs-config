import { afterEach, describe, expect, it, vi } from "vitest";
import { ClientLogCmd, ClientLogContext } from "../src/protocol.js";
import timestampFixtureRaw from "../../proto/vocab/log-timestamp.json?raw";
import {
  ForwardingLogger,
  bindLogContext,
  clearLogDedup,
  log,
  logVerbose,
  resetLoggingForTests,
  setLogger,
} from "../src/wslog.js";

/** A logger wired to spies: captured forwards, captured console lines. */
function spyLogger(sendResult = true): {
  logger: ForwardingLogger;
  forwarded: ClientLogCmd[];
  consoleLines: string[];
} {
  const forwarded: ClientLogCmd[] = [];
  const consoleLines: string[] = [];
  const logger = new ForwardingLogger(
    (cmd) => {
      forwarded.push(cmd);
      return sendResult;
    },
    (level, line) => consoleLines.push(`${level}: ${line}`),
  );
  return { logger, forwarded, consoleLines };
}

function canonicalRecord(
  level: ClientLogCmd["level"],
  message: string,
  context: Record<string, unknown> = {},
): ClientLogContext {
  return {
    timestamp: "2026-07-28T12:00:00.000Z",
    runtime: "webapp",
    level,
    verbosity: "normal",
    operation: "test.forwarding",
    message,
    context,
    connection_id: "test-connection",
  };
}

function consoleRecord(line: string): Record<string, unknown> {
  return JSON.parse(line.slice(line.indexOf(": ") + 2)) as Record<string, unknown>;
}

function installCanonicalLogger(logger: ForwardingLogger): void {
  setLogger(logger);
  bindLogContext({
    workspace_dir: "/repo/workspace",
    workspace_id: "workspace-1",
    connection_id: "connection-1",
    agent_repl_session_id: "session-1",
  });
}

describe("ForwardingLogger", () => {
  it("forwards a line to the daemon and echoes it to the console", () => {
    // Arrange
    const spy = spyLogger();
    // Act
    const record = canonicalRecord("warn", "seq gap: have 3, got 7");
    spy.logger.write("warn", "seq gap: have 3, got 7", record);
    // Assert
    expect(spy.forwarded).toEqual([
      { type: "client-log", level: "warn", message: "seq gap: have 3, got 7", context: record },
    ]);
    expect(consoleRecord(spy.consoleLines[0])).toEqual(record);
  });

  it("retains a refused forward and flushes it when the socket becomes available", () => {
    let available = false;
    const forwarded: ClientLogCmd[] = [];
    const consoleLines: string[] = [];
    const logger = new ForwardingLogger((cmd) => {
      if (!available) return false;
      forwarded.push(cmd);
      return true;
    }, (level, line) => consoleLines.push(`${level}: ${line}`));

    logger.write("error", "boom", canonicalRecord("error", "boom"));
    expect(logger.pendingCount()).toBe(1);
    expect(forwarded).toEqual([]);
    expect(consoleRecord(consoleLines[0])).toMatchObject({ level: "error", message: "boom" });

    available = true;
    expect(logger.flush()).toBe(true);
    expect(logger.pendingCount()).toBe(0);
    expect(forwarded).toHaveLength(1);
  });

  it("fails loudly instead of dropping a record when the forwarding queue is full", () => {
    const logger = new ForwardingLogger(() => false, () => {}, 1);
    logger.write("error", "first", canonicalRecord("error", "first"));

    expect(() => logger.write("error", "second", canonicalRecord("error", "second")))
      .toThrow("queue exhausted");
    expect(logger.pendingCount()).toBe(1);
  });

  it("retains a record when a writable transport throws during send", () => {
    const failure = new Error("socket send raced close");
    const logger = new ForwardingLogger(() => {
      throw failure;
    }, () => {});

    expect(() => logger.write("error", "raced", canonicalRecord("error", "raced")))
      .toThrow(failure);
    expect(logger.pendingCount()).toBe(1);
  });

  it("retains the current record when flushing an older record throws", () => {
    let transport: "closed" | "throw" = "closed";
    const logger = new ForwardingLogger(() => {
      if (transport === "throw") throw new Error("flush raced close");
      return false;
    }, () => {});
    logger.write("warn", "older", canonicalRecord("warn", "older"));
    transport = "throw";

    expect(() => logger.write("error", "current", canonicalRecord("error", "current")))
      .toThrow("flush raced close");
    expect(logger.pendingCount()).toBe(2);
  });

  it("persists every normal record without rate-limit suppression", () => {
    const spy = spyLogger();
    for (let i = 0; i < 80; i++) {
      spy.logger.write("info", `line ${i}`, canonicalRecord("info", `line ${i}`));
    }
    expect(spy.forwarded).toHaveLength(80);
    expect(spy.consoleLines).toHaveLength(80);
  });
});

describe("module-level singleton", () => {
  afterEach(() => {
    resetLoggingForTests();
    vi.restoreAllMocks();
  });

  it("routes log() through the installed ForwardingLogger", () => {
    // Arrange
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    // Act
    log("warn", "routed", { operation: "test.route" });
    // Assert
    expect(consoleRecord(spy.consoleLines[0])).toMatchObject({ level: "warn", message: "routed", operation: "test.route" });
  });

  it("fails loudly before a logger is installed", () => {
    resetLoggingForTests();
    expect(() => log("warn", "early line", { operation: "test.early" })).toThrow("wslog logger is not installed");
  });

  it.each([
    ["workspace_id", { workspace_id: "workspace-1", connection_id: "connection-1" }, "workspace_dir"],
    ["workspace_dir", { workspace_dir: "/repo/workspace", connection_id: "connection-1" }, "workspace_id"],
  ])("fails before forwarding when %s is supplied without complete workspace routing", (_field, context, expected) => {
    const spy = spyLogger();
    setLogger(spy.logger);
    bindLogContext(context);

    expect(() => log("warn", "missing workspace", { operation: "test.missing-workspace" }))
      .toThrow(expected);
    expect(spy.forwarded).toEqual([]);
    expect(spy.consoleLines).toEqual([]);
  });

  it.each([
    ["invalid level", "trace" as ClientLogCmd["level"], { operation: "test.invalid-level" }, "invalid level"],
    ["empty operation", "warn" as ClientLogCmd["level"], { operation: "" }, "operation"],
    ["invalid identity", "warn" as ClientLogCmd["level"], { operation: "test.invalid-identity", context: { request_id: 3 } }, "request_id"],
    ["operation override", "warn" as ClientLogCmd["level"], { operation: "test.operation", context: { operation: "wrong" } }, "must not override operation"],
    ["bound identity conflict", "warn" as ClientLogCmd["level"], { operation: "test.identity", context: { agent_repl_session_id: "wrong" } }, "conflicts with bound identity"],
  ])("rejects %s without forwarding or console mutation", (_name, level, context, expected) => {
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);

    expect(() => log(level, "must not persist", context)).toThrow(expected);
    expect(spy.forwarded).toEqual([]);
    expect(spy.consoleLines).toEqual([]);
  });

  it("forwards a JSON-safe record when context contains an Error and a cycle", () => {
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    const cycle: Record<string, unknown> = {};
    cycle.self = cycle;

    log("error", "serialization failed", { operation: "test.serialize", context: { cause: new Error("broken"), cycle } });

    const record = spy.forwarded[0].context;
    expect(() => JSON.stringify(record)).not.toThrow();
    expect(record).toMatchObject({
      context: { cause: { name: "Error", message: "broken" }, cycle: { self: "[Circular]" } },
    });
  });

  it("dedupKey suppresses a repeated message", () => {
    // Arrange
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    // Act — a per-frame guard fires three times with the same error.
    log("error", "boom", { operation: "test.dedup", dedupKey: "feed-render" });
    log("error", "boom", { operation: "test.dedup", dedupKey: "feed-render" });
    log("error", "boom", { operation: "test.dedup", dedupKey: "feed-render" });
    // Assert
    expect(spy.consoleLines).toHaveLength(1);
    expect(consoleRecord(spy.consoleLines[0])).toMatchObject({ level: "error", message: "boom" });
  });

  it("dedupKey logs again when its message changes", () => {
    // Arrange
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    log("warn", "first error", { operation: "test.changed", dedupKey: "k" });
    // Act
    log("warn", "second error", { operation: "test.changed", dedupKey: "k" });
    // Assert
    expect(spy.consoleLines.map((line) => consoleRecord(line).message)).toEqual(["first error", "second error"]);
  });

  it("dedup keys are independent", () => {
    // Arrange
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    // Act
    log("info", "same", { operation: "test.independent", dedupKey: "a" });
    log("info", "same", { operation: "test.independent", dedupKey: "b" });
    // Assert
    expect(spy.consoleLines).toHaveLength(2);
  });

  it("clearLogDedup re-arms a key for an identical message", () => {
    // Arrange
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    log("warn", "tail fetch failed", { operation: "test.rearm", dedupKey: "poll:t1" });
    // Act — recovery observed, then the same failure returns.
    clearLogDedup("poll:t1");
    log("warn", "tail fetch failed", { operation: "test.rearm", dedupKey: "poll:t1" });
    // Assert
    expect(spy.consoleLines).toHaveLength(2);
  });
});

describe("structured context (E4 ClientLogCmd.context)", () => {
  it("forwards a context object on the frame", () => {
    // Arrange
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    // Act
    const record = canonicalRecord("warn", "render stall", { pendingMs: 1200, visibility: "visible" });
    logger.write("warn", "render stall", record);
    // Assert
    expect(sent).toHaveLength(1);
    expect(sent[0].context).toEqual(record);
  });

  it("always forwards the canonical record context", () => {
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    const record = canonicalRecord("info", "plain line");
    logger.write("info", "plain line", record);
    expect(sent[0].context).toEqual(record);
  });

  it("writes the full canonical record as JSON to the console", () => {
    const lines: string[] = [];
    const logger = new ForwardingLogger(() => true, (_l, line) => lines.push(line));
    const record = canonicalRecord("warn", "render stall", { pendingMs: 1200 });
    logger.write("warn", "render stall", record);
    expect(JSON.parse(lines[0])).toEqual(record);
  });

  it("carries context through the module-level log()", () => {
    // Arrange
    const sent: ClientLogCmd[] = [];
    const logger = new ForwardingLogger((cmd) => {
      sent.push(cmd);
      return true;
    }, () => {});
    installCanonicalLogger(logger);
    // Act
    log("error", "poll failed", { operation: "test.context", context: { taskId: "bg1" } });
    // Assert
    expect(sent[0].context).toMatchObject({ operation: "test.context", context: { taskId: "bg1" } });
  });
});

describe("verbose and bound runtime context", () => {
  const verboseStorage = new Map<string, string>();

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  afterEach(() => {
    verboseStorage.clear();
    resetLoggingForTests();
  });

  it("always forwards verbose logs while gating their console output", () => {
    vi.stubGlobal("localStorage", {
      getItem: (key: string) => verboseStorage.get(key) ?? null,
      setItem: (key: string, value: string) => verboseStorage.set(key, value),
    });
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    logVerbose("info", "hidden locally", { operation: "test.verbose" });
    expect(spy.forwarded).toHaveLength(1);
    expect(spy.consoleLines).toEqual([]);
    verboseStorage.set("agent-repl-log-verbose", "true");
    logVerbose("info", "visible locally", { operation: "test.verbose" });
    expect(spy.forwarded).toHaveLength(2);
    expect(consoleRecord(spy.consoleLines[0])).toMatchObject({
      level: "info",
      message: "visible locally",
      verbosity: "verbose",
    });
  });

  it("always forwards verbose logs after the normal forwarding budget is exhausted", () => {
    vi.stubGlobal("localStorage", { getItem: () => null });
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    for (let i = 0; i < 65; i++) {
      log("info", `normal ${i}`, { operation: "test.normal" });
    }

    logVerbose("info", "durable verbose record", { operation: "test.verbose" });

    expect(spy.forwarded.at(-1)).toMatchObject({
      type: "client-log",
      level: "info",
      message: "durable verbose record",
      context: { verbosity: "verbose", operation: "test.verbose" },
    });
  });

  it("merges bound runtime identity with call-specific context", () => {
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    bindLogContext({ agent_repl_session_id: "s1", connection_id: "c1" });
    log("error", "request failed", { operation: "submit", context: { request_id: "r1" } });
    expect(spy.forwarded[0].context).toMatchObject({
      runtime: "webapp", operation: "submit", workspace_dir: "/repo/workspace", workspace_id: "workspace-1",
      connection_id: "c1", agent_repl_session_id: "s1", request_id: "r1",
    });
  });

  it("omits an empty bound identity rather than refusing the record", () => {
    // Arrange — a workspace-addressed page binds no session id until the daemon
    // rules on which session its workspace owns.
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    bindLogContext({ agent_repl_session_id: "", connection_id: "c1" });

    // Act
    log("info", "socket opening", { operation: "connect" });

    // Assert — the record is written, attributed to the workspace alone.
    expect(spy.forwarded).toHaveLength(1);
    expect(spy.forwarded[0].context).not.toHaveProperty("agent_repl_session_id");
  });

  it("still refuses a bound identity of the wrong type", () => {
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);
    bindLogContext({ agent_repl_session_id: 7 as unknown as string, connection_id: "c1" });
    expect(() => log("info", "socket opening", { operation: "connect" })).toThrow(
      "requires agent_repl_session_id",
    );
  });
});

describe("local-only canonical option", () => {
  it("writes to the console without forwarding", () => {
    // Arrange — forwarding a clientLog-rejection report would earn another
    // rejection and loop, so this path must never send.
    const sent: ClientLogCmd[] = [];
    const lines: string[] = [];
    const logger = new ForwardingLogger(
      (cmd) => {
        sent.push(cmd);
        return true;
      },
      (_l, line) => lines.push(line),
    );
    // Act
    const record = canonicalRecord("error", "clientLog rejected: no message");
    logger.write("error", "clientLog rejected: no message", record, true, false);
    // Assert
    expect(JSON.parse(lines[0])).toEqual(record);
    expect(sent).toEqual([]);
  });
});


describe("emitted record timestamps", () => {
  it("conform to the cross-language timestamp contract", () => {
    // Arrange: the webapp compiles the shared renderer, whose own suite lives
    // beside it; this asserts the emitted record actually carries its output.
    const spy = spyLogger();
    installCanonicalLogger(spy.logger);

    // Act
    log("info", "conformance probe", { operation: "test.timestamp" });

    // Assert
    const fixture = JSON.parse(timestampFixtureRaw) as { pattern: string };
    const record = spy.forwarded[0]!.context as unknown as { timestamp: string };
    expect(record.timestamp).toMatch(new RegExp(fixture.pattern));
  });
});
