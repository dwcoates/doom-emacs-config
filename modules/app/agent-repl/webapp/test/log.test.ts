import { beforeEach, describe, expect, it, vi } from "vitest";
import type { ClientLogRecord } from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { LevelWindow, selectLevel } from "../../agent-shim/logging/ts/level-window.js";
import { siteKey } from "../../agent-shim/logging/ts/logsite.js";
import { spyLog } from "./log-spy.js";
import {
  CLIENT_LOG_HELD_BOUND,
  ForwardingLogger,
  HELD_DROPPED_OPERATION,
  bindLogContext,
  buildClientLogRecord,
  clearLogDedup,
  createLogger,
  logLevelSelection,
  parseClientLogLevel,
  resetLoggingForTests,
  restampRecordIdentity,
  setLogger,
  type ClientLogLevel,
  type ClientLogSink,
  type WebappLogRecord,
} from "../src/log.js";

const LEVELS: readonly ClientLogLevel[] = ["debug", "info", "warn", "error"];

const log = createLogger("webapp.test.log");

interface Harness {
  logger: ForwardingLogger;
  sent: ClientLogRecord[];
  console: Array<[ClientLogLevel, string]>;
}

/** A logger whose sink resolves and whose console is captured. */
function install(
  sink?: ClientLogSink,
  minimumLevel: ClientLogLevel | LevelWindow = "debug",
): Harness {
  const sent: ClientLogRecord[] = [];
  const consoleLines: Array<[ClientLogLevel, string]> = [];
  const logger = new ForwardingLogger(
    sink ??
      (async (record) => {
        sent.push(record);
        return "accepted";
      }),
    (level, line) => consoleLines.push([level, line]),
    {},
    minimumLevel,
  );
  setLogger(logger);
  bindLogContext({ connection_id: "test-connection" });
  return { logger, sent, console: consoleLines };
}

function webappRecord(
  level: ClientLogLevel,
  overrides: Partial<WebappLogRecord> = {},
): WebappLogRecord {
  return {
    timestamp: "2026-09-10T12:34:56.789000-04:00",
    runtime: "webapp",
    level,
    verbosity: "normal",
    operation: "test.op",
    site: "0123456789ab",
    template: "m",
    message: "m",
    context: {},
    connection_id: "connection-1",
    ...overrides,
  };
}

/**
 * Release the throttle's window and let the sink's promise settle.
 *
 * A non-error record waits out `clientlog-throttle`'s window by design (an
 * error flushes immediately, being the record someone goes looking for), so a
 * test asserting on what was SENT has to open the window rather than assume
 * one record is one call.
 */
async function flushAndSettle(h: Harness): Promise<void> {
  h.logger.flush();
  for (let i = 0; i < 10; i += 1) await Promise.resolve();
}

beforeEach(() => {
  resetLoggingForTests();
});

describe("buildClientLogRecord: the level oneof", () => {
  it("a module's logger exposes one method per level, beside message and log", () => {
    expect(Object.keys(log).sort()).toEqual(["debug", "error", "info", "log", "message", "warn"]);
  });

  for (const level of LEVELS) {
    it(`maps ${level} onto its own arm`, () => {
      // ARRANGE / ACT
      const record = buildClientLogRecord(webappRecord(level));
      // ASSERT
      expect(record.level.case).toBe(level);
    });
  }

  it("sets exactly one arm, never two", () => {
    const record = buildClientLogRecord(webappRecord("warn"));
    expect(record.level.case).toBe("warn");
  });
});

describe("buildClientLogRecord: the record's fields", () => {
  it("carries the human sentence", () => {
    const record = buildClientLogRecord(webappRecord("info", { message: "the webapp booted" }));
    expect(record.message).toBe("the webapp booted");
  });

  it("lifts the operation onto its own field, for a machine to route on", () => {
    const record = buildClientLogRecord(webappRecord("info", { operation: "main.boot" }));
    expect(record.operation).toBe("main.boot");
  });

  it("carries the client's own timestamp", () => {
    const record = buildClientLogRecord(webappRecord("info"));
    expect(record.timestamp).toBe("2026-09-10T12:34:56.789000-04:00");
  });

  it("the live timestamp uses six fractional digits and an explicit offset", async () => {
    const h = install();
    log.info("m", { operation: "test.timestamp-shape" });
    await flushAndSettle(h);
    expect(h.sent[0].timestamp).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}[+-]\d{2}:\d{2}$/);
  });

  it("carries the client's own verbosity class", () => {
    const record = buildClientLogRecord(webappRecord("info", { verbosity: "verbose" }));
    expect(record.verbose).toBe(true);
  });

  it("keeps the call site's fields directly in the context Struct", () => {
    const record = buildClientLogRecord(webappRecord("info", { context: { rpc: "WatchFooter" } }));
    expect(record.context).toMatchObject({ rpc: "WatchFooter" });
  });

  it("does not nest the complete built record inside context", () => {
    const record = buildClientLogRecord(webappRecord("info", { context: { rows: 3 } }));
    expect(record.context).not.toHaveProperty("context");
  });

  it("builds a real generated message", () => {
    expect(buildClientLogRecord(webappRecord("info")).$typeName).toBe(
      "agentrepl.v1.ClientLogRecord",
    );
  });
});

describe("log: forwarding", () => {
  it("hands one record to the sink", async () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(1);
  });

  it("forwards the level the call site chose", async () => {
    const h = install();
    log.error("boom", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent[0].level.case).toBe("error");
  });

  it("forwards the operation", async () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent[0].operation).toBe("test.op");
  });

  it("forwards the call site's structured evidence", async () => {
    const h = install();
    log.info("hello", { operation: "test.op", context: { rows: 3 } });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({ rows: 3 });
  });

  it("stamps the bound connection id on every record", async () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({ connection_id: "test-connection" });
  });

  it("keeps workspace routing out of context because the request carries it", async () => {
    const h = install();
    bindLogContext({ workspace_id: "ws-1", workspace_dir: "/w" });
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent[0].context).not.toHaveProperty("workspace_id");
  });

  it("marks a normal record's verbosity", async () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.sent[0].verbose).toBe(false);
  });

  it("does not forward a localOnly record, the emergency console path", async () => {
    const h = install();
    log.error("hello", { operation: "test.op", localOnly: true });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(0);
  });

  it("still consoles a localOnly record", () => {
    const h = install();
    log.error("hello", { operation: "test.op", localOnly: true });
    expect(h.console).toHaveLength(1);
  });
});

describe("log: the console", () => {
  it("emits a normal record", () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    expect(h.console).toHaveLength(1);
  });

  it("emits it at the record's own level", () => {
    const h = install();
    log.warn("hello", { operation: "test.op" });
    expect(h.console[0][0]).toBe("warn");
  });

  it("emits the record as JSON, so a human can read the evidence", () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    expect(JSON.parse(h.console[0][1])).toMatchObject({ operation: "test.op", message: "hello" });
  });
});

describe("the verbosity class", () => {
  it("forwards a verbose record", async () => {
    const h = install();
    log.info("hot path", { operation: "test.op", verbosity: "verbose" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(1);
  });

  it("keeps a verbose record on the console when its level is enabled", () => {
    const h = install();
    log.info("hot path", { operation: "test.op", verbosity: "verbose" });
    expect(h.console).toHaveLength(1);
  });

  it("marks the record's verbosity", async () => {
    const h = install();
    log.info("hot path", { operation: "test.op", verbosity: "verbose" });
    await flushAndSettle(h);
    expect(h.sent[0].verbose).toBe(true);
  });
});

describe("AGENT_REPL_LOG_LEVEL", () => {
  const cases: ReadonlyArray<{
    name: string;
    minimum: ClientLogLevel;
    emitted: ClientLogLevel;
    want: number;
  }> = [
    { name: "debug admits debug", minimum: "debug", emitted: "debug", want: 1 },
    { name: "info rejects debug", minimum: "info", emitted: "debug", want: 0 },
    { name: "warn rejects info", minimum: "warn", emitted: "info", want: 0 },
    { name: "error admits error", minimum: "error", emitted: "error", want: 1 },
  ];

  for (const tc of cases) {
    it(tc.name, async () => {
      const h = install(undefined, tc.minimum);
      log[tc.emitted]("threshold case", { operation: "test.level" });
      await flushAndSettle(h);
      expect(h.sent).toHaveLength(tc.want);
    });
  }

  it("uses info only when the delivered value is absent", () => {
    expect(parseClientLogLevel(null)).toBe("info");
  });

  it("refuses an empty delivered value", () => {
    expect(() => parseClientLogLevel("")).toThrow(/invalid log_level/);
  });

  it("refuses an unknown delivered value", () => {
    expect(() => parseClientLogLevel("trace")).toThrow(/invalid log_level/);
  });
});

describe("the page's level window", () => {
  /** A logger at debug until Unix second 1000300, on a clock the test moves. */
  function installDebugWindow(clock: { at: number }): Harness {
    return install(undefined, new LevelWindow(selectLevel("debug", "1000300", clock.at), () => clock.at));
  }

  it("admits debug records before the window ends", async () => {
    // ARRANGE
    const clock = { at: 1_000_000_000 };
    const h = installDebugWindow(clock);
    clock.at = 1_000_299_000;
    // ACT
    log.debug("inside", { operation: "test.inside" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent.map((r) => r.operation)).toEqual(["test.inside"]);
  });

  it("reverts to info when the window ends and records the revert at info", async () => {
    // ARRANGE
    const clock = { at: 1_000_000_000 };
    const h = installDebugWindow(clock);
    clock.at = 1_000_300_000;
    // ACT
    log.debug("after", { operation: "test.after" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent.map((r) => [r.operation, r.level.case])).toEqual([["webapp.log.level-window", "info"]]);
    expect(h.sent[0].context).toMatchObject({ outcome: "window_ended", from_level: "debug" });
  });
});

describe("a sink failure never recurses", () => {
  it("counts a rejection", async () => {
    // ARRANGE
    const h = install(async () => {
      throw new Error("unavailable");
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    // ACT
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.logger.sinkFailureCount()).toBe(1);
  });

  it("counts EVERY rejection, so the scale of the loss is known", async () => {
    const h = install(async () => {
      throw new Error("unavailable");
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("a", { operation: "test.op" });
    log.info("b", { operation: "test.op" });
    log.info("c", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.logger.sinkFailureCount()).toBe(3);
  });

  it("announces the failure exactly ONCE, so a broken sink is not a storm", async () => {
    const h = install(async () => {
      throw new Error("unavailable");
    });
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("a", { operation: "test.op" });
    log.info("b", { operation: "test.op" });
    await flushAndSettle(h);
    expect(spy).toHaveBeenCalledOnce();
  });

  it("does not route the failure back through the logging API", async () => {
    // ARRANGE: a record ABOUT the failed send would fail to send, and loop.
    const h = install(async () => {
      throw new Error("unavailable");
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    // ACT
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT: one attempt, not a cascade.
    expect(h.logger.sinkFailureCount()).toBe(1);
  });

  it("keeps logging after the sink broke, since the console is still evidence", async () => {
    const h = install(async () => {
      throw new Error("unavailable");
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("a", { operation: "test.op" });
    await flushAndSettle(h);
    log.info("b", { operation: "test.op" });
    expect(h.console).toHaveLength(2);
  });

  it("reports no failures on a healthy sink", async () => {
    const h = install();
    log.info("hello", { operation: "test.op" });
    await flushAndSettle(h);
    expect(h.logger.sinkFailureCount()).toBe(0);
  });
});

/** A sink the test takes down and brings back: it records each record it accepts, and rejects while down. */
function outage(): { sink: ClientLogSink; accepted: ClientLogRecord[]; down: (isDown: boolean) => void } {
  const accepted: ClientLogRecord[] = [];
  let isDown = true;
  return {
    accepted,
    down: (value) => {
      isDown = value;
    },
    sink: async (record) => {
      if (isDown) throw new Error("unavailable");
      accepted.push(record);
      return "accepted";
    },
  };
}

describe("a record the daemon could not take is held, then resent", () => {
  it("holds a rejected record", async () => {
    // ARRANGE
    const daemon = outage();
    const h = install(daemon.sink);
    vi.spyOn(console, "error").mockImplementation(() => {});
    // ACT
    log.info("while down", { operation: "test.held" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.logger.heldCount()).toBe(1);
  });

  it("resends every held record, oldest first, once the daemon answers again", async () => {
    // ARRANGE
    const daemon = outage();
    const h = install(daemon.sink);
    vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("first", { operation: "test.first" });
    log.info("second", { operation: "test.second" });
    await flushAndSettle(h);
    daemon.down(false);
    // ACT
    log.info("back", { operation: "test.back" });
    await flushAndSettle(h);
    // ASSERT
    expect([daemon.accepted.map((r) => r.operation), h.logger.heldCount()]).toEqual([
      ["test.back", "test.first", "test.second"],
      0,
    ]);
  });

  it("holds a resent record again when the daemon rejects it again", async () => {
    // ARRANGE: the daemon answers one call, then goes again.
    const daemon = outage();
    let answers = 0;
    const h = install(async (record) => {
      if (record.operation === "test.back" && answers === 0) {
        answers += 1;
        return "accepted";
      }
      return daemon.sink(record);
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("while down", { operation: "test.held" });
    await flushAndSettle(h);
    // ACT
    log.info("back", { operation: "test.back" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.logger.heldCount()).toBe(1);
  });

  it("drops the oldest held record past the bound, and names the count in a warn once the daemon answers", async () => {
    // ARRANGE
    const daemon = outage();
    const h = install(daemon.sink);
    vi.spyOn(console, "error").mockImplementation(() => {});
    for (let i = 0; i <= CLIENT_LOG_HELD_BOUND; i += 1) {
      log.error("while down {i}", { operation: `test.held-${String(i)}` }, { i });
      await flushAndSettle(h);
    }
    daemon.down(false);
    // ACT
    log.error("back", { operation: "test.back" });
    await flushAndSettle(h);
    // ASSERT
    const warn = daemon.accepted.find((r) => r.operation === HELD_DROPPED_OPERATION);
    expect([
      warn?.level.case,
      warn?.context?.dropped,
      daemon.accepted.some((r) => r.operation === "test.held-0"),
      daemon.accepted.some((r) => r.operation === "test.held-1"),
    ]).toEqual(["warn", 1, false, true]);
  });

  it("announces each outage once on the console", async () => {
    // ARRANGE
    const daemon = outage();
    const h = install(daemon.sink);
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    log.info("first outage", { operation: "test.op" });
    await flushAndSettle(h);
    daemon.down(false);
    log.info("back", { operation: "test.op" });
    await flushAndSettle(h);
    daemon.down(true);
    // ACT
    log.info("second outage", { operation: "test.op" });
    log.info("still down", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect(spy).toHaveBeenCalledTimes(2);
  });

  it("drops what is held once the daemon no longer knows the workspace", async () => {
    // ARRANGE
    let down = true;
    const h = install(async () => {
      if (down) throw new Error("unavailable");
      return "workspace_departed";
    });
    vi.spyOn(console, "error").mockImplementation(() => {});
    vi.spyOn(console, "debug").mockImplementation(() => {});
    log.info("while down", { operation: "test.held" });
    await flushAndSettle(h);
    down = false;
    // ACT
    log.info("departed", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect([h.logger.heldCount(), h.logger.workspaceDeparted()]).toEqual([0, true]);
  });
});

describe("the installation invariant", () => {
  it("refuses to log before a logger is installed", () => {
    resetLoggingForTests();
    expect(() => log.info("hello", { operation: "test.op" })).toThrow(/not installed/);
  });

  it("refuses a context that overrides the operation", () => {
    install();
    expect(() =>
      log.info("hello", { operation: "a", context: { operation: "b" } }),
    ).toThrow(/operation/);
  });

  it("refuses a context that contradicts a bound identity", () => {
    install();
    bindLogContext({ workspace_id: "ws-1" });
    expect(() =>
      log.info("hello", { operation: "a", context: { workspace_id: "ws-2" } }),
    ).toThrow(/conflicts/);
  });

  it("refuses a record with no connection id", () => {
    setLogger(new ForwardingLogger(async () => "accepted", () => {}, {}, "debug"));
    // A reset clears the bound context to the harness default; unbind it.
    bindLogContext({ connection_id: "" });
    expect(() => log.info("hello", { operation: "a" })).toThrow(/connection_id/);
  });
});

describe("dedup", () => {
  it("suppresses a repeat of the same message under a key", async () => {
    const h = install();
    log.warn("same", { operation: "op", dedupKey: "k" });
    log.warn("same", { operation: "op", dedupKey: "k" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(1);
  });

  it("lets a DIFFERENT message through, because the condition changed", async () => {
    const h = install();
    log.warn("first", { operation: "op", dedupKey: "k" });
    log.warn("second", { operation: "op", dedupKey: "k" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(2);
  });

  it("re-arms the key when the caller observed recovery", async () => {
    const h = install();
    log.warn("same", { operation: "op", dedupKey: "k" });
    clearLogDedup("k");
    log.warn("same", { operation: "op", dedupKey: "k" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(2);
  });

  it("keeps keys independent", async () => {
    const h = install();
    log.warn("same", { operation: "op", dedupKey: "a" });
    log.warn("same", { operation: "op", dedupKey: "b" });
    await flushAndSettle(h);
    expect(h.sent).toHaveLength(2);
  });

  it("suppresses the console too, since the first line carried the evidence", () => {
    const h = install();
    log.warn("same", { operation: "op", dedupKey: "k" });
    log.warn("same", { operation: "op", dedupKey: "k" });
    expect(h.console).toHaveLength(1);
  });
});

describe("restampRecordIdentity", () => {
  it("stamps the identity bound right now", () => {
    install();
    bindLogContext({ agent_repl_session_id: "s-2" });
    expect(restampRecordIdentity({ agent_repl_session_id: "s-1" })).toMatchObject({
      agent_repl_session_id: "s-2",
    });
  });

  it("REMOVES the field when nothing is bound, rather than sending an empty one", () => {
    resetLoggingForTests();
    install();
    expect(restampRecordIdentity({ agent_repl_session_id: "s-1" })).not.toHaveProperty(
      "agent_repl_session_id",
    );
  });

  it("leaves the event's own evidence untouched", () => {
    install();
    bindLogContext({ agent_repl_session_id: "s-2" });
    expect(restampRecordIdentity({ rows: 3 })).toMatchObject({ rows: 3 });
  });

  it("restamps the vendor session identity too", () => {
    install();
    bindLogContext({ claude_session_id: "c-2" });
    expect(restampRecordIdentity({ claude_session_id: "c-1" })).toMatchObject({
      claude_session_id: "c-2",
    });
  });

  it("applies at SEND time, so a rotation does not send the retired id", async () => {
    // ARRANGE: the record is built, then the workspace's session rotates.
    const h = install();
    bindLogContext({ agent_repl_session_id: "s-1" });
    // ACT
    log.info("hello", { operation: "op" });
    bindLogContext({ agent_repl_session_id: "s-2" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent[0].context).toMatchObject({ agent_repl_session_id: "s-2" });
  });
});

describe("jsonSafe conversion", () => {
  it("renders an Error as its name and message, not as an empty object", async () => {
    const h = install();
    log.error("boom", { operation: "op", context: { cause: new TypeError("nope") } });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({
      cause: { name: "TypeError", message: "nope" },
    });
  });

  it("renders a bigint as a string, which a Struct can carry", async () => {
    const h = install();
    log.info("m", { operation: "op", context: { at_ms: 1_700_000_000_000n } });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({ at_ms: "1700000000000" });
  });

  it("marks a circular reference rather than throwing on it", async () => {
    const h = install();
    const cycle: Record<string, unknown> = {};
    cycle.self = cycle;
    log.info("m", { operation: "op", context: { cycle } });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({ cycle: { self: "[Circular]" } });
  });

  it("renders a non-finite number as a string, which JSON cannot carry", async () => {
    const h = install();
    log.info("m", { operation: "op", context: { ratio: Number.POSITIVE_INFINITY } });
    await flushAndSettle(h);
    expect(h.sent[0].context).toMatchObject({ ratio: "Infinity" });
  });
});

describe("pendingCount", () => {
  it("is zero once a record has been released", async () => {
    const h = install();
    log.info("hello", { operation: "op" });
    await flushAndSettle(h);
    expect(h.logger.pendingCount()).toBe(0);
  });

  it("buffers a non-error record behind the throttle's window", () => {
    const h = install();
    log.info("hello", { operation: "op" });
    expect(h.logger.pendingCount()).toBe(1);
  });

  it("flushes an ERROR immediately, since it is the record someone looks for", () => {
    const h = install();
    log.error("boom", { operation: "op" });
    expect(h.logger.pendingCount()).toBe(0);
  });

  it("releases the buffer on an explicit flush", () => {
    const h = install();
    log.info("hello", { operation: "op" });
    h.logger.flush();
    expect(h.logger.pendingCount()).toBe(0);
  });
});

describe("the default console, when no console function is injected", () => {
  it("routes an error record to console.error", () => {
    // ARRANGE
    const spy = vi.spyOn(console, "error").mockImplementation(() => {});
    const logger = new ForwardingLogger(async () => "accepted");
    // ACT
    logger.write(buildClientLogRecord(webappRecord("error")), '{"operation":"op"}');
    // ASSERT
    expect(spy).toHaveBeenCalledWith('{"operation":"op"}');
    spy.mockRestore();
  });

  it("routes a warn record to console.warn", () => {
    const spy = vi.spyOn(console, "warn").mockImplementation(() => {});
    const logger = new ForwardingLogger(async () => "accepted");
    logger.write(buildClientLogRecord(webappRecord("warn")), '{"operation":"op"}');
    expect(spy).toHaveBeenCalledWith('{"operation":"op"}');
    spy.mockRestore();
  });

  it("routes an info record to console.log, the level having no console of its own", () => {
    const spy = vi.spyOn(console, "log").mockImplementation(() => {});
    const logger = new ForwardingLogger(async () => "accepted");
    logger.write(buildClientLogRecord(webappRecord("info")), '{"operation":"op"}');
    expect(spy).toHaveBeenCalledWith('{"operation":"op"}');
    spy.mockRestore();
  });
});

describe("the level a record must carry", () => {
  it("refuses a level outside the four, rather than forwarding it", () => {
    // ARRANGE
    const h = install();
    // ACT / ASSERT
    expect(
      () => new ForwardingLogger(async () => "accepted", () => {}, {}, "trace" as ClientLogLevel),
    ).toThrow("webapp log record has invalid level trace");
    expect(h.sent).toHaveLength(0);
  });
});

describe("a departed workspace stops the forwarder", () => {
  it("sends nothing more once the daemon answers unknown_workspace", async () => {
    // ARRANGE
    const sent: ClientLogRecord[] = [];
    const h = install(async (record) => {
      sent.push(record);
      return "workspace_departed";
    });
    vi.spyOn(console, "debug").mockImplementation(() => {});
    log.info("the last one the daemon knew about", { operation: "test.op" });
    await flushAndSettle(h);
    // ACT
    log.info("after the workspace departed", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect(sent).toHaveLength(1);
  });

  it("records the queued lines it dropped, at debug", async () => {
    // ARRANGE
    const h = install(async () => "workspace_departed");
    const debug = vi.spyOn(console, "debug").mockImplementation(() => {});
    log.info("the record that draws the refusal", { operation: "test.op" });
    h.logger.flush();
    // These queue behind the refusal, which has not settled yet: they are what
    // the drop count is about.
    log.info("queued behind it", { operation: "test.op" });
    log.info("and another", { operation: "test.op" });
    // ACT
    for (let i = 0; i < 10; i += 1) await Promise.resolve();
    // ASSERT
    expect(debug).toHaveBeenCalledWith(expect.stringContaining("dropped 2 queued record(s)"));
  });

  it("keeps consoling every record after it has stopped forwarding", async () => {
    // ARRANGE
    const h = install(async () => "workspace_departed");
    vi.spyOn(console, "debug").mockImplementation(() => {});
    log.info("the record that draws the refusal", { operation: "test.op" });
    await flushAndSettle(h);
    // ACT
    log.info("after the workspace departed", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.console.filter(([, line]) => line.includes("after the workspace departed"))).toHaveLength(1);
  });

  it("leaves forwarding alone when the record is merely refused", async () => {
    // ARRANGE
    const sent: ClientLogRecord[] = [];
    const h = install(async (record) => {
      sent.push(record);
      return "accepted";
    });
    log.info("one", { operation: "test.op" });
    await flushAndSettle(h);
    // ACT
    log.info("two", { operation: "test.op" });
    await flushAndSettle(h);
    // ASSERT
    expect(sent).toHaveLength(2);
  });
});

describe("log sites", () => {
  it("forwards each record with its site's key, derived from the runtime, the logger's scope and the template", async () => {
    // ARRANGE
    const h = install();
    // ACT
    createLogger("webapp.test.sites").info("hello {name}", { operation: "test.site" }, { name: "world" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent.map((r) => [r.site, r.template])).toEqual([[siteKey("webapp", "webapp.test.sites", "hello {name}"), "hello {name}"]]);
  });

  it("renders the forwarded message from the template's named values", async () => {
    // ARRANGE
    const h = install();
    // ACT
    log.info("moved {rows} rows to {place}", { operation: "test.site" }, { rows: 3, place: "the tail" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent[0]?.message).toBe("moved 3 rows to the tail");
  });

  it("records each named value in the context under its placeholder's name", async () => {
    // ARRANGE
    const h = install();
    // ACT
    log.info("moved {rows} rows", { operation: "test.site", context: { other: "kept" } }, { rows: 3 });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent[0]?.context).toMatchObject({ rows: 3, other: "kept" });
  });

  it("writes a doubled brace as a literal brace", async () => {
    // ARRANGE
    const h = install();
    // ACT
    log.info("a {{literal}} beside {value}", { operation: "test.site" }, { value: "x" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent[0]?.message).toBe("a {literal} beside x");
  });

  it("keys one template differently under two scopes", async () => {
    // ARRANGE
    const h = install();
    // ACT
    createLogger("webapp.test.one").info("same words", { operation: "test.site" });
    createLogger("webapp.test.two").info("same words", { operation: "test.site" });
    await flushAndSettle(h);
    // ASSERT
    expect(new Set(h.sent.map((r) => r.site)).size).toBe(2);
  });

  it("refuses a named value the context already states", () => {
    // ARRANGE
    install();
    // ACT / ASSERT
    expect(() => log.info("moved {rows} rows", { operation: "test.site", context: { rows: 1 } }, { rows: 2 })).toThrow("already states");
  });

  it("refuses a scope that is not dotted lowercase segments", () => {
    // ACT / ASSERT
    expect(() => createLogger("NoDots")).toThrow("dotted lowercase");
  });

  it("writes a caller's message at the level a helper picks, keyed under the caller's scope", async () => {
    // ARRANGE
    const h = install();
    const message = createLogger("webapp.test.caller").message("the {thing} broke", { thing: "chain" });
    // ACT
    createLogger("webapp.test.helper").log("warn", message, { operation: "test.site" });
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent.map((r) => [r.level.case, r.message, r.site])).toEqual([
      ["warn", "the chain broke", siteKey("webapp", "webapp.test.caller", "the {thing} broke")],
    ]);
  });

  it("words a startup level decision as the shared level window does", async () => {
    // ARRANGE
    const h = install();
    // ACT
    logLevelSelection(selectLevel("debug", undefined, 1_000_000_000, "log_level"));
    await flushAndSettle(h);
    // ASSERT
    expect(h.sent.map((r) => [r.message, r.operation])).toEqual([
      ['log level "debug" ignored without AGENT_REPL_LOG_LEVEL_UNTIL; starting at info', "webapp.log.level-window"],
    ]);
  });

  it("shows a test's spy each call with its rendered message and named values", () => {
    // ARRANGE
    install();
    const info = spyLog("info");
    // ACT
    log.info("moved {rows} rows", { operation: "test.site" }, { rows: 3 });
    // ASSERT
    expect(info).toHaveBeenCalledWith("moved 3 rows", { operation: "test.site", context: { rows: 3 } });
  });

  it("refuses a template that is not a literal, at compile time", () => {
    // ARRANGE
    install();
    const built: string = `built ${String(1)}`;
    // ACT / ASSERT: the directive fails the typecheck if the call compiles.
    // @ts-expect-error a template must be a string literal
    expect(() => log.info(built, { operation: "test.site" })).not.toThrow();
  });

  it("refuses a template literal with a run-time value in it, at compile time", () => {
    // ARRANGE
    install();
    const rows = 3;
    // ACT / ASSERT
    // @ts-expect-error a value goes in a named placeholder, never in the template
    expect(() => log.info(`moved ${String(rows)} rows`, { operation: "test.site" })).not.toThrow();
  });

  it("refuses a named value the template does not name, at compile time", () => {
    // ARRANGE
    install();
    // ACT / ASSERT
    // @ts-expect-error the template names {rows}, not {count}
    expect(() => log.info("moved {rows} rows", { operation: "test.site" }, { count: 1 })).toThrow("names no {count}");
  });

  it("refuses a scope that is not a literal, at compile time", () => {
    // ARRANGE
    const scope: string = "webapp.test.dynamic";
    // ACT / ASSERT
    // @ts-expect-error a scope must be a string literal
    expect(() => createLogger(scope)).not.toThrow();
  });
});
