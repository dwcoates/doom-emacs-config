import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { logRecordsSince, logSinkMark } from "./log-records.js";
import { containing } from "./expect-shapes.js";
import { writeSync } from "node:fs";

const priorVerbose = process.env.AGENT_REPL_LOG_VERBOSE;
const priorLevel = process.env.AGENT_REPL_LOG_LEVEL;
const priorUntil = process.env.AGENT_REPL_LOG_LEVEL_UNTIL;
const mockedWriteSync = vi.mocked(writeSync);

async function freshLog() {
  vi.resetModules();
  return import("../src/log.js");
}

/** One emergency-stderr line, parsed as the record it is. */
function record(line: string): { message?: string } {
  return JSON.parse(line) as { message?: string };
}

describe("shim runtime logging", () => {
  beforeEach(() => {
    process.env.AGENT_REPL_LOG_LEVEL = "debug";
    // A level other than info is a window (proto/vocab/log-level-window.json).
    process.env.AGENT_REPL_LOG_LEVEL_UNTIL = String(Math.floor(Date.now() / 1000) + 300);
    mockedWriteSync.mockReset();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => args[3] as number));
  });
  afterEach(() => {
    vi.restoreAllMocks();
    if (priorVerbose === undefined) delete process.env.AGENT_REPL_LOG_VERBOSE;
    else process.env.AGENT_REPL_LOG_VERBOSE = priorVerbose;
    if (priorLevel === undefined) delete process.env.AGENT_REPL_LOG_LEVEL;
    else process.env.AGENT_REPL_LOG_LEVEL = priorLevel;
    if (priorUntil === undefined) delete process.env.AGENT_REPL_LOG_LEVEL_UNTIL;
    else process.env.AGENT_REPL_LOG_LEVEL_UNTIL = priorUntil;
  });

  async function configured() {
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" });
    return log;
  }

  function stderr(): string[] {
    const records: string[] = [];
    vi.spyOn(process.stderr, "write").mockImplementation((record) => { records.push(String(record)); return true; });
    return records;
  }

  it("writes one byte-accurate JSONL record to inherited fd 3 and echoes normal records", async () => {
    const log = await configured();
    const terminal = stderr();
    log.bindLog({ component: "shim-test", operation: "shim.test.persist" }).debug({ request_id: "request-1" }, "store write accepted");
    expect(mockedWriteSync).toHaveBeenCalledWith(3, expect.any(Buffer), 0, expect.any(Number));
    expect(terminal).toHaveLength(1);
    expect(logRecordsSince(0)[0]).toMatchObject({ workspace_dir: "/canonical/workspace", workspace_id: "00000000000000dd", agent_repl_session_id: "agent-session-1", request_id: "request-1" });
  });

  it.each([
    ["debug", ["debug", "info", "warn", "error"]],
    ["info", ["info", "warn", "error"]],
    ["warn", ["warn", "error"]],
    ["error", ["error"]],
  ] as const)("persists and mirrors only %s-or-higher records", async (minimum, expected) => {
    // Arrange.
    process.env.AGENT_REPL_LOG_LEVEL = minimum;
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" });
    const terminal = stderr();
    const logger = log.bindLog({ operation: "shim.test.threshold" });

    // Act.
    logger.debug({}, "debug");
    logger.info({}, "info");
    logger.warn({}, "warn");
    logger.error({}, "error");

    // Assert.
    expect(logRecordsSince(0).map((record) => record.level)).toEqual(expected);
    expect(terminal.map((line) => JSON.parse(line) as { level: string }).map((record) => record.level)).toEqual(expected);
  });

  it("uses info as the persistence and mirror threshold when AGENT_REPL_LOG_LEVEL is absent", async () => {
    // Arrange.
    delete process.env.AGENT_REPL_LOG_LEVEL;
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" });
    const terminal = stderr();
    const logger = log.bindLog({ operation: "shim.test.default-threshold" });

    // Act.
    logger.debug({}, "debug");
    logger.info({}, "info");

    // Assert.
    expect(logRecordsSince(0).map((record) => record.level)).toEqual(["info"]);
    expect(terminal).toHaveLength(1);
  });

  describe("the level window", () => {
    const configureAt = async (clock: { at: number }) => {
      const log = await freshLog();
      log.configureLog({
        fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1",
        now: () => clock.at,
      });
      return log;
    };

    it("starts a debug level with no window at info and says so at info", async () => {
      // Arrange.
      delete process.env.AGENT_REPL_LOG_LEVEL_UNTIL;
      const clock = { at: 1_000_000_000 };

      // Act.
      const log = await configureAt(clock);
      log.bindLog({ operation: "shim.test.leftover" }).debug({}, "dropped");

      // Assert.
      const records = logRecordsSince(0);
      expect(records.map((r) => r.operation)).toEqual(["shim.logging.level-window"]);
      expect(records[0]).toMatchObject({ level: "info", context: { outcome: "no_expiry" } });
    });

    it("admits debug records before the window ends", async () => {
      // Arrange.
      const clock = { at: 1_000_000_000 };
      process.env.AGENT_REPL_LOG_LEVEL_UNTIL = String(1_000_000 + 300);
      const log = await configureAt(clock);
      const mark = logSinkMark();
      clock.at = 1_000_299_000;

      // Act.
      log.bindLog({ operation: "shim.test.inside" }).debug({}, "inside");

      // Assert.
      expect(logRecordsSince(mark).map((r) => r.level)).toEqual(["debug"]);
    });

    it("reverts to info when the window ends and records the revert at info", async () => {
      // Arrange.
      const clock = { at: 1_000_000_000 };
      process.env.AGENT_REPL_LOG_LEVEL_UNTIL = String(1_000_000 + 300);
      const log = await configureAt(clock);
      const mark = logSinkMark();
      clock.at = 1_000_300_000;

      // Act.
      log.bindLog({ operation: "shim.test.after" }).debug({}, "after");

      // Assert.
      const records = logRecordsSince(mark);
      expect(records.map((r) => r.operation)).toEqual(["shim.logging.level-window"]);
      expect(records[0]).toMatchObject({ level: "info", context: { outcome: "window_ended", from_level: "debug" } });
    });

    it("refuses a malformed window without configuring the sink", async () => {
      // Arrange.
      process.env.AGENT_REPL_LOG_LEVEL_UNTIL = "soon";
      const log = await freshLog();

      // Act + Assert.
      expect(() => log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" })).toThrow(
        /AGENT_REPL_LOG_LEVEL_UNTIL must be a Unix second/,
      );
      expect(mockedWriteSync).not.toHaveBeenCalled();
    });
  });

  it.each(["", "trace", "INFO"])("rejects invalid AGENT_REPL_LOG_LEVEL value %j without configuring the sink", async (value) => {
    // Arrange.
    process.env.AGENT_REPL_LOG_LEVEL = value;
    const log = await freshLog();

    // Act + Assert.
    expect(() => log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" })).toThrow(
      /AGENT_REPL_LOG_LEVEL must be one of debug\|info\|warn\|error/,
    );
    expect(mockedWriteSync).not.toHaveBeenCalled();
  });

  it("refuses an empty request id rather than stamping a blank field", async () => {
    const log = await configured();
    expect(() => log.setRequestId("")).toThrow(/request id is required/);
  });

  it("states the daemon's workspace id without resolving cwd, and propagates learned Claude identity", async () => {
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/workspace/link-is-intentional", workspaceId: "00000000000000dd", agentReplSessionId: "a" });
    const logger = log.bindLog({ operation: "shim.test.identity" });
    logger.debug({}, "before");
    log.setClaudeSessionId("claude-42");
    logger.debug({}, "after");
    expect(logRecordsSince(0)[0]).toMatchObject({ workspace_id: "00000000000000dd" });
    expect(logRecordsSince(0)[1]).toMatchObject({ claude_session_id: "claude-42" });
  });

  // WHAT THE DAEMON CALLS THIS WORKSPACE is the only thing `workspace_id`
  // carries: `bin/logs.sh --workspace` and the realtest harvest group by it,
  // and the shim's md5 prefix of the cwd grouped its records under a workspace
  // the rest of the fleet never wrote to.
  it("never derives the workspace id from the workspace directory", async () => {
    // Arrange.
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/workspace/link-is-intentional", workspaceId: "0100059cb65649bc", agentReplSessionId: "a" });

    // Act.
    log.bindLog({ operation: "shim.test.identity" }).debug({}, "one record");

    // Assert: "b3d05752" is the md5 prefix of that cwd, and is not the answer.
    expect(logRecordsSince(0)[0]).toMatchObject({ workspace_id: "0100059cb65649bc" });
  });

  // NOTHING IS LOST BY THE MOVE: the md5 prefix is what names the workspace
  // LOCK FILE, so it is the only thing joining a record to that file on disk.
  it("keeps its own md5 workspace key as a context field", async () => {
    // Arrange.
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/workspace/link-is-intentional", workspaceId: "0100059cb65649bc", agentReplSessionId: "a" });

    // Act.
    log.bindLog({ operation: "shim.test.identity" }).debug({}, "one record");

    // Assert.
    expect(logRecordsSince(0)[0].context.shim_workspace_hash).toBe(
      "b3d05752",
    );
  });

  it("persists verbose once and gates only terminal visibility", async () => {
    const log = await configured();
    const terminal = stderr();
    const logger = log.bindLog({ operation: "shim.test.verbose" });
    logger.logVerbose({}, "hidden");
    process.env.AGENT_REPL_LOG_VERBOSE = "1";
    logger.logVerbose({}, "shown");
    expect(mockedWriteSync).toHaveBeenCalledTimes(2);
    expect(terminal).toHaveLength(1);
  });

  it("subjects verbose-class records to the debug level threshold", async () => {
    // Arrange.
    process.env.AGENT_REPL_LOG_LEVEL = "info";
    process.env.AGENT_REPL_LOG_VERBOSE = "1";
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" });
    const terminal = stderr();

    // Act.
    log.bindLog({ operation: "shim.test.verbose-threshold" }).logVerbose({}, "below threshold");

    // Assert.
    expect(logRecordsSince(0)).toEqual([]);
    expect(terminal).toEqual([]);
  });

  it("writes multibyte JSONL with byte-accurate short-write offsets", async () => {
    const log = await configured();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => Math.min(5, args[3] as number)));
    log.bindLog({ operation: "shim.test.unicode" }).debug({}, "snowman ☃ and rocket 🚀");
    const calls = mockedWriteSync.mock.calls as unknown as Array<[number, Buffer, number, number]>;
    expect(calls.length).toBeGreaterThan(1);
    expect(calls.every(([, bytes, offset, length]) => bytes.subarray(offset, offset + length).length === length)).toBe(true);
    const persistedBytes = Buffer.concat(calls.map(([, bytes, offset, length]) => bytes.subarray(offset, offset + Math.min(5, length))));
    expect(JSON.parse(persistedBytes.toString("utf8"))).toMatchObject({ message: "snowman ☃ and rocket 🚀" });
  });

  it.each([
    [{ fd: -1, cwd: "/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent" }, "fd"],
    [{ fd: 3, cwd: "", workspaceId: "00000000000000dd", agentReplSessionId: "agent" }, "cwd"],
    [{ fd: 3, cwd: "/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "" }, "session id"],
    // A WORKSPACE RECORD OWES A WORKSPACE ID, so an absent one is a refusal
    // rather than a record filed under nothing.
    [{ fd: 3, cwd: "/workspace", workspaceId: "", agentReplSessionId: "agent" }, "workspace id"],
  ])("rejects invalid logger configuration %o without sink mutation", async (config, expected) => {
    const log = await freshLog();
    expect(() => log.configureLog(config)).toThrow(expected);
    expect(mockedWriteSync).not.toHaveBeenCalled();
  });

  it("rejects a second logger configuration without replacing the original sink", async () => {
    const log = await configured();
    expect(() => log.configureLog({ fd: 4, cwd: "/other", workspaceId: "00000000000000dd", agentReplSessionId: "other" })).toThrow("already been configured");
    log.bindLog({ operation: "shim.test.immutable" }).debug({}, "first sink remains");
    expect(mockedWriteSync).toHaveBeenCalledWith(3, expect.any(Buffer), 0, expect.any(Number));
  });

  it.each([
    [{ level: "trace" }, "selected by its logger method"],
    [{ request_id: 9 }, "request_id"],
  ])("rejects malformed record fields without partial emission", async (fields, expected) => {
    const log = await configured();
    const terminal = stderr();
    expect(() => log.bindLog({ operation: "shim.test.invalid" }).debug(fields, "nope")).toThrow(expected);
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(terminal).toEqual([]);
  });

  it("names an object level by its shape rather than as [object Object]", async () => {
    // A level is `unknown` on the way in, and the refusal exists to say WHICH
    // value was refused. `String()` renders every object identically, which is
    // the one answer that tells the reader nothing.
    const log = await configured();
    expect(() => log.bindLog({ operation: "shim.test.shaped" }).info({ level: { want: "trace" } }, "nope")).toThrow(
      '{"want":"trace"}',
    );
  });

  it("serializes Error and circular evidence in one valid JSONL record", async () => {
    const log = await configured();
    const circular: Record<string, unknown> = { count: 9n };
    circular.self = circular;
    log.bindLog({ operation: "shim.test.serialize" }).debug({ cause: new Error("cannot connect"), circular }, "failed");
    expect(logRecordsSince(0)[0]).toMatchObject({ context: { cause: { name: "Error", message: "cannot connect" }, circular: { count: "9", self: "[Circular]" } } });
  });

  it("fails without partial normal emission when unconfigured or malformed", async () => {
    const log = await freshLog();
    const terminal = stderr();
    expect(() => log.bindLog({ operation: "shim.test.unconfigured" }).debug({}, "nope")).toThrow("not configured");
    log.configureLog({ fd: 3, cwd: "/canonical", workspaceId: "00000000000000dd", agentReplSessionId: "a" });
    expect(() => log.bindLog({}).debug({}, "nope")).toThrow("operation");
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(terminal).toEqual([]);
  });

  it("uses emergency stderr when the sink errors or makes zero progress", async () => {
    const log = await configured();
    const terminal = stderr();
    // NOT FATAL (the EPIPE incident, 2026-08-10): a shim outlives its daemon by
    // design, so a process that died because its log line could not be written
    // would take a live turn with it. The loss is announced ONCE on the
    // emergency path and then stated as a standing degraded window; repeating
    // it per record would only bury the announcement.
    mockedWriteSync.mockImplementation(() => 0);
    log.bindLog({ operation: "shim.test.zero" }).debug({}, "nope");
    expect(JSON.parse(terminal[0])).toMatchObject({
      runtime: "shim", level: "error", operation: "shim.logging.emergency",
      workspace_dir: "/canonical/workspace",
    });
    const writesAfterFailure = mockedWriteSync.mock.calls.length;
    log.bindLog({ operation: "shim.test.poisoned" }).debug({}, "again");
    expect(mockedWriteSync).toHaveBeenCalledTimes(writesAfterFailure);
    expect(terminal).toHaveLength(1);
  });

  it("uses emergency stderr when the sink throws or over-reports bytes", async () => {
    const log = await configured();
    const terminal = stderr();
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }));
    log.bindLog({ operation: "shim.test.throw" }).debug({}, "nope");
    expect(record(terminal[0]).message).toContain("bad fd");
    vi.clearAllMocks();
    const overLog = await configured();
    const overTerminal = stderr();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => (args[3] as number) + 1));
    overLog.bindLog({ operation: "shim.test.over" }).debug({}, "nope");
    expect(record(overTerminal[0]).message).toContain("invalid write length");
  });

  it("reports fatal errors canonically after configuration and through bootstrap stderr before it", async () => {
    const bootstrapLog = await freshLog();
    // src/fatal.js, NOT src/main.js: the fatal reporter is deliberately off the
    // wiring graph, so this test pulls in one small module rather than the whole
    // shim (engine, store, service, sdk) on every fresh module registry.
    const { reportFatal: bootstrapFatal } = await import("../src/fatal.js");
    const bootstrapTerminal = stderr();
    bootstrapFatal(new Error("bootstrap"));
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(JSON.parse(bootstrapTerminal[0])).toMatchObject({
      runtime: "shim", level: "error", operation: "shim.logging.emergency",
    });
    expect(record(bootstrapTerminal[0]).message).toContain("bootstrap");
    vi.clearAllMocks();
    bootstrapLog.configureLog({ fd: 3, cwd: "/canonical/workspace", workspaceId: "00000000000000dd", agentReplSessionId: "agent-session-1" });
    const configuredTerminal = stderr();
    bootstrapFatal(new Error("configured"));
    expect(logRecordsSince(0)[0]).toMatchObject({
      level: "error",
      operation: "shim.main.fatal",
      context: containing({
        cause_class: "unrecoverable_entrypoint_failure",
        cause_type: "Error",
        exit_outcome: "process_exit_1",
        cause: containing({ name: "Error", message: "configured" }),
      }),
    });
    expect(configuredTerminal).toHaveLength(1);
  });

  it("survives a broken stderr pipe instead of dying with the daemon that owned it", async () => {
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw Object.assign(new Error("write EPIPE"), { code: "EPIPE" }); });
    expect(() => log.bindLog({ operation: "shim.test.epipe" }).debug({}, "after the daemon exited")).not.toThrow();
  });

  it("records the retirement of the stderr mirror on the durable sink", async () => {
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "after the daemon exited");
    expect(logRecordsSince(0).map((record) => record.operation)).toContain("shim.logging.stderr-mirror");
  });

  it("records the retirement at info, because a shim outliving its daemon is the design", async () => {
    // Arrange: a shim whose daemon has gone, which is every daemon bounce.
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });

    // Act.
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "after the daemon exited");

    // Assert: the record stands, and it is not a warning about anything.
    expect(
      logRecordsSince(0).find((record) => record.operation === "shim.logging.stderr-mirror"),
    ).toMatchObject({ level: "info" });
  });

  it("keeps logging durably after the stderr mirror is retired", async () => {
    const log = await configured();
    const terminal = vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "first");
    terminal.mockClear();
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "second");
    expect(terminal).not.toHaveBeenCalled();
    expect(logRecordsSince(0).at(-1)).toMatchObject({ message: "second" });
  });

  it("retires the mirror on an asynchronous stderr error rather than letting it go uncaught", async () => {
    const log = await configured();
    process.stderr.emit("error", new Error("write EPIPE"));
    const terminal = vi.spyOn(process.stderr, "write").mockImplementation(() => true);
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "after the async error");
    expect(terminal).not.toHaveBeenCalled();
  });

  it("still surfaces a durable-sink failure after the stderr mirror is retired", async () => {
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "retire the mirror");
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }));
    const terminal = stderr();
    log.bindLog({ operation: "shim.test.epipe" }).debug({}, "durable failure");

    // The mirror is gone and the durable sink has just died, so the emergency
    // path is the only channel left -- and it still carries the cause.
    expect(record(terminal[0]).message).toContain("bad fd");
  });

  it("tells a registered observer that the sink is poisoned", async () => {
    // WatchSession is where a lost log becomes visible to anyone outside the
    // process; without an observer the loss stayed inside the dead logger.
    const log = await configured();
    stderr();
    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }));

    log.bindLog({ operation: "shim.test.observed" }).debug({}, "durable failure");

    expect(causes).toEqual(["bad fd"]);
  });

  it("tells an observer that registers AFTER the poisoning, since it is a standing condition", async () => {
    const log = await configured();
    stderr();
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }));
    log.bindLog({ operation: "shim.test.late" }).debug({}, "durable failure");

    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));

    expect(causes).toEqual(["bad fd"]);
  });

  it("announces the poisoning exactly once, however many records are lost after it", async () => {
    const log = await configured();
    stderr();
    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }));

    log.bindLog({ operation: "shim.test.once" }).debug({}, "one");
    log.bindLog({ operation: "shim.test.once" }).debug({}, "two");

    expect(causes).toHaveLength(1);
  });

  it("refuses an empty Claude session id rather than stamping a blank identity", async () => {
    const log = await configured();
    expect(() => log.setClaudeSessionId("")).toThrow(/claude session id is required/);
  });

  it("refuses a Claude session id that is not a string at all", async () => {
    const log = await configured();
    expect(() => log.setClaudeSessionId(7 as unknown as string)).toThrow(/claude session id is required/);
  });

  it("refuses a request id that is not a string at all", async () => {
    const log = await configured();
    expect(() => log.setRequestId(7 as unknown as string)).toThrow(/request id is required/);
  });

  it("clearing the request id before the logger is configured is a no-op, not a crash", async () => {
    // The turn's teardown must not be the thing that kills a shim whose
    // logger never came up.
    const log = await freshLog();
    expect(() => log.clearRequestId()).not.toThrow();
  });

  it("carries a non-finite number as its stringified form rather than dropping the field", async () => {
    const log = await configured();
    log.bindLog({ operation: "shim.test.jsonsafe" }).debug({ ratio: Number.POSITIVE_INFINITY }, "budget");
    expect(logRecordsSince(0)[0].context).toMatchObject({ ratio: "Infinity" });
  });

  it("carries a bigint as a decimal string, which JSON has no other way to hold", async () => {
    const log = await configured();
    log.bindLog({ operation: "shim.test.jsonsafe" }).debug({ offset: 9007199254740993n }, "offset");
    expect(logRecordsSince(0)[0].context).toMatchObject({ offset: "9007199254740993" });
  });

  it("carries a function-valued field as its stringified form", async () => {
    const log = await configured();
    log.bindLog({ operation: "shim.test.jsonsafe" }).debug({ hook: function named() {} }, "hook");
    expect(String(logRecordsSince(0)[0].context.hook)).toContain("named");
  });

  it("carries a symbol-valued field as its stringified form", async () => {
    const log = await configured();
    log.bindLog({ operation: "shim.test.jsonsafe" }).debug({ tag: Symbol("marker") }, "tag");
    expect(logRecordsSince(0)[0].context).toMatchObject({ tag: "Symbol(marker)" });
  });

  it("poisons the sink when the durable write fails WHILE recording the mirror's retirement", async () => {
    // The retirement record is itself a durable write, and its failure is the
    // one case that cannot be reported through the channel it is about.
    const log = await configured();
    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));
    const terminal = stderr();
    terminal.length = 0;
    vi.spyOn(process.stderr, "write").mockImplementation((record) => {
      terminal.push(String(record));
      throw new Error("write EPIPE");
    });
    let durableWrites = 0;
    mockedWriteSync.mockImplementation(((...args: unknown[]) => {
      durableWrites += 1;
      if (durableWrites > 1) throw new Error("bad fd");
      return args[3] as number;
    }));

    log.bindLog({ operation: "shim.test.retire-poison" }).debug({}, "the record that retires the mirror");

    expect(causes).toEqual(["bad fd"]);
  });

  it("poisons the sink with a stringified cause when the durable write throws a non-Error", async () => {
    const log = await configured();
    stderr();
    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));
    mockedWriteSync.mockImplementation(() => { throw "EBADF"; });

    log.bindLog({ operation: "shim.test.nonerror" }).debug({}, "durable failure");

    expect(causes).toEqual(["EBADF"]);
  });

  it("retires the mirror when the stderr write throws a non-Error", async () => {
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw "EPIPE"; });
    log.bindLog({ operation: "shim.test.nonerror-mirror" }).debug({}, "retire me");
    expect(logRecordsSince(0).map((record) => record.operation)).toContain("shim.logging.stderr-mirror");
  });

  it("stamps the emergency record with the Claude identity once the SDK has revealed it", async () => {
    const log = await configured();
    log.setClaudeSessionId("claude-77");
    const terminal = stderr();
    log.emergencyStderr("the sink is gone");
    expect(JSON.parse(terminal[0])).toMatchObject({
      operation: "shim.logging.emergency",
      claude_session_id: "claude-77",
    });
  });

  it("POISONS the sink when the retirement record's own write throws a non-Error", async () => {
    // Arrange: the mirror dies, and the durable write that records its death
    // fails with a bare string rather than an Error.
    const log = await configured();
    const causes: string[] = [];
    log.onLogSinkPoisoned((cause) => causes.push(cause.message));
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });
    mockedWriteSync
      .mockImplementationOnce(((...args: unknown[]) => args[3] as number))
      .mockImplementation(() => { throw "the inherited fd is gone"; });

    // Act.
    log.bindLog({ operation: "shim.test.retire-nonerror" }).debug({}, "a record");

    // Assert. The bare string is surfaced as the poisoning's cause, not lost.
    expect(causes).toEqual(["the inherited fd is gone"]);
  });

  it("retires the mirror when the emergency channel itself throws a non-Error", async () => {
    // Arrange.
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw "the terminal is gone"; });

    // Act.
    log.emergencyStderr("the sink is gone");

    // Assert. The mirror is retired, so ordinary records no longer echo there.
    const terminal = stderr();
    log.bindLog({ operation: "shim.test.after-emergency" }).debug({}, "after");
    expect(terminal).toEqual([]);
  });

  it("does not let the emergency channel's own failure escape the caller", async () => {
    // This is the escape hatch used while the real error is on its way out;
    // letting a dead pipe throw over it would replace a surfaced error with a
    // process death.
    const log = await configured();
    vi.spyOn(process.stderr, "write").mockImplementation(() => { throw new Error("write EPIPE"); });
    expect(() => log.emergencyStderr("the sink is gone")).not.toThrow();
  });
});
