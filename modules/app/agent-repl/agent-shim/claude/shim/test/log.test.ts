import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { writeSync } from "node:fs";

const priorVerbose = process.env.AGENT_REPL_LOG_VERBOSE;
const mockedWriteSync = vi.mocked(writeSync);

async function freshLog() {
  vi.resetModules();
  return import("../src/uds/log.js");
}

function persisted(): Record<string, unknown>[] {
  const calls = mockedWriteSync.mock.calls as unknown as Array<[number, Buffer, number, number]>;
  return calls.map(([, bytes, offset, length]) =>
    JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as Record<string, unknown>,
  );
}

describe("shim runtime logging", () => {
  beforeEach(() => {
    mockedWriteSync.mockReset();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => args[3] as number) as typeof writeSync);
  });
  afterEach(() => {
    vi.restoreAllMocks();
    if (priorVerbose === undefined) delete process.env.AGENT_REPL_LOG_VERBOSE;
    else process.env.AGENT_REPL_LOG_VERBOSE = priorVerbose;
  });

  async function configured() {
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/canonical/workspace", agentReplSessionId: "agent-session-1" });
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
    log.bindLog({ component: "shim-test", operation: "shim.test.persist" }).log({ request_id: "request-1" }, "store write accepted");
    expect(mockedWriteSync).toHaveBeenCalledWith(3, expect.any(Buffer), 0, expect.any(Number));
    expect(terminal).toHaveLength(1);
    expect(persisted()[0]).toMatchObject({ workspace_dir: "/canonical/workspace", workspace_id: "cdb4ebd1", agent_repl_session_id: "agent-session-1", request_id: "request-1" });
  });

  it("derives identity without resolving cwd and propagates learned Claude identity", async () => {
    const log = await freshLog();
    log.configureLog({ fd: 3, cwd: "/workspace/link-is-intentional", agentReplSessionId: "a" });
    const logger = log.bindLog({ operation: "shim.test.identity" });
    logger.log({}, "before");
    log.setClaudeSessionId("claude-42");
    logger.log({}, "after");
    expect(persisted()[0]).toMatchObject({ workspace_id: "b3d05752" });
    expect(persisted()[1]).toMatchObject({ claude_session_id: "claude-42" });
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

  it("writes multibyte JSONL with byte-accurate short-write offsets", async () => {
    const log = await configured();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => Math.min(5, args[3] as number)) as typeof writeSync);
    log.bindLog({ operation: "shim.test.unicode" }).log({}, "snowman ☃ and rocket 🚀");
    const calls = mockedWriteSync.mock.calls as unknown as Array<[number, Buffer, number, number]>;
    expect(calls.length).toBeGreaterThan(1);
    expect(calls.every(([, bytes, offset, length]) => bytes.subarray(offset, offset + length).length === length)).toBe(true);
    const persistedBytes = Buffer.concat(calls.map(([, bytes, offset, length]) => bytes.subarray(offset, offset + Math.min(5, length))));
    expect(JSON.parse(persistedBytes.toString("utf8"))).toMatchObject({ message: "snowman ☃ and rocket 🚀" });
  });

  it.each([
    [{ fd: -1, cwd: "/workspace", agentReplSessionId: "agent" }, "fd"],
    [{ fd: 3, cwd: "", agentReplSessionId: "agent" }, "cwd"],
    [{ fd: 3, cwd: "/workspace", agentReplSessionId: "" }, "session id"],
  ])("rejects invalid logger configuration %o without sink mutation", async (config, expected) => {
    const log = await freshLog();
    expect(() => log.configureLog(config)).toThrow(expected);
    expect(mockedWriteSync).not.toHaveBeenCalled();
  });

  it("rejects a second logger configuration without replacing the original sink", async () => {
    const log = await configured();
    expect(() => log.configureLog({ fd: 4, cwd: "/other", agentReplSessionId: "other" })).toThrow("already been configured");
    log.bindLog({ operation: "shim.test.immutable" }).log({}, "first sink remains");
    expect(mockedWriteSync).toHaveBeenCalledWith(3, expect.any(Buffer), 0, expect.any(Number));
  });

  it.each([
    [{ level: "trace" }, "invalid level"],
    [{ request_id: 9 }, "request_id"],
  ])("rejects malformed record fields without partial emission", async (fields, expected) => {
    const log = await configured();
    const terminal = stderr();
    expect(() => log.bindLog({ operation: "shim.test.invalid" }).log(fields, "nope")).toThrow(expected);
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(terminal).toEqual([]);
  });

  it("serializes Error and circular evidence in one valid JSONL record", async () => {
    const log = await configured();
    const circular: Record<string, unknown> = { count: 9n };
    circular.self = circular;
    log.bindLog({ operation: "shim.test.serialize" }).log({ cause: new Error("cannot connect"), circular }, "failed");
    expect(persisted()[0]).toMatchObject({ context: { cause: { name: "Error", message: "cannot connect" }, circular: { count: "9", self: "[Circular]" } } });
  });

  it("fails without partial normal emission when unconfigured or malformed", async () => {
    const log = await freshLog();
    const terminal = stderr();
    expect(() => log.bindLog({ operation: "shim.test.unconfigured" }).log({}, "nope")).toThrow("not configured");
    log.configureLog({ fd: 3, cwd: "/canonical", agentReplSessionId: "a" });
    expect(() => log.bindLog({}).log({}, "nope")).toThrow("operation");
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(terminal).toEqual([]);
  });

  it("uses emergency stderr when the sink errors or makes zero progress", async () => {
    const log = await configured();
    const terminal = stderr();
    mockedWriteSync.mockImplementation(() => 0);
    expect(() => log.bindLog({ operation: "shim.test.zero" }).log({}, "nope")).toThrow("made no progress");
    expect(JSON.parse(terminal[0]!)).toMatchObject({
      runtime: "shim", level: "error", operation: "shim.logging.emergency",
      workspace_dir: "/canonical/workspace",
    });
    const writesAfterFailure = mockedWriteSync.mock.calls.length;
    expect(() => log.bindLog({ operation: "shim.test.poisoned" }).log({}, "again")).toThrow("made no progress");
    expect(mockedWriteSync).toHaveBeenCalledTimes(writesAfterFailure);
    expect(JSON.parse(terminal[1]!)).toMatchObject({ operation: "shim.logging.emergency" });
  });

  it("uses emergency stderr when the sink throws or over-reports bytes", async () => {
    const log = await configured();
    const terminal = stderr();
    mockedWriteSync.mockImplementation((() => { throw new Error("bad fd"); }) as typeof writeSync);
    expect(() => log.bindLog({ operation: "shim.test.throw" }).log({}, "nope")).toThrow("bad fd");
    expect(JSON.parse(terminal[0]!).message).toContain("bad fd");
    vi.clearAllMocks();
    const overLog = await configured();
    const overTerminal = stderr();
    mockedWriteSync.mockImplementation(((...args: unknown[]) => (args[3] as number) + 1) as typeof writeSync);
    expect(() => overLog.bindLog({ operation: "shim.test.over" }).log({}, "nope")).toThrow("invalid write length");
    expect(JSON.parse(overTerminal[0]!).message).toContain("invalid write length");
  });

  it("reports fatal errors canonically after configuration and through bootstrap stderr before it", async () => {
    const bootstrapLog = await freshLog();
    const { reportFatal: bootstrapFatal } = await import("../src/main.js");
    const bootstrapTerminal = stderr();
    bootstrapFatal(new Error("bootstrap"));
    expect(mockedWriteSync).not.toHaveBeenCalled();
    expect(JSON.parse(bootstrapTerminal[0]!)).toMatchObject({
      runtime: "shim", level: "error", operation: "shim.logging.emergency",
    });
    expect(JSON.parse(bootstrapTerminal[0]!).message).toContain("bootstrap");
    vi.clearAllMocks();
    await bootstrapLog.configureLog({ fd: 3, cwd: "/canonical/workspace", agentReplSessionId: "agent-session-1" });
    const configuredTerminal = stderr();
    bootstrapFatal(new Error("configured"));
    expect(persisted()[0]).toMatchObject({ level: "error", operation: "shim.main.fatal" });
    expect(configuredTerminal).toHaveLength(1);
  });
});

/** RFC 3339, 24-hour clock, fixed-width microseconds, explicit numeric offset. */
const CANONICAL_TIMESTAMP = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}[+-]\d{2}:\d{2}$/;

describe("shim log timestamp", () => {
  it("renders the canonical fixed-width layout", async () => {
    // Arrange: a whole second, which toISOString would render with milliseconds only.
    const { logTimestamp } = await freshLog();
    const at = new Date(2026, 6, 28, 12, 34, 56, 0);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered).toMatch(CANONICAL_TIMESTAMP);
  });

  it("renders the local wall clock rather than UTC", async () => {
    // Arrange
    const { logTimestamp } = await freshLog();
    const at = new Date(2026, 6, 28, 12, 34, 56, 789);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered.slice(0, 23)).toBe("2026-07-28T12:34:56.789");
  });

  it("carries the local UTC offset instead of a Z suffix", async () => {
    // Arrange
    const { logTimestamp } = await freshLog();
    const at = new Date(2026, 6, 28, 12, 34, 56, 789);
    const offsetMinutes = -at.getTimezoneOffset();
    const sign = offsetMinutes < 0 ? "-" : "+";
    const absolute = Math.abs(offsetMinutes);
    const expected = `${sign}${String(Math.floor(absolute / 60)).padStart(2, "0")}:${String(absolute % 60).padStart(2, "0")}`;

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered.slice(-6)).toBe(expected);
  });

  it("pads microseconds because JavaScript instants resolve to milliseconds", async () => {
    // Arrange
    const { logTimestamp } = await freshLog();
    const at = new Date(2026, 6, 28, 12, 34, 56, 7);

    // Act
    const rendered = logTimestamp(at);

    // Assert
    expect(rendered.slice(19, 26)).toBe(".007000");
  });
});
