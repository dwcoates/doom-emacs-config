/** Canonical JSONL logging owned by the Claude shim runtime. */
import { createHash } from "node:crypto";
import { writeSync } from "node:fs";
import { logTimestamp } from "../../../../logging/ts/timestamp.js";

export type LogLevel = "debug" | "info" | "warn" | "error";
export type LogFields = Record<string, unknown>;

export interface ShimLogger {
  log(fields: LogFields, message: string): void;
  logVerbose(fields: LogFields, message: string): void;
  with(fields: LogFields): ShimLogger;
}

export interface ShimLogConfiguration {
  /** Already-open descriptor inherited from the daemon; this module never opens paths. */
  fd: number;
  /** Authoritative, already-canonical workspace path supplied by the daemon. */
  cwd: string;
  agentReplSessionId: string;
}

interface RuntimeContext {
  fd: number;
  workspace_dir: string;
  workspace_id: string;
  agent_repl_session_id: string;
  claude_session_id?: string;
  write: (fd: number, bytes: Buffer, offset: number, length: number) => number;
  poisoned?: Error;
}

// THE STDERR MIRROR IS A CONVENIENCE, AND IT USED TO BE A LIFELINE.
//
// stderr is a PIPE whose read end belongs to the daemon that spawned this shim.
// A shim is designed to outlive that daemon: it redials the socket and the next
// daemon reattaches to the turn still running. But every log line was mirrored
// to stderr with no failure handling, so the first line written after the
// daemon exited raised EPIPE on process.stderr — an error event with no
// listener, i.e. an uncaught exception — and the shim died. Silently, because
// the channel that would have reported it is the broken one.
//
// That killed EVERY shim on every daemon bounce (2026-08-10 19:41: five
// preserved shims, last durable line at the daemon's clean exit, all gone
// seconds later with no record), destroying the in-flight async work the
// preservation design exists to protect.
//
// So the mirror is now RETIRABLE: losing it is recorded once on the durable
// sink and never again attempted. The durable sink keeps its old contract —
// its failures poison the logger and are rethrown — because that one IS the
// record, and losing it must never pass unnoticed.
let stderrMirror: "live" | "retired" = "live";

interface ShimLogRecord {
  timestamp: string;
  runtime: "shim";
  level: LogLevel;
  verbosity: "normal" | "verbose";
  operation: string;
  message: string;
  context: Record<string, unknown>;
  pid: number;
  workspace_dir: string;
  workspace_id: string;
  agent_repl_session_id: string;
  claude_session_id?: string;
  request_id?: string;
}

let runtimeContext: RuntimeContext | undefined;
const RESERVED_FIELDS = new Set([
  "level", "operation", "workspace_dir", "workspace_id",
  "agent_repl_session_id", "claude_session_id", "request_id",
]);

function requireString(fields: LogFields, field: string): string {
  const value = fields[field];
  if (typeof value !== "string" || value.length === 0) throw new Error(`shim log record requires ${field}`);
  return value;
}

function requireContext(): RuntimeContext {
  if (runtimeContext === undefined) throw new Error("shim logger is not configured for a UDS workspace");
  return runtimeContext;
}

/** Configure the durable inherited sink exactly once for one shim process. */
export function configureLog(config: ShimLogConfiguration): void {
  if (!Number.isInteger(config.fd) || config.fd < 0) throw new Error("shim log fd must be a non-negative integer");
  if (typeof config.cwd !== "string" || config.cwd.length === 0) throw new Error("shim log cwd is required");
  if (typeof config.agentReplSessionId !== "string" || config.agentReplSessionId.length === 0) {
    throw new Error("shim agent-repl session id is required");
  }
  if (runtimeContext !== undefined) throw new Error("shim logger has already been configured");
  // Do not realpath this value: the daemon supplied the canonical cwd and owns symlink resolution.
  runtimeContext = {
    fd: config.fd,
    workspace_dir: config.cwd,
    workspace_id: createHash("md5").update(config.cwd).digest("hex").slice(0, 8),
    agent_repl_session_id: config.agentReplSessionId,
    write: (fd, bytes, offset, length) => writeSync(fd, bytes, offset, length),
  };
  stderrMirror = "live";
  // An asynchronous EPIPE (the daemon's read end closing between writes) lands
  // as an 'error' event, not as a throw from write(). Without this listener it
  // is an uncaught exception and the shim dies mid-turn.
  process.stderr.on("error", (err: Error) => retireStderrMirror(err));
}

/**
 * Stop mirroring to stderr, recording the loss durably exactly once.
 *
 * The durable write here is deliberately NOT emit(): emit mirrors, and a mirror
 * failure recursing into itself is how one broken pipe becomes a stack
 * overflow. A durable-sink failure while recording this poisons the logger, so
 * the next log call throws — the failure is surfaced, never swallowed.
 */
function retireStderrMirror(cause: Error): void {
  if (stderrMirror === "retired") return;
  stderrMirror = "retired";
  const runtime = runtimeContext;
  if (runtime === undefined || runtime.poisoned !== undefined) return;
  const record = buildRecord("normal", {
    level: "warn",
    operation: "shim.logging.stderr-mirror",
    cause: cause.message,
  }, "stderr mirror RETIRED — the daemon that owned this pipe is gone; this shim keeps running and keeps logging durably, because a shim outlives its daemon by design");
  try {
    writeDurable(runtime, Buffer.from(`${JSON.stringify(record)}\n`, "utf8"));
  } catch (err) {
    runtime.poisoned = err instanceof Error ? err : new Error(String(err));
  }
}

/** Record the vendor identity as soon as the SDK reveals it. */
export function setClaudeSessionId(claudeSessionId: string): void {
  if (typeof claudeSessionId !== "string" || claudeSessionId.length === 0) {
    throw new Error("shim claude session id is required");
  }
  requireContext().claude_session_id = claudeSessionId;
}

function logLevel(fields: LogFields, verbosity: ShimLogRecord["verbosity"]): LogLevel {
  const value = fields.level;
  if (value === undefined) return verbosity === "verbose" ? "debug" : "info";
  if (value === "debug" || value === "info" || value === "warn" || value === "error") return value;
  throw new Error(`shim log record has invalid level ${String(value)}`);
}

function jsonSafe(value: unknown, seen = new WeakSet<object>()): unknown {
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") return Number.isFinite(value) ? value : String(value);
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "undefined" || typeof value === "function" || typeof value === "symbol") return String(value);
  if (value instanceof Error) return { name: value.name, message: value.message, ...(value.stack === undefined ? {} : { stack: value.stack }) };
  if (typeof value !== "object") return String(value);
  if (seen.has(value)) return "[Circular]";
  seen.add(value);
  if (Array.isArray(value)) return value.map((entry) => jsonSafe(entry, seen));
  return Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, jsonSafe(entry, seen)]));
}

function buildRecord(verbosity: ShimLogRecord["verbosity"], fields: LogFields, message: string): ShimLogRecord {
  const runtime = requireContext();
  const operation = requireString(fields, "operation");
  const context: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(fields)) {
    if (!RESERVED_FIELDS.has(key) && value !== undefined) context[key] = jsonSafe(value);
  }
  const record: ShimLogRecord = {
    timestamp: logTimestamp(), runtime: "shim", level: logLevel(fields, verbosity), verbosity,
    operation, message, context, pid: process.pid,
    workspace_dir: runtime.workspace_dir, workspace_id: runtime.workspace_id,
    agent_repl_session_id: runtime.agent_repl_session_id,
    ...(runtime.claude_session_id === undefined ? {} : { claude_session_id: runtime.claude_session_id }),
  };
  if (fields.request_id !== undefined) record.request_id = requireString(fields, "request_id");
  if (fields.claude_session_id !== undefined) record.claude_session_id = requireString(fields, "claude_session_id");
  return record;
}

/** The only pre-logger/sink-failure escape hatch. It must never persist. */
export function emergencyStderr(message: string): void {
  const runtime = runtimeContext;
  if (stderrMirror === "retired") return;
  // A failure HERE is not the failure being reported: this is the escape hatch
  // used while the real error is on its way to the caller, and letting a dead
  // pipe throw over it would replace a surfaced error with a process death.
  // The real error is rethrown by every caller, so nothing is swallowed.
  try {
    process.stderr.write(`${JSON.stringify({
    timestamp: logTimestamp(),
    runtime: "shim",
    level: "error",
    verbosity: "normal",
    operation: "shim.logging.emergency",
    message,
    context: {},
    pid: process.pid,
    ...(runtime === undefined ? {} : {
      workspace_dir: runtime.workspace_dir,
      workspace_id: runtime.workspace_id,
      agent_repl_session_id: runtime.agent_repl_session_id,
      ...(runtime.claude_session_id === undefined ? {} : { claude_session_id: runtime.claude_session_id }),
    }),
    })}\n`);
  } catch (err) {
    retireStderrMirror(err instanceof Error ? err : new Error(String(err)));
  }
}

function emit(verbosity: ShimLogRecord["verbosity"], fields: LogFields, message: string): void {
  // Construct and serialize completely before either sink is touched: invalid records emit nowhere.
  const bytes = Buffer.from(`${JSON.stringify(buildRecord(verbosity, fields, message))}\n`, "utf8");
  const runtime = requireContext();
  if (runtime.poisoned !== undefined) {
    emergencyStderr(`shim log sink is poisoned: ${runtime.poisoned.message}`);
    throw runtime.poisoned;
  }
  try {
    writeDurable(runtime, bytes);
  } catch (err) {
    const failure = err instanceof Error ? err : new Error(String(err));
    runtime.poisoned = failure;
    emergencyStderr(`shim log sink failure: ${failure.message}`);
    throw failure;
  }
  if (stderrMirror === "retired") return;
  if (verbosity === "normal" || process.env.AGENT_REPL_LOG_VERBOSE === "1") {
    // A synchronous EPIPE from a dead daemon's pipe retires the mirror. It
    // must not reach the caller: the record IS written, and killing a shim
    // over a lost convenience copy is the incident this guard exists for.
    try {
      process.stderr.write(bytes);
    } catch (err) {
      retireStderrMirror(err instanceof Error ? err : new Error(String(err)));
    }
  }
}

/** Write one record to the durable inherited sink, short writes included. */
function writeDurable(runtime: RuntimeContext, bytes: Buffer): void {
  let offset = 0;
  while (offset < bytes.length) {
    const written = runtime.write(runtime.fd, bytes, offset, bytes.length - offset);
    if (!Number.isInteger(written) || written <= 0) throw new Error(`shim log sink made no progress after ${offset} bytes`);
    if (written > bytes.length - offset) throw new Error(`shim log sink reported invalid write length ${written}`);
    offset += written;
  }
}

class BoundShimLogger implements ShimLogger {
  constructor(private readonly fields: LogFields) {}
  log(fields: LogFields, message: string): void { emit("normal", { ...this.fields, ...fields }, message); }
  logVerbose(fields: LogFields, message: string): void { emit("verbose", { ...this.fields, ...fields }, message); }
  with(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...this.fields, ...fields }); }
}

/** Bind component and stable-operation context without mutating global state. */
export function bindLog(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...fields }); }
