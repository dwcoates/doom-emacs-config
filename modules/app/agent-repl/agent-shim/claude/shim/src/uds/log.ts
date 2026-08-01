/** Canonical JSONL logging owned by the Claude shim runtime. */
import { createHash } from "node:crypto";
import { writeSync } from "node:fs";

/**
 * The log timestamp representation shared by every agent-repl runtime:
 * RFC 3339 in the machine's local zone, on a 24-hour clock, with fixed-width
 * microseconds and an explicit numeric offset. Fixed width keeps records from
 * different runtimes lexically comparable. JavaScript instants resolve to
 * milliseconds, so the last three microsecond digits are always zero.
 */
export function logTimestamp(at: Date = new Date()): string {
  const pad = (value: number, width: number): string => String(value).padStart(width, "0");
  const offsetMinutes = -at.getTimezoneOffset();
  const sign = offsetMinutes < 0 ? "-" : "+";
  const offset = Math.abs(offsetMinutes);
  return (
    `${pad(at.getFullYear(), 4)}-${pad(at.getMonth() + 1, 2)}-${pad(at.getDate(), 2)}` +
    `T${pad(at.getHours(), 2)}:${pad(at.getMinutes(), 2)}:${pad(at.getSeconds(), 2)}` +
    `.${pad(at.getMilliseconds(), 3)}000` +
    `${sign}${pad(Math.floor(offset / 60), 2)}:${pad(offset % 60, 2)}`
  );
}

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
    let offset = 0;
    while (offset < bytes.length) {
      const written = runtime.write(runtime.fd, bytes, offset, bytes.length - offset);
      if (!Number.isInteger(written) || written <= 0) throw new Error(`shim log sink made no progress after ${offset} bytes`);
      if (written > bytes.length - offset) throw new Error(`shim log sink reported invalid write length ${written}`);
      offset += written;
    }
  } catch (err) {
    const failure = err instanceof Error ? err : new Error(String(err));
    runtime.poisoned = failure;
    emergencyStderr(`shim log sink failure: ${failure.message}`);
    throw failure;
  }
  if (verbosity === "normal" || process.env.AGENT_REPL_LOG_VERBOSE === "1") process.stderr.write(bytes);
}

class BoundShimLogger implements ShimLogger {
  constructor(private readonly fields: LogFields) {}
  log(fields: LogFields, message: string): void { emit("normal", { ...this.fields, ...fields }, message); }
  logVerbose(fields: LogFields, message: string): void { emit("verbose", { ...this.fields, ...fields }, message); }
  with(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...this.fields, ...fields }); }
}

/** Bind component and stable-operation context without mutating global state. */
export function bindLog(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...fields }); }
