/** Canonical JSONL logging owned by the Claude shim runtime. */
import { createHash } from "node:crypto";
import { writeSync } from "node:fs";
import { logTimestamp } from "../../../logging/ts/timestamp.js";
import {
  expiryContext,
  expiryMessage,
  LevelWindow,
  selectionContext,
  selectionNote,
  selectLevel,
  UNTIL_ENV,
  type LevelSelection,
} from "../../../logging/ts/level-window.js";

export type LogLevel = "debug" | "info" | "warn" | "error";
export type LogFields = Record<string, unknown>;

export interface ShimLogger {
  debug(fields: LogFields, message: string): void;
  info(fields: LogFields, message: string): void;
  warn(fields: LogFields, message: string): void;
  error(fields: LogFields, message: string): void;
  /** A debug-level record whose verbose class only affects stderr mirroring. */
  logVerbose(fields: LogFields, message: string): void;
  with(fields: LogFields): ShimLogger;
}

export interface ShimLogConfiguration {
  /** Already-open descriptor inherited from the daemon; this module never opens paths. */
  fd: number;
  /** Authoritative, already-canonical workspace path supplied by the daemon. */
  cwd: string;
  /**
   * THE DAEMON'S OWN 16-hex workspace id, and the only thing `workspace_id`
   * ever carries.
   *
   * It is the daemon's minted identity for the workspace, not a function of
   * the path: `bin/logs.sh --workspace` and the realtest harvest group records
   * by it, so a shim that answered with an id of its own devising was filed
   * under a workspace nothing else in the fleet had ever heard of. The shim
   * does not mint it and cannot derive it from the workspace directory -- it
   * reads it off the listen socket the daemon named.
   */
  workspaceId: string;
  agentReplSessionId: string;
  /** The clock a level window's end is checked against; tests inject one. */
  now?: () => number;
}

interface RuntimeContext {
  fd: number;
  workspace_dir: string;
  workspace_id: string;
  /**
   * The shim's own md5-prefix key for this workspace directory.
   *
   * KEPT, AS A SEPARATE FIELD. It is what the workspace LOCK FILE is named
   * after, so it is the only thing that joins a log record to a lock file on
   * disk; it is simply not the fleet's workspace identity and never was.
   */
  shim_workspace_hash: string;
  agent_repl_session_id: string;
  /** The live threshold: a level other than info reverts to info when its window ends. */
  window: LevelWindow;
  claude_session_id?: string;
  request_id?: string;
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
  // The logger states this one itself; a caller's copy could disagree with it.
  "shim_workspace_hash",
]);
const LOG_LEVEL_ENV = "AGENT_REPL_LOG_LEVEL";
/** The operation of every level window record: the startup decision and a window that ended. */
const LEVEL_WINDOW_OPERATION = "shim.logging.level-window";

function requireString(fields: LogFields, field: string): string {
  const value = fields[field];
  if (typeof value !== "string" || value.length === 0) throw new Error(`shim log record requires ${field}`);
  return value;
}

function requireContext(): RuntimeContext {
  if (runtimeContext === undefined) throw new Error("shim logger is not configured for a UDS workspace");
  return runtimeContext;
}

/**
 * Resolve the process-startup persistence and mirror threshold. A level other
 * than info holds only inside the window `AGENT_REPL_LOG_LEVEL_UNTIL` names
 * (proto/vocab/log-level-window.json); an unknown level or a malformed window
 * is a refusal.
 */
function configuredLogLevel(nowMs: number): LevelSelection {
  return selectLevel(process.env[LOG_LEVEL_ENV], process.env[UNTIL_ENV], nowMs, LOG_LEVEL_ENV);
}

/** Configure the durable inherited sink exactly once for one shim process. */
export function configureLog(config: ShimLogConfiguration): void {
  if (!Number.isInteger(config.fd) || config.fd < 0) throw new Error("shim log fd must be a non-negative integer");
  if (typeof config.cwd !== "string" || config.cwd.length === 0) throw new Error("shim log cwd is required");
  if (typeof config.agentReplSessionId !== "string" || config.agentReplSessionId.length === 0) {
    throw new Error("shim agent-repl session id is required");
  }
  // A REFUSAL, NOT A DEFAULT. Every workspace record owes a `workspace_id`,
  // and one the fleet cannot recognize is worse than none: it files the record
  // under a workspace nothing else ever writes to.
  if (typeof config.workspaceId !== "string" || config.workspaceId.length === 0) {
    throw new Error("shim workspace id is required");
  }
  if (runtimeContext !== undefined) throw new Error("shim logger has already been configured");
  const now = config.now ?? Date.now;
  const selection = configuredLogLevel(now());
  // Do not realpath this value: the daemon supplied the canonical cwd and owns symlink resolution.
  runtimeContext = {
    fd: config.fd,
    workspace_dir: config.cwd,
    workspace_id: config.workspaceId,
    shim_workspace_hash: createHash("md5").update(config.cwd).digest("hex").slice(0, 8),
    agent_repl_session_id: config.agentReplSessionId,
    window: new LevelWindow(selection, now),
    write: (fd, bytes, offset, length) => writeSync(fd, bytes, offset, length),
  };
  stderrMirror = "live";
  // An asynchronous EPIPE (the daemon's read end closing between writes) lands
  // as an 'error' event, not as a throw from write(). Without this listener it
  // is an uncaught exception and the shim dies mid-turn.
  process.stderr.on("error", (err: Error) => retireStderrMirror(err));
  const note = selectionNote(selection);
  if (note !== null) emit("info", "normal", { operation: LEVEL_WINDOW_OPERATION, ...selectionContext(selection) }, note);
}

/**
 * Whether RUNTIME's live threshold admits LEVEL now. The first call to find a
 * level window ended records the revert at info; the window is info by then,
 * so that record cannot end it again.
 */
function levelEnabled(runtime: RuntimeContext, level: LogLevel): boolean {
  const { allowed, ended } = runtime.window.allows(level);
  if (ended !== null) emit("info", "normal", { operation: LEVEL_WINDOW_OPERATION, ...expiryContext(ended) }, expiryMessage(ended));
  return allowed;
}

/**
 * Stop mirroring to stderr, recording the loss durably exactly once.
 *
 * The durable write here is deliberately NOT emit(): emit mirrors, and a mirror
 * failure recursing into itself is how one broken pipe becomes a stack
 * overflow. A durable-sink failure while recording this poisons the logger, so
 * the next log call throws — the failure is surfaced, never swallowed.
 *
 * AND IT IS NOT A WARNING, for the reason the message itself states: a shim
 * OUTLIVES its daemon by design, so the read end of this pipe closing is the
 * ordinary end of an ordinary daemon, not a defect of anyone's. Nothing is lost
 * with it either — the mirror was only ever a second copy of a record the
 * durable sink already holds, and this very record proves the sink still works.
 * A warning here is a warning on every daemon bounce, every deploy and every
 * editor quit, which tells nobody anything while costing a harvest whose bar
 * admits no WARN at all. The record stays, at the level a designed-for
 * lifecycle event warrants.
 *
 * A DURABLE-SINK FAILURE IS THE DIFFERENT THING and is untouched below: it
 * poisons the logger and the next call throws.
 */
function retireStderrMirror(cause: Error): void {
  if (stderrMirror === "retired") return;
  stderrMirror = "retired";
  const runtime = runtimeContext;
  if (runtime === undefined || runtime.poisoned !== undefined) return;
  if (!levelEnabled(runtime, "info")) return;
  const record = buildRecord("info", "normal", {
    operation: "shim.logging.stderr-mirror",
    cause: cause.message,
  }, "stderr mirror RETIRED — the daemon that owned this pipe is gone; this shim keeps running and keeps logging durably, because a shim outlives its daemon by design");
  try {
    writeDurable(runtime, Buffer.from(`${JSON.stringify(record)}\n`, "utf8"));
  } catch (err) {
    poisonSink(runtime, err instanceof Error ? err : new Error(String(err)));
  }
}

/** Record the vendor identity as soon as the SDK reveals it. */
export function setClaudeSessionId(claudeSessionId: string): void {
  if (typeof claudeSessionId !== "string" || claudeSessionId.length === 0) {
    throw new Error("shim claude session id is required");
  }
  requireContext().claude_session_id = claudeSessionId;
}

/**
 * Stamp the vendor request the CURRENT TURN is running under.
 *
 * The SDK names it only on its assistant messages, so it is learned mid-turn
 * and holds until the turn ends. It is cleared there rather than left standing:
 * a request id outliving its turn would attribute the next turn's records --
 * and every idle record between turns -- to a request that is already answered.
 */
export function setRequestId(requestId: string): void {
  if (typeof requestId !== "string" || requestId.length === 0) {
    throw new Error("shim request id is required");
  }
  requireContext().request_id = requestId;
}

/** Drop the turn's request id at the end of the turn that revealed it. */
export function clearRequestId(): void {
  const runtime = runtimeContext;
  if (runtime === undefined) return;
  delete runtime.request_id;
}

/**
 * A value as an error message can name it.
 *
 * `String(x)` renders any object as "[object Object]", which is the one thing a
 * message about an unrecognized value must not do. JSON names the shape; the
 * fallback covers what JSON declines to encode (a symbol, a function).
 */
function describe(value: unknown): string {
  const encoded = typeof value === "string" ? value : JSON.stringify(value);
  return encoded ?? typeof value;
}

function requireMethodLevel(fields: LogFields): void {
  if (Object.hasOwn(fields, "level")) {
    throw new Error(`shim log level is selected by its logger method, not a level field: ${describe(fields.level)}`);
  }
}

function jsonSafe(value: unknown, seen = new WeakSet<object>()): unknown {
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") return Number.isFinite(value) ? value : String(value);
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "undefined" || typeof value === "function" || typeof value === "symbol") return String(value);
  if (value instanceof Error) return { name: value.name, message: value.message, ...(value.stack === undefined ? {} : { stack: value.stack }) };
  // An unreachable backstop, kept deliberately: every `typeof` result but "object" is answered
  // above, so nothing narrows the static type here and eslint reads this as stringifying an
  // object. A runtime that grows a new `typeof` must land in the log as SOMETHING rather than
  // throwing inside the logger.
  // eslint-disable-next-line @typescript-eslint/no-base-to-string -- see above
  if (typeof value !== "object") return String(value);
  if (seen.has(value)) return "[Circular]";
  seen.add(value);
  if (Array.isArray(value)) return value.map((entry) => jsonSafe(entry, seen));
  return Object.fromEntries(Object.entries(value).map(([key, entry]) => [key, jsonSafe(entry, seen)]));
}

function buildRecord(
  level: LogLevel,
  verbosity: ShimLogRecord["verbosity"],
  fields: LogFields,
  message: string,
): ShimLogRecord {
  const runtime = requireContext();
  requireMethodLevel(fields);
  const operation = requireString(fields, "operation");
  // NOTHING IS LOST BY THE MOVE. `workspace_id` now carries the fleet's own
  // identity, so the shim's md5 prefix -- the only thing that joins a record to
  // the workspace lock FILE on disk -- travels beside it as its own key.
  const context: Record<string, unknown> = { shim_workspace_hash: runtime.shim_workspace_hash };
  for (const [key, value] of Object.entries(fields)) {
    if (!RESERVED_FIELDS.has(key) && value !== undefined) context[key] = jsonSafe(value);
  }
  const record: ShimLogRecord = {
    timestamp: logTimestamp(), runtime: "shim", level, verbosity,
    operation, message, context, pid: process.pid,
    workspace_dir: runtime.workspace_dir, workspace_id: runtime.workspace_id,
    agent_repl_session_id: runtime.agent_repl_session_id,
    ...(runtime.claude_session_id === undefined ? {} : { claude_session_id: runtime.claude_session_id }),
    ...(runtime.request_id === undefined ? {} : { request_id: runtime.request_id }),
  };
  if (fields.request_id !== undefined) record.request_id = requireString(fields, "request_id");
  if (fields.claude_session_id !== undefined) record.claude_session_id = requireString(fields, "claude_session_id");
  return record;
}

/** The only pre-logger/sink-failure escape hatch. It must never persist. */
export function emergencyStderr(message: string): void {
  const runtime = runtimeContext;
  // NOT GATED ON THE MIRROR. The mirror is retired for ROUTINE records, which
  // are a convenience copy of what fd 3 already holds; an emergency is the
  // opposite case -- fd 3 is exactly what has failed, so this is the only
  // channel the failure has left, and skipping it here would be the one place
  // the shim swallowed an error outright.
  //
  // A failure HERE is not the failure being reported: this is the escape hatch
  // used while the real error is on its way out, and letting a dead pipe throw
  // over it would replace a surfaced error with a process death.
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

/**
 * Who is told when the durable sink dies.
 *
 * Module-level because the logger is: `bindLog` is imported everywhere and
 * there is one process-wide sink. The engine registers here at construction so
 * a poisoning becomes a `SessionFault` and a degraded window on WatchSession.
 */
const poisonListeners = new Set<(cause: Error) => void>();

/**
 * Observe the durable sink dying. Returns the unsubscribe.
 *
 * Called at once with the cause if the sink is ALREADY poisoned, so a listener
 * that registered late still learns of it — the fault is a standing condition,
 * not an instant.
 */
export function onLogSinkPoisoned(listener: (cause: Error) => void): () => void {
  poisonListeners.add(listener);
  const already = runtimeContext?.poisoned;
  if (already !== undefined) listener(already);
  return () => poisonListeners.delete(listener);
}

/**
 * Record the sink's death: loudly, durably where anything is still durable,
 * and exactly ONCE.
 *
 * NOT FATAL, and that is the whole point (the EPIPE incident, 2026-08-10): a
 * shim outlives its daemon by design, so a process that died because its log
 * line could not be written would take a live turn with it. The failure is not
 * swallowed either — it goes to the emergency stderr path and it becomes a
 * SessionFault plus a standing degraded window, which is the loudest channel
 * the shim has once its own log is gone.
 */
function poisonSink(runtime: RuntimeContext, failure: Error): void {
  if (runtime.poisoned !== undefined) return;
  runtime.poisoned = failure;
  emergencyStderr(`shim log sink is POISONED and records are being lost: ${failure.message}`);
  for (const listener of poisonListeners) listener(failure);
}

function emit(level: LogLevel, verbosity: ShimLogRecord["verbosity"], fields: LogFields, message: string): void {
  // Construct and serialize completely before either sink is touched: invalid records emit nowhere.
  const record = buildRecord(level, verbosity, fields, message);
  const runtime = requireContext();
  if (!levelEnabled(runtime, level)) return;
  const bytes = Buffer.from(`${JSON.stringify(record)}\n`, "utf8");
  // A POISONED SINK LOSES RECORDS; IT DOES NOT END THE PROCESS. The loss was
  // announced once, and is standing as a degraded window on WatchSession, so
  // repeating it per record would only bury the announcement.
  if (runtime.poisoned !== undefined) return;
  try {
    writeDurable(runtime, bytes);
  } catch (err) {
    poisonSink(runtime, err instanceof Error ? err : new Error(String(err)));
    return;
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
  debug(fields: LogFields, message: string): void { emit("debug", "normal", { ...this.fields, ...fields }, message); }
  info(fields: LogFields, message: string): void { emit("info", "normal", { ...this.fields, ...fields }, message); }
  warn(fields: LogFields, message: string): void { emit("warn", "normal", { ...this.fields, ...fields }, message); }
  error(fields: LogFields, message: string): void { emit("error", "normal", { ...this.fields, ...fields }, message); }
  logVerbose(fields: LogFields, message: string): void { emit("debug", "verbose", { ...this.fields, ...fields }, message); }
  with(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...this.fields, ...fields }); }
}

/** Bind component and stable-operation context without mutating global state. */
export function bindLog(fields: LogFields): ShimLogger { return new BoundShimLogger({ ...fields }); }
