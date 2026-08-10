/**
 * Client→daemon diagnostic forwarding (§2.15).
 *
 * The webapp runs inside an Emacs webview whose JS console nobody can
 * see and nothing persists — a delivery-path failure there (seq-gap
 * loop, lost replay-request, stalled rAF) previously left no evidence
 * anywhere. Every normal line logged here goes to the local console as JSON
 * and is mirrored to the daemon as a `client-log` frame,
 * which the daemon writes to its on-disk log.
 *
 * Normal records are persisted through the daemon and shown in the browser
 * console. Verbose records are persisted too, with console visibility gated
 * by the local verbose setting.
 */
import { ClientLogCmd, ClientLogContext } from "./protocol.js";
import { logTimestamp } from "../../agent-shim/logging/ts/timestamp.js";

export type ClientLogLevel = ClientLogCmd["level"];
export type { ClientLogContext };

export type LogContext = ClientLogContext;
export interface LogOptions {
  /** Stable machine-readable operation for this record. */
  operation: string;
  context?: LogContext;
  dedupKey?: string;
  /** Emergency console-only path for a rejected `clientLog` acknowledgement. */
  localOnly?: boolean;
}

export interface RuntimeLogContext {
  workspace_dir?: string;
  workspace_id?: string;
  agent_repl_session_id?: string;
  claude_session_id?: string;
  connection_id?: string;
  request_id?: string;
}

type LogLevel = ClientLogLevel | "debug";

interface WebappLogRecord {
  timestamp: string;
  runtime: "webapp";
  level: LogLevel;
  verbosity: "normal" | "verbose";
  operation: string;
  message: string;
  context: Record<string, unknown>;
  connection_id?: string;
  workspace_dir?: string;
  workspace_id?: string;
  agent_repl_session_id?: string;
  claude_session_id?: string;
  request_id?: string;
}

export class ForwardingLogger {
  /**
   * Logging shares the frontend WebSocket with ordinary commands. Startup and
   * reconnect therefore have a real interval where records exist before their
   * transport does. Retain those records in-order and flush them when the send
   * path becomes writable. The bound is fail-hard: exhausting it is a logging
   * subsystem failure, never permission to discard the oldest evidence.
   */
  private readonly pending: ClientLogCmd[] = [];

  /**
   * SEND pushes one frame toward the daemon. A rejected send is a failure of
   * the currently available transport, so the record remains queued for the
   * next successful send. CONSOLE_FN is injectable for tests.
   */
  constructor(
    private readonly send: (cmd: ClientLogCmd) => boolean,
    private readonly consoleFn: (level: ClientLogLevel, line: string) => void = defaultConsole,
    private readonly maximumPending = 1024,
  ) {}

  /**
   * Log one line. CONTEXT, when given, is structured evidence (ids, counters,
   * timings) forwarded on the frame's `context` Struct — the console side still
   * gets the message text, since a console line is read by a human.
   */
  write(
    level: ClientLogLevel,
    message: string,
    context: ClientLogContext,
    consoleEnabled = true,
    forward = true,
  ): void {
    if (consoleEnabled) this.consoleFn(level, JSON.stringify(context));
    if (!forward) return;
    const command: ClientLogCmd = { type: "client-log", level, message, context };
    let flushed: boolean;
    try {
      flushed = this.flush();
    } catch (err) {
      // A throw means the transport failed between its availability check and
      // send. Preserve the new record alongside any queue head flush() left in
      // place, then surface the transport failure unchanged.
      this.enqueue(command);
      throw err;
    }
    if (!flushed) {
      this.enqueue(command);
      return;
    }
    let sent: boolean;
    try {
      sent = this.send(command);
    } catch (err) {
      this.enqueue(command);
      throw err;
    }
    if (!sent) this.enqueue(command);
  }

  /**
   * Flush queued records in original emission order. False means the transport
   * is still unavailable and the first unsent record remains at the head.
   */
  flush(): boolean {
    while (this.pending.length > 0) {
      if (!this.send(this.pending[0])) return false;
      this.pending.shift();
    }
    return true;
  }

  /** Number of durable records awaiting an available forwarding transport. */
  pendingCount(): number {
    return this.pending.length;
  }

  private enqueue(command: ClientLogCmd): void {
    if (this.pending.length >= this.maximumPending) {
      throw new Error(`webapp log forwarding queue exhausted its ${this.maximumPending}-record bound`);
    }
    this.pending.push(command);
  }
}

function defaultConsole(level: ClientLogLevel, line: string): void {
  if (level === "error") console.error(line);
  else if (level === "warn") console.warn(line);
  else console.log(line);
}

// ---------------------------------------------------------------------------
// Module-level singleton, so modules deep in the render walk (partition,
// stream-member, chess-game, the WatcherPoller constructed inside
// FeedRenderer) can log without threading a logger through every
// constructor. main.ts must install the daemon-forwarding logger before normal
// runtime work begins. Logging before installation is an invariant violation.
// wslog imports only the protocol leaf, so importing { log } from here
// can never create an import cycle.
// ---------------------------------------------------------------------------

let active: ForwardingLogger | null = null;
let boundContext: RuntimeLogContext = {};
const VERBOSE_STORAGE_KEY = "agent-repl-log-verbose";

/** Install (or clear, with null) the app-wide logger. */
export function setLogger(logger: ForwardingLogger | null): void {
  active = logger;
}

/** Bind runtime identity included in every subsequent record. */
export function bindLogContext(context: RuntimeLogContext): void {
  boundContext = { ...boundContext, ...context };
}

/**
 * Restamp one forwarded record's SOURCE SESSION IDENTITY with the identity
 * bound RIGHT NOW, immediately before it is handed to the socket.
 *
 * WHY THE STAMP CANNOT BE THE EMISSION'S. A record is built when it is emitted
 * and forwarded when a transport exists, and a daemon bounce puts a long
 * interval between the two: the socket is down, so records pile up in the
 * throttle's window and the forwarding logger's queue, and the session the
 * workspace owns rotates while they wait. Flushing them as-built sends the
 * retired session id, which the daemon refuses per record — 2,606 refusals in
 * three minutes in production, all of them for records whose only fault was
 * being older than the rotation.
 *
 * WHY RESTAMPING IS CORRECT RATHER THAN A LIE. These fields are the record's
 * SOURCE ATTRIBUTION — which conversation this page belongs to — not evidence
 * about the event, which lives in the message and the context fields and is
 * untouched here. The daemon files every accepted record under the identity its
 * own registry holds regardless of what the record said, and reads the record's
 * copy for exactly one purpose: to refuse a record that would otherwise be
 * filed under a conversation it does not describe. A page that has adopted the
 * new identity IS the new conversation's page, so the stamp it should carry is
 * the one it holds at send time.
 *
 * An unbound identity removes the field rather than sending an empty one:
 * absence is a legitimate state (a workspace-addressed page before the daemon
 * has ruled), and the daemon reads an absent identity as "attributed to the
 * workspace alone".
 */
export function restampRecordIdentity(context: ClientLogContext): ClientLogContext {
  const stamped: Record<string, unknown> = { ...context };
  for (const identity of ["agent_repl_session_id", "claude_session_id"] as const) {
    const value = boundContext[identity];
    if (value === undefined || value === "") delete stamped[identity];
    else stamped[identity] = value;
  }
  return stamped as ClientLogContext;
}

function verboseConsoleEnabled(): boolean {
  return typeof localStorage !== "undefined" && localStorage.getItem(VERBOSE_STORAGE_KEY) === "true";
}

function requireString(context: Record<string, unknown>, field: string): string {
  const value = context[field];
  if (typeof value !== "string" || value.length === 0) throw new Error(`webapp log record requires ${field}`);
  return value;
}

function requireLevel(level: ClientLogLevel): ClientLogLevel {
  if (level === "info" || level === "warn" || level === "error") return level;
  throw new Error(`webapp log record has invalid level ${String(level)}`);
}

/** Convert browser values into JSON-safe evidence before protobuf Struct encoding. */
function jsonSafe(value: unknown, seen = new WeakSet<object>()): unknown {
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") return Number.isFinite(value) ? value : String(value);
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "undefined" || typeof value === "function" || typeof value === "symbol") return String(value);
  if (value instanceof Error) return { name: value.name, message: value.message, ...(value.stack !== undefined ? { stack: value.stack } : {}) };
  if (typeof value !== "object") return String(value);
  if (seen.has(value)) return "[Circular]";
  seen.add(value);
  if (Array.isArray(value)) return value.map((entry) => jsonSafe(entry, seen));
  const result: Record<string, unknown> = {};
  for (const [key, entry] of Object.entries(value)) result[key] = jsonSafe(entry, seen);
  return result;
}

function buildRecord(level: ClientLogLevel, message: string, context: Record<string, unknown>, verbosity: WebappLogRecord["verbosity"]): WebappLogRecord {
  const canonicalLevel = requireLevel(level);
  const fields = context;
  const operation = requireString(fields, "operation");
  const hasWorkspaceRouting = fields.workspace_dir !== undefined || fields.workspace_id !== undefined;
  const routing = {
    connection_id: requireString(fields, "connection_id"),
    ...(hasWorkspaceRouting
      ? {
      workspace_dir: requireString(fields, "workspace_dir"),
      workspace_id: requireString(fields, "workspace_id"),
      }
      : {}),
  };
  const reserved = new Set(["operation", "workspace_dir", "workspace_id", "connection_id", "agent_repl_session_id", "claude_session_id", "request_id"]);
  const evidence: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(fields)) if (!reserved.has(key) && value !== undefined) evidence[key] = jsonSafe(value);
  const record: WebappLogRecord = {
    timestamp: logTimestamp(), runtime: "webapp", level: canonicalLevel, verbosity, operation, message,
    context: evidence, ...routing,
  };
  // An identity is stamped on the record only when the caller HAS one. An empty
  // string is the absence of an identity, not a malformed one: a
  // workspace-addressed page carries no session id until the daemon rules on
  // which session its workspace owns, and a record written before that ruling
  // is correctly attributed to the workspace alone. A wrong TYPE is still a
  // programming error and still refused, so the check that matters is kept.
  for (const identity of ["agent_repl_session_id", "claude_session_id", "request_id"] as const) {
    const value = fields[identity];
    if (value === undefined || value === "") continue;
    record[identity] = requireString(fields, identity);
  }
  return record;
}

function emit(level: ClientLogLevel, message: string, options: LogOptions, verbose: boolean): void {
  if (options.dedupKey !== undefined) {
    if (dedupLast.get(options.dedupKey) === message) return;
  }
  if (active === null) throw new Error("wslog logger is not installed");
  const localContext = options.context ?? {};
  for (const identity of ["workspace_dir", "workspace_id", "connection_id", "agent_repl_session_id", "claude_session_id"] as const) {
    if (boundContext[identity] !== undefined && localContext[identity] !== undefined && boundContext[identity] !== localContext[identity]) {
      throw new Error(`webapp log context ${identity} conflicts with bound identity`);
    }
  }
  if (localContext.operation !== undefined) throw new Error("webapp log context must not override operation");
  const record = buildRecord(level, message, { ...boundContext, ...localContext, operation: options.operation }, verbose ? "verbose" : "normal");
  active.write(
    level,
    message,
    jsonSafe(record) as ClientLogContext,
    !verbose || verboseConsoleEnabled(),
    !options.localOnly,
  );
  // A failed write must not arm dedup and silently suppress a later attempt.
  if (options.dedupKey !== undefined) dedupLast.set(options.dedupKey, message);
}

/** Emit a normal diagnostic to the console and daemon persistence path. */
export function log(level: ClientLogLevel, message: string, options: LogOptions): void {
  emit(level, message, options, false);
}

/** Emit a verbose diagnostic to the daemon sink, with console gated by localStorage. */
export function logVerbose(level: ClientLogLevel, message: string, options: LogOptions): void {
  emit(level, message, options, true);
}

/**
 * Per-key dedup for hot paths (per-frame render guards, per-tick
 * pollers): a repeat of the SAME message under a key is suppressed
 * entirely — console included — because the caller fires every frame
 * and the first line already carries the evidence. A different message
 * under the key logs again (the error changed); clearDedup re-arms the
 * key (the caller observed recovery).
 */
const dedupLast = new Map<string, string>();

export function clearLogDedup(key: string): void {
  dedupLast.delete(key);
}


/** Test hook: drop the installed logger and all dedup state. */
export function resetLoggingForTests(): void {
  active = null;
  boundContext = { connection_id: "test-connection" };
  dedupLast.clear();
}
