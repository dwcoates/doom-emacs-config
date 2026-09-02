/**
 * The webapp's canonical logging API (§2.15), over the `ClientLog` rpc.
 *
 * WHY THE WEBAPP FORWARDS ITS CONSOLE AT ALL. This page runs inside an Emacs
 * xwidget whose JS console nobody can see and nothing persists: a delivery
 * failure here — a stream that never reopened, a view that would not decode —
 * would otherwise leave no evidence anywhere. `ClientLog` is the daemon's
 * console-less-client relay: one record per call, written to the workspace's
 * durable structured log beside its other telemetry.
 *
 * WHAT CHANGED WITH THE TRANSPORT PORT. The sink used to be a `client-log`
 * frame on the shared frontend WebSocket, which is why the old logger carried
 * a pending queue: records existed before their transport did. A Connect
 * client has no such interval — it is constructed before boot logs anything
 * and a call that cannot reach the daemon simply rejects — so the queue is
 * gone and the throttle in front of it (`clientlog-throttle.ts`) is what keeps
 * the daemon's log proportional to what actually happened.
 *
 * THE LEVELS ARE THE PROTO'S ARMS. `ClientLogRecord.level` is a oneof of four
 * empty messages, so the arm IS the level; `debug` joins the three the old
 * WebSocket command carried.
 *
 * A SINK FAILURE MUST NOT RECURSE. Logging a failed log would produce another
 * failed log, so a rejected `ClientLog` is counted (`sinkFailureCount`, which
 * tests read) and announced exactly once through the documented logger-sink
 * emergency console path — never through this module's own API.
 */
import { create, type JsonObject } from "@bufbuild/protobuf";
import {
  ClientLogRecordSchema,
  type ClientLogRecord,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { ClientLogThrottle, type ClientLogThrottleOptions } from "./clientlog-throttle.js";
import { logTimestamp } from "../../agent-shim/logging/ts/timestamp.js";

/** The four arms of `ClientLogRecord.level`. */
export type ClientLogLevel = "debug" | "info" | "warn" | "error";

/** Free-shape diagnostic evidence, as the call site composed it. */
export type ClientLogContext = Record<string, unknown>;
export type LogContext = ClientLogContext;

export interface LogOptions {
  /** Stable machine-readable operation for this record ("rpc.unary-call"). */
  operation: string;
  context?: LogContext;
  dedupKey?: string;
  /** Emergency console-only path; the record is not forwarded. */
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

interface WebappLogRecord {
  timestamp: string;
  runtime: "webapp";
  level: ClientLogLevel;
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

/** Hands one record to the daemon. Rejects when the call did not land. */
export type ClientLogSink = (record: ClientLogRecord) => Promise<void>;

/**
 * The `level` oneof arm each level name selects. Spelled as data so a renamed
 * arm fails the build here rather than silently filing every record as debug.
 */
const LEVEL_ARM = {
  debug: "debug",
  info: "info",
  warn: "warn",
  error: "error",
} as const satisfies Record<ClientLogLevel, NonNullable<ClientLogRecord["level"]["case"]>>;

/**
 * The forwarding logger: console on the way past, `ClientLog` behind the
 * throttle.
 */
export class ForwardingLogger {
  private readonly throttle: ClientLogThrottle;
  private sinkFailures = 0;
  private announcedSinkFailure = false;

  /**
   * SEND is the `ClientLog` call, built from the client by main.ts.
   * CONSOLE_FN is injectable so tests keep the suite's output clean.
   */
  constructor(
    private readonly send: ClientLogSink,
    private readonly consoleFn: (level: ClientLogLevel, line: string) => void = defaultConsole,
    throttleOptions: Omit<ClientLogThrottleOptions, "send"> = {},
  ) {
    this.throttle = new ClientLogThrottle({
      ...throttleOptions,
      send: (level, message, context) => this.forward(level, message, context ?? {}),
    });
  }

  /**
   * Log one line. CONTEXT is the fully built record (the routing identities
   * plus the call site's evidence); the console gets its JSON, because a
   * console line is read by a human debugging this page.
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
    this.throttle.write(level, message, context);
  }

  /** Release the throttle's window now (a page about to be torn down). */
  flush(): void {
    this.throttle.flush();
  }

  /** Records waiting on the throttle's window. */
  pendingCount(): number {
    return this.throttle.bufferedCount();
  }

  /** `ClientLog` calls that did not land. Read by tests; never logged. */
  sinkFailureCount(): number {
    return this.sinkFailures;
  }

  /**
   * Build the proto record and issue the call.
   *
   * Always returns true: the throttle's false means "the buffer was full and
   * the record is lost", which is a different fact from "the call is in
   * flight". A rejection lands in the emergency path below, not back in the
   * throttle, because re-queueing a record whose sink is down is how a broken
   * sink turns into an unbounded queue.
   */
  private forward(level: ClientLogLevel, message: string, context: ClientLogContext): boolean {
    const record = buildClientLogRecord(level, message, restampRecordIdentity(context));
    void this.send(record).catch((err: unknown) => this.noteSinkFailure(err));
    return true;
  }

  private noteSinkFailure(err: unknown): void {
    this.sinkFailures += 1;
    if (this.announcedSinkFailure) return;
    this.announcedSinkFailure = true;
    // THE DOCUMENTED LOGGER-SINK EMERGENCY PATH. Routing this through log()
    // would log the failure of logging, which fails, which logs. Once, direct,
    // and never again for the life of this logger.
    console.error(
      `webapp ClientLog forwarding failed and further failures are counted only: ${String(err)}`,
    );
  }
}

/**
 * Assemble the proto record from a level, a sentence and the built context.
 *
 * `ClientLogRecord.context` is a `google.protobuf.Struct`, which protobuf-es
 * represents as a plain `JsonObject` rather than a tree of `Value` messages —
 * the library does the Struct encoding at the wire. The context reaching here
 * has already been through `jsonSafe`, so every leaf is one of the five shapes
 * Struct can carry.
 */
export function buildClientLogRecord(
  level: ClientLogLevel,
  message: string,
  context: ClientLogContext,
): ClientLogRecord {
  return create(ClientLogRecordSchema, {
    level: { case: LEVEL_ARM[level], value: {} },
    operation: typeof context.operation === "string" ? context.operation : "",
    message,
    context: context as JsonObject,
  });
}

function defaultConsole(level: ClientLogLevel, line: string): void {
  if (level === "error") console.error(line);
  else if (level === "warn") console.warn(line);
  else console.log(line);
}

// ---------------------------------------------------------------------------
// Module-level singleton, so modules deep in the render walk can log without
// threading a logger through every constructor. main.ts installs the
// forwarding logger before normal runtime work begins; logging before
// installation is an invariant violation. This module imports only the
// generated record and the throttle, so importing { log } from anywhere can
// never create an import cycle.
// ---------------------------------------------------------------------------

let active: ForwardingLogger | null = null;
let boundContext: RuntimeLogContext = {};

/**
 * Whether verbose records also reach the browser console.
 *
 * A MODULE FLAG, NOT localStorage. The localStorage verbose toggle is dead
 * with the overhaul (nothing is persisted client-side except the webview-local
 * view preferences), so the gate is a flag a developer flips from the console
 * or a test sets directly. Verbose records are PERSISTED either way — the gate
 * is console noise only.
 */
let verboseConsole = false;

/** Install (or clear, with null) the app-wide logger. */
export function setLogger(logger: ForwardingLogger | null): void {
  active = logger;
}

/** Turn browser-console output for verbose records on or off. */
export function setVerboseConsole(enabled: boolean): void {
  verboseConsole = enabled;
}

/** Bind runtime identity included in every subsequent record. */
export function bindLogContext(context: RuntimeLogContext): void {
  boundContext = { ...boundContext, ...context };
}

/**
 * Restamp one forwarded record's SOURCE SESSION IDENTITY with the identity
 * bound RIGHT NOW, immediately before it is handed to the sink.
 *
 * WHY THE STAMP CANNOT BE THE EMISSION'S. A record is built when it is emitted
 * and forwarded when the throttle's window opens, and a daemon bounce puts a
 * long interval between the two: records pile up while the session the
 * workspace owns rotates. Flushing them as-built sends the retired session id,
 * which the daemon refuses per record — 2,606 refusals in three minutes in
 * production, all of them for records whose only fault was being older than
 * the rotation.
 *
 * WHY RESTAMPING IS CORRECT RATHER THAN A LIE. These fields are the record's
 * SOURCE ATTRIBUTION — which conversation this page belongs to — not evidence
 * about the event, which lives in the message and the context fields and is
 * untouched here. A page that has adopted the new identity IS the new
 * conversation's page.
 *
 * An unbound identity REMOVES the field rather than sending an empty one:
 * absence is a legitimate state, and the daemon reads an absent identity as
 * "attributed to the workspace alone".
 */
export function restampRecordIdentity(context: ClientLogContext): ClientLogContext {
  const stamped: Record<string, unknown> = { ...context };
  for (const identity of ["agent_repl_session_id", "claude_session_id"] as const) {
    const value = boundContext[identity];
    if (value === undefined || value === "") delete stamped[identity];
    else stamped[identity] = value;
  }
  return stamped;
}

function requireString(context: Record<string, unknown>, field: string): string {
  const value = context[field];
  if (typeof value !== "string" || value.length === 0) throw new Error(`webapp log record requires ${field}`);
  return value;
}

function requireLevel(level: ClientLogLevel): ClientLogLevel {
  if (level === "debug" || level === "info" || level === "warn" || level === "error") return level;
  throw new Error(`webapp log record has invalid level ${String(level)}`);
}

/** Convert browser values into JSON-safe evidence before Struct encoding. */
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

function buildRecord(
  level: ClientLogLevel,
  message: string,
  context: Record<string, unknown>,
  verbosity: WebappLogRecord["verbosity"],
): WebappLogRecord {
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
  // An identity is stamped only when the caller HAS one. An empty string is
  // the absence of an identity, not a malformed one: a workspace-addressed
  // page carries no session id until the daemon rules on which session its
  // workspace owns. A wrong TYPE is still a programming error and refused.
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
  if (active === null) throw new Error("the webapp logger is not installed");
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
    !verbose || verboseConsole,
    !options.localOnly,
  );
  // A failed write must not arm dedup and silently suppress a later attempt.
  if (options.dedupKey !== undefined) dedupLast.set(options.dedupKey, message);
}

/** Emit a normal diagnostic to the console and the daemon's durable log. */
export function log(level: ClientLogLevel, message: string, options: LogOptions): void {
  emit(level, message, options, false);
}

/** Emit a verbose diagnostic; persisted always, console gated by the flag. */
export function logVerbose(level: ClientLogLevel, message: string, options: LogOptions): void {
  emit(level, message, options, true);
}

/**
 * Per-key dedup for hot paths (per-push render guards, per-tick pollers): a
 * repeat of the SAME message under a key is suppressed entirely — console
 * included — because the caller fires every frame and the first line already
 * carries the evidence. A different message under the key logs again (the
 * error changed); clearLogDedup re-arms the key (the caller observed recovery).
 */
const dedupLast = new Map<string, string>();

export function clearLogDedup(key: string): void {
  dedupLast.delete(key);
}

/** Test hook: drop the installed logger, the verbose gate and all dedup state. */
export function resetLoggingForTests(): void {
  active = null;
  boundContext = { connection_id: "test-connection" };
  verboseConsole = false;
  dedupLast.clear();
}
