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
 * empty messages, so the arm IS the level. The public logger exposes exactly
 * `debug`, `info`, `warn` and `error`, while `LogOptions.verbosity` carries the
 * independent normal/verbose classification.
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
import { expiryContext, expiryMessage, LevelWindow, type LevelExpiry } from "../../agent-shim/logging/ts/level-window.js";

/** The four arms of `ClientLogRecord.level`. */
export type ClientLogLevel = "debug" | "info" | "warn" | "error";
export type ClientLogVerbosity = "normal" | "verbose";

/** Free-shape diagnostic evidence, as the call site composed it. */
export type ClientLogContext = Record<string, unknown>;
export type LogContext = ClientLogContext;

export interface LogOptions {
  /** Stable machine-readable operation for this record ("rpc.unary-call"). */
  operation: string;
  context?: LogContext;
  /** Classification orthogonal to severity. Omitted means normal. */
  verbosity?: ClientLogVerbosity;
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

export interface WebappLogRecord {
  timestamp: string;
  runtime: "webapp";
  level: ClientLogLevel;
  verbosity: ClientLogVerbosity;
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
/**
 * What the daemon did with one forwarded record.
 *
 * `workspace_departed` is `ClientLogError.unknown_workspace`: the daemon no
 * longer knows the workspace this page logs for, and never will again. It is
 * ORDINARY (the daemon records it at INFO) and it is TERMINAL — the sink is
 * gone, not failing — so it is a distinct outcome from a rejection.
 */
export type ClientLogSinkOutcome = "accepted" | "workspace_departed";
export type ClientLogSink = (record: ClientLogRecord) => Promise<ClientLogSinkOutcome>;

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

/** The operation of every level window record: a startup decision and a window that ended. */
export const LEVEL_WINDOW_OPERATION = "webapp.log.level-window";

/** Parse the page-delivered `AGENT_REPL_LOG_LEVEL` value. */
export function parseClientLogLevel(value: string | null): ClientLogLevel {
  if (value === null) return "info";
  if (value === "debug" || value === "info" || value === "warn" || value === "error") return value;
  throw new Error(`the page address carries invalid log_level '${value}'`);
}

/**
 * The forwarding logger: console on the way past, `ClientLog` behind the
 * throttle.
 */
export class ForwardingLogger {
  private readonly throttle: ClientLogThrottle;
  /** The live threshold: a level other than info reverts to info when its window ends. */
  private readonly window: LevelWindow;
  private sinkFailures = 0;
  // Set once the daemon answers unknown_workspace: forwarding is over for the
  // life of this logger, because the workspace it logs for is gone.
  private departed = false;
  private announcedSinkFailure = false;

  /**
   * SEND is the `ClientLog` call, built from the client by main.ts.
   * CONSOLE_FN is injectable so tests keep the suite's output clean.
   */
  constructor(
    private readonly send: ClientLogSink,
    private readonly consoleFn: (level: ClientLogLevel, line: string) => void = defaultConsole,
    throttleOptions: Omit<ClientLogThrottleOptions, "send" | "droppedRecord"> = {},
    threshold: ClientLogLevel | LevelWindow = "info",
  ) {
    this.window = typeof threshold === "string" ? LevelWindow.fixed(requireLevel(threshold)) : threshold;
    this.throttle = new ClientLogThrottle({
      ...throttleOptions,
      send: (record) => this.forward(record),
      droppedRecord: (dropped, bufferBound) =>
        buildClientLogRecord(
          buildRecord(
            "warn",
            `client log forwarding dropped ${dropped} record(s) over its ${bufferBound}-record buffer bound`,
            {
              ...boundContext,
              operation: "webapp.client-log-throttle-dropped",
              dropped,
              buffer_bound: bufferBound,
            },
            "normal",
          ),
        ),
    });
  }

  /**
   * Whether the page's level admits LEVEL now, and the level window this call
   * found ended (the caller records it at info; the window is info by then).
   */
  admits(level: ClientLogLevel): { allowed: boolean; ended: LevelExpiry | null } {
    return this.window.allows(level);
  }

  /**
   * Log one line. RECORD is already timestamped at the call site. CONSOLE_LINE
   * is the full JSON record a human debugging this page reads.
   */
  write(
    record: ClientLogRecord,
    consoleLine: string,
    forward = true,
  ): void {
    const level = requireRecordLevel(record);
    this.consoleFn(level, consoleLine);
    if (!forward) return;
    this.throttle.write(record);
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
  private forward(record: ClientLogRecord): boolean {
    if (this.departed) return true;
    const restamped = create(ClientLogRecordSchema, {
      ...record,
      context: restampRecordIdentity(record.context ?? {}) as JsonObject,
    });
    void this.send(restamped)
      .then((outcome) => {
        if (outcome === "workspace_departed") this.noteWorkspaceDeparted();
      })
      .catch((err: unknown) => this.noteSinkFailure(err));
    return true;
  }

  /**
   * The daemon has forgotten the workspace this page logs for. STOP FORWARDING
   * — every later record would draw the same refusal — and drop what is queued
   * for it, since no daemon will ever file those records.
   *
   * NOTHING GOES SILENT. The console remains the sink for every record from
   * here on (`write` consoles before it forwards), and the count of what the
   * queue lost is stated once, at debug, through the same direct console path
   * the sink-failure announcement uses: routing it through `log` would forward
   * a record about having stopped forwarding.
   */
  private noteWorkspaceDeparted(): void {
    if (this.departed) return;
    this.departed = true;
    const dropped = this.throttle.discard();
    console.debug(
      `webapp ClientLog stopped: the daemon no longer knows this workspace; ` +
        `dropped ${String(dropped)} queued record(s), the console is now the only sink`,
    );
  }

  /** Whether forwarding has stopped because the workspace departed. */
  workspaceDeparted(): boolean {
    return this.departed;
  }

  private noteSinkFailure(err: unknown): void {
    this.sinkFailures += 1;
    if (this.announcedSinkFailure) return;
    this.announcedSinkFailure = true;
    // THE DOCUMENTED LOGGER-SINK EMERGENCY PATH. Routing this through `log`
    // would log the failure of logging, which fails, which logs. Once, direct,
    // and never again for the life of this logger.
    console.error(
      `webapp ClientLog forwarding failed and further failures are counted only: ${String(err)}`,
    );
  }
}

/**
 * Assemble the proto record from the complete client-side record.
 *
 * `ClientLogRecord.context` is a `google.protobuf.Struct`, which protobuf-es
 * represents as a plain `JsonObject` rather than a tree of `Value` messages —
 * the library does the Struct encoding at the wire. Only call-site evidence
 * and logger-bound identities enter the Struct; the timestamp, level,
 * verbosity, operation and message use their dedicated protobuf fields.
 */
export function buildClientLogRecord(record: WebappLogRecord): ClientLogRecord {
  const context: ClientLogContext = { ...record.context, connection_id: record.connection_id };
  for (const identity of ["agent_repl_session_id", "claude_session_id", "request_id"] as const) {
    const value = record[identity];
    if (value !== undefined) context[identity] = value;
  }
  return create(ClientLogRecordSchema, {
    level: { case: LEVEL_ARM[record.level], value: {} },
    operation: record.operation,
    message: record.message,
    context: context as JsonObject,
    timestamp: record.timestamp,
    verbose: record.verbosity === "verbose",
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

function requireRecordLevel(record: ClientLogRecord): ClientLogLevel {
  const level = record.level.case;
  if (level === undefined) throw new Error("webapp log record requires a level");
  return requireLevel(level);
}

/** Convert browser values into JSON-safe evidence before Struct encoding. */
function jsonSafe(value: unknown, seen = new WeakSet<object>()): unknown {
  if (value === null || typeof value === "string" || typeof value === "boolean") return value;
  if (typeof value === "number") return Number.isFinite(value) ? value : String(value);
  if (typeof value === "bigint") return value.toString();
  if (typeof value === "undefined" || typeof value === "function" || typeof value === "symbol") return String(value);
  if (value instanceof Error) return { name: value.name, message: value.message, ...(value.stack !== undefined ? { stack: value.stack } : {}) };
  // An unreachable backstop, kept deliberately: every `typeof` result but "object" is answered
  // above, so nothing narrows the static type here and eslint reads this as stringifying an
  // object. A runtime that grows a new `typeof` must land in the log as SOMETHING rather than
  // throwing inside the logger.
  // eslint-disable-next-line @typescript-eslint/no-base-to-string -- see above
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

function emit(level: ClientLogLevel, message: string, options: LogOptions): void {
  if (active === null) throw new Error("the webapp logger is not installed");
  const { allowed, ended } = active.admits(level);
  if (ended !== null) emit("info", expiryMessage(ended), { operation: LEVEL_WINDOW_OPERATION, context: expiryContext(ended) });
  if (!allowed) return;
  if (options.dedupKey !== undefined) {
    if (dedupLast.get(options.dedupKey) === message) return;
  }
  const localContext = options.context ?? {};
  for (const identity of ["workspace_dir", "workspace_id", "connection_id", "agent_repl_session_id", "claude_session_id"] as const) {
    if (boundContext[identity] !== undefined && localContext[identity] !== undefined && boundContext[identity] !== localContext[identity]) {
      throw new Error(`webapp log context ${identity} conflicts with bound identity`);
    }
  }
  if (localContext.operation !== undefined) throw new Error("webapp log context must not override operation");
  const record = buildRecord(level, message, { ...boundContext, ...localContext, operation: options.operation }, options.verbosity ?? "normal");
  active.write(buildClientLogRecord(record), JSON.stringify(jsonSafe(record)), !options.localOnly);
  // A failed write must not arm dedup and silently suppress a later attempt.
  if (options.dedupKey !== undefined) dedupLast.set(options.dedupKey, message);
}

export type LogMethod = (message: string, options: LogOptions) => void;

/** The canonical app-wide logger: one method for each severity. */
export const log = {
  debug: (message, options) => emit("debug", message, options),
  info: (message, options) => emit("info", message, options),
  warn: (message, options) => emit("warn", message, options),
  error: (message, options) => emit("error", message, options),
} as const satisfies Record<ClientLogLevel, LogMethod>;

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

/** Test hook: drop the installed logger, bound identities and all dedup state. */
export function resetLoggingForTests(): void {
  active = null;
  boundContext = { connection_id: "test-connection" };
  dedupLast.clear();
}
