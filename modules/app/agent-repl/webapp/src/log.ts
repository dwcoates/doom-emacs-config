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
 * tests read) and announced once per outage through the documented
 * logger-sink emergency console path — never through this module's own API.
 *
 * A REJECTED RECORD IS HELD, NEVER DROPPED: a daemon out of reach (a restart,
 * a handover) is exactly when the page has errors worth keeping. The record
 * waits in a bounded hold (`CLIENT_LOG_HELD_BOUND`, the oldest dropped past
 * it and counted), and the first `ClientLog` the daemon answers again resends
 * every held record, oldest first, with a warn naming any the bound dropped.
 * The sink answering IS the daemon being back, so nothing polls for it.
 */
import { create, type JsonObject } from "@bufbuild/protobuf";
import {
  ClientLogRecordSchema,
  type ClientLogRecord,
} from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { ClientLogThrottle, type ClientLogThrottleOptions } from "./clientlog-throttle.js";
import { logTimestamp } from "../../agent-shim/logging/ts/timestamp.js";
import {
  expiryContext,
  LevelWindow,
  selectionContext,
  selectionNote,
  WINDOW_SECONDS,
  type LevelExpiry,
  type LevelSelection,
} from "../../agent-shim/logging/ts/level-window.js";
import {
  renderSite,
  renderTemplate,
  SiteTable,
  type Literal,
  type LogSite,
  type LogValuesArg,
} from "../../agent-shim/logging/ts/logsite.js";

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
  /** Which of Emacs's two web views wrote the record (host/document.ts); absent in a workspace page. */
  webhost_document?: string;
}

export interface WebappLogRecord {
  timestamp: string;
  runtime: "webapp";
  level: ClientLogLevel;
  verbosity: ClientLogVerbosity;
  operation: string;
  /** The call's site key (logsite.ts `siteKey`). */
  site: string;
  /** The call's unrendered template, the site's summary. */
  template: string;
  /** TEMPLATE rendered from the call's named values. */
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

/** The most records a rejected `ClientLog` holds for resending: the throttle's own buffer bound. */
export const CLIENT_LOG_HELD_BOUND = 500;

/** The operation of the warn naming held records the bound dropped. */
export const HELD_DROPPED_OPERATION = "webapp.client-log-held-dropped";

/** The operation of every level window record: a startup decision and a window that ended. */
export const LEVEL_WINDOW_OPERATION = "webapp.log.level-window";

/** The runtime every webapp site key is derived under. */
export const WEBAPP_LOG_RUNTIME = "webapp";

/** The sites this module's own records are written at. */
const LOGGING_SITES = new SiteTable(WEBAPP_LOG_RUNTIME, "webapp.log");

/** No named values: what a template without placeholders is rendered from. */
const NO_VALUES: Readonly<Record<string, unknown>> = Object.freeze({});

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
  /** The records a rejected `ClientLog` left, oldest first, resent once the daemon answers. */
  private readonly held: ClientLogRecord[] = [];
  /** Held records the bound dropped since the last resend. */
  private heldDropped = 0;

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
            LOGGING_SITES.site("client log forwarding dropped {dropped} record(s) over its {buffer_bound}-record buffer bound"),
            { dropped, buffer_bound: bufferBound },
            { ...boundContext, operation: "webapp.client-log-throttle-dropped" },
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

  /** Records held for resending once the daemon answers again. */
  heldCount(): number {
    return this.held.length;
  }

  /**
   * Build the proto record and issue the call.
   *
   * Always returns true: the throttle's false means "the buffer was full and
   * the record is lost", which is a different fact from "the call is in
   * flight". A rejection lands in the bounded hold below (`noteSinkFailure`),
   * not back in the throttle, and is resent only once the daemon answers a
   * later call, so a broken sink never turns into an unbounded queue or a
   * retry loop.
   */
  private forward(record: ClientLogRecord): boolean {
    if (this.departed) return true;
    const restamped = create(ClientLogRecordSchema, {
      ...record,
      context: restampRecordIdentity(record.context ?? {}) as JsonObject,
    });
    this.deliver(restamped);
    return true;
  }

  /** Issue the call for RECORD: an answer resends what is held, a rejection holds RECORD. */
  private deliver(record: ClientLogRecord): void {
    void this.send(record)
      .then((outcome) => {
        if (outcome === "workspace_departed") this.noteWorkspaceDeparted();
        else this.resendHeld();
      })
      .catch((err: unknown) => this.noteSinkFailure(err, record));
  }

  /**
   * The daemon answered a `ClientLog`, so it is in reach again: resend every
   * held record, oldest first, after a warn naming any the bound dropped. A
   * resend that is rejected again is held again.
   */
  private resendHeld(): void {
    if (this.held.length === 0 && this.heldDropped === 0) return;
    const resend = this.held.splice(0);
    const dropped = this.heldDropped;
    this.heldDropped = 0;
    this.announcedSinkFailure = false;
    if (dropped > 0) {
      this.deliver(
        buildClientLogRecord(
          buildRecord(
            "warn",
            LOGGING_SITES.site("client log forwarding dropped {dropped} record(s) held over its {held_bound}-record bound while the daemon was out of reach"),
            { dropped, held_bound: CLIENT_LOG_HELD_BOUND },
            { ...boundContext, operation: HELD_DROPPED_OPERATION },
            "normal",
          ),
        ),
      );
    }
    for (const record of resend) this.deliver(record);
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
    const dropped = this.throttle.discard() + this.held.splice(0).length;
    this.heldDropped = 0;
    console.debug(
      `webapp ClientLog stopped: the daemon no longer knows this workspace; ` +
        `dropped ${String(dropped)} queued record(s), the console is now the only sink`,
    );
  }

  /** Whether forwarding has stopped because the workspace departed. */
  workspaceDeparted(): boolean {
    return this.departed;
  }

  private noteSinkFailure(err: unknown, record: ClientLogRecord): void {
    this.sinkFailures += 1;
    if (this.departed) return;
    if (this.held.length >= CLIENT_LOG_HELD_BOUND) {
      this.held.shift();
      this.heldDropped += 1;
    }
    this.held.push(record);
    if (this.announcedSinkFailure) return;
    this.announcedSinkFailure = true;
    // THE DOCUMENTED LOGGER-SINK EMERGENCY PATH. Routing this through `log`
    // would log the failure of logging, which fails, which logs. Once per
    // outage, direct: the next answer from the daemon ends the outage.
    console.error(
      `webapp ClientLog forwarding failed; records are held and resent once the daemon answers again: ${String(err)}`,
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
    site: record.site,
    template: record.template,
    message: record.message,
    context: context as JsonObject,
    timestamp: record.timestamp,
    verbose: record.verbosity === "verbose",
  });
}

/** Write LINE to the browser console at LEVEL: the console every logger writes to by default. */
export function defaultConsole(level: ClientLogLevel, line: string): void {
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
  site: LogSite,
  values: Readonly<Record<string, unknown>>,
  context: Record<string, unknown>,
  verbosity: WebappLogRecord["verbosity"],
): WebappLogRecord {
  const canonicalLevel = requireLevel(level);
  const message = renderSite(site, values);
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
  // Each named value is recorded under its placeholder's name. A name the
  // context already states would leave the record two answers for one key.
  for (const [name, value] of Object.entries(values)) {
    if (reserved.has(name) || Object.hasOwn(fields, name)) {
      throw new Error(`webapp log template ${JSON.stringify(site.template)} names {${name}}, which the record's context already states`);
    }
    evidence[name] = jsonSafe(value);
  }
  const record: WebappLogRecord = {
    timestamp: logTimestamp(), runtime: "webapp", level: canonicalLevel, verbosity, operation,
    site: site.key, template: site.template, message,
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

function emit(
  level: ClientLogLevel,
  sites: SiteTable,
  template: string,
  values: Readonly<Record<string, unknown>>,
  options: LogOptions,
): void {
  // Parsed and keyed once per site (SiteTable), rendered once per record.
  const site = sites.site(template);
  const observer = callObservers.get(level);
  if (observer !== undefined) {
    const named = Object.keys(values).length === 0 ? options : { ...options, context: { ...options.context, ...values } };
    observer(renderSite(site, values), named);
  }
  if (active === null) throw new Error("the webapp logger is not installed");
  const { allowed, ended } = active.admits(level);
  if (ended !== null) noteLevelExpiry(ended);
  if (!allowed) return;
  const localContext = options.context ?? {};
  for (const identity of ["workspace_dir", "workspace_id", "connection_id", "agent_repl_session_id", "claude_session_id"] as const) {
    if (boundContext[identity] !== undefined && localContext[identity] !== undefined && boundContext[identity] !== localContext[identity]) {
      throw new Error(`webapp log context ${identity} conflicts with bound identity`);
    }
  }
  if (localContext.operation !== undefined) throw new Error("webapp log context must not override operation");
  const record = buildRecord(level, site, values, { ...boundContext, ...localContext, operation: options.operation }, options.verbosity ?? "normal");
  if (options.dedupKey !== undefined) {
    if (dedupLast.get(options.dedupKey) === record.message) return;
  }
  active.write(buildClientLogRecord(record), JSON.stringify(jsonSafe(record)), !options.localOnly);
  // A failed write must not arm dedup and silently suppress a later attempt.
  if (options.dedupKey !== undefined) dedupLast.set(options.dedupKey, record.message);
}

/** Write one of this module's own records, at a "webapp.log" site. */
function internal<T extends string>(level: ClientLogLevel, options: LogOptions, template: Literal<T>, ...values: LogValuesArg<T>): void {
  emit(level, LOGGING_SITES, template, values[0] ?? NO_VALUES, options);
}

/** Record that a level window ended, as the shared level window's `expiryMessage` words it. */
function noteLevelExpiry(ended: LevelExpiry): void {
  internal(
    "info",
    { operation: LEVEL_WINDOW_OPERATION, context: expiryContext(ended) },
    "log level {from} window ended at {until_text}; reverted to info",
    { from: ended.from, until_text: new Date(ended.untilMs).toISOString() },
  );
}

/**
 * Record a startup level decision that did not hold the level asked for, as
 * the shared level window's `selectionNote` words it, one site per outcome; a
 * default or an honored window records nothing.
 */
export function logLevelSelection(selection: LevelSelection): void {
  if (selectionNote(selection) === null) return;
  const options: LogOptions = { operation: LEVEL_WINDOW_OPERATION, context: selectionContext(selection) };
  // Quoted, as `selectionNote` quotes it: the value came from the page address.
  const requested = JSON.stringify(selection.requested ?? "");
  const until = selection.requestedUntil ?? "";
  switch (selection.outcome) {
    case "no_expiry":
      internal("info", options, "log level {requested} ignored without AGENT_REPL_LOG_LEVEL_UNTIL; starting at info", { requested });
      return;
    case "expired":
      internal("info", options, "log level {requested} ignored: its window ended at {until_text}; starting at info", { requested, until_text: until });
      return;
    case "beyond_window":
      internal("info", options, "log level {requested} ignored: its window ends at {until_text}, more than {window_seconds}s away; starting at info", { requested, until_text: until, window_seconds: WINDOW_SECONDS });
      return;
    case "default":
    case "honored":
      return;
  }
}

/**
 * One log method: TEMPLATE, then the record's OPTIONS (its operation and
 * context evidence), then the template's named values.
 *
 * TEMPLATE MUST BE A STRING LITERAL, and the compiler refuses anything else:
 * the template is the site's summary and part of its key (the logger's scope,
 * the template and the runtime, hashed), so a message built at run time would
 * make every record its own site, and rewording a template gives its site a
 * new key. A dynamic value goes in VALUES under a name the template writes as
 * `{name}` (a literal brace is `{{` or `}}`); a missing or an extra value is a
 * type error. Each value is rendered into the message and recorded in the
 * record's context under its name. See logging-contract.md "Log sites".
 */
export type WebappLogMethod = <T extends string>(template: Literal<T>, options: LogOptions, ...values: LogValuesArg<T>) => void;

/**
 * A log message a helper writes ON ITS CALLER'S BEHALF: the caller's literal
 * template and its named values, made with the CALLER's logger
 * (`WebappLogger.message`) where the caller writes it. So the template is
 * still a literal at a call the static scan reads, and the record is keyed by
 * the caller's scope, never by the helper's, whichever logger writes it
 * (`WebappLogger.log`) and at whichever level.
 */
export interface LogMessage {
  readonly template: string;
  readonly values: Readonly<Record<string, unknown>>;
  /** The sites of the scope the message was written in. */
  readonly sites: SiteTable;
}

/** MESSAGE's template rendered from its values: the text its record's message carries. */
export function renderMessage(message: LogMessage): string {
  return renderTemplate(message.template, message.values);
}

/** A module's logger: one method per severity, keyed under its scope. */
export interface WebappLogger {
  readonly debug: WebappLogMethod;
  readonly info: WebappLogMethod;
  readonly warn: WebappLogMethod;
  readonly error: WebappLogMethod;
  /** The message written as TEMPLATE in this logger's scope, for a helper to write (LogMessage). */
  message<T extends string>(template: Literal<T>, ...values: LogValuesArg<T>): LogMessage;
  /** Write MESSAGE at LEVEL with OPTIONS, keyed under the scope it was written in. */
  log(level: ClientLogLevel, message: LogMessage, options: LogOptions): void;
}

/**
 * The logger of SCOPE, over the app-wide forwarding logger (`setLogger`).
 *
 * SCOPE IS A STRING LITERAL, one per logger in the whole webapp (a test holds
 * them unique): it stands in for the function a Go call is keyed by, since a
 * minified bundle has no caller to read at run time, so it is part of every
 * site key the logger's calls derive. Renaming it re-keys every one of them.
 */
export function createLogger<S extends string>(scope: Literal<S>): WebappLogger {
  const sites = new SiteTable(WEBAPP_LOG_RUNTIME, scope);
  const method =
    (level: ClientLogLevel): WebappLogMethod =>
    (template, options, ...values) =>
      emit(level, sites, template, values[0] ?? NO_VALUES, options);
  return {
    debug: method("debug"),
    info: method("info"),
    warn: method("warn"),
    error: method("error"),
    message: (template, ...values) => ({ template, values: values[0] ?? NO_VALUES, sites }),
    log: (level, message, options) => emit(level, message.sites, message.template, message.values, options),
  };
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

/** A test's observer of one level's log calls: the rendered message, and the options with the named values in the context. */
type LogCallObserver = (message: string, options: LogOptions) => void;
const callObservers = new Map<ClientLogLevel, LogCallObserver>();

/** Test hook (test/log-spy.ts): observe every call at LEVEL, whatever its scope; null stops. */
export function observeLogCallsForTests(level: ClientLogLevel, observer: LogCallObserver | null): void {
  if (observer === null) callObservers.delete(level);
  else callObservers.set(level, observer);
}

/** Test hook: drop the installed logger, bound identities, call observers and all dedup state. */
export function resetLoggingForTests(): void {
  active = null;
  callObservers.clear();
  boundContext = { connection_id: "test-connection" };
  dedupLast.clear();
}
