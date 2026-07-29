/**
 * CommandDispatcher — the webapp's frontend→daemon command plane.
 *
 * Every command is an `agentshim.frontend.v1.FrontendCommand` protojson frame
 * (frontend-command.ts) sent over the WebSocket. This owns request-id
 * generation and the two correlation patterns the daemon uses:
 *
 * - MOST commands (submitPrompt, interrupt, permissionAnswer, resync,
 *   deleteSession) resolve on the `CommandAck` receipt correlated by
 *   `requestId`: ok resolves, a non-empty error rejects loudly.
 * - CREATE SESSION is the locked S7 two-phase pattern: the `CommandAck` is a
 *   BARE RECEIPT (it carries no session id), and the new session id arrives on
 *   a pushed `SessionView`. The webapp — unlike Emacs, which supplies the
 *   workspace dir and matches by it — creates one session at a time with a
 *   daemon-assigned cwd, so it correlates on the FIRST non-terminal
 *   `SessionView` whose id was NOT already known when the create began (and, if
 *   a cwd was supplied, whose workspace matches it). A rejected create ack
 *   still fails the promise.
 *
 * The dispatcher is pure w.r.t. transport: `send` returns whether the socket
 * accepted the frame, and inbound decoded frames are fed back via `observe`.
 * That makes the whole plane testable against a mocked WS.
 */

import type { CommandAck, FrontendFrame, SessionView, SystemFailure } from "./frontend-proto.js";
import {
  encodeFrontendCommand,
  type ClientLogBody,
  type ClientLogBodyLevel,
  type CommandStruct,
  type FrontendCommandBody,
} from "./frontend-command.js";
import { log, logVerbose } from "./wslog.js";

/**
 * How many in-flight `clientLog` request ids are remembered for ack
 * recognition. Bounded so a socket that drops mid-flight cannot leak ids.
 */
const MAX_TRACKED_CLIENT_LOGS = 256;

function asError(value: unknown): Error {
  return value instanceof Error ? value : new Error(String(value));
}

function requiredSelectedModel(ack: CommandAck, disposition: "ack" | "nack"): string {
  const selected = ack.selectedModel;
  if (selected === "" || selected.trim() === "<synthetic>") {
    throw new Error(`setModel ${disposition} protocol violation: selectedModel is absent, empty, or <synthetic>`);
  }
  return selected;
}

/**
 * The interrupt confirmation CHALLENGE (I1), as a rejection a caller can tell
 * apart from a failure.
 *
 * A `CommandAck` carrying `interruptConfirmRequired` is NOT an error: the
 * daemon understood the interrupt and deliberately did not perform it, because
 * no turn was live and stopping working subagents is worth a deliberate second
 * keystroke. It still rejects the promise — the command did not happen, and a
 * resolve would say it did — but as a TYPED rejection carrying `liveTasks`, so
 * an affordance can ask the concrete question ("interrupt 3 running
 * subagents?") and resend `interrupt(workspace, true)`.
 *
 * A generic "interrupt rejected" string could not be distinguished from a real
 * refusal without parsing prose, which is exactly what the typed challenge
 * field exists to avoid.
 */
export class InterruptConfirmRequiredError extends Error {
  constructor(readonly liveTasks: number) {
    super(`interrupt needs confirmation: ${liveTasks} live task${liveTasks === 1 ? "" : "s"}`);
    this.name = "InterruptConfirmRequiredError";
  }
}

/**
 * A rejected model request still carries the shim-confirmed selection. The
 * selector must render that value rather than retaining its browser-local
 * tentative choice, so callers need a typed refusal rather than prose to
 * parse.
 */
export class ModelSelectionRejectedError extends Error {
  constructor(readonly selectedModel: string, reason: string) {
    super(`setModel rejected: ${reason}`);
    this.name = "ModelSelectionRejectedError";
  }
}

export interface CreateSessionArgs {
  cwd: string;
  permissionMode: string;
  configDir: string;
  /** "" = fresh; else resume this durable CLI conversation uuid. */
  resumeClaudeSessionId: string;
  fake: boolean;
}

export interface PermissionAnswerArgs {
  permissionRequestId: string;
  allow: boolean;
  updatedInput?: CommandStruct;
  denyMessage: string;
}

export interface DispatchOptions {
  /** Send one encoded frame; returns false when the socket is not open. */
  send: (raw: string) => boolean;
  /** Correlation-id factory (injected for deterministic tests). */
  newRequestId?: () => string;
  /**
   * LOCAL-ONLY diagnostic sink, used for the one thing `log` must never carry:
   * a rejected `clientLog` ack. `log` forwards to the daemon AS a clientLog, so
   * reporting a clientLog failure through it would re-send a clientLog, get
   * another rejection, and loop. The main bootstrap supplies wslog's
   * documented local-only emergency path.
   */
  logLocal: (message: string) => void;
  /**
   * The classified refusal sink (F4). A rejected command used to reach a
   * human through NOTHING on this side: `error` went to a local log and the
   * promise it rejected was swallowed by every caller. This hands the
   * daemon's classified failure to whoever can show it, so "invisible" stops
   * being an acceptable disposition for a rejected prompt.
   */
  onFailure?: (failure: SystemFailure) => void;
}

interface PendingAck {
  command: string;
  resolve: (ack: CommandAck) => void;
  reject: (err: Error) => void;
}

interface CreateWaiter {
  requestId: string;
  cwd: string;
  knownAtStart: Set<string>;
  resolve: (sessionId: string) => void;
  reject: (err: Error) => void;
  settled: boolean;
}

export class CommandDispatcher {
  private readonly pending = new Map<string, PendingAck>();
  private readonly knownSessions = new Set<string>();
  private readonly creates: CreateWaiter[] = [];
  private readonly clientLogAcks = new Set<string>();
  private counter = 0;

  constructor(private readonly opts: DispatchOptions) {
    log("info", "command dispatcher initialized", {
      operation: "command-dispatch.initialize",
      context: { pending_count: this.pending.size, known_session_count: this.knownSessions.size },
    });
  }

  private newId(): string {
    if (this.opts.newRequestId) return this.opts.newRequestId();
    return `fe-${++this.counter}-${Math.random().toString(16).slice(2, 6)}`;
  }

  /** Feed a decoded inbound frame in so acks + SessionViews can correlate. */
  observe(frame: FrontendFrame): void {
    switch (frame.frame.case) {
      case "commandAck":
        // STRUCTURAL NO-REENTRY BOUNDARY. A client-log acknowledgement must be
        // consumed before any diagnostic about the arriving frame. Logging the
        // acknowledgement would send another clientLog and create an endless
        // acknowledgement chain.
        this.onAck(frame.frame.value);
        break;
      case "sessionView":
        logVerbose("info", "command dispatcher selected session correlation", {
          operation: "command-dispatch.observe-session-view",
          context: { agent_repl_session_id: frame.frame.value.sessionId, terminal: frame.frame.value.terminal },
        });
        this.onSessionView(frame.frame.value);
        break;
      case "snapshot":
        log("info", "command dispatcher selected snapshot session correlation", {
          operation: "command-dispatch.observe-snapshot",
          context: { session_count: frame.frame.value.sessions.length },
        });
        for (const sv of frame.frame.value.sessions) this.onSessionView(sv);
        break;
      default:
        logVerbose("info", "command dispatcher ignored frame outside correlation plane", {
          operation: "command-dispatch.observe-ignored",
          context: { frame_case: frame.frame.case },
        });
        break;
    }
  }

  // --- ack-correlated commands ----------------------------------------------

  submitPrompt(workspace: string, text: string, permissionMode = ""): Promise<void> {
    return this.dispatch(workspace, { case: "submitPrompt", text, permissionMode });
  }

  interrupt(workspace: string, confirmAgents = false): Promise<void> {
    return this.dispatch(workspace, { case: "interrupt", confirmAgents });
  }

  permissionAnswer(workspace: string, args: PermissionAnswerArgs): Promise<void> {
    return this.dispatch(workspace, {
      case: "permissionAnswer",
      permissionRequestId: args.permissionRequestId,
      allow: args.allow,
      ...(args.updatedInput !== undefined ? { updatedInput: args.updatedInput } : {}),
      denyMessage: args.denyMessage,
    });
  }

  resync(workspace: string, fromSeq: number): Promise<void> {
    return this.dispatch(workspace, { case: "resync", fromSeq });
  }

  deleteSession(sessionId: string): Promise<void> {
    return this.dispatch("", { case: "deleteSession", sessionId });
  }

  /** Request a model switch and return the shim-confirmed selected model. */
  setModel(workspace: string, model: string): Promise<string> {
    const requestId = this.newId();
    log("info", "command dispatcher dispatching model selection request", {
      operation: "command-dispatch.set-model",
      context: { request_id: requestId, workspace, requested_model: model, pending_count: this.pending.size },
    });
    return new Promise<string>((resolve, reject) => {
      this.pending.set(requestId, {
        command: "setModel",
        resolve: (ack) => {
          try {
            resolve(requiredSelectedModel(ack, "ack"));
          } catch (err) {
            reject(asError(err));
          }
        },
        reject,
      });
      if (!this.opts.send(encodeFrontendCommand({ requestId, workspace, body: { case: "setModel", model } }))) {
        this.pending.delete(requestId);
        reject(new Error("setModel: socket not open"));
      }
    });
  }

  // The held-prompt queue controls (E4). Ack-correlated, unlike clientLog:
  // these are OPERATIONS the user asked for, so a rejection (an entry that has
  // since been delivered or cancelled) must reach the caller instead of
  // vanishing.
  queueForce(workspace: string, entryId: string): Promise<void> {
    return this.dispatch(workspace, { case: "queueForce", entryId });
  }

  queueAccept(workspace: string, entryId: string): Promise<void> {
    return this.dispatch(workspace, { case: "queueAccept", entryId });
  }

  queueCancel(workspace: string, entryId: string): Promise<void> {
    return this.dispatch(workspace, { case: "queueCancel", entryId });
  }

  private dispatch(workspace: string, body: FrontendCommandBody): Promise<void> {
    const requestId = this.newId();
    log("info", "command dispatcher dispatching acknowledgement-correlated command", {
      operation: "command-dispatch.dispatch",
      context: { request_id: requestId, workspace, command: body.case, pending_count: this.pending.size },
    });
    return new Promise<void>((resolve, reject) => {
      this.pending.set(requestId, { command: body.case, resolve: () => resolve(), reject });
      if (!this.opts.send(encodeFrontendCommand({ requestId, workspace, body }))) {
        this.pending.delete(requestId);
        log("error", "command dispatcher command send was rejected", {
          operation: "command-dispatch.dispatch-rejected",
          context: { request_id: requestId, workspace, command: body.case, pending_count: this.pending.size, cause: "socket not open" },
        });
        reject(new Error(`${body.case}: socket not open`));
      }
    });
  }

  /**
   * Mirror one diagnostic line to the daemon (E4). Fire-and-forget by design,
   * and the ONE command that is not promise/ack-correlated:
   *
   * - A log is evidence, not an operation, so nothing waits on its outcome.
   * - An awaited ack would need a rejection handler, and the only way to report
   *   that rejection is a log — which would send another clientLog. Not
   *   awaiting removes the loop rather than damping it.
   *
   * Returns whether the socket accepted the frame, which is what the forwarding
   * logger reports as delivery. Its request id is still REMEMBERED so the ack
   * receipt is recognized instead of being reported as an unknown-request
   * anomaly (which would itself be logged, and forwarded, and acked…).
   */
  clientLog(workspace: string, level: ClientLogBodyLevel, message: string, context?: CommandStruct): boolean {
    const requestId = this.newId();
    const body: ClientLogBody = {
      case: "clientLog",
      level,
      message,
      ...(context !== undefined ? { context } : {}),
    };
    const sent = this.opts.send(encodeFrontendCommand({ requestId, workspace, body }));
    if (sent) this.rememberClientLog(requestId);
    return sent;
  }

  /** Number of client-log acknowledgements retained for correlation. */
  trackedClientLogCount(): number {
    return this.clientLogAcks.size;
  }

  /**
   * Track an outstanding clientLog id, bounded: an unacked log (a socket that
   * dropped mid-flight) must not accumulate ids forever. Evicting the oldest
   * only costs that id its ack recognition, which degrades to one local line.
   */
  private rememberClientLog(requestId: string): void {
    this.clientLogAcks.add(requestId);
    while (this.clientLogAcks.size > MAX_TRACKED_CLIENT_LOGS) {
      const oldest = this.clientLogAcks.values().next();
      if (oldest.done === true) break;
      this.clientLogAcks.delete(oldest.value);
      // The bookkeeping belongs to the logging transport itself. Forwarding
      // its eviction through that same transport would add another tracked id
      // and recurse forever while the set remained over its bound.
      this.opts.logLocal(
        `clientLog acknowledgement tracking evicted request ${oldest.value} ` +
        `(tracked=${this.clientLogAcks.size} maximum=${MAX_TRACKED_CLIENT_LOGS})`,
      );
    }
  }

  private onAck(ack: CommandAck): void {
    if (this.clientLogAcks.delete(ack.requestId)) {
      // A rejected client log is reported LOCALLY ONLY — see `logLocal`.
      if (!ack.ok) {
        // Logger-sink emergency: forwarding through wslog here would create a
        // clientLog acknowledgement loop, so the injected local-only sink owns
        // the sole record.
        this.opts.logLocal(`clientLog rejected: ${ack.error}`);
      }
      return;
    }
    const p = this.pending.get(ack.requestId);
    if (p === undefined) {
      log("warn", `commandAck for unknown request '${ack.requestId}'`, {
        operation: "command-dispatch.ack-unknown-request",
        context: { request_id: ack.requestId, ok: ack.ok, error: ack.error, pending_count: this.pending.size },
      });
      return;
    }
    this.pending.delete(ack.requestId);
    if (ack.ok) {
      log("info", "command dispatcher acknowledgement resolved command", {
        operation: "command-dispatch.ack-resolved",
        context: { request_id: ack.requestId, command: p.command, pending_count: this.pending.size },
      });
      p.resolve(ack);
      return;
    }
    // The CHALLENGE arm, checked before the failure path: it is not a failure
    // and must not reach the failure sink, which would show the user a refusal
    // card for a question the daemon is asking them.
    if (ack.interruptConfirmRequired !== undefined) {
      log("info", "command dispatcher acknowledgement requested interrupt confirmation", {
        operation: "command-dispatch.ack-interrupt-confirmation",
        context: { request_id: ack.requestId, command: p.command, live_task_count: ack.interruptConfirmRequired.liveTasks },
      });
      p.reject(new InterruptConfirmRequiredError(ack.interruptConfirmRequired.liveTasks));
      return;
    }
    // Surface BEFORE rejecting: every caller of these promises swallows the
    // rejection into a log line, so the reject alone reaches no one.
    if (ack.failure !== undefined) this.opts.onFailure?.(ack.failure);
    // CreateSession has its own two-phase waiter. Its rejection flows through
    // failCreate, which owns the one canonical failure record.
    if (p.command !== "createSession") {
      log("error", "command dispatcher acknowledgement rejected command", {
        operation: "command-dispatch.ack-rejected",
        context: { request_id: ack.requestId, command: p.command, error: ack.error, has_classified_failure: ack.failure !== undefined },
      });
    }
    if (p.command === "setModel") {
      try {
        p.reject(new ModelSelectionRejectedError(requiredSelectedModel(ack, "nack"), ack.error));
      } catch (err) {
        p.reject(asError(err));
      }
      return;
    }
    p.reject(new Error(`${p.command} rejected: ${ack.error}`));
  }

  // --- createSession (SessionView-correlated) -------------------------------

  createSession(args: CreateSessionArgs): Promise<string> {
    const requestId = this.newId();
    log("info", "command dispatcher dispatching session creation", {
      operation: "command-dispatch.create-session",
      context: { request_id: requestId, workspace: args.cwd, permission_mode: args.permissionMode, fake: args.fake, known_session_count: this.knownSessions.size },
    });
    return new Promise<string>((resolve, reject) => {
      const waiter: CreateWaiter = {
        requestId,
        cwd: args.cwd,
        knownAtStart: new Set(this.knownSessions),
        resolve,
        reject,
        settled: false,
      };
      this.creates.push(waiter);
      // The ack is a bare receipt: a success ack is a no-op (the SessionView
      // carries the id), but a rejected ack still fails the create.
      this.pending.set(requestId, {
        command: "createSession",
        resolve: () => {},
        reject: (err) => this.failCreate(waiter, err),
      });
      const frame = encodeFrontendCommand({
        requestId,
        workspace: args.cwd,
        body: { case: "createSession", ...args },
      });
      if (!this.opts.send(frame)) {
        this.pending.delete(requestId);
        this.failCreate(waiter, new Error("createSession: socket not open"));
      }
    });
  }

  private onSessionView(sv: SessionView): void {
    if (sv.sessionId !== "") {
      for (const waiter of this.creates) {
        if (waiter.settled || sv.terminal) continue;
        if (waiter.knownAtStart.has(sv.sessionId)) continue;
        if (waiter.cwd !== "" && sv.workspace !== waiter.cwd) continue;
        waiter.settled = true;
        this.pending.delete(waiter.requestId);
        this.removeCreate(waiter);
        log("info", "command dispatcher correlated session creation with session view", {
          operation: "command-dispatch.create-session-resolved",
          context: { request_id: waiter.requestId, workspace: sv.workspace, agent_repl_session_id: sv.sessionId, known_session_count: this.knownSessions.size },
        });
        waiter.resolve(sv.sessionId);
        break; // one SessionView resolves at most one waiter
      }
      this.knownSessions.add(sv.sessionId);
    }
  }

  private failCreate(waiter: CreateWaiter, err: Error): void {
    if (waiter.settled) return;
    waiter.settled = true;
    this.pending.delete(waiter.requestId);
    this.removeCreate(waiter);
    log("error", "command dispatcher session creation failed", {
      operation: "command-dispatch.create-session-failed",
      context: { request_id: waiter.requestId, workspace: waiter.cwd, cause: err.message, pending_count: this.pending.size },
    });
    waiter.reject(err);
  }

  private removeCreate(waiter: CreateWaiter): void {
    const i = this.creates.indexOf(waiter);
    if (i !== -1) this.creates.splice(i, 1);
  }
}
