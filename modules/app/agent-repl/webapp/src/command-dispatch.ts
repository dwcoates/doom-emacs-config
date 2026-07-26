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

import type { CommandAck, FrontendFrame, SessionView } from "./frontend-proto.js";
import {
  encodeFrontendCommand,
  type ClientLogBody,
  type ClientLogBodyLevel,
  type CommandStruct,
  type FrontendCommandBody,
} from "./frontend-command.js";

/**
 * How many in-flight `clientLog` request ids are remembered for ack
 * recognition. Bounded so a socket that drops mid-flight cannot leak ids.
 */
const MAX_TRACKED_CLIENT_LOGS = 256;

/** The default local-only sink for a rejected clientLog ack. */
function defaultLocalLog(message: string): void {
  console.warn(message);
}

export interface CreateSessionArgs {
  cwd: string;
  model: string;
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
  log?: (level: "warn" | "error", message: string) => void;
  /**
   * LOCAL-ONLY diagnostic sink, used for the one thing `log` must never carry:
   * a rejected `clientLog` ack. `log` forwards to the daemon AS a clientLog, so
   * reporting a clientLog failure through it would re-send a clientLog, get
   * another rejection, and loop. Defaults to the console.
   */
  logLocal?: (message: string) => void;
}

interface PendingAck {
  command: string;
  resolve: () => void;
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

  constructor(private readonly opts: DispatchOptions) {}

  private newId(): string {
    if (this.opts.newRequestId) return this.opts.newRequestId();
    return `fe-${++this.counter}-${Math.random().toString(16).slice(2, 6)}`;
  }

  /** Feed a decoded inbound frame in so acks + SessionViews can correlate. */
  observe(frame: FrontendFrame): void {
    switch (frame.frame.case) {
      case "commandAck":
        this.onAck(frame.frame.value);
        break;
      case "sessionView":
        this.onSessionView(frame.frame.value);
        break;
      case "snapshot":
        for (const sv of frame.frame.value.sessions) this.onSessionView(sv);
        break;
      default:
        break;
    }
  }

  // --- ack-correlated commands ----------------------------------------------

  submitPrompt(workspace: string, text: string, permissionMode = ""): Promise<void> {
    return this.dispatch(workspace, { case: "submitPrompt", text, permissionMode });
  }

  interrupt(workspace: string, hard = false): Promise<void> {
    return this.dispatch(workspace, { case: "interrupt", hard });
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

  /**
   * Attest that the feed has been PAINTED through `throughSeq`.
   *
   * Sent from the completion of a render that actually drew, never from the
   * arrival of the frames describing one: receiving the history and drawing
   * it are different facts, and only the second earns the ready state. A
   * webview that cannot paint simply never calls this, and the workspace
   * stays on the compromised-route state — which is the honest answer, not a
   * gap to be papered over with a timeout.
   */
  paintAck(workspace: string, throughSeq: number): Promise<void> {
    return this.dispatch(workspace, { case: "paintAck", throughSeq });
  }

  deleteSession(sessionId: string): Promise<void> {
    return this.dispatch("", { case: "deleteSession", sessionId });
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
    return new Promise<void>((resolve, reject) => {
      this.pending.set(requestId, { command: body.case, resolve, reject });
      if (!this.opts.send(encodeFrontendCommand({ requestId, workspace, body }))) {
        this.pending.delete(requestId);
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
    }
  }

  private onAck(ack: CommandAck): void {
    if (this.clientLogAcks.delete(ack.requestId)) {
      // A rejected client log is reported LOCALLY ONLY — see `logLocal`.
      if (!ack.ok) {
        (this.opts.logLocal ?? defaultLocalLog)(`clientLog rejected: ${ack.error}`);
      }
      return;
    }
    const p = this.pending.get(ack.requestId);
    if (p === undefined) {
      this.opts.log?.("warn", `commandAck for unknown request '${ack.requestId}'`);
      return;
    }
    this.pending.delete(ack.requestId);
    if (ack.ok) p.resolve();
    else p.reject(new Error(`${p.command} rejected: ${ack.error}`));
  }

  // --- createSession (SessionView-correlated) -------------------------------

  createSession(args: CreateSessionArgs): Promise<string> {
    const requestId = this.newId();
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
    waiter.reject(err);
  }

  private removeCreate(waiter: CreateWaiter): void {
    const i = this.creates.indexOf(waiter);
    if (i !== -1) this.creates.splice(i, 1);
  }
}
