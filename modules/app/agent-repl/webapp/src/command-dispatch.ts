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
  type CommandStruct,
  type FrontendCommandBody,
} from "./frontend-command.js";

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

  deleteSession(sessionId: string): Promise<void> {
    return this.dispatch("", { case: "deleteSession", sessionId });
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

  private onAck(ack: CommandAck): void {
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
