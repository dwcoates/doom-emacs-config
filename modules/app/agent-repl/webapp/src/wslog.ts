/**
 * Client→daemon diagnostic forwarding (§2.15).
 *
 * The webapp runs inside an Emacs webview whose JS console nobody can
 * see and nothing persists — a delivery-path failure there (seq-gap
 * loop, lost replay-request, stalled rAF) previously left no evidence
 * anywhere. Every line logged here goes to the local console ALWAYS,
 * and is best-effort mirrored to the daemon as a `client-log` frame,
 * which the daemon writes to its on-disk log.
 *
 * Forwarding is rate-limited (the daemon clamps per-message size; this
 * caps message COUNT) so a pathological log loop cannot flood the
 * daemon's disk. The console side is never limited — locally attached
 * debuggers deserve the full stream.
 */
import { ClientLogCmd } from "./protocol.js";

export type ClientLogLevel = ClientLogCmd["level"];

/** Forwarding budget: at most MAX_FORWARDS_PER_WINDOW per window. */
export const FORWARD_WINDOW_MS = 60_000;
export const MAX_FORWARDS_PER_WINDOW = 60;

export class ForwardingLogger {
  private windowStart = 0;
  private forwarded = 0;
  private suppressed = false;

  /**
   * SEND pushes one frame toward the daemon, reporting delivery (a
   * closed socket returns false — the line then lives on console
   * only). CONSOLE_FN is injectable for tests; NOW is the rate-limit
   * clock.
   */
  constructor(
    private readonly send: (cmd: ClientLogCmd) => boolean,
    private readonly consoleFn: (level: ClientLogLevel, line: string) => void = defaultConsole,
    private readonly now: () => number = () => Date.now(),
  ) {}

  log(level: ClientLogLevel, message: string): void {
    this.consoleFn(level, message);
    const t = this.now();
    if (t - this.windowStart >= FORWARD_WINDOW_MS) {
      this.windowStart = t;
      this.forwarded = 0;
      this.suppressed = false;
    }
    if (this.forwarded >= MAX_FORWARDS_PER_WINDOW) {
      if (!this.suppressed) {
        this.suppressed = true;
        // The notice itself is the window's last forward, so the daemon
        // log shows WHERE the stream was cut rather than just thinning.
        this.send({
          type: "client-log",
          level: "warn",
          message: `client-log rate limit (${MAX_FORWARDS_PER_WINDOW}/${FORWARD_WINDOW_MS}ms) reached — suppressing forwards for the rest of the window`,
        });
      }
      return;
    }
    this.forwarded++;
    this.send({ type: "client-log", level, message });
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
// constructor. main.ts installs the real ForwardingLogger at boot; until
// then lines fall back to the console so nothing is ever dropped.
// wslog imports only the protocol leaf, so importing { log } from here
// can never create an import cycle.
// ---------------------------------------------------------------------------

let active: ForwardingLogger | null = null;

/** Install (or clear, with null) the app-wide logger. */
export function setLogger(logger: ForwardingLogger | null): void {
  active = logger;
}

/** Log through the installed logger, or console-only before boot. */
export function log(level: ClientLogLevel, message: string): void {
  if (active !== null) active.log(level, message);
  else defaultConsole(level, message);
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

export function logDedup(key: string, level: ClientLogLevel, message: string): void {
  if (dedupLast.get(key) === message) return;
  dedupLast.set(key, message);
  log(level, message);
}

/** Re-arm KEY so its next logDedup fires even with an identical message. */
export function clearDedup(key: string): void {
  dedupLast.delete(key);
}

/** Test hook: drop the installed logger and all dedup state. */
export function resetLoggingForTests(): void {
  active = null;
  dedupLast.clear();
}
