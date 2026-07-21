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
