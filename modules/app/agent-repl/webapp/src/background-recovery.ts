/**
 * BackgroundRecovery — state repair that does not wait to be looked at.
 *
 * THE DEFECT THIS ENDS: after a daemon bounce, a HIDDEN webview sat on
 * "lost the connection to the daemon; reconnecting close=1005" until the user
 * switched to its workspace, at which point the page repopulated at once.
 * Nothing was broken about the transport — the repair was simply GATED ON
 * BEING SEEN. Every trigger that re-dialled and re-asked for history was a
 * visibility or focus event (`visibilitychange`, `window.focus`), so a page
 * nobody was looking at had no trigger at all.
 *
 * The rule this module encodes: RECOVERY IS NEVER SCHEDULED ON A PAINT.
 * `requestAnimationFrame` does not run in a backgrounded WKWebView, so any
 * repair riding a frame is a repair that happens when the user arrives, not
 * when the daemon returns. Recovery therefore rides only timers and socket
 * events, which keep running in a hidden page. Only RENDERING may defer to
 * visibility (see RenderCoalescer, which still does).
 *
 * `recover()` is the single repair path and takes no view of who called it:
 * a heartbeat tick, a socket that came back, a page becoming visible, and a
 * focus event all mean the same thing — this page may be behind — and all run
 * exactly the same steps, so a hidden page is repaired by the identical code
 * that repairs a visible one.
 */

/** How this module reports itself; mirrors the app's client-log levels. */
export type BackgroundRecoveryLogLevel = "info" | "warn" | "error";

export interface BackgroundRecoveryOptions {
  /** Dial if the transport state calls for one; a no-op on a healthy socket. */
  ensureConnected: () => void;
  /** Whether the socket carries authoritative state right now. */
  isCurrent: () => boolean;
  /**
   * Ask for the delta above this page's applied high-water mark. Called only
   * on a current socket — a command over a socket that is not current has
   * nothing to carry it.
   */
  resync: (reason: string) => void;
  /**
   * Retract the "lost the connection to the daemon" card. Called on every
   * completed repair rather than only on the socket-restore edge, so a page
   * that recovered while hidden is not still showing the banner when the user
   * arrives.
   */
  clearConnectionBanner: () => void;
  log?: (level: BackgroundRecoveryLogLevel, message: string) => void;
}

/**
 * Heartbeat period. Short enough that a hidden page rejoins a bounced daemon
 * on the order of seconds rather than whenever it is next visited, long enough
 * that an idle page's resyncs are not traffic. Each tick is a no-op on a
 * current socket that is already caught up: `resync` asks for the delta above
 * the applied mark, which is empty when nothing moved.
 */
export const RECOVERY_INTERVAL_MS = 5_000;

/** Minimal timer surface, injectable so tests drive the clock themselves. */
export interface RecoveryTimerHost {
  setInterval: (callback: () => void, ms: number) => number;
  clearInterval: (handle: number) => void;
}

/** The window's own timers — the ones a hidden page keeps running. */
export function windowRecoveryTimerHost(win: Window): RecoveryTimerHost {
  return {
    setInterval: (callback, ms) => win.setInterval(callback, ms),
    clearInterval: (handle) => win.clearInterval(handle),
  };
}

export class BackgroundRecovery {
  private timer: number | null = null;

  constructor(
    private readonly opts: BackgroundRecoveryOptions,
    private readonly timers: RecoveryTimerHost,
    private readonly intervalMs: number = RECOVERY_INTERVAL_MS,
  ) {}

  /**
   * Repair now, naming why.
   *
   * Returns whether the resync half ran, which is the only half that needs a
   * current socket; a dial-only outcome is the honest answer while the
   * transport is still coming back, and the next tick finishes the job.
   */
  recover(reason: string): boolean {
    this.opts.ensureConnected();
    if (!this.opts.isCurrent()) {
      this.opts.log?.("info", `recovery: dialled on ${reason}; socket not current yet`);
      return false;
    }
    this.opts.resync(reason);
    // The banner is a claim about the CONNECTION, and this page has just
    // proved the connection carries authoritative state again. Clearing it
    // here (not on a render) is what makes the clear independent of whether
    // anyone is looking at the page.
    this.opts.clearConnectionBanner();
    return true;
  }

  /** Begin the visibility-independent heartbeat. Idempotent. */
  start(): void {
    if (this.timer !== null) return;
    this.timer = this.timers.setInterval(() => {
      this.recover("recovery_heartbeat");
    }, this.intervalMs);
  }

  /** Stop the heartbeat. Idempotent. */
  stop(): void {
    if (this.timer === null) return;
    this.timers.clearInterval(this.timer);
    this.timer = null;
  }
}
