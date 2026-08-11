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
  /**
   * Whether anything has changed for this page since its last completed
   * resync — a moved applied mark, a rotated fence, a changed daemon identity,
   * a detected gap.
   *
   * THE HEARTBEAT'S COST IS WHAT THIS BOUNDS. A resync is answered with a full
   * `StateSnapshot`, so an unconditional tick asks the daemon to rebuild and
   * ship the whole world every interval for every open page whether or not
   * anything moved: a sampled tail showed 3,140 `re-armed on
   * recovery_heartbeat` lines and 233 resyncs in a two-minute window with
   * nothing wrong. When this reports false the page is current AND caught up,
   * and the snapshot it would receive is the one it already holds.
   *
   * Absent, every tick resyncs, which is the old behaviour.
   */
  hasPendingWork?: () => boolean;
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

/**
 * How many quiet heartbeats may be skipped before one goes out regardless.
 *
 * THE SKIP IS AN OPTIMIZATION, NOT A NEW GUARANTEE. `hasPendingWork` answers
 * from what this page can see, and the whole reason this module exists is the
 * page that cannot see that it is behind — a webview WebKit froze, a daemon
 * replaced under a socket that never cycled. Letting a clean answer suppress
 * the probe FOREVER would hand that page's recovery back to whoever next looks
 * at it, which is the defect the module was written to end.
 *
 * So the unconditional probe survives at a rate set by what it is for: catching
 * a page that has gone quietly wrong, which is rare and not urgent to the
 * second. At the default interval this is one full snapshot per page per
 * minute instead of twelve.
 */
export const RECOVERY_FULL_CHECK_EVERY_TICKS = 12;

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
  /** Consecutive heartbeats skipped because this page had nothing to catch up on. */
  private skippedTicks = 0;

  constructor(
    private readonly opts: BackgroundRecoveryOptions,
    private readonly timers: RecoveryTimerHost,
    private readonly intervalMs: number = RECOVERY_INTERVAL_MS,
    private readonly fullCheckEveryTicks: number = RECOVERY_FULL_CHECK_EVERY_TICKS,
  ) {}

  /** Heartbeats skipped so far because the page was current and caught up. */
  get skippedHeartbeats(): number {
    return this.skippedTicks;
  }

  /**
   * One heartbeat tick.
   *
   * A TICK IS A GUESS, AND EVERY OTHER CALLER OF `recover` IS EVIDENCE. A
   * socket that came back, a page that became visible, the host's own link-up
   * hook: each of those is a fact about the world that this page has just
   * learned, and each is owed its resync unconditionally. The heartbeat has
   * learned nothing — it is a clock — so it is the one caller that may be
   * talked out of the full snapshot its resync provokes, and the only one this
   * gate applies to.
   *
   * The dial half is NEVER skipped. It is local, it costs nothing, and a page
   * whose socket is down is precisely the page a heartbeat is for.
   */
  private heartbeat(): void {
    this.opts.ensureConnected();
    if (!this.opts.isCurrent()) {
      this.skippedTicks = 0;
      this.opts.log?.("info", "recovery: dialled on recovery_heartbeat; socket not current yet");
      return;
    }
    const pending = this.opts.hasPendingWork?.() ?? true;
    if (!pending && this.skippedTicks + 1 < this.fullCheckEveryTicks) {
      this.skippedTicks += 1;
      // Deliberately unlogged. This is the quiet, expected case — one line per
      // page per interval is the same telemetry flood the resyncs were.
      return;
    }
    if (!pending) {
      this.opts.log?.(
        "info",
        `recovery: heartbeat asking anyway after ${this.skippedTicks} quiet ticks; ` +
          "a page cannot always see that it is behind",
      );
    }
    this.skippedTicks = 0;
    this.opts.resync("recovery_heartbeat");
    this.opts.clearConnectionBanner();
  }

  /**
   * Repair now, naming why.
   *
   * Returns whether the resync half ran, which is the only half that needs a
   * current socket; a dial-only outcome is the honest answer while the
   * transport is still coming back, and the next tick finishes the job.
   */
  recover(reason: string): boolean {
    // An evidence-driven repair discharges the quiet-tick debt: the full
    // check this counter is counting down to is about to happen anyway.
    this.skippedTicks = 0;
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
      this.heartbeat();
    }, this.intervalMs);
  }

  /** Stop the heartbeat. Idempotent. */
  stop(): void {
    if (this.timer === null) return;
    this.timers.clearInterval(this.timer);
    this.timer = null;
  }
}
