/**
 * Render coalescing: many render requests, one render per animation frame.
 *
 * Every WebSocket message that changes the store asks for a render, and a
 * message burst — the buffered backlog draining when a hidden webview's
 * workspace is switched back to, a reconnect's replay, a fast delta
 * stream — otherwise runs a full feed render per message, with paints
 * interleaved. That churn is the switch-in jitter. Riding the requests on
 * requestAnimationFrame collapses a burst into one render per paint, and
 * has a second property the hidden webview wants: rAF does not fire while
 * the page is invisible, so a hidden feed applies frames silently and
 * paints once when it is next on screen.
 *
 * Nothing here touches the DOM. The scheduler is injected, which is what
 * lets the collapse be tested: the webapp's test dependencies carry no DOM.
 */

/** The frame scheduler renders ride on (`window`, in the browser). */
export interface FrameHost {
  requestAnimationFrame(callback: () => void): number;
  cancelAnimationFrame(id: number): void;
}

/** `window` as a FrameHost: the browser's own paint scheduler. */
export function windowFrameHost(win: Window): FrameHost {
  return {
    requestAnimationFrame: (callback) => win.requestAnimationFrame(callback),
    cancelAnimationFrame: (id) => win.cancelAnimationFrame(id),
  };
}

/**
 * Stall watchdog knobs. A frame is normally serviced within ~16ms, so a
 * request still pending after a second while more schedule() calls keep
 * arriving means the host has stopped running rAF callbacks — the
 * signature of an Emacs webview whose WebKit process believes it is
 * occluded after an xwidget reparent: compositor CSS animations keep
 * breathing while the main-thread rAF (and with it every feed render)
 * is suspended, which reads as "cards stopped arriving".
 */
export interface RenderCoalescerOptions {
  /** Clock for stall detection (performance.now in the browser). */
  now?: () => number;
  /** A pending frame older than this counts as stalled (ms). */
  stallAfterMs?: number;
  /** Fired once per stall episode, from the schedule() that detects it. */
  onStall?: (pendingMs: number) => void;
  /** Fired when a stalled frame finally runs, with its total age. */
  onStallRecover?: (pendingMs: number) => void;
}

const STALL_AFTER_MS_DEFAULT = 1000;

/**
 * Collapses a burst of render requests into one RENDER call per animation
 * frame. `schedule()` is cheap to call once per message; `cancel()` drops
 * a pending render when something else has just painted the same state
 * (the restored render) or the state is about to be discarded (a session
 * rebind).
 *
 * With a clock in OPTS the coalescer also watchdogs its own scheduler:
 * schedule() calls that keep landing on a long-pending frame report a
 * stall (see RenderCoalescerOptions), because a lost/suspended rAF
 * otherwise wedges the feed silently — every later schedule() no-ops
 * against the stuck `pending` while the store keeps applying frames.
 */
export class RenderCoalescer {
  private pending: number | null = null;
  private armedAt = 0;
  private stalled = false;

  constructor(
    private readonly host: FrameHost,
    private readonly render: () => void,
    private readonly opts: RenderCoalescerOptions = {},
  ) {}

  /** Ask for a render; requests before the next frame collapse into one. */
  schedule(): void {
    if (this.pending !== null) {
      this.checkStall();
      return;
    }
    this.armedAt = this.opts.now?.() ?? 0;
    this.pending = this.host.requestAnimationFrame(() => {
      this.pending = null;
      if (this.stalled) {
        this.stalled = false;
        this.opts.onStallRecover?.((this.opts.now?.() ?? 0) - this.armedAt);
      }
      this.render();
    });
  }

  /** Drop the pending render, if any. */
  cancel(): void {
    if (this.pending === null) return;
    this.host.cancelAnimationFrame(this.pending);
    this.pending = null;
    this.stalled = false;
  }

  /** Report (once per episode) a pending frame the host never serviced. */
  private checkStall(): void {
    if (this.stalled || this.opts.now === undefined || this.opts.onStall === undefined) {
      return;
    }
    const age = this.opts.now() - this.armedAt;
    if (age >= (this.opts.stallAfterMs ?? STALL_AFTER_MS_DEFAULT)) {
      this.stalled = true;
      this.opts.onStall(age);
    }
  }
}
