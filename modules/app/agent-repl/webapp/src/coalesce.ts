/**
 * Render coalescing: many render requests, one render per animation frame.
 *
 * Every WebSocket message that changes the store asks for a render, and a
 * message burst — the buffered backlog draining when a hidden webview's
 * workspace is switched back to, a reconnect's replay, a fast delta
 * stream — otherwise runs a full feed render per message, with paints
 * interleaved. That churn is the switch-in jitter. Riding the requests on
 * requestAnimationFrame collapses a burst into one render per paint.
 *
 * A hidden page cannot use rAF at all. WKWebView suspends animation
 * frames for an invisible page, so riding rAF while hidden does not
 * "apply frames silently" — it applies nothing, and every later
 * schedule() collapses into the same never-serviced frame. The whole
 * interval's worth of store movement then lands in one render at the
 * moment the workspace is switched back to, which is the visible
 * switch-in delay. While hidden the coalescer therefore rides an eager
 * deferral (a timer) instead: WKWebView throttles hidden-page timers to
 * roughly one tick a second, which is exactly the wanted bound — one
 * coalesced apply per tick rather than an unbounded backlog. The rAF
 * path is untouched whenever the page is visible.
 *
 * Nothing here touches the DOM. Both schedulers are injected, which is
 * what lets the collapse be tested: the webapp's test deps carry no DOM.
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
 * The paint-independent scheduler renders ride while the page is hidden.
 * Unlike rAF this keeps running when nothing is on screen, so the store's
 * movement reaches the DOM as it arrives instead of piling up.
 */
export interface EagerHost {
  scheduleEager(callback: () => void): number;
  cancelEager(id: number): void;
}

/**
 * `window` as an EagerHost. A zero-delay timer rather than a microtask:
 * a microtask cannot be cancelled, and the timer's hidden-page throttling
 * (~1s in WKWebView) is a feature here — it coalesces a hidden interval's
 * updates into one apply per tick instead of one apply per message.
 */
export function windowEagerHost(win: Window): EagerHost {
  return {
    scheduleEager: (callback) => win.setTimeout(callback, 0),
    cancelEager: (id) => win.clearTimeout(id),
  };
}

/** Which scheduler the currently-pending render is riding. */
export type RenderKind = "frame" | "eager";

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
  /**
   * A pending EAGER render older than this counts as stalled (ms). The
   * hidden path is deliberately slack: a hidden-page timer legitimately
   * ticks about once a second, so the frame threshold would report that
   * healthy throttling as a defect. It is still watchdogged — a hidden
   * host that has stopped running timers altogether is a real wedge and
   * must not go unreported.
   */
  eagerStallAfterMs?: number;
  /** Fired once per stall episode, from the schedule() that detects it. */
  onStall?: (pendingMs: number, kind: RenderKind) => void;
  /** Fired when a stalled render finally runs, with its total age. */
  onStallRecover?: (pendingMs: number, kind: RenderKind) => void;
  /**
   * True while the page is hidden. With an EAGERHOST alongside it, this
   * routes scheduling off rAF for as long as it reports hidden; without
   * either, the coalescer is rAF-only exactly as it was.
   */
  isHidden?: () => boolean;
  /** The scheduler used in place of rAF while ISHIDDEN reports hidden. */
  eagerHost?: EagerHost;
}

const STALL_AFTER_MS_DEFAULT = 1000;
const EAGER_STALL_AFTER_MS_DEFAULT = 5000;

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
  private kind: RenderKind | null = null;
  private armedAt = 0;
  private stalled = false;

  constructor(
    private readonly host: FrameHost,
    private readonly render: () => void,
    private readonly opts: RenderCoalescerOptions = {},
  ) {}

  /** Ask for a render; requests before the next one runs collapse into it. */
  schedule(): void {
    if (this.pending !== null) {
      // A render armed while visible, on a page that has since gone
      // hidden, is riding a scheduler that will not run again until the
      // page comes back. Move it onto the eager path rather than letting
      // it — and every request collapsing into it — wait for the reveal.
      if (this.kind === "frame" && this.hiddenPathAvailable()) {
        this.dropPending();
        this.arm(true);
        return;
      }
      this.checkStall();
      return;
    }
    this.arm(false);
  }

  /** Drop the pending render, if any. */
  cancel(): void {
    if (this.pending === null) return;
    this.dropPending();
    this.stalled = false;
  }

  /**
   * Run any pending render NOW, synchronously. Called when the page
   * becomes visible: whatever is on screen is older than the store, and
   * the first visible frame must show the current state rather than
   * whatever the last hidden tick happened to leave behind. Reports
   * whether it rendered, so a caller with nothing to flush can fall back
   * to an ordinary scheduled render.
   */
  flush(): boolean {
    if (this.pending === null) return false;
    const kind = this.kind;
    this.dropPending();
    this.runRender(kind);
    return true;
  }

  /** Arm the pending render on whichever scheduler currently applies. */
  private arm(keepArmedAt: boolean): void {
    if (!keepArmedAt) this.armedAt = this.opts.now?.() ?? 0;
    if (this.hiddenPathAvailable()) {
      this.kind = "eager";
      // Non-null: hiddenPathAvailable() is exactly the check that it is set.
      this.pending = this.opts.eagerHost!.scheduleEager(() => {
        this.pending = null;
        this.runRender("eager");
      });
      return;
    }
    this.kind = "frame";
    this.pending = this.host.requestAnimationFrame(() => {
      this.pending = null;
      this.runRender("frame");
    });
  }

  /** True when the page is hidden AND an eager scheduler was supplied. */
  private hiddenPathAvailable(): boolean {
    return this.opts.eagerHost !== undefined && this.opts.isHidden?.() === true;
  }

  /** Clear the pending render from its own scheduler, keeping stall state. */
  private dropPending(): void {
    if (this.pending === null) return;
    if (this.kind === "eager") {
      // Non-null: only the eager path ever sets this kind.
      this.opts.eagerHost!.cancelEager(this.pending);
    } else {
      this.host.cancelAnimationFrame(this.pending);
    }
    this.pending = null;
    this.kind = null;
  }

  /** Report any stall recovery, then render. */
  private runRender(kind: RenderKind | null): void {
    this.kind = null;
    if (this.stalled) {
      this.stalled = false;
      this.opts.onStallRecover?.((this.opts.now?.() ?? 0) - this.armedAt, kind ?? "frame");
    }
    this.render();
  }

  /** Report (once per episode) a pending render the host never serviced. */
  private checkStall(): void {
    if (this.stalled || this.opts.now === undefined || this.opts.onStall === undefined) {
      return;
    }
    const kind = this.kind ?? "frame";
    const threshold =
      kind === "eager"
        ? (this.opts.eagerStallAfterMs ?? EAGER_STALL_AFTER_MS_DEFAULT)
        : (this.opts.stallAfterMs ?? STALL_AFTER_MS_DEFAULT);
    const age = this.opts.now() - this.armedAt;
    if (age >= threshold) {
      this.stalled = true;
      this.opts.onStall(age, kind);
    }
  }
}
