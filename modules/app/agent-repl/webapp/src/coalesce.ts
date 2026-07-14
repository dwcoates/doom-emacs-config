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
 * Collapses a burst of render requests into one RENDER call per animation
 * frame. `schedule()` is cheap to call once per message; `cancel()` drops
 * a pending render when something else has just painted the same state
 * (the restored render) or the state is about to be discarded (a session
 * rebind).
 */
export class RenderCoalescer {
  private pending: number | null = null;

  constructor(
    private readonly host: FrameHost,
    private readonly render: () => void,
  ) {}

  /** Ask for a render; requests before the next frame collapse into one. */
  schedule(): void {
    if (this.pending !== null) return;
    this.pending = this.host.requestAnimationFrame(() => {
      this.pending = null;
      this.render();
    });
  }

  /** Drop the pending render, if any. */
  cancel(): void {
    if (this.pending === null) return;
    this.host.cancelAnimationFrame(this.pending);
    this.pending = null;
  }
}
