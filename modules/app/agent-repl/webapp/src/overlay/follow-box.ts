/**
 * follow-box — A FIXED OVERLAY KEPT ON ANOTHER ELEMENT'S RECTANGLE.
 *
 * The feed's overlays (the news digest, the image overlay) cover the feed's
 * own box and nothing else, so the topbar, the rail and the footer stay in
 * view: each is a `position: fixed` host whose top, left, width and height are
 * copied from the box it covers. The copy is refreshed whenever that box
 * resizes and whenever the window does, and only while the host is shown.
 */

/** A host kept on a target's rectangle. */
export interface BoxFollower {
  /** Copy the target's rectangle onto the host now (a no-op while it is hidden). */
  readonly follow: () => void;
  /** Stop following. */
  readonly dispose: () => void;
}

/** Keep HOST on TARGET's rectangle while HOST is shown. */
export function followBox(host: HTMLElement, target: HTMLElement): BoxFollower {
  const follow = (): void => {
    if (host.hidden) return;
    const rect = target.getBoundingClientRect();
    host.style.top = `${rect.top}px`;
    host.style.left = `${rect.left}px`;
    host.style.width = `${rect.width}px`;
    host.style.height = `${rect.height}px`;
  };
  const observer = new ResizeObserver(follow);
  observer.observe(target);
  window.addEventListener("resize", follow);
  return {
    follow,
    dispose(): void {
      observer.disconnect();
      window.removeEventListener("resize", follow);
    },
  };
}
