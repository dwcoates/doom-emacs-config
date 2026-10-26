/**
 * daemon-origin — WHICH DAEMON A DOCUMENT DIALS, as a value it is told.
 *
 * A workspace page dials the daemon that served it, at its own origin, for its
 * whole life. The web view host does not: it outlives the daemon that served
 * it, because a handover moves the workspaces to a successor without
 * reloading the host (lisp/webhost.el, `setDaemon`), so the host's sidebar
 * dials whichever daemon Emacs last named. The transport reads the origin at
 * every call (rpc/transport.ts), and every standing stream is TOLD the origin
 * moved (`onMoved`) and reopens at the successor at once.
 *
 * A STREAM NEVER WAITS FOR THE OLD DAEMON TO LEAVE. It used to: the sidebar's
 * roster reopened at the successor only when its stream at the old daemon
 * ended, and a handover a workspace refused to finish leaves the old daemon
 * serving for good. Its roster stood still for every workspace that had moved,
 * so a prompt sent to one turned no row red (2026-10-26, the 0.260.0 deploy).
 */

/** The origin a document's daemon calls go to, and the one way to move it. */
export interface DaemonOrigin {
  /** The origin every call goes to now. */
  current(): string;
  /** Point every later call at ORIGIN, telling every `onMoved` listener. Answers whether it moved. */
  set(origin: string): boolean;
  /**
   * Run FN, synchronously inside `set`, every time the origin moves. Answers
   * the unsubscribe.
   */
  onMoved(fn: () => void): () => void;
}

/** A daemon origin starting at INITIAL. */
export function createDaemonOrigin(initial: string): DaemonOrigin {
  let origin = initial;
  const listeners = new Set<() => void>();
  return {
    current: () => origin,
    set(next) {
      if (next === origin) return false;
      origin = next;
      // A copy: a listener may unsubscribe itself as it runs.
      for (const fn of [...listeners]) fn();
      return true;
    },
    onMoved(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}

/** Whether VALUE is an http origin a daemon serves: scheme, host and port only. */
export function isDaemonOrigin(value: unknown): value is string {
  if (typeof value !== "string") return false;
  try {
    const url = new URL(value);
    return url.protocol === "http:" && url.origin === value;
  } catch {
    return false;
  }
}
