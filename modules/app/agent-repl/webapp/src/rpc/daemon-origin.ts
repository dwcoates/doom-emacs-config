/**
 * daemon-origin — WHICH DAEMON A DOCUMENT DIALS, as a value it is told.
 *
 * A workspace page dials the daemon that served it, at its own origin, for its
 * whole life. The web view host does not: it outlives the daemon that served
 * it, because a handover moves the workspaces to a successor without
 * reloading the host (lisp/webhost.el, `setDaemon`), so the host's sidebar
 * dials whichever daemon Emacs last named. The transport reads the origin at
 * every call (rpc/transport.ts), so a stream that ends at the old daemon's
 * departure reopens at the successor.
 */

/** The origin a document's daemon calls go to, and the one way to move it. */
export interface DaemonOrigin {
  /** The origin every call goes to now. */
  current(): string;
  /** Point every later call at ORIGIN. Answers whether it moved. */
  set(origin: string): boolean;
}

/** A daemon origin starting at INITIAL. */
export function createDaemonOrigin(initial: string): DaemonOrigin {
  let origin = initial;
  return {
    current: () => origin,
    set(next) {
      if (next === origin) return false;
      origin = next;
      return true;
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
