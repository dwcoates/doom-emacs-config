/**
 * listeners — the one way a module keeps the functions it tells about an edge.
 *
 * Every subscribe-and-notify in the webapp is the same three things: a set of
 * functions, an add that answers its own unsubscribe, and a notify that calls
 * each one. They were written out by hand at every site, and the sites had
 * drifted: most notified over a snapshot, a few over the live set. This is the
 * one copy, and it always notifies over a SNAPSHOT, so a listener may
 * unsubscribe itself (or another) as it runs, and one subscribed during a
 * notify is first told on the next.
 */

/** A set of listeners taking ARGS. */
export interface Listeners<A extends unknown[]> {
  /**
   * Subscribe FN. Answers its unsubscribe, which answers whether FN was still
   * subscribed, so a caller that does more on the last unsubscribe can tell.
   */
  add(fn: (...args: A) => void): () => boolean;
  /** Call every listener subscribed now with ARGS, in the order they subscribed. */
  notify(...args: A): void;
  /** How many listeners are subscribed. */
  readonly size: number;
  /** Unsubscribe every listener. */
  clear(): void;
}

/** An empty set of listeners taking ARGS. */
export function createListeners<A extends unknown[] = []>(): Listeners<A> {
  const fns = new Set<(...args: A) => void>();
  return {
    add(fn) {
      fns.add(fn);
      return () => fns.delete(fn);
    },
    notify(...args) {
      for (const fn of [...fns]) fn(...args);
    },
    get size() {
      return fns.size;
    },
    clear() {
      fns.clear();
    },
  };
}
