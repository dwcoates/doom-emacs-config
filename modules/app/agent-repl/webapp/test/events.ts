/**
 * A test waits for the EVENT it needs, never for time or by polling (the
 * agent-repl-tests skill, "Synchronization: events, never time"). These are
 * the two events most webapp tests wait on: a fake rpc being called, and the
 * DOM reaching a state.
 */

/** Every call a fake rpc received, and a wait for the Nth. */
export interface Recorded<T> {
  /** Every call so far, in order. */
  readonly calls: T[];
  /** Records one call: the fake rpc's body calls it. */
  record(call: T): void;
  /** Resolves once COUNT calls have been recorded (at once if they have). */
  reached(count: number): Promise<void>;
  /**
   * Resolves with the first call PREDICATE accepts: one already in `calls`,
   * or the next one recorded. A test that empties `calls` (`length = 0`)
   * waits on what is recorded after.
   */
  seen(predicate: (call: T) => boolean): Promise<T>;
}

/** A recorder for one fake rpc's calls. */
export function recorded<T>(): Recorded<T> {
  const calls: T[] = [];
  const counts: { count: number; resolve: () => void }[] = [];
  const matches: { predicate: (call: T) => boolean; resolve: (call: T) => void }[] = [];
  return {
    calls,
    record(call: T): void {
      calls.push(call);
      for (let index = counts.length - 1; index >= 0; index--) {
        const waiter = counts[index];
        if (waiter !== undefined && calls.length >= waiter.count) {
          counts.splice(index, 1);
          waiter.resolve();
        }
      }
      for (let index = matches.length - 1; index >= 0; index--) {
        const waiter = matches[index];
        if (waiter !== undefined && waiter.predicate(call)) {
          matches.splice(index, 1);
          waiter.resolve(call);
        }
      }
    },
    reached(count: number): Promise<void> {
      if (calls.length >= count) return Promise.resolve();
      return new Promise((resolve) => {
        counts.push({ count, resolve });
      });
    },
    seen(predicate: (call: T) => boolean): Promise<T> {
      const already = calls.find(predicate);
      if (already !== undefined) return Promise.resolve(already);
      return new Promise((resolve) => {
        matches.push({ predicate, resolve });
      });
    },
  };
}

/** The fallback bound on a DOM wait: only a state that never arrives meets it. */
export const DOM_WAIT_BOUND_MS = 2000;

/**
 * Resolves with READ's answer once it is not null, reading it again at every
 * DOM mutation under ROOT. BOUND_MS is the fallback for a state that never
 * arrives; the rejection names WHAT and DESCRIBE's account of the DOM then.
 */
export function awaitDom<T>(
  root: Node,
  what: string,
  read: () => T | null,
  describe: () => string = () => (root instanceof Element ? root.outerHTML : String(root.textContent)),
  boundMs: number = DOM_WAIT_BOUND_MS,
): Promise<T> {
  return new Promise<T>((resolve, reject) => {
    const now = read();
    if (now !== null) {
      resolve(now);
      return;
    }
    const observer = new MutationObserver(() => {
      const value = read();
      if (value === null) return;
      observer.disconnect();
      clearTimeout(bound);
      resolve(value);
    });
    observer.observe(root, { subtree: true, childList: true, attributes: true, characterData: true });
    const bound = setTimeout(() => {
      observer.disconnect();
      reject(new Error(`${what} did not happen within ${boundMs}ms; the DOM holds ${describe()}`));
    }, boundMs);
  });
}
