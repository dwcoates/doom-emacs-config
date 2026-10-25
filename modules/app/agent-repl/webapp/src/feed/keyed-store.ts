/**
 * keyed-store — the persistent store a feed arrangement keeps its run
 * containers in across arrange passes.
 *
 * `arrangeSubfeedRows` (renderers.ts) re-lays the whole feed on every push, and
 * a container it wraps a run of rows in (a tabbed tool group, a coalesced run
 * of final responses) must SURVIVE that re-lay: the same wrapper element, so
 * nothing already in place is re-attached (owner rule, 2026-09-23: the user
 * owns the scroll), and the same state, so a reader's pick sticks. Each pass
 * `use`s the key of every run it lays out; `prune` then disposes every entry
 * the pass did not use, because its run broke or its rows left the feed.
 */

/** An entry the store can drop. */
export interface Disposable {
  dispose(): void;
}

/** One feed's entries, keyed, persistent across arrange passes. */
export interface KeyedStore<T extends Disposable> {
  /** The entry for KEY, created by CREATE the first time KEY is used. */
  use(key: string, create: () => T): T;
  /**
   * Dispose and forget every entry not `use`d since the previous prune. A
   * property rather than a method, so a caller may hand it on unbound: it
   * reads no `this`.
   */
  prune: () => void;
}

/** Build an empty keyed store. */
export function createKeyedStore<T extends Disposable>(): KeyedStore<T> {
  const entries = new Map<string, T>();
  let seen = new Set<string>();
  return {
    use(key, create) {
      seen.add(key);
      let entry = entries.get(key);
      if (entry === undefined) {
        entry = create();
        entries.set(key, entry);
      }
      return entry;
    },
    prune() {
      for (const [key, entry] of entries) {
        if (seen.has(key)) continue;
        entry.dispose();
        entries.delete(key);
      }
      seen = new Set();
    },
  };
}
