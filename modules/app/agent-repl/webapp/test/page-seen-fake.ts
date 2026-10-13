/**
 * fakePageSeen — a `PageSeen` (src/page-seen.ts) the test moves by hand.
 *
 * A unit test that is not about whether the page is seen gets one that says
 * seen, so every type-out and roster draw runs as it does on the page on
 * screen. A test about the unseen path flips it with `set`, which fires the
 * transition exactly as the real one does.
 */
import type { PageSeen, SeenCause } from "../src/page-seen.js";

export interface FakePageSeen extends PageSeen {
  /** Move the page to SEEN, telling every subscriber when that is a change. */
  set(seen: boolean, cause?: SeenCause): void;
}

export function fakePageSeen(initial = true): FakePageSeen {
  let seen = initial;
  const listeners = new Set<(seen: boolean, cause: SeenCause) => void>();
  return {
    isSeen: () => seen,
    noteCurrent: () => undefined,
    onChange(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    dispose: () => listeners.clear(),
    set(next: boolean, cause: SeenCause = "presence"): void {
      if (next === seen) return;
      seen = next;
      for (const fn of [...listeners]) fn(seen, cause);
    },
  };
}
