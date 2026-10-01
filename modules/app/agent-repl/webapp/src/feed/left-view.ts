/**
 * left-view — THE ONE "A ROW LEFT THE VIEWPORT AFTER BEING SEEN" DETECTOR.
 *
 * Two things end when a feed entry has been scrolled wholly out of view: the
 * reply selection (selection-visibility.ts tells the daemon `left_view`), and
 * an entry a JUMP expanded (jump-collapse.ts closes it again). Both ask the
 * same question of the feed's scroll box, and this is the one place it is
 * answered, so the two cannot drift apart about what "left" means.
 *
 * WHY "SEEN" FIRST. A watched element can be out of view at the moment it is
 * watched (an older response far above, a jump target not centered yet). The
 * observer's first report about it can come before the scroll that brings it
 * in has landed, and a report of "not visible" then is no departure. So a
 * departure counts only after an arrival.
 *
 * ONE REPORT PER WATCH. An element that keeps wandering in and out is
 * reported once; a fresh `watch` starts a fresh, unseen state.
 *
 * A DETACHED ELEMENT IS NOT A DEPARTURE. A page replace or a row removal
 * detaches the element; the observer then reports it not intersecting, which
 * is no scroll at all. The watcher's `onDetached` hears it instead, every time,
 * and nothing is reported as left.
 *
 * "OUT OF VIEW" IS ANY PART: the observer's threshold is 0, so an element is
 * in view while a single pixel of it intersects the box, and has left only
 * once none does.
 *
 * Absent `IntersectionObserver` (a bare jsdom), there is no watch at all
 * (null), the same standing overscan.ts gives it.
 */

/** What a watcher hears about the one element it watches. */
export interface LeftViewHandlers {
  /** The element was seen in the viewport and has now left it wholly. Once. */
  onLeft(): void;
  /** The element was reported out of view because it is no longer in the document. */
  onDetached(): void;
}

/** Watches elements against the feed's scroll box. */
export interface LeftViewWatch {
  /**
   * Watch ELEMENT from an unseen state; answers the unwatch. Watching an
   * element already watched replaces its handlers and resets its state.
   */
  watch(element: HTMLElement, handlers: LeftViewHandlers): () => void;
  /** Tear the observer down when the feed disposes. */
  dispose(): void;
}

/** One watched element's state. */
interface Watched {
  handlers: LeftViewHandlers;
  seen: boolean;
  reported: boolean;
}

/** Build the watch rooted on the feed's scroll BOX, or null with no observer. */
export function createLeftViewWatch(box: HTMLElement): LeftViewWatch | null {
  const Ctor = (globalThis as { IntersectionObserver?: typeof IntersectionObserver })
    .IntersectionObserver;
  if (Ctor === undefined) return null;
  const watched = new Map<Element, Watched>();
  const observer = new Ctor(
    (entries) => {
      for (const entry of entries) {
        const state = watched.get(entry.target);
        if (state === undefined) continue;
        if (entry.isIntersecting) {
          state.seen = true;
          continue;
        }
        if (!entry.target.isConnected) {
          state.handlers.onDetached();
          continue;
        }
        if (!state.seen || state.reported) continue;
        state.reported = true;
        state.handlers.onLeft();
      }
    },
    { root: box, threshold: 0 },
  );
  return {
    watch: (element, handlers) => {
      const state: Watched = { handlers, seen: false, reported: false };
      watched.set(element, state);
      observer.observe(element);
      return () => {
        // A later watch of the same element owns it now; this unwatch is spent.
        if (watched.get(element) !== state) return;
        watched.delete(element);
        observer.unobserve(element);
      };
    },
    dispose: () => {
      watched.clear();
      observer.disconnect();
    },
  };
}
