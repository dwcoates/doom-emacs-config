/**
 * page-seen — THE ONE ANSWER to "can a reader see this page right now".
 *
 * Emacs keeps a page for every opened workspace, and only one of them is on
 * screen. Work a reader cannot see (a roster rebuild, a type-out) is wasted on
 * every other page, and worse, WebKit suspends a hidden page's animation
 * frames, so a type-out begun there is still typing when the page is next
 * shown: the reader switches in and waits for text that arrived long ago.
 * Every consumer that skips unseen work asks here, so they agree.
 *
 * WHAT "SEEN" MEANS. The page's presence (presence.ts) says whether the host
 * shows it and its document is visible. That alone is NOT it: Emacs's xwidget
 * reports "hidden" whenever Emacs is not the frontmost application, even for
 * the page on screen, and that page must keep drawing
 * for a reader glancing at it from another application. So a page is seen
 * while its presence says so, OR while the webhost shows it (the webhost shows
 * exactly the page of the workspace Emacs is standing in), OR, for a page on
 * its own, while its own workspace is the roster's `current`. A page is unseen
 * only when all say no.
 *
 * The presence halves are read off presence.ts; the current half is told
 * (`noteCurrent`) by the one reader of the roster stream in a page on its own,
 * its sidebar (sidebar/unseen.ts). A page in the webhost draws no sidebar and
 * is never told. Until the first roster arrives no workspace is current.
 */
import type { Presence } from "./presence.js";

/** Which of the two facts last moved the page across seen/unseen. */
export type SeenCause = "presence" | "current";

/** The page's seen state, and its transitions. */
export interface PageSeen {
  /** Whether a reader can see this page now. */
  isSeen(): boolean;
  /** The roster's current workspace changed (null: it names none). */
  noteCurrent(workspaceId: string | null): void;
  /**
   * Run FN on every transition between seen and unseen, with the new state and
   * the fact that moved it. Returns its unsubscriber.
   */
  onChange(fn: (seen: boolean, cause: SeenCause) => void): () => void;
  /** Stop following the presence and drop every subscriber. */
  dispose(): void;
}

/** Track the page for workspace OWN, whose presence PRESENCE reports. */
export function createPageSeen(own: string, presence: Presence): PageSeen {
  let current: string | null = null;
  const listeners = new Set<(seen: boolean, cause: SeenCause) => void>();
  const seenNow = (): boolean => presence.seen() || (presence.embedded && presence.shown()) || current === own;
  let seen = seenNow();

  const settle = (cause: SeenCause): void => {
    const next = seenNow();
    if (next === seen) return;
    seen = next;
    // A copy: a listener may unsubscribe itself as it runs.
    for (const fn of [...listeners]) fn(seen, cause);
  };
  const unfollow = presence.onChange(() => settle("presence"));

  return {
    isSeen: () => seen,
    noteCurrent(workspaceId: string | null): void {
      current = workspaceId;
      settle("current");
    },
    onChange(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    dispose(): void {
      unfollow();
      listeners.clear();
    },
  };
}
