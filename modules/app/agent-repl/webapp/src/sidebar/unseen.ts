/**
 * unseen — the rail draws only rosters someone can see; an UNSEEN page keeps
 * the latest and draws it once when it is seen again.
 *
 * WHY. Emacs keeps a webview page for every opened workspace, and every page
 * watches the one roster. A roster push is a WHOLE view (sidebar.ts), so every
 * page rebuilt the entire rail on every status change of every workspace: the
 * pages nobody was looking at burned CPU drawing transitions no one saw
 * (1,995 drawn statuses across hidden pages in one hour, 2026-10-12), and a
 * page WebKit had suspended replayed its whole queue on its first show, one
 * full rebuild per queued push at ~27ms each, before it became visible
 * (first show p50 679ms, max 2.6s; docs/investigations/
 * 2026-10-12-first-switch-and-daemon-rss.md). Because each push supersedes the
 * last, drawing only the newest is the same final rail with the unseen
 * intermediate states skipped.
 *
 * WHAT "SEEN" MEANS. The page's presence (presence.ts) says whether the
 * host shows it and its document is visible. That alone is NOT it: Emacs's
 * xwidget reports "hidden" whenever Emacs is not the frontmost application,
 * even for the page on screen (prompt-wave-driver.ts), and that page must keep
 * drawing for a reader glancing at it from another application. So a page is
 * seen while its presence says so OR while its own workspace is the roster's
 * `current` -- the workspace Emacs is standing in, whose page is the one shown.
 * A page is held only when both say no.
 *
 * A HELD ROSTER IS DRAWN THE MOMENT THE PAGE IS SEEN AGAIN: on the push that
 * makes its workspace current (that push is the newest, so it is the one
 * drawn), and on the page's presence turning seen (the held one is drawn then).
 */
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { log } from "../log.js";
import { createPresence, pagePresence, type Presence } from "../presence.js";
import { requireMessage } from "../rpc/strict.js";

/** What the gate needs from its environment, injectable for tests. */
export interface UnseenEnv {
  readonly doc: Document;
  /** Monotonic milliseconds, for the resumed draw's cost. */
  readonly now: () => number;
  /** Whether a reader can see the page; the document's own visibility when absent. */
  readonly presence?: Presence;
}

/** The gate between the roster stream and the rail's drawing. */
export interface UnseenGate {
  /** Draw ROSTER now when the page is seen, or hold it as the latest. */
  offer(roster: WorkspaceRoster): void;
  /** How many pushes are held, undrawn, right now. */
  held(): number;
  /** Detach the presence listener and drop what is held. */
  dispose(): void;
}

/** The default environment: the page itself. */
export function unseenEnv(): UnseenEnv {
  return { doc: document, now: () => performance.now(), presence: pagePresence() };
}

/** The workspace id ROSTER names current, or null when it names none. */
function currentOf(roster: WorkspaceRoster): string | null {
  return roster.current === undefined
    ? null
    : requireMessage(roster.current.workspace, "WorkspaceRoster.current.workspace").id;
}

/**
 * Gate the rail of the page for workspace OWN: DRAW is called with exactly
 * the rosters a reader can see, newest last.
 */
export function createUnseenGate(own: string, draw: (roster: WorkspaceRoster) => void, env: UnseenEnv = unseenEnv()): UnseenGate {
  const { doc, now } = env;
  const owned = env.presence === undefined ? createPresence(doc, false) : null;
  const presence = env.presence ?? owned;
  if (presence === null) throw new Error("unseen: no presence");
  let latest: WorkspaceRoster | null = null;
  let heldCount = 0;

  /** Draw ROSTER as the end of a hold that SKIPPED pushes never drawn. */
  const drawHeld = (cause: "seen" | "current", roster: WorkspaceRoster, skipped: number): void => {
    latest = null;
    heldCount = 0;
    const started = now();
    draw(roster);
    log.info(`the rail drew its newest roster, skipping ${skipped} it held while unseen`, {
      operation: "sidebar.unseen-resumed",
      context: {
        cause,
        skipped,
        draw_ms: Math.round(now() - started),
      },
    });
  };

  const unsubscribe = presence.onChange((seen) => {
    if (!seen || latest === null) return;
    // The newest held push is the one drawn; the rest were skipped.
    drawHeld("seen", latest, heldCount - 1);
  });

  return {
    offer(roster: WorkspaceRoster): void {
      if (currentOf(roster) === own) {
        if (heldCount > 0) {
          // THIS push is newer than every held one, so it is what is drawn
          // and every held one was skipped.
          drawHeld("current", roster, heldCount);
          return;
        }
        draw(roster);
        return;
      }
      if (presence.seen()) {
        latest = null;
        heldCount = 0;
        draw(roster);
        return;
      }
      latest = roster;
      heldCount += 1;
    },
    held(): number {
      return heldCount;
    },
    dispose(): void {
      unsubscribe();
      owned?.dispose();
      latest = null;
      heldCount = 0;
    },
  };
}
