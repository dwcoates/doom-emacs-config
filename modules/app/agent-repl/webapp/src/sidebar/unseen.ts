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
 * WHAT "SEEN" MEANS is page-seen.ts's, the one answer every consumer of it
 * shares (the page's presence, or its workspace being the roster's
 * `current`). This gate is also the one that TELLS it the roster's `current`,
 * since the rail is the roster stream's one reader.
 *
 * A HELD ROSTER IS DRAWN THE MOMENT THE PAGE IS SEEN AGAIN: on the push that
 * makes its workspace current (that push is the newest, so it is the one
 * drawn), and on the page's presence turning seen (the held one is drawn then).
 */
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";
import type { PageSeen } from "../page-seen.js";

/** What the gate needs from its environment, injectable for tests. */
export interface UnseenEnv {
  /** Whether a reader can see the page (page-seen.ts). */
  readonly seen: PageSeen;
  /** Monotonic milliseconds, for the resumed draw's cost. */
  readonly now: () => number;
}

/** The gate between the roster stream and the rail's drawing. */
export interface UnseenGate {
  /** Draw ROSTER now when the page is seen, or hold it as the latest. */
  offer(roster: WorkspaceRoster): void;
  /** How many pushes are held, undrawn, right now. */
  held(): number;
  /** Detach the seen listener and drop what is held. */
  dispose(): void;
}

/** The default environment: the page's own seen state and clock. */
export function unseenEnv(seen: PageSeen): UnseenEnv {
  return { seen, now: () => performance.now() };
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
export function createUnseenGate(own: string, draw: (roster: WorkspaceRoster) => void, env: UnseenEnv): UnseenGate {
  const { seen, now } = env;
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

  // A push that makes this workspace current draws itself (`offer`), so only
  // the page's presence turning seen draws what is held.
  const unsubscribe = seen.onChange((nowSeen, cause) => {
    if (!nowSeen || cause !== "presence" || latest === null) return;
    // The newest held push is the one drawn; the rest were skipped.
    drawHeld("seen", latest, heldCount - 1);
  });

  return {
    offer(roster: WorkspaceRoster): void {
      const current = currentOf(roster);
      seen.noteCurrent(current);
      if (!seen.isSeen()) {
        latest = roster;
        heldCount += 1;
        return;
      }
      if (heldCount > 0) {
        // THIS push is newer than every held one, so it is what is drawn
        // and every held one was skipped.
        drawHeld(current === own ? "current" : "seen", roster, heldCount);
        return;
      }
      draw(roster);
    },
    held(): number {
      return heldCount;
    },
    dispose(): void {
      unsubscribe();
      latest = null;
      heldCount = 0;
    },
  };
}
