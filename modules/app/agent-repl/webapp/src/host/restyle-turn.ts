/**
 * restyle-turn — A PAGE THE HOST IS NOT SHOWING RESTYLES IN ITS TURN.
 *
 * WHY. Every workspace page is an iframe of the one host web view, and all of
 * them share one main thread (host/protocol.ts). A change every page takes at
 * once, the feed text zoom above all, made every page restyle and lay out in
 * the same frame: one zoom nudge blocked the thread 135ms and then 416ms with
 * about 25 pages open, and the page on screen waited behind the pages nobody
 * could see (logs, 2026-10-27 12:06:48).
 *
 * HOW. A page the host is not showing asks the host for a turn instead of
 * restyling at once; the host grants ONE page a turn at a time, and the next
 * only a frame after the last one ended (host.ts), so the hidden pages' work
 * is spread one page a frame and never lands in a frame with the shown page's.
 * The page on screen never asks: it restyles at once.
 *
 * A page asks at most once while it waits: a later ask in the same wait
 * replaces the work its turn runs, so the turn runs only the latest. The turn
 * always ends, even when its work throws, or the host would hold every other
 * page's turn behind it.
 */
import { perDocument } from "../per-document.js";

/** What a page's turns need from its host link: how to ask, and how to end a turn. */
export type RestyleTurnSend = (kind: "restyleTurn" | "restyled") => void;

/** A page's turns, as the work that waits on them sees them. */
export interface RestyleTurns {
  /** Run RUN in the page's next turn, replacing the work a turn already waited with. */
  request(run: () => void): void;
}

/** The page's turns plus the two ends its host link holds. */
export interface RestyleTurnsSource extends RestyleTurns {
  /** The host link is up: SEND reaches the host. */
  connect(send: RestyleTurnSend): void;
  /** The host granted the page its turn: run the waiting work, and end the turn. */
  granted(): void;
}

/** A page's turns, with no host link until `connect`. */
export function createRestyleTurns(): RestyleTurnsSource {
  let send: RestyleTurnSend | null = null;
  let waiting: (() => void) | null = null;
  let asked = false;
  return {
    connect(next) {
      send = next;
    },
    request(run) {
      if (send === null) {
        throw new Error("restyle turn: a page asked for a turn with no host link to ask through");
      }
      waiting = run;
      if (asked) return;
      asked = true;
      send("restyleTurn");
    },
    granted() {
      if (send === null) throw new Error("restyle turn: a grant arrived with no host link");
      const run = waiting;
      waiting = null;
      asked = false;
      try {
        run?.();
      } finally {
        send("restyled");
      }
    },
  };
}

/** The page's own turns: one per document, made on first use. */
export const pageRestyleTurns = perDocument((): RestyleTurnsSource => createRestyleTurns());
