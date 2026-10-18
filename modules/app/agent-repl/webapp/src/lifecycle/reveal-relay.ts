/**
 * reveal-relay — a row the daemon asked this page to reveal, held until the
 * feed can reveal it.
 *
 * A SELECTION CAN NAME A ROW TO REVEAL (`SelectWorkspaceRequest.reveal`): an
 * async merge's queue entry selects its requester and opens the merge's bubble
 * there (owner ruling, 2026-10-17). The ask arrives on the page's web stream
 * (`WebWorkspaceRevealRow`), which the lifecycle opens BEFORE the feed is
 * mounted -- a page the selection loaded is handed it first thing -- so the
 * relay keeps the latest ask until the feed binds its reveal, and delivers it
 * then. Only the latest is kept: a second ask before the bind supersedes the
 * first, as a second click would.
 */
import { log } from "../log.js";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";

/** Reveals one row of the page's feed, answering whether it was revealed. */
export type RevealFn = (row: FeedId) => Promise<boolean>;

export interface RevealRelay {
  /** The daemon asked for ROW: revealed now, or once the feed binds. */
  request(row: FeedId): void;
  /** The feed's reveal; a held ask is delivered at once. */
  bind(reveal: RevealFn): void;
}

export function createRevealRelay(): RevealRelay {
  let reveal: RevealFn | null = null;
  let held: FeedId | null = null;

  function deliver(fn: RevealFn, row: FeedId): void {
    void fn(row).then((revealed) => {
      log.info(revealed ? "revealed the row the daemon asked for" : "the row the daemon asked to reveal was not found", {
        operation: "lifecycle.reveal-row",
        context: { row: row.value, revealed },
      });
    });
  }

  return {
    request(row) {
      if (reveal === null) {
        log.info("the daemon asked to reveal a row before the feed was mounted; it is revealed once it is", {
          operation: "lifecycle.reveal-row-held",
          context: { row: row.value, superseded: held?.value ?? "none" },
        });
        held = row;
        return;
      }
      deliver(reveal, row);
    },
    bind(fn) {
      reveal = fn;
      if (held === null) return;
      const row = held;
      held = null;
      deliver(fn, row);
    },
  };
}
