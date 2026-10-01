/**
 * jump-collapse — AN ENTRY A JUMP EXPANDED CLOSES ONCE IT IS WHOLLY OUT OF VIEW
 * (owner request, 2026-10-01).
 *
 * Jumping to a feed entry (a footer detached-work row, a breadcrumb, a hook's
 * gated-call link) expands the entry, then centers it. The expansion is the
 * jump's, not the reader's, so it is undone on its own: once no part of the
 * expanded entry is visible in the feed any more, it is collapsed, and
 * scrolling back to it shows it closed.
 *
 * ONLY WHAT THE JUMP EXPANDED. An entry that was already open when the jump
 * landed (the reader opened it by hand) is never tracked here. An entry the
 * reader toggles by hand after the jump becomes theirs: `release` drops its
 * watch, and it is left as they leave it.
 *
 * EACH JUMP IS WATCHED ON ITS OWN. A second jump to another entry adds a second
 * watch; neither ends the other.
 *
 * "OUT OF VIEW" IS THE SHARED DETECTOR'S (left-view.ts): seen first, then
 * wholly outside the feed's scroll box. A row a page replace or a removal
 * DETACHED is no departure: its watch is dropped and nothing is collapsed.
 *
 * `clear` is for a collapse that already happened elsewhere (the reader
 * returning to the tail closes every expanded entry at once), so no entry is
 * ever collapsed twice.
 */
import { log } from "../log.js";
import { createLeftViewWatch } from "./left-view.js";

/** An entry a jump expanded, and how to close it again. */
export interface JumpedEntry {
  /** The entry's feed row: the element watched, and the key it is known by. */
  readonly row: HTMLElement;
  /** Whether the entry is still expanded at the moment of asking. */
  isExpanded(): boolean;
  /** Collapse it through the entry's own one collapse. */
  collapse(): void;
}

/** The watches over the entries jumps expanded. */
export interface JumpCollapse {
  /** Watch ENTRY, which a jump has just expanded. */
  track(entry: JumpedEntry): void;
  /** The reader toggled ROW by hand: it is theirs now, and is not collapsed here. */
  release(row: HTMLElement): void;
  /** Every tracked entry was collapsed elsewhere: drop every watch. */
  clear(): void;
  /** Tear the observer down when the feed disposes. */
  dispose(): void;
}

/** Build the watches rooted on the feed's scroll BOX, or null with no observer. */
export function createJumpCollapse(box: HTMLElement): JumpCollapse | null {
  const detector = createLeftViewWatch(box);
  if (detector === null) return null;
  const tracked = new Map<HTMLElement, () => void>();

  const drop = (row: HTMLElement): void => {
    tracked.get(row)?.();
    tracked.delete(row);
  };

  return {
    track: (entry) => {
      drop(entry.row);
      log.debug("watching an entry a jump expanded", {
        operation: "feed.jump-collapse-watch",
        context: { row: rowName(entry.row) },
      });
      tracked.set(
        entry.row,
        detector.watch(entry.row, {
          onDetached: () => {
            drop(entry.row);
            log.debug("an entry a jump expanded was detached, not scrolled away; nothing collapses", {
              operation: "feed.jump-collapse-detached",
              context: { row: rowName(entry.row) },
            });
          },
          onLeft: () => {
            drop(entry.row);
            if (!entry.isExpanded()) {
              log.debug("an entry a jump expanded left the view already closed", {
                operation: "feed.jump-collapse-closed",
                context: { row: rowName(entry.row) },
              });
              return;
            }
            log.info("an entry a jump expanded left the view; it collapses", {
              operation: "feed.jump-collapse",
              context: { row: rowName(entry.row) },
            });
            entry.collapse();
          },
        }),
      );
    },
    release: (row) => {
      if (!tracked.has(row)) return;
      drop(row);
      log.debug("the reader toggled an entry a jump expanded; it is theirs now", {
        operation: "feed.jump-collapse-released",
        context: { row: rowName(row) },
      });
    },
    clear: () => {
      for (const row of [...tracked.keys()]) drop(row);
    },
    dispose: () => {
      tracked.clear();
      detector.dispose();
    },
  };
}

/** A row's name in a record: the FeedId its element carries. */
function rowName(row: HTMLElement): string {
  return row.getAttribute("data-feed-row") ?? "unset";
}
