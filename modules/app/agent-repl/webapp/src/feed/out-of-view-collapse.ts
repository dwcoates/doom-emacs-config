/**
 * out-of-view-collapse — AN ENTRY OPENED TO BE GLANCED AT CLOSES ONCE IT IS
 * WHOLLY OUT OF VIEW.
 *
 * Some expansions are not the reader's to keep. An entry is watched here, by
 * the act that opened it (its `cause`), and once no part of it is visible in
 * the feed any more it is collapsed, so scrolling back to it shows it closed:
 *
 *   - `jump` (owner request, 2026-10-01): jumping to a feed entry (a footer
 *     detached-work row, a breadcrumb, a hook's gated-call link) expands the
 *     entry, then centers it. The expansion is the jump's, not the reader's.
 *     An entry that was already open when the jump landed (the reader opened
 *     it by hand) is never tracked.
 *   - `thinkingClick` (owner specification, 2026-10-14): a click on a
 *     root-feed THINKING bubble only expands it, with no selection
 *     (bubble-selection.ts). Scrolling away still closes it, as it did while
 *     the expansion was the selection's (which ended on leaving the view).
 *
 * A HAND TOGGLE AFTER THE WATCH BEGINS ENDS IT: `release` drops the entry's
 * watch, and it is left as the reader leaves it.
 *
 * EACH ENTRY IS WATCHED ON ITS OWN. A second entry adds a second watch; neither
 * ends the other.
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

/** The act that opened a watched entry (see the module comment). */
export type OutOfViewCause = "jump" | "thinkingClick";

/** An entry that closes once out of view, and how to close it again. */
export interface CollapsingEntry {
  /** The entry's feed row: the element watched, and the key it is known by. */
  readonly row: HTMLElement;
  /** The act that opened it. */
  readonly cause: OutOfViewCause;
  /** Whether the entry is still expanded at the moment of asking. */
  isExpanded(): boolean;
  /** Collapse it through the entry's own one collapse. */
  collapse(): void;
}

/** The watches over the entries that close once out of view. */
export interface OutOfViewCollapse {
  /** Watch ENTRY, which its cause has just expanded. */
  track(entry: CollapsingEntry): void;
  /** The reader toggled ROW by hand: it is theirs now, and is not collapsed here. */
  release(row: HTMLElement): void;
  /** Every tracked entry was collapsed elsewhere: drop every watch. */
  clear(): void;
  /** Tear the observer down when the feed disposes. */
  dispose(): void;
}

/** Build the watches rooted on the feed's scroll BOX, or null with no observer. */
export function createOutOfViewCollapse(box: HTMLElement): OutOfViewCollapse | null {
  const detector = createLeftViewWatch(box);
  if (detector === null) return null;
  const tracked = new Map<HTMLElement, { cause: OutOfViewCause; unwatch: () => void }>();

  const drop = (row: HTMLElement): void => {
    tracked.get(row)?.unwatch();
    tracked.delete(row);
  };

  return {
    track: (entry) => {
      drop(entry.row);
      const context = { row: rowName(entry.row), cause: entry.cause };
      log.debug("watching an entry that closes once out of view", {
        operation: "feed.out-of-view-collapse-watch",
        context,
      });
      tracked.set(entry.row, {
        cause: entry.cause,
        unwatch: detector.watch(entry.row, {
          onDetached: () => {
            drop(entry.row);
            log.debug("a watched entry was detached, not scrolled away; nothing collapses", {
              operation: "feed.out-of-view-collapse-detached",
              context,
            });
          },
          onLeft: () => {
            drop(entry.row);
            if (!entry.isExpanded()) {
              log.debug("a watched entry left the view already closed", {
                operation: "feed.out-of-view-collapse-closed",
                context,
              });
              return;
            }
            log.info("a watched entry left the view; it collapses", {
              operation: "feed.out-of-view-collapse",
              context,
            });
            entry.collapse();
          },
        }),
      });
    },
    release: (row) => {
      const watched = tracked.get(row);
      if (watched === undefined) return;
      drop(row);
      log.debug("the reader toggled a watched entry; it is theirs now", {
        operation: "feed.out-of-view-collapse-released",
        context: { row: rowName(row), cause: watched.cause },
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
