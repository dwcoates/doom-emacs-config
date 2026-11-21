/**
 * queue-selection — THE SELECTED WORKSPACE'S MERGES ARE HIGHLIGHTED IN THE
 * MERGE QUEUE, AND SCROLLED INTO ITS VIEW (owner request, 2026-11-17).
 *
 * THE HIGHLIGHT. Every queue line credited to the selected workspace (the
 * head's card, a merge out of line, a waiting merge, a GitHub standing, a
 * merge off its queue: `drawSubjectControl`'s lines) wears the Open
 * section's selected fill (`--row-highlight-tint`). The Deploys section is
 * kept across pushes that leave its queue unchanged (`roster.ts`
 * `keepOrDraw`), so the highlight is never drawn with a line: it is laid over
 * the rail after every draw and every `markShown`, by the one selected id the
 * rail's rows follow.
 *
 * THE SCROLL. Once per selection, the Merge Queue's viewport scrolls only as
 * far as the selected workspace's first laid-out line and its neighbors need
 * to show, through the Open section's own scroll (`scrollLineAmong`, the same
 * `SELECTION_CONTEXT_ROWS` and scroll padding), among the band's lines
 * (`regionLines`).
 *
 * AND BACK (owner request, 2026-11-17). When the selection no longer names a
 * line in the queue (another workspace is selected, or the merge left the
 * queue), the viewport goes back to where it stood before the first of the
 * automatic scrolls, unless the reader scrolled it since. A selection moving
 * from one queue line to another keeps that first position, so the queue
 * comes back to where the reader had it.
 */
import { queryAll } from "../dom.js";
import { createLogger } from "../log.js";
import { scrollKept } from "../scroll.js";
import { regionLines } from "./band-fit.js";
import { MERGE_QUEUE_SECTION_CLASS } from "./section-classes.js";
import { MERGE_QUEUE_SUBJECT_CLASS } from "./merge-queue-view.js";
import { scrollerOf, scrollLineAmong } from "./selection-scroll.js";

const log = createLogger("webapp.sidebar.queue-selection");

/** The class a highlighted queue line wears, as a highlighted row does. */
export const QUEUE_CURRENT_CLASS = "current";

/** Every queue line under ROOT, each credited to the workspace its `data-workspace` names. */
function queueLines(root: HTMLElement): HTMLElement[] {
  return queryAll<HTMLElement>(root, `.${MERGE_QUEUE_SUBJECT_CLASS}`);
}

/**
 * Highlight every queue line under ROOT credited to SELECTED, and no other.
 * Answers the highlighted lines, top to bottom.
 */
export function markQueueSelection(root: HTMLElement, selected: string | null): HTMLElement[] {
  const marked: HTMLElement[] = [];
  for (const line of queueLines(root)) {
    const highlighted = selected !== null && line.getAttribute("data-workspace") === selected;
    line.classList.toggle(QUEUE_CURRENT_CLASS, highlighted);
    if (highlighted) marked.push(line);
  }
  return marked;
}

/** The Merge Queue's rows region under ROOT, or null while no queue is drawn. */
function queueRegion(root: HTMLElement): HTMLElement | null {
  return root.querySelector<HTMLElement>(`.${MERGE_QUEUE_SECTION_CLASS} > .rows`);
}

/** What became of following a selection in the Merge Queue. */
export type QueueFollowOutcome =
  /** The selection names no queue line, and no automatic scroll is to be undone. */
  | "none"
  /** The selection's line was already followed. */
  | "followed"
  /** The selection's lines draw no box (a folded group), so they are left for a later draw. */
  | "unlaid"
  /** The selection's line already shows with its neighbors. */
  | "visible"
  /** The queue scrolled to show the selection's line with its neighbors. */
  | "scrolled"
  /** The selection left the queue, and the queue went back to where it stood before the automatic scroll. */
  | "restored"
  /** The selection left the queue, but the reader had scrolled it since, so it stays. */
  | "kept";

/** The rail's follower of the selection in the Merge Queue. */
export interface QueueSelectionScroll {
  /** Highlight SELECTED's queue lines under ROOT, and scroll to or back from them. */
  follow(root: HTMLElement, selected: string | null): QueueFollowOutcome;
}

/** An automatic scroll still to be undone: where the queue stood before it, and where the last one left it. */
interface Undo {
  before: number;
  left: number;
}

/**
 * Whether REGION still stands where the automatic scroll LEFT it, or short
 * of there only because its lines shrank and the box clamped it to its end.
 */
function untouched(region: HTMLElement, left: number): boolean {
  if (region.scrollTop === left) return true;
  const end = region.scrollHeight - region.clientHeight;
  return region.scrollTop < left && region.scrollTop >= end;
}

/** The Merge Queue's follower: it remembers the workspace it followed and the scroll it owes back. */
export function createQueueSelectionScroll(): QueueSelectionScroll {
  let followed: string | null = null;
  let undo: Undo | null = null;

  return {
    follow(root: HTMLElement, selected: string | null): QueueFollowOutcome {
      const marked = markQueueSelection(root, selected);
      const line = marked.find((l) => l.getBoundingClientRect().height > 0) ?? null;
      if (line !== null) {
        if (selected === followed) return "followed";
        followed = selected;
        const region = scrollerOf(line);
        const before = region.scrollTop;
        const scrolled = scrollLineAmong(line, regionLines(region));
        const record = { operation: "sidebar.queue-selection-scroll", context: { workspace: selected, ...(scrolled ?? {}) } };
        if (scrolled === null) {
          log.debug("the selected workspace's merge already shows in the merge queue", record);
          return "visible";
        }
        undo = { before: undo?.before ?? before, left: scrolled.to };
        log.info("scrolled the selected workspace's merge into the merge queue's view", record);
        return "scrolled";
      }
      if (marked.length > 0) return "unlaid";
      followed = null;
      if (undo === null) return "none";
      const owed = undo;
      undo = null;
      const region = queueRegion(root);
      if (region === null) {
        log.debug("the selection left the merge queue, which is no longer drawn", {
          operation: "sidebar.queue-selection-scroll.back",
          context: { workspace: selected, outcome: "none" },
        });
        return "none";
      }
      const found = region.scrollTop;
      if (!untouched(region, owed.left)) {
        log.debug("the selection left the merge queue, which the reader had scrolled; it stays", {
          operation: "sidebar.queue-selection-scroll.back",
          context: { workspace: selected, outcome: "kept", found, left: owed.left },
        });
        return "kept";
      }
      scrollKept(region, owed.before);
      log.info("the selection left the merge queue; it is back where it stood before the automatic scroll", {
        operation: "sidebar.queue-selection-scroll.back",
        context: { workspace: selected, outcome: "restored", from: found, to: region.scrollTop },
      });
      return "restored";
    },
  };
}
