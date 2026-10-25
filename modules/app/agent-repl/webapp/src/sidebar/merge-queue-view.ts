/**
 * merge-queue-view — THE OPEN SECTION'S MERGE-QUEUE TOGGLE, and the queue a
 * repository section draws while it is on (owner request, 2026-10-24).
 *
 * The OPEN header line carries the merge glyph the feed's merge bubble draws
 * (`merge-glyph.ts`). In the expanded repository names' colour it says each
 * repository section shows its open workspaces; a click asks for the merge
 * queues, and the glyph turns the purple of a merge in progress while every
 * section lists its queue instead. A click on the purple glyph asks for the
 * workspaces again.
 *
 * WHAT THE SECTIONS SHOW IS THE DAEMON'S (`frontend.v1.RosterOpenSection.shows`,
 * restated on every `RosterRepoSection.shows`), like every fold, so every
 * page draws the same view. The glyph is painted at once and held until the
 * wire agrees (`view.ts`); the sections follow on the push that carries
 * their queues.
 *
 * A QUEUE IS DRAWN AS ROWS, the same rows the workspaces draw (`row.ts`),
 * front first: the running merge's row on its `merging` arm, then each
 * waiting merge's on `merge_queued`. Nothing about a row changes but which
 * rows the section lists, so the queue reads exactly as the workspaces do.
 */
import type {
  RosterOpenSection,
  RosterRepoMergeQueue,
  RosterRepoSection,
} from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createControl } from "../control.js";
import { MERGE_GLYPH } from "../merge-glyph.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import type { SidebarContext } from "./context.js";
import { drawRosterFamily } from "./row.js";
import { changeView } from "./view-change.js";

/**
 * The key what the open section shows is held under (`view.ts`): TRUE while
 * the repository sections show their merge queues. FIXED, because the roster
 * carries exactly one open section.
 */
export const OPEN_SHOWS_VIEW_KEY = "shows:open";

/** The class of the merge-queue toggle on the OPEN header line. */
export const MERGE_QUEUE_TOGGLE_CLASS = "sb-merge-queue-toggle";

/**
 * The attribute the toggle says what the sections show in: `workspaces` or
 * `merge-queues`. The stylesheet colours the glyph from it.
 */
export const SHOWS_ATTRIBUTE = "data-shows";

/** The class of a repository section's body while it lists its merge queue. */
export const MERGE_QUEUE_CLASS = "sb-merge-queue";

/** Whether U's repository sections show their merge queues. EVERY arm is named; unset is malformed. */
export function drawRosterOpenSectionShows(u: RosterOpenSection, path: string): boolean {
  const arm = requireCase(u.shows, `${path}.shows`);
  switch (arm.case) {
    case "workspaces":
      return false;
    case "mergeQueues":
      return true;
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.shows`, other.case);
    }
  }
}

/** What a repository section's body shows. */
export type RepoShows = { case: "workspaces" } | { case: "mergeQueue"; queue: RosterRepoMergeQueue };

/** What U's body shows. EVERY arm is named; unset is malformed. */
export function drawRosterRepoShows(u: RosterRepoSection, path: string): RepoShows {
  const arm = requireCase(u.shows, `${path}.shows`);
  switch (arm.case) {
    case "workspaces":
      return { case: "workspaces" };
    case "mergeQueue":
      return { case: "mergeQueue", queue: arm.value };
    default: {
      const other: { case: string } = arm;
      return unreachableArm(`${path}.shows`, other.case);
    }
  }
}

/** Paint TOGGLE as the sections stand: showing their merge queues or not. */
export function paintMergeQueueToggle(toggle: HTMLElement, mergeQueues: boolean): void {
  toggle.setAttribute(SHOWS_ATTRIBUTE, mergeQueues ? "merge-queues" : "workspaces");
  toggle.setAttribute("aria-pressed", mergeQueues ? "true" : "false");
  toggle.title = mergeQueues ? "show each repository's workspaces" : "show each repository's merge queue";
}

/**
 * The merge-queue toggle, drawn as WIRE_MERGE_QUEUES says unless this page's
 * own ask is still in flight. A click asks for the other view; it never
 * reaches the header, whose click folds the section.
 */
export function drawMergeQueueToggle(sc: SidebarContext, wireMergeQueues: boolean): HTMLElement {
  const toggle = createControl();
  toggle.className = MERGE_QUEUE_TOGGLE_CLASS;
  toggle.textContent = MERGE_GLYPH;
  paintMergeQueueToggle(toggle, sc.view.track(OPEN_SHOWS_VIEW_KEY, wireMergeQueues, (value) => paintMergeQueueToggle(toggle, value)));
  toggle.addEventListener("click", (event) => {
    event.preventDefault();
    event.stopPropagation();
    const mergeQueues = toggle.getAttribute(SHOWS_ATTRIBUTE) !== "merge-queues";
    void changeView(sc, {
      key: OPEN_SHOWS_VIEW_KEY,
      value: mergeQueues,
      control: toggle,
      change: {
        case: "showOpen",
        value: { shows: mergeQueues ? { case: "mergeQueues", value: {} } : { case: "workspaces", value: {} } },
      },
    });
  });
  return toggle;
}

/**
 * A repository section's body while it shows its merge queue: one row per
 * merge in line, front first, drawn as the workspaces' rows are. It is no
 * arranged list (no bucket), so a row in it is no drag grip.
 */
export function drawRosterMergeQueue(u: RosterRepoMergeQueue, sc: SidebarContext, path: string): HTMLElement {
  const body = document.createElement("div");
  body.className = `rows ${MERGE_QUEUE_CLASS}`;
  u.rows.forEach((row, index) => body.appendChild(drawRosterFamily(row, sc, `${path}.rows[${index}]`, "grouping")));
  return body;
}
