/**
 * response-run — a maximal run of CONTIGUOUS final responses, drawn as ONE
 * bubble.
 *
 * Two turns that each conclude on a green answer with nothing drawn between
 * them (a turn the agent started on its own, with no prompt row of its own)
 * would otherwise stack two green bubbles back to back. Like a run of tool
 * cards (tool-group.ts), the run is coalesced where adjacency is known,
 * `arrangeSubfeedRows` (renderers.ts). UNLIKE a tool group it has NO TABS:
 * every member is shown, one under the other, inside one bordered bubble, each
 * separated from the one above by a subtle grey line, so the run reads as one
 * answer made of its turns' answers in order.
 *
 * WHAT IS CONTIGUOUS. A final response (`isFinalResponseRow`) followed by
 * another, with nothing between them but rows that draw nothing: a concluded
 * turn ending (`.turn-ended-concluded`, which takes no slot in the feed). Any
 * other row, a parented row, or a row of another kind breaks the run. A
 * concluded ending between two members rides inside the run so the order
 * stays the feed's; one after the last member stays outside it.
 *
 * THE MEMBERS ARE NOT REDRAWN. A member is the SAME `<article>` the
 * controller owns and reuses across pushes (feed-view.ts); this module only
 * moves it into the run's container, so its selection, its usage corner and
 * its reveal are untouched. The stylesheet strips each member bubble's own
 * border, corners, shadow and margins, and draws them once on the container.
 *
 * THE RUN IS PERSISTENT (held in a store across redraws, keyed by its first
 * member), so a live push that extends it moves nothing already in place.
 */
import type { FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { placeChildren } from "../dom.js";
import { log } from "../log.js";
import { createKeyedStore } from "./keyed-store.js";

/** The class the run's one bubble wears. */
export const RESPONSE_RUN_CLASS = "response-run";

/** The class the run's top-level `.feed-item` wrapper wears. */
export const RESPONSE_RUN_ITEM_CLASS = "response-run-item";

/**
 * Whether ROW is a final response: the agent's own prose the daemon stamped
 * as its turn's concluded answer. A thinking bubble and a vendor notice are
 * never the answer drawn green, so neither is one.
 */
export function isFinalResponseRow(row: FeedRow): boolean {
  if (row.row.case !== "activity") return false;
  const unit = row.row.value.unit;
  if (unit.case !== "response") return false;
  const response = unit.value;
  return response.finalAnswer && !response.thinking && response.notice === undefined;
}

/** Whether ROW draws nothing, so it does not break a run around it. */
function isInvisibleBetween(row: FeedRow): boolean {
  return row.row.case === "turnEnded" && row.row.value.outcome.case === "concluded";
}

/**
 * The exclusive end of the run that starts at ROWS[START].
 *
 * START + 1 when ROWS[START] is no top-level final response or no other final
 * response follows it contiguously, so a return past START + 1 always spans
 * two or more final responses, and always ENDS on one.
 */
export function responseRunEnd(rows: readonly FeedRow[], start: number): number {
  const first = rows[start];
  if (first === undefined || first.parent?.row !== undefined || !isFinalResponseRow(first)) return start + 1;
  let last = start;
  for (let k = start + 1; k < rows.length; k += 1) {
    const row = rows[k];
    if (row.parent?.row !== undefined) break;
    if (isFinalResponseRow(row)) last = k;
    else if (!isInvisibleBetween(row)) break;
  }
  return last + 1;
}

/** The key a run is stored under: its first member's id. */
export function responseRunKey(firstMemberId: string): string {
  return `response-run::${firstMemberId}`;
}

/** The store of open runs, one per body renderer (so per open feed). */
export interface ResponseRunStore {
  /**
   * Reuse or create the run for KEY, lay MEMBERS out inside it in order, and
   * return the wrapper to place at the top level.
   */
  arrange(key: string, members: readonly HTMLElement[]): HTMLElement;
  /** Dispose every run not `arrange`d since the previous prune. */
  prune(): void;
}

/** Build a store of coalesced response runs for one feed. */
export function createResponseRunStore(): ResponseRunStore {
  const runs = createKeyedStore<ResponseRun>();
  return {
    arrange(key, members) {
      const run = runs.use(key, createResponseRun);
      run.update(members);
      return run.wrapper;
    },
    prune: runs.prune,
  };
}

/** One coalesced run, persistent across arrange passes. */
interface ResponseRun {
  readonly wrapper: HTMLElement;
  update(members: readonly HTMLElement[]): void;
  dispose(): void;
}

function createResponseRun(): ResponseRun {
  // A `.feed-item` like every top-level entry, so the feed's own spacing
  // applies to the run as it does to a lone response. It is arrange-owned,
  // never a controller row, so it carries no `data-feed-row` of its own.
  const wrapper = document.createElement("article");
  wrapper.className = `feed-item ${RESPONSE_RUN_ITEM_CLASS}`;
  const run = document.createElement("div");
  run.className = RESPONSE_RUN_CLASS;
  wrapper.append(run);
  return {
    wrapper,
    update(members) {
      // Only a member out of place is moved (`placeChildren`): re-attaching
      // one already in place would lose the reader's scroll inside it.
      const moved = placeChildren(run, members);
      log.debug("laid out a coalesced run of final responses", {
        operation: "feed.response-run.layout",
        context: { members: members.length, moved },
      });
    },
    dispose() {
      // The members are the controller's; the arrange pass that dropped this
      // run has already re-placed or discarded each of them.
      wrapper.remove();
    },
  };
}
