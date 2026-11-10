/**
 * hold — A DRAWING HELD WHILE THE READER REARRANGES IT, the one hold every
 * drag-to-reorder surface shares: the sidebar's arrangement (`arrange.ts`)
 * and the feed's merge queue tab (`feed/merge/queue.ts`).
 *
 * A drag moves the dragged element in the page's own DOM as it goes, so the
 * drop already shows the new order. A push drawn while the drag stands would
 * throw that DOM away under the pointer, and one drawn while the ask is in
 * flight could predate the change and put the moved item back for an
 * instant. So the drawing is HELD from the moment the item lifts until the
 * ask settles: a push offered meanwhile is kept (the newest only, since each
 * push is whole) and drawn when the hold lets go.
 *
 * LETTING GO ALWAYS DRAWS, the newest push held back or else the one last
 * drawn: a drag rearranged the page's own DOM, and a drawing the daemon did
 * not take (a refusal, a cancelled drag) must not outlive the hold.
 */
import { log } from "./log.js";

/** The page's hold on one drawing while it is dragged or asked about. */
export interface DrawingHold<T> {
  /** Draw PUSH now, or keep it for when the hold lets go. */
  offer(push: T): void;
  /** A drag started: hold every push until it ends. */
  drag(): void;
  /** A drag ended with nothing asked: let go. */
  cancel(): void;
  /** An ask is in flight: keep holding until it settles. */
  await(): void;
  /**
   * The ask settled. REVISION is the revision the change arrived at, or null
   * when the ask did not take (or there is nothing to wait for): the hold then
   * lets go at once.
   */
  settle(revision: bigint | null): void;
  /** The stream run ended: an awaited revision is no longer coming. */
  reset(): void;
  /** Whether the drawing is held right now. */
  holding(): boolean;
}

/** What a hold draws through, and how it reads and records its pushes. */
export interface DrawingHoldSpec<T> {
  /** Draw PUSH. */
  draw: (push: T) => void;
  /**
   * The revision PUSH carries. Unsaid for a drawing whose pushes carry none,
   * whose asks then always settle with null.
   */
  revisionOf?: (push: T) => bigint;
  /** The operation the hold's letting go is recorded under. */
  operation: string;
}

/** The hold over SPEC's drawing. */
export function createDrawingHold<T>(spec: DrawingHoldSpec<T>): DrawingHold<T> {
  type State = { case: "free" } | { case: "held" } | { case: "awaiting"; revision: bigint };
  let state: State = { case: "free" };
  /** The newest push offered while held, null when none was. */
  let kept: T | null = null;
  /** The push last drawn, null before the first. */
  let drawn: T | null = null;

  const revisionOf = (push: T): bigint => {
    if (spec.revisionOf === undefined) throw new Error(`invariant: ${spec.operation} settles at a revision only when its pushes carry one`);
    return spec.revisionOf(push);
  };

  const drawNow = (push: T): void => {
    drawn = push;
    spec.draw(push);
  };

  const letGo = (why: string): void => {
    state = { case: "free" };
    const push = kept ?? drawn;
    kept = null;
    log.debug("a held drawing let go", {
      operation: spec.operation,
      context: {
        why,
        revision: push === null || spec.revisionOf === undefined ? null : String(spec.revisionOf(push)),
      },
    });
    if (push !== null) drawNow(push);
  };

  return {
    offer(push) {
      if (state.case === "free") {
        drawNow(push);
        return;
      }
      kept = push;
      if (state.case === "awaiting" && revisionOf(push) >= state.revision) letGo("a push carried the arrangement");
    },
    drag() {
      state = { case: "held" };
    },
    cancel() {
      letGo("the drag was cancelled");
    },
    await() {
      state = { case: "held" };
    },
    settle(revision) {
      if (revision === null) {
        letGo("the arrangement did not take");
        return;
      }
      // A CHANGE THAT LEFT THE DRAWING AS IT WAS publishes no new push (the
      // daemon drops a duplicate), so a revision already drawn or held is
      // the arrangement arriving, not one still to come.
      if ((drawn !== null && revisionOf(drawn) >= revision) || (kept !== null && revisionOf(kept) >= revision)) {
        letGo("a push already carried the arrangement");
        return;
      }
      state = { case: "awaiting", revision };
    },
    reset() {
      // A DRAG IN PROGRESS KEEPS ITS HOLD: the dragged element is in the DOM
      // the next push would throw away. An awaited revision belongs to the
      // run that ended, and a successor daemon numbers its own.
      if (state.case === "awaiting") letGo("the stream ended");
    },
    holding: () => state.case !== "free",
  };
}
