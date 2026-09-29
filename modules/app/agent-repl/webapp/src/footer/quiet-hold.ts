/**
 * quiet-hold — the ended quiet-stretch line, held until its successor shows.
 *
 * The daemon ends a quiet stretch the moment the feed DRAWS the next item, but
 * this page paints that item later; clearing the line on the daemon's edge left
 * a visible gap with neither the line nor the item on screen (owner report,
 * 2026-09-29). So the daemon states the ended line and the row that ended it
 * (`FooterStatusQuietStretchEnding`), and this page decides when it goes:
 *
 * - It draws the ended line, in place of the status's activity, until it has
 *   PAINTED that row (`feed/painted.ts`), and for `QUIET_HOLD_DWELL_MS` more.
 * - A reader not following the live tail is not looking where the row paints,
 *   so the hold is released at once.
 * - A push that states no ending, or another row's, releases the hold: the
 *   daemon stood a new line, an activity that outranks the line, or a new
 *   status. A row whose hold was released is never held again.
 */
import { clone, create } from "@bufbuild/protobuf";
import {
  FooterStatusActivityQuietStretchSchema,
  FooterStatusBackgroundActivitySchema,
  FooterStripSchema,
  FooterStatusWorkingActivitySchema,
  type FooterStatus,
  type FooterStatusQuietStretchEnding,
  type FooterStrip,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { PaintWatch } from "../feed/painted.js";
import { log } from "../log.js";
import { requireMessage } from "../rpc/strict.js";

/** How long the ended line stays after its successor was painted. */
export const QUIET_HOLD_DWELL_MS = 500;

export interface QuietHoldDeps {
  readonly paints: PaintWatch;
  /** Whether the reader is following the feed's live tail. */
  readonly followingTail: () => boolean;
  readonly now: () => number;
  readonly setTimer: (fn: () => void, ms: number) => number;
  readonly clearTimer: (handle: number) => void;
  /** The hold was released: redraw without it. */
  readonly redraw: () => void;
}

export interface QuietHold {
  /** Take a push's ending (undefined when it states none). */
  observe(ending: FooterStatusQuietStretchEnding | undefined): void;
  /** The ending being held right now, or null. */
  held(): FooterStatusQuietStretchEnding | null;
  dispose(): void;
}

/** The ending a status states: only `working` and `background` carry one. */
export function quietStretchEndingOf(
  status: FooterStatus,
): FooterStatusQuietStretchEnding | undefined {
  switch (status.status.case) {
    case "working":
    case "background":
      return status.status.value.quietStretchEnding;
    default:
      return undefined;
  }
}

/**
 * STRIP with the held line drawn in its status's activity. Only `working` and
 * `background` state an ending, so only they are rewritten.
 */
export function withHeldLine(strip: FooterStrip, ending: FooterStatusQuietStretchEnding): FooterStrip {
  const out = clone(FooterStripSchema, strip);
  const status = requireMessage(out.status, "FooterStrip.status");
  const at = requireMessage(ending.at, "FooterStatusQuietStretchEnding.at");
  const line = create(FooterStatusActivityQuietStretchSchema, { text: ending.text });
  switch (status.status.case) {
    case "working":
      status.status.value.activity = create(FooterStatusWorkingActivitySchema, {
        at,
        kind: { case: "quietStretch", value: line },
      });
      return out;
    case "background":
      status.status.value.activity = create(FooterStatusBackgroundActivitySchema, {
        at,
        kind: { case: "quietStretch", value: line },
      });
      return out;
    default:
      return strip;
  }
}

/** Build the hold. */
export function createQuietHold(deps: QuietHoldDeps): QuietHold {
  let current: FooterStatusQuietStretchEnding | null = null;
  let released: string | null = null;
  let timer: number | null = null;
  let unsubscribe: (() => void) | null = null;

  return {
    observe(ending) {
      const row = ending?.untilPainted?.value;
      if (ending === undefined || row === undefined || row === "") {
        release("the push states no ending", false);
        return;
      }
      if (current?.untilPainted?.value === row) {
        current = ending;
        return;
      }
      release("the push states another row's ending", false);
      if (released === row) return;
      current = ending;
      if (!deps.followingTail()) {
        release("the reader is not following the live tail", false);
        return;
      }
      log.debug(`holding the ended quiet-stretch line until row ${row} is painted`, {
        operation: "footer.quiet-hold",
        context: { row, text: ending.text },
      });
      const painted = deps.paints.paintedAt(row);
      if (painted !== null) {
        dwellFrom(painted);
        return;
      }
      unsubscribe = deps.paints.onPainted((id, at) => {
        if (id === row) dwellFrom(at);
      });
    },
    held: () => current,
    dispose() {
      release("the footer was disposed", false);
    },
  };

  /** Release the hold QUIET_HOLD_DWELL_MS after the row painted at AT. */
  function dwellFrom(at: number): void {
    unsubscribe?.();
    unsubscribe = null;
    const wait = Math.max(0, at + QUIET_HOLD_DWELL_MS - deps.now());
    timer = deps.setTimer(() => {
      timer = null;
      release("its successor was painted and the dwell ran out", true);
    }, wait);
  }

  /** Drop the hold, redrawing when REDRAW (a push draws on its own). */
  function release(reason: string, redraw: boolean): void {
    if (current === null) return;
    const row = current.untilPainted?.value ?? "";
    log.debug(`released the ended quiet-stretch line: ${reason}`, {
      operation: "footer.quiet-hold-released",
      context: { row, reason },
    });
    released = row;
    current = null;
    unsubscribe?.();
    unsubscribe = null;
    if (timer !== null) deps.clearTimer(timer);
    timer = null;
    if (redraw) deps.redraw();
  }
}
