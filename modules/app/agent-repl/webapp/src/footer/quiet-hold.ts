/**
 * quiet-hold — the ended quiet-stretch line, held until its successor shows.
 *
 * The daemon ends a quiet stretch the moment the feed DRAWS the next item, but
 * this page paints that item later; clearing the line on the daemon's edge left
 * a visible gap with neither the line nor the item on screen (owner report,
 * 2026-09-29). So the daemon states the ended line and the row that ended it
 * (`FooterStatusQuietStretchEnding`), and this page decides when it goes:
 *
 * - It draws the ended line as the cell's quiet-stretch line (the daemon states
 *   an ending only while the cell would draw its enduring line), until it has
 *   PAINTED that row (`feed/painted.ts`), and clears it on that paint (owner
 *   ruling, 2026-09-29: no dwell after it). A row already painted when the
 *   ending arrives holds nothing.
 * - A reader not following the live tail is not looking where the row paints,
 *   so the hold is released at once.
 * - A push that states no ending, or another row's, releases the hold: the
 *   daemon stood a new line, an activity that outranks the line, or a new
 *   status. A row whose hold was released is never held again.
 */
import { clone, create } from "@bufbuild/protobuf";
import {
  FooterStatusActivityQuietStretchSchema,
  FooterStripSchema,
  type FooterStatus,
  type FooterStatusQuietStretchEnding,
  type FooterStrip,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import type { PaintWatch } from "../feed/painted.js";
import { log } from "../log.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireMessage } from "../rpc/strict.js";

export interface QuietHoldDeps {
  readonly paints: PaintWatch;
  /** Whether the reader is following the feed's live tail. */
  readonly followingTail: () => boolean;
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
 * STRIP with the held line drawn as its status's quiet-stretch line. Only
 * `working` and `background` state an ending, and the daemon states one only
 * while their cell is unpinned with nothing above its enduring line, so the
 * held line takes the quiet tier's place and every other tier is kept as
 * pushed.
 */
export function withHeldLine(strip: FooterStrip, ending: FooterStatusQuietStretchEnding): FooterStrip {
  const out = clone(FooterStripSchema, strip);
  const status = requireMessage(out.status, "FooterStrip.status");
  const line = create(FooterStatusActivityQuietStretchSchema, { text: ending.text });
  switch (status.status.case) {
    case "working":
    case "background": {
      const activity = requireMessage(
        status.status.value.activity,
        `FooterStatus.${status.status.case}.activity`,
      );
      // The daemon states an ending only while nothing stands above the
      // enduring line, so an ending beside a salient line is a malformed push.
      if (activity.tier.case !== "unpinned") {
        throw new MalformedView(
          `FooterStatus.${status.status.case}.quiet_stretch_ending`,
          "an ending is stated beside a salient line",
        );
      }
      activity.tier.value.quietStretch = line;
      return out;
    }
    default:
      return strip;
  }
}

/** Build the hold. */
export function createQuietHold(deps: QuietHoldDeps): QuietHold {
  let current: FooterStatusQuietStretchEnding | null = null;
  let released: string | null = null;
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
      if (deps.paints.paintedAt(row) !== null) {
        release("its successor was already painted", false);
        return;
      }
      unsubscribe = deps.paints.onPainted((id) => {
        if (id === row) release("its successor was painted", true);
      });
    },
    held: () => current,
    dispose() {
      release("the footer was disposed", false);
    },
  };

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
    if (redraw) deps.redraw();
  }
}
