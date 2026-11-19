/**
 * view-dwell — a roster row's NAME recedes to the viewed grey continuously
 * over the editor's view dwell, instead of flipping to it when the dwell ends
 * (owner request, 2026-11-19).
 *
 * THE WIRE DECIDES WHEN. `frontend.v1.RosterRowViewDwell` stands on the
 * selected row while the editor counts the user's dwell on it, and only where
 * the dwell's completion will draw the row PARTIAL (`viewed.ts`). Its start
 * and length place the name in the fade: the animation's delay is minus the
 * time already dwelt, so a row redrawn by a roster push rejoins the fade where
 * the clock says it has got to. The fade ends at the viewed grey exactly as
 * the viewed marker arrives. When the marker is withdrawn early (the user
 * left, closed the panels, or the status changed) the next draw carries no
 * class, so the name is back at its unviewed colour at once.
 *
 * THE FADE IS A CROSSFADE ON THE COMPOSITOR, the breath's technique
 * (`name-ink.ts`): the ink box's `::before` copy, in the name's own colour
 * (any ended-turn fade included), fades out as its `::after` copy, in the
 * viewed grey, fades in, by `opacity` alone. `styles.css` owns every look;
 * this only stamps the class and the fade's place.
 */
import type { RosterRowViewDwell } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../rpc/malformed.js";
import { msOf } from "../rpc/strict.js";

/** The class of a name receding over the view dwell; the ink box keys on it. */
export const VIEW_DWELL_CLASS = "viewing";
/** The custom property carrying the dwell's length. */
export const VIEW_DWELL_PROPERTY = "--name-view-dwell";
/** The custom property carrying minus the time already dwelt. */
export const VIEW_DWELL_DELAY_PROPERTY = "--name-view-dwell-delay";

/** A view dwell as the wire states it, in epoch milliseconds. */
export interface ViewDwell {
  startedAtMs: number;
  lengthMs: number;
}

/**
 * The view-dwell marker, validated. A length that is not positive is a
 * contract breach and throws rather than drawing a fade of no length.
 */
export function drawRosterRowViewDwell(u: RosterRowViewDwell, path: string): ViewDwell {
  const lengthMs = msOf(u.lengthMs, `${path}.length_ms`);
  if (lengthMs <= 0) throw new MalformedView(`${path}.length_ms`, `a dwell lasts a positive time, not ${lengthMs}ms`);
  return { startedAtMs: msOf(u.startedAtMs, `${path}.started_at_ms`), lengthMs };
}

/** How far into DWELL the name is at NOW, clamped to the dwell. */
export function viewDwellElapsedMs(dwell: ViewDwell, nowMs: number): number {
  return Math.min(dwell.lengthMs, Math.max(0, nowMs - dwell.startedAtMs));
}

/** Draw DWELL on the name LABEL, as of NOW. */
export function applyViewDwell(label: HTMLElement, dwell: ViewDwell, nowMs: number): void {
  label.classList.add(VIEW_DWELL_CLASS);
  label.style.setProperty(VIEW_DWELL_PROPERTY, `${dwell.lengthMs}ms`);
  label.style.setProperty(VIEW_DWELL_DELAY_PROPERTY, `-${Math.round(viewDwellElapsedMs(dwell, nowMs))}ms`);
}
