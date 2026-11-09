/**
 * name-tint — a workspace name starts at its status color and fades to its
 * greyscale (owner request, 2026-10-29).
 *
 * While a turn runs the name wears the status tone steadily. When a turn
 * ends the name starts at the tone and the stylesheet fades it to the name's
 * resting grey (`--name-rest`: unviewed, viewed or cold-gated). THE NAME HOLDS
 * 20 MINUTES, THEN FADES OVER 10 (owner request, 2026-11-08): the animation
 * `--name-fade` spans both (`proto/vocab/name-fade.json`), static first. This
 * is independent of the page hold (lisp/page-unload.el), which the fade no
 * longer indicates. The fade is a finite CSS animation whose delay
 * is minus the time since the turn ended, so a row redrawn by a roster push
 * rejoins the fade where the clock says it has got to, and a turn that ended
 * long ago draws at rest.
 *
 * VIEWING ENDS THE FADE (owner request, 2026-10-30): a row the user has
 * viewed (the PARTIAL display mode, `viewed.ts`) draws its name at the viewed
 * grey at once. The fade is a CSS animation, and an animation outranks the
 * `.viewed` class's color, so a fading name would otherwise stay in its tone
 * for the rest of the window however long the user looked at it.
 *
 * A SYNC MERGE HOLDS THE NAME AT THE MERGE'S PURPLE (owner request,
 * 2026-10-31): while the row's when-column counts a sync merge
 * (`frontend.v1.RosterRowWhen.sync_merging`), the name wears the merge arm's
 * own vocabulary color steadily, whatever the row's arm (the merge's own
 * turns included) and whether or not the row was viewed. An ASYNC merge never
 * reaches that column (`frontend.v1.RosterRowAsyncMerging` leaves the row its
 * turn clock), so its name keeps its turn's tint.
 */
import nameFade from "../../../proto/vocab/name-fade.json";
import { rosterStatusColor, toneClass } from "../vocab.js";

/** The class of a name held at its status tone (a turn is running). */
export const NAME_STEADY_CLASS = "tinted";
/** The class of a name fading from its status tone to its resting grey. */
export const NAME_FADING_CLASS = "fading";
/** The custom property carrying the fade's length: the page hold. */
export const NAME_FADE_PROPERTY = "--name-fade";
/** How long a finished workspace's name holds its tone before fading. */
export const NAME_HOLD_MS = nameFade.hold_seconds * 1000;
/** How long the fade itself takes, once the hold has passed. */
export const NAME_FADE_ONLY_MS = nameFade.fade_seconds * 1000;
/** The fade animation's whole length: the hold, then the fade. */
export const NAME_FADE_MS = NAME_HOLD_MS + NAME_FADE_ONLY_MS;
/** The custom property carrying minus the time since the turn ended. */
export const NAME_FADE_DELAY_PROPERTY = "--name-fade-delay";

/** What a name draws of its status tone. */
export type NameTint =
  | { kind: "steady" }
  | { kind: "fading"; elapsedMs: number }
  | { kind: "syncMerging"; tone: string };

/**
 * The tint a row's name wears, or null for none. `shown` is the case of the
 * row's `when` column, `endedMs` the last turn's end when that column
 * carries one, `tone` the row's `.tone-<color>` class, and `viewed` whether
 * the row draws in the PARTIAL display mode.
 */
export function nameTint(
  shown: string,
  endedMs: number | undefined,
  tone: string,
  viewed: boolean,
  nowMs: number,
): NameTint | null {
  if (shown === "syncMerging") return { kind: "syncMerging", tone: toneClass(rosterStatusColor("merging")) };
  if (tone === toneClass("none")) return null;
  if (viewed) return null;
  if (shown === "turnRunning") return { kind: "steady" };
  if (shown === "noTurnRunning" && endedMs !== undefined) {
    return { kind: "fading", elapsedMs: Math.max(0, nowMs - endedMs) };
  }
  return null;
}

/** Draw TINT on the name LABEL. */
export function applyNameTint(label: HTMLElement, tint: NameTint | null): void {
  if (tint === null) return;
  if (tint.kind === "steady") {
    label.classList.add(NAME_STEADY_CLASS);
    return;
  }
  if (tint.kind === "syncMerging") {
    // The tone class on the name outranks the row's (src/styles.css).
    label.classList.add(NAME_STEADY_CLASS, tint.tone);
    return;
  }
  label.classList.add(NAME_FADING_CLASS);
  label.style.setProperty(NAME_FADE_PROPERTY, `${NAME_FADE_MS}ms`);
  label.style.setProperty(NAME_FADE_DELAY_PROPERTY, `-${Math.round(tint.elapsedMs)}ms`);
}
