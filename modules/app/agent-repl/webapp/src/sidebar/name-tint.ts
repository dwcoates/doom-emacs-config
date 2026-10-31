/**
 * name-tint — a workspace name starts at its status color and fades to its
 * greyscale (owner request, 2026-10-29).
 *
 * While a turn runs the name wears the status tone steadily. When a turn
 * ends the name starts at the tone and the stylesheet fades it to the name's
 * resting grey (`--name-rest`: unviewed, viewed or cold-gated) over
 * `--name-fade` (10 minutes). The fade is a finite CSS animation whose delay
 * is minus the time since the turn ended, so a row redrawn by a roster push
 * rejoins the fade where the clock says it has got to, and a turn that ended
 * long ago draws at rest.
 *
 * VIEWING ENDS THE FADE (owner request, 2026-10-30): a row the user has
 * viewed (the PARTIAL display mode, `viewed.ts`) draws its name at the viewed
 * grey at once. The fade is a CSS animation, and an animation outranks the
 * `.viewed` class's color, so a fading name would otherwise stay in its tone
 * for the rest of the window however long the user looked at it.
 */
import { toneClass } from "../vocab.js";

/** The class of a name held at its status tone (a turn is running). */
export const NAME_STEADY_CLASS = "tinted";
/** The class of a name fading from its status tone to its resting grey. */
export const NAME_FADING_CLASS = "fading";
/** The custom property carrying minus the time since the turn ended. */
export const NAME_FADE_DELAY_PROPERTY = "--name-fade-delay";

/** What a name draws of its status tone. */
export type NameTint = { kind: "steady" } | { kind: "fading"; elapsedMs: number };

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
  label.classList.add(NAME_FADING_CLASS);
  label.style.setProperty(NAME_FADE_DELAY_PROPERTY, `-${Math.round(tint.elapsedMs)}ms`);
}
