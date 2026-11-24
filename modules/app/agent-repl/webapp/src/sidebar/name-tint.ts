/**
 * name-tint — a workspace name wears its status color until the user views
 * the workspace (owner request, 2026-11-23).
 *
 * A row that is not viewed (`frontend.v1.RosterRowViewed` absent) draws its
 * name in the status tone, steadily, on every status and however long ago its
 * turn ended: there is no fade. A viewed row draws its name in the viewed grey
 * (`viewed.ts`), and a status the vocabulary gives no color (`none`) draws the
 * name in its resting grey. The daemon lowers the viewed marker on every
 * status change, so a name returns to its tone the moment the workspace moves.
 *
 * A SYNC MERGE HOLDS THE NAME AT THE MERGE'S PURPLE (owner request,
 * 2026-10-31): while the row's when-column counts a sync merge
 * (`frontend.v1.RosterRowWhen.sync_merging`), the name wears the merge arm's
 * own vocabulary color steadily, whatever the row's arm (the merge's own
 * turns included) and whether or not the row was viewed. An ASYNC merge never
 * reaches that column (`frontend.v1.RosterRowAsyncMerging` leaves the row its
 * turn clock), so its name keeps its turn's tint.
 */
import { rosterStatusColor, toneClass } from "../vocab.js";

/** The class of a name held at its status tone. */
export const NAME_STEADY_CLASS = "tinted";

/** What a name draws of its status tone. */
export type NameTint = { kind: "steady" } | { kind: "syncMerging"; tone: string };

/**
 * The tint a row's name wears, or null for none. `shown` is the case of the
 * row's `when` column, `tone` the row's `.tone-<color>` class, and `viewed`
 * whether the row draws in the PARTIAL display mode.
 */
export function nameTint(shown: string, tone: string, viewed: boolean): NameTint | null {
  if (shown === "syncMerging") return { kind: "syncMerging", tone: toneClass(rosterStatusColor("merging")) };
  if (tone === toneClass("none")) return null;
  if (viewed) return null;
  return { kind: "steady" };
}

/** Draw TINT on the name LABEL. */
export function applyNameTint(label: HTMLElement, tint: NameTint | null): void {
  if (tint === null) return;
  label.classList.add(NAME_STEADY_CLASS);
  if (tint.kind === "syncMerging") {
    // The tone class on the name outranks the row's (src/styles.css).
    label.classList.add(tint.tone);
  }
}
