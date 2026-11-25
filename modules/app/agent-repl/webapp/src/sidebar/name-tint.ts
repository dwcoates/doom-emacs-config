/**
 * name-tint — which ink a workspace's name draws in (owner rulings,
 * 2026-11-23 and 2026-11-25).
 *
 * The rules, first match wins:
 *
 * - A SYNC MERGE HOLDS THE NAME AT THE MERGE'S PURPLE (owner request,
 *   2026-10-31): while the row's when-column counts a sync merge
 *   (`frontend.v1.RosterRowWhen.sync_merging`), the name wears the merge
 *   arm's own vocabulary color, whatever the row's arm (the merge's own turns
 *   included) and whether or not the row was viewed.
 * - AN ASYNC MERGE CHANGES NOTHING OF THE NAME: the row's purple mark says
 *   the merge, and the name follows the rules below on the row's own arm, so
 *   an idle async-merging name greys once viewed like any idle name.
 * - A WORKING OR MERGING ROW HOLDS ITS STATUS COLOR: a red arm (a turn in
 *   flight, a context cut) or a purple one (the row's own merge) draws the
 *   name in its tone, viewed or not. A background row
 *   (`frontend.v1.RosterRowStatusIdleAsync`) that runs a turn is on a red
 *   arm, so it is held red the same way.
 * - EVERY OTHER ROW IS IDLE, and its name wears its status color until the
 *   user views it (`frontend.v1.RosterRowViewed`), then the viewed grey. A
 *   status the vocabulary gives no color (`none`) draws the resting ink. An
 *   idle background row draws the color of the unread turn end beneath it
 *   (`frontend.v1.RosterRowIdleAsyncUnread`: green or turquoise), never the
 *   background's yellow, and the resting ink with no unread turn end.
 *
 * The daemon lowers the viewed marker when a turn starts, so an idle name
 * returns to its tone with the next turn.
 */
import { rosterStatusColor, toneClass } from "../vocab.js";

/** The class of a name held at a status tone. */
export const NAME_STEADY_CLASS = "tinted";

/** The class of a name drawn in the viewed grey. */
export const NAME_VIEWED_CLASS = "viewed";

/**
 * What a row's name draws: its row's tone (`steady`), a tone of its own
 * (`toned`, the tone class put on the name), the viewed grey, or the resting
 * ink.
 */
export type NameTint =
  | { kind: "steady" }
  | { kind: "toned"; tone: string }
  | { kind: "viewed" }
  | { kind: "rest" };

/** The facts of a row the name's ink is decided from. */
export interface NameFacts {
  /** The case of the row's `when` column. */
  shown: string;
  /** The row's status arm. */
  arm: string;
  /** The row's `.tone-<color>` class. */
  tone: string;
  /** Whether the row draws in the PARTIAL (viewed) display mode. */
  viewed: boolean;
  /** The arm of the unread turn end under a background row, if one stands. */
  unreadTurnEnd: string | undefined;
}

/** The tones a name holds whether or not the row was viewed: working and merging. */
const HELD_TONES: ReadonlySet<string> = new Set([toneClass("red"), toneClass("purple")]);

/** The ink a row's name draws, by the rules above. */
export function nameTint(f: NameFacts): NameTint {
  if (f.shown === "syncMerging") return { kind: "toned", tone: toneClass(rosterStatusColor("merging")) };
  if (HELD_TONES.has(f.tone)) return { kind: "steady" };
  if (f.viewed) return { kind: "viewed" };
  if (f.arm === "idleAsync") {
    if (f.unreadTurnEnd === undefined) return { kind: "rest" };
    return { kind: "toned", tone: toneClass(rosterStatusColor(f.unreadTurnEnd)) };
  }
  if (f.tone === toneClass("none")) return { kind: "rest" };
  return { kind: "steady" };
}

/** Draw TINT on the name LABEL. */
export function applyNameTint(label: HTMLElement, tint: NameTint): void {
  switch (tint.kind) {
    case "rest":
      return;
    case "viewed":
      label.classList.add(NAME_VIEWED_CLASS);
      return;
    case "steady":
      label.classList.add(NAME_STEADY_CLASS);
      return;
    case "toned":
      // The tone class on the name outranks the row's (src/styles.css).
      label.classList.add(NAME_STEADY_CLASS, tint.tone);
      return;
  }
}
