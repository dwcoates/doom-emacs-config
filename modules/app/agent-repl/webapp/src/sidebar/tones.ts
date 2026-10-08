/**
 * tones — the rail's ONE table from a `RosterRow.status` arm to what the row
 * draws for it: a color class, a glyph name, and the character (if any) that
 * glyph is drawn with.
 *
 * THIS TABLE IS A THIN WRAPPER, NEVER A SECOND OPINION. The colors live in
 * `proto/vocab/render-colors.json`, which Go, TypeScript and elisp all read,
 * and `src/vocab.ts` is this renderer's one reader of it. The table below
 * exists so the rail has a single enumerated home for its arms — and so
 * `test/sidebar/tones.test.ts` can assert it ROW FOR ROW against the file and
 * against `RosterRowSchema`'s own oneof. That assertion is the whole point: an
 * arm added to the proto without a color, or a color added to the file without
 * a row here, fails loudly in the suite instead of drawing a quiet grey dot.
 *
 * GLYPHS, NOT EMOJIS. The merge arms spend no color (they are `none` in the
 * file) and report themselves with a glyph instead; `inactive` draws a plain
 * question mark, `none` and a landed merge's `check` draw nothing at all but
 * still hold the column's width so names stay aligned. Every character here is a plain text glyph —
 * no emoji presentation, no variation selectors.
 */
import { toneClass, rosterStatusColor, mergeGlyph } from "../vocab.js";
import { MalformedView } from "../rpc/malformed.js";

/**
 * Every arm of `frontend.v1.RosterRow.status`, in the generated lowerCamel
 * spelling a received row carries. Declaration order is the proto's.
 */
export const ROSTER_STATUS_CASES = [
  "submitting",
  "thinking",
  "clearing",
  "compacting",
  "permission",
  "question",
  "done",
  "interrupted",
  "turnFailed",
  "ready",
  "idleAsync",
  "vendorBlocked",
  "vendorFault",
  "networkFault",
  "apiRetrying",
  "closing",
  "daemonImpaired",
  "waiting",
  "init",
  "severed",
  "startFailed",
  "degraded",
  "dead",
  "turnDied",
  "merging",
  "mergeQueued",
  "mergeFailed",
  "merged",
  "none",
  "inactive",
] as const;

/** One arm of the status oneof, as the generated code spells it. */
export type RosterStatusCase = (typeof ROSTER_STATUS_CASES)[number];

/**
 * The color class each arm paints, resolved through the shared vocabulary.
 *
 * Derived rather than transcribed: transcribing the five colors here would
 * create the second table the vocabulary file exists to prevent. What the
 * rail owns is the LIST of arms above; what a listed arm is worth in color is
 * the file's, every time.
 */
export const ROSTER_ARM_CLASS: Readonly<Record<RosterStatusCase, string>> = Object.freeze(
  Object.fromEntries(
    ROSTER_STATUS_CASES.map((arm) => [arm, toneClass(rosterStatusColor(arm))]),
  ) as Record<RosterStatusCase, string>,
);

/**
 * The character each shared glyph NAME is drawn with.
 *
 * The names are the vocabulary file's (`merge_glyphs`); which mark stands for
 * one is this surface's own business, which is exactly why the file ships
 * names rather than characters.
 */
export const GLYPH_CHARS: Readonly<Record<string, string>> = Object.freeze({
  queue: "≡",
  recycle: "⟳",
  failed: "✕",
  // A LANDED MERGE DRAWS NOTHING (owner request, 2026-10-08: no green check in
  // the rail). The vocabulary still names its glyph `check`, which other
  // surfaces may draw; here the name maps to no character, so the mark's box
  // (a `.st-glyph`, no disc) is empty yet holds the column, as `none` does.
  check: "",
  inactive: "?",
  dot: "",
  none: "",
});

/** What a row draws for its status. */
export interface RosterArmMark {
  /** The `.tone-<color>` class from the shared vocabulary. */
  toneClass: string;
  /** The `data-glyph` name: a merge glyph, or `dot` / `inactive` / `none`. */
  glyph: string;
  /** The character drawn inside the mark; "" leaves a bare CSS disc. */
  char: string;
}

/**
 * The mark for ARM.
 *
 * An arm with no row in `ROSTER_ARM_CLASS` is a MalformedView rather than a
 * default: a build that has never heard of a status cannot honestly draw one,
 * and the vocabulary check in the suite is what keeps that unreachable.
 */
export function rosterArmMark(arm: string): RosterArmMark {
  const tone = ROSTER_ARM_CLASS[arm as RosterStatusCase];
  if (tone === undefined) {
    throw new MalformedView(
      "RosterRow.status",
      `arm '${arm}' has no drawn mark in the rail's table`,
    );
  }
  const glyph = glyphName(arm as RosterStatusCase);
  return { toneClass: tone, glyph, char: GLYPH_CHARS[glyph] ?? "" };
}

/**
 * The glyph NAME an arm reports itself with.
 *
 * The merge family asks the shared vocabulary (so the rail and the tab bar
 * name the same mark); the two session-less arms have their own names, and
 * every lifecycle arm is simply the dot.
 */
export function glyphName(arm: RosterStatusCase): string {
  if (arm === "inactive" || arm === "none") return arm;
  if (arm.startsWith("merg")) return mergeGlyph(arm);
  return "dot";
}

/**
 * Whether the arm's mark BREATHES — the rail's one animation, and its meaning
 * is always the same: work is in flight and a prompt cannot land right now.
 *
 * The legacy rail already made exactly this claim for these arms and it is
 * kept unchanged: the four busy states and a pending permission, question or
 * other wait on you (ready AND waiting on you) breathe; detached work breathes in its own amber; a merge
 * in hand spins instead. Everything else is still, because nothing about it is
 * in progress.
 */
export function armBreathes(arm: RosterStatusCase): boolean {
  switch (arm) {
    case "submitting":
    case "thinking":
    case "clearing":
    case "compacting":
    case "permission":
    case "question":
    case "waiting":
    case "idleAsync":
      return true;
    default:
      return false;
  }
}

/** Whether the arm's glyph SPINS: the merge run the queue is actively on. */
export function armSpins(arm: RosterStatusCase): boolean {
  return arm === "merging";
}
