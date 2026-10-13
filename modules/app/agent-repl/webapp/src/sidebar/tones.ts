/**
 * tones — the rail's ONE table from a `RosterRow.status` arm to what the row
 * draws for it: a color class, a glyph name, the character (if any) that
 * glyph is drawn with, the ink the mark takes, and its motion.
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
 * ONE SIZE FOR EVERY MARK (owner ruling, 2026-10-13): the dot, the merge glyph
 * and the three emojis all draw at the dot's visible size, so no mark reads
 * bigger than another. The merge glyph is drawn by the stylesheet (three bars
 * in the dot's box, so it turns on its own center); the gate emojis ❗ ❓ 🛑 and
 * a working one-shot's 🎯 are characters, the same ones the Emacs tab bar
 * draws before the workspace name.
 */
import {
  toneClass,
  rosterStatusColor,
  mergeGlyph,
  gateGlyph,
  glyphInk,
  oneShotGlyphArm,
} from "../vocab.js";
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
  "coldGate",
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
 * The names are the vocabulary file's (`merge_glyphs`, `gate_glyphs`); which
 * mark stands for one is this surface's own business, which is exactly why the
 * file ships names rather than characters. A name mapped to "" is drawn by the
 * stylesheet alone: the dot's disc, and the merge glyph's three bars.
 */
export const GLYPH_CHARS: Readonly<Record<string, string>> = Object.freeze({
  // EVERY MERGE ARM (owner ruling, 2026-10-13): three bars, drawn by the
  // stylesheet in the dot's box. Its ink says which arm, and `merging` turns
  // it on its own center.
  merge: "",
  // A GATE ON YOU (owner requests, 2026-10-11 and 2026-10-13): the emoji
  // replaces the dot, and matches the one the Emacs tab bar draws.
  exclaim: "❗",
  ask: "❓",
  stop: "🛑",
  // A ONE-SHOT AT WORK (owner ruling, 2026-10-13): the emoji replaces the dot
  // while the one-shot works, and matches the Emacs tab bar's.
  target: "🎯",
  dot: "",
  none: "",
});

/** How a mark moves: the rail's two animations, or none. */
export type MarkMotion = "breathes" | "spins" | "still";

/** What a row draws for its status. */
export interface RosterArmMark {
  /** The `.tone-<color>` class from the shared vocabulary: the WORKSPACE's color. */
  toneClass: string;
  /** The `data-glyph` name: a merge, gate or one-shot glyph, or `dot` / `none`. */
  glyph: string;
  /** The character drawn inside the mark; "" leaves the stylesheet's shape. */
  char: string;
  /** The mark's own ink where it differs from the tone (`glyph_inks`), or undefined. */
  ink: string | undefined;
  /** The mark's motion. */
  motion: MarkMotion;
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
  return {
    toneClass: tone,
    glyph,
    char: GLYPH_CHARS[glyph] ?? "",
    ink: glyphInk(arm),
    motion: armMotion(arm as RosterStatusCase),
  };
}

/**
 * The mark for a row standing on ARM, ONE_SHOT saying whether the row carries
 * the one-shot mark (`RosterRowOneShot`).
 *
 * A one-shot draws its glyph in place of the arm's own mark ONLY while it
 * works (`render-colors.json#one_shot_glyph_arms`, owner ruling 2026-10-13):
 * submitting, thinking or clearing, never compacting, a gate, a merge or any
 * other arm. The glyph is STILL, and the row keeps the arm's tone.
 */
export function rosterRowMark(arm: string, oneShot: boolean): RosterArmMark {
  const mark = rosterArmMark(arm);
  if (!oneShot || !oneShotGlyphArm(arm)) return mark;
  return { toneClass: mark.toneClass, glyph: "target", char: GLYPH_CHARS.target, ink: undefined, motion: "still" };
}

/**
 * The glyph NAME an arm reports itself with.
 *
 * The merge and gate families ask the shared vocabulary (so the rail and the
 * tab bar name the same mark); `none` draws nothing, and every other arm,
 * `inactive` and `init` included, is simply the dot.
 */
export function glyphName(arm: RosterStatusCase): string {
  if (arm === "none") return "none";
  if (arm.startsWith("merg")) return mergeGlyph(arm);
  return gateGlyph(arm) ?? "dot";
}

/**
 * How the arm's mark MOVES. Both animations make one claim each, everywhere on
 * the rail: a breath is work in flight or a gate waiting on you, a spin is the
 * merge the queue is running.
 *
 * The four busy states, the three gates, an interrupt landing and detached
 * work breathe; `merging` spins; everything else is still, because nothing
 * about it is in progress.
 */
export function armMotion(arm: RosterStatusCase): MarkMotion {
  switch (arm) {
    case "submitting":
    case "thinking":
    case "clearing":
    case "compacting":
    case "permission":
    case "question":
    case "coldGate":
    case "waiting":
    case "idleAsync":
      return "breathes";
    case "merging":
      return "spins";
    default:
      return "still";
  }
}
