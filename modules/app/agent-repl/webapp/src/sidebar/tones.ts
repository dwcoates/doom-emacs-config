/**
 * tones — the rail's ONE table from a `RosterRow.status` arm to what the row
 * draws for it: a color class, a glyph name, the picture (if any) that
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
 * ONE SIZE FOR EVERY MARK (owner ruling, 2026-10-13): the dot and every glyph
 * draw at the dot's visible size, so no mark reads bigger than another. Every
 * glyph -- the merge glyph's three bars, the three gates' exclamation mark,
 * question mark and snowflake, and a working one-shot's target -- is a
 * picture, images/glyphs/<name>.svg, the SAME file the Emacs tab bar draws
 * before the workspace name (lisp/status.el, "The glyph's image"). Each
 * picture is cropped to fill its file's full height, so drawn at the dot's
 * height it is exactly as tall as the dot.
 */
import {
  toneClass,
  rosterStatusColor,
  mergeGlyph,
  gateGlyph,
  glyphInk,
  oneShotGlyphArm,
  asyncMergeMark,
  pausedMergeMark,
} from "../vocab.js";
import { MalformedView } from "../rpc/malformed.js";
import mergePicture from "../../../images/glyphs/merge.svg?raw";
import exclaimPicture from "../../../images/glyphs/exclaim.svg?raw";
import askPicture from "../../../images/glyphs/ask.svg?raw";
import snowflakePicture from "../../../images/glyphs/snowflake.svg?raw";
import targetPicture from "../../../images/glyphs/target.svg?raw";
import { oneofArms } from "../rpc/oneof.js";
import { RosterRowSchema, type RosterRow } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";

/** One arm of the status oneof, as the generated code spells it. */
export type RosterStatusCase = NonNullable<RosterRow["status"]["case"]>;

/**
 * Every arm of `frontend.v1.RosterRow.status`, in the generated lowerCamel
 * spelling a received row carries, read off the SCHEMA (`rpc/oneof.ts`) in
 * the proto's declaration order. An arm added to the proto reaches the colour
 * table below by itself, and `ROSTER_ARM_CLASS` then fails at import for an
 * arm the shared vocabulary has no colour for.
 */
export const ROSTER_STATUS_CASES = oneofArms(RosterRowSchema, "status") as readonly RosterStatusCase[];

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
 * The picture each shared glyph NAME is drawn with: the SVG markup of
 * images/glyphs/<name>.svg, which the Emacs tab bar draws too.
 *
 * The names are the vocabulary file's (`merge_glyphs`, `gate_glyphs`); the file
 * ships names rather than pictures, and this surface and the tab bar map each
 * name to the one shared file. A name mapped to "" is drawn by the stylesheet
 * alone: the dot's disc. `merge` and the dot are drawn in `currentColor`, so
 * the tone or the ink colors them; the other pictures carry their own colors.
 */
export const GLYPH_IMAGES: Readonly<Record<string, string>> = Object.freeze({
  // EVERY MERGE ARM (owner ruling, 2026-10-13): three bars. Its ink says which
  // arm, and `merging` breathes the row's name.
  merge: mergePicture,
  // A GATE ON YOU (owner requests, 2026-10-11 and 2026-10-13): the picture
  // replaces the dot.
  exclaim: exclaimPicture,
  ask: askPicture,
  // A COLD GATE (owner ruling, 2026-10-13): the session has gone cold.
  snowflake: snowflakePicture,
  // A ONE-SHOT AT WORK (owner ruling, 2026-10-13): the picture replaces the dot
  // while the one-shot works.
  target: targetPicture,
  dot: "",
  none: "",
});

/**
 * How a row moves: the rail's one animation, or none. The mark itself never
 * moves (owner request, 2026-10-14): row.ts puts the breath on the row's NAME.
 */
export type MarkMotion = "breathes" | "still";

/** What a row draws for its status. */
export interface RosterArmMark {
  /** The `.tone-<color>` class from the shared vocabulary: the WORKSPACE's color. */
  toneClass: string;
  /** The `data-glyph` name: a merge, gate or one-shot glyph, or `dot` / `none`. */
  glyph: string;
  /** The SVG picture drawn inside the mark; "" leaves the stylesheet's shape. */
  image: string;
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
    image: GLYPH_IMAGES[glyph] ?? "",
    ink: glyphInk(arm),
    motion: armMotion(arm as RosterStatusCase),
  };
}

/**
 * The row's markers that change its status MARK, each read off its own
 * `frontend.v1.RosterRow` field by presence: `one_shot`
 * (`RosterRowOneShot`), `async_merging` (`RosterRowAsyncMerging`) and
 * `merge_paused` (`RosterRowMergePaused`).
 */
export interface RowMarkers {
  oneShot: boolean;
  asyncMerging: boolean;
  mergePaused: boolean;
}

/** A row carrying no marker. */
export const NO_MARKERS: RowMarkers = Object.freeze({ oneShot: false, asyncMerging: false, mergePaused: false });

/** The status arms that draw the row's OWN merge: a merge running or waiting. */
const OWN_MERGE_ARMS: ReadonlySet<string> = new Set(["merging", "mergeQueued"]);

/**
 * The mark for a row standing on ARM with MARKERS.
 *
 * A one-shot draws its glyph in place of the arm's own mark ONLY while it
 * works (`render-colors.json#one_shot_glyph_arms`, owner ruling 2026-10-13):
 * submitting, thinking or clearing, never compacting, a gate, a merge or any
 * other arm. The row's name breathes, as the work it stands for does, and
 * the row keeps the arm's tone.
 *
 * An ASYNC MERGE of the workspace's own branch draws the merge glyph in place
 * of the PLAIN dot -- the `dot` glyph with no ink of its own -- in the glyph,
 * ink and motion of `render-colors.json#async_merge_mark` (owner requests,
 * 2026-10-13 and 2026-10-14). Every other mark outranks it: a gate's glyph,
 * the one-shot glyph, a merge glyph, an inked dot (compacting's orange) and no
 * mark at all. The row keeps the arm's tone either way, so its NAME stays the
 * workspace's own status color.
 *
 * A PAUSED QUEUE (`merge_paused`, owner ruling 2026-10-14) greys every merge
 * indicator the row draws, in `render-colors.json#paused_merge_mark`'s ink,
 * and takes the merge's breath off the row's name. The merge glyph of a
 * `merging` or `merge_queued` arm takes the section's motion. The async-merge
 * mark takes the ARM's own motion back, so a turn in flight under it still
 * breathes, because that breath is the turn's. The tone stands, so the name
 * keeps its color. The marker on a row drawing neither the arm nor the async
 * marker breaks the contract and is refused.
 */
export function rosterRowMark(arm: string, markers: RowMarkers): RosterArmMark {
  const mark = rosterArmMark(arm);
  const ownMerge = OWN_MERGE_ARMS.has(arm);
  if (markers.mergePaused && !ownMerge && !markers.asyncMerging) {
    throw new MalformedView(
      "RosterRow.merge_paused",
      `the marker stands on arm '${arm}' with no async_merging marker, so the row draws no merge for it to pause`,
    );
  }
  const paused = pausedMergeMark();
  if (markers.oneShot && oneShotGlyphArm(arm)) {
    return { toneClass: mark.toneClass, glyph: "target", image: GLYPH_IMAGES.target, ink: undefined, motion: "breathes" };
  }
  if (markers.asyncMerging && mark.glyph === "dot" && mark.ink === undefined) {
    const merge = asyncMergeMark();
    const drawn = { ...mark, glyph: merge.glyph, image: GLYPH_IMAGES[merge.glyph], ink: merge.ink, motion: merge.motion as MarkMotion };
    return markers.mergePaused ? { ...drawn, ink: paused.ink, motion: mark.motion } : drawn;
  }
  if (markers.mergePaused && ownMerge) {
    return { ...mark, ink: paused.ink, motion: paused.motion as MarkMotion };
  }
  return mark;
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
 * How the arm's row MOVES. The rail has ONE animation, the breath on the
 * row's name (owner request, 2026-10-14: never on the mark), and it
 * makes ONE claim everywhere (owner ruling, 2026-10-13): something is in
 * progress. A turn in flight, its context cuts, detached work, a session
 * booting and a merge running breathe (and a row with an async merge, through
 * `rosterRowMark`). A gate (permission, question, cold
 * gate) and an interrupt landing wait on the user, so nothing is in progress
 * and they hold still, as everything else does.
 */
export function armMotion(arm: RosterStatusCase): MarkMotion {
  switch (arm) {
    case "submitting":
    case "thinking":
    case "clearing":
    case "compacting":
    case "idleAsync":
    case "init":
    case "merging":
      return "breathes";
    default:
      return "still";
  }
}
