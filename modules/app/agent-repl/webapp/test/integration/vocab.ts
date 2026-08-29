/**
 * THE VOCABULARY, read straight from `proto/vocab/*.json`.
 *
 * The suites assert the app's painted classes against THIS, not against a
 * table copied into the test: a copy would drift with the app's own copy and
 * agree with it while both diverged from the contract. Reading the file is
 * what makes a color assignment that changed in one place fail in the other.
 *
 * The app reads the same files through `src/vocab.ts`; the suites deliberately
 * do NOT import that module, so a bug in the app's accessors cannot make its
 * own assertions pass.
 */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const vocabFile = (name: string): unknown =>
  JSON.parse(readFileSync(fileURLToPath(new URL(`../../../proto/vocab/${name}`, import.meta.url)), "utf8"));

interface RenderColors {
  colors: string[];
  precedence: string[];
  roster_status: Record<string, string>;
  merge_glyphs: Record<string, string>;
  feed_merge_head_glyph: string;
  footer_status: Record<string, string>;
  topbar_connectivity: Record<string, string>;
  topbar_tones: string[];
  failure_sides: Record<string, string>;
}

interface PaintClasses {
  plain: string;
  ansi: string[];
  syntax: string[];
  ansi_precedence: string[];
}

export const RENDER_COLORS = vocabFile("render-colors.json") as RenderColors;
export const PAINT_CLASSES = vocabFile("paint-classes.json") as PaintClasses;

/** proto arm names are lowerCamel in the generated code, snake_case in the file. */
const snake = (arm: string): string => arm.replace(/[A-Z]/g, (c) => `_${c.toLowerCase()}`);

/** The color the vocabulary assigns a RosterRow.status arm. */
export const rosterStatusColor = (arm: string): string => lookup(RENDER_COLORS.roster_status, arm, "roster_status");

/** The color the vocabulary assigns a FooterStatus arm. */
export const footerStatusColor = (arm: string): string => lookup(RENDER_COLORS.footer_status, arm, "footer_status");

/** The glyph name the vocabulary assigns a merge roster arm. */
export const mergeGlyph = (arm: string): string => lookup(RENDER_COLORS.merge_glyphs, arm, "merge_glyphs");

/** The color a failure side takes; the client-local arms are all one side. */
export const failureSideColor = (side: "machinery" | "vendor" | "client_local"): string =>
  lookup(RENDER_COLORS.failure_sides, side, "failure_sides");

/** Whether `tone` is a tone the topbar may serve at all. */
export const isKnownTone = (tone: string): boolean => RENDER_COLORS.topbar_tones.includes(tone);

/** Whether `name` is a paint class either vocabulary list declares. */
export const isKnownPaintClass = (name: string): boolean =>
  PAINT_CLASSES.syntax.includes(name) || PAINT_CLASSES.ansi.includes(name);

/**
 * The class a span with `paintClass` must carry: `paint-<name>` for a declared
 * name, nothing for "" (plain) and nothing for an unknown name (unstyled — a
 * warning, never an error, so an unrecognized name still shows its text).
 */
export const expectedPaintClass = (name: string): string | undefined =>
  name === "" || !isKnownPaintClass(name) ? undefined : `paint-${name}`;

function lookup(table: Record<string, string>, arm: string, which: string): string {
  const key = snake(arm);
  const found = table[key];
  if (found === undefined) {
    throw new Error(`render-colors.json ${which} has no entry for ${JSON.stringify(key)} (arm ${arm})`);
  }
  return found;
}

/**
 * Assert a vocabulary table and a proto oneof's arms are the SAME SET — the
 * check that makes a new arm landing without a color fail loudly.
 */
export function assertVocabCoversArms(
  table: Record<string, string>,
  arms: readonly string[],
  which: string,
): void {
  const declared = arms.map(snake).sort();
  const keys = Object.keys(table).sort();
  const missing = declared.filter((a) => !keys.includes(a));
  const extra = keys.filter((k) => !declared.includes(k));
  if (missing.length > 0 || extra.length > 0) {
    throw new Error(
      `render-colors.json ${which}: arms with no entry [${missing.join(", ")}]` +
        `; entries that are not arms [${extra.join(", ")}]`,
    );
  }
}
