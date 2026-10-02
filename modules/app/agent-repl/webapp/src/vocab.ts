/**
 * The shared rendering vocabulary, as typed accessors over the two JSON files
 * that ARE the contract.
 *
 * `proto/vocab/render-colors.json` and `proto/vocab/paint-classes.json` are
 * cross-language: Go, TypeScript and elisp each assert their own table against
 * the same file, row for row, which is the only mechanism that makes a
 * divergence between three renderers fail loudly instead of quietly. A
 * workspace that reads green in the webapp's rail and red in the Emacs tab bar
 * is the failure those files exist to prevent.
 *
 * THIS MODULE IS THE WEBAPP'S ONE READER OF THEM. A component asks for a
 * color by ARM NAME and gets one; it never keeps a local table, because a
 * local table is exactly the drift the files close off. The files pin WHICH
 * of the five colors a state takes, never the hex — `.tone-<color>` in the
 * stylesheet owns appearance.
 *
 * THERE IS NO TEAL. Five colors plus "none", and "none" is a real answer
 * rather than a gap: the merge states carry glyphs instead of spending a
 * color, and a workspace with no session has no lifecycle verdict to paint.
 *
 * ARMS ARRIVE IN GENERATED SPELLING. A consumer holds `row.status.case`,
 * which protobuf-es spells lowerCamel (`idleAsync`); the files are keyed by
 * the proto's own snake_case arm names (`idle_async`). The conversion happens
 * here, once, so no call site has to know there are two spellings.
 */
import renderColors from "../../proto/vocab/render-colors.json";
import paintClasses from "../../proto/vocab/paint-classes.json";
import { MalformedView } from "./rpc/malformed.js";
import { log } from "./log.js";

/** The six colors, plus the explicit absence of one. */
export type Color = "none" | "blue" | "purple" | "red" | "turquoise" | "yellow" | "green";

/** The three sides a failure can land on, from `failure_sides`. */
export type FailureSide = "machinery" | "vendor" | "client_local";

/** The CSS class a color paints with. Defined once by the chassis. */
export function toneClass(color: Color): string {
  return `tone-${color}`;
}

/**
 * The proto arm name for a generated lowerCamel case name.
 *
 * protobuf-es derives `idleAsync` from `idle_async`, and this reverses it.
 * Purely mechanical: an uppercase letter becomes an underscore and its own
 * lowercase, which is exact for the arm names in this contract (no arm has a
 * digit group or an acronym run).
 */
export function protoArmName(generatedCase: string): string {
  return generatedCase.replace(/[A-Z]/g, (ch) => `_${ch.toLowerCase()}`);
}

const ROSTER_STATUS: Readonly<Record<string, Color>> = renderColors.roster_status as Record<string, Color>;
const FOOTER_STATUS: Readonly<Record<string, Color>> = renderColors.footer_status as Record<string, Color>;
const TOPBAR_CONNECTIVITY: Readonly<Record<string, Color>> = renderColors.topbar_connectivity as Record<string, Color>;
const MERGE_GLYPHS: Readonly<Record<string, string>> = renderColors.merge_glyphs;
const FAILURE_SIDES: Readonly<Record<string, Color>> = renderColors.failure_sides as Record<string, Color>;
const FEED_SUBAGENT_DOT: Readonly<Record<string, Color>> = renderColors.feed_subagent_dot as Record<string, Color>;
const FEED_SHELL_DOT: Readonly<Record<string, Color>> = renderColors.feed_shell_dot as Record<string, Color>;

/** Every `feed_subagent_dot` key, for the row-for-row assertion. */
export const FEED_SUBAGENT_DOT_KEYS: readonly string[] = Object.keys(FEED_SUBAGENT_DOT);
/** Every `feed_shell_dot` key, for the row-for-row assertion. */
export const FEED_SHELL_DOT_KEYS: readonly string[] = Object.keys(FEED_SHELL_DOT);

/** The glyph NAME the feed's merge bubble head takes. */
export const FEED_MERGE_HEAD_GLYPH: string = renderColors.feed_merge_head_glyph;

/** Every `footer_status` key, for the same assertion on the footer. */
export const FOOTER_STATUS_ARMS: readonly string[] = Object.keys(FOOTER_STATUS);
/** The closed set of tones a received `TopbarConnectivity.tone` may name. */
export const TOPBAR_TONES: readonly string[] = renderColors.topbar_tones;

/**
 * The color a `RosterRow.status` arm paints the rail dot.
 *
 * ARM is the generated case name. An arm with no row here is a MalformedView
 * rather than a default: the file is the contract, and an unpainted dot that
 * silently picked grey is the drift this whole mechanism exists to catch.
 * The webapp takes NO surface overrides — its rail carries a glyph and a
 * status word beside every dot.
 */
export function rosterStatusColor(arm: string): Color {
  return lookup(ROSTER_STATUS, arm, "render-colors.json#roster_status");
}

/** The color a `FooterStatus.status` arm paints the footer strip. */
export function footerStatusColor(arm: string): Color {
  return lookup(FOOTER_STATUS, arm, "render-colors.json#footer_status");
}

/** The footer colors under which a composer is closed. */
const COMPOSER_CLOSED_COLORS: readonly string[] = renderColors.composer_closed_colors;

/**
 * The DECLARED exceptions to the composer invariant: per footer status arm
 * (proto spelling), the substatus arms under which the composer stays open
 * although the status's color closes it.
 */
const COMPOSER_OPEN_SUBSTATUSES: Readonly<Record<string, readonly string[]>> =
  renderColors.composer_open_substatuses;

/**
 * Whether a composer is CLOSED while its footer reads ARM.
 *
 * THE COMPOSER INVARIANT (owner ruling, 2026-09-28): a composer is closed
 * exactly when the footer's status color is one of
 * `render-colors.json#composer_closed_colors` — blue, an unusable workspace.
 * A merge in flight (purple) leaves it open: its prompts are held. The gate is DERIVED from the color, never
 * from a list of arm names, so an arm cannot be drawn usable and gated shut,
 * or drawn unusable and left open. An arm the file has no color for is a
 * MalformedView, exactly as it is for the color itself.
 *
 * The one way a closing color leaves the composer open is a substatus
 * DECLARED in `render-colors.json#composer_open_substatuses`; none is today
 * (the one there was, `blocked · api_retrying`, became the turquoise
 * `vendor_fault · api_retrying`, open by its color). SUBSTATUS is the substatus arm the footer drew, in the generated
 * spelling, or undefined for an arm with none.
 */
export function composerClosedFor(arm: string, substatus?: string): boolean {
  if (!COMPOSER_CLOSED_COLORS.includes(footerStatusColor(arm))) return false;
  const open = COMPOSER_OPEN_SUBSTATUSES[protoArmName(arm)] ?? [];
  return substatus === undefined || !open.includes(protoArmName(substatus));
}

/** The color a `TopbarConnectivity` link state paints its dot. */
export function topbarConnectivityColor(arm: string): Color {
  return lookup(TOPBAR_CONNECTIVITY, arm, "render-colors.json#topbar_connectivity");
}

/**
 * A received `TopbarConnectivity.tone`, validated against the closed set.
 *
 * The tone arrives as a STRING on the wire rather than as an arm, so it is the
 * one place a color is not already typed by the schema — and topbar.proto's
 * own comment still lists a "teal" that no producer may emit. The file is
 * authoritative, so a tone outside it is refused here rather than reaching a
 * `.tone-teal` rule that does not exist.
 */
export function topbarTone(tone: string): Color {
  if (!TOPBAR_TONES.includes(tone)) {
    throw new MalformedView(
      "TopbarConnectivity.tone",
      `tone '${tone}' is not one of render-colors.json#topbar_tones (${TOPBAR_TONES.join(", ")})`,
    );
  }
  return tone as Color;
}

/**
 * The glyph NAME a merge arm reports itself with.
 *
 * The merge arms take no color, so the glyph is the whole report. This returns
 * the shared NAME ("queue", "recycle", "failed", "check"); which
 * character or CSS shape draws it is the surface's own business.
 */
export function mergeGlyph(arm: string): string {
  const name = MERGE_GLYPHS[protoArmName(arm)];
  if (name === undefined) {
    throw new MalformedView(
      "render-colors.json#merge_glyphs",
      `arm '${arm}' has no glyph; a merge state without one cannot report itself`,
    );
  }
  return name;
}

/**
 * The color a failure card takes from the SIDE it landed on.
 *
 * Card color IS state color, from this one table, so a user cannot see a
 * purple workspace explained by a blue card. `client_local` — the six arms
 * this frontend mints — is blue, the same as `machinery`: both mean the route
 * to a working session is broken and neither is the vendor's doing.
 */
export function failureSideColor(side: FailureSide): Color {
  const color = FAILURE_SIDES[side];
  if (color === undefined) {
    throw new MalformedView("render-colors.json#failure_sides", `side '${side}' has no color`);
  }
  return color;
}

/**
 * The color a feed subagent bubble's head dot takes: STATE is `live`, or the
 * settled outcome arm (generated spelling). `none` is drawn hollow.
 */
export function feedSubagentDotColor(state: string): Color {
  return lookup(FEED_SUBAGENT_DOT, state, "render-colors.json#feed_subagent_dot");
}

/** The same for a detached shell bubble's head dot. */
export function feedShellDotColor(state: string): Color {
  return lookup(FEED_SHELL_DOT, state, "render-colors.json#feed_shell_dot");
}

const PAINT_SYNTAX: readonly string[] = paintClasses.syntax;
const PAINT_ANSI: readonly string[] = paintClasses.ansi;
/** The closed inventory of `paint_class` names, both families. */
export const PAINT_CLASS_NAMES: readonly string[] = [...PAINT_SYNTAX, ...PAINT_ANSI];

/**
 * The CSS class for a span's `paint_class`, or null for plain text.
 *
 * THE EMPTY STRING MEANS PLAIN and is the only value with that meaning, so
 * there is exactly one spelling of unstyled text on the wire.
 *
 * AN UNKNOWN CLASS IS UNSTYLED TEXT, NEVER AN ERROR — the one place in this
 * renderer where an unfamiliar value does not throw, and deliberately so. The
 * text is always the substance and the class is always decoration: a client
 * that has not caught up with a newly added class must still draw the span,
 * because dropping it or throwing would lose output the user needs to read
 * over a styling detail they do not. It is logged at warn so the drift is
 * still visible to whoever debugs it.
 */
export function paintClass(name: string): string | null {
  if (name === "") return null;
  if (PAINT_CLASS_NAMES.includes(name)) return `paint-${name}`;
  log.warn(`paint class '${name}' is not in the shared inventory; drawing the span unstyled`, {
    operation: "vocab.unknown-paint-class",
    context: { paint_class: name },
    dedupKey: `vocab.paint-class:${name}`,
  });
  return null;
}

function lookup(table: Readonly<Record<string, Color>>, arm: string, where: string): Color {
  const color = table[protoArmName(arm)];
  if (color === undefined) {
    throw new MalformedView(where, `arm '${arm}' has no color in the shared vocabulary`);
  }
  return color;
}
