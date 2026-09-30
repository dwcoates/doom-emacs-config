/**
 * tones — the footer's ONE colour table, and the one place a typed datum in an
 * activity line is given a colour.
 *
 * THE STATUS TABLE IS NOT A TABLE. `render-colors.json#footer_status` is the
 * contract, so `STATUS_ARM_CLASS` is BUILT from `footerStatusColor` at module
 * load rather than restated beside it: a local copy is exactly the drift the
 * shared file exists to close off, and building it here means an arm the file
 * has no colour for fails LOUDLY the moment this module is imported instead of
 * drawing an unpainted status word. The suite then asserts the built map row
 * for row against the file and against `FooterStatus`'s own oneof, so an arm
 * landing in the proto without a colour — or a colour landing in the file
 * without an arm — fails the suite rather than the screen.
 *
 * THE ALLOWANCE TABLE IS NOT A TABLE EITHER, for the same reason: the vendor's
 * status arms are keyed in `render-colors.json#footer_allowance`, so the map is
 * built from `footerAllowanceColor` at load and never restated here.
 *
 * THE DATUM COLOURS ARE THIS COMPONENT'S OWN. footer.proto's rendering rules
 * say a STATICALLY TYPED datum inside an activity (a count, an instant, a sha)
 * deserves colour, but the shared vocabulary files scope only whole-state
 * colours — a roster dot, a footer status, a failure side — so there is nothing
 * upstream to read this from. The assignment follows the preamble's beyond-the-
 * vocabulary convention: a FIGURE the reader is tracking takes yellow (the
 * context figure's colour), and an IDENTITY takes blue.
 */
import {
  FooterAllowanceSchema,
  FooterStatusSchema,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { HUE_GREEN, HUE_ORANGE, HUE_RED, HUE_YELLOW, percentGradientColor, type PercentStop } from "../percent-gradient.js";
import { footerAllowanceColor, footerStatusColor, toneClass, type Color } from "../vocab.js";

/**
 * Every `FooterStatus.status` arm, in the generated spelling.
 *
 * Read off the SCHEMA rather than typed out, so the set is the proto's own and
 * an arm added to footer.proto reaches every consumer of this constant — the
 * strip's exhaustive switch, the colour map below, the suite's assertions —
 * without anybody remembering to extend a list.
 */
export const FOOTER_STATUS_CASES: readonly string[] = (
  FooterStatusSchema.oneofs.find((oneof) => oneof.name === "status")?.fields ?? []
).map((field) => field.localName);

/**
 * The colour each status arm paints the strip, resolved through the shared
 * vocabulary. Built eagerly: an arm with no colour throws here, at import.
 */
export const STATUS_ARM_CLASS: Readonly<Record<string, Color>> = Object.freeze(
  Object.fromEntries(FOOTER_STATUS_CASES.map((arm) => [arm, footerStatusColor(arm)])),
);

/**
 * The CSS class the status cell wears for ARM.
 *
 * A thin wrapper over the shared vocabulary — the strip never reads the map
 * directly, so there is exactly one call path from an arm to a class.
 */
export function statusArmClass(arm: string): string {
  return toneClass(footerStatusColor(arm));
}

/**
 * Every `FooterAllowance.status` arm, in the generated spelling.
 *
 * Read off the SCHEMA for the same reason the status cases are: the vendor's
 * status word was a free string until the vocabulary landed in evidence, and
 * now that it is an arm the client's set of arms is the proto's own.
 */
export const FOOTER_ALLOWANCE_STATUS_CASES: readonly string[] = (
  FooterAllowanceSchema.oneofs.find((oneof) => oneof.name === "status")?.fields ?? []
).map((field) => field.localName);

/**
 * The colour each allowance status paints its cell, resolved through the shared
 * vocabulary exactly as the status arms are. Built eagerly: an arm the file has
 * no colour for throws HERE, at import, rather than drawing an unpainted cell.
 */
export const ALLOWANCE_ARM_CLASS: Readonly<Record<string, Color>> = Object.freeze(
  Object.fromEntries(
    FOOTER_ALLOWANCE_STATUS_CASES.map((arm) => [arm, footerAllowanceColor(arm)]),
  ),
);

/**
 * The CSS class an allowance cell wears for ARM.
 *
 * A thin wrapper over the shared vocabulary, like `statusArmClass`: there is
 * exactly one call path from an arm to a class, and an arm `render-colors.json`
 * has no row for is a MalformedView rather than a silently grey cell.
 */
export function allowanceStatusClass(arm: string): string {
  return toneClass(footerAllowanceColor(arm));
}

/** The typed datums an activity line colours. */
export type ActivityDatum = "sha" | "agent" | "attempt" | "count" | "position";

/**
 * The colour a typed datum takes inside an activity line.
 *
 * A sha and a transient's subagent label NAME something and take the identity
 * blue; every other datum is a FIGURE the reader is watching move (a retry's
 * attempt, a queue place) and takes the figure yellow, so the line reads as
 * one kind of thing with one exception rather than as five competing colours.
 * A percentage is the exception to the table: its colour says how full it is
 * (`footerPercentColor`).
 */
const ACTIVITY_DATUM_COLOR: Readonly<Record<ActivityDatum, Color>> = Object.freeze({
  sha: "blue",
  agent: "blue",
  attempt: "yellow",
  count: "yellow",
  position: "yellow",
});

/** The CSS class a typed datum wears. */
export function activityDatumClass(datum: ActivityDatum): string {
  return toneClass(ACTIVITY_DATUM_COLOR[datum]);
}

/**
 * Where a footer percentage's colors sit (owner, 2026-09-30): green below 40,
 * yellow by 70, orange by 90, red at 90 and above. Between 40 and 90 the hue
 * runs continuously, so 55 sits halfway between green and yellow and 80
 * halfway between yellow and orange; 90 is a hard step to red.
 */
const FOOTER_PERCENT_STOPS: readonly PercentStop[] = [
  { at: 40, hue: HUE_GREEN },
  { at: 70, hue: HUE_YELLOW },
  { at: 90, hue: HUE_ORANGE },
  { at: 90, hue: HUE_RED },
];

/**
 * The colour a footer percentage takes: an allowance's use, the context
 * window's fill, a rate-limit line's utilization. PERCENT is the figure as
 * drawn, 0..100.
 */
export function footerPercentColor(percent: number): string {
  return percentGradientColor(percent, FOOTER_PERCENT_STOPS);
}
