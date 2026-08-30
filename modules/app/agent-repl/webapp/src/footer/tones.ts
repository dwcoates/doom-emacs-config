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
import { MalformedView } from "../rpc/malformed.js";
import { footerStatusColor, toneClass, type Color } from "../vocab.js";

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
 * The colour each allowance status paints its cell.
 *
 * THIS COMPONENT'S OWN, like the datum colours above and for the same reason:
 * `render-colors.json` carries no `footer_allowance` section, so there is
 * nothing upstream to read this from. The assignment is the traffic-light
 * reading the arms already are — headroom is green, the vendor's own warning is
 * the warning yellow, and a rejected call is the error red. Should the shared
 * file grow a `footer_allowance` section, this table is the one place that
 * changes to read it through `src/vocab.ts`.
 */
const ALLOWANCE_STATUS_COLOR: Readonly<Record<string, Color>> = Object.freeze({
  allowed: "green",
  allowedWarning: "yellow",
  rejected: "red",
});

/**
 * The CSS class an allowance cell wears for ARM.
 *
 * An arm with no colour is a MALFORMED VIEW rather than an unpainted cell: the
 * table above and the schema are meant to agree, and the suite holds them to it
 * row for row, so reaching here with an unknown arm means they have drifted.
 */
export function allowanceStatusClass(arm: string): string {
  const color = ALLOWANCE_STATUS_COLOR[arm];
  if (color === undefined) {
    throw new MalformedView("FooterAllowance.status", `no colour for the allowance arm ${arm}`);
  }
  return toneClass(color);
}

/** The typed datums an activity line colours. */
export type ActivityDatum = "sha" | "attempt" | "count" | "position" | "percent";

/**
 * The colour a typed datum takes inside an activity line.
 *
 * A sha NAMES something and takes the identity blue; every other datum is a
 * FIGURE the reader is watching move (a retry's attempt, a queue place, an
 * allowance percentage) and takes the figure yellow, so the line reads as one
 * kind of thing with one exception rather than as five competing colours.
 */
const ACTIVITY_DATUM_COLOR: Readonly<Record<ActivityDatum, Color>> = Object.freeze({
  sha: "blue",
  attempt: "yellow",
  count: "yellow",
  position: "yellow",
  percent: "yellow",
});

/** The CSS class a typed datum wears. */
export function activityDatumClass(datum: ActivityDatum): string {
  return toneClass(ACTIVITY_DATUM_COLOR[datum]);
}
