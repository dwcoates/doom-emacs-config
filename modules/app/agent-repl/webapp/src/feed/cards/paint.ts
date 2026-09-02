/**
 * paint — a span's `paint_class` as a CSS class, for every card that draws
 * daemon-painted output.
 *
 * THE DAEMON PAINTS, THE CLIENT STYLES. `FeedCodeSpan.paint_class` (and the
 * merge bubble's test spans) carry a name out of the closed inventory in
 * `proto/vocab/paint-classes.json`: the daemon's highlighter and its ANSI
 * parser both assert against that file, so the client never parses escapes,
 * never guesses a language, and never owns a token table. It maps a name to
 * `paint-<name>` and lets the stylesheet do the rest.
 *
 * THE JUDGMENT ITSELF IS `src/vocab.ts`'s, not a second copy of it. That
 * module is the typed accessor over the vocabulary files, it already holds the
 * inventory and the two rules that go with it — the empty string is the one
 * spelling of plain text, and an unknown name is UNSTYLED TEXT rather than an
 * error, logged at warn so the drift stays visible — and a second table here
 * would be a second thing to keep in step. This module is the CARD-side shape
 * of that answer (a class attribute value, so `""` rather than `null`) and the
 * owner of the stylesheet section the names key into.
 *
 * The inventory is asserted row for row against `paint-classes.json` in this
 * module's suite, so a name added to the file without a rule in the stylesheet
 * — or a rule for a name the file does not carry — is visible at test time.
 */
import { paintClass } from "../../vocab.js";

/**
 * The CSS class for a span's `paint_class`, or `""` for a span that carries no
 * styling at all (plain text, or a name this build has never heard of).
 *
 * `""` is returned rather than `null` because every call site is setting a
 * `class` attribute, and an empty class list is exactly "no styling" there.
 */
export function paintSpanClass(name: string): string {
  return paintClass(name) ?? "";
}
