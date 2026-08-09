/**
 * failure-card — the feed's system-failure card, rendered from the RESOLVED
 * `FailureCardView` the daemon publishes.
 *
 * WHAT REPLACED WHAT. This is the reader for `FailureCardView` +`FailureKind`,
 * and it retires the `SystemFailureItem` shape that preceded it: an
 * `error_class` enum beside a free-text `error_type`, from which this end
 * derived a color, a retryability and a "fatal" reading by rules the daemon did
 * not share. A free-text type is a vocabulary with no members — a consumer that
 * meets an unfamiliar one silently renders it as something else. The arms
 * cannot do that: an unfamiliar arm fails to match, loudly.
 *
 * THE THREE STEPS, and there are only three, because everything else was
 * resolved before it reached this end:
 *
 *   kind (which arm) → SIDE (machinery or vendor) → TONE (a color from the
 *   shared render-colors vocabulary).
 *
 * The side is stated ONCE, as data, in `proto-names.ts`'s `FAILURE_KIND_SIDE`,
 * keyed by the generated arm union so a new arm on the wire fails the build
 * until somebody says which side it is on. The tone table below is the
 * `error_classes` section of `proto/vocab/render-colors.json` — machinery is
 * the color that file assigns `ERROR_CLASS_INTERNAL`, vendor the one it assigns
 * `ERROR_CLASS_API` — and `test/failure-card.test.ts` asserts both rows against
 * the fixture itself, which is what keeps this renderer from drifting away from
 * the workspace color the same failure paints.
 *
 * A MALFORMED KIND IS NOT RENDERED. `decodeFailureKind` throws on an unset or
 * double-set kind, and this module throws on an arm it has no side for. Neither
 * degrades into a generic error card: a card that says "something failed" in a
 * colour nobody chose is worse than a loud refusal, because it looks like an
 * answer.
 */

import { escapeHtml } from "./highlight.js";
import type { FailureCardView, FailureKind } from "./frontend-proto.js";
import { FAILURE_KIND_SIDE, type FailureSide } from "./proto-names.js";

/** The color a side resolves to, from the shared render-colors vocabulary. */
export type FailureTone = "blue" | "purple";

/**
 * SIDE → TONE, the `error_classes` rows of `proto/vocab/render-colors.json`.
 *
 * Machinery resolves the workspace BLUE (agent-repl's own plumbing is
 * compromised) and vendor resolves it PURPLE (the vendor or the account stopped
 * the work). Card color IS state color, from that one table, so a user can
 * never see a purple workspace explained by a card of another color.
 */
export const FAILURE_SIDE_TONE: Readonly<Record<FailureSide, FailureTone>> = {
  machinery: "blue",
  vendor: "purple",
};

/**
 * The CSS class each tone is drawn with. The two class names predate the arm
 * vocabulary and are kept: they are what `styles.css` and the cross-language
 * color test already name, and renaming them would move the assertion without
 * moving the contract.
 */
export const FAILURE_TONE_CLASS: Readonly<Record<FailureTone, string>> = {
  blue: "failure-internal",
  purple: "failure-api",
};

/**
 * WHICH SIDE a failure is on.
 *
 * Throws on an arm with no side rather than picking one. The lookup cannot fail
 * for a kind that came through `decodeFailureKind` — it checks the same table —
 * so reaching this throw means a `FailureKind` was built somewhere that skipped
 * the decoder, which is itself the bug worth failing on.
 */
export function failureSide(kind: FailureKind): FailureSide {
  const arm = kind.kind.case;
  if (arm === undefined) {
    throw new Error("failure-card: FailureKind sets no arm; an unset kind is malformed");
  }
  const side = (FAILURE_KIND_SIDE as Record<string, FailureSide | undefined>)[arm];
  if (side === undefined) {
    throw new Error(`failure-card: FailureKind arm '${arm}' has no side in the vocabulary`);
  }
  return side;
}

/** The color class a failure's card, row and workspace all take. */
export function failureTone(kind: FailureKind): FailureTone {
  return FAILURE_SIDE_TONE[failureSide(kind)];
}

/** The stylesheet class for a failure's tone. */
export function failureToneClass(kind: FailureKind): string {
  return FAILURE_TONE_CLASS[failureTone(kind)];
}

/** The arm name, for logs and for the card's own data attribute. */
export function failureKindName(kind: FailureKind): string {
  const arm = kind.kind.case;
  if (arm === undefined) {
    throw new Error("failure-card: FailureKind sets no arm; an unset kind is malformed");
  }
  return arm;
}

/**
 * The failure kinds whose window CLOSING means the card DISAPPEARS, rather than
 * settling in place with a resolved stamp.
 *
 * Both report a transport link that was momentarily down and is now up again.
 * Once the link is back the card names a condition that no longer exists, beside
 * a feed that is visibly live: a settled "lost the connection; reconnecting"
 * reads as a standing fault to anyone who does not notice the small timestamp
 * under it, and a flapping link leaves one such ghost per drop.
 *
 * Deliberately NOT every window-shaped failure. A resolved
 * `shimStoreWriteRejected` carries a count of conversation that is permanently
 * gone, and a resolved rate limit explains a gap in the transcript. Those
 * settle; only the pure connectivity windows vanish.
 */
export const CONNECTIVITY_WINDOW_KINDS: readonly string[] = [
  /** This end's own socket to the daemon closed and then came back. */
  "daemonUnreachable",
  /** The daemon's missed-traffic window to the shim, since resumed. */
  "shimDegraded",
];

/** Whether a card's lifecycle has settled with a resolution stamp. */
export function failureResolvedAtMs(view: FailureCardView): number {
  return view.lifecycle.case === "resolved" ? view.lifecycle.resolvedAtMs : 0;
}

/**
 * The card's body.
 *
 * `message` is the daemon's sentence and `detail` its evidence; both are drawn
 * verbatim and escaped, and neither is composed here. An empty detail renders
 * NOTHING rather than an empty quieter-register block, because the proto states
 * outright that it is allowed to be empty.
 */
export function failureCardHtml(view: FailureCardView, uuid: string): string {
  const tone = failureToneClass(view.kind);
  const arm = failureKindName(view.kind);
  const lifecycle = view.lifecycle.case;
  const detail =
    view.detail === ""
      ? ""
      : `<div class="failure-detail">${escapeHtml(view.detail)}</div>`;
  // The RESOLVED arm stamps when the window closed. `open` invites waiting and
  // `terminal` does not, so they are distinct classes rather than one "not
  // resolved" — a terminal card that looked like an open one would leave a
  // reader waiting for an all-clear that is never coming.
  const stamp =
    view.lifecycle.case === "resolved"
      ? `<div class="failure-resolved">resolved at ${escapeHtml(
          new Date(view.lifecycle.resolvedAtMs).toISOString(),
        )}</div>`
      : "";
  return (
    `<div class="failure-card ${tone} failure-${escapeHtml(lifecycle)}" ` +
    `data-failure-kind="${escapeHtml(arm)}" data-failure-uuid="${escapeHtml(uuid)}">` +
    `<div class="failure-message">${escapeHtml(view.message)}</div>` +
    detail +
    stamp +
    `</div>`
  );
}
