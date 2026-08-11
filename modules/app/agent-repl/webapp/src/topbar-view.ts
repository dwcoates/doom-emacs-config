/**
 * topbar-view — the header's identity strip, drawn from the RESOLVED
 * `TopbarView` the daemon publishes.
 *
 * EVERYTHING HERE IS VERBATIM. `title` arrives already composed (a workspace
 * name, or a name and a branch when the daemon resolved a branch worth
 * showing), so this module never concatenates identity fragments: the
 * composition rule is the daemon's, and a second copy of it in a renderer
 * drifts from the first the moment either changes. The same goes for
 * `sessionLine`, `modelDisplay`, each warning's `text`, and the connectivity
 * glyph and tooltip.
 *
 * ABSENCE RENDERS ABSENCE. A workspace whose topbar has not been published yet
 * renders NOTHING — not a placeholder title, not a spinner, not a name pieced
 * together from whatever else the store happens to hold. A client-composed
 * stand-in is indistinguishable, on screen, from a real resolution, which is
 * precisely the confusion these resolved views exist to end.
 *
 * TONE IS NOT MAPPED HERE. `TopbarConnectivity.tone` already names a color
 * class from the shared render-colors vocabulary; this module spells it into a
 * class name and stops. The connectivity-enum-to-color table a frontend used to
 * carry is gone, and reintroducing one here would be a second authority on the
 * same fact.
 */

import { escapeHtml } from "./highlight.js";
import { dropdownChipHtml } from "./counter-menu.js";
import { tokenBreakdownViewHtml } from "./token-breakdown-view.js";
import type {
  TokenBreakdownView,
  TopbarConnectivity,
  TopbarView,
} from "./frontend-proto.js";

/** The attribute the model selector's change handler reads its choice from. */
export const MODEL_OPTION_ATTR = "data-model-value";

/**
 * The connectivity glyph, or "" when the daemon published none.
 *
 * An unpublished glyph is not "unknown connectivity" to be drawn in grey — it
 * is the daemon not having spoken, and the strip simply has no indicator until
 * it does.
 */
export function topbarConnectivityHtml(connectivity: TopbarConnectivity | undefined): string {
  if (connectivity === undefined) return "";
  return (
    `<span class="topbar-connectivity tone-${escapeHtml(connectivity.tone)}" ` +
    `title="${escapeHtml(connectivity.title)}">${escapeHtml(connectivity.glyph)}</span>`
  );
}

/**
 * The model selector's options, in the daemon's display order.
 *
 * The list is rendered exactly as given: no sort, no de-duplication, no
 * "current model first". Order is a resolved fact here like every other.
 */
export function topbarModelOptionsHtml(view: TopbarView): string {
  return view.modelOptions
    .map(
      (option) =>
        `<option ${MODEL_OPTION_ATTR}="${escapeHtml(option.value)}" ` +
        `value="${escapeHtml(option.value)}" title="${escapeHtml(option.description)}"` +
        `${option.value === view.modelDisplay ? " selected" : ""}>` +
        `${escapeHtml(option.displayName)}</option>`,
    )
    .join("");
}

/**
 * The whole strip, or "" when no topbar has been published for the workspace.
 *
 * Each slot is omitted when its string is empty, which is the daemon saying
 * there is nothing to show there: an empty `modelDisplay` means the selector
 * renders its placeholder. Nothing is filled in from somewhere else.
 *
 * THE WARNINGS ARE NOT IN THE STRIP. They used to be — the retired
 * `accountingLine` printed a settled turn's whole sentence inline, so a
 * degraded turn put a paragraph of prose across the header. They render as the
 * warning indicator (`topbarWarningsHtml`), which the caller places beside the
 * strip because its dropdown is disclosure state the caller owns.
 *
 * NEITHER IS THE SESSION LINE. `sessionLine` is documented on the contract as
 * the line "shown in the hover/expanded state", and this renderer used to print
 * it inline anyway — so two opaque correlation ids sat across the header,
 * meaningless to read and re-measured on every session rotation. It renders as
 * the ids disclosure (`topbarSessionDisclosureHtml`) now, which is the expanded
 * state the field always named.
 */
export function topbarViewHtml(view: TopbarView | null): string {
  if (view === null) return "";
  const parts: string[] = [
    `<span class="topbar-title">${escapeHtml(view.title)}</span>`,
  ];
  const connectivity = topbarConnectivityHtml(view.connectivity);
  if (connectivity !== "") parts.push(connectivity);
  parts.push(
    `<select class="topbar-model" data-model-select="1">` +
      (view.modelDisplay === ""
        ? `<option value="" selected disabled>select a model</option>`
        : "") +
      topbarModelOptionsHtml(view) +
      `</select>`,
  );
  return `<div class="topbar-view">${parts.join("")}</div>`;
}

/**
 * The ids disclosure: the chip that opens the daemon's resolved session line.
 *
 * WHY A DISCLOSURE AND NOT STRIP TEXT. `sessionLine` states two opaque
 * correlation ids ("session s_9cebb553b0bf3924 · conversation 81cb5e3e-…").
 * They say nothing to a reader, they are the widest and most volatile thing the
 * strip ever carried, and they are re-measured every time a session rotates —
 * so drawn inline they made the whole strip reflow for text nobody reads. They
 * are also LOAD-BEARING for debugging, so they are moved rather than dropped.
 *
 * WHY THIS AFFORDANCE. It is the one the warning indicator already established:
 * the shared `dropdownChipHtml` shell, so the chip is a real keyboard-reachable
 * `<button>` and dismisses through the same outside-click and Escape handling
 * as every other topbar dropdown. A `title` tooltip was the alternative and is
 * worse for the one job these ids have — a tooltip cannot be selected, so it
 * cannot be pasted into an issue or a log query.
 *
 * ABSENCE RENDERS ABSENCE. A workspace between sessions has an empty
 * `sessionLine`, and an empty line gets NO chip: a control over an identity
 * that does not exist only invites the click that proves it does not exist.
 *
 * THE TEXT IS THE DAEMON'S, verbatim. This renderer escapes it and prints it;
 * it never splits the line, relabels its halves, or recomposes it from parts.
 */
export function topbarSessionDisclosureHtml(view: TopbarView | null, open: boolean): string {
  if (view === null || view.sessionLine === "") return "";
  return dropdownChipHtml(
    "session",
    "ids",
    "the daemon's resolved session and conversation identifiers",
    open,
    () =>
      `<ul class="session-overlay" role="menu">` +
      `<li class="session-row" role="menuitem">${escapeHtml(view.sessionLine)}</li>` +
      `</ul>`,
  );
}

/**
 * The glyph the warning indicator draws. Literal here rather than resolved
 * daemon-side because it says nothing about WHICH warning is raised — it is
 * the affordance itself, the way the caret on every dropdown chip is.
 */
export const WARNING_GLYPH = "❗";

/**
 * The warning indicator: a red exclamation chip that opens a dropdown listing
 * every warning the daemon raised, each as the sentence the daemon composed.
 *
 * NO WARNINGS MEANS NO AFFORDANCE. An empty list renders "" — not a greyed
 * chip, not a quiet one — for the same reason the counter chips hide at zero:
 * a control over an empty list only invites the click that proves it is empty.
 *
 * THE TEXT IS THE DAEMON'S. This renderer escapes it and prints it; it never
 * re-derives a sentence from a verdict, which is exactly the duplication the
 * resolved views exist to end. The count is the only thing computed here, and
 * it is a count of the list it is drawn from.
 *
 * It rides the SHARED dropdown-chip shell (`dropdownChipHtml`), so it is
 * keyboard-reachable as a real `<button>` and dismisses through the same
 * outside-click and Escape handling as every other topbar dropdown.
 *
 * OPEN is renderer-owned disclosure state, held by the caller across the
 * per-frame chrome re-render, exactly as the tokens breakdown is.
 */
export function topbarWarningsHtml(view: TopbarView | null, open: boolean): string {
  if (view === null || view.warnings.length === 0) return "";
  const count = view.warnings.length;
  return dropdownChipHtml(
    "warnings",
    `<span class="topbar-warning-glyph" aria-hidden="true">${WARNING_GLYPH}</span>` +
      `<span class="topbar-warning-count">${count}</span>`,
    count === 1 ? "1 warning" : `${count} warnings`,
    open,
    () =>
      `<ul class="warnings-overlay" role="menu">` +
      view.warnings
        .map(
          (w) =>
            `<li class="warning-row" role="menuitem">${escapeHtml(w.text)}</li>`,
        )
        .join("") +
      `</ul>`,
  );
}

/**
 * The tokens disclosure: the chip that opens the breakdown, and the resolved
 * breakdown itself when it is open.
 *
 * ABSENCE RENDERS ABSENCE, chip included. A workspace whose breakdown has not
 * been published has no chip at all — not a chip reading "—", not one that
 * opens an empty menu. A control over a breakdown that does not exist invites
 * the click that proves it does not exist.
 *
 * The chip's label is the WORD, not a figure. The retired strip put a context
 * count on it that this renderer would have to source from somewhere else, and
 * every candidate source is a client-side recomposition of a number the
 * breakdown itself already resolves. The figures live in the menu, where the
 * daemon put them.
 *
 * OPEN is renderer-owned disclosure state, held by the caller across the
 * per-frame chrome re-render, exactly as the footer's rosters are.
 */
export function tokensDisclosureHtml(view: TokenBreakdownView | null, open: boolean): string {
  if (view === null) return "";
  return dropdownChipHtml(
    "tokens",
    "tokens",
    "the daemon's resolved token breakdown for this workspace",
    open,
    () => tokenBreakdownViewHtml(view),
  );
}

/** Which of the strip's three dropdowns the caller currently holds open. */
export interface TopbarDisclosures {
  session: boolean;
  tokens: boolean;
  warnings: boolean;
}

/**
 * The WHOLE header strip: the resolved identity view and the three chips that
 * hang off it, in one composition.
 *
 * It lives here rather than as three concatenated calls at the paint site so
 * the strip's contents and their order are one testable fact rather than a
 * shape nobody can assert on.
 */
export function topbarStripHtml(
  view: TopbarView | null,
  breakdown: TokenBreakdownView | null,
  open: TopbarDisclosures,
): string {
  return (
    topbarViewHtml(view) +
    topbarSessionDisclosureHtml(view, open.session) +
    tokensDisclosureHtml(breakdown, open.tokens) +
    topbarWarningsHtml(view, open.warnings)
  );
}
