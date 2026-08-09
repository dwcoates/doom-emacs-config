/**
 * topbar-view — the header's identity strip, drawn from the RESOLVED
 * `TopbarView` the daemon publishes.
 *
 * EVERYTHING HERE IS VERBATIM. `title` arrives already composed (a workspace
 * name, or a name and a branch when the daemon resolved a branch worth
 * showing), so this module never concatenates identity fragments: the
 * composition rule is the daemon's, and a second copy of it in a renderer
 * drifts from the first the moment either changes. The same goes for
 * `sessionLine`, `modelDisplay`, `accountingLine`, and the connectivity
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
import type { TopbarConnectivity, TopbarView } from "./frontend-proto.js";

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
 * renders its placeholder, and an empty `accountingLine` means no turn has
 * settled yet. Neither is filled in from somewhere else.
 */
export function topbarViewHtml(view: TopbarView | null): string {
  if (view === null) return "";
  const parts: string[] = [
    `<span class="topbar-title">${escapeHtml(view.title)}</span>`,
  ];
  const connectivity = topbarConnectivityHtml(view.connectivity);
  if (connectivity !== "") parts.push(connectivity);
  if (view.sessionLine !== "") {
    parts.push(`<span class="topbar-session-line">${escapeHtml(view.sessionLine)}</span>`);
  }
  parts.push(
    `<select class="topbar-model" data-model-select="1">` +
      (view.modelDisplay === ""
        ? `<option value="" selected disabled>select a model</option>`
        : "") +
      topbarModelOptionsHtml(view) +
      `</select>`,
  );
  if (view.accountingLine !== "") {
    parts.push(`<span class="topbar-accounting">${escapeHtml(view.accountingLine)}</span>`);
  }
  return `<div class="topbar-view">${parts.join("")}</div>`;
}
