/**
 * token-breakdown-view — the token-breakdown menu, drawn from the RESOLVED
 * `TokenBreakdownView` the daemon publishes.
 *
 * THIS MODULE PERFORMS NO ARITHMETIC ON A ROW. Not a sum, not a total, not a
 * percentage, not a re-rounding. Every figure arrives resolved: the count is
 * the count, and the share is already permille and already rounded, computed by
 * the one place that owns the canonical `TokenUsage` derivation. A renderer
 * that added its own total would be publishing a second answer to a question
 * that already has one — and the two disagree the first time the daemon changes
 * what counts as expensive.
 *
 * THE ONLY THING DECIDED HERE is presentation of the two layout facts the
 * daemon resolved for us: `emphasized` (headline versus detail row) and `depth`
 * (indent for nested rows).
 *
 * A SHARE OF -1 IS NOT A ZERO. It means no share applies to this row, and the
 * percentage is omitted entirely; 0 is a real zero percent and is printed. The
 * distinction is the reason the field is signed.
 *
 * ABSENCE RENDERS ABSENCE: a workspace whose breakdown has not been published
 * renders nothing at all, never a zeroed table.
 */

import { escapeHtml } from "./highlight.js";
import type {
  TokenBreakdownRow,
  TokenBreakdownSection,
  TokenBreakdownView,
} from "./frontend-proto.js";

/** The sentinel share meaning "no share applies to this row". */
export const NO_SHARE_PERMILLE = -1;

/**
 * A row's share as the menu prints it, or "" when no share applies.
 *
 * The permille is DIVIDED for display only — the value is already rounded, so
 * this cannot change which number is shown, only where its decimal point goes.
 */
export function shareLabel(sharePermille: number): string {
  if (sharePermille === NO_SHARE_PERMILLE) return "";
  return `${(sharePermille / 10).toFixed(1)}%`;
}

/** One row: its label, its count, and its share when one applies. */
export function tokenBreakdownRowHtml(row: TokenBreakdownRow): string {
  const share = shareLabel(row.sharePermille);
  return (
    `<div class="tb-row${row.emphasized ? " tb-emphasized" : ""}" ` +
    `data-tb-depth="${String(row.depth)}">` +
    `<span class="tb-label">${escapeHtml(row.label)}</span>` +
    `<span class="tb-tokens">${escapeHtml(String(row.tokens))}</span>` +
    (share === "" ? "" : `<span class="tb-share">${escapeHtml(share)}</span>`) +
    `</div>`
  );
}

/** One titled section, rows in the daemon's display order. */
export function tokenBreakdownSectionHtml(section: TokenBreakdownSection): string {
  return (
    `<div class="tb-section">` +
    `<div class="tb-section-label">${escapeHtml(section.label)}</div>` +
    section.rows.map(tokenBreakdownRowHtml).join("") +
    `</div>`
  );
}

/**
 * The whole menu, or "" when no breakdown has been published for the workspace.
 *
 * A published view with NO sections is still a published view, and it renders
 * an empty menu rather than nothing: the daemon saying "there is nothing to
 * break down yet" is a different fact from the daemon not having said anything.
 */
export function tokenBreakdownViewHtml(view: TokenBreakdownView | null): string {
  if (view === null) return "";
  return `<div class="tb-menu">${view.sections.map(tokenBreakdownSectionHtml).join("")}</div>`;
}
