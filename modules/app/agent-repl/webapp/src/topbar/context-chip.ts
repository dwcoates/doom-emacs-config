/**
 * The context chip and the token-breakdown menu behind it.
 *
 * THE CHIP IS A NUMBER, IN YELLOW. "How full is my context" is the question the
 * topbar's token presence exists to answer, so the figure stays visible rather
 * than hiding behind an icon — and the figure is the DAEMON'S text ("142.3k"),
 * never a formatting of a count this end did itself.
 *
 * THE BREAKDOWN IS SESSION-SCOPED, ALWAYS POPULATED. It rides the view so the
 * reveal costs no round trip, and it never carries turn figures: those are the
 * footer's, deliberately, because the topbar is turn-nonspecific by nature.
 *
 * THE ROWS DO NO ARITHMETIC. The share is precomputed in permille and is drawn
 * only when SET — presence, not a sentinel, so a row with no meaningful share
 * shows none instead of a misleading 0%. `tokens` is a COUNT, so it is grouped
 * with thousands separators and never abbreviated: the chip is the place for
 * the rounded figure, and the breakdown is the place for the exact one.
 */
import { createControl } from "../control.js";
import type {
  TokenBreakdownHeading,
  TokenBreakdownRow,
  TokenBreakdownSection,
  TokenBreakdownView,
  TopbarContextChip,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import { msOf, requireMessage } from "../rpc/strict.js";
import { toneClass } from "../vocab.js";
import type { TopbarContext } from "./context.js";
import { asAnchor } from "./strip.js";

/** How deep a nested detail row is indented, per level. */
const DEPTH_INDENT_REM = 0.75;

/**
 * The chip: the current context size as a yellow number, opening the session's
 * usage breakdown.
 *
 * ON CLICK, not on hover. The proto calls it a hover, but a hover-only reveal
 * is unreachable by keyboard and unreadable on a touch surface, and this reveal
 * is a LIST the reader scrolls — a menu that vanishes when the pointer leaves
 * the chip cannot be read. (Surfaced as a UX question in the report.)
 */
export function drawTopbarContextChip(u: TopbarContextChip, tc: TopbarContext): HTMLElement {
  const breakdown = requireMessage(u.breakdown, "TopbarContextChip.breakdown");
  log.debug("drawing the context chip", {
    operation: "topbar.context-chip",
    context: { sections: breakdown.sections.length },
  });

  const wrap = document.createElement("div");
  // YELLOW rides the control itself as well as the figure: `.topbar-context` is
  // the hook the DOM contract names, and the context figure's color is a fact
  // about the control, not about one span inside it.
  wrap.className = `topbar-context ${toneClass("yellow")}`;

  const button = createControl();
  // YELLOW is the context figure's color across the app.
  button.className = `topbar-context-figure ${toneClass("yellow")}`;
  button.textContent = u.text;
  wrap.append(button);

  // The wrap is the control; see `drawTopbarModelSelector` for the reasoning.
  asAnchor(wrap, "context");
  const body = (): HTMLElement => drawTokenBreakdownView(breakdown);
  tc.reveals.register("context", "context", body);
  wrap.addEventListener("click", () => {
    tc.reveals.toggle("context", "context", body);
  });
  return wrap;
}

/** The menu: titled sections of rows, in the served order. */
export function drawTokenBreakdownView(u: TokenBreakdownView): HTMLElement {
  const menu = document.createElement("div");
  menu.className = "token-breakdown";
  for (const [index, section] of u.sections.entries()) {
    menu.append(drawTokenBreakdownSection(section, `TokenBreakdownView.sections[${index}]`));
  }
  return menu;
}

/** One titled section. */
export function drawTokenBreakdownSection(
  u: TokenBreakdownSection,
  path: string,
): HTMLElement {
  const element = document.createElement("div");
  element.className = "token-breakdown-section";
  element.append(
    drawTokenBreakdownHeading(requireMessage(u.heading, `${path}.heading`)),
  );
  const rows = document.createElement("div");
  // The shared delimiter class: a topbar dropdown's rows are delimited exactly
  // as the expanded footer's and the tray's are.
  rows.className = "token-breakdown-rows list-rows";
  for (const [index, row] of u.rows.entries()) {
    rows.append(drawTokenBreakdownRow(row, `${path}.rows[${index}]`));
  }
  element.append(rows);
  return element;
}

/** A section's heading, verbatim. */
export function drawTokenBreakdownHeading(u: TokenBreakdownHeading): HTMLElement {
  const element = document.createElement("div");
  element.className = "token-breakdown-heading";
  element.textContent = u.text;
  return element;
}

/** One row: label, count, and the share when the producer computed one. */
export function drawTokenBreakdownRow(u: TokenBreakdownRow, path: string): HTMLElement {
  const row = document.createElement("div");
  row.className = "token-breakdown-row";
  row.setAttribute("data-row", "");
  row.setAttribute("data-depth", String(u.depth));
  // A LAYOUT FACT RESOLVED DAEMON-SIDE: emphasized rows are headlines and sit
  // unindented, detail rows sit under them.
  // THE VALUE IS "true", per the DOM hooks contract (`[data-emphasized="true"]`);
  // an unemphasized row carries no attribute at all rather than "false".
  if (u.emphasized) row.setAttribute("data-emphasized", "true");
  if (u.depth > 0) row.style.paddingLeft = `${u.depth * DEPTH_INDENT_REM}rem`;

  const label = document.createElement("span");
  label.className = "token-breakdown-label";
  label.textContent = u.label;

  const tokens = document.createElement("span");
  tokens.className = "token-breakdown-tokens";
  tokens.textContent = formatTokenCount(msOf(u.tokens, `${path}.tokens`));

  row.append(label, tokens);

  if (u.sharePermille !== undefined) {
    const share = document.createElement("span");
    share.className = "token-breakdown-share";
    share.setAttribute("data-share", "");
    share.textContent = formatSharePermille(u.sharePermille);
    row.append(share);
  }
  return row;
}

/**
 * A token COUNT: grouped, never abbreviated.
 *
 * `Intl` rather than a hand-rolled grouping, so the separator is the reader's
 * own locale's and not a hard-coded comma.
 */
export function formatTokenCount(tokens: number): string {
  return new Intl.NumberFormat().format(tokens);
}

/** A share in permille as "NN.N%", the resolution the producer rounded to. */
export function formatSharePermille(permille: number): string {
  return `${(permille / 10).toFixed(1)}%`;
}
