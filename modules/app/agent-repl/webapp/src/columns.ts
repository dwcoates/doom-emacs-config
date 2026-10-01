/**
 * columns — the shared pieces of a COLUMN TABLE: one grid whose header and
 * rows take its columns through `subgrid` (styles.css, "shared column
 * tables"), so every row's cell in a column shares one width.
 *
 * Two surfaces draw one: the expanded footer's agents panel (main · tokens ·
 * duration · caret) and the merge bubble's queue tab (workspace · stage ·
 * duration). A row of either wears `COLUMNS_ROW_CLASS`, a header cell is built
 * here, and the duration column's floor, bar and tabular figures are the one
 * stylesheet rule both share.
 */

/** The class a header or a row of a column table wears to take its columns. */
export const COLUMNS_ROW_CLASS = "footer-columns";

/** A column's header, above its column, naming it with `data-column`. */
export function columnHeader(name: string): HTMLElement {
  const el = document.createElement("span");
  el.className = "footer-column-header";
  el.setAttribute("data-column", name);
  el.textContent = name;
  return el;
}
