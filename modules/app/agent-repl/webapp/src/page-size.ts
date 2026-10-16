/**
 * page-size — how big one page's document is, as every record that ties a
 * cost to a page's size states it: its element count and its feed rows.
 */
import { queryAll } from "./dom.js";

/** One document's size. */
export interface PageSize {
  readonly dom_nodes: number;
  readonly feed_rows: number;
}

/** The size of DOC. */
export function pageSize(doc: Document): PageSize {
  return {
    dom_nodes: doc.getElementsByTagName("*").length,
    feed_rows: queryAll(doc, "[data-feed-row]").length,
  };
}
