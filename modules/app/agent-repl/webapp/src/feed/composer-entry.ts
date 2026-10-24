/**
 * composer-entry — THE NODE THE COMPOSER IS ANSWERING, highlighted while the
 * person types its entry in Emacs.
 *
 * The daemon pushes, on the root feed's watch, the node of a response's tree
 * that the composer's entry at point answers (frontend.v1.FeedComposerEntry),
 * and says so again whenever it moves. The highlight is that node's own lines
 * (its head line and its wrap lines, never its children's), drawn by ONE
 * stylesheet rule this module rewrites on every push. A rule rather than a
 * class on the lines, because a response's prose is repainted whenever it
 * re-wraps or re-arrives: a rule addresses whatever lines stand, so no paint
 * can drop the highlight and nothing has to re-mark it.
 */
import type { FeedComposerEntry } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { log } from "../log.js";
import { TREE_LINE_CLASS, TREE_NODE_ATTRIBUTE } from "../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The id of the one style element the highlight is drawn by. */
export const COMPOSER_ENTRY_STYLE_ID = "composer-entry-highlight";

/** VALUE inside a double-quoted CSS attribute selector. */
function quoted(value: string): string {
  return `"${value.replace(/["\\]/gu, (c) => `\\${c}`).replace(/\n/gu, "\\a ")}"`;
}

/**
 * The stylesheet ENTRY draws: a rule over the lines of its node in its row, or
 * nothing when no node is answered. A push with no state, or a node naming no
 * row, is refused.
 */
export function composerEntryRule(entry: FeedComposerEntry): string {
  const at = requireCase(entry.at, "FeedComposerEntry.at");
  switch (at.case) {
    case "none":
      return "";
    case "node": {
      const row = requireMessage(at.value.row, "FeedComposerEntryNode.row").value;
      return (
        `[data-feed-row=${quoted(row)}] .${TREE_LINE_CLASS}[${TREE_NODE_ATTRIBUTE}=${quoted(at.value.label)}]` +
        ` { background: var(--composer-entry-bg); box-shadow: inset 2px 0 0 var(--accent); }`
      );
    }
    default: {
      const other: { case: string } = at;
      return unreachableArm("FeedComposerEntry.at", other.case);
    }
  }
}

/** The highlight of one page: `apply` draws each pushed state. */
export interface ComposerEntryHighlight {
  apply(entry: FeedComposerEntry): void;
}

/** The highlight drawn in DOC, by the one style element it owns. */
export function createComposerEntryHighlight(doc: Document): ComposerEntryHighlight {
  let style = doc.getElementById(COMPOSER_ENTRY_STYLE_ID);
  if (style === null) {
    style = doc.createElement("style");
    style.id = COMPOSER_ENTRY_STYLE_ID;
    doc.head.append(style);
  }
  const sheet = style;
  return {
    apply(entry: FeedComposerEntry): void {
      sheet.textContent = composerEntryRule(entry);
      const node = entry.at.case === "node" ? entry.at.value : undefined;
      log.info(node === undefined ? "the composer answers no node; nothing is highlighted" : "highlighted the node the composer answers", {
        operation: "feed.composer-entry.applied",
        context: { row: node?.row?.value ?? "none", label: node?.label ?? "none" },
      });
    },
  };
}
