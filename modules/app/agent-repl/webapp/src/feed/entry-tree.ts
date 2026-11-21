/**
 * entry-tree — A PROMPT'S RUN OF ENTRIES, DRAWN AS THE TREE THEIR LABELS MAKE
 * (frontend.v1.ProseEntryTree), by the one tree renderer: each entry's line a
 * click target (`ENTRY_LABEL_CLASS`), each implied ancestor drawn by its label
 * (`ENTRY_PATH_CLASS`), in the entry labels' one shade.
 *
 * IT IS ITS OWN MODULE because the bubble body draws it (bubble/body.ts),
 * while what an entry does once drawn (entries.ts) waits on the body's paint:
 * the drawing sits below the body and the behavior above it, so neither
 * imports the other in a cycle (test/import-cycles.test.ts).
 */
import { create } from "@bufbuild/protobuf";
import { ProseTreeNodeSchema, ProseTreeSchema, type ProseEntryTree } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import {
  renderTreeHtml,
  type InlineMarkup,
  type TreeIssue,
  type TreeLineDecor,
  type TreeMeasure,
} from "../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The class every line of an entry wears, its label in `ENTRY_ATTRIBUTE`. */
export const ENTRY_CLASS = "mp-entry";

/** The attribute naming the label an entry's lines answer. */
export const ENTRY_ATTRIBUTE = "data-entry-label";

/** The class an entry's label wears: the click target that reveals the response node it answers. */
export const ENTRY_LABEL_CLASS = "mp-entry-label";

/** The class an implied ancestor's line wears: no entry answers it, so it is drawn by its label alone. */
export const ENTRY_PATH_CLASS = "mp-entry-path";

/**
 * TREE, a run of a prompt's entries, as HTML: the tree their labels make,
 * drawn by the one tree renderer within MEASURE (no blank line between its
 * roots), each entry's lines wearing `ENTRY_CLASS` with its label in
 * `ENTRY_ATTRIBUTE` and its label the click target, each implied ancestor's
 * line `ENTRY_PATH_CLASS`. A node with no place, label or kind is refused.
 */
export function entryTreeHtml(tree: ProseEntryTree, inline: InlineMarkup, measure: TreeMeasure, onIssue?: TreeIssue): string {
  const decor: (TreeLineDecor | undefined)[] = [];
  const nodes = tree.nodes.map((node, i) => {
    const path = `ProseEntryTree.nodes[${i}]`;
    const label = requireMessage(node.label, `${path}.label`).text;
    const kind = requireCase(node.kind, `${path}.kind`);
    let text = "";
    switch (kind.case) {
      case "answer":
        text = requireMessage(kind.value.text, `${path}.answer.text`).markdown;
        decor.push({ lineClass: ENTRY_CLASS, attributes: { [ENTRY_ATTRIBUTE]: label }, labelClass: ENTRY_LABEL_CLASS });
        break;
      case "path":
        decor.push({ lineClass: ENTRY_PATH_CLASS });
        break;
      default: {
        const other: { case: string } = kind;
        return unreachableArm(`${path}.kind`, other.case);
      }
    }
    const place = requireCase(node.place, `${path}.place`);
    return create(ProseTreeNodeSchema, {
      id: requireMessage(node.id, `${path}.id`),
      label: { text: label },
      text: { markdown: text },
      place:
        place.case === "root"
          ? { case: "root", value: {} }
          : { case: "branch", value: { parent: requireMessage(place.value.parent, `${path}.branch.parent`) } },
    });
  });
  const drawn = create(ProseTreeSchema, { nodes });
  return renderTreeHtml(drawn, inline, measure, onIssue, { decorate: (i) => decor[i], rootGap: false });
}
