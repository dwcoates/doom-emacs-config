/**
 * entries — A PROMPT'S ENTRIES, AND THE LABELS THEY ANSWER.
 *
 * A prompt answers a response's tree by label (`3.1.2 - my answer`). The
 * daemon reads the entries (frontend.v1.ProseEntry) and binds each to the
 * response it answers (frontend.v1.FeedResponse.answers); this module draws
 * both ends and nothing more:
 *
 *   - a run of entries as the tree their labels make (frontend.v1.ProseEntryTree,
 *     `entryTreeHtml`), drawn by the one tree renderer: each entry's line a
 *     click target (`ENTRY_LABEL_CLASS`) that selects the prompt and has
 *     Emacs fill the composer with the entry (SelectFeedRow's `entry` move,
 *     bubble-selection.ts), each implied ancestor dimmed (`ENTRY_PATH_CLASS`);
 *   - a response's answered labels, marked as links (`LABEL_LINK_CLASS`) on
 *     every paint of its prose (`linkAnswers`, the slot's mark): a click
 *     selects the answering prompt, centered (SelectFeedRow's `answer` move);
 *   - the flash of the entry such a click reveals (`flashEntry`), once the
 *     daemon's selection push names it (FeedSelectionRowViewportReveal).
 */
import type { FeedResponseAnswer } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { create } from "@bufbuild/protobuf";
import {
  ProseTreeNodeSchema,
  ProseTreeSchema,
  type ProseEntryTree,
} from "../../../proto/gen/ts/frontend/v1/prose_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import {
  TREE_LABEL_ATTRIBUTE,
  TREE_LABEL_CLASS,
  renderTreeHtml,
  type InlineMarkup,
  type TreeIssue,
  type TreeLineDecor,
} from "../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { MARKDOWN_SLOT_ATTRIBUTE, whenSlotPainted } from "../bubble/body.js";

/** The class every line of an entry wears, its label in `ENTRY_ATTRIBUTE`. */
export const ENTRY_CLASS = "mp-entry";

/** The attribute naming the label an entry's lines answer. */
export const ENTRY_ATTRIBUTE = "data-entry-label";

/** The class an entry's label wears: the click target that edits the entry. */
export const ENTRY_LABEL_CLASS = "mp-entry-label";

/** The class an entry wears while it flashes, revealed by a linked label. */
export const ENTRY_FLASH_CLASS = "mp-entry-flash";

/** The class a tree label a later prompt answers wears: a link to that prompt. */
export const LABEL_LINK_CLASS = "mp-label-linked";

/** The attribute naming the prompt row a linked label's answer is in. */
export const ANSWER_PROMPT_ATTRIBUTE = "data-answer-prompt";

/** The class an implied ancestor's line wears: no entry answers it, so it is dimmed. */
export const ENTRY_PATH_CLASS = "mp-entry-path";

/**
 * TREE, a run of a prompt's entries, as HTML: the tree their labels make,
 * drawn by the one tree renderer at WIDTH columns (no blank line between its
 * roots), each entry's lines wearing `ENTRY_CLASS` with its label in
 * `ENTRY_ATTRIBUTE` and its label the click target, each implied ancestor's
 * line `ENTRY_PATH_CLASS`. A node with no place, label or kind is refused.
 */
export function entryTreeHtml(tree: ProseEntryTree, inline: InlineMarkup, width: number, onIssue?: TreeIssue): string {
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
  return renderTreeHtml(drawn, inline, width, onIssue, { decorate: (i) => decor[i], rootGap: false });
}

/**
 * Mark every tree label in RENDERED that one of ANSWERS answers as a link to
 * the prompt answering it. A label several prompts answer links to the latest.
 * Run on every paint of a response's prose (a slot mark), so a repaint keeps
 * the links.
 */
export function linkAnswers(rendered: HTMLElement, answers: readonly FeedResponseAnswer[]): void {
  const latest = new Map<string, string>();
  for (const answer of answers) {
    latest.set(answer.label, requireMessage(answer.prompt, "FeedResponseAnswer.prompt").value);
  }
  let linked = 0;
  for (const label of queryAll<HTMLElement>(rendered, `.${TREE_LABEL_CLASS}`)) {
    const prompt = latest.get(label.getAttribute(TREE_LABEL_ATTRIBUTE) ?? "");
    label.classList.toggle(LABEL_LINK_CLASS, prompt !== undefined);
    if (prompt === undefined) {
      label.removeAttribute(ANSWER_PROMPT_ATTRIBUTE);
      label.removeAttribute("title");
      continue;
    }
    label.setAttribute(ANSWER_PROMPT_ATTRIBUTE, prompt);
    label.title = "show the prompt that answered this";
    linked++;
  }
  log.debug("a response's answered labels were linked", {
    operation: "feed.entries.labels-linked",
    context: { answers: answers.length, linked },
  });
}

/**
 * Flash ROW's entry answering LABEL, which a linked label revealed: every line
 * the entry is drawn on, as the row is drawn now. A row that draws no such
 * entry is recorded, because the daemon named an entry this drawing lacks.
 */
export function flashEntry(row: HTMLElement, label: string): void {
  const lines = queryAll<HTMLElement>(row, `.${ENTRY_CLASS}`).filter((el) => el.getAttribute(ENTRY_ATTRIBUTE) === label);
  if (lines.length === 0) {
    log.warn("a revealed prompt draws no entry answering the label", {
      operation: "feed.entries.flash-absent",
      context: { row: row.getAttribute("data-feed-row") ?? "unset", label },
    });
    return;
  }
  for (const line of lines) {
    line.classList.remove(ENTRY_FLASH_CLASS);
    // A FORCED LAYOUT restarts the animation when the same entry flashes twice.
    void line.offsetWidth;
    line.classList.add(ENTRY_FLASH_CLASS);
    line.addEventListener("animationend", () => line.classList.remove(ENTRY_FLASH_CLASS), { once: true });
  }
  log.info("a revealed prompt's entry flashed", {
    operation: "feed.entries.flashed",
    context: { row: row.getAttribute("data-feed-row") ?? "unset", label, lines: lines.length },
  });
}

/**
 * Flash ROW's entry answering LABEL once every markdown slot of ROW has
 * painted: a prompt just expanded paints its prose after it is laid out, and
 * the entry is not drawn before then.
 */
export function flashEntryOncePainted(row: HTMLElement, label: string): void {
  const slots = queryAll<HTMLElement>(row, `[${MARKDOWN_SLOT_ATTRIBUTE}]`);
  let waiting = slots.length;
  if (waiting === 0) {
    flashEntry(row, label);
    return;
  }
  for (const slot of slots) {
    whenSlotPainted(slot, () => {
      waiting--;
      if (waiting === 0) flashEntry(row, label);
    });
  }
}
