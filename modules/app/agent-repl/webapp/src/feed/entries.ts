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
 *     bubble-selection.ts), each implied ancestor drawn by its label
 *     (`ENTRY_PATH_CLASS`), in the entry labels' one shade;
 *   - a response's answered labels, marked as links (`LABEL_LINK_CLASS`) on
 *     every paint of its prose (`linkAnswers`, the slot's mark): a click
 *     selects the answering prompt, centered (SelectFeedRow's `answer` move);
 *   - the flash of the entry such a click reveals (`flashEntry`), once the
 *     daemon's selection push names it (FeedSelectionRowViewportReveal);
 *   - the response node a prompt's label answers (`answeredNodeAt`), which
 *     the pointer boxes while it is on that label (branch-hover.ts), found by
 *     every prompt answering each response label (`ANSWERED_BY_ATTRIBUTE`).
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
  TREE_NODE_ATTRIBUTE,
  renderTreeHtml,
  type InlineMarkup,
  type TreeIssue,
  type TreeLineDecor,
  type TreeMeasure,
} from "../metaprompt-tree.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { MARKDOWN_SLOT_ATTRIBUTE, whenSlotPainted } from "../bubble/body.js";
import { feedRowId, feedRowOf } from "./row-id.js";
import { inSubtree } from "./subtree-box.js";

/** The class every line of an entry wears, its label in `ENTRY_ATTRIBUTE`. */
export const ENTRY_CLASS = "mp-entry";

/** The attribute naming the label an entry's lines answer. */
export const ENTRY_ATTRIBUTE = "data-entry-label";

/** The class an entry's label wears: the click target that reveals the response node it answers. */
export const ENTRY_LABEL_CLASS = "mp-entry-label";

/** The class an entry wears while it flashes, revealed by a linked label. */
export const ENTRY_FLASH_CLASS = "mp-entry-flash";

/** The class a tree label a later prompt answers wears: a link to that prompt. */
export const LABEL_LINK_CLASS = "mp-label-linked";

/** The attribute naming the prompt row a linked label's answer is in. */
export const ANSWER_PROMPT_ATTRIBUTE = "data-answer-prompt";

/**
 * The attribute naming every prompt row answering a response's tree label,
 * space-separated: a label several prompts answer links to the latest
 * (`ANSWER_PROMPT_ATTRIBUTE`), but each of them finds its node by this.
 */
export const ANSWERED_BY_ATTRIBUTE = "data-answered-by";

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

/**
 * Mark every tree label in RENDERED that one of ANSWERS answers as a link to
 * the prompt answering it. A label several prompts answer links to the latest.
 * Run on every paint of a response's prose (a slot mark), so a repaint keeps
 * the links.
 */
export function linkAnswers(rendered: HTMLElement, answers: readonly FeedResponseAnswer[]): void {
  const answering = new Map<string, string[]>();
  for (const answer of answers) {
    const prompt = requireMessage(answer.prompt, "FeedResponseAnswer.prompt").value;
    if (prompt === "" || /\s/.test(prompt)) throw new Error(`an answering prompt's row id ${JSON.stringify(prompt)} is not one token`);
    answering.set(answer.label, [...(answering.get(answer.label) ?? []), prompt]);
  }
  let linked = 0;
  for (const label of queryAll<HTMLElement>(rendered, `.${TREE_LABEL_CLASS}`)) {
    const prompts = answering.get(label.getAttribute(TREE_LABEL_ATTRIBUTE) ?? "");
    label.classList.toggle(LABEL_LINK_CLASS, prompts !== undefined);
    if (prompts === undefined) {
      label.removeAttribute(ANSWER_PROMPT_ATTRIBUTE);
      label.removeAttribute(ANSWERED_BY_ATTRIBUTE);
      label.removeAttribute("title");
      continue;
    }
    label.setAttribute(ANSWER_PROMPT_ATTRIBUTE, prompts[prompts.length - 1]);
    label.setAttribute(ANSWERED_BY_ATTRIBUTE, prompts.join(" "));
    label.title = "show the prompt that answered this";
    linked++;
  }
  log.debug("a response's answered labels were linked", {
    operation: "feed.entries.labels-linked",
    context: { answers: answers.length, linked },
  });
}

/**
 * The response node the label TARGET is on, in the prompt row PROMPT drawn in
 * HOST, answers, or null: TARGET is not on a label of PROMPT's entries, or no
 * response drawn in HOST is answered by them. An entry's label answers its
 * own node; an implied ancestor's answers its own node in the response its
 * entries beneath it answer.
 */
export function answeredNodeAt(host: ParentNode, prompt: HTMLElement, target: Element): { row: HTMLElement; label: string } | null {
  const line = target.closest(`.${TREE_LABEL_CLASS}`)?.closest<HTMLElement>(`.${ENTRY_CLASS}, .${ENTRY_PATH_CLASS}`);
  if (line === undefined || line === null || !prompt.contains(line)) return null;
  const label = line.getAttribute(TREE_NODE_ATTRIBUTE);
  if (label === null) throw new Error("a prompt's entry line names no node");
  const id = feedRowId(prompt);
  if (id === null) throw new Error("a prompt drawing entries carries no row id");
  const entries = new Set(
    queryAll<HTMLElement>(prompt, `.${ENTRY_CLASS}`)
      .map((entry) => entry.getAttribute(ENTRY_ATTRIBUTE))
      .filter((entry): entry is string => inSubtree(entry, label)),
  );
  const answered = queryAll<HTMLElement>(host, `.${TREE_LABEL_CLASS}[${ANSWERED_BY_ATTRIBUTE}]`).find(
    (el) =>
      entries.has(el.getAttribute(TREE_LABEL_ATTRIBUTE) ?? "") &&
      (el.getAttribute(ANSWERED_BY_ATTRIBUTE) ?? "").split(" ").includes(id),
  );
  const row = answered === undefined ? null : feedRowOf<HTMLElement>(answered);
  return row === null ? null : { row, label };
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
      context: { row: feedRowId(row) ?? "unset", label },
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
    context: { row: feedRowId(row) ?? "unset", label, lines: lines.length },
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
