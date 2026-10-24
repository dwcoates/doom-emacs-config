/**
 * entries — A PROMPT'S ENTRIES, AND THE LABELS THEY ANSWER.
 *
 * A prompt answers a response's tree by label (`3.1.2 - my answer`). The
 * daemon reads the entries (frontend.v1.ProseEntry) and binds each to the
 * response it answers (frontend.v1.FeedResponse.answers); this module draws
 * both ends and nothing more:
 *
 *   - an entry block, its label a click target (`ENTRY_LABEL_CLASS`): a click
 *     selects the prompt and has Emacs fill the composer with the entry
 *     (SelectFeedRow's `entry` move, bubble-selection.ts);
 *   - a response's answered labels, marked as links (`LABEL_LINK_CLASS`) on
 *     every paint of its prose (`linkAnswers`, the slot's mark): a click
 *     selects the answering prompt, centered (SelectFeedRow's `answer` move);
 *   - the flash of the entry such a click reveals (`flashEntry`), once the
 *     daemon's selection push names it (FeedSelectionRowViewportReveal).
 */
import type { FeedResponseAnswer } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { ProseEntry } from "../../../proto/gen/ts/frontend/v1/prose_pb";
import { escapeHtml } from "../highlight.js";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { renderMarkdown } from "../markdown.js";
import { TREE_LABEL_ATTRIBUTE, TREE_LABEL_CLASS } from "../metaprompt-tree.js";
import { requireMessage } from "../rpc/strict.js";
import { MARKDOWN_SLOT_ATTRIBUTE, whenSlotPainted } from "../bubble/body.js";

/** The class an entry block wears, its label in `ENTRY_ATTRIBUTE`. */
export const ENTRY_CLASS = "mp-entry";

/** The attribute naming the label an entry block answers. */
export const ENTRY_ATTRIBUTE = "data-entry-label";

/** The class an entry's label wears: the click target that edits the entry. */
export const ENTRY_LABEL_CLASS = "mp-entry-label";

/** The class an entry wears while it flashes, revealed by a linked label. */
export const ENTRY_FLASH_CLASS = "mp-entry-flash";

/** The class a tree label a later prompt answers wears: a link to that prompt. */
export const LABEL_LINK_CLASS = "mp-label-linked";

/** The attribute naming the prompt row a linked label's answer is in. */
export const ANSWER_PROMPT_ATTRIBUTE = "data-answer-prompt";

/** ENTRY as HTML: its label, then what it says as markdown. */
export function entryHtml(entry: ProseEntry): string {
  const label = requireMessage(entry.label, "ProseEntry.label").text;
  const says = requireMessage(entry.text, "ProseEntry.text").markdown;
  const attr = escapeHtml(label);
  return (
    `<div class="${ENTRY_CLASS}" ${ENTRY_ATTRIBUTE}="${attr}">` +
    `<span class="${ENTRY_LABEL_CLASS}" title="edit this entry and resend the prompt">${attr}</span>` +
    `<div class="mp-entry-text">${says === "" ? "" : renderMarkdown(says)}</div>` +
    `</div>`
  );
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
 * Flash ROW's entry answering LABEL, which a linked label revealed. The entry
 * is found in the row as it is drawn now; a row that draws no such entry is
 * recorded, because the daemon named an entry this drawing lacks.
 */
export function flashEntry(row: HTMLElement, label: string): void {
  const entry = queryAll<HTMLElement>(row, `.${ENTRY_CLASS}`).find(
    (el) => el.getAttribute(ENTRY_ATTRIBUTE) === label,
  );
  if (entry === undefined) {
    log.warn("a revealed prompt draws no entry answering the label", {
      operation: "feed.entries.flash-absent",
      context: { row: row.getAttribute("data-feed-row") ?? "unset", label },
    });
    return;
  }
  entry.classList.remove(ENTRY_FLASH_CLASS);
  // A FORCED LAYOUT restarts the animation when the same entry flashes twice.
  void entry.offsetWidth;
  entry.classList.add(ENTRY_FLASH_CLASS);
  entry.addEventListener("animationend", () => entry.classList.remove(ENTRY_FLASH_CLASS), { once: true });
  log.info("a revealed prompt's entry flashed", {
    operation: "feed.entries.flashed",
    context: { row: row.getAttribute("data-feed-row") ?? "unset", label },
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
