/**
 * known-commands — the known slash commands a user prompt names, marked in
 * its drawn text as the Emacs composer marks them while they are typed
 * (lisp/command-completion.el, `agent-repl-command-completion-known`).
 *
 * WHICH COMMANDS ARE KNOWN IS THE DAEMON'S. A user-prompt row carries
 * `frontend.v1.FeedUserPrompt.commands`: the names of the workspace's standing
 * catalog its text holds in slashed form. This end never asks for a catalog;
 * it finds the slash words in what it drew and marks one exactly when its name
 * is listed.
 *
 * THE SLASH WORD IS THE COMPOSER'S. A slash word lies in a run of
 * non-whitespace: it begins with `/` at the run's start or after the run's
 * leading opening brackets and quotes, and ends at the run's end less its
 * trailing closing brackets, quotes and sentence punctuation, never before the
 * slash's next character. So `(/merge-queue).` marks `/merge-queue`, and
 * `/usr/local` names `usr/local`, which no catalog lists.
 */

/** The punctuation a slash word may follow inside its run (the composer's set). */
const OPENING = "([{<\"'`“‘";

/** The punctuation a slash word's run may end in, outside the word (the composer's set). */
const CLOSING = ")]}>\"'`.,;:!?”’…";

/** The class a marked slash word wears; the stylesheet colors it. */
export const KNOWN_COMMAND_CLASS = "known-command";

/** One slash word: its span in the text, and its name without the slash. */
export interface SlashWord {
  readonly start: number;
  readonly end: number;
  readonly name: string;
}

/** Every slash word of TEXT, in order. */
export function slashWords(text: string): SlashWord[] {
  const words: SlashWord[] = [];
  const runs = /\S+/g;
  for (let run = runs.exec(text); run !== null; run = runs.exec(text)) {
    const runStart = run.index;
    const runEnd = runStart + run[0].length;
    let start = runStart;
    while (start < runEnd && OPENING.includes(text.charAt(start))) start++;
    if (start === runEnd || text.charAt(start) !== "/") continue;
    let end = runEnd;
    while (end > start + 1 && CLOSING.includes(text.charAt(end - 1))) end--;
    words.push({ start, end, name: text.slice(start + 1, end) });
  }
  return words;
}

/**
 * Mark every slash word under ROOT whose name is in NAMES: each is wrapped in
 * a `span.known-command`. A text node is the run boundary, so a word an
 * element splits (`/merge-**queue**`) is two runs and neither is marked.
 */
export function markKnownCommands(root: HTMLElement, names: ReadonlySet<string>): void {
  if (names.size === 0) return;
  const doc = root.ownerDocument;
  const walker = doc.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  const texts: Text[] = [];
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    texts.push(node as Text);
  }
  for (const node of texts) {
    const text = node.data;
    const known = slashWords(text).filter((word) => names.has(word.name));
    if (known.length === 0) continue;
    const parts = doc.createDocumentFragment();
    let at = 0;
    for (const word of known) {
      if (word.start > at) parts.append(text.slice(at, word.start));
      const mark = doc.createElement("span");
      mark.className = KNOWN_COMMAND_CLASS;
      mark.textContent = text.slice(word.start, word.end);
      parts.append(mark);
      at = word.end;
    }
    if (at < text.length) parts.append(text.slice(at));
    node.replaceWith(parts);
  }
}
