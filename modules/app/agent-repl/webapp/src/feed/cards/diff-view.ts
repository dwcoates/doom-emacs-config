/**
 * diff-view — THE CLASSIC DIFF a tool card's output section draws (owner
 * request, 2026-10-08): an added line on a green background, a removed line on
 * a red one, a context line on neither, and NO +/- MARKER drawn as text. The
 * hunk and file header lines stay readable, verbatim, in their own quieter
 * treatment. The backgrounds are theme tokens (`--diff-add-bg`,
 * `--diff-del-bg`), so both themes carry them.
 *
 * TWO SOURCES, ONE DRAWING (`drawClassicDiff`):
 *
 *   - STRUCTURED: the daemon's `diff` output form, whose lines are already
 *     arm-typed and carry no prefix. That is the feed's own knowledge of the
 *     output, and it always wins.
 *   - DETECTED: a `text` output that is a unified diff (a shell's `git diff`,
 *     a patch a tool printed), recognized by `parseUnifiedDiff`. The detector
 *     is CONSERVATIVE on purpose, so ordinary command output is never redrawn
 *     as a diff by accident; see its own comment for the exact rule.
 */
import { log } from "../../log.js";

/** What one line of a classic diff is. */
export type ClassicDiffKind = "added" | "removed" | "context" | "header" | "meta";

/** One line of a classic diff: its kind, and its text WITHOUT any +/-/space marker. */
export interface ClassicDiffLine {
  readonly kind: ClassicDiffKind;
  readonly text: string;
}

/** The class each kind wears inside `.diff` (the stylesheet's vocabulary). */
const KIND_CLASS: Readonly<Record<ClassicDiffKind, string>> = {
  added: "add",
  removed: "del",
  context: "ctx",
  header: "hunk",
  meta: "meta",
};

/**
 * Draw LINES as a classic diff: one block line each, in order, its text alone.
 * The kind rides `data-diff-line` and the class; the color is the
 * stylesheet's, so no marker character is ever part of the text.
 */
export function drawClassicDiff(lines: readonly ClassicDiffLine[], path: string): HTMLElement {
  log.debug("drawing a classic diff", {
    operation: "feed.cards.diff-view.draw",
    context: { path, lines: lines.length },
  });
  const pre = document.createElement("pre");
  pre.className = "tool-output diff-output diff diff-classic";
  for (const line of lines) {
    const el = document.createElement("span");
    el.className = `diff-line ${KIND_CLASS[line.kind]}`;
    el.setAttribute("data-diff-line", line.kind);
    el.textContent = line.text;
    pre.appendChild(el);
  }
  return pre;
}

/** A unified diff's hunk header: "@@ -12,7 +12,9 @@ optional section". */
const HUNK_HEADER = /^@@ -\d+(?:,(\d+))? \+\d+(?:,(\d+))? @@/;

/** The file-level header lines a unified diff (git's included) carries between hunks. */
const FILE_HEADER = [
  /^diff /,
  /^index /,
  /^--- /,
  /^\+\+\+ /,
  /^(?:new|deleted) file mode /,
  /^(?:old|new) mode /,
  /^similarity index /,
  /^dissimilarity index /,
  /^rename (?:from|to) /,
  /^copy (?:from|to) /,
  /^Binary files /,
];

/**
 * Read TEXT as a unified diff, or answer null when it is not one.
 *
 * THE RULE (conservative: a false "no" draws plain text, which is how the card
 * always drew it, while a false "yes" would mangle real output):
 *
 *   - Every line outside a hunk is a file header line (`diff `, `index `,
 *     `--- `, `+++ `, mode, rename, copy and binary lines). Any other line —
 *     a compiler message, a "On branch main" — and the text is not a diff.
 *   - There is at least one hunk, and the first one is preceded by a `--- `
 *     and a `+++ ` line.
 *   - A hunk is read BY ITS COUNTS: the header's old and new line counts are
 *     consumed by its ` ` (or empty), `-` and `+` lines, so a removed line
 *     that itself reads "--- x" is never mistaken for a file header. A
 *     `\ No newline at end of file` line is meta. A hunk the text ends inside
 *     (a truncated output) is kept as far as it goes.
 *
 * One trailing empty line (the output's final newline) is dropped.
 */
export function parseUnifiedDiff(text: string): ClassicDiffLine[] | null {
  const raw = text.split("\n");
  if (raw.length > 0 && raw[raw.length - 1] === "") raw.pop();
  const out: ClassicDiffLine[] = [];
  let oldLeft = 0;
  let newLeft = 0;
  let sawOld = false;
  let sawNew = false;
  let hunks = 0;
  for (const line of raw) {
    if (oldLeft > 0 || newLeft > 0) {
      const marker = line.charAt(0);
      if (marker === "\\") {
        out.push({ kind: "meta", text: line });
        continue;
      }
      if (marker === "+" && newLeft > 0) {
        newLeft -= 1;
        out.push({ kind: "added", text: line.slice(1) });
        continue;
      }
      if (marker === "-" && oldLeft > 0) {
        oldLeft -= 1;
        out.push({ kind: "removed", text: line.slice(1) });
        continue;
      }
      if ((marker === " " || line === "") && oldLeft > 0 && newLeft > 0) {
        oldLeft -= 1;
        newLeft -= 1;
        out.push({ kind: "context", text: line.slice(1) });
        continue;
      }
      return null;
    }
    const hunk = HUNK_HEADER.exec(line);
    if (hunk !== null) {
      if (!sawOld || !sawNew) return null;
      oldLeft = hunk[1] === undefined ? 1 : Number(hunk[1]);
      newLeft = hunk[2] === undefined ? 1 : Number(hunk[2]);
      hunks += 1;
      out.push({ kind: "header", text: line });
      continue;
    }
    if (line.startsWith("\\")) {
      out.push({ kind: "meta", text: line });
      continue;
    }
    if (!FILE_HEADER.some((re) => re.test(line))) return null;
    if (line.startsWith("--- ")) sawOld = true;
    if (line.startsWith("+++ ")) sawNew = true;
    out.push({ kind: "meta", text: line });
  }
  return hunks > 0 ? out : null;
}
