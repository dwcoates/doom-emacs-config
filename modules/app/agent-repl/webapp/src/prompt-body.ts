/**
 * Fenced-code rendering for the user's own prompt bubble.
 *
 * A prompt is shown verbatim in a `<pre>` — every character the person
 * typed, unformatted — EXCEPT for markdown fenced code blocks, which
 * render as the same syntax-highlighted card the agent's own markdown
 * fences get (see `markdown.ts`'s fence rule). Everything outside a fence
 * stays literal escaped text, so prose that happens to contain
 * *asterisks* or _underscores_ is never reinterpreted as markdown — only
 * genuine code fences are lifted out.
 *
 * A code block is a `` ```lang `` opening fence, a body, and a closing
 * `` ``` `` fence. The opening fence MUST carry a language token (a bare
 * `` ``` `` is left as literal text), matching "the triple-backtick
 * followed by a programming language". An opening fence with no matching
 * closing fence is not a block either — its lines stay literal — so a
 * stray `` ``` `` in prose never swallows the rest of the prompt.
 *
 * Safety: both branches only ever emit escaped text — `highlightCode`
 * escapes its own output (hljs does, and the unknown-language fallback is
 * `escapeHtml`), and text segments go through `escapeHtml` — so the
 * result is safe to inject wherever the plain `<pre>` was.
 */

import { escapeHtml, highlightCode } from "./highlight.js";

/** One run of the prompt: a literal text span or a fenced code block. */
export type PromptSegment =
  | { kind: "text"; text: string }
  | { kind: "code"; lang: string; code: string };

// An opening fence: three backticks, then a language token (no spaces,
// no backticks), and only trailing blanks after it. The language
// requirement is what distinguishes a code block from a bare ``` the
// user typed as prose.
const OPEN_FENCE = /^```([^\s`]+)[ \t]*$/;
// A closing fence: three backticks alone on the line.
const CLOSE_FENCE = /^```[ \t]*$/;

/**
 * Split PROMPT into literal-text and fenced-code segments. A fence pairs
 * an opening `` ```lang `` line with the next closing `` ``` `` line;
 * everything between them is the block's body, everything else is text.
 * An unpaired opening fence contributes no block — its lines fall back to
 * text — so the split never runs past the end of a real block.
 */
export function splitPromptSegments(prompt: string): PromptSegment[] {
  const lines = prompt.split("\n");
  const segments: PromptSegment[] = [];
  let textStart = 0; // first line index of the pending text run
  const flushText = (end: number): void => {
    if (end > textStart) {
      segments.push({ kind: "text", text: lines.slice(textStart, end).join("\n") });
    }
  };
  let i = 0;
  while (i < lines.length) {
    const open = OPEN_FENCE.exec(lines[i]);
    if (open) {
      let j = i + 1;
      while (j < lines.length && !CLOSE_FENCE.test(lines[j])) j++;
      if (j < lines.length) {
        flushText(i);
        segments.push({ kind: "code", lang: open[1], code: lines.slice(i + 1, j).join("\n") });
        i = j + 1;
        textStart = i;
        continue;
      }
    }
    i++;
  }
  flushText(lines.length);
  return segments;
}

/** The syntax-highlighted card for one fenced block, matching the shape
 * `markdown.ts`'s fence rule emits so a prompt's code reads identically
 * to the agent's. */
function codeSegmentHtml(lang: string, code: string): string {
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  return `<pre class="md-code"><code class="hljs${langClass}">${highlightCode(code, lang)}</code></pre>`;
}

/**
 * HTML body for a prompt bubble: literal text in `<pre>`, fenced code as
 * highlighted `md-code` cards. A prompt with no fence keeps the exact
 * single-`<pre>` shape the bubble has always had, so a plain prompt
 * renders byte-for-byte as before.
 */
export function renderPromptBody(prompt: string): string {
  const segments = splitPromptSegments(prompt);
  if (segments.length === 1 && segments[0].kind === "text") {
    return `<pre>${escapeHtml(segments[0].text)}</pre>`;
  }
  return segments
    .map((seg) =>
      seg.kind === "text"
        ? `<pre>${escapeHtml(seg.text)}</pre>`
        : codeSegmentHtml(seg.lang, seg.code),
    )
    .join("");
}
