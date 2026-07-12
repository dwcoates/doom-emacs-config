/**
 * Metaprompt TLDR-tree detection and rendering.
 *
 * The metaprompt renders final responses as a numbered ASCII tree
 * (├──/└──/│ connectors plus dotted labels like `2.1`, root nodes
 * emoji-prefixed). As plain <pre> text, a branch longer than the feed
 * width wraps to column 0, visually shearing the tree. Instead each
 * tree line renders as a flex row of a fixed-width PREFIX span (the
 * connectors, dotted label, and root emoji — everything up to the
 * first non-bullet character) and a flexing CONTENT span: the browser
 * wraps the content within its own column, so continuation lines get
 * a hanging indent at the content start with no width measurement at
 * all.
 */

import { escapeHtml } from "./highlight.js";

/**
 * The bullet machinery at the start of one tree line: leading vertical
 * bars/space, one connector, the dotted hierarchical label, and the
 * root node's prefixing emoji. Every group is optional so plain lines
 * (e.g. the `Response (…)` header) split as all-content.
 */
const PREFIX_RE =
  /^([\s│]*(?:(?:├──|└──)\s+)?(?:\d+(?:\.\d+)*\s+)?(?:\p{Extended_Pictographic}\uFE0F?\s+)?)(.*)$/u;

const CONNECTOR_RE = /^[\s│]*(?:├──|└──)\s/;
const ROOT_LABEL_RE = /^\d+(?:\.\d+)*\s/;
const EMOJI_ROOT_RE = /^\d+(?:\.\d+)*\s+\p{Extended_Pictographic}/u;

/** Minimum fraction of non-blank lines that must look tree-shaped. */
const TREE_LINE_RATIO = 0.6;

/**
 * Whether TEXT reads as a metaprompt TLDR tree: at least two non-blank
 * lines, most of them tree-shaped (connector or dotted-label start),
 * anchored by either a connector line or an emoji-prefixed root — the
 * two shapes ordinary prose and markdown lists never produce.
 */
export function isMetapromptTree(text: string): boolean {
  const lines = text.split("\n").filter((l) => l.trim() !== "");
  if (lines.length < 2) return false;
  let treeish = 0;
  let anchored = false;
  for (const line of lines) {
    const connector = CONNECTOR_RE.test(line);
    if (connector || ROOT_LABEL_RE.test(line)) treeish++;
    if (connector || EMOJI_ROOT_RE.test(line)) anchored = true;
  }
  return anchored && treeish / lines.length >= TREE_LINE_RATIO;
}

/** Split one tree line into its bullet PREFIX and its CONTENT text. */
export function splitTreeLine(line: string): { prefix: string; content: string } {
  const m = line.match(PREFIX_RE);
  if (!m) return { prefix: "", content: line };
  return { prefix: m[1], content: m[2] };
}

/**
 * Render TEXT as mp-line flex rows. INLINE post-processes the escaped
 * content span (markdown.ts's inline pass — injected rather than
 * imported so this module never depends back on markdown.ts); pass the
 * identity function for plain escaped text.
 */
export function renderTreeHtml(
  text: string,
  inline: (escaped: string) => string,
): string {
  return text
    .split("\n")
    .map((line) => {
      if (line.trim() === "") return `<div class="mp-line mp-blank"></div>`;
      const { prefix, content } = splitTreeLine(line);
      return `<div class="mp-line"><span class="mp-prefix">${escapeHtml(
        prefix,
      )}</span><span class="mp-content">${inline(escapeHtml(content))}</span></div>`;
    })
    .join("");
}
