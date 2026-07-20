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

/** Minimum fraction of non-blank lines that must look tree-shaped. */
const TREE_LINE_RATIO = 0.6;

// --- Cheap, allocation-free line classification -----------------------------
//
// Classifying tree lines runs on every assistant render (and drives the
// tree-bounds scan below), so these helpers walk code points directly rather
// than lean on Unicode regex: a connector is a leading │/space run then
// ├──/└──, a root is a column-0 dotted label followed by a space and an
// emoji, and the header is the mandated `Response (…)` opener. Bounds are
// newline-delimited, so slicing the tree out of surrounding prose needs no
// width or wrap reasoning at all.

const CH_SPACE = 0x20;
const CH_TAB = 0x09;
const CH_BAR = 0x2502; // │
const CH_TEE = 0x251c; // ├
const CH_ELL = 0x2514; // └
const CH_HORIZ = 0x2500; // ─
const CH_DOT = 0x2e; // .
const CH_ZERO = 0x30;
const CH_NINE = 0x39;
const CH_BACKTICK = 0x60;
// Floor of the Unicode symbol/arrow/emoji range. Every metaprompt root emoji
// sits at or above it (✅ U+2705, ✏️ U+270F, 🔧/👀 as surrogate pairs from
// U+D83D…), while ASCII prose after a bare number (e.g. `1 first point`)
// stays below it and so is never mistaken for a root.
const EMOJI_FLOOR = 0x2190;

const HEADER_PREFIX = "Response (";

function isDigit(code: number): boolean {
  return code >= CH_ZERO && code <= CH_NINE;
}

/** A child branch line: a leading │/space run, then `├──` or `└──`. */
function isConnectorLine(line: string): boolean {
  let i = 0;
  const n = line.length;
  while (i < n) {
    const c = line.charCodeAt(i);
    if (c === CH_SPACE || c === CH_BAR) i++;
    else break;
  }
  const c = line.charCodeAt(i);
  if (c !== CH_TEE && c !== CH_ELL) return false;
  return line.charCodeAt(i + 1) === CH_HORIZ && line.charCodeAt(i + 2) === CH_HORIZ;
}

/**
 * Consume a column-0 dotted label (`1`, `2.1`, `3.4.5`) and return the index
 * just past it, or -1 when the line does not open on a dotted label. A dot is
 * consumed only when a digit follows it, so a trailing dot ends the label.
 */
function dottedLabelEnd(line: string): number {
  if (!isDigit(line.charCodeAt(0))) return -1;
  let i = 1;
  const n = line.length;
  while (i < n) {
    const c = line.charCodeAt(i);
    if (isDigit(c)) {
      i++;
      continue;
    }
    if (c === CH_DOT && isDigit(line.charCodeAt(i + 1))) {
      i += 2;
      continue;
    }
    break;
  }
  return i;
}

/**
 * A dotted label followed by whitespace, emoji not required. Mirrors the old
 * ROOT_LABEL_RE and feeds only the tree-shaped-line ratio, never the anchor.
 */
function isDottedLabelLine(line: string): boolean {
  const end = dottedLabelEnd(line);
  if (end < 0) return false;
  const c = line.charCodeAt(end);
  return c === CH_SPACE || c === CH_TAB;
}

/** A root branch line: a dotted label, one space, then an emoji root marker. */
function isEmojiRootLine(line: string): boolean {
  const end = dottedLabelEnd(line);
  if (end < 0) return false;
  if (line.charCodeAt(end) !== CH_SPACE) return false;
  return line.charCodeAt(end + 1) >= EMOJI_FLOOR;
}

/** The mandated `Response (…)` opener that heads every metaprompt response. */
function isHeaderLine(line: string): boolean {
  return line.startsWith(HEADER_PREFIX);
}

/** A ``` code-fence delimiter, after any leading spaces. */
function isFenceDelimiter(line: string): boolean {
  let i = 0;
  while (line.charCodeAt(i) === CH_SPACE) i++;
  return (
    line.charCodeAt(i) === CH_BACKTICK &&
    line.charCodeAt(i + 1) === CH_BACKTICK &&
    line.charCodeAt(i + 2) === CH_BACKTICK
  );
}

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
    const connector = isConnectorLine(line);
    const emojiRoot = isEmojiRootLine(line);
    if (connector || emojiRoot || isDottedLabelLine(line)) treeish++;
    if (connector || emojiRoot) anchored = true;
  }
  return anchored && treeish / lines.length >= TREE_LINE_RATIO;
}

/** The line bounds of a metaprompt tree carved out of a text segment. */
export interface TreeRegion {
  /** Lines before the tree, kept on the markdown path (may be empty). */
  before: string;
  /** The tree block itself, rendered as hanging-indent tree lines. */
  tree: string;
  /** Lines after the tree, kept on the markdown path (may be empty). */
  after: string;
}

/**
 * Locate the metaprompt tree's line bounds inside TEXT and split it into the
 * prose BEFORE the tree, the TREE block, and the prose AFTER it. This lets a
 * tree survive stray prefix/postfix lines (or a stray fenced block) the model
 * emits around it despite the format: only the tree region renders as tree
 * lines, and the surrounding lines stay on the markdown path. Returns null
 * when TEXT carries no tree, i.e. fewer than two connector/root lines outside
 * any fence. Fence-aware: lines inside a ``` fence are never tree lines, so a
 * fenced tree is left for the markdown fence handler.
 */
export function findTreeRegion(text: string): TreeRegion | null {
  const lines = text.split("\n");
  const n = lines.length;
  // core[i]: a connector/root line outside any fence. head[i]: the header.
  const core: boolean[] = new Array(n).fill(false);
  const head: boolean[] = new Array(n).fill(false);
  let inFence = false;
  for (let i = 0; i < n; i++) {
    const line = lines[i];
    if (isFenceDelimiter(line)) {
      inFence = !inFence;
      continue;
    }
    if (inFence) continue;
    if (isConnectorLine(line) || isEmojiRootLine(line)) core[i] = true;
    else if (isHeaderLine(line)) head[i] = true;
  }
  // The first tree-core line anchors the region.
  let start = -1;
  for (let i = 0; i < n; i++) {
    if (core[i]) {
      start = i;
      break;
    }
  }
  if (start === -1) return null;
  // Extend across interior blanks; the first non-blank, non-core line (prose
  // or a fence) ends the region. `end` tracks the last core line.
  let end = start;
  let coreCount = 0;
  for (let i = start; i < n; i++) {
    if (core[i]) {
      end = i;
      coreCount++;
      continue;
    }
    if (lines[i].trim() === "") continue;
    break;
  }
  // A lone stray connector buried in prose is not a tree.
  if (coreCount < 2) return null;
  // Pull a directly-preceding `Response (…)` header (across blanks) into the
  // region so it renders inside the tree block.
  let top = start;
  for (let i = start - 1; i >= 0; i--) {
    if (lines[i].trim() === "") continue;
    if (head[i]) top = i;
    break;
  }
  return {
    before: lines.slice(0, top).join("\n"),
    tree: lines.slice(top, end + 1).join("\n"),
    after: lines.slice(end + 1).join("\n"),
  };
}

/**
 * Whether TEXT opens on the mandated `Response (…)` header, marking it as an
 * intended metaprompt response. Used to flag a postprocessing misfire: a
 * header-led segment that yielded no tree region (see findTreeRegion).
 */
export function looksLikeIntendedTree(text: string): boolean {
  for (const line of text.split("\n")) {
    if (line.trim() === "") continue;
    return isHeaderLine(line);
  }
  return false;
}

/** Split one tree line into its bullet PREFIX and its CONTENT text. */
export function splitTreeLine(line: string): { prefix: string; content: string } {
  const m = line.match(PREFIX_RE);
  if (!m) return { prefix: "", content: line };
  return { prefix: m[1], content: m[2] };
}

/**
 * Character columns of PREFIX whose vertical rail must repaint on a
 * wrapped branch's continuation lines: every leading `│`, plus the
 * connector column when the connector is `├` (its rail continues down
 * to the next sibling). A `└` connector ENDS its rail, so it
 * contributes nothing — without this distinction a wrapped `├──`
 * branch visually severs from the sibling below it.
 *
 * Columns are ch offsets: every character preceding a rail char is a
 * space or `│`, single-width in the tree's monospace font.
 */
export function railOffsets(prefix: string): number[] {
  const cols: number[] = [];
  for (let i = 0; i < prefix.length; i++) {
    const ch = prefix[i];
    if (ch === "│" || ch === "├") cols.push(i);
  }
  return cols;
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
      // Rail hairlines: painted from the row's second visual line down
      // (glyphs cover the first), centered in their ch cell. A row that
      // never wraps gives them zero height — invisible.
      const rails = railOffsets(prefix)
        .map((col) => `<i class="mp-rail" style="left:${col + 0.5}ch"></i>`)
        .join("");
      return `<div class="mp-line"><span class="mp-prefix">${escapeHtml(
        prefix,
      )}${rails}</span><span class="mp-content">${inline(escapeHtml(content))}</span></div>`;
    })
    .join("");
}
