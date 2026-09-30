/**
 * Metaprompt TLDR-tree detection and wrapping.
 *
 * The metaprompt renders final responses as a numbered Unicode tree
 * (├──/└──/│ connectors plus dotted labels like `2.1`, root nodes
 * emoji-prefixed). The tree arrives UNWRAPPED — one physical line per branch —
 * and this module wraps every branch too wide for the live column limit onto
 * continuation lines, so no line exceeds the width the bubble can show and the
 * tree fills its bubble and re-flows on resize.
 *
 * THE WRAP ENGINE IS A ONE-TO-ONE PORT of the daemon's `treefmt` package
 * (`daemon/internal/treefmt/treefmt.go`), which is itself the owner's
 * format_trees.py ported to Go: the same parser, the same packer, the same
 * width model, the same idempotence, the same error surface. The wrapping used
 * to run in the daemon at a fixed 105 columns; it runs HERE now, at the live
 * width the bubble measures, because the daemon cannot know how many characters
 * fit a given pixel width and so cannot re-flow on resize. Each ported function
 * carries its Go/Python name so the three can be read side by side.
 *
 * A wrapped branch's continuation lines
 *
 *   - start at the column where the branch text starts, so the wrapped
 *     remainder reads as a hanging indent under its own branch, and
 *   - carry the vertical connectors of every sibling branch the wrap now
 *     bisects (`├── ` becomes `│   `, `└── ` becomes four spaces), and hold
 *     open the connector column of the branch's own children when any are
 *     rendered beneath the wrap, so the tree's vertical rules stay unbroken.
 *
 * The rails are therefore REAL characters the wrapper emits, not rails a
 * renderer repaints: the continuation prefix is `│   ` / spaces text, so the
 * wrapped output renders as ordinary monospace lines with no width reasoning
 * left to the stylesheet.
 *
 * Width is measured in RENDERED columns, not source length: HTML tags
 * contribute nothing, HTML entities count as the single character they denote,
 * and emoji count as two columns. Inline elements are atomic, so a wrap never
 * lands between a tag and its text; an element too long to fit alone is split
 * with its tags closed at the end of one line and reopened at the start of the
 * next.
 *
 * THE COUNT IS THE DRAWN WIDTH BY CONSTRUCTION. A double-width character (an
 * emoji above all) renders wider than two monospace columns in the webview's
 * fonts, so every character the width model counts as two is drawn inside a
 * `WIDE_CHAR_CLASS` box the stylesheet sizes to exactly `2ch` of the tree font.
 *
 * THERE IS NO DEFAULT WIDTH. The caller measures the column budget; a width
 * that is not a positive whole number of columns is refused loudly, never
 * replaced by a guess.
 */

import { escapeHtml, highlightCode } from "./highlight.js";
import { log } from "./log.js";

/**
 * The class of the inline box every double-width character of a tree line is
 * drawn in. The stylesheet makes it exactly `2ch` wide in the tree's own font,
 * which is the two columns `charWidth` counts it as.
 */
export const WIDE_CHAR_CLASS = "mp-wide";

// ---------------------------------------------------------------------------
// Cheap line classification (detection only)
// ---------------------------------------------------------------------------
//
// Classifying tree lines runs on every assistant render (and drives the
// tree-bounds scan below), so these helpers walk code points directly rather
// than lean on Unicode regex: a connector is a leading │/space run then
// ├──/└──, a root is a column-0 dotted label followed by a space and an emoji,
// and the header is the mandated `Response (…)` opener.

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
const CH_BACKSLASH = 0x5c;
// Floor of the Unicode symbol/arrow/emoji range. Every metaprompt root emoji
// sits at or above it (✅ U+2705, ✏️ U+270F, 🔧/👀 as surrogate pairs from
// U+D83D…), while ASCII prose after a bare number (e.g. `1 first point`)
// stays below it and so is never mistaken for a root.
const EMOJI_FLOOR = 0x2190;

const HEADER_PREFIX = "Response (";

/** Minimum fraction of non-blank lines that must look tree-shaped. */
const TREE_LINE_RATIO = 0.6;

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

/** A dotted label followed by whitespace, emoji not required. */
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

/**
 * A ``` code-fence delimiter, after any leading rail (fenceIndent). A fence the
 * model drew behind its branch's `│` rail is still a fence: read as a plain
 * line it is neither tree-core nor a delimiter, so it ENDED the tree region and
 * every later branch spilled onto the markdown path (2026-09-30).
 */
function isFenceDelimiter(line: string): boolean {
  const i = fenceIndent(line);
  return (
    line.charCodeAt(i) === CH_BACKTICK &&
    line.charCodeAt(i + 1) === CH_BACKTICK &&
    line.charCodeAt(i + 2) === CH_BACKTICK
  );
}

/**
 * The width of LINE's leading rail: spaces and `│` connectors. A fence body
 * is dedented by its opening delimiter's rail, so code drawn beneath a branch
 * with or without the tree's vertical rules reads as the same plain block.
 */
function fenceIndent(line: string): number {
  let i = 0;
  for (;;) {
    const c = line.charCodeAt(i);
    if (c !== CH_SPACE && c !== CH_BAR) return i;
    i++;
  }
}

/**
 * Whether TEXT reads as a metaprompt TLDR tree: at least two non-blank lines,
 * most of them tree-shaped (connector or dotted-label start), anchored by
 * either a connector line or an emoji-prefixed root — the two shapes ordinary
 * prose and markdown lists never produce.
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
  /** The tree block itself, wrapped and rendered as tree lines. */
  tree: string;
  /** Lines after the tree, kept on the markdown path (may be empty). */
  after: string;
}

/**
 * Locate the metaprompt tree's line bounds inside TEXT and split it into the
 * prose BEFORE the tree, the TREE block, and the prose AFTER it. This lets a
 * tree survive stray prefix/postfix lines (or a stray fenced block) the model
 * emits around it despite the format: only the tree region is wrapped as tree
 * lines, and the surrounding lines stay on the markdown path. Returns null when
 * TEXT carries no tree, i.e. fewer than two connector/root lines outside any
 * fence. Fence-aware: lines inside a ``` fence are never tree lines, so a
 * fenced tree is left for the markdown fence handler.
 */
export function findTreeRegion(text: string): TreeRegion | null {
  const lines = text.split("\n");
  const n = lines.length;
  // core[i]: a connector/root line outside any fence. head[i]: the header.
  const core: boolean[] = new Array<boolean>(n).fill(false);
  const head: boolean[] = new Array<boolean>(n).fill(false);
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
  // Extend across interior blanks; the first non-blank, non-core line ends the
  // region — EXCEPT an interior fenced block, which the metaprompt allows
  // attached beneath a branch. A fence delimiter is not itself a tree-core line,
  // so a fenced code block nested under a branch (and every branch after it) must
  // be spanned rather than treated as a hard boundary. The region absorbs such a
  // block and RESUMES tree detection past its close. A fence still ends the
  // region when it is NOT followed by more tree-core lines: a genuinely trailing
  // fenced block after the tree stays in `after`. `end` tracks the last line kept.
  let end = start;
  let coreCount = 0;
  let i = start;
  while (i < n) {
    if (core[i]) {
      end = i;
      coreCount++;
      i++;
      continue;
    }
    if (lines[i].trim() === "") {
      i++;
      continue;
    }
    if (isFenceDelimiter(lines[i])) {
      // Find the matching close (or the end of text for an unterminated fence).
      let close = i + 1;
      while (close < n && !isFenceDelimiter(lines[close])) close++;
      // Peek past the close for the next non-blank line: only a tree-core line
      // there makes this an INTERIOR block worth spanning.
      let peek = close + 1;
      while (peek < n && lines[peek].trim() === "") peek++;
      if (close < n && peek < n && core[peek]) {
        end = close;
        i = close + 1;
        continue;
      }
      // Trailing or unterminated fenced block: it is `after`, not the tree.
      break;
    }
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

// ---------------------------------------------------------------------------
// Rendered width (treefmt: StripTags / charWidth / textWidth / VisibleWidth)
// ---------------------------------------------------------------------------

const TAG_RE = /<[^>]*>/;
const TAG_NAME_RE = /^<\/?\s*([A-Za-z][A-Za-z0-9]*)/;

const VARIATION_SELECTOR_16 = "️";
const ZERO_WIDTH_JOINER = "‍";

const voidTags = new Set([
  "br",
  "hr",
  "img",
  "wbr",
  "input",
  "meta",
  "link",
]);

// A code point that occupies no column of its own: a combining mark or a
// format character (charWidth's Mn/Me/Cf test). The zero-width joiner is Cf and
// so is covered here too.
const ZERO_WIDTH_RE = /^(?:\p{Mn}|\p{Me}|\p{Cf})$/u;

// East Asian Wide (W) and Fullwidth (F) ranges, the code points x/text/width
// reports as two columns. Ambiguous (box drawing 2500–257F included) and
// Narrow stay single-width, so the tree's own connectors count as one column
// each. Emoji live in the astral pictographic ranges and count as two.
const WIDE_RANGES: ReadonlyArray<readonly [number, number]> = [
  [0x1100, 0x115f],
  [0x231a, 0x231b],
  [0x2329, 0x232a],
  [0x23e9, 0x23ec],
  [0x23f0, 0x23f0],
  [0x23f3, 0x23f3],
  [0x25fd, 0x25fe],
  [0x2614, 0x2615],
  [0x2648, 0x2653],
  [0x267f, 0x267f],
  [0x2693, 0x2693],
  [0x26a1, 0x26a1],
  [0x26aa, 0x26ab],
  [0x26bd, 0x26be],
  [0x26c4, 0x26c5],
  [0x26ce, 0x26ce],
  [0x26d4, 0x26d4],
  [0x26ea, 0x26ea],
  [0x26f2, 0x26f3],
  [0x26f5, 0x26f5],
  [0x26fa, 0x26fa],
  [0x26fd, 0x26fd],
  [0x2705, 0x2705],
  [0x270a, 0x270b],
  [0x2728, 0x2728],
  [0x274c, 0x274c],
  [0x274e, 0x274e],
  [0x2753, 0x2755],
  [0x2757, 0x2757],
  [0x2795, 0x2797],
  [0x27b0, 0x27b0],
  [0x27bf, 0x27bf],
  [0x2b1b, 0x2b1c],
  [0x2b50, 0x2b50],
  [0x2b55, 0x2b55],
  [0x2e80, 0x303e],
  [0x3041, 0x33ff],
  [0x3400, 0x4dbf],
  [0x4e00, 0x9fff],
  [0xa000, 0xa4cf],
  [0xa960, 0xa97f],
  [0xac00, 0xd7a3],
  [0xf900, 0xfaff],
  [0xfe10, 0xfe19],
  [0xfe30, 0xfe6f],
  [0xff00, 0xff60],
  [0xffe0, 0xffe6],
  [0x1b000, 0x1b001],
  [0x1f004, 0x1f004],
  [0x1f0cf, 0x1f0cf],
  [0x1f18e, 0x1f18e],
  [0x1f191, 0x1f19a],
  [0x1f200, 0x1f251],
  [0x1f300, 0x1f64f],
  [0x1f680, 0x1f6ff],
  [0x1f900, 0x1f9ff],
  [0x1fa70, 0x1faff],
  [0x20000, 0x3fffd],
];

function isWideCodePoint(cp: number): boolean {
  for (const [lo, hi] of WIDE_RANGES) {
    if (cp < lo) return false;
    if (cp <= hi) return true;
  }
  return false;
}

/** decode_entities: the HTML entities the width model must count as one char. */
const NAMED_ENTITIES: Readonly<Record<string, string>> = {
  amp: "&",
  lt: "<",
  gt: ">",
  quot: '"',
  apos: "'",
  nbsp: " ",
};

function decodeEntities(text: string): string {
  return text.replace(/&(#x?[0-9a-fA-F]+|[a-zA-Z][a-zA-Z0-9]*);/g, (whole, body: string) => {
    if (body[0] === "#") {
      const cp =
        body[1] === "x" || body[1] === "X"
          ? Number.parseInt(body.slice(2), 16)
          : Number.parseInt(body.slice(1), 10);
      if (!Number.isFinite(cp) || cp < 0 || cp > 0x10ffff) return whole;
      try {
        return String.fromCodePoint(cp);
      } catch {
        return whole;
      }
    }
    const named = NAMED_ENTITIES[body];
    return named ?? whole;
  });
}

/** strip_tags: raw with HTML tags removed and entities decoded. */
export function stripTags(raw: string): string {
  return decodeEntities(raw.replace(new RegExp(TAG_RE.source, "g"), ""));
}

/**
 * char_width: the columns CHAR occupies. A character carrying the emoji
 * variation selector renders as an emoji and takes two columns; the selector
 * itself, combining marks and format characters take none.
 */
function charWidth(char: string, followedByVS16: boolean): number {
  if (char === ZERO_WIDTH_JOINER || ZERO_WIDTH_RE.test(char)) return 0;
  if (followedByVS16) return 2;
  return isWideCodePoint(char.codePointAt(0) ?? 0) ? 2 : 1;
}

/** text_width: the rendered column width of already-decoded text. */
function textWidth(text: string): number {
  const chars = [...text];
  let total = 0;
  for (let i = 0; i < chars.length; i++) {
    const next = i + 1 < chars.length ? chars[i + 1] : "";
    total += charWidth(chars[i], next === VARIATION_SELECTOR_16);
  }
  return total;
}

/** visible_width: the rendered column width of RAW, which may contain markup. */
export function visibleWidth(raw: string): number {
  return textWidth(stripTags(raw));
}

// ---------------------------------------------------------------------------
// Markup-aware tokenization (treefmt: Run / Subword / Atom / Tokenize)
// ---------------------------------------------------------------------------

interface Run {
  raw: string;
  isTag: boolean;
}

function runWidth(r: Run): number {
  return r.isTag ? 0 : textWidth(decodeEntities(r.raw));
}

interface Subword {
  raw: string;
  width: number;
  openAfter: string[];
}

interface Atom {
  runs: Run[];
}

function atomRaw(a: Atom): string {
  return a.runs.map((r) => r.raw).join("");
}

function atomWidth(a: Atom): number {
  let total = 0;
  for (const r of a.runs) total += runWidth(r);
  return total;
}

/** tag_name: the lower-cased element name of TAG, or "". */
function tagName(tag: string): string {
  const m = TAG_NAME_RE.exec(tag);
  return m ? m[1].toLowerCase() : "";
}

/** apply_tag: update STACK for the HTML tag, ignoring void and self-closing. */
function applyTag(stack: string[], tag: string): void {
  const m = TAG_NAME_RE.exec(tag);
  if (!m) return;
  const name = m[1].toLowerCase();
  if (voidTags.has(name) || tag.endsWith("/>")) return;
  if (tag.startsWith("</")) {
    for (let i = stack.length - 1; i >= 0; i--) {
      if (tagName(stack[i]) === name) {
        stack.splice(i, 1);
        return;
      }
    }
    return;
  }
  stack.push(tag);
}

/** close_sequence: close every open element, innermost first. */
function closeSequence(stack: string[]): string {
  let out = "";
  for (let i = stack.length - 1; i >= 0; i--) out += `</${tagName(stack[i])}>`;
  return out;
}

/** open_sequence: reopen every open element in order. */
function openSequence(stack: string[]): string {
  return stack.join("");
}

/** str.isspace: every code point of S is white space, and S is not empty. */
function isSpace(s: string): boolean {
  return s !== "" && /^\s+$/u.test(s);
}

/**
 * split_whitespace: alternating whitespace and non-whitespace segments, empties
 * dropped (Python's re.split(r"(\s+)")).
 */
function splitWhitespace(text: string): string[] {
  return text.match(/\s+|\S+/gu) ?? [];
}

/** parse_runs: split RAW into its markup and content runs. */
function parseRuns(raw: string): Run[] {
  const runs: Run[] = [];
  const re = new RegExp(TAG_RE.source, "g");
  let position = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(raw)) !== null) {
    if (m.index > position) runs.push({ raw: raw.slice(position, m.index), isTag: false });
    runs.push({ raw: m[0], isTag: true });
    position = m.index + m[0].length;
  }
  if (position < raw.length) runs.push({ raw: raw.slice(position), isTag: false });
  return runs;
}

/**
 * Atom.subwords: split the atom into whitespace-delimited words, tracking the
 * open tag stack. Used only as the fallback for an atom too wide to fit a line
 * on its own; the tag stack lets each resulting line close and reopen whatever
 * element the split lands inside.
 */
function atomSubwords(a: Atom): Subword[] {
  const result: Subword[] = [];
  const stack: string[] = [];
  let pending = "";
  let pendingWidth = 0;
  const flush = (): void => {
    if (pending !== "") {
      result.push({ raw: pending, width: pendingWidth, openAfter: [...stack] });
      pending = "";
      pendingWidth = 0;
    }
  };
  for (const r of a.runs) {
    if (r.isTag) {
      applyTag(stack, r.raw);
      if (r.raw.startsWith("</") && pending === "" && result.length > 0) {
        // A close tag separated from its text by whitespace still belongs to
        // the word it closes, not to the word after it.
        const last = result[result.length - 1];
        result[result.length - 1] = { raw: last.raw + r.raw, width: last.width, openAfter: [...stack] };
      } else {
        pending += r.raw;
      }
      continue;
    }
    for (const segment of splitWhitespace(decodeEntities(r.raw))) {
      if (isSpace(segment)) {
        flush();
      } else {
        pending += segment;
        pendingWidth += textWidth(segment);
      }
    }
  }
  flush();
  return result;
}

/**
 * tokenize: split branch text into the atoms a wrap may be placed between.
 * Whitespace inside an inline element does not separate atoms, so an element
 * stays whole; whitespace outside any element does.
 */
function tokenize(raw: string): Atom[] {
  const atoms: Atom[] = [];
  const stack: string[] = [];
  let current: Atom = { runs: [] };
  const flush = (): void => {
    if (current.runs.length > 0) {
      atoms.push(current);
      current = { runs: [] };
    }
  };
  for (const r of parseRuns(raw)) {
    if (r.isTag) {
      current.runs.push(r);
      applyTag(stack, r.raw);
      continue;
    }
    if (stack.length > 0) {
      current.runs.push(r);
      continue;
    }
    for (const segment of splitWhitespace(r.raw)) {
      if (isSpace(segment)) flush();
      else current.runs.push({ raw: segment, isTag: false });
    }
  }
  flush();
  return atoms;
}

// ---------------------------------------------------------------------------
// Branch parsing (treefmt: Branch / parse_prefix / match_label / parse_branch)
// ---------------------------------------------------------------------------

const SEGMENT_WIDTH = 4;

// segment_continuations: what must appear beneath each prefix segment on a
// continuation line. A `├── ` connector means the branch has following
// siblings, so the vertical rule continues past the wrap; a `└── ` connector
// means it does not, so the column goes blank.
const SEGMENT_CONTINUATIONS: Readonly<Record<string, string>> = {
  "│   ": "│   ",
  "|   ": "|   ",
  "    ": "    ",
  "├── ": "│   ",
  "└── ": "    ",
  "|-- ": "|   ",
  "+-- ": "|   ",
  "`-- ": "    ",
};

// connector_segments: the segments that terminate a prefix. A branch has
// exactly one connector, and it is the last segment before the label.
const CONNECTOR_SEGMENTS = new Set([
  "├── ",
  "└── ",
  "|-- ",
  "+-- ",
  "`-- ",
]);

interface Branch {
  /**
   * Leading whitespace before the aligned tree prefix. The daemon formatter's
   * output is 4-column-aligned, but the model sometimes emits a stray leading
   * space (or a non-4-aligned run) before a connector; that indent is preserved
   * verbatim so the branch and its wrapped continuation hang together, rather
   * than desyncing segment alignment and forcing the line to render raw.
   */
  indent: string;
  prefix: string;
  label: string;
  body: string;
}

/** segments: cut PREFIX into its 4-code-point pieces. */
function segments(prefix: string): string[] {
  const cps = [...prefix];
  const out: string[] = [];
  for (let i = 0; i < cps.length; i += SEGMENT_WIDTH) {
    out.push(cps.slice(i, i + SEGMENT_WIDTH).join(""));
  }
  return out;
}

/** Branch.continuation_prefix: the prefix a wrapped remainder carries. */
function continuationPrefix(b: Branch): string {
  let out = "";
  for (const segment of segments(b.prefix)) out += SEGMENT_CONTINUATIONS[segment] ?? "";
  return out;
}

/** Branch.text_column: the column at which the branch text starts. */
function textColumn(b: Branch): number {
  return visibleWidth(b.indent) + visibleWidth(b.prefix) + visibleWidth(b.label);
}

/**
 * Branch.continuation_indent: the full indent a wrapped remainder carries. The
 * label's columns become padding, except that the first of them holds a
 * vertical rule when the branch has children rendered beneath the wrap.
 */
function continuationIndent(b: Branch, hasChildren: boolean): string {
  const labelWidth = visibleWidth(b.label);
  const padding =
    hasChildren && labelWidth > 0
      ? "│" + " ".repeat(labelWidth - 1)
      : " ".repeat(labelWidth);
  return b.indent + continuationPrefix(b) + padding;
}

/** Branch.is_parent_of: whether OTHER is a direct child of B. The leading
 * indent is part of the structure a child inherits, so it is compared too. */
function isParentOf(b: Branch, other: Branch): boolean {
  const cont = b.indent + continuationPrefix(b);
  const otherFull = other.indent + other.prefix;
  return (
    otherFull.startsWith(cont) &&
    [...otherFull].length === [...cont].length + SEGMENT_WIDTH
  );
}

/** aligned_prefix: walk LINE's 4-column tree segments from the start, stopping
 * at the connector. This is the daemon formatter's exact prefix model, which
 * assumes the prefix begins at column 0. */
function alignedPrefix(line: string): { prefix: string; remainder: string } {
  const cps = [...line];
  let position = 0;
  let collected = "";
  for (;;) {
    const segment = cps.slice(position, position + SEGMENT_WIDTH).join("");
    if (!(segment in SEGMENT_CONTINUATIONS)) break;
    collected += segment;
    position += SEGMENT_WIDTH;
    if (CONNECTOR_SEGMENTS.has(segment)) break;
  }
  return { prefix: collected, remainder: cps.slice(position).join("") };
}

/** Whether CODE is a space or tab, the only leading indent the peeler steps
 * over. */
function isIndentCp(code: number): boolean {
  return code === CH_SPACE || code === CH_TAB;
}

/**
 * parse_prefix: split LINE into an optional leading INDENT, its aligned tree
 * PREFIX, and the REMAINDER after it.
 *
 * REGRESSION WATCH (leading whitespace before a connector must NOT force raw):
 * a stray non-4-aligned leading space before a connector desyncs the aligned
 * segment walk, so a branch like ` └── 1.1 …` would otherwise parse to an empty
 * prefix and, lacking a label at its head, render raw and overflow the bubble.
 * The peeler steps over the MINIMAL leading-whitespace run that lets the aligned
 * walk find a connector-terminated prefix or a labelled remainder, so a genuine
 * 4-aligned space segment (`    ├── …`) is still consumed as a segment and only
 * the stray excess becomes indent. When no branch shape is found at any offset,
 * it falls back to the aligned walk at column 0 (so continuations and genuine
 * prose behave exactly as before).
 */
function parsePrefix(line: string): { indent: string; prefix: string; remainder: string } {
  const cps = [...line];
  for (let skip = 0; skip <= cps.length; skip++) {
    if (skip > 0 && !isIndentCp(cps[skip - 1].charCodeAt(0))) break;
    const { prefix, remainder } = alignedPrefix(cps.slice(skip).join(""));
    const segs = segments(prefix);
    const hasConnector = prefix !== "" && CONNECTOR_SEGMENTS.has(segs[segs.length - 1]);
    if (hasConnector || matchLabel(remainder)) {
      return { indent: cps.slice(0, skip).join(""), prefix, remainder };
    }
  }
  const { prefix, remainder } = alignedPrefix(line);
  return { indent: "", prefix, remainder };
}

const LABEL_RE = /^(\d+(?:\.\d+)*\.?\s+)/;

/** match_label: LABEL_RE — the label (digits, dots, trailing whitespace). */
function matchLabel(s: string): { label: string; end: number } | null {
  const m = LABEL_RE.exec(s);
  if (!m) return null;
  return { label: m[1], end: m[1].length };
}

/** parse_branch: parse LINE as a branch head, or return null. */
function parseBranch(line: string): Branch | null {
  if (line.trim() === "") return null;
  const { indent, prefix, remainder } = parsePrefix(line);
  const segs = segments(prefix);
  const hasConnector = prefix !== "" && CONNECTOR_SEGMENTS.has(segs[segs.length - 1]);
  const matched = matchLabel(remainder);
  if (!matched) {
    // A branch with no label is still a branch when it carries a connector.
    if (!hasConnector) return null;
    return { indent, prefix, label: "", body: remainder.trim() };
  }
  return { indent, prefix, label: matched.label, body: remainder.slice(matched.end).trim() };
}

/**
 * parse_continuation: the (text column, text) of LINE if it can be a wrapped
 * remainder. A continuation carries no connector and no label: it is vertical
 * rules and blanks, then alignment padding, then text.
 */
function parseContinuation(line: string): { column: number; text: string } | null {
  if (line.trim() === "") return null;
  const { indent, prefix, remainder } = parsePrefix(line);
  if (prefix !== "") {
    const segs = segments(prefix);
    if (CONNECTOR_SEGMENTS.has(segs[segs.length - 1])) return null;
  }
  // The padding region may carry the branch's held-open child connector, so a
  // leading vertical rule counts as padding rather than as text.
  const stripped = remainder.replace(/^[ │|]+/, "");
  const padding = [...remainder].length - [...stripped].length;
  const text = stripped;
  if (text === "") return null;
  if (matchLabel(text)) return null;
  return { column: visibleWidth(indent) + visibleWidth(prefix) + padding, text: text.replace(/\s+$/u, "") };
}

// ---------------------------------------------------------------------------
// Wrapping (treefmt: Overflow / Piece / to_pieces / pack / wrap_branch)
// ---------------------------------------------------------------------------

/** Overflow: a single branch whose prefix alone cannot fit the column limit. */
export class TreeOverflowError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "TreeOverflowError";
  }
}

interface Piece {
  raw: string;
  width: number;
  breakSuffix: string;
  breakPrefix: string;
}

/** to_pieces: turn atoms into packer pieces, expanding any atom too wide. */
function toPieces(atoms: Atom[], width: number): Piece[] {
  const pieces: Piece[] = [];
  for (const atom of atoms) {
    if (atomWidth(atom) <= width) {
      pieces.push({ raw: atomRaw(atom), width: atomWidth(atom), breakSuffix: "", breakPrefix: "" });
      continue;
    }
    let stack: string[] = [];
    for (const sw of atomSubwords(atom)) {
      pieces.push({
        raw: sw.raw,
        width: sw.width,
        breakSuffix: closeSequence(stack),
        breakPrefix: openSequence(stack),
      });
      stack = sw.openAfter;
    }
  }
  return pieces;
}

/**
 * The markdown inline-code state carried across a branch's pieces: whether a
 * backtick code span is currently open and, if so, the length of its opening
 * backtick run (a span closes only on a run of exactly that length, per
 * CommonMark). The wrapper text is RAW MARKDOWN — the branch body is later run
 * through the `inline()` markdown pass (see renderTreeHtml) — so backtick spans
 * are plain characters to the tag-aware machinery and must be balanced here.
 */
interface CodeState {
  open: boolean;
  delim: number;
}

const CODE_CLOSED: CodeState = { open: false, delim: 0 };

/**
 * scan_code: advance the inline-code state across RAW. Outside a span, a
 * backslash-escaped backtick (`\``) is a literal, not a delimiter; any other
 * backtick run opens a span of that run's length. Inside a span, a backtick run
 * of the SAME length closes it (a different-length run is literal content, and
 * backslash is not special inside code per CommonMark). Whitespace never joins
 * two backtick runs, so scanning a piece's raw in isolation and carrying the
 * result to the next piece matches scanning the whole body.
 */
function scanCode(raw: string, state: CodeState): CodeState {
  let { open, delim } = state;
  let i = 0;
  const n = raw.length;
  while (i < n) {
    const c = raw.charCodeAt(i);
    if (!open && c === CH_BACKSLASH && raw.charCodeAt(i + 1) === CH_BACKTICK) {
      i += 2;
      continue;
    }
    if (c === CH_BACKTICK) {
      let k = 1;
      while (raw.charCodeAt(i + k) === CH_BACKTICK) k++;
      if (!open) {
        open = true;
        delim = k;
      } else if (k === delim) {
        open = false;
        delim = 0;
      }
      i += k;
      continue;
    }
    i++;
  }
  return { open, delim };
}

/**
 * pack: greedily pack PIECES into lines of at most WIDTH rendered columns. It
 * returns the packed lines plus every word that could not be made to fit, which
 * the caller surfaces rather than silently truncating.
 *
 * REGRESSION WATCH (a wrap break inside an inline-code span must stay BALANCED):
 * the branch body is RAW MARKDOWN and is rendered through the `inline()` pass, so
 * a backtick-delimited code span split across two wrapped lines would otherwise
 * leave the first line with an unclosed span and the continuation with a dangling
 * backtick — `inline()` then renders broken markdown (styling bleeds, a literal
 * backtick shows). So when a break falls while a code span is open, this closes
 * the span at the end of the wrapped line (append the delimiter run) and reopens
 * it at the start of the continuation (prepend the same run), making every
 * emitted line a self-contained, balanced inline-code span. The delimiter
 * backticks count toward a line's rendered width (`visibleWidth`/`textWidth`
 * measure the RAW body, backticks included — consistent with how the branch's
 * own backticks are already measured), so a line reserves room for its closing
 * backticks (`closeW`) and its leading reopen backticks (`reopenW`) and never
 * itself overflows. Triple-backtick FENCED blocks are handled opaquely elsewhere
 * (splitTreeSegments / renderFenceBlock) and never reach here.
 */
function pack(pieces: Piece[], width: number): { lines: string[]; overflows: string[] } {
  const lines: string[] = [];
  const overflows: string[] = [];
  // The inline-code state entering and leaving each piece. `before[i]` is the
  // state at the end of the current line when a break falls before piece i;
  // `after[i]` says whether the line ending with piece i is left open (needs a
  // closing run).
  const before: CodeState[] = [];
  const after: CodeState[] = [];
  let state = CODE_CLOSED;
  for (const piece of pieces) {
    before.push(state);
    state = scanCode(piece.raw, state);
    after.push(state);
  }
  let current = "";
  let currentWidth = 0;
  let placed = false;
  for (let i = 0; i < pieces.length; i++) {
    const piece = pieces[i];
    // Room a line must keep for balancing backticks: `reopenW` if this piece
    // begins a continuation line inside an open span, `closeW` if the line
    // ending with this piece is left open and must be closed.
    const reopenW = before[i].open ? before[i].delim : 0;
    const closeW = after[i].open ? after[i].delim : 0;
    if (placed && currentWidth + 1 + piece.width + closeW > width) {
      // Break: close the current line's open span, then reopen it on the next.
      const closing = before[i].open ? "`".repeat(before[i].delim) : "";
      lines.push(current + piece.breakSuffix + closing);
      const reopen = before[i].open ? "`".repeat(before[i].delim) : "";
      current = piece.breakPrefix + reopen;
      currentWidth = reopen.length;
      placed = false;
    }
    if (placed) {
      current += " ";
      currentWidth++;
    } else if (piece.width + reopenW + closeW > width) {
      overflows.push(stripTags(piece.raw));
    }
    current += piece.raw;
    currentWidth += piece.width;
    placed = true;
  }
  if (placed) lines.push(current);
  if (lines.length === 0) lines.push("");
  return { lines, overflows };
}

/** One output line of a wrapped tree, split so inline markup applies to the
 * body alone and the structural prefix is emitted verbatim. */
export interface RenderLine {
  /** Connectors + label (first line) or continuation indent (wrap lines);
   * empty for a passed-through non-branch line. */
  prefix: string;
  /** The branch text on this line, or the whole line for a raw passthrough. */
  body: string;
  /** True for a non-branch line the wrapper passed through untouched. */
  raw: boolean;
}

/** The exact text of one output line, matching the daemon formatter's output. */
export function lineText(l: RenderLine): string {
  return l.raw ? l.body : (l.prefix + l.body).replace(/\s+$/u, "");
}

/** wrap_branch: render BRANCH as one or more lines, none wider than WIDTH. */
function wrapBranch(
  branch: Branch,
  width: number,
  hasChildren: boolean,
): { lines: RenderLine[]; overflows: string[] } {
  const prefixWidth = textColumn(branch);
  const field = width - prefixWidth;
  if (field <= 0) {
    throw new TreeOverflowError(
      `branch prefix occupies ${prefixWidth} columns, leaving no room within ${width}: ${branch.prefix}${branch.label}`,
    );
  }
  const packed = pack(toPieces(tokenize(branch.body), field), field);
  const indent = continuationIndent(branch, hasChildren);
  const lines: RenderLine[] = [
    { prefix: branch.indent + branch.prefix + branch.label, body: packed.lines[0], raw: false },
  ];
  for (let i = 1; i < packed.lines.length; i++) {
    lines.push({ prefix: indent, body: packed.lines[i], raw: false });
  }
  return { lines, overflows: packed.overflows };
}

// ---------------------------------------------------------------------------
// Block formatting (treefmt: Entry / join_wrapped / format_block)
// ---------------------------------------------------------------------------

interface Entry {
  branch: Branch | null;
  raw: string;
}

/** join_wrapped: collapse already-wrapped branches back into one entry each. */
function joinWrapped(lines: string[]): Entry[] {
  const entries: Entry[] = [];
  for (const line of lines) {
    const branch = parseBranch(line);
    if (branch) {
      entries.push({ branch, raw: line });
      continue;
    }
    const cont = parseContinuation(line);
    const last = entries[entries.length - 1];
    if (cont && last && last.branch) {
      if (cont.column === textColumn(last.branch)) {
        last.branch.body = (last.branch.body + " " + cont.text).trim();
        continue;
      }
    }
    entries.push({ branch: null, raw: line });
  }
  return entries;
}

/**
 * format_block: wrap every branch in one block's lines. Throws
 * TreeOverflowError when a branch's prefix alone exceeds WIDTH — the one
 * condition the formatter refuses. Returns the rendered lines plus every word
 * that could not be made to fit (served wider, never truncated).
 */
export function formatTree(lines: string[], width: number): { lines: RenderLine[]; overflows: string[] } {
  const entries = joinWrapped(lines);
  const out: RenderLine[] = [];
  const overflows: string[] = [];
  for (let i = 0; i < entries.length; i++) {
    const entry = entries[i];
    if (!entry.branch) {
      out.push({ prefix: "", body: entry.raw, raw: true });
      continue;
    }
    let hasChildren = false;
    const next = entries[i + 1];
    if (next && next.branch) hasChildren = isParentOf(entry.branch, next.branch);
    const wrapped = wrapBranch(entry.branch, width, hasChildren);
    out.push(...wrapped.lines);
    overflows.push(...wrapped.overflows);
  }
  return { lines: out, overflows };
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/** A callback the renderer surfaces a wrap issue through (the caller logs it). */
export type TreeIssue = (message: string, context: Record<string, unknown>) => void;

/**
 * Render TEXT as wrapped tree lines at WIDTH columns. Each output line is an
 * `.mp-line` div carrying its structural prefix verbatim and its branch body
 * with INLINE markdown applied (markdown.ts's inline pass, injected rather than
 * imported so this module never depends back on markdown.ts).
 *
 * When a branch's prefix alone exceeds WIDTH the wrapper refuses (the tree is
 * too deep for the width); the issue is surfaced through ONISSUE and the tree is
 * rendered unwrapped rather than not at all, so the reader still sees it.
 */
export function renderTreeHtml(
  text: string,
  inline: (escaped: string) => string,
  width: number,
  onIssue?: TreeIssue,
): string {
  if (!Number.isInteger(width) || width < 1) {
    log.error("a metaprompt tree was handed a column budget that is not a positive whole number", {
      operation: "metaprompt-tree.invalid-width",
      context: { width },
    });
    throw new RangeError(`metaprompt tree: column budget must be a positive integer, got ${String(width)}`);
  }
  const cols = width;
  const segments = splitTreeSegments(text.split("\n"));
  const parts: string[] = [];
  let overflowCount = 0;
  for (const segment of segments) {
    if (segment.kind === "fence") {
      // An interior fenced block is opaque: verbatim, escaped, NOT run through
      // the inline pass or the tree wrapper — no rails, no hanging indent.
      parts.push(renderFenceBlock(segment));
      continue;
    }
    let rendered: RenderLine[];
    try {
      const formatted = formatTree(segment.lines, cols);
      rendered = formatted.lines;
      overflowCount += formatted.overflows.length;
    } catch (err) {
      if (!(err instanceof TreeOverflowError)) throw err;
      if (onIssue) {
        onIssue("a metaprompt tree could not be wrapped to the column limit and is rendered as it arrived", {
          operation: "feed.cards.response.tree-unwrappable",
          width: cols,
          error: err.message,
        });
      }
      rendered = segment.lines.map((line) => ({ prefix: "", body: line, raw: true }));
    }
    parts.push(rendered.map((l) => renderLine(l, inline)).join(""));
  }
  if (onIssue && overflowCount > 0) {
    onIssue("a metaprompt tree holds words wider than the column limit; served wider, never truncated", {
      operation: "feed.cards.response.tree-overflow",
      width: cols,
      overflows: overflowCount,
    });
  }
  return parts.join("");
}

/** A run of tree lines, wrapped and rendered as tree lines. */
interface TreeLineSegment {
  kind: "tree";
  lines: string[];
}

/** An interior fenced code block, rendered opaquely as a code block. */
interface FenceSegment {
  kind: "fence";
  /** The language tag after the opening ``` (may be ""). */
  lang: string;
  /** The dedented code lines between the fence delimiters. */
  code: string[];
}

type TreeSegment = TreeLineSegment | FenceSegment;

/**
 * Split a tree region's lines into runs of tree lines and interior fenced
 * blocks, in order. The fence delimiters themselves are consumed; a fence's
 * body is dedented by the opening delimiter's own indentation so the code reads
 * as a plain block rather than carrying the branch's tree nesting. An
 * unterminated fence takes every remaining line as its body.
 */
function splitTreeSegments(lines: string[]): TreeSegment[] {
  const segments: TreeSegment[] = [];
  let treeLines: string[] = [];
  const flushTree = (): void => {
    if (treeLines.length > 0) {
      segments.push({ kind: "tree", lines: treeLines });
      treeLines = [];
    }
  };
  let i = 0;
  const n = lines.length;
  while (i < n) {
    if (isFenceDelimiter(lines[i])) {
      flushTree();
      const indent = fenceIndent(lines[i]);
      const lang = fenceLanguage(lines[i]);
      const code: string[] = [];
      let j = i + 1;
      while (j < n && !isFenceDelimiter(lines[j])) {
        code.push(dedent(lines[j], indent));
        j++;
      }
      segments.push({ kind: "fence", lang, code });
      // Skip the closing delimiter too, when there is one.
      i = j < n ? j + 1 : j;
      continue;
    }
    treeLines.push(lines[i]);
    i++;
  }
  flushTree();
  return segments;
}

/** The language tag following the ``` of a fence delimiter, or "". */
function fenceLanguage(line: string): string {
  const rest = line.slice(fenceIndent(line) + 3);
  return rest.trim().split(/\s+/)[0] ?? "";
}

/** Strip up to WIDTH leading rail characters (spaces and `│`) from LINE. */
function dedent(line: string, width: number): string {
  return line.slice(Math.min(width, fenceIndent(line)));
}

/**
 * Render an interior fenced block as an opaque code block: the body escaped (or
 * syntax-highlighted for a known language, which also escapes), never run
 * through the markdown/inline pass, so tokens like `__name__` and `->` stay
 * literal. It carries the same `md-code` shape a markdown fence renders as.
 */
function renderFenceBlock(segment: FenceSegment): string {
  const body = segment.code.join("\n");
  const html = highlightCode(body, segment.lang);
  const langClass = segment.lang === "" ? "" : ` lang-${escapeHtml(segment.lang)}`;
  return `<pre class="md-code"><code class="hljs${langClass}">${html}</code></pre>`;
}

function renderLine(l: RenderLine, inline: (escaped: string) => string): string {
  if (l.raw && l.body.trim() === "") return `<div class="mp-line mp-blank"></div>`;
  const content = boxWideChars(inline(escapeHtml(l.body)));
  if (l.raw) {
    return `<div class="mp-line"><span class="mp-content">${content}</span></div>`;
  }
  return `<div class="mp-line"><span class="mp-prefix">${boxWideChars(
    escapeHtml(l.prefix),
  )}</span><span class="mp-content">${content}</span></div>`;
}

/**
 * Draw every character the width model counts as two columns inside a
 * `WIDE_CHAR_CLASS` box, so the drawn width equals the counted width.
 *
 * HTML is the INPUT: only the text between tags is touched, so an attribute or
 * a tag name is never split. A box holds its character plus the zero-width
 * marks that follow it (the emoji variation selector above all), which is the
 * cluster `charWidth` counts as two; a zero-width joiner stays OUTSIDE, so a
 * joined sequence draws as the separate two-column glyphs the model counted.
 */
export function boxWideChars(html: string): string {
  return html
    .split(/(<[^>]*>)/)
    .map((part, i) => (i % 2 === 1 ? part : boxWideText(part)))
    .join("");
}

/** boxWideChars over one run of text that carries no tag. */
function boxWideText(text: string): string {
  const chars = [...text];
  let out = "";
  let i = 0;
  while (i < chars.length) {
    const next = i + 1 < chars.length ? chars[i + 1] : "";
    if (charWidth(chars[i], next === VARIATION_SELECTOR_16) !== 2) {
      out += chars[i];
      i++;
      continue;
    }
    let cluster = chars[i];
    i++;
    while (i < chars.length && chars[i] !== ZERO_WIDTH_JOINER && charWidth(chars[i], false) === 0) {
      cluster += chars[i];
      i++;
    }
    out += `<span class="${WIDE_CHAR_CLASS}">${cluster}</span>`;
  }
  return out;
}
