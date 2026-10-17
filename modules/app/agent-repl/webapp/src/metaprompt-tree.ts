/**
 * Metaprompt TLDR-tree wrapping.
 *
 * The metaprompt renders final responses as a numbered Unicode tree
 * (├──/└──/│ connectors plus dotted labels like `2.1`, root nodes
 * emoji-prefixed). The tree arrives UNWRAPPED — one physical line per branch —
 * and this module wraps every branch too wide for the live column limit onto
 * continuation lines, so no line exceeds the width the bubble can show and the
 * tree fills its bubble and re-flows on resize.
 *
 * THIS MODULE WRAPS; IT DOES NOT PARSE. Every tree reaches it already parsed
 * by the daemon, as a frontend.v1.ProseTree: a flat list of nodes, each naming
 * its parent, with its label, its text and any code under it. This module lays
 * that graph out as rows (layoutTree), synthesizing each row's rails from the
 * node's ancestors, and what is left here is the width model and the wrapper.
 *
 * THE WRAP ENGINE IS A ONE-TO-ONE PORT of the daemon's former wrapper
 * (`treefmt`, since deleted): the same packer, the same width model, the same
 * idempotence, the same error surface (its parser is now the daemon's). The
 * wrapping used to run in the daemon at a fixed 105 columns; it runs HERE now,
 * at the live width the bubble measures, because the daemon cannot know how
 * many characters fit a given pixel width and so cannot re-flow on resize.
 * This is the only implementation. Each ported function carries the name it
 * had there.
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

import { codeBlockHtml } from "./code-block.js";
import { escapeHtml } from "./highlight.js";
import { log } from "./log.js";
import type { ProseTree, ProseTreeCode } from "../../proto/gen/ts/frontend/v1/prose_pb";

/**
 * The class of the inline box every double-width character of a tree line is
 * drawn in. The stylesheet makes it exactly `2ch` wide in the tree's own font,
 * which is the two columns `charWidth` counts it as.
 */
export const WIDE_CHAR_CLASS = "mp-wide";

// The two characters the inline-code scanner (scanCode) reads.
const CH_BACKTICK = 0x60;
const CH_BACKSLASH = 0x5c;

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
 * Whitespace inside an inline element, or inside a CLOSED inline-code span,
 * does not separate atoms, so the element stays whole; whitespace outside any
 * element does.
 *
 * A BACKTICK THAT OPENS NO SPAN IS PROSE. An unmatched backtick is a literal
 * character in markdown, so a body whose last span never closes is tokenized
 * with no span at all: kept open, it would swallow every space after it into
 * one atom no line could wrap.
 */
function tokenize(raw: string, markdown: boolean): Atom[] {
  if (!markdown) return tokenizeWith(raw, false).atoms;
  const spans = tokenizeWith(raw, true);
  return spans.balanced ? spans.atoms : tokenizeWith(raw, false).atoms;
}

/** tokenizeWith is tokenize, honoring inline-code spans or not. */
function tokenizeWith(raw: string, honorCode: boolean): { atoms: Atom[]; balanced: boolean } {
  const atoms: Atom[] = [];
  const stack: string[] = [];
  // INLINE CODE KEEPS ITS SPACES. A whitespace run inside an open backtick
  // span is the span's own content, so it stays in the atom rather than
  // splitting it: split, the packer rejoined the pieces with ONE space and
  // `a    b` drew as `a b`. A span wider than the whole field is still broken
  // into subwords (atomSubwords), where its breaks fall.
  let code: CodeState = CODE_CLOSED;
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
      if (isSpace(segment) && !(honorCode && code.open)) {
        flush();
        continue;
      }
      current.runs.push({ raw: segment, isTag: false });
      code = scanCode(segment, code);
    }
  }
  flush();
  return { atoms, balanced: !code.open };
}

// ---------------------------------------------------------------------------
// Branch shape (treefmt: Branch / continuation_prefix / text_column)
// ---------------------------------------------------------------------------

const SEGMENT_WIDTH = 4;

// segment_continuations: what must appear beneath each prefix segment on a
// continuation line. A `├── ` connector means the branch has following
// siblings, so the vertical rule continues past the wrap; a `└── ` connector
// means it does not, so the column goes blank.
const SEGMENT_CONTINUATIONS: Readonly<Record<string, string>> = {
  "│   ": "│   ",
  "    ": "    ",
  "├── ": "│   ",
  "└── ": "    ",
};

/** One drawn branch head: the rails and connector drawn from the graph, the
 * node's label with its trailing space, and its text. */
interface Branch {
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
  return visibleWidth(b.prefix) + visibleWidth(b.label);
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
  return continuationPrefix(b) + padding;
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
 * (the daemon serves each as a node's code, drawn by renderFenceBlock) and never reach here.
 */
function pack(pieces: Piece[], width: number, markdown: boolean): { lines: string[]; overflows: string[] } {
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
    // An HTML body has no backtick spans left to balance: inline() drew them.
    if (markdown) state = scanCode(piece.raw, state);
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
  /** The branch text on this line, or the whole line for a raw passthrough:
   * raw markdown, or the inline HTML when `formatTree` was handed INLINE. */
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
  inline: InlineMarkup | undefined,
): { lines: RenderLine[]; overflows: string[] } {
  const prefixWidth = textColumn(branch);
  const field = width - prefixWidth;
  if (field <= 0) {
    throw new TreeOverflowError(
      `branch prefix occupies ${prefixWidth} columns, leaving no room within ${width}: ${branch.prefix}${branch.label}`,
    );
  }
  const markdown = inline === undefined;
  const body = markdown ? branch.body : inline(escapeHtml(branch.body));
  const packed = pack(toPieces(tokenize(body, markdown), field), field, markdown);
  const indent = continuationIndent(branch, hasChildren);
  const lines: RenderLine[] = [
    { prefix: branch.prefix + branch.label, body: packed.lines[0], raw: false },
  ];
  for (let i = 1; i < packed.lines.length; i++) {
    lines.push({ prefix: indent, body: packed.lines[i], raw: false });
  }
  return { lines, overflows: packed.overflows };
}

// ---------------------------------------------------------------------------
// Layout: the graph as drawn rows (the rails are drawn here, never served)
// ---------------------------------------------------------------------------

/** One drawn row of a tree: a node's head, a passthrough line, or a code block. */
export type TreeRow =
  | { kind: "branch"; branch: Branch; hasChildren: boolean }
  | { kind: "raw"; text: string }
  | { kind: "code"; code: ProseTreeCode };

/** A tree the daemon served that breaks the guarantees ProseTree documents. */
function malformedTree(message: string, context: Record<string, unknown>): never {
  log.error(`a served tree is malformed: ${message}`, { operation: "metaprompt-tree.malformed", context });
  throw new Error(`metaprompt tree: ${message}`);
}

/**
 * layout: TREE as the rows it draws, in reading order. A node's rails are drawn
 * from the graph — each ancestor below the root contributes `│   ` while it
 * still has a sibling below and four spaces once it is the last, and the node's
 * own connector is `├── `, or `└── ` for a last child — so a malformed rail the
 * model wrote is never drawn. Roots are drawn with their marker before their
 * text, a blank line between roots, and the header above the first.
 */
export function layoutTree(tree: ProseTree): TreeRow[] {
  const nodes = tree.nodes;
  const parent: (number | null)[] = [];
  const children: number[][] = nodes.map(() => []);
  nodes.forEach((n, i) => {
    if (n.id?.index !== i) malformedTree("a node's id is not its index", { index: i, id: n.id?.index });
    if (n.label === undefined || n.text === undefined) malformedTree("a node lacks its label or text", { index: i });
    switch (n.place.case) {
      case "root":
        parent.push(null);
        return;
      case "branch": {
        const of = n.place.value.parent?.index;
        if (of === undefined || of >= i) malformedTree("a branch names no earlier parent", { index: i, parent: of });
        parent.push(of);
        children[of].push(i);
        return;
      }
      default:
        malformedTree("a node has no place", { index: i });
    }
  });
  const isLast = (i: number): boolean => {
    const of = parent[i];
    if (of === null) return true;
    const siblings = children[of];
    return siblings[siblings.length - 1] === i;
  };

  const rows: TreeRow[] = [];
  if (tree.header !== undefined) rows.push({ kind: "raw", text: tree.header.text }, { kind: "raw", text: "" });
  let roots = 0;
  nodes.forEach((n, i) => {
    let prefix = "";
    let body = n.text?.markdown ?? "";
    if (n.place.case === "root") {
      if (roots > 0) rows.push({ kind: "raw", text: "" });
      roots++;
      const marker = n.place.value.marker?.text;
      if (marker !== undefined) body = body === "" ? marker : `${marker} ${body}`;
    } else {
      const chain: number[] = [];
      for (let a = parent[i]; a !== null; a = parent[a]) chain.unshift(a);
      for (const a of chain.slice(1)) prefix += isLast(a) ? "    " : "│   ";
      prefix += isLast(i) ? "└── " : "├── ";
    }
    rows.push({
      kind: "branch",
      branch: { prefix, label: `${n.label?.text ?? ""} `, body },
      hasChildren: children[i].length > 0,
    });
    for (const code of n.code) rows.push({ kind: "code", code });
  });
  return rows;
}

/**
 * format_block: wrap every branch row of TREE. Throws TreeOverflowError when a
 * branch's prefix alone exceeds WIDTH — the one condition the formatter
 * refuses. Returns the drawn lines (code blocks are not lines, and are left
 * out) plus every word that could not be made to fit (served wider, never
 * truncated).
 *
 * WITHOUT INLINE, every body is the RAW MARKDOWN it arrived as, wrapped with its
 * backtick spans balanced, which is the daemon formatter's own output. WITH
 * INLINE, every body is drawn to HTML first and wrapped as HTML, so a line is
 * measured by the text it DRAWS: a file link counts as its file name
 * (`fileLinkLabel`), never its path or its `[text](href)` markup, and a link
 * or a bold phrase is an element no wrap can cut into broken markdown.
 */
export function formatTree(
  tree: ProseTree,
  width: number,
  inline?: InlineMarkup,
): { lines: RenderLine[]; overflows: string[] } {
  const out: RenderLine[] = [];
  const overflows: string[] = [];
  for (const row of layoutTree(tree)) {
    if (row.kind === "code") continue;
    if (row.kind === "raw") {
      out.push({ prefix: "", body: inline === undefined ? row.text : inline(escapeHtml(row.text)), raw: true });
      continue;
    }
    const wrapped = wrapBranch(row.branch, width, row.hasChildren, inline);
    out.push(...wrapped.lines);
    overflows.push(...wrapped.overflows);
  }
  return { lines: out, overflows };
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/** markdown.ts's inline pass: already-escaped text in, inline HTML out. */
export type InlineMarkup = (escaped: string) => string;

/** A callback the renderer surfaces a wrap issue through (the caller logs it). */
export type TreeIssue = (message: string, context: Record<string, unknown>) => void;

/**
 * The column budget a tree wraps to, measured from the bubble only when a tree
 * is actually drawn (bubble/body.ts `measureTreeCols`). There is no default.
 */
export type TreeCols = () => number;

/**
 * Render TREE (served as a graph, frontend.v1.ProseTree) as wrapped tree lines
 * at WIDTH columns. Each output line is an `.mp-line` div carrying its
 * structural prefix verbatim and its branch body with INLINE markdown applied
 * (markdown.ts's inline pass, injected rather than imported so this module
 * never depends back on markdown.ts). INLINE runs BEFORE the wrap, so the
 * width a line is wrapped to is the width it draws. A node's code blocks are
 * drawn beneath its head, opaque.
 *
 * When a branch's prefix alone exceeds WIDTH the wrapper refuses (the tree is
 * too deep for the width); the issue is surfaced through ONISSUE and the tree is
 * rendered unwrapped rather than not at all, so the reader still sees it.
 */
export function renderTreeHtml(
  tree: ProseTree,
  inline: InlineMarkup,
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
  const rows = layoutTree(tree);
  const parts: string[] = [];
  let overflowCount = 0;
  let unwrapped = false;
  for (const row of rows) {
    if (row.kind === "code") {
      parts.push(renderFenceBlock(row.code));
      continue;
    }
    if (row.kind === "raw") {
      parts.push(renderLine({ prefix: "", body: inline(escapeHtml(row.text)), raw: true }));
      continue;
    }
    let lines: RenderLine[];
    try {
      if (unwrapped) throw new TreeOverflowError("an earlier branch could not be wrapped");
      const wrapped = wrapBranch(row.branch, width, row.hasChildren, inline);
      lines = wrapped.lines;
      overflowCount += wrapped.overflows.length;
    } catch (err) {
      if (!(err instanceof TreeOverflowError)) throw err;
      if (!unwrapped && onIssue) {
        onIssue("a metaprompt tree could not be wrapped to the column limit and is rendered as it arrived", {
          operation: "feed.cards.response.tree-unwrappable",
          width,
          error: err.message,
        });
      }
      unwrapped = true;
      lines = [{ prefix: row.branch.prefix + row.branch.label, body: inline(escapeHtml(row.branch.body)), raw: false }];
    }
    parts.push(lines.map(renderLine).join(""));
  }
  if (onIssue && overflowCount > 0) {
    onIssue("a metaprompt tree holds words wider than the column limit; served wider, never truncated", {
      operation: "feed.cards.response.tree-overflow",
      width,
      overflows: overflowCount,
    });
  }
  return parts.join("");
}

/**
 * Render a code block drawn beneath a node as an opaque code block: the body
 * escaped (or syntax-highlighted for a known language, which also escapes),
 * never run through the markdown/inline pass, so tokens like `__name__` and
 * `->` stay literal. It is the one code block (code-block.ts) a markdown fence
 * renders as.
 */
function renderFenceBlock(code: ProseTreeCode): string {
  return codeBlockHtml(code.body, code.language ?? "");
}

function renderLine(l: RenderLine): string {
  if (l.raw && l.body.trim() === "") return `<div class="mp-line mp-blank"></div>`;
  const content = boxWideChars(l.body);
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
