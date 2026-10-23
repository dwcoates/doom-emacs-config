/**
 * tree-sitter-tldr — THE ONE GRAMMAR of the metaprompt's TLDR tree and of the
 * prompt entries that answer it.
 *
 * Every program that reads a tree parses it with THIS grammar: the daemon to
 * parse a response's tree (with tldr) and to bind a prompt's
 * entries to the labels of the response they answer, and Emacs (`treesit`) to
 * highlight those entries while the user types. One grammar file generates
 * every parser, so no two programs can disagree on what a label, a branch or
 * an entry is.
 *
 * THE GRAMMAR IS LINE-ORIENTED AND FLAT. It classifies each line and groups the
 * lines under the column-0 HEADING that opens them, and nothing more:
 *
 *   - A HEADING is a line that opens on a dotted label at column 0. In a
 *     response it is a root branch (`1 🔧 text`); in a prompt it is an ENTRY
 *     answering that label (`3.1.2 - text`). The grammar does not know which:
 *     the reader does, from the bubble it parses.
 *   - A SECTION is a heading plus every line up to the next heading. In a
 *     response that is a root and its subtree; in a prompt it is one entry,
 *     however many lines it runs to. The lines before the first heading are
 *     the PREAMBLE.
 *   - A BRANCH is a child line: a prefix of rails ending in a connector
 *     (`│   └── `), then an optional label, then text.
 *   - A CONTINUATION is a line that opens on rails or indentation but no
 *     connector: a wrapped branch's remainder, or an indented line of an entry.
 *   - A FENCE is a ``` block, opaque, optionally drawn behind rails.
 *   - Anything else is PROSE, and a line holding only whitespace is BLANK.
 *
 * NESTING IS NOT PARSED. A branch's depth is its label's (and its prefix's),
 * which every reader already has; a grammar that nested branches would turn a
 * model's slightly misdrawn rail into an ERROR node instead of a branch.
 *
 * WHAT IS NOT A HEADING. A column-0 number is a heading only when the line
 * says it is a label, because prose and markdown open on numbers too
 * (src/scanner.c, `scan_heading_label`):
 *
 *   - `3.1.2 text`, `3.1.2`         a dotted label, then whitespace or the end
 *   - `3 - text`, `3.1: text`       any label, then a separator (- – — :)
 *   - `1 🔧 text`                   any label, then a symbol or emoji marker
 *   - `10 items`, `1. First`, `2026 was` stay prose: a bare number, a markdown
 *     ordered list, a year. `1) First` too: `)` is not a separator, because
 *     markdown reads it as a list.
 *
 * Every line ends at a newline or at the end of the text (`_line_end`), so a
 * response still arriving parses without an error on its last, unfinished line
 * or inside its unclosed fence.
 * Generated at ABI 14 (`npm run generate`): Emacs 30 loads ABI 13 to 14 only.
 */

/** The connector that ends a branch's prefix, Unicode or ASCII. */
const CONNECTOR = /(├──|└──|\|--|\+--|`--)/;

/** One code point, by number, so the grammar's Unicode ranges stay legible. */
const cp = n => String.fromCodePoint(n);

/** The floor of the symbol, arrow and emoji ranges: the webapp's EMOJI_FLOOR. */
const MARKER_FLOOR = cp(0x2190);

/**
 * A root's marker: a symbol or emoji at or above MARKER_FLOOR, or any code
 * point the emoji presentation selector (U+FE0F) follows (ℹ️, ‼️, ©️, the
 * keycaps), with any variation selector, keycap (U+20E3), skin tone (U+1F3FB
 * to U+1F3FF) or zero-width-joined (U+200D) continuation. tldr's scanner and
 * daemon/internal/prose read a marker by the same rule.
 */
const MARKER = new RegExp(
  `[ \\t]*(?:[${MARKER_FLOOR}-${cp(0x10ffff)}]|[^\\s]${cp(0xfe0f)})` +
    `(${cp(0xfe0f)}|${cp(0x20e3)}|${cp(0x200d)}[${MARKER_FLOOR}-${cp(0x10ffff)}]${cp(0xfe0f)}?|[${cp(0x1f3fb)}-${cp(0x1f3ff)}])*`,
  'u',
);

module.exports = grammar({
  name: 'tldr_prompt',

  extras: _ => [],

  externals: $ => [
    $.heading_label,
    $.branch_label,
    $._line_end,
    $._error_sentinel,
  ],

  rules: {
    document: $ => seq(optional($.preamble), repeat($.section)),

    preamble: $ => repeat1($._line),

    section: $ => seq($.heading, repeat($._line)),

    heading: $ => seq(
      alias($.heading_label, $.label),
      optional($.separator),
      optional($.marker),
      optional($.text),
      $._line_end,
    ),

    _line: $ => choice($.branch, $.continuation, $.fence, $.prose, $.blank),

    branch: $ => seq(
      $.prefix,
      optional(alias($.branch_label, $.label)),
      optional($.text),
      $._line_end,
    ),

    // Rails and any stray indent, then exactly one connector and its space.
    // One token: only the webapp's wrapper needs its 4-column segments, and it
    // cuts them itself.
    prefix: _ => token(prec(3, seq(/[ \t│|]*/, CONNECTOR, /[ \t]?/))),

    continuation: $ => seq($.rails, optional($.text), $._line_end),

    rails: _ => token(prec(1, /[ \t│]+/)),

    // An unclosed fence runs to the end of the text, as markdown's does, so a
    // response still arriving mid-block parses without an error.
    // prec.right: a fence keeps every line it can, so once open it holds every
    // line up to its close, never handing one back to the tree. Its own tokens
    // outrank every token that opens a line: inside a fence the lexer also
    // offers the tokens that would follow the fence, and a branch's prefix or a
    // blank line must not end it early.
    fence: $ => prec.right(seq(
      $.fence_open,
      $._line_end,
      repeat(choice(seq($.fence_line, $._line_end), $._fence_blank)),
      optional(seq($.fence_close, $._line_end)),
    )),

    _fence_blank: _ => token(prec(6, /[ \t]*\n/)),

    fence_open: _ => token(prec(4, /[ \t│]*```[^\n]*/)),

    fence_close: _ => token(prec(7, /[ \t│]*```[ \t]*/)),

    fence_line: _ => token(prec(6, /[^\n]+/)),

    separator: _ => token(prec(2, /[ \t]*[-–—:]/)),

    marker: _ => token(prec(2, MARKER)),

    text: _ => token(prec(0, /[^\n]+/)),

    prose: $ => seq(alias(token(prec(-1, /[^\n]+/)), $.text), $._line_end),

    blank: _ => token(prec(2, /[ \t]*\n/)),
  },
});
