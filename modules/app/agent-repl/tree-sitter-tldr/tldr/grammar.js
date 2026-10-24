/**
 * tree-sitter-tldr -- THE PARSER of the metaprompt's TLDR tree, a one-to-one
 * port of the webapp's (webapp/src/metaprompt-tree.ts before it moved here).
 *
 * The scanner (src/scanner.c) does all of the line-level parsing: it reads
 * each line whole and tells the grammar its CLASS (connector, root, header,
 * dotted label, blank, other, or a ``` delimiter) and its BODY (the branch,
 * continuation or raw line parseBranch and parseContinuation made of it).
 * This grammar arranges those lines exactly as findTreeRegion did:
 *
 *   - BEFORE is every line up to the first connector or root line outside a
 *     fence. A fence there is skipped whole: lines inside it are never core.
 *   - The TREE starts at that line, pulling in a `Response (` header that
 *     precedes it across blank lines, and runs over core lines and the blank
 *     lines between them. A closed fence is inside the tree only when a core
 *     line follows it, after any blank lines; any other line ends the tree.
 *   - A tree holds at least two core lines. When the first run of core lines
 *     holds just one, the text has no tree at all.
 *   - AFTER is everything past the tree's last line.
 *
 * THE GRAMMAR IS UNAMBIGUOUS. Where a line could either continue the tree or
 * end it, the parser tries both, and the lines that follow always rule one
 * out: AFTER can only begin on a line that ends the tree (or a fence that no
 * core line follows), and BEFORE can never end on a header that a core line
 * follows across blanks, so that header is the tree's. A parse that kept two
 * readings would let tree-sitter choose between them locally, where they meet,
 * and the choice would not always be findTreeRegion's.
 *
 * Generated at ABI 14 (`npm run generate`): Emacs 30 loads ABI 13 to 14 only.
 */

module.exports = grammar({
  name: 'tldr',

  extras: _ => [],

  externals: $ => [
    $._open_mark,
    $._close_mark,
    $._content_mark,
    $._connector_mark,
    $._root_mark,
    $._header_mark,
    $._dotted_mark,
    $._blank_mark,
    $._other_mark,
    $._branch_mark,
    $._continuation_mark,
    $._raw_mark,
    $.indent,
    $.prefix,
    $.label,
    $.padding,
    $.text,
    $._lead,
    $._trail,
    $.rails,
    $.ticks,
    $.language,
    $.info,
    $.dedent,
    $._line_end,
    $._error_sentinel,
  ],

  conflicts: $ => [
    [$._tree_headed],
    [$._tree_bare],
    [$._tree_headed, $._lone_headed],
    [$._before_core, $._quiet],
    [$._header_lead, $._quiet],
    [$.document, $._before_core],
    [$._tree_bare, $._lone_bare],
  ],

  rules: {
    // The text before the tree may end anywhere when the tree opens on its
    // own header (that header is the nearest non-blank line to the tree's
    // first core line), and must not end on a header and blank lines when it
    // does not (that header would be the tree's).
    document: $ => choice(
      seq(
        optional(alias($._before_any, $.before)),
        choice(alias($._tree_headed, $.tree), $._lone_headed),
        optional($.after),
      ),
      seq(
        optional(alias($._before_core, $.before)),
        choice(alias($._tree_bare, $.tree), $._lone_bare),
        optional($.after),
      ),
      optional(alias($._before_any, $.before)),
    ),

    // Before a core line the tree does not head: never ending on a header and
    // blank lines.
    _before_core: $ => choice(
      repeat1($.blank_line),
      seq(repeat($._quiet), choice($.label_line, $.other_line, $.fence), repeat($.blank_line)),
    ),

    _before_any: $ => repeat1($._quiet),

    _tree_headed: $ => seq($._header_lead, $._core, repeat1($._step)),
    _tree_bare: $ => seq($._core, repeat1($._step)),

    // The first run of core lines holds one: the text has no tree.
    _lone_headed: $ => seq($._header_lead, $._core),
    _lone_bare: $ => $._core,

    _header_lead: $ => seq($.header_line, repeat($.blank_line)),

    _step: $ => seq(
      repeat($.blank_line),
      optional(seq(alias($._closed_fence, $.fence), repeat($.blank_line))),
      $._core,
    ),

    // Everything past the tree. It opens on what ended the tree: blank lines
    // to the end of the text, or blank lines and then a non-core line, an
    // unclosed fence, or a closed fence that no core line follows (the end of
    // the text, or a non-core line or fence, after any blank lines).
    after: $ => choice(
      repeat1($.blank_line),
      seq(repeat($.blank_line), choice($.header_line, $.label_line, $.other_line), repeat($._any)),
      seq(repeat($.blank_line), alias($._open_fence, $.fence)),
      seq(repeat($.blank_line), alias($._closed_fence, $.fence), repeat($.blank_line)),
      seq(
        repeat($.blank_line),
        alias($._closed_fence, $.fence),
        repeat($.blank_line),
        choice($.header_line, $.label_line, $.other_line, $.fence),
        repeat($._any),
      ),
    ),

    _any: $ => choice($._core, $._quiet),

    _core: $ => choice($.connector_line, $.root_line),

    _quiet: $ => choice($.blank_line, $.header_line, $.label_line, $.other_line, $.fence),

    connector_line: $ => seq($._connector_mark, $._body, $._line_end),
    root_line: $ => seq($._root_mark, $._body, $._line_end),
    header_line: $ => seq($._header_mark, $._body, $._line_end),
    label_line: $ => seq($._dotted_mark, $._body, $._line_end),
    blank_line: $ => seq($._blank_mark, $._body, $._line_end),
    other_line: $ => seq($._other_mark, $._body, $._line_end),

    _body: $ => choice($.branch, $.continuation, $.raw),

    branch: $ => seq(
      $._branch_mark,
      optional($.indent),
      optional($.prefix),
      optional($.label),
      optional($._lead),
      optional($.text),
      optional($._trail),
    ),

    continuation: $ => seq(
      $._continuation_mark,
      optional($.indent),
      optional($.prefix),
      optional($.padding),
      optional($.text),
      optional($._trail),
    ),

    raw: $ => seq($._raw_mark, optional($.text)),

    // A fence runs from its open delimiter to its close, or to the end of the
    // text when none closes it (splitTreeSegments' pairing).
    fence: $ => seq($.fence_open, repeat($.fence_line), optional($.fence_close)),

    _closed_fence: $ => seq($.fence_open, repeat($.fence_line), $.fence_close),

    _open_fence: $ => seq($.fence_open, repeat($.fence_line)),

    fence_open: $ => seq($._open_mark, $._delimiter),
    fence_close: $ => seq($._close_mark, $._delimiter),

    _delimiter: $ => seq(
      optional($.rails),
      $.ticks,
      optional($._lead),
      optional($.language),
      optional($.info),
      $._line_end,
    ),

    // A line inside a fence keeps its class, which isMetapromptTree counted
    // whatever the fences; its text is dedented by the open delimiter's rail.
    fence_line: $ => seq(
      $._content_mark,
      field('class', choice(
        alias($._connector_mark, $.connector),
        alias($._root_mark, $.root),
        alias($._header_mark, $.header),
        alias($._dotted_mark, $.dotted),
        alias($._blank_mark, $.blank),
        alias($._other_mark, $.other),
      )),
      optional($.dedent),
      optional($.text),
      $._line_end,
    ),
  },
});
