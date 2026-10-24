/*
 * scanner.c — the three tokens of tree-sitter-tldr that need to look past
 * their own end (grammar.js says what each one means).
 *
 *   HEADING_LABEL  a column-0 dotted label, accepted only when the line says
 *                  it is a label: a dotted label then whitespace or the end, or
 *                  any label then a separator (- – — :) or a marker (at or
 *                  above U+2190, or a code point U+FE0F follows). The token
 *                  is the label alone; what decided it is
 *                  left for the grammar to read.
 *   BRANCH_LABEL   a dotted label after a branch's connector, accepted only
 *                  when whitespace or the end of the line follows it, so
 *                  `├── 3D printing` is text, not label `3`.
 *   LINE_END       a newline, or the end of the text after a non-empty line:
 *                  a response still arriving ends on an unfinished line,
 *                  which is a line.
 *
 * The scanner keeps no state, so it serializes nothing.
 */
#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

enum TokenType {
  HEADING_LABEL,
  BRANCH_LABEL,
  LINE_END,
  ERROR_SENTINEL,
  FENCE_OPEN,
  FENCE_CLOSE,
  FENCE_LINE,
};

/* The smallest backtick run that opens a fence, as in markdown. */
#define FENCE_MIN 3

/* The webapp's EMOJI_FLOOR: every metaprompt root marker sits at or above it. */
#define MARKER_FLOOR 0x2190

/* The emoji presentation selector: a code point below MARKER_FLOOR that it
 * follows opens a marker too (ℹ️, ‼️, ©️, the keycaps), as tldr's scanner
 * reads a root's marker. */
#define EMOJI_PRESENTATION 0xfe0f

static bool is_digit(int32_t c) { return c >= '0' && c <= '9'; }
static bool is_blank(int32_t c) { return c == ' ' || c == '\t'; }
static bool is_separator(int32_t c) {
  return c == '-' || c == ':' || c == 0x2013 /* – */ || c == 0x2014 /* — */;
}

static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

/*
 * Consume a dotted label (`1`, `2.1`, `3.4.5`) and mark its end. A dot is
 * part of the label only when a digit follows it, so `1.` ends at `1`; that
 * trailing dot is peeked past, never part of the token, and makes the label
 * refused: the metaprompt's labels carry no trailing dot, and markdown's
 * ordered lists (`1. First`) do. Answers whether a label was read, and
 * through DOTTED whether it held a dot.
 */
static bool scan_label(TSLexer *lexer, bool *dotted) {
  if (!is_digit(lexer->lookahead)) return false;
  *dotted = false;
  while (is_digit(lexer->lookahead)) advance(lexer);
  lexer->mark_end(lexer);
  while (lexer->lookahead == '.') {
    advance(lexer);
    if (!is_digit(lexer->lookahead)) return false; /* a trailing dot */
    while (is_digit(lexer->lookahead)) advance(lexer);
    lexer->mark_end(lexer);
    *dotted = true;
  }
  return true;
}

static bool at_line_end(TSLexer *lexer) {
  return lexer->eof(lexer) || lexer->lookahead == '\n' || lexer->lookahead == '\r';
}

static bool scan_heading_label(TSLexer *lexer) {
  if (lexer->get_column(lexer) != 0) return false;
  bool dotted = false;
  if (!scan_label(lexer, &dotted)) return false;
  /* Past mark_end now: everything below only peeks. */
  if (dotted && (at_line_end(lexer) || is_blank(lexer->lookahead))) return true;
  while (is_blank(lexer->lookahead)) advance(lexer);
  if (is_separator(lexer->lookahead)) return true;
  if (lexer->lookahead >= MARKER_FLOOR) return true;
  if (at_line_end(lexer)) return false;
  advance(lexer);
  return lexer->lookahead == EMOJI_PRESENTATION;
}

static bool scan_branch_label(TSLexer *lexer) {
  bool dotted = false;
  if (!scan_label(lexer, &dotted)) return false;
  return at_line_end(lexer) || is_blank(lexer->lookahead);
}

/*
 * The end of the text ends a line only when the line holds something: an
 * empty line at the end is no line at all, which also keeps the zero-width
 * token from matching again and again in the same place.
 */
static bool scan_line_end(TSLexer *lexer) {
  if (lexer->eof(lexer)) return lexer->get_column(lexer) != 0;
  if (lexer->lookahead == '\r') advance(lexer);
  if (lexer->lookahead != '\n') return false;
  advance(lexer);
  lexer->mark_end(lexer);
  return true;
}

/*
 * THE FENCE. The scanner's one piece of state is the open fence's backtick
 * count (0 when no fence is open), because markdown closes a fence only on a
 * run at least as long as the one that opened it, with nothing after it but
 * blanks. A shorter run, or a run with text after it (```yaml inside a ````
 * fence), is a line of the fence. The run may follow rails or blanks, as the
 * fence a tree's branch carries does.
 */
typedef struct {
  uint8_t open;
} Scanner;

static bool is_rail(int32_t c) { return is_blank(c) || c == 0x2502 /* │ */; }
static bool is_rail_or_backtick(int32_t c) { return is_rail(c) || c == '`'; }

static unsigned count_backticks(TSLexer *lexer) {
  unsigned n = 0;
  while (lexer->lookahead == '`') {
    advance(lexer);
    n++;
  }
  return n;
}

/* A fence's own line: its close, or one of its lines. A blank line is not
 * claimed here, so the grammar's own blank token takes it. */
static bool scan_inside_fence(Scanner *scanner, TSLexer *lexer, const bool *valid_symbols) {
  bool content = false;
  while (is_rail(lexer->lookahead)) {
    if (!is_blank(lexer->lookahead)) content = true;
    advance(lexer);
  }
  unsigned run = count_backticks(lexer);
  if (run > 0) content = true;
  if (run >= scanner->open && valid_symbols[FENCE_CLOSE]) {
    while (is_blank(lexer->lookahead)) advance(lexer);
    if (at_line_end(lexer)) {
      lexer->mark_end(lexer);
      lexer->result_symbol = FENCE_CLOSE;
      scanner->open = 0;
      return true;
    }
  }
  if (!valid_symbols[FENCE_LINE]) return false;
  while (!at_line_end(lexer)) {
    if (!is_blank(lexer->lookahead)) content = true;
    advance(lexer);
  }
  if (!content) return false;
  lexer->mark_end(lexer);
  lexer->result_symbol = FENCE_LINE;
  return true;
}

/* A fence's opening line: rails or blanks, at least FENCE_MIN backticks, and
 * anything after them (its info string) to the end of the line. */
static bool scan_fence_open(Scanner *scanner, TSLexer *lexer) {
  while (is_rail(lexer->lookahead)) advance(lexer);
  unsigned run = count_backticks(lexer);
  if (run < FENCE_MIN) return false;
  while (!at_line_end(lexer)) advance(lexer);
  lexer->mark_end(lexer);
  lexer->result_symbol = FENCE_OPEN;
  scanner->open = run > 255 ? 255 : (uint8_t)run;
  return true;
}

void *tree_sitter_tldr_prompt_external_scanner_create(void) {
  Scanner *scanner = calloc(1, sizeof(Scanner));
  return scanner;
}
void tree_sitter_tldr_prompt_external_scanner_destroy(void *payload) { free(payload); }
unsigned tree_sitter_tldr_prompt_external_scanner_serialize(void *payload, char *buffer) {
  buffer[0] = (char)((Scanner *)payload)->open;
  return 1;
}
void tree_sitter_tldr_prompt_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  ((Scanner *)payload)->open = length > 0 ? (uint8_t)buffer[0] : 0;
}

bool tree_sitter_tldr_prompt_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *scanner = payload;
  /* Error recovery marks every symbol valid; no token here is a recovery aid. */
  if (valid_symbols[ERROR_SENTINEL]) return false;
  if (valid_symbols[LINE_END] && at_line_end(lexer)) {
    lexer->result_symbol = LINE_END;
    return scan_line_end(lexer);
  }
  if (scanner->open > 0 && (valid_symbols[FENCE_CLOSE] || valid_symbols[FENCE_LINE])) {
    return scan_inside_fence(scanner, lexer, valid_symbols);
  }
  if (scanner->open == 0 && valid_symbols[FENCE_OPEN] && is_rail_or_backtick(lexer->lookahead)) {
    return scan_fence_open(scanner, lexer);
  }
  if (valid_symbols[HEADING_LABEL] && is_digit(lexer->lookahead)) {
    lexer->result_symbol = HEADING_LABEL;
    return scan_heading_label(lexer);
  }
  if (valid_symbols[BRANCH_LABEL] && is_digit(lexer->lookahead)) {
    lexer->result_symbol = BRANCH_LABEL;
    return scan_branch_label(lexer);
  }
  return false;
}
