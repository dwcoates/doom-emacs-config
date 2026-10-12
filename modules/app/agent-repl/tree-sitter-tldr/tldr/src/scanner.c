/*
 * scanner.c -- every token of tree-sitter-tldr. The grammar (grammar.js) only
 * arranges the lines this scanner classifies.
 *
 * THIS IS A ONE-TO-ONE PORT of the webapp's line parsing
 * (webapp/src/metaprompt-tree.ts before it moved here): each function below
 * carries the name of the TypeScript function it ports, and answers what that
 * function answered for every line (test/golden/golden.json holds those
 * answers). Nothing here wraps; wrapping stays in the webapp.
 *
 * A LINE IS SCANNED WHOLE, ONCE, AT ITS START. The scanner reads the line to
 * its end, decides everything about it, and queues its tokens; the first is
 * zero-width, so the lexer is back at the line's start when the queue's
 * tokens are handed out, one per call. The queue and the fence state are the
 * scanner's whole state, serialized after every token.
 *
 * Every line's tokens, in order:
 *
 *   a delimiter      OPEN or CLOSE, then [RAILS] TICKS [lead] [LANGUAGE] [INFO]
 *   inside a fence   CONTENT, its CLASS, then [DEDENT] [TEXT]
 *   any other line   its CLASS, then its BODY: one of
 *                      BRANCH        [INDENT] [PREFIX] [LABEL] [lead] [TEXT] [trail]
 *                      CONTINUATION  [INDENT] [PREFIX] [PADDING] [TEXT] [trail]
 *                      RAW           [TEXT]
 *   and last         LINE_END: the newline, or zero-width at the end of text
 *
 * A bracketed token is left out when it would be empty. The CLASS markers are
 * the webapp's line predicates (isConnectorLine, isEmojiRootLine,
 * isHeaderLine, isDottedLabelLine), tested in the order findTreeRegion tests
 * them; the BODY is parseBranch's answer, else parseContinuation's, else the
 * raw line, as joinWrapped asks them.
 */
#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

enum TokenType {
  OPEN_MARK,
  CLOSE_MARK,
  CONTENT_MARK,
  CONNECTOR_MARK,
  ROOT_MARK,
  HEADER_MARK,
  DOTTED_MARK,
  BLANK_MARK,
  OTHER_MARK,
  BRANCH_MARK,
  CONTINUATION_MARK,
  RAW_MARK,
  INDENT,
  PREFIX,
  LABEL,
  PADDING,
  TEXT,
  LEAD,
  TRAIL,
  RAILS,
  TICKS,
  LANGUAGE,
  INFO,
  DEDENT,
  LINE_END,
  ERROR_SENTINEL,
};

/* The most tokens one line queues: a branch's marks, six pieces, its end. */
#define QUEUE_CAPACITY 12

typedef struct {
  uint8_t symbol;
  uint32_t length; /* in code points; LINE_END's is 0 or 1 */
} Step;

typedef struct {
  /* Inside a ``` fence: every delimiter toggles it (findTreeRegion's inFence,
   * splitTreeSegments' pairing). */
  bool in_fence;
  /* The open delimiter's rail width (fenceIndent), which dedents its body. */
  uint32_t open_indent;
  Step queue[QUEUE_CAPACITY];
  uint8_t queued;
  uint8_t next;
  /* The line being planned, as code points. Scratch: never serialized. */
  int32_t *line;
  uint32_t line_capacity;
} Scanner;

/* ---- character classes -------------------------------------------------- */

#define CH_BAR 0x2502   /* │ */
#define CH_TEE 0x251c   /* ├ */
#define CH_ELL 0x2514   /* └ */
#define CH_HORIZ 0x2500 /* ─ */

/* The webapp's EMOJI_FLOOR: every metaprompt root emoji sits at or above it,
 * while ASCII prose after a bare number stays below it. */
#define EMOJI_FLOOR 0x2190

static bool is_digit(int32_t c) { return c >= '0' && c <= '9'; }

/* JavaScript's whitespace, which `\s` and String.prototype.trim match:
 * WhiteSpace (tab, vertical tab, form feed, space, no-break space, the byte
 * order mark, and every Zs character) plus LineTerminator. */
static bool is_js_space(int32_t c) {
  return (c >= 0x09 && c <= 0x0d) || c == 0x20 || c == 0xa0 || c == 0x1680 ||
         (c >= 0x2000 && c <= 0x200a) || c == 0x2028 || c == 0x2029 || c == 0x202f ||
         c == 0x205f || c == 0x3000 || c == 0xfeff;
}

/* ---- the line ------------------------------------------------------------ */

typedef struct {
  const int32_t *cp;
  uint32_t n;
} Line;

/* The code point at I, or -1 past the end (JavaScript's charCodeAt answers
 * NaN there, which every comparison below treats as false). */
static int32_t at(Line l, uint32_t i) { return i < l.n ? l.cp[i] : -1; }

/* line.trim() === "" */
static bool is_blank_line(Line l) {
  for (uint32_t i = 0; i < l.n; i++) {
    if (!is_js_space(l.cp[i])) return false;
  }
  return true;
}

/* isConnectorLine: a leading │/space run, then ├── or └──. */
static bool is_connector_line(Line l) {
  uint32_t i = 0;
  while (i < l.n && (l.cp[i] == ' ' || l.cp[i] == CH_BAR)) i++;
  int32_t c = at(l, i);
  if (c != CH_TEE && c != CH_ELL) return false;
  return at(l, i + 1) == CH_HORIZ && at(l, i + 2) == CH_HORIZ;
}

/* dottedLabelEnd: the index just past a column-0 dotted label, or -1. A dot
 * is consumed only when a digit follows it. */
static int64_t dotted_label_end(Line l) {
  if (!is_digit(at(l, 0))) return -1;
  uint32_t i = 1;
  while (i < l.n) {
    if (is_digit(l.cp[i])) {
      i++;
      continue;
    }
    if (l.cp[i] == '.' && is_digit(at(l, i + 1))) {
      i += 2;
      continue;
    }
    break;
  }
  return i;
}

/* isDottedLabelLine: a dotted label followed by a space or a tab. */
static bool is_dotted_label_line(Line l) {
  int64_t end = dotted_label_end(l);
  if (end < 0) return false;
  int32_t c = at(l, (uint32_t)end);
  return c == ' ' || c == '\t';
}

/* isEmojiRootLine: a dotted label, one space, then an emoji root marker. */
static bool is_emoji_root_line(Line l) {
  int64_t end = dotted_label_end(l);
  if (end < 0) return false;
  if (at(l, (uint32_t)end) != ' ') return false;
  return at(l, (uint32_t)end + 1) >= EMOJI_FLOOR;
}

/* isHeaderLine: the line starts with `Response (`. */
static bool is_header_line(Line l) {
  static const char header[] = "Response (";
  for (uint32_t i = 0; header[i] != '\0'; i++) {
    if (at(l, i) != header[i]) return false;
  }
  return true;
}

/* fenceIndent: the width of the leading rail, spaces and │. */
static uint32_t fence_indent(Line l) {
  uint32_t i = 0;
  while (i < l.n && (l.cp[i] == ' ' || l.cp[i] == CH_BAR)) i++;
  return i;
}

/* isFenceDelimiter: ``` after any leading rail. */
static bool is_fence_delimiter(Line l) {
  uint32_t i = fence_indent(l);
  return at(l, i) == '`' && at(l, i + 1) == '`' && at(l, i + 2) == '`';
}

/* ---- branch parsing (parsePrefix / matchLabel / parseBranch) ------------- */

#define SEGMENT_WIDTH 4

/* Whether the four code points at I are one of SEGMENT_CONTINUATIONS' keys,
 * and through CONNECTOR whether they are one of CONNECTOR_SEGMENTS. */
static bool segment_at(Line l, uint32_t i, bool *connector) {
  if (i + SEGMENT_WIDTH > l.n) return false;
  int32_t a = l.cp[i], b = l.cp[i + 1], c = l.cp[i + 2], d = l.cp[i + 3];
  if (d != ' ') return false;
  *connector = false;
  if (b == ' ' && c == ' ') return a == CH_BAR || a == '|' || a == ' ';
  if (b == CH_HORIZ && c == CH_HORIZ) return *connector = (a == CH_TEE || a == CH_ELL);
  if (b == '-' && c == '-') return *connector = (a == '|' || a == '+' || a == '`');
  return false;
}

/* alignedPrefix: the length of the 4-column tree segments starting at START,
 * stopping after a connector; through CONNECTOR, whether it ended on one. */
static uint32_t aligned_prefix(Line l, uint32_t start, bool *connector) {
  uint32_t position = start;
  *connector = false;
  bool is_connector = false;
  while (segment_at(l, position, &is_connector)) {
    position += SEGMENT_WIDTH;
    if (is_connector) {
      *connector = true;
      break;
    }
  }
  return position - start;
}

/* matchLabel: LABEL_RE, `^(\d+(?:\.\d+)*\.?\s+)`, at START: the label's length
 * with its trailing whitespace, or 0 when there is none. */
static uint32_t match_label(Line l, uint32_t start) {
  uint32_t i = start;
  if (!is_digit(at(l, i))) return 0;
  while (is_digit(at(l, i))) i++;
  while (at(l, i) == '.' && is_digit(at(l, i + 1))) {
    i++;
    while (is_digit(at(l, i))) i++;
  }
  if (at(l, i) == '.') i++;
  if (!is_js_space(at(l, i))) return 0;
  while (is_js_space(at(l, i))) i++;
  return i - start;
}

typedef struct {
  uint32_t indent;
  uint32_t prefix;
  bool connector;
} Prefix;

/* parsePrefix: the MINIMAL leading space/tab run that lets the aligned walk
 * find a connector-terminated prefix or a labelled remainder; else the walk
 * at column 0. */
static Prefix parse_prefix(Line l) {
  for (uint32_t skip = 0; skip <= l.n; skip++) {
    if (skip > 0 && l.cp[skip - 1] != ' ' && l.cp[skip - 1] != '\t') break;
    bool connector = false;
    uint32_t prefix = aligned_prefix(l, skip, &connector);
    if (connector || match_label(l, skip + prefix) > 0) {
      return (Prefix){skip, prefix, connector};
    }
  }
  bool connector = false;
  uint32_t prefix = aligned_prefix(l, 0, &connector);
  return (Prefix){0, prefix, connector};
}

/* ---- planning ------------------------------------------------------------ */

static void push(Scanner *s, enum TokenType symbol, uint32_t length) {
  s->queue[s->queued++] = (Step){(uint8_t)symbol, length};
}

/* Queue SYMBOL over [from, to) only when that span is not empty. */
static void push_span(Scanner *s, enum TokenType symbol, uint32_t from, uint32_t to) {
  if (to > from) push(s, symbol, to - from);
}

/* Queue [lead] TEXT [trail] over [from, to): the span trimmed of JavaScript
 * whitespace at both ends, as String.prototype.trim trims. */
static void push_trimmed(Scanner *s, Line l, uint32_t from, uint32_t to) {
  uint32_t a = from, b = to;
  while (a < b && is_js_space(l.cp[a])) a++;
  while (b > a && is_js_space(l.cp[b - 1])) b--;
  push_span(s, LEAD, from, a);
  push_span(s, TEXT, a, b);
  push_span(s, TRAIL, b, to);
}

/* parseBranch: queue the branch's tokens and answer true, or answer false. */
static bool plan_branch(Scanner *s, Line l) {
  if (is_blank_line(l)) return false;
  Prefix p = parse_prefix(l);
  uint32_t rest = p.indent + p.prefix;
  uint32_t label = match_label(l, rest);
  if (label == 0 && !p.connector) return false;
  push(s, BRANCH_MARK, 0);
  push_span(s, INDENT, 0, p.indent);
  push_span(s, PREFIX, p.indent, rest);
  push_span(s, LABEL, rest, rest + label);
  push_trimmed(s, l, rest + label, l.n);
  return true;
}

/* parseContinuation: queue the continuation's tokens and answer true, or
 * answer false. Its padding is the [ │|] run after the prefix, and its text
 * keeps what follows minus trailing whitespace. */
static bool plan_continuation(Scanner *s, Line l) {
  if (is_blank_line(l)) return false;
  Prefix p = parse_prefix(l);
  if (p.connector) return false;
  uint32_t rest = p.indent + p.prefix;
  uint32_t text = rest;
  while (text < l.n && (l.cp[text] == ' ' || l.cp[text] == CH_BAR || l.cp[text] == '|')) text++;
  if (text == l.n) return false;
  if (match_label(l, text) > 0) return false;
  uint32_t end = l.n;
  while (end > text && is_js_space(l.cp[end - 1])) end--;
  push(s, CONTINUATION_MARK, 0);
  push_span(s, INDENT, 0, p.indent);
  push_span(s, PREFIX, p.indent, rest);
  push_span(s, PADDING, rest, text);
  push_span(s, TEXT, text, end);
  push_span(s, TRAIL, end, l.n);
  return true;
}

/* The line's class: the predicates in findTreeRegion's order. */
static enum TokenType line_class(Line l) {
  if (is_connector_line(l)) return CONNECTOR_MARK;
  if (is_emoji_root_line(l)) return ROOT_MARK;
  if (is_header_line(l)) return HEADER_MARK;
  if (is_blank_line(l)) return BLANK_MARK;
  if (is_dotted_label_line(l)) return DOTTED_MARK;
  return OTHER_MARK;
}

/* A delimiter: [RAILS] TICKS, then the info string, whose first word is the
 * fence's language (fenceLanguage: rest.trim().split(/\s+/)[0]). */
static void plan_delimiter(Scanner *s, Line l, enum TokenType mark) {
  uint32_t rails = fence_indent(l);
  push(s, mark, 0);
  push_span(s, RAILS, 0, rails);
  push(s, TICKS, 3);
  uint32_t lead = rails + 3, word = lead, info = lead;
  while (word < l.n && is_js_space(l.cp[word])) word++;
  info = word;
  while (info < l.n && !is_js_space(l.cp[info])) info++;
  push_span(s, LEAD, lead, word);
  push_span(s, LANGUAGE, word, info);
  push_span(s, INFO, info, l.n);
}

/* Plan the line in S->line: queue every token it is made of. */
static void plan_line(Scanner *s, uint32_t n, bool newline) {
  Line l = {s->line, n};
  s->queued = 0;
  s->next = 0;
  if (is_fence_delimiter(l)) {
    if (s->in_fence) {
      plan_delimiter(s, l, CLOSE_MARK);
      s->in_fence = false;
    } else {
      plan_delimiter(s, l, OPEN_MARK);
      s->in_fence = true;
      s->open_indent = fence_indent(l);
    }
  } else if (s->in_fence) {
    /* dedent: up to the open delimiter's rail width of this line's own rail. */
    uint32_t rail = fence_indent(l);
    uint32_t dedent = rail < s->open_indent ? rail : s->open_indent;
    push(s, CONTENT_MARK, 0);
    push(s, line_class(l), 0);
    push_span(s, DEDENT, 0, dedent);
    push_span(s, TEXT, dedent, n);
  } else {
    push(s, line_class(l), 0);
    if (!plan_branch(s, l) && !plan_continuation(s, l)) {
      push(s, RAW_MARK, 0);
      push_span(s, TEXT, 0, n);
    }
  }
  push(s, LINE_END, newline ? 1 : 0);
}

/* ---- tree-sitter's entry points ------------------------------------------ */

void *tree_sitter_tldr_external_scanner_create(void) {
  Scanner *s = ts_calloc(1, sizeof(Scanner));
  return s;
}

void tree_sitter_tldr_external_scanner_destroy(void *payload) {
  Scanner *s = payload;
  ts_free(s->line);
  ts_free(s);
}

unsigned tree_sitter_tldr_external_scanner_serialize(void *payload, char *buffer) {
  Scanner *s = payload;
  unsigned size = 0;
  buffer[size++] = (char)s->in_fence;
  memcpy(buffer + size, &s->open_indent, sizeof(s->open_indent));
  size += sizeof(s->open_indent);
  buffer[size++] = (char)(s->queued - s->next);
  for (uint8_t i = s->next; i < s->queued; i++) {
    buffer[size++] = (char)s->queue[i].symbol;
    memcpy(buffer + size, &s->queue[i].length, sizeof(uint32_t));
    size += sizeof(uint32_t);
  }
  return size;
}

void tree_sitter_tldr_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  Scanner *s = payload;
  s->in_fence = false;
  s->open_indent = 0;
  s->queued = 0;
  s->next = 0;
  if (length == 0) return;
  unsigned size = 0;
  s->in_fence = buffer[size++];
  memcpy(&s->open_indent, buffer + size, sizeof(s->open_indent));
  size += sizeof(s->open_indent);
  uint8_t remaining = (uint8_t)buffer[size++];
  for (uint8_t i = 0; i < remaining; i++) {
    s->queue[i].symbol = (uint8_t)buffer[size++];
    memcpy(&s->queue[i].length, buffer + size, sizeof(uint32_t));
    size += sizeof(uint32_t);
  }
  s->queued = remaining;
}

/* Read the line at the lexer into S->line, answering its length and, through
 * NEWLINE, whether a newline ends it. The lexer is left past the line. */
static uint32_t read_line(Scanner *s, TSLexer *lexer, bool *newline) {
  uint32_t n = 0;
  while (!lexer->eof(lexer) && lexer->lookahead != '\n') {
    if (n == s->line_capacity) {
      s->line_capacity = s->line_capacity ? s->line_capacity * 2 : 128;
      s->line = ts_realloc(s->line, s->line_capacity * sizeof(int32_t));
    }
    s->line[n++] = lexer->lookahead;
    lexer->advance(lexer, false);
  }
  *newline = !lexer->eof(lexer);
  return n;
}

bool tree_sitter_tldr_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  Scanner *s = payload;
  /* Error recovery marks every symbol valid; no token here is a recovery aid. */
  if (valid_symbols[ERROR_SENTINEL]) return false;
  if (s->next == s->queued) {
    if (lexer->eof(lexer)) return false;
    /* The line's first token is zero-width: mark its end before reading on. */
    lexer->mark_end(lexer);
    bool newline = false;
    uint32_t n = read_line(s, lexer, &newline);
    plan_line(s, n, newline);
    Step first = s->queue[s->next++];
    lexer->result_symbol = first.symbol;
    return true;
  }
  Step step = s->queue[s->next++];
  for (uint32_t i = 0; i < step.length; i++) lexer->advance(lexer, false);
  lexer->mark_end(lexer);
  lexer->result_symbol = step.symbol;
  return true;
}
