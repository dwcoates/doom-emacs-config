package pyedit

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"
)

// tokKind is a python token's kind.
type tokKind int

const (
	tokName tokKind = iota
	tokString
	tokNumber
	tokOp
	tokNewline
	tokIndent
	tokDedent
	tokEOF
)

// token is one python token. A string token's text is its DECODED value; its
// raw source rides raw, and fstring marks an f-string, whose value is never
// computed.
type token struct {
	kind    tokKind
	text    string
	raw     string
	fstring bool
	line    int
}

// threeCharOps and twoCharOps are python's multi-character operators, longest
// first so a scan takes the longest match.
var threeCharOps = []string{"...", "**=", "//=", ">>=", "<<="}
var twoCharOps = []string{"==", "!=", "<=", ">=", ":=", "**", "//", "->", "+=", "-=", "*=", "/=", "%=", "|=", "&=", "^=", ">>", "<<", "@="}

// lex tokenizes a python program: names, decoded strings, numbers,
// operators, and the NEWLINE/INDENT/DEDENT structure python's own tokenizer
// emits. Anything it cannot read exactly (a bytes literal, an unknown
// character, an indentation that matches no enclosing level) is an error.
func lex(src string) ([]token, error) {
	var toks []token
	indents := []string{""}
	depth := 0
	line := 1
	atLineStart := true
	i := 0
	for i < len(src) {
		if atLineStart && depth == 0 {
			j := i
			for j < len(src) && (src[j] == ' ' || src[j] == '\t') {
				j++
			}
			// A blank or comment-only line carries no structure.
			if j == len(src) || src[j] == '\n' || src[j] == '#' || src[j] == '\r' {
				for j < len(src) && src[j] != '\n' {
					j++
				}
				if j < len(src) {
					j++
					line++
				}
				i = j
				continue
			}
			indent := src[i:j]
			top := indents[len(indents)-1]
			switch {
			case indent == top:
			case strings.HasPrefix(indent, top):
				indents = append(indents, indent)
				toks = append(toks, token{kind: tokIndent, line: line})
			default:
				for len(indents) > 1 && indents[len(indents)-1] != indent {
					if !strings.HasPrefix(indents[len(indents)-1], indent) {
						return nil, fmt.Errorf("line %d: an inconsistent indentation", line)
					}
					indents = indents[:len(indents)-1]
					toks = append(toks, token{kind: tokDedent, line: line})
				}
				if indents[len(indents)-1] != indent {
					return nil, fmt.Errorf("line %d: a dedent to no enclosing level", line)
				}
			}
			i = j
			atLineStart = false
		}
		c := src[i]
		switch {
		case c == '\n':
			if depth == 0 {
				toks = append(toks, token{kind: tokNewline, line: line})
				atLineStart = true
			}
			line++
			i++
		case c == ' ' || c == '\t' || c == '\r' || c == '\f':
			i++
		case c == '#':
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case c == '\\':
			if i+1 < len(src) && src[i+1] == '\n' {
				i += 2
				line++
				continue
			}
			return nil, fmt.Errorf("line %d: a stray backslash", line)
		case isNameStart(c):
			j := i
			for j < len(src) && isNameChar(src[j]) {
				j++
			}
			word := src[i:j]
			if j < len(src) && (src[j] == singleQuote || src[j] == '"') && isStringPrefix(word) {
				tok, next, lines, err := lexString(src, i, j, word, line)
				if err != nil {
					return nil, err
				}
				toks = append(toks, tok)
				line += lines
				i = next
				continue
			}
			toks = append(toks, token{kind: tokName, text: word, line: line})
			i = j
		case c >= utf8.RuneSelf:
			return nil, fmt.Errorf("line %d: a non-ASCII character outside a string", line)
		case c == singleQuote || c == '"':
			tok, next, lines, err := lexString(src, i, i, "", line)
			if err != nil {
				return nil, err
			}
			toks = append(toks, tok)
			line += lines
			i = next
		case c >= '0' && c <= '9' || c == '.' && i+1 < len(src) && src[i+1] >= '0' && src[i+1] <= '9':
			j := i
			for j < len(src) && (isNameChar(src[j]) || src[j] == '.') {
				j++
			}
			toks = append(toks, token{kind: tokNumber, text: src[i:j], line: line})
			i = j
		default:
			op := ""
			for _, candidate := range threeCharOps {
				if strings.HasPrefix(src[i:], candidate) {
					op = candidate
					break
				}
			}
			if op == "" {
				for _, candidate := range twoCharOps {
					if strings.HasPrefix(src[i:], candidate) {
						op = candidate
						break
					}
				}
			}
			if op == "" {
				if !strings.ContainsRune("()[]{},:.;@=+-*/%<>|&^~", rune(c)) {
					return nil, fmt.Errorf("line %d: an unknown character %q", line, c)
				}
				op = string(c)
			}
			switch op {
			case "(", "[", "{":
				depth++
			case ")", "]", "}":
				depth--
				if depth < 0 {
					return nil, fmt.Errorf("line %d: an unbalanced %q", line, op)
				}
			}
			toks = append(toks, token{kind: tokOp, text: op, line: line})
			i += len(op)
		}
	}
	if depth != 0 {
		return nil, fmt.Errorf("an unclosed bracket")
	}
	if len(toks) > 0 && toks[len(toks)-1].kind != tokNewline && toks[len(toks)-1].kind != tokDedent {
		toks = append(toks, token{kind: tokNewline, line: line})
	}
	for len(indents) > 1 {
		indents = indents[:len(indents)-1]
		toks = append(toks, token{kind: tokDedent, line: line})
	}
	return append(toks, token{kind: tokEOF, line: line}), nil
}

func isNameStart(c byte) bool { return c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' }
func isNameChar(c byte) bool  { return isNameStart(c) || c >= '0' && c <= '9' }

// isStringPrefix is a string literal's prefix: any of r, u, f, b, in either
// case, in the combinations python allows.
func isStringPrefix(word string) bool {
	switch strings.ToLower(word) {
	case "r", "u", "f", "b", "rb", "br", "fr", "rf":
		return true
	}
	return false
}

// lexString reads the string literal whose prefix starts at start and whose
// opening quote is at quote, answering the token, the index past its closing
// quote, and how many newlines it spans.
func lexString(src string, start, quote int, prefix string, line int) (token, int, int, error) {
	lower := strings.ToLower(prefix)
	if strings.Contains(lower, "b") {
		return token{}, 0, 0, fmt.Errorf("line %d: a bytes literal", line)
	}
	raw := strings.Contains(lower, "r")
	fstring := strings.Contains(lower, "f")
	q := src[quote : quote+1]
	delim := q
	if strings.HasPrefix(src[quote:], q+q+q) {
		delim = q + q + q
	}
	bodyStart := quote + len(delim)
	j := bodyStart
	for {
		if j >= len(src) {
			return token{}, 0, 0, fmt.Errorf("line %d: an unterminated string", line)
		}
		if src[j] == '\\' {
			j += 2
			continue
		}
		if strings.HasPrefix(src[j:], delim) {
			break
		}
		if src[j] == '\n' && len(delim) == 1 {
			return token{}, 0, 0, fmt.Errorf("line %d: a newline inside a one-line string", line)
		}
		j++
	}
	body := src[bodyStart:j]
	next := j + len(delim)
	tok := token{kind: tokString, raw: src[start:next], fstring: fstring, line: line}
	switch {
	case fstring:
	case raw:
		tok.text = body
	default:
		decoded, err := unescape(body)
		if err != nil {
			return token{}, 0, 0, fmt.Errorf("line %d: %w", line, err)
		}
		tok.text = decoded
	}
	return tok, next, strings.Count(src[start:next], "\n"), nil
}

// unescape decodes a non-raw python string body's escape sequences exactly as
// python does. An escape python does not know keeps its backslash, as python
// keeps it.
func unescape(body string) (string, error) {
	if !strings.Contains(body, `\`) {
		return body, nil
	}
	var b strings.Builder
	for i := 0; i < len(body); i++ {
		c := body[i]
		if c != '\\' || i+1 == len(body) {
			b.WriteByte(c)
			continue
		}
		i++
		e := body[i]
		switch e {
		case '\n':
		case '\\', singleQuote, '"':
			b.WriteByte(e)
		case 'n':
			b.WriteByte('\n')
		case 't':
			b.WriteByte('\t')
		case 'r':
			b.WriteByte('\r')
		case 'a':
			b.WriteByte('\a')
		case 'b':
			b.WriteByte('\b')
		case 'f':
			b.WriteByte('\f')
		case 'v':
			b.WriteByte('\v')
		case 'x', 'u', 'U':
			width := map[byte]int{'x': 2, 'u': 4, 'U': 8}[e]
			if i+1+width > len(body) {
				return "", fmt.Errorf("a truncated \\%c escape", e)
			}
			n, err := strconv.ParseUint(body[i+1:i+1+width], 16, 32)
			if err != nil {
				return "", fmt.Errorf("a malformed \\%c escape", e)
			}
			if !utf8.ValidRune(rune(n)) {
				return "", fmt.Errorf("an escape naming no character")
			}
			b.WriteRune(rune(n))
			i += width
		case '0', '1', '2', '3', '4', '5', '6', '7':
			j := i
			for j < len(body) && j < i+3 && body[j] >= '0' && body[j] <= '7' {
				j++
			}
			n, err := strconv.ParseUint(body[i:j], 8, 32)
			if err != nil {
				return "", fmt.Errorf("a malformed octal escape: %w", err)
			}
			b.WriteRune(rune(n))
			i = j - 1
		case 'N':
			return "", fmt.Errorf("a \\N{...} escape")
		default:
			b.WriteByte('\\')
			b.WriteByte(e)
		}
	}
	return b.String(), nil
}
