package paint

import (
	"strings"
)

// The classes the highlighter may emit. New checks every one against the
// inventory, so a class that drifts out of paint-classes.json fails at
// construction rather than as an unstyled span nobody notices.
var highlightClasses = []string{
	"keyword",
	"string",
	"comment",
	"number",
	"type",
	"function",
	"operator",
	"punctuation",
	"variable",
	"constant",
	"attribute",
	"heading",
	"link",
	"emphasis",
	"strong",
}

// funcRule says how a language marks a call site, which is the only way this
// highlighter tells a function from any other identifier.
type funcRule int

const (
	// funcNone: the language gets no function class.
	funcNone funcRule = iota
	// funcBeforeParen: an identifier immediately followed by '(' is a call.
	funcBeforeParen
	// funcAfterParen: the head of a parenthesized form is a call (lisp).
	funcAfterParen
)

// langSpec is one language's lexical table. The highlighter is deliberately
// one generic scanner over these tables: the class vocabulary is
// language-neutral, so the per-language part is data, not code.
type langSpec struct {
	lineComments []string
	blockOpen    string
	blockClose   string
	// quotes are backslash-escaping string delimiters.
	quotes string
	// rawQuotes are delimiters with no escape processing.
	rawQuotes string
	keywords  map[string]bool
	types     map[string]bool
	constants map[string]bool
	// identExtra are the non-alphanumeric bytes that continue an identifier.
	identExtra  string
	operators   string
	punctuation string
	funcRule    funcRule
	// dollarVariables paints `$name` and `${name}` as a variable (shells).
	dollarVariables bool
	// colonKeywords paints a leading-colon symbol as a constant (elisp).
	colonKeywords bool
	// decorators paints a leading-'@' word as an attribute (python).
	decorators bool
}

func set(words ...string) map[string]bool {
	m := make(map[string]bool, len(words))
	for _, w := range words {
		m[w] = true
	}
	return m
}

var goSpec = &langSpec{
	lineComments: []string{"//"},
	blockOpen:    "/*", blockClose: "*/",
	quotes: "\"'", rawQuotes: "`",
	keywords: set("break", "case", "chan", "const", "continue", "default", "defer", "else",
		"fallthrough", "for", "func", "go", "goto", "if", "import", "interface", "map",
		"package", "range", "return", "select", "struct", "switch", "type", "var"),
	types: set("bool", "byte", "complex64", "complex128", "error", "float32", "float64",
		"int", "int8", "int16", "int32", "int64", "rune", "string", "uint", "uint8",
		"uint16", "uint32", "uint64", "uintptr", "any"),
	constants:   set("true", "false", "nil", "iota"),
	identExtra:  "_",
	operators:   "+-*/%=<>!&|^~:",
	punctuation: "(){}[],;.",
	funcRule:    funcBeforeParen,
}

var tsSpec = &langSpec{
	lineComments: []string{"//"},
	blockOpen:    "/*", blockClose: "*/",
	quotes: "\"'", rawQuotes: "`",
	keywords: set("abstract", "as", "async", "await", "break", "case", "catch", "class",
		"const", "continue", "debugger", "declare", "default", "delete", "do", "else",
		"enum", "export", "extends", "finally", "for", "from", "function", "get", "if",
		"implements", "import", "in", "instanceof", "interface", "keyof", "let", "new",
		"of", "private", "protected", "public", "readonly", "return", "satisfies", "set",
		"static", "super", "switch", "this", "throw", "try", "type", "typeof", "var",
		"void", "while", "yield"),
	types: set("any", "bigint", "boolean", "never", "number", "object", "string", "symbol",
		"unknown", "Array", "Promise", "Record", "Map", "Set"),
	constants:   set("true", "false", "null", "undefined", "NaN", "Infinity"),
	identExtra:  "_$",
	operators:   "+-*/%=<>!&|^~?:",
	punctuation: "(){}[],;.",
	funcRule:    funcBeforeParen,
}

var pythonSpec = &langSpec{
	lineComments: []string{"#"},
	quotes:       "\"'",
	keywords: set("and", "as", "assert", "async", "await", "break", "class", "continue",
		"def", "del", "elif", "else", "except", "finally", "for", "from", "global", "if",
		"import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
		"try", "while", "with", "yield", "match", "case"),
	types: set("bool", "bytes", "dict", "float", "frozenset", "int", "list", "set", "str",
		"tuple", "object", "type"),
	constants:   set("True", "False", "None", "Ellipsis", "NotImplemented"),
	identExtra:  "_",
	operators:   "+-*/%=<>!&|^~:@",
	punctuation: "(){}[],;.",
	funcRule:    funcBeforeParen,
	decorators:  true,
}

var shellSpec = &langSpec{
	lineComments: []string{"#"},
	quotes:       "\"'",
	keywords: set("if", "then", "elif", "else", "fi", "for", "while", "until", "do", "done",
		"case", "esac", "in", "function", "select", "time", "return", "break", "continue",
		"local", "export", "readonly", "declare", "typeset", "shift", "set", "unset",
		"source", "trap", "exit", "echo", "cd", "test"),
	constants:       set("true", "false"),
	identExtra:      "_",
	operators:       "+-*/%=<>!&|^~",
	punctuation:     "(){}[];,",
	funcRule:        funcNone,
	dollarVariables: true,
}

var jsonSpec = &langSpec{
	quotes:      "\"",
	constants:   set("true", "false", "null"),
	identExtra:  "_",
	operators:   "",
	punctuation: "{}[],:",
	funcRule:    funcNone,
}

var elispSpec = &langSpec{
	lineComments: []string{";"},
	quotes:       "\"",
	keywords: set("defun", "defmacro", "defvar", "defconst", "defcustom", "defgroup",
		"defface", "define-minor-mode", "define-derived-mode", "lambda", "let", "let*",
		"if", "when", "unless", "cond", "while", "progn", "prog1", "prog2", "setq",
		"setq-default", "setf", "require", "provide", "condition-case", "unwind-protect",
		"catch", "throw", "dolist", "dotimes", "pcase", "cl-defun", "cl-loop", "and",
		"or", "not", "quote", "function", "save-excursion", "with-current-buffer"),
	constants:     set("nil", "t"),
	identExtra:    "-*+/_?!<>=&%:",
	operators:     "",
	punctuation:   "()[]'`,",
	funcRule:      funcAfterParen,
	colonKeywords: true,
}

var protobufSpec = &langSpec{
	lineComments: []string{"//"},
	blockOpen:    "/*", blockClose: "*/",
	quotes: "\"'",
	keywords: set("syntax", "package", "import", "public", "weak", "option", "message",
		"enum", "service", "rpc", "returns", "oneof", "repeated", "optional", "required",
		"reserved", "extend", "extensions", "to", "stream", "group", "map"),
	types: set("double", "float", "int32", "int64", "uint32", "uint64", "sint32", "sint64",
		"fixed32", "fixed64", "sfixed32", "sfixed64", "bool", "string", "bytes"),
	constants:   set("true", "false"),
	identExtra:  "_.",
	operators:   "=",
	punctuation: "(){}[]<>,;",
	funcRule:    funcNone,
}

// specs maps every language name the highlighter answers to its table. A name
// outside this map yields one plain span rather than an error.
var specs = map[string]*langSpec{
	"go":         goSpec,
	"golang":     goSpec,
	"ts":         tsSpec,
	"typescript": tsSpec,
	"tsx":        tsSpec,
	"js":         tsSpec,
	"jsx":        tsSpec,
	"javascript": tsSpec,
	"py":         pythonSpec,
	"python":     pythonSpec,
	"sh":         shellSpec,
	"bash":       shellSpec,
	"zsh":        shellSpec,
	"shell":      shellSpec,
	"json":       jsonSpec,
	"jsonc":      jsonSpec,
	"elisp":      elispSpec,
	"emacs-lisp": elispSpec,
	"lisp":       elispSpec,
	"proto":      protobufSpec,
	"protobuf":   protobufSpec,
}

// markdownLanguages are the names routed to the line-oriented markdown pass.
var markdownLanguages = map[string]bool{"md": true, "markdown": true}

// Highlight turns a code block into spans using the language-neutral highlight
// inventory. An unrecognized language yields one plain span.
func (p *painter) Highlight(language, code string) (Spans, error) {
	name := strings.ToLower(strings.TrimSpace(language))
	if markdownLanguages[name] {
		return p.highlightMarkdown(code)
	}
	spec, ok := specs[name]
	if !ok {
		return p.emit(Spans{}, code, "")
	}
	return p.scan(spec, code)
}

func isDigit(b byte) bool { return b >= '0' && b <= '9' }

func isAlpha(b byte) bool {
	return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z')
}

func (s *langSpec) isIdent(b byte) bool {
	return isAlpha(b) || isDigit(b) || strings.IndexByte(s.identExtra, b) >= 0
}

func (s *langSpec) isIdentStart(b byte) bool {
	return isAlpha(b) || strings.IndexByte(s.identExtra, b) >= 0
}

// scan is the one generic lexer. It walks the code once, emitting a class per
// token and plain text for everything it does not classify.
func (p *painter) scan(spec *langSpec, code string) (Spans, error) {
	spans := Spans{}
	var err error
	add := func(text, class string) bool {
		spans, err = p.emit(spans, text, class)
		return err == nil
	}
	// prevSignificant is the last non-space byte emitted, which the lisp call
	// rule reads.
	prevSignificant := byte(0)

	for i := 0; i < len(code); {
		b := code[i]

		// Line comments.
		if marker := spec.matchLineComment(code, i); marker != "" {
			end := strings.IndexByte(code[i:], '\n')
			if end < 0 {
				end = len(code)
			} else {
				end += i
			}
			if !add(code[i:end], "comment") {
				return nil, err
			}
			i = end
			continue
		}

		// Block comments.
		if spec.blockOpen != "" && strings.HasPrefix(code[i:], spec.blockOpen) {
			end := strings.Index(code[i+len(spec.blockOpen):], spec.blockClose)
			if end < 0 {
				end = len(code)
			} else {
				end = i + len(spec.blockOpen) + end + len(spec.blockClose)
			}
			if !add(code[i:end], "comment") {
				return nil, err
			}
			i = end
			continue
		}

		// Strings.
		if strings.IndexByte(spec.quotes, b) >= 0 {
			end := scanString(code, i, b, true)
			if !add(code[i:end], "string") {
				return nil, err
			}
			prevSignificant = b
			i = end
			continue
		}
		if spec.rawQuotes != "" && strings.IndexByte(spec.rawQuotes, b) >= 0 {
			end := scanString(code, i, b, false)
			if !add(code[i:end], "string") {
				return nil, err
			}
			prevSignificant = b
			i = end
			continue
		}

		// Shell variables.
		if spec.dollarVariables && b == '$' {
			end := i + 1
			if end < len(code) && code[end] == '{' {
				for end < len(code) && code[end] != '}' {
					end++
				}
				if end < len(code) {
					end++
				}
			} else {
				for end < len(code) && (isAlpha(code[end]) || isDigit(code[end]) || code[end] == '_') {
					end++
				}
			}
			if !add(code[i:end], "variable") {
				return nil, err
			}
			prevSignificant = '$'
			i = end
			continue
		}

		// Python decorators.
		if spec.decorators && b == '@' && i+1 < len(code) && spec.isIdentStart(code[i+1]) {
			end := i + 1
			for end < len(code) && (spec.isIdent(code[end]) || code[end] == '.') {
				end++
			}
			if !add(code[i:end], "attribute") {
				return nil, err
			}
			prevSignificant = '@'
			i = end
			continue
		}

		// Numbers.
		if isDigit(b) {
			end := i
			for end < len(code) && (isDigit(code[end]) || isAlpha(code[end]) || code[end] == '.' || code[end] == '_') {
				end++
			}
			if !add(code[i:end], "number") {
				return nil, err
			}
			prevSignificant = '0'
			i = end
			continue
		}

		// Identifiers.
		if spec.isIdentStart(b) {
			end := i
			for end < len(code) && spec.isIdent(code[end]) {
				end++
			}
			word := code[i:end]
			class := spec.classifyWord(word, code, end, prevSignificant)
			if !add(word, class) {
				return nil, err
			}
			prevSignificant = b
			i = end
			continue
		}

		// Operators and punctuation.
		if strings.IndexByte(spec.operators, b) >= 0 {
			if !add(code[i:i+1], "operator") {
				return nil, err
			}
			prevSignificant = b
			i++
			continue
		}
		if strings.IndexByte(spec.punctuation, b) >= 0 {
			if !add(code[i:i+1], "punctuation") {
				return nil, err
			}
			prevSignificant = b
			i++
			continue
		}

		if !add(code[i:i+1], "") {
			return nil, err
		}
		if b != ' ' && b != '\t' && b != '\n' && b != '\r' {
			prevSignificant = b
		}
		i++
	}
	return spans, nil
}

func (s *langSpec) matchLineComment(code string, i int) string {
	for _, marker := range s.lineComments {
		if strings.HasPrefix(code[i:], marker) {
			return marker
		}
	}
	return ""
}

// classifyWord answers the class of one identifier.
func (s *langSpec) classifyWord(word, code string, end int, prevSignificant byte) string {
	if s.colonKeywords && strings.HasPrefix(word, ":") {
		return "constant"
	}
	switch {
	case s.keywords[word]:
		return "keyword"
	case s.constants[word]:
		return "constant"
	case s.types[word]:
		return "type"
	}
	switch s.funcRule {
	case funcBeforeParen:
		if nextNonSpace(code, end) == '(' {
			return "function"
		}
	case funcAfterParen:
		if prevSignificant == '(' {
			return "function"
		}
	}
	return ""
}

func nextNonSpace(code string, i int) byte {
	for ; i < len(code); i++ {
		switch code[i] {
		case ' ', '\t':
		default:
			return code[i]
		}
	}
	return 0
}

// scanString answers the index just past a string literal opened at i. An
// unterminated literal runs to the end of the input, which is what the text
// actually is.
func scanString(code string, i int, quote byte, escapes bool) int {
	j := i + 1
	for j < len(code) {
		if escapes && code[j] == '\\' {
			j += 2
			continue
		}
		if code[j] == quote {
			return j + 1
		}
		j++
	}
	return len(code)
}
