package paint

import "testing"

// classOf answers the class the highlighter gave the first span whose text is
// exactly want, or a miss report.
func classOf(t *testing.T, spans Spans, text string) string {
	t.Helper()
	for _, span := range spans {
		if span.Text == text {
			return span.Class
		}
	}
	t.Fatalf("no span carries %q; spans = %+v", text, spans)
	return ""
}

func highlight(t *testing.T, language, code string) Spans {
	t.Helper()
	p := newPainter(t)
	spans, err := p.Highlight(language, code)
	if err != nil {
		t.Fatalf("Highlight: %v", err)
	}
	assertInventory(t, loadClasses(t), spans, code)
	return spans
}

func TestHighlightGo(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "func main() {}", token: "func", want: "keyword"},
		{name: "type", code: "var s string", token: "string", want: "type"},
		{name: "constant", code: "x := nil", token: "nil", want: "constant"},
		{name: "function call", code: "println(1)", token: "println", want: "function"},
		{name: "string literal", code: `s := "hi"`, token: `"hi"`, want: "string"},
		{name: "raw string literal", code: "s := `hi`", token: "`hi`", want: "string"},
		{name: "line comment", code: "// note\nx := 1", token: "// note", want: "comment"},
		{name: "block comment", code: "/* note */ x := 1", token: "/* note */", want: "comment"},
		{name: "number", code: "x := 42", token: "42", want: "number"},
		{name: "operator", code: "a + b", token: "+", want: "operator"},
		{name: "punctuation", code: "f(a, b)", token: ",", want: "punctuation"},
		{name: "plain identifier", code: "alpha = 1", token: "alpha ", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "go", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightTypeScript(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "const x = 1", token: "const", want: "keyword"},
		{name: "type", code: "let n: number = 1", token: "number", want: "type"},
		{name: "constant", code: "let x = undefined", token: "undefined", want: "constant"},
		{name: "function call", code: "render(x)", token: "render", want: "function"},
		{name: "template literal", code: "const s = `hi`", token: "`hi`", want: "string"},
		{name: "single quoted string", code: "const s = 'hi'", token: "'hi'", want: "string"},
		{name: "line comment", code: "// note\nlet x", token: "// note", want: "comment"},
		{name: "dollar identifier", code: "$el = 1", token: "$el ", want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "typescript", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightPython(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "def f():\n    pass", token: "def", want: "keyword"},
		{name: "type", code: "x: int = 1", token: "int", want: "type"},
		{name: "constant", code: "x = None", token: "None", want: "constant"},
		{name: "function call", code: "print(1)", token: "print", want: "function"},
		{name: "hash comment", code: "# note\nx = 1", token: "# note", want: "comment"},
		{name: "decorator", code: "@property\ndef f(): pass", token: "@property", want: "attribute"},
		{name: "string", code: "s = 'hi'", token: "'hi'", want: "string"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "python", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightShell(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "if true; then echo hi; fi", token: "if", want: "keyword"},
		{name: "constant", code: "if true; then :; fi", token: "true", want: "constant"},
		{name: "comment", code: "# note\nls", token: "# note", want: "comment"},
		{name: "bare variable", code: "echo $HOME", token: "$HOME", want: "variable"},
		{name: "braced variable", code: "echo ${HOME}", token: "${HOME}", want: "variable"},
		{name: "double quoted string", code: `echo "hi"`, token: `"hi"`, want: "string"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "bash", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightJSON(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "key", code: `{"a": 1}`, token: `"a"`, want: "string"},
		{name: "number", code: `{"a": 1}`, token: "1", want: "number"},
		{name: "constant", code: `{"a": true}`, token: "true", want: "constant"},
		{name: "punctuation", code: `{"a": 1}`, token: ":", want: "punctuation"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "json", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightElisp(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "(defun f () nil)", token: "defun", want: "keyword"},
		{name: "constant", code: "(defun f () nil)", token: "nil", want: "constant"},
		{name: "colon keyword", code: "(f :key 1)", token: ":key", want: "constant"},
		{name: "call head", code: "(message \"hi\")", token: "message", want: "function"},
		{name: "comment", code: ";; note\n(f)", token: ";; note", want: "comment"},
		{name: "string", code: "(message \"hi\")", token: "\"hi\"", want: "string"},
		{name: "paren", code: "(f)", token: "(", want: "punctuation"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "elisp", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightProtobuf(t *testing.T) {
	tests := []struct {
		name  string
		code  string
		token string
		want  string
	}{
		{name: "keyword", code: "message M { string a = 1; }", token: "message", want: "keyword"},
		{name: "scalar type", code: "message M { string a = 1; }", token: "string", want: "type"},
		{name: "number", code: "message M { string a = 1; }", token: "1", want: "number"},
		{name: "operator", code: "message M { string a = 1; }", token: "=", want: "operator"},
		{name: "comment", code: "// note\nmessage M {}", token: "// note", want: "comment"},
		{name: "string", code: `syntax = "proto3";`, token: `"proto3"`, want: "string"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			spans := highlight(t, "proto", tc.code)

			// Assert.
			if got := classOf(t, spans, tc.token); got != tc.want {
				t.Fatalf("class of %q = %q, want %q", tc.token, got, tc.want)
			}
		})
	}
}

func TestHighlightUnknownLanguageIsOnePlainSpan(t *testing.T) {
	// Arrange.
	code := "SELECT 1 FROM t;"

	// Act.
	spans := highlight(t, "sql", code)

	// Assert.
	if len(spans) != 1 || spans[0].Class != "" {
		t.Fatalf("spans = %+v, want one plain span", spans)
	}
}

func TestHighlightEmptyLanguageIsOnePlainSpan(t *testing.T) {
	// Act.
	spans := highlight(t, "", "anything")

	// Assert.
	if len(spans) != 1 || spans[0].Class != "" {
		t.Fatalf("spans = %+v, want one plain span", spans)
	}
}

func TestHighlightLanguageNameIsCaseInsensitive(t *testing.T) {
	// Act.
	spans := highlight(t, "  Go  ", "func f() {}")

	// Assert.
	if got := classOf(t, spans, "func"); got != "keyword" {
		t.Fatalf("class of func = %q, want keyword", got)
	}
}

func TestHighlightKeepsAnUnterminatedStringAsString(t *testing.T) {
	// Arrange: a literal the block was truncated inside.
	code := `s := "unterminated`

	// Act.
	spans := highlight(t, "go", code)

	// Assert.
	if got := classOf(t, spans, `"unterminated`); got != "string" {
		t.Fatalf("class = %q, want string", got)
	}
}

func TestHighlightKeepsAnUnterminatedBlockCommentAsComment(t *testing.T) {
	// Arrange.
	code := "/* truncated"

	// Act.
	spans := highlight(t, "go", code)

	// Assert.
	if got := classOf(t, spans, code); got != "comment" {
		t.Fatalf("class = %q, want comment", got)
	}
}

func TestHighlightKeepsAnEscapedQuoteInsideAString(t *testing.T) {
	// Arrange.
	code := `s := "a\"b"`

	// Act.
	spans := highlight(t, "go", code)

	// Assert.
	if got := classOf(t, spans, `"a\"b"`); got != "string" {
		t.Fatalf("class = %q, want string", got)
	}
}

func TestHighlightEmitsEveryClassInsideTheInventory(t *testing.T) {
	tests := []struct {
		language string
		code     string
	}{
		{language: "go", code: "package main\n// c\nfunc f(x int) string { return \"a\" }\n"},
		{language: "typescript", code: "export const f = (x: number): string => `a${x}`;\n"},
		{language: "python", code: "@dec\ndef f(x: int) -> str:\n    return 'a'  # c\n"},
		{language: "bash", code: "#!/bin/sh\nfor f in $DIR/*; do echo \"$f\"; done\n"},
		{language: "json", code: "{\"a\": [1, 2.5, true, null]}\n"},
		{language: "markdown", code: "# H\n\n- a **b** _c_ `d` [e](f)\n\n```go\nx := 1\n```\n"},
		{language: "elisp", code: ";; c\n(defun f (x) (message \"a\" :k 1))\n"},
		{language: "proto", code: "syntax = \"proto3\";\nmessage M { repeated string a = 1; }\n"},
	}
	classes := loadClasses(t)
	p := newPainter(t)
	for _, tc := range tests {
		t.Run(tc.language, func(t *testing.T) {
			// Act.
			spans, err := p.Highlight(tc.language, tc.code)

			// Assert.
			if err != nil {
				t.Fatalf("Highlight: %v", err)
			}
			assertInventory(t, classes, spans, tc.code)
		})
	}
}

func TestHighlightRefusesAClassOutsideTheInventory(t *testing.T) {
	// Arrange: an inventory that lost the comment class the code demands.
	classes := loadClasses(t)
	pruned := make([]string, 0, len(classes.Syntax))
	for _, class := range classes.Syntax {
		if class != "comment" {
			pruned = append(pruned, class)
		}
	}
	classes.Syntax = pruned
	p := &painter{classes: classes, ansiRank: classes.ANSIPrecedence}

	// Act.
	_, err := p.Highlight("go", "// note")

	// Assert.
	if err == nil {
		t.Fatal("Highlight emitted a class outside the inventory")
	}
}
