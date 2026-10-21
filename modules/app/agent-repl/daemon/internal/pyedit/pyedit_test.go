package pyedit

import (
	"reflect"
	"strings"
	"testing"
)

func rep(old, new string) Replacement   { return Replacement{Old: old, New: new} }
func first(old, new string) Replacement { return Replacement{Old: old, New: new, First: true} }

func TestParseStatesEdits(t *testing.T) {
	tests := []struct {
		name    string
		command string
		want    []FileEdit
	}{
		{
			name: "the canonical pathlib replace",
			command: "python3 - <<'EOF'\nimport pathlib\np=pathlib.Path('a.go'); s=p.read_text()\n" +
				"old='''x := 1'''\nnew='''x := 2'''\nassert old in s; s=s.replace(old,new); p.write_text(s)\nEOF",
			want: []FileEdit{{Path: "a.go", Replacements: []Replacement{rep("x := 1", "x := 2")}}},
		},
		{
			name:    "a count of one replaces only the first occurrence",
			command: "python3 - <<'EOF'\nfrom pathlib import Path\np = Path(\"a\")\ns = p.read_text()\ns = s.replace('a', 'b', 1)\np.write_text(s)\nEOF",
			want:    []FileEdit{{Path: "a", Replacements: []Replacement{first("a", "b")}}},
		},
		{
			name:    "open() read and write",
			command: "python3 - <<'EOF'\np='f.el'\ns=open(p).read()\ns=s.replace(\"(a)\",\"(b)\")\nopen(p,'w').write(s)\nEOF",
			want:    []FileEdit{{Path: "f.el", Replacements: []Replacement{rep("(a)", "(b)")}}},
		},
		{
			name:    "with blocks around the read and the write",
			command: "python3 - <<'EOF'\nwith open('f') as h:\n    s = h.read()\ns = s.replace('a', 'b')\nwith open('f', 'w') as h:\n    h.write(s)\nEOF",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "b")}}},
		},
		{
			name: "a loop over literal pairs unrolls in order",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\ns = p.read_text()\n" +
				"pairs = [('a', 'A'), ('b', 'B')]\nfor old, new in pairs:\n    assert s.count(old) == 1, old[:40]\n    s = s.replace(old, new)\np.write_text(s)\nEOF",
			want: []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "A"), rep("b", "B")}}},
		},
		{
			name: "a helper with global s",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\ns = p.read_text()\n" +
				"def sub(old, new):\n    global s\n    assert s.count(old) == 1, old\n    s = s.replace(old, new)\nsub('a', 'A')\nsub(new='B', old='b')\np.write_text(s)\nEOF",
			want: []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "A"), rep("b", "B")}}},
		},
		{
			name: "a helper that edits a file per call",
			command: "python3 - <<'EOF'\nfrom pathlib import Path\ndef edit(path, old, new, count=1):\n    p = Path(path); s = p.read_text()\n" +
				"    assert s.count(old) == count, (path, old[:80])\n    p.write_text(s.replace(old, new))\nedit('a', 'x', 'y')\nedit('b', 'u', 'v')\nEOF",
			want: []FileEdit{
				{Path: "a", Replacements: []Replacement{rep("x", "y")}},
				{Path: "b", Replacements: []Replacement{rep("u", "v")}},
			},
		},
		{
			name:    "escapes decode as python decodes them",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a\\tb\\n', 'c\\\\d\\x41\\u00e9\\q'))\nEOF",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("a\tb\n", "c\\dAé\\q")}}},
		},
		{
			name:    "a raw string keeps its backslashes",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace(r'\\d+', r'\\w'))\nEOF",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep(`\d+`, `\w`)}}},
		},
		{
			name:    "adjacent literals and + concatenate",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\nold = ('a'\n       'b') + 'c'\np.write_text(p.read_text().replace(old, 'x'))\nEOF",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("abc", "x")}}},
		},
		{
			name:    "a known string's own replace is computed",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\nold = 'aXa'\nnew = old.replace('a', 'b')\np.write_text(p.read_text().replace(old, new))\nEOF",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("aXa", "bXb")}}},
		},
		{
			name:    "the shell around the here-document is free",
			command: "cd mod && python3 - <<'EOF' && git diff --stat\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a', 'b'))\nEOF\ngo test ./...",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "b")}}},
		},
		{
			name:    "an unquoted delimiter whose body expands nothing",
			command: "python3 <<PY\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a', 'b'))\nPY",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "b")}}},
		},
		{
			name: "two scripts in one command",
			command: "python3 - <<'A'\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a', 'b'))\nA\n" +
				"python3 - <<'B'\nimport pathlib\np = pathlib.Path('g')\np.write_text(p.read_text().replace('c', 'd'))\nB",
			want: []FileEdit{
				{Path: "f", Replacements: []Replacement{rep("a", "b")}},
				{Path: "g", Replacements: []Replacement{rep("c", "d")}},
			},
		},
		{
			name:    "a non-python here-document is ignored",
			command: "cat > notes.txt <<'EOF'\npython3 would be here\nEOF\npython3 - <<'PY'\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a', 'b'))\nPY",
			want:    []FileEdit{{Path: "f", Replacements: []Replacement{rep("a", "b")}}},
		},
		{
			name:    "a replacement by itself changes nothing and states nothing",
			command: "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f')\np.write_text(p.read_text().replace('a', 'a'))\nEOF",
			want:    nil,
		},
		{
			name:    "text replaced but never written states nothing",
			command: "python3 - <<'EOF'\nimport pathlib\ns = pathlib.Path('f').read_text().replace('a', 'b')\nprint(s.count('b'))\nEOF",
			want:    nil,
		},
		{
			name:    "a command without python states nothing",
			command: "go test ./... && git status",
			want:    nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := Parse(tt.command)

			// Assert.
			if err != nil {
				t.Fatalf("Parse: %v", err)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Parse = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestParseDeclines(t *testing.T) {
	const pre = "python3 - <<'EOF'\nimport pathlib, re, sys\np = pathlib.Path('f')\ns = p.read_text()\n"
	const post = "\np.write_text(s)\nEOF"
	tests := []struct {
		name    string
		command string
		reason  string
	}{
		{name: "an if", command: pre + "if 'a' in s:\n    s = s.replace('a', 'b')" + post, reason: `"if" statement`},
		{name: "a regex", command: pre + "s = re.sub('a', 'b', s)" + post, reason: `attribute "sub"`},
		{name: "a positional splice", command: pre + "i = s.index('a')" + post, reason: `method "index"`},
		{name: "an f-string value", command: pre + "x = 1\ns = s.replace('a', f'{x}')" + post, reason: "f-string"},
		{name: "a whole-file write", command: "python3 - <<'EOF'\nimport pathlib\npathlib.Path('f').write_text('all new')\nEOF", reason: "write of something other"},
		{name: "a copy into another file", command: "python3 - <<'EOF'\nimport pathlib\npathlib.Path('g').write_text(pathlib.Path('f').read_text())\nEOF", reason: `"f"'s text into "g"`},
		{name: "a second write to one file", command: pre + "p.write_text(s)" + post, reason: "second write"},
		{name: "an empty old string", command: pre + "s = s.replace('', 'x')" + post, reason: "empty string"},
		{name: "a count other than one", command: pre + "s = s.replace('a', 'b', 2)" + post, reason: "count other than 1"},
		{name: "a sys.exit", command: pre + "sys.exit(0)" + post, reason: `attribute "exit"`},
		{name: "an unbound name", command: pre + "s = s.replace(old, 'b')" + post, reason: `unbound name "old"`},
		{name: "a local read before it is bound", command: pre + "def f():\n    t = s.replace('a', 'b')\n    s = t\nf()" + post, reason: `local "s" read before`},
		{name: "a recursive helper", command: pre + "def f():\n    f()\nf()" + post, reason: "recursive call"},
		{name: "a helper missing an argument", command: pre + "def f(a):\n    pass\nf()" + post, reason: `missing "a"`},
		{name: "an impure print", command: pre + "print(p.write_text(s))" + post, reason: `call to "write_text"`},
		{name: "a print of an unbound name", command: pre + "print(nope)" + post, reason: `unbound name "nope"`},
		{name: "an augmented assignment", command: pre + "s += 'x'" + post, reason: "operator the subset refuses"},
		{name: "a bytes literal", command: pre + "s = s.replace(b'a', b'b')" + post, reason: "bytes literal"},
		{name: "a python script file elsewhere in the command", command: pre + "s = s.replace('a', 'b')" + post + "\npython3 other.py", reason: "only 1 as a bare here-document"},
		{name: "python -c", command: "python3 -c 'print(1)'", reason: "only 0 as a bare here-document"},
		{name: "an unquoted delimiter whose body would expand", command: "python3 - <<EOF\nimport pathlib\np = pathlib.Path('$HOME/f')\nEOF", reason: "would expand"},
		{name: "a here-document never closed", command: "python3 - <<'EOF'\nimport pathlib", reason: "never closed"},
		{name: "a here-document opened inside arithmetic", command: "echo $((1<<3))", reason: "never closed"},
		{name: "an unterminated shell quote", command: "echo 'abc", reason: "unterminated quote"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := Parse(tt.command)

			// Assert.
			if err == nil {
				t.Fatalf("Parse = %+v, want a decline naming %q", got, tt.reason)
			}
			if got != nil {
				t.Fatalf("a declined command still stated %+v", got)
			}
			if !strings.Contains(err.Error(), tt.reason) {
				t.Fatalf("decline = %q, want one naming %q", err, tt.reason)
			}
		})
	}
}

func TestHeredocsIgnoresOperatorsInQuotesAndComments(t *testing.T) {
	tests := []struct {
		name string
		line string
	}{
		{name: "inside single quotes", line: "echo 'a << b'"},
		{name: "inside double quotes", line: `echo "a << b"`},
		{name: "inside a comment", line: "ls # cat <<EOF"},
		{name: "a here-string", line: "cat <<< word"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			docs, _, err := heredocs(tt.line)

			// Assert.
			if err != nil {
				t.Fatalf("heredocs: %v", err)
			}
			if len(docs) != 0 {
				t.Fatalf("heredocs = %+v, want none", docs)
			}
		})
	}
}

func TestHeredocsStripsTabsForTheDashForm(t *testing.T) {
	// Act.
	docs, _, err := heredocs("python3 - <<-'EOF'\n\timport pathlib\n\tEOF")

	// Assert.
	if err != nil {
		t.Fatalf("heredocs: %v", err)
	}
	if len(docs) != 1 || docs[0].body != "import pathlib\n" || docs[0].command != "python3 -" {
		t.Fatalf("heredocs = %+v", docs)
	}
}

func TestUnescape(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
		err  string
	}{
		{name: "no escapes", body: "plain", want: "plain"},
		{name: "the simple escapes", body: `\n\t\r\\\'\"\a\b\f\v`, want: "\n\t\r\\'\"\a\b\f\v"},
		{name: "a hex escape", body: `\x41`, want: "A"},
		{name: "a 16-bit escape", body: `\u00e9`, want: "é"},
		{name: "a 32-bit escape", body: `\U0001F600`, want: "😀"},
		{name: "an octal escape", body: `\101\0`, want: "A\x00"},
		{name: "a line continuation", body: "a\\\nb", want: "ab"},
		{name: "an unknown escape keeps its backslash", body: `\d`, want: `\d`},
		{name: "a truncated hex escape", body: `\x4`, err: "truncated"},
		{name: "a malformed hex escape", body: `\xzz`, err: "malformed"},
		{name: "a named escape", body: `\N{BULLET}`, err: `\N`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := unescape(tt.body)

			// Assert.
			if tt.err != "" {
				if err == nil || !strings.Contains(err.Error(), tt.err) {
					t.Fatalf("unescape(%q) error = %v, want one naming %q", tt.body, err, tt.err)
				}
				return
			}
			if err != nil {
				t.Fatalf("unescape(%q): %v", tt.body, err)
			}
			if got != tt.want {
				t.Fatalf("unescape(%q) = %q, want %q", tt.body, got, tt.want)
			}
		})
	}
}

func TestLexStructure(t *testing.T) {
	// Arrange.
	src := "for a in b:\n    x = 1\n\n    # a comment\n    y = (2,\n         3)\nz = 4\n"

	// Act.
	toks, err := lex(src)

	// Assert.
	if err != nil {
		t.Fatalf("lex: %v", err)
	}
	var kinds []tokKind
	for _, tok := range toks {
		if tok.kind == tokIndent || tok.kind == tokDedent || tok.kind == tokNewline {
			kinds = append(kinds, tok.kind)
		}
	}
	want := []tokKind{tokNewline, tokIndent, tokNewline, tokNewline, tokDedent, tokNewline}
	if !reflect.DeepEqual(kinds, want) {
		t.Fatalf("structure = %v, want %v", kinds, want)
	}
}

func TestLexRejectsADedentToNoLevel(t *testing.T) {
	// Act.
	_, err := lex("for a in b:\n    x = 1\n  y = 2\n")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no enclosing level") {
		t.Fatalf("lex error = %v, want a dedent to no level", err)
	}
}
