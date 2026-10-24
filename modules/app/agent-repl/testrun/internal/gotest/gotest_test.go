package gotest

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"runtime"
	"strings"
	"testing"
)

func TestIsTestFunc(t *testing.T) {
	src := `package p

import "testing"

func TestA(t *testing.T)      {}
func Test_b(t *testing.T)     {}
func Testable(t *testing.T)   {}
func TestMain(m *testing.M)   {}
func TestNoParam()            {}
func TestTwo(a, b *testing.T) {}
func TestValue(t testing.T)   {}
func FuzzF(f *testing.F)      {}
`
	f, err := parser.ParseFile(token.NewFileSet(), "p_test.go", src, 0)
	if err != nil {
		t.Fatal(err)
	}
	want := map[string]bool{"TestA": true, "Test_b": true, "Testable": false, "TestMain": false, "TestNoParam": false, "TestTwo": false, "TestValue": false}
	for _, d := range f.Decls {
		fn, ok := d.(*ast.FuncDecl)
		if !ok {
			continue
		}
		name := fn.Name.Name
		if name == "FuzzF" {
			if !IsTestFunc(fn, "Fuzz", "F") || IsTestFunc(fn, "Test", "T") {
				t.Errorf("FuzzF is a fuzz test and only that")
			}
			continue
		}
		if got := IsTestFunc(fn, "Test", "T"); got != want[name] {
			t.Errorf("IsTestFunc(%s) = %t, want %t", name, got, want[name])
		}
	}
}

func TestTopPatternSelectsEveryNamedTopAndNoOther(t *testing.T) {
	// Arrange
	re := regexp.MustCompile(TopPattern([]string{"TestA", "TestB.x"}))

	// Act / Assert
	for name, want := range map[string]bool{"TestA": true, "TestB.x": true, "TestBzx": false, "TestAB": false} {
		if re.MatchString(name) != want {
			t.Errorf("TopPattern matches %s = %t, want %t", name, !want, want)
		}
	}
}

func TestResultLineReadsOutcomeNameAndSeconds(t *testing.T) {
	tests := []struct {
		line string
		want []string
	}{
		{line: "--- PASS: TestA (0.01s)", want: []string{"PASS", "TestA", "0.01"}},
		{line: "    --- FAIL: TestA/sub_x (1.50s)", want: []string{"FAIL", "TestA/sub_x", "1.50"}},
		{line: "--- SKIP: TestS (0.00s)", want: []string{"SKIP", "TestS", "0.00"}},
		{line: "=== RUN   TestA", want: nil},
	}
	for _, tt := range tests {
		// Act
		m := ResultLine.FindStringSubmatch(tt.line)

		// Assert
		var got []string
		if m != nil {
			got = m[1:]
		}
		if !reflect.DeepEqual(got, tt.want) {
			t.Errorf("%q read %v, want %v", tt.line, got, tt.want)
		}
	}
}

// TestNoOtherFileReimplementsTheseConventions holds every testrun source to
// the shared helpers: a second result-line regexp, pattern builder or
// test-func check would drift from these.
func TestNoOtherFileReimplementsTheseConventions(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", ".."))
	shapes := map[string]*regexp.Regexp{
		"a -v result-line regexp":      regexp.MustCompile("--- \\(PASS\\|FAIL"),
		"a -test.run alternation":      regexp.MustCompile(`"\^\(" \+ strings\.Join`),
		"a test-func check of its own": regexp.MustCompile(`func isTestFunc\(`),
	}
	var found []string

	// Act
	err := filepath.WalkDir(module, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || path == filepath.Join(module, "internal", "gotest", "gotest.go") {
			return nil
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for what, re := range shapes {
			if re.Match(b) {
				rel, _ := filepath.Rel(module, path)
				found = append(found, rel+": "+what)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(found) > 0 {
		t.Fatalf("use internal/gotest instead:\n%s", strings.Join(found, "\n"))
	}
}

func TestPathPattern(t *testing.T) {
	cases := []struct {
		path string
		want string
	}{
		{path: "TestA", want: "^TestA$"},
		{path: "TestA/a_case/inner", want: "^TestA$/^a_case$/^inner$"},
		{path: "TestA/x.y(1)", want: "^TestA$/^x\\.y\\(1\\)$"},
	}
	for _, tc := range cases {
		t.Run(tc.path, func(t *testing.T) {
			// Act
			got := PathPattern(tc.path)

			// Assert
			if got != tc.want {
				t.Fatalf("PathPattern(%q) = %q, want %q", tc.path, got, tc.want)
			}
		})
	}
}

func TestLiteralAlternation(t *testing.T) {
	// Act
	got := LiteralAlternation([]string{"a.b", "c|d"})

	// Assert
	if got != `(a\.b|c\|d)` {
		t.Fatalf("LiteralAlternation = %q", got)
	}
}
