package testenv

import (
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestAppendJSONLineAppendsOneLinePerCall(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "lines.jsonl")

	// Act
	for _, v := range []map[string]int{{"a": 1}, {"b": 2}} {
		if err := appendJSONLine(path, "things", v); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	got, err := os.ReadFile(path)
	if err != nil || string(got) != "{\"a\":1}\n{\"b\":2}\n" {
		t.Fatalf("file = %q, %v, want one line per call", got, err)
	}
}

func TestAppendJSONLineRefuses(t *testing.T) {
	tests := []struct {
		name  string
		path  func(t *testing.T) string
		value any
		want  string
	}{
		{"a value that is no JSON", func(t *testing.T) string { return filepath.Join(t.TempDir(), "f") }, make(chan int), "encode a line of the things"},
		{"a path that cannot be opened", func(t *testing.T) string { return filepath.Join(t.TempDir(), "absent", "f") }, 1, "open the things "},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := appendJSONLine(tt.path(t), "things", tt.value)
			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("appendJSONLine = %v, want an error containing %q", err, tt.want)
			}
		})
	}
}

func TestReadJSONLinesNumbersLinesAndStopsAtTheHandlersError(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "lines")
	if err := os.WriteFile(path, []byte("one\ntwo\nthree\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	boom := errors.New("stop")
	var seen []string

	// Act
	ok, err := readJSONLines(path, "things", func(n int, line []byte) error {
		seen = append(seen, string(line))
		if n == 2 {
			return boom
		}
		return nil
	})

	// Assert
	if !ok || !errors.Is(err, boom) || !reflect.DeepEqual(seen, []string{"one", "two"}) {
		t.Fatalf("readJSONLines = (%v, %v) after %v, want to stop at the second line", ok, err, seen)
	}
}

func TestReadJSONLinesCases(t *testing.T) {
	dir := t.TempDir()
	tests := []struct {
		name    string
		path    string
		wantOK  bool
		wantErr string
	}{
		{"a file nobody wrote reads as no lines", filepath.Join(dir, "absent"), false, ""},
		{"a directory cannot be read as a file", dir, true, "read the things"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			ok, err := readJSONLines(tt.path, "things", func(int, []byte) error { return nil })
			// Assert
			if ok != tt.wantOK || (tt.wantErr == "") != (err == nil) || (err != nil && !strings.Contains(err.Error(), tt.wantErr)) {
				t.Fatalf("readJSONLines = (%v, %v), want ok=%v and an error containing %q", ok, err, tt.wantOK, tt.wantErr)
			}
		})
	}
}

// TestEveryJSONLinesFileGoesThroughTheSharedHelpers pins that the files the
// runner and a harness share, line by line (the log record counts, the impact
// windows), are written and read only through appendJSONLine and
// readJSONLines, so a hand-rolled site cannot drift from the others'
// append-mode writes and line reads.
func TestEveryJSONLinesFileGoesThroughTheSharedHelpers(t *testing.T) {
	// Arrange
	forbidden := map[string]bool{"OpenFile": true, "NewScanner": true}
	var offenders []string

	// Act
	for _, file := range []string{"logrecords.go", "impact.go"} {
		f, err := parser.ParseFile(token.NewFileSet(), file, nil, 0)
		if err != nil {
			t.Fatal(err)
		}
		ast.Inspect(f, func(n ast.Node) bool {
			if sel, ok := n.(*ast.SelectorExpr); ok && forbidden[sel.Sel.Name] {
				if id, ok := sel.X.(*ast.Ident); ok && (id.Name == "os" || id.Name == "bufio") {
					offenders = append(offenders, file+" "+id.Name+"."+sel.Sel.Name)
				}
			}
			return true
		})
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled line file access in %v: use appendJSONLine and readJSONLines", offenders)
	}
}
