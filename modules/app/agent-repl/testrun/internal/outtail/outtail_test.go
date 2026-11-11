package outtail

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"path/filepath"
	"strings"
	"testing"
)

func TestOf(t *testing.T) {
	long := strings.Repeat("x", Bytes)
	tests := []struct {
		name   string
		output string
		want   string
	}{
		{name: "output within the bound is whole", output: "a\nb\n", want: "a\nb\n"},
		{name: "a long output is cut at the first line start in the window", output: "early\n" + long[:Bytes-10] + "\nlate line\n", want: "late line\n"},
		{name: "a window with no line start is cut at the bound", output: "y" + long, want: long},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Of([]byte(tt.output))

			// Assert
			if got != tt.want {
				t.Fatalf("Of = %q, want %q", got, tt.want)
			}
			if len(got) > Bytes {
				t.Fatalf("Of = %d bytes, want at most %d", len(got), Bytes)
			}
		})
	}
}

// TestNoTestrunPackageHandRollsAnOutputTail pins that testrun's tails of
// process output go through Of: a function named tail anywhere else in the
// module is a second copy drifting from this one.
func TestNoTestrunPackageHandRollsAnOutputTail(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}
		f, err := parser.ParseFile(token.NewFileSet(), path, nil, 0)
		if err != nil {
			return err
		}
		for _, decl := range f.Decls {
			if fn, ok := decl.(*ast.FuncDecl); ok && fn.Recv == nil && strings.EqualFold(fn.Name.Name, "tail") {
				offenders = append(offenders, path)
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("functions named tail in %v: quote a process's output through outtail.Of", offenders)
	}
}
