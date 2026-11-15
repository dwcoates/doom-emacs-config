package reposcans

import (
	"bytes"
	"fmt"
	"go/format"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// unformattedGoFiles answers every .go file under root that gofmt would
// rewrite, as paths relative to root and sorted: the list `gofmt -l` prints.
// A node_modules tree is not the module's own source, so the walk skips it.
// A file gofmt cannot parse is an error naming it, never a clean file.
func unformattedGoFiles(root string) ([]string, error) {
	var out []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "node_modules" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(d.Name(), ".go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		formatted, err := format.Source(src)
		if err != nil {
			return fmt.Errorf("gofmt %s: %w", path, err)
		}
		if !bytes.Equal(src, formatted) {
			rel, err := filepath.Rel(root, path)
			if err != nil {
				return err
			}
			out = append(out, filepath.ToSlash(rel))
		}
		return nil
	})
	slices.Sort(out)
	return out, err
}

// TestEveryGoFileIsGofmtClean is the module's gofmt gate: every Go file of
// every system is as gofmt writes it, so formatting drift fails the merge
// gate instead of landing (before 2026-11-14 nothing ran gofmt, and
// unformatted files accumulated across the daemon's packages).
func TestEveryGoFileIsGofmtClean(t *testing.T) {
	// Arrange.
	root := moduleRootDir(t)

	// Act.
	files, err := unformattedGoFiles(root)

	// Assert.
	if err != nil {
		t.Fatalf("scan the module's Go files: %v", err)
	}
	if len(files) > 0 {
		t.Errorf("these Go files are not gofmt-clean; run gofmt -w on each:\n%s", strings.Join(files, "\n"))
	}
}

func TestUnformattedGoFiles(t *testing.T) {
	const clean = "package p\n\nfunc F() {}\n"
	const drifted = "package p\nfunc   F() {}\n"
	tests := []struct {
		name    string
		files   map[string]string
		want    []string
		wantErr string
	}{
		{name: "a clean tree lists nothing", files: map[string]string{"a.go": clean, "sub/b.go": clean}},
		{name: "a drifted file is listed by its relative path", files: map[string]string{"a.go": clean, "sub/b.go": drifted}, want: []string{"sub/b.go"}},
		{name: "every drifted file is listed, sorted", files: map[string]string{"z.go": drifted, "a/y.go": drifted}, want: []string{"a/y.go", "z.go"}},
		{name: "a node_modules tree is skipped", files: map[string]string{"node_modules/x/a.go": drifted}},
		{name: "a file that is not Go is ignored", files: map[string]string{"notes.txt": drifted}},
		{name: "a file gofmt cannot parse is an error naming it", files: map[string]string{"bad.go": "package p\nfunc {\n"}, wantErr: "bad.go"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := t.TempDir()
			for rel, body := range tt.files {
				path := filepath.Join(root, filepath.FromSlash(rel))
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatal(err)
				}
				if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act.
			got, err := unformattedGoFiles(root)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want one naming %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("unformattedGoFiles: %v", err)
			}
			if !slices.Equal(got, tt.want) {
				t.Errorf("got %v, want %v", got, tt.want)
			}
		})
	}
}
