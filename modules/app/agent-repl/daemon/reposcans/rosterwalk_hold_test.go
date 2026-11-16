package reposcans

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// handFoldedPushes answers every non-test Go source under ROOT that folds a
// roster push's streams into what it holds by hand, outside the rosterwalk
// package, as paths relative to ROOT. A hand-rolled fold is the shape
// `held.Workspaces = push.Workspaces`; generated code and node_modules are
// not the module's own source.
func handFoldedPushes(root string) ([]string, error) {
	fold := regexp.MustCompile(`\.(Workspaces|MergeQueue|RecentlyMerged|Closed)\s*=\s*push\.`)
	var out []string
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "node_modules" || d.Name() == "gen" || d.Name() == "rosterwalk" {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if fold.Match(src) {
			rel, err := filepath.Rel(root, path)
			if err != nil {
				return err
			}
			out = append(out, filepath.ToSlash(rel))
		}
		return nil
	})
	return out, err
}

// TestNoGoReaderFoldsAPushByHand fails a Go source that folds a roster push
// into what it holds itself rather than through rosterwalk.Hold: two
// hand-rolled folds drift apart, the way one once held only two streams.
func TestNoGoReaderFoldsAPushByHand(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)

	// Act
	offenders, err := handFoldedPushes(root)

	// Assert
	if err != nil {
		t.Fatalf("scan the module's Go files: %v", err)
	}
	if len(offenders) > 0 {
		t.Errorf("these sources fold a roster push by hand instead of through rosterwalk.Hold:\n%s", strings.Join(offenders, "\n"))
	}
}

func TestHandFoldedPushes(t *testing.T) {
	cases := []struct {
		name string
		src  string
		want []string
	}{
		{"a hand-rolled fold is found", "package x\nfunc f() { held.Closed = push.Closed }\n", []string{"x.go"}},
		{"a fold through Hold is not", "package x\nfunc f() { held = rosterwalk.Hold(held, push) }\n", nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			if err := os.WriteFile(filepath.Join(root, "x.go"), []byte(tc.src), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := handFoldedPushes(root)

			// Assert
			if err != nil {
				t.Fatalf("handFoldedPushes: %v", err)
			}
			if strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("handFoldedPushes = %v, want %v", got, tc.want)
			}
		})
	}
}
