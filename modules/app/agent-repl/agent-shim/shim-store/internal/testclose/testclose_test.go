package testclose

import (
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

type closer struct{ err error }

func (c closer) Close() error { return c.err }

// recorder captures what OrFail reports without failing the real test.
type recorder struct {
	testing.TB
	errors []string
}

func (r *recorder) Helper() {}

func (r *recorder) Errorf(format string, args ...any) {
	r.errors = append(r.errors, format)
}

func TestOrFail(t *testing.T) {
	tests := []struct {
		name       string
		err        error
		wantErrors int
	}{
		{name: "a clean close reports nothing", err: nil, wantErrors: 0},
		{name: "a failed close fails the test", err: errors.New("boom"), wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			rec := &recorder{TB: t}

			// Act
			OrFail(rec, closer{err: tt.err})

			// Assert
			if len(rec.errors) != tt.wantErrors {
				t.Fatalf("errors reported = %d, want %d", len(rec.errors), tt.wantErrors)
			}
		})
	}
}

// TestNoPackageHandRollsItsOwnCloseHelper keeps every call site on this one
// helper: a package that defines its own copy fails here instead of drifting.
func TestNoPackageHandRollsItsOwnCloseHelper(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	copyDef := regexp.MustCompile(`func\s+closeOrFail\s*\(`)
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if copyDef.Match(src) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walking the module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these test files define their own closeOrFail instead of using testclose.OrFail: %v", offenders)
	}
}
