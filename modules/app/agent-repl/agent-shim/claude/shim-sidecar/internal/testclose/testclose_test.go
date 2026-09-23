package testclose

import (
	"errors"
	"os"
	"path/filepath"
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
			// Arrange.
			rec := &recorder{TB: t}

			// Act.
			OrFail(rec, closer{err: tt.err})

			// Assert.
			if len(rec.errors) != tt.wantErrors {
				t.Fatalf("errors reported = %d, want %d", len(rec.errors), tt.wantErrors)
			}
		})
	}
}

func TestRemoveOrFail(t *testing.T) {
	tests := []struct {
		name       string
		makePath   func(t *testing.T) string
		wantErrors int
	}{
		{
			name: "removing an existing file reports nothing",
			makePath: func(t *testing.T) string {
				path := filepath.Join(t.TempDir(), "present")
				if err := os.WriteFile(path, []byte("x"), 0o644); err != nil {
					t.Fatalf("writing fixture: %v", err)
				}
				return path
			},
			wantErrors: 0,
		},
		{
			name: "removing a path that never existed reports nothing",
			makePath: func(t *testing.T) string {
				return filepath.Join(t.TempDir(), "absent")
			},
			wantErrors: 0,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			path := tt.makePath(t)
			rec := &recorder{TB: t}

			// Act.
			RemoveOrFail(rec, path)

			// Assert.
			if len(rec.errors) != tt.wantErrors {
				t.Fatalf("errors reported = %d, want %d", len(rec.errors), tt.wantErrors)
			}
		})
	}
}

// TestRemoveOrFailReportsAGenuineFailure drives the one edge case the table
// above cannot: a removal that fails for a reason other than absence. A
// directory containing a file cannot be removed by os.Remove, which is the
// cheapest real failure available without touching permissions.
func TestRemoveOrFailReportsAGenuineFailure(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "child"), []byte("x"), 0o644); err != nil {
		t.Fatalf("writing fixture: %v", err)
	}
	rec := &recorder{TB: t}

	// Act.
	RemoveOrFail(rec, dir)

	// Assert.
	if len(rec.errors) != 1 {
		t.Fatalf("errors reported = %d, want 1", len(rec.errors))
	}
}
