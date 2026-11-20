package goldenfile

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// recorder is a testing.TB that records what Check reports. Its Fatalf ends
// the goroutine Check runs on, as the real one does.
type recorder struct {
	testing.TB
	errors, fatals []string
}

func (r *recorder) Helper() {}

func (r *recorder) Errorf(format string, args ...any) {
	r.errors = append(r.errors, fmt.Sprintf(format, args...))
}

func (r *recorder) Fatalf(format string, args ...any) {
	r.fatals = append(r.fatals, fmt.Sprintf(format, args...))
	runtime.Goexit()
}

// check runs Check against a recorder and answers what it recorded.
func check(path string, want []byte, update bool) *recorder {
	r := &recorder{}
	done := make(chan struct{})
	go func() {
		defer close(done)
		Check(r, path, want, update)
	}()
	<-done
	return r
}

func TestCheck(t *testing.T) {
	tests := []struct {
		name       string
		existing   *string
		update     bool
		wantErrors int
		wantFatals int
		wantFile   string
	}{
		{name: "a current file passes", existing: ptr("want\n"), wantFile: "want\n"},
		{name: "a stale file fails", existing: ptr("old\n"), wantErrors: 1, wantFile: "old\n"},
		{name: "update rewrites a stale file and passes", existing: ptr("old\n"), update: true, wantFile: "want\n"},
		{name: "update writes a missing file", update: true, wantFile: "want\n"},
		{name: "a missing file is fatal", wantFatals: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "golden.json")
			if tc.existing != nil {
				if err := os.WriteFile(path, []byte(*tc.existing), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			r := check(path, []byte("want\n"), tc.update)

			// Assert
			if len(r.errors) != tc.wantErrors || len(r.fatals) != tc.wantFatals {
				t.Fatalf("errors %q, fatals %q; want %d and %d", r.errors, r.fatals, tc.wantErrors, tc.wantFatals)
			}
			for _, msg := range append(r.errors, r.fatals...) {
				if !strings.Contains(msg, path) {
					t.Fatalf("report %q does not name %s", msg, path)
				}
			}
			if tc.wantFile != "" {
				got, err := os.ReadFile(path)
				if err != nil || string(got) != tc.wantFile {
					t.Fatalf("file = %q (%v), want %q", got, err, tc.wantFile)
				}
			}
		})
	}
}

func ptr(s string) *string { return &s }
