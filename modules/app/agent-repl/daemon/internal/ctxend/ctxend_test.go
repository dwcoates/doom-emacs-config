package ctxend

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

func TestIsNamesOnlyAContextEnding(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want bool
	}{
		{name: "cancelled", err: context.Canceled, want: true},
		{name: "deadline exceeded", err: context.DeadlineExceeded, want: true},
		{name: "wrapped cancellation", err: fmt.Errorf("wsm: commit transaction: %w", context.Canceled), want: true},
		{name: "wrapped deadline", err: fmt.Errorf("outer: %w", fmt.Errorf("inner: %w", context.DeadlineExceeded)), want: true},
		{name: "a cancel cause's context error", err: func() error {
			ctx, cancel := context.WithCancelCause(context.Background())
			cancel(errors.New("the daemon is exiting"))
			return ctx.Err()
		}(), want: true},
		{name: "a failure", err: errors.New("database is locked"), want: false},
		{name: "no error", err: nil, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := Is(tt.err)
			// Assert.
			if got != tt.want {
				t.Fatalf("Is(%v) = %v, want %v", tt.err, got, tt.want)
			}
		})
	}
}

// handRolled is the classification Is replaces, spelled out at a call site.
var handRolled = regexp.MustCompile(`errors\.Is\((\w+(?:\.\w+)?), context\.Canceled\)\s*\|\|\s*errors\.Is\((\w+(?:\.\w+)?), context\.DeadlineExceeded\)`)

// TestNoSiteHandRollsTheClassification pins that every production site asks
// Is, so no site draws the line between a context ending and a failure
// somewhere else.
func TestNoSiteHandRollsTheClassification(t *testing.T) {
	// Arrange.
	var offenders []string
	for _, dir := range []string{"..", "../../cmd"} {
		err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return err
			}
			if filepath.Base(filepath.Dir(path)) == "ctxend" {
				return nil
			}
			src, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			// Act.
			if handRolled.Match(src) {
				offenders = append(offenders, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walking %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these sites classify a context ending by hand instead of through ctxend.Is: %v", offenders)
	}
}
