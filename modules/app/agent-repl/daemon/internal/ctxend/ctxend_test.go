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

	"agent-repld/internal/dlog"
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

func TestLogSplitsByWhetherTheContextEnded(t *testing.T) {
	tests := []struct {
		name        string
		err         error
		wantVia     string
		wantMessage string
	}{
		{name: "cancelled", err: context.Canceled, wantVia: "cut", wantMessage: "the call was cut"},
		{name: "wrapped deadline", err: fmt.Errorf("read: %w", context.DeadlineExceeded), wantVia: "cut", wantMessage: "the call was cut"},
		{name: "a failure", err: errors.New("database is locked"), wantVia: "failed", wantMessage: "the call failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			var via, message, operation string
			var fields dlog.Context
			recorder := func(name string) func(string, string, dlog.Context) {
				return func(op, msg string, ctx dlog.Context) { via, operation, message, fields = name, op, msg, ctx }
			}

			// Act.
			Log(tt.err, recorder("cut"), recorder("failed"), "daemon.test", "the call was cut", "the call failed", dlog.Context{"k": "v"})

			// Assert.
			if via != tt.wantVia || message != tt.wantMessage || operation != "daemon.test" || fields["k"] != "v" {
				t.Fatalf("recorded via %q: %q %q %v, want via %q: %q with the fields", via, operation, message, fields, tt.wantVia, tt.wantMessage)
			}
		})
	}
}

// TestNoSiteHandRollsTheLogSplit fails a site that spells out ctxend.Log by
// hand: a one-line cut record and a one-line failed record on the two sides
// of ctxend.Is, each followed by the same return.
func TestNoSiteHandRollsTheLogSplit(t *testing.T) {
	// Arrange.
	record := regexp.MustCompile(`^\s*\S+\.(Info|Debug|Error)\(.*\)$`)
	var offenders []string
	for _, dir := range []string{"..", "../../cmd"} {
		err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return err
			}
			src, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			// Act.
			lines := strings.Split(string(src), "\n")
			for i := 0; i+5 < len(lines); i++ {
				if strings.TrimSpace(lines[i]) != "if ctxend.Is(err) {" {
					continue
				}
				cut, cutReturn, closing, failed, failedReturn := lines[i+1], strings.TrimSpace(lines[i+2]), strings.TrimSpace(lines[i+3]), lines[i+4], strings.TrimSpace(lines[i+5])
				if record.MatchString(cut) && !strings.Contains(cut, ".Error(") && strings.HasPrefix(cutReturn, "return") &&
					closing == "}" && strings.Contains(failed, ".Error(") && record.MatchString(failed) && failedReturn == cutReturn {
					offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
				}
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walking %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these sites spell out ctxend.Log by hand: %v", offenders)
	}
}
