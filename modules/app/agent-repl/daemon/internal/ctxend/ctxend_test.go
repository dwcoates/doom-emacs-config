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
	"agentrepl/logging/logsite"
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
		name         string
		err          error
		cutLevel     string
		wantLevel    string
		wantTemplate string
	}{
		{name: "cancelled", err: context.Canceled, cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelInfo, wantTemplate: "the call was cut"},
		{name: "a debug cut", err: context.Canceled, cutLevel: dlog.LevelDebug, wantLevel: dlog.LevelDebug, wantTemplate: "the call was cut"},
		{name: "wrapped deadline", err: fmt.Errorf("read: %w", context.DeadlineExceeded), cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelInfo, wantTemplate: "the call was cut"},
		{name: "a failure", err: errors.New("database is locked"), cutLevel: dlog.LevelInfo, wantLevel: dlog.LevelError, wantTemplate: "the call failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			Log(tt.err, log, tt.cutLevel, "daemon.test", logsite.Msg("the call was cut"), logsite.Msg("the call failed"), dlog.Context{"k": "v"})

			// Assert.
			records := log.Records()
			if len(records) != 1 {
				t.Fatalf("records = %v, want one", records)
			}
			got := records[0]
			wantSite := logsite.Key(dlog.RuntimeDaemon, "agent-repld/internal/ctxend.TestLogSplitsByWhetherTheContextEnded", tt.wantTemplate)
			if got.Level != tt.wantLevel || got.Template != tt.wantTemplate || got.Operation != "daemon.test" || got.Context["k"] != "v" || got.Site != wantSite {
				t.Fatalf("recorded %+v, want %s %q with the fields, site %s", got, tt.wantLevel, tt.wantTemplate, wantSite)
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
