package freshinput

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestOfCountsEveryInputTokenThatWasNotACacheHit(t *testing.T) {
	tests := []struct {
		name  string
		usage *conversationv1.TokenUsage
		want  uint64
	}{
		{name: "a nil usage is zero", usage: nil, want: 0},
		{name: "an empty usage is zero", usage: &conversationv1.TokenUsage{}, want: 0},
		{
			name: "uncached input alone counts",
			usage: &conversationv1.TokenUsage{
				InputMisses: &conversationv1.TokenCacheMisses{Unwritten: 7},
			},
			want: 7,
		},
		{
			name: "cache writes count",
			usage: &conversationv1.TokenUsage{
				InputMisses: &conversationv1.TokenCacheMisses{Written: 4_000},
			},
			want: 4_000,
		},
		{
			name: "cache reads never count",
			usage: &conversationv1.TokenUsage{
				InputHits: &conversationv1.TokenCacheHits{Read: 90_000},
			},
			want: 0,
		},
		{
			name: "output never counts",
			usage: &conversationv1.TokenUsage{
				OutputTokens: 1_200, OutputThinkingTokens: 300,
			},
			want: 0,
		},
		{
			name: "every bucket together counts only the misses",
			usage: &conversationv1.TokenUsage{
				InputHits:    &conversationv1.TokenCacheHits{Read: 90_000},
				InputMisses:  &conversationv1.TokenCacheMisses{Written: 4_000, Unwritten: 7},
				OutputTokens: 1_200,
			},
			want: 4_007,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act.
			got := Of(tt.usage)

			// Assert.
			if got != tt.want {
				t.Fatalf("Of() = %d, want %d", got, tt.want)
			}
		})
	}
}

// handRolledSum matches a written/unwritten cache-miss sum spelled out inline:
// either miss read beside a `+`.
var handRolledSum = regexp.MustCompile(`GetWritten\(\)\s*\+|\+\s*[\w.()]*GetUnwritten\(\)|GetUnwritten\(\)\s*\+|\+\s*[\w.()]*GetWritten\(\)`)

func TestNoDaemonSiteHandRollsTheFreshInputSum(t *testing.T) {
	// Arrange: every production source of the daemon's internal packages.
	root := ".."
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(filepath.Dir(path)) == "freshinput" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(src), "\n") {
			if handRolledSum.MatchString(line) {
				offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
			}
		}
		return nil
	})

	// Assert: fresh input is summed in one place, freshinput.Of.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled fresh-input sums (use freshinput.Of): %s", strings.Join(offenders, ", "))
	}
}
