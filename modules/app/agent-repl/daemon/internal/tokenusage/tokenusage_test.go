package tokenusage

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

// usage builds a usage report: read, written, unwritten, output, thinking.
func usage(read, written, unwritten, output, thinking uint64, provisional bool) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputHits:            &conversationv1.TokenCacheHits{Read: read},
		InputMisses:          &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
		OutputTokens:         output,
		OutputThinkingTokens: thinking,
		Provisional:          provisional,
	}
}

func TestSame(t *testing.T) {
	base := func() *conversationv1.TokenUsage { return usage(1, 2, 3, 4, 2, false) }
	cases := []struct {
		name string
		a, b *conversationv1.TokenUsage
		want bool
	}{
		{"two nil usages are the same", nil, nil, true},
		{"a nil and a held usage differ", nil, base(), false},
		{"a held and a nil usage differ", base(), nil, false},
		{"equal fields are the same", base(), base(), true},
		{"a different cache read differs", base(), usage(9, 2, 3, 4, 2, false), false},
		{"a different cache write differs", base(), usage(1, 9, 3, 4, 2, false), false},
		{"a different uncached input differs", base(), usage(1, 2, 9, 4, 2, false), false},
		{"a different output differs", base(), usage(1, 2, 3, 9, 2, false), false},
		{"a different thinking share differs", base(), usage(1, 2, 3, 4, 3, false), false},
		{"a different provisional mark differs", base(), usage(1, 2, 3, 4, 2, true), false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the two usages are the case's.

			// Act
			got := Same(tc.a, tc.b)

			// Assert
			if got != tc.want {
				t.Fatalf("Same = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestReconcile(t *testing.T) {
	settled := usage(10, 20, 30, 40, 5, false)
	settledOther := usage(11, 20, 30, 41, 5, false)
	provisional := usage(10, 20, 30, 12, 5, true)
	provisionalOther := usage(10, 20, 30, 25, 5, true)
	thinkingHeavy := usage(10, 20, 30, 4, 9, false)
	thinkingHeavyProvisional := usage(10, 20, 30, 4, 9, true)
	cases := []struct {
		name               string
		previous, next     *conversationv1.TokenUsage
		wantTake           bool
		wantContradictions []string
	}{
		{"no next report changes nothing", settled, nil, false, nil},
		{"no next report with none held changes nothing", nil, nil, false, nil},
		{"a first report is taken", nil, settled, true, nil},
		{"a first report with thinking over output is taken and contradicts", nil, thinkingHeavy, true, []string{ThinkingExceedsOutput}},
		{"the same settled report again changes nothing", settled, usage(10, 20, 30, 40, 5, false), false, nil},
		{"the same provisional report again changes nothing", provisional, usage(10, 20, 30, 12, 5, true), false, nil},
		{"a settled report supersedes a provisional one without contradiction", provisional, settled, true, nil},
		{"a provisional refresh replaces a provisional one without contradiction", provisional, provisionalOther, true, nil},
		{"a provisional report never overrides a settled one", settled, provisional, false, nil},
		{"a provisional report with thinking over output after a settled one is not taken but contradicts", settled, thinkingHeavyProvisional, false, []string{ThinkingExceedsOutput}},
		{"two differing settled reports are taken and contradict", settled, settledOther, true, []string{TwoDifferentUsages}},
		{"two differing settled reports with thinking over output make both contradictions", settled, thinkingHeavy, true, []string{ThinkingExceedsOutput, TwoDifferentUsages}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the held and the new usage are the case's.

			// Act
			got := Reconcile(tc.previous, tc.next)

			// Assert
			if got.Take != tc.wantTake {
				t.Errorf("Take = %v, want %v", got.Take, tc.wantTake)
			}
			if strings.Join(got.Contradictions, "|") != strings.Join(tc.wantContradictions, "|") {
				t.Errorf("Contradictions = %q, want %q", got.Contradictions, tc.wantContradictions)
			}
		})
	}
}

// TestResolversReconcileOnlyThroughThisPackage enforces that usage
// reconciliation has ONE home: no resolver compares usages field by field or
// spells a contradiction text itself, so the rules cannot drift apart between
// the topbar and the footer again.
func TestResolversReconcileOnlyThroughThisPackage(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "resolve")
	forbidden := []struct {
		name    string
		pattern *regexp.Regexp
	}{
		{"a usage getter compared by hand", regexp.MustCompile(`Get(OutputTokens|OutputThinkingTokens|Read|Written|Unwritten|Provisional)\(\)\s*(==|!=|>|<)`)},
		{"a thinking share compared by hand", regexp.MustCompile(`\.thinking\s*(==|!=|>|<)`)},
		{"the two-usages contradiction text", regexp.MustCompile(`two different usages`)},
		{"the thinking contradiction text", regexp.MustCompile(`more thinking tokens`)},
	}
	var offenses []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(body), "\n") {
			for _, f := range forbidden {
				if f.pattern.MatchString(line) {
					offenses = append(offenses, fmt.Sprintf("%s:%d: %s: %s", path, i+1, f.name, strings.TrimSpace(line)))
				}
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walking %s: %v", root, err)
	}
	if len(offenses) > 0 {
		t.Fatalf("usage reconciliation done outside package tokenusage:\n%s", strings.Join(offenses, "\n"))
	}
}
