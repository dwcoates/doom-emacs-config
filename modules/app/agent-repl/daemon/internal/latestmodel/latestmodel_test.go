package latestmodel

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

func TestRecord(t *testing.T) {
	tests := []struct {
		name     string
		agent    string
		filed    string
		model    *conversationv1.AgentModel
		wantMove bool
		want     string
	}{
		{name: "a first model", agent: "a", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: true, want: "m1"},
		{name: "a changed model", agent: "a", filed: "m1", model: &conversationv1.AgentModel{Name: "m2"}, wantMove: true, want: "m2"},
		{name: "the same model", agent: "a", filed: "m1", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: false, want: "m1"},
		{name: "no model", agent: "a", filed: "m1", model: nil, wantMove: false, want: "m1"},
		{name: "an empty name", agent: "a", filed: "m1", model: &conversationv1.AgentModel{}, wantMove: false, want: "m1"},
		{name: "no agent", agent: "", model: &conversationv1.AgentModel{Name: "m1"}, wantMove: false, want: ""},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			ledger := Ledger{}
			if test.filed != "" {
				ledger[test.agent] = test.filed
			}

			// Act.
			moved := ledger.Record(&conversationv1.AgentId{Value: test.agent}, test.model)

			// Assert.
			if moved != test.wantMove || ledger.Of(test.agent) != test.want {
				t.Fatalf("moved %v filed %q, want %v %q", moved, ledger.Of(test.agent), test.wantMove, test.want)
			}
		})
	}
}

func TestOfAnAgentNoResponseNamedIsEmpty(t *testing.T) {
	// Arrange.
	ledger := Ledger{"a": "m1"}

	// Act.
	got := ledger.Of("b")

	// Assert.
	if got != "" {
		t.Fatalf("Of(b) = %q, want empty", got)
	}
}

// activityModelRead is a resolver reading an activity's model; every such
// read must hand it straight to a Ledger.
var activityModelRead = regexp.MustCompile(`act\.(GetModel\(\)|Model\b)`)

func TestNoDaemonSiteReadsAnActivitysModelOutsideTheLedger(t *testing.T) {
	// Arrange: every production source of the daemon's resolvers.
	root := filepath.Join("..", "resolve")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(src), "\n") {
			if activityModelRead.MatchString(line) && !strings.Contains(line, ".Record(") {
				offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
			}
		}
		return nil
	})

	// Assert: a subagent's model is filed in one place, Ledger.Record.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled activity model reads (use latestmodel.Ledger): %s", strings.Join(offenders, ", "))
	}
}
