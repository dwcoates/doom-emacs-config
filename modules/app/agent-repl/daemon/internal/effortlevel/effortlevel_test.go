package effortlevel

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestWord(t *testing.T) {
	tests := []struct {
		level conversationv1.AgentEffortLevel
		want  string
	}{
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW, "low"},
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM, "medium"},
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH, "high"},
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_XHIGH, "xhigh"},
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX, "max"},
		{conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, ""},
		{conversationv1.AgentEffortLevel(99), ""},
	}
	for _, tt := range tests {
		t.Run(tt.level.String(), func(t *testing.T) {
			// Arrange: the table. Act.
			got := Word(tt.level)

			// Assert.
			if got != tt.want {
				t.Errorf("Word(%v) = %q, want %q", tt.level, got, tt.want)
			}
		})
	}
}

func TestParseRoundTripsEveryWord(t *testing.T) {
	for _, s := range spellings {
		t.Run(s.word, func(t *testing.T) {
			// Arrange: the table. Act.
			got, err := Parse(s.word)

			// Assert.
			if err != nil || got != s.level {
				t.Errorf("Parse(%q) = (%v, %v), want (%v, nil)", s.word, got, err, s.level)
			}
		})
	}
}

func TestParseTreatsTheEmptyWordAsUnstated(t *testing.T) {
	// Arrange, Act.
	got, err := Parse("")

	// Assert.
	if err != nil || got != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		t.Errorf("Parse(\"\") = (%v, %v), want (UNSPECIFIED, nil)", got, err)
	}
}

func TestParseRefusesAnUnknownWord(t *testing.T) {
	// Arrange, Act.
	_, err := Parse("ultra")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), `"ultra"`) {
		t.Errorf("err = %v, want one naming the word", err)
	}
}

// TestNoOtherPackageSpellsALevel asserts the call sites share this table: a
// production file elsewhere naming a level's enum value by hand is a second
// table drifting beside this one.
func TestNoOtherPackageSpellsALevel(t *testing.T) {
	// Arrange.
	root := filepath.Join("..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(filepath.Dir(path)) == "effortlevel" {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "AGENT_EFFORT_LEVEL_XHIGH") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) != 0 {
		t.Errorf("files spelling a level outside effortlevel: %v", offenders)
	}
}
