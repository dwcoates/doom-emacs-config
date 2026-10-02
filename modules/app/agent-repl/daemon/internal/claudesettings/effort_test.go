package claudesettings

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

const (
	unspecified = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED
	low         = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	medium      = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MEDIUM
	high        = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH
	xhigh       = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_XHIGH
	maxLevel    = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_MAX
)

// rootWith is a config root whose settings file holds BODY.
func rootWith(t *testing.T, body string) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, FileName), []byte(body), 0o600); err != nil {
		t.Fatal(err)
	}
	return dir
}

func TestReadEffortReadsTheFileItsFacts(t *testing.T) {
	tests := []struct {
		name         string
		body         string
		wantDefault  conversationv1.AgentEffortLevel
		wantPerModel map[string]conversationv1.AgentEffortLevel
	}{
		{"a file naming no level states nothing", `{"model":"opus"}`, unspecified, map[string]conversationv1.AgentEffortLevel{}},
		{"the top-level level", `{"effortLevel":"medium"}`, medium, map[string]conversationv1.AgentEffortLevel{}},
		{"low maps", `{"effortLevel":"low"}`, low, map[string]conversationv1.AgentEffortLevel{}},
		{"high maps", `{"effortLevel":"high"}`, high, map[string]conversationv1.AgentEffortLevel{}},
		{"xhigh maps", `{"effortLevel":"xhigh"}`, xhigh, map[string]conversationv1.AgentEffortLevel{}},
		{"max maps", `{"effortLevel":"max"}`, maxLevel, map[string]conversationv1.AgentEffortLevel{}},
		{"a per-model override", `{"modelSettings":{"claude-opus-4-8":{"effortLevel":"low"}}}`, unspecified,
			map[string]conversationv1.AgentEffortLevel{"claude-opus-4-8": low}},
		{"a per-model entry naming no level is not an override", `{"modelSettings":{"claude-opus-4-8":{"maxEffortLevel":"high"}}}`, unspecified,
			map[string]conversationv1.AgentEffortLevel{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			dir := rootWith(t, tt.body)

			// Act.
			got, err := ReadEffort(dir)

			// Assert.
			if err != nil {
				t.Fatalf("ReadEffort: %v", err)
			}
			if got.Default != tt.wantDefault {
				t.Errorf("Default = %v, want %v", got.Default, tt.wantDefault)
			}
			if len(got.PerModel) != len(tt.wantPerModel) {
				t.Fatalf("PerModel = %v, want %v", got.PerModel, tt.wantPerModel)
			}
			for model, want := range tt.wantPerModel {
				if got.PerModel[model] != want {
					t.Errorf("PerModel[%q] = %v, want %v", model, got.PerModel[model], want)
				}
			}
		})
	}
}

func TestReadEffortTreatsAMissingFileAsStatingNothing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	got, err := ReadEffort(dir)

	// Assert.
	if err != nil {
		t.Fatalf("ReadEffort: %v", err)
	}
	if got.Default != unspecified || len(got.PerModel) != 0 || got.Path != filepath.Join(dir, FileName) {
		t.Errorf("got %+v, want an empty Effort naming the path", got)
	}
}

func TestReadEffortRefuses(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		wantErr string
	}{
		{"a malformed file", `{"effortLevel":`, "parse"},
		{"an unknown top-level spelling", `{"effortLevel":"ultra"}`, `"ultra" is not an effort level`},
		{"an unknown per-model spelling", `{"modelSettings":{"m":{"effortLevel":"ultra"}}}`, `modelSettings["m"]`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			dir := rootWith(t, tt.body)

			// Act.
			_, err := ReadEffort(dir)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Errorf("err = %v, want one containing %q", err, tt.wantErr)
			}
		})
	}
}

func TestReadEffortRefusesAnUnreadableFile(t *testing.T) {
	// Arrange: a directory where the file belongs reads as an error, not as absent.
	dir := t.TempDir()
	if err := os.Mkdir(filepath.Join(dir, FileName), 0o700); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := ReadEffort(dir)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "read") {
		t.Errorf("err = %v, want a read error", err)
	}
}

func TestEffortFor(t *testing.T) {
	settings := Effort{
		Default:  high,
		PerModel: map[string]conversationv1.AgentEffortLevel{"claude-opus-4-8": low, "claude-sonnet-5": maxLevel},
	}
	tests := []struct {
		name       string
		effort     Effort
		model      string
		wantLevel  conversationv1.AgentEffortLevel
		wantSource Source
	}{
		{"an exact per-model key", settings, "claude-opus-4-8", low, SourceModelSettings},
		{"a dated id matches its canonical key", settings, "claude-opus-4-8-20260101", low, SourceModelSettings},
		{"a [1m] spelling matches its canonical key", settings, "claude-sonnet-5[1m]", maxLevel, SourceModelSettings},
		{"a model with no override takes the top-level level", settings, "claude-haiku-4-5", high, SourceDefault},
		{"nothing stated is unset", Effort{}, "claude-haiku-4-5", unspecified, SourceUnset},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table.

			// Act.
			gotLevel, gotSource := tt.effort.For(tt.model)

			// Assert.
			if gotLevel != tt.wantLevel || gotSource != tt.wantSource {
				t.Errorf("For(%q) = (%v, %v), want (%v, %v)", tt.model, gotLevel, gotSource, tt.wantLevel, tt.wantSource)
			}
		})
	}
}

func TestCanonical(t *testing.T) {
	tests := []struct {
		model, want string
	}{
		{"claude-opus-4-8", "claude-opus-4-8"},
		{"claude-haiku-4-5-20251001", "claude-haiku-4-5"},
		{"claude-sonnet-5[1m]", "claude-sonnet-5"},
		{"opus", "opus"},
	}
	for _, tt := range tests {
		t.Run(tt.model, func(t *testing.T) {
			// Arrange: the table. Act.
			got := Canonical(tt.model)

			// Assert.
			if got != tt.want {
				t.Errorf("Canonical(%q) = %q, want %q", tt.model, got, tt.want)
			}
		})
	}
}
