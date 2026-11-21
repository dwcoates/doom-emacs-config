package agentverbs

import (
	"strings"
	"testing"
)

func TestReplacementUsageNamesTheVerbOfEachReplacedCommand(t *testing.T) {
	tests := []struct {
		name    string
		command string
		want    string
		wantOK  bool
	}{
		{name: "/compact", command: SlashCompact, want: CompactUsage, wantOK: true},
		{name: "/model", command: SlashModel, want: ModelUsage, wantOK: true},
		{name: "a command with no verb", command: "/clear"},
		{name: "a command without its slash", command: "compact"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := ReplacementUsage(tt.command)

			// Assert
			if got != tt.want || ok != tt.wantOK {
				t.Fatalf("ReplacementUsage(%q) = %q, %v; want %q, %v", tt.command, got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestSlashCommandRefusalSaysSlashCommandsAreNotSupportedAndNamesTheReplacement(t *testing.T) {
	tests := []struct {
		name    string
		command string
		want    []string
		absent  []string
	}{
		{name: "/compact names its one verb and usage", command: "/compact",
			want:   []string{"slash commands are not supported from agents", "/compact", "agent-repld workspace " + CompactUsage},
			absent: []string{"workspace model"}},
		{name: "/model names its one verb and usage", command: "/model",
			want:   []string{"slash commands are not supported from agents", "/model", "agent-repld workspace " + ModelUsage},
			absent: []string{"workspace compact"}},
		{name: "another command names both verbs", command: "/clear",
			want: []string{"slash commands are not supported from agents", "/clear", "`agent-repld workspace compact` for /compact", "`agent-repld workspace model` for /model"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := SlashCommandRefusal(tt.command)

			// Assert
			for _, want := range tt.want {
				if !strings.Contains(got, want) {
					t.Fatalf("refusal %q lacks %q", got, want)
				}
			}
			for _, absent := range tt.absent {
				if strings.Contains(got, absent) {
					t.Fatalf("refusal %q names %q, a verb that does not replace %s", got, absent, tt.command)
				}
			}
		})
	}
}
