package addsupport

import (
	"strings"
	"testing"
)

func TestValidateCommand(t *testing.T) {
	tests := []struct {
		name    string
		command string
		wantErr string
	}{
		{name: "plain builtin", command: "status"},
		{name: "namespaced", command: "gns-cowork:gns-bootstrap"},
		{name: "underscores and digits", command: "foo_bar2"},
		{name: "empty", command: "", wantErr: "command is required"},
		{name: "too long", command: strings.Repeat("a", maxCommandLen+1), wantErr: "exceeds 64 characters"},
		{name: "path traversal", command: "../etc", wantErr: "invalid character"},
		{name: "slash", command: "a/b", wantErr: "invalid character"},
		{name: "whitespace", command: "a b", wantErr: "invalid character"},
		{name: "shell metacharacter", command: "a;b", wantErr: "invalid character"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange + Act.
			err := ValidateCommand(tc.command)

			// Assert.
			if tc.wantErr == "" {
				if err != nil {
					t.Fatalf("ValidateCommand(%q) error = %v, want nil", tc.command, err)
				}
				return
			}
			if err == nil {
				t.Fatalf("ValidateCommand(%q) error = nil, want %q", tc.command, tc.wantErr)
			}
			if !strings.Contains(err.Error(), tc.wantErr) {
				t.Errorf("ValidateCommand(%q) error = %q, want it to contain %q", tc.command, err, tc.wantErr)
			}
		})
	}
}

func TestWorkspaceNameForAPlainCommand(t *testing.T) {
	// Arrange + Act.
	got := WorkspaceName("status")

	// Assert.
	if want := "add-support-status"; got != want {
		t.Errorf("WorkspaceName() = %q, want %q", got, want)
	}
}

func TestWorkspaceNameFlattensAColonIllegalInAGitRef(t *testing.T) {
	// Arrange + Act.
	got := WorkspaceName("plugin:cmd")

	// Assert.
	if strings.Contains(got, ":") {
		t.Errorf("WorkspaceName() = %q, want no colon (illegal in a git ref)", got)
	}
	if want := "add-support-plugin-cmd"; got != want {
		t.Errorf("WorkspaceName() = %q, want %q", got, want)
	}
}

func TestPromptNamesTheCommand(t *testing.T) {
	// Arrange + Act.
	got := Prompt("status", "/home/u/.claude")

	// Assert.
	if !strings.Contains(got, "/status") {
		t.Errorf("Prompt() = %q, want it to name /status", got)
	}
}

func TestPromptNamesAnExplicitConfigDir(t *testing.T) {
	// Arrange + Act.
	got := Prompt("status", "/home/u/.claude-chesscom")

	// Assert.
	if !strings.Contains(got, "/home/u/.claude-chesscom") {
		t.Errorf("Prompt() = %q, want it to name the session's config dir", got)
	}
}

func TestPromptFallsBackToTheDefaultRootWhenConfigDirIsEmpty(t *testing.T) {
	// Arrange + Act.
	got := Prompt("status", "")

	// Assert.
	if !strings.Contains(got, "~/.claude") {
		t.Errorf("Prompt() = %q, want it to name the CLI's default config root", got)
	}
}

func TestPromptIsNeverEmptyForAValidCommand(t *testing.T) {
	// Arrange + Act.
	got := Prompt("status", "")

	// Assert.
	// workspacecmd refuses an empty prompt, and Emacs warns loudly on one,
	// so a silently blank brief must be impossible here.
	if strings.TrimSpace(got) == "" {
		t.Error("Prompt() = empty, want a non-empty brief")
	}
}
