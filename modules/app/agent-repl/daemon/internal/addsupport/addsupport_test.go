package addsupport

import (
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

// usePrompts points the prompt loader at this checkout's real prompts/
// directory for one test. A `go test` binary lives in the build cache, so the
// ordinary executable walk-up has no checkout above it to find.
func usePrompts(t *testing.T) {
	t.Helper()
	dir, err := prompts.SourceDir()
	if err != nil {
		t.Fatalf("resolve the checkout's prompts directory: %v", err)
	}
	t.Setenv(prompts.DirEnv, dir)
}

// mustPrompt returns a checker bound to t, used as `mustPrompt(t)(Prompt(...))`.
func mustPrompt(t *testing.T) func(string, error) string {
	t.Helper()
	return func(text string, err error) string {
		t.Helper()
		if err != nil {
			t.Fatalf("compose prompt: %v", err)
		}
		return text
	}
}

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
	usePrompts(t)
	got := mustPrompt(t)(Prompt("status", "/home/u/.claude"))

	// Assert.
	if !strings.Contains(got, "/status") {
		t.Errorf("Prompt() = %q, want it to name /status", got)
	}
}

func TestPromptNamesAnExplicitConfigDir(t *testing.T) {
	// Arrange + Act.
	usePrompts(t)
	got := mustPrompt(t)(Prompt("status", "/home/u/.claude-chesscom"))

	// Assert.
	if !strings.Contains(got, "/home/u/.claude-chesscom") {
		t.Errorf("Prompt() = %q, want it to name the session's config dir", got)
	}
}

func TestPromptFallsBackToTheDefaultRootWhenConfigDirIsEmpty(t *testing.T) {
	// Arrange + Act.
	usePrompts(t)
	got := mustPrompt(t)(Prompt("status", ""))

	// Assert.
	if !strings.Contains(got, "~/.claude") {
		t.Errorf("Prompt() = %q, want it to name the CLI's default config root", got)
	}
}

func TestPromptIsNeverEmptyForAValidCommand(t *testing.T) {
	// Arrange + Act.
	usePrompts(t)
	got := mustPrompt(t)(Prompt("status", ""))

	// Assert.
	// workspacecmd refuses an empty prompt, and Emacs warns loudly on one,
	// so a silently blank brief must be impossible here.
	if strings.TrimSpace(got) == "" {
		t.Error("Prompt() = empty, want a non-empty brief")
	}
}

// TestPromptMatchesTheGolden pins prompts/add-support-slash-command.md against
// the brief that lived in addsupport.go before the prompt moved out of source.
func TestPromptMatchesTheGolden(t *testing.T) {
	// Arrange.
	usePrompts(t)
	want := strings.Join([]string{
		"The agent-repl GUI has no support for the `/status` slash command.",
		"",
		"Running it in this non-interactive session answers only `/status isn't available in this environment.`, because the Claude Code CLI implements it as an interactive terminal panel and the GUI's session is headless. The feature is therefore unreachable from the GUI today.",
		"",
		"Your job: investigate adding RICH GRAPHICAL support for `/status` inside the agent-repl GUI, so the GUI stops depending on a terminal panel it can never open.",
		"",
		"Where the underlying data lives, in order of preference:",
		"1. The Claude Code CLI itself. Work out what `/status` would render, then find whether any NON-interactive surface already exposes the same data (another subcommand, a flag, a machine-readable output mode, or an SDK/stream event the shim already receives). Prefer a supported surface over anything else.",
		"2. The session's Claude config directory, at /home/u/.claude. Inspect the files there BY HAND to find the state the command reports. Read them directly rather than assuming a schema.",
		"",
		"Deliverable: the feature rendered richly in the agent-repl webapp, following the module's existing patterns rather than inventing new ones. Study how a comparable feature already flows end to end (shim event, daemon frame, webapp store, webapp render) and mirror it.",
		"",
		"Constraints:",
		"- Investigate FIRST and report what you found before building, since the right surface is the whole question here.",
		"- If no supported surface exists and the config files are the only source, say so explicitly and explain what a config-file read would have to assume.",
		"- Follow the repo's testing requirements in CLAUDE.md: one test file per source module, one edge case per test, and every test run and passing before you commit.",
	}, "\n")

	// Act.
	got := mustPrompt(t)(Prompt("status", "/home/u/.claude"))

	// Assert.
	if got != want {
		t.Fatalf("brief drifted from the pre-extraction text.\n got: %q\nwant: %q", got, want)
	}
}

func TestPromptErrorsWhenItsFileIsMissing(t *testing.T) {
	// Arrange — an empty prompts directory stands for a deleted or misnamed file.
	t.Setenv(prompts.DirEnv, t.TempDir())

	// Act.
	got, err := Prompt("status", "")

	// Assert — no baked-in fallback brief.
	if err == nil {
		t.Fatalf("Prompt() = %q, nil; want a loud error when %s is unreadable", got, PromptFile)
	}
}
