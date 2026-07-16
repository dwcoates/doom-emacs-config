// Package addsupport turns "the CLI refused a slash command in this
// environment" into the workspace request that engineers the gap away.
//
// A slash command the CLI only implements as an interactive terminal
// panel is dead weight in the agent-repl GUI: the non-interactive session
// answers "/x isn't available in this environment." and that is the end
// of it. That refusal is not a condition to live with — the data behind
// the command still exists, reachable through some other CLI surface or
// by reading the session's Claude config directory — so the GUI offers a
// button that asks Emacs to open a workspace whose job is to build a rich
// graphical rendering of the feature instead.
//
// This package only composes the request (name, prompt) and validates its
// input. Emitting it is workspacecmd's job, and acting on it is Emacs's.
package addsupport

import (
	"fmt"
	"strings"
)

// maxCommandLen bounds the caller-supplied command name. The longest real
// built-in is nowhere near this, so anything longer is a malformed or
// hostile payload rather than a command.
const maxCommandLen = 64

// namePrefix opens the generated workspace's branch name.
const namePrefix = "add-support-"

// ValidateCommand checks a caller-supplied slash-command name.
//
// The name reaches a git branch name and a workspace name, so it is
// validated against the CLI's own command-name charset (its Nio()
// predicate: letters, digits, colon, hyphen, underscore) rather than
// trusted. Anything else is refused outright: no sanitizing pass, because
// silently rewriting a name into something that "looks valid" would let a
// malformed request through under a name nobody asked for.
func ValidateCommand(command string) error {
	if command == "" {
		return fmt.Errorf("addsupport: command is required")
	}
	if len(command) > maxCommandLen {
		return fmt.Errorf("addsupport: command exceeds %d characters", maxCommandLen)
	}
	for _, r := range command {
		switch {
		case r >= 'a' && r <= 'z',
			r >= 'A' && r <= 'Z',
			r >= '0' && r <= '9',
			r == ':', r == '-', r == '_':
		default:
			return fmt.Errorf("addsupport: command contains an invalid character %q", r)
		}
	}
	return nil
}

// WorkspaceName is the branch/workspace name for a command's support work.
// A namespaced command ("plugin:cmd") flattens its colon to a hyphen,
// since a colon is not legal in a git ref. Emacs disambiguates collisions
// on its own, so repeated requests need no uniqueness here.
func WorkspaceName(command string) string {
	return namePrefix + strings.ReplaceAll(command, ":", "-")
}

// Prompt is the generated workspace's brief for adding graphical support
// for command. configDir names the session's CLAUDE_CONFIG_DIR, and is
// empty when the session runs under the CLI's own default root.
func Prompt(command, configDir string) string {
	root := configDir
	if root == "" {
		root = "~/.claude (the CLI's default, since this session sets no CLAUDE_CONFIG_DIR)"
	}
	return strings.Join([]string{
		fmt.Sprintf("The agent-repl GUI has no support for the `/%s` slash command.", command),
		"",
		fmt.Sprintf("Running it in this non-interactive session answers only `/%s isn't available in this environment.`, because the Claude Code CLI implements it as an interactive terminal panel and the GUI's session is headless. The feature is therefore unreachable from the GUI today.", command),
		"",
		fmt.Sprintf("Your job: investigate adding RICH GRAPHICAL support for `/%s` inside the agent-repl GUI, so the GUI stops depending on a terminal panel it can never open.", command),
		"",
		"Where the underlying data lives, in order of preference:",
		fmt.Sprintf("1. The Claude Code CLI itself. Work out what `/%s` would render, then find whether any NON-interactive surface already exposes the same data (another subcommand, a flag, a machine-readable output mode, or an SDK/stream event the shim already receives). Prefer a supported surface over anything else.", command),
		fmt.Sprintf("2. The session's Claude config directory, at %s. Inspect the files there BY HAND to find the state the command reports. Read them directly rather than assuming a schema.", root),
		"",
		"Deliverable: the feature rendered richly in the agent-repl webapp, following the module's existing patterns rather than inventing new ones. Study how a comparable feature already flows end to end (shim event, daemon frame, webapp store, webapp render) and mirror it.",
		"",
		"Constraints:",
		"- Investigate FIRST and report what you found before building, since the right surface is the whole question here.",
		"- If no supported surface exists and the config files are the only source, say so explicitly and explain what a config-file read would have to assume.",
		"- Follow the repo's testing requirements in CLAUDE.md: one test file per source module, one edge case per test, and every test run and passing before you commit.",
	}, "\n")
}
