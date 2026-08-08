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

	"claude-repld/internal/prompts"
)

// PromptFile is the prompt file the generated workspace's brief is read from.
const PromptFile = "add-support-slash-command.md"

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

// defaultConfigRoot is what the brief names when the session runs under the
// CLI's own default root. It stays in code rather than in the prompt file
// because it is a statement of FACT about this session's environment, derived
// the same way the non-empty case is; only the surrounding brief is editable.
const defaultConfigRoot = "~/.claude (the CLI's default, since this session sets no CLAUDE_CONFIG_DIR)"

// Prompt is the generated workspace's brief for adding graphical support for
// command, read from prompts/PromptFile at every use. configDir names the
// session's CLAUDE_CONFIG_DIR, and is empty when the session runs under the
// CLI's own default root.
//
// An error means the brief could not be composed, and the caller refuses the
// add-support request outright. Opening a workspace with a broken or empty
// brief would burn a worktree, a branch, and a session on nothing.
func Prompt(command, configDir string) (string, error) {
	root := configDir
	if root == "" {
		root = defaultConfigRoot
	}
	return prompts.Render(PromptFile, map[string]string{
		"command":     command,
		"config_root": root,
	})
}
