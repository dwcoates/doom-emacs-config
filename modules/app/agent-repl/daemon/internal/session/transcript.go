// Package session, after the agent-shim consumption cutover, retains only the
// pure transcript-path helpers the daemon still needs: the CLI's project-dir
// encoding (TranscriptPath) and the per-session config-root resolution
// (ClaudeConfigDir). Everything else that once lived here — the Layer-2
// streaming hub (Session, Client, the stdio Run loop, the Translator, replay,
// queue/classify/summarize, the detached-task tailer) — was deleted when the
// daemon moved to consuming each session's UDS shim through internal/sessiondrv
// and rendering onto the frontend.v1 surface + SSM. These helpers are pure
// (filesystem-path arithmetic only) and back the resume-viability gate, the
// account-switch transcript migration, and registry rehydration.
package session

import (
	"os"
	"path/filepath"
	"regexp"
)

// DefaultClaudeConfigDir returns the Claude CLI config root: the
// CLAUDE_CONFIG_DIR override when set, else ~/.claude.
func DefaultClaudeConfigDir() string {
	if dir := os.Getenv("CLAUDE_CONFIG_DIR"); dir != "" {
		return dir
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	return filepath.Join(home, ".claude")
}

// ClaudeConfigDir resolves the config root for ONE session: its per-session dir
// when Emacs supplied one (agent-repl--compute-config-dir picks the account
// from the project dir — ~/.claude-chesscom under $MULTI_REPO_ROOT, ~/.claude
// elsewhere), else the daemon-wide default.
//
// Every transcript lookup MUST route through here rather than
// DefaultClaudeConfigDir: a session whose CLI writes into ~/.claude-chesscom
// has no transcript under ~/.claude, so resolving against the daemon's own env
// would fail the resume-viability gate and silently downgrade a resume into a
// fresh conversation.
func ClaudeConfigDir(dir string) string {
	if dir != "" {
		return dir
	}
	return DefaultClaudeConfigDir()
}

var transcriptSlugRe = regexp.MustCompile(`[^A-Za-z0-9]`)

// TranscriptPath returns the transcript JSONL path for claudeSessionID rooted
// at cwd, mirroring the CLI's project-dir encoding (every non-alphanumeric byte
// of the absolute cwd becomes "-").
func TranscriptPath(configDir, cwd, claudeSessionID string) string {
	slug := transcriptSlugRe.ReplaceAllString(cwd, "-")
	return filepath.Join(configDir, "projects", slug, claudeSessionID+".jsonl")
}
