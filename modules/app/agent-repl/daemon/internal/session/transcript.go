// Package session, after the agent-shim consumption cutover, retains only the
// pure transcript-path helpers the daemon still needs: the CLI's project-dir
// encoding (TranscriptPath) and the per-session config-root resolution
// (ClaudeConfigDir). Everything else that once lived here — the Layer-2
// streaming hub (Session, Client, the stdio Run loop, the Translator, replay,
// queue/classify/summarize, the detached-task tailer) — was deleted when the
// daemon moved to consuming each session's UDS shim through internal/sessioncontroller
// and rendering onto the frontend.v1 surface + SSM. These helpers are pure
// (filesystem-path arithmetic only) and back the resume-viability gate, the
// account-switch transcript migration, and registry rehydration.
package session

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"time"
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

// EncodeCWD returns the CLI's `projects/<dir>` encoding of an absolute cwd:
// every non-alphanumeric byte becomes "-".
func EncodeCWD(cwd string) string {
	return transcriptSlugRe.ReplaceAllString(cwd, "-")
}

// ProjectDir returns the directory the CLI files cwd's transcripts under.
func ProjectDir(configDir, cwd string) string {
	return filepath.Join(configDir, "projects", EncodeCWD(cwd))
}

// TranscriptPath returns the transcript JSONL path for claudeSessionID rooted
// at cwd, mirroring the CLI's project-dir encoding (every non-alphanumeric byte
// of the absolute cwd becomes "-").
func TranscriptPath(configDir, cwd, claudeSessionID string) string {
	return filepath.Join(ProjectDir(configDir, cwd), claudeSessionID+".jsonl")
}

// TranscriptExists reports whether the transcript naming claudeSessionID is on
// disk, and the path it looked at (so a caller can name it in the refusal).
//
// It is the ONE resume-viability question, shared by the two places that ask
// it: the create gate, which hard-fails a client asking to resume something
// gone, and the spawner, which must never hand `--resume <uuid>` to a CLI that
// will exit 1 on it. Two copies of this stat would be two chances to disagree
// about which root or which encoding the uuid resolves under.
//
// A stat error of any kind is a MISS. The question is "can the CLI resume
// this", and an unreadable path answers that as decisively as a missing one.
func TranscriptExists(configDir, cwd, claudeSessionID string) (path string, exists bool) {
	path = TranscriptPath(ClaudeConfigDir(configDir), cwd, claudeSessionID)
	if _, err := os.Stat(path); err != nil {
		return path, false
	}
	return path, true
}

// Transcript is one session transcript found on disk.
type Transcript struct {
	// ConfigDir is the `~/.claude*` root the transcript was found under.
	ConfigDir string
	// SessionID is the vendor (Claude CLI) session uuid — the filename stem,
	// which is exactly the id `--resume` takes.
	SessionID string
	Path      string
	ModTime   time.Time
}

// NewestTranscript returns the most recently modified `<uuid>.jsonl` the CLI
// has written for cwd under configDir, and whether one exists.
//
// A missing project dir is a MISS, not an error: a workspace that has never
// been talked to simply has no transcript. Anything else (an unreadable dir, a
// broken stat) is surfaced.
func NewestTranscript(configDir, cwd string) (Transcript, bool, error) {
	dir := ProjectDir(configDir, cwd)
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return Transcript{}, false, nil
		}
		return Transcript{}, false, fmt.Errorf("session: read project dir %s: %w", dir, err)
	}
	var newest Transcript
	found := false
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".jsonl") {
			continue
		}
		info, err := e.Info()
		if err != nil {
			return Transcript{}, false, fmt.Errorf("session: stat transcript %s: %w", filepath.Join(dir, e.Name()), err)
		}
		if found && !info.ModTime().After(newest.ModTime) {
			continue
		}
		newest = Transcript{
			ConfigDir: configDir,
			SessionID: strings.TrimSuffix(e.Name(), ".jsonl"),
			Path:      filepath.Join(dir, e.Name()),
			ModTime:   info.ModTime(),
		}
		found = true
	}
	return newest, found, nil
}

// Discovery is the result of probing every known config dir for a workspace's
// transcripts.
type Discovery struct {
	// Adopted is the newest transcript under the session's OWN config dir. Only
	// this one may be bound to the session: it is the root the CLI would resume
	// from.
	Adopted Transcript
	// Found reports whether Adopted is set.
	Found bool
	// Migrations are transcripts found under OTHER config dirs. They are
	// REPORTED, never adopted: adopting one would silently resume a
	// conversation the session's own CLI root cannot see, so the honest move is
	// to name it loudly and leave the decision to a human.
	Migrations []Transcript
}

// Discover probes ownDir first, then every other dir in configDirs, for cwd's
// newest transcript. Duplicate and empty dirs are skipped.
func Discover(ownDir string, configDirs []string, cwd string) (Discovery, error) {
	var out Discovery
	if ownDir != "" {
		t, ok, err := NewestTranscript(ownDir, cwd)
		if err != nil {
			return Discovery{}, err
		}
		out.Adopted, out.Found = t, ok
	}
	seen := map[string]bool{ownDir: true, "": true}
	others := append([]string(nil), configDirs...)
	sort.Strings(others)
	for _, dir := range others {
		if seen[dir] {
			continue
		}
		seen[dir] = true
		t, ok, err := NewestTranscript(dir, cwd)
		if err != nil {
			return Discovery{}, err
		}
		if ok {
			out.Migrations = append(out.Migrations, t)
		}
	}
	return out, nil
}
