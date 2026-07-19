// Package workspacecmd emits workspace commands for the Emacs side to act
// on. Emacs owns the workspaces entirely; the daemon only asks.
//
// The channel is the same one the managed emit-workspace-commands.sh skill
// writes to: a JSON array dropped at
// $AGENT_REPL_STATE_DIR/output/workspace_commands_<id>.json, which Emacs
// watches (agent-repl--drain-workspace-commands-files) and dispatches by
// each entry's "type" through its own handler table. The daemon emits
// "create" (open a workspace), "switch" (focus one), and "fold" (set a
// sidebar repo section's folded state). Writing the file IS the request;
// there is no reply, because the daemon has no say in what Emacs does
// with it.
//
// Atomicity matters: Emacs's watcher fires on any file matching
// workspace_commands_*.json, so a half-written file would be drained and
// fail to parse. Every write therefore lands on a dot-prefixed temp name
// the watcher's regexp cannot match, then renames into place.
package workspacecmd

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/stateroot"
)

// OutputDirName is the component under the state root that Emacs watches.
// It MUST match `agent-repl-workspace-commands-output-dir' in worktree.el.
const OutputDirName = "output"

// filePrefix MUST match `agent-repl-workspace-commands-file-prefix' in
// worktree.el, which is half of the watcher's regexp.
const filePrefix = "workspace_commands_"

// Entry is one entry in a workspace-commands array. Every entry carries
// the "type" tag Emacs dispatches on, and Validate refuses any entry the
// matching Emacs handler would refuse, so a rejection happens here
// rather than as a warning in the Emacs log after the file is drained.
type Entry interface {
	Validate() error
}

// Create is one "create" entry in a workspace-commands array, carrying the
// fields agent-repl--handle-create-command requires. Name and GitRoot are
// mandatory there (a create missing either is refused and warned about),
// and a create without a Prompt boots an idle workspace, so all three are
// validated here rather than discovered as a warning in the Emacs log.
//
// Name collisions need no handling: Emacs disambiguates the name itself
// via agent-repl--disambiguate-workspace-name.
type Create struct {
	Type    string `json:"type"`
	Name    string `json:"name"`
	GitRoot string `json:"git_root"`
	Prompt  string `json:"prompt"`
}

// NewCreate builds a create entry with the type tag already set.
func NewCreate(name, gitRoot, prompt string) Create {
	return Create{Type: "create", Name: name, GitRoot: gitRoot, Prompt: prompt}
}

func (c Create) Validate() error {
	if c.Type != "create" {
		return fmt.Errorf("workspacecmd: type must be %q, got %q", "create", c.Type)
	}
	if c.Name == "" {
		return fmt.Errorf("workspacecmd: name is required")
	}
	if c.GitRoot == "" {
		return fmt.Errorf("workspacecmd: git_root is required")
	}
	if c.Prompt == "" {
		return fmt.Errorf("workspacecmd: prompt is required")
	}
	return nil
}

// Switch is one "switch" entry: ask Emacs to switch to the workspace
// whose project directory is Dir. Dir is mandatory — a switch without a
// target names no workspace at all.
type Switch struct {
	Type string `json:"type"`
	Dir  string `json:"dir"`
}

// NewSwitch builds a switch entry with the type tag already set.
func NewSwitch(dir string) Switch {
	return Switch{Type: "switch", Dir: dir}
}

func (s Switch) Validate() error {
	if s.Type != "switch" {
		return fmt.Errorf("workspacecmd: type must be %q, got %q", "switch", s.Type)
	}
	if s.Dir == "" {
		return fmt.Errorf("workspacecmd: dir is required")
	}
	return nil
}

// Fold is one "fold" entry: set the folded state of the sidebar repo
// section keyed by RepoKey. Folded is a pointer because false and absent
// are different requests — "unfold" versus "no state supplied" — and an
// absent value is refused rather than defaulted.
type Fold struct {
	Type    string `json:"type"`
	RepoKey string `json:"repo_key"`
	Folded  *bool  `json:"folded"`
}

// NewFold builds a fold entry with the type tag already set.
func NewFold(repoKey string, folded bool) Fold {
	return Fold{Type: "fold", RepoKey: repoKey, Folded: &folded}
}

func (f Fold) Validate() error {
	if f.Type != "fold" {
		return fmt.Errorf("workspacecmd: type must be %q, got %q", "fold", f.Type)
	}
	if f.RepoKey == "" {
		return fmt.Errorf("workspacecmd: repo_key is required")
	}
	if f.Folded == nil {
		return fmt.Errorf("workspacecmd: folded is required")
	}
	return nil
}

// Dir returns the directory Emacs watches for workspace commands.
func Dir() (string, error) {
	root, err := stateroot.Root()
	if err != nil {
		return "", err
	}
	return filepath.Join(root, OutputDirName), nil
}

// Emit writes cmds as one workspace-commands file into dir and returns the
// path written. An empty cmds slice is an error: an empty array would be
// drained and dispatched as nothing, which reads as success while asking
// for nothing at all.
func Emit(dir string, cmds []Entry) (string, error) {
	if len(cmds) == 0 {
		return "", fmt.Errorf("workspacecmd: no commands to emit")
	}
	for i, c := range cmds {
		if err := c.Validate(); err != nil {
			return "", fmt.Errorf("workspacecmd: command %d: %w", i, err)
		}
	}
	payload, err := json.Marshal(cmds)
	if err != nil {
		return "", fmt.Errorf("workspacecmd: marshal: %w", err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("workspacecmd: create %s: %w", dir, err)
	}
	id, err := newID()
	if err != nil {
		return "", err
	}

	// The temp name is dot-prefixed so the Emacs watcher's
	// "^workspace_commands_.*\\.json$" cannot match it before the rename.
	tmp, err := os.CreateTemp(dir, "."+filePrefix+"*.json")
	if err != nil {
		return "", fmt.Errorf("workspacecmd: create temp in %s: %w", dir, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName) // No-op once the rename below succeeds.

	if _, err := tmp.Write(payload); err != nil {
		tmp.Close()
		return "", fmt.Errorf("workspacecmd: write %s: %w", tmpName, err)
	}
	if err := tmp.Close(); err != nil {
		return "", fmt.Errorf("workspacecmd: close %s: %w", tmpName, err)
	}
	path := filepath.Join(dir, filePrefix+id+".json")
	if err := os.Rename(tmpName, path); err != nil {
		return "", fmt.Errorf("workspacecmd: rename to %s: %w", path, err)
	}
	return path, nil
}

// newID mints the filename's uniqueness component. The skill script uses
// uuidgen; any collision-free token serves, since Emacs only matches the
// prefix and the .json suffix.
func newID() (string, error) {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("workspacecmd: mint id: %w", err)
	}
	return hex.EncodeToString(b[:]), nil
}
