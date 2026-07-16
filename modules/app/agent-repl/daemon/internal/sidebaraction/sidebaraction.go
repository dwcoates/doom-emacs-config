// Package sidebaraction emits sidebar actions for the Emacs side to
// execute. Sidebar actions mutate Emacs-owned state (visit, kill,
// merge, …), so the daemon has no say in the outcome: it validates
// shape only and asks by dropping one JSON file per action into
// $AGENT_REPL_STATE_DIR/sidebar-actions/, which Emacs watches,
// executes, deletes, and answers through the next snapshot's
// last_action_result — keyed by the id minted here. Writing the file
// IS the request; there is no reply on this channel.
//
// Atomicity matters: Emacs's watcher fires on any file matching
// sidebar_action_*.json, so a half-written file would be drained and
// fail to parse. Every write therefore lands on a dot-prefixed temp
// name the watcher's regexp cannot match, then renames into place —
// the same discipline as workspacecmd.
package sidebaraction

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/stateroot"
)

// DirName is the component under the state root that Emacs watches.
// It MUST match `agent-repl-sidebar-actions-dir' in sidebar.el.
const DirName = "sidebar-actions"

// filePrefix MUST match the Emacs watcher's regexp in sidebar.el, which
// matches sidebar_action_*.json.
const filePrefix = "sidebar_action_"

// Action is one sidebar-action file's payload, per the "Sidebar action
// files" section of shared/protocol.md. The daemon validates only that
// an action is named (and that an id exists to name the file); which
// actions exist, what their targets and args mean, and whether the
// action is even permitted are all Emacs's business, reported back
// through last_action_result rather than judged here.
type Action struct {
	// ID is minted by the caller via NewID, names the file, and is echoed
	// by Emacs in last_action_result — the correlation key that lets a
	// frontend match the outcome to its request.
	ID string `json:"id"`
	// Action names the operation (visit, kill, send-prompt, …).
	Action string `json:"action"`
	// Targets carries workspace names (repo keys for toggle-fold); empty
	// for global actions like clear-marks.
	Targets []string `json:"targets"`
	// Args carries action-specific parameters (send-prompt's prompt).
	Args map[string]any `json:"args"`
	// Confirmed marks the user as having answered a destructive
	// confirmation prompt (finishing a MERGED workspace).
	Confirmed bool `json:"confirmed"`
	// TS is the emit time in unix seconds, stamped by the caller.
	TS int64 `json:"ts"`
}

func (a Action) validate() error {
	if a.ID == "" {
		return fmt.Errorf("sidebaraction: id is required (mint one with NewID)")
	}
	if a.Action == "" {
		return fmt.Errorf("sidebaraction: action is required")
	}
	return nil
}

// Dir returns the directory Emacs watches for sidebar actions.
func Dir() (string, error) {
	root, err := stateroot.Root()
	if err != nil {
		return "", err
	}
	return filepath.Join(root, DirName), nil
}

// Emit writes a as one sidebar-action file into dir and returns the
// path written: dir/sidebar_action_<a.ID>.json. Nil Targets and Args
// are normalized to their empty forms first, so the file always carries
// a JSON array and object where the contract shows containers — Emacs
// never sees null there.
func Emit(dir string, a Action) (string, error) {
	if err := a.validate(); err != nil {
		return "", err
	}
	if a.Targets == nil {
		a.Targets = []string{}
	}
	if a.Args == nil {
		a.Args = map[string]any{}
	}
	payload, err := json.Marshal(a)
	if err != nil {
		return "", fmt.Errorf("sidebaraction: marshal: %w", err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("sidebaraction: create %s: %w", dir, err)
	}

	// The temp name is dot-prefixed so the Emacs watcher's
	// "^sidebar_action_.*\\.json$" cannot match it before the rename.
	tmp, err := os.CreateTemp(dir, "."+filePrefix+"*.json")
	if err != nil {
		return "", fmt.Errorf("sidebaraction: create temp in %s: %w", dir, err)
	}
	tmpName := tmp.Name()
	defer os.Remove(tmpName) // No-op once the rename below succeeds.

	if _, err := tmp.Write(payload); err != nil {
		tmp.Close()
		return "", fmt.Errorf("sidebaraction: write %s: %w", tmpName, err)
	}
	if err := tmp.Close(); err != nil {
		return "", fmt.Errorf("sidebaraction: close %s: %w", tmpName, err)
	}
	path := filepath.Join(dir, filePrefix+a.ID+".json")
	if err := os.Rename(tmpName, path); err != nil {
		return "", fmt.Errorf("sidebaraction: rename to %s: %w", path, err)
	}
	return path, nil
}

// NewID mints an action id: the filename's uniqueness component and the
// correlation key last_action_result echoes back. Any collision-free
// token serves, since Emacs only matches the prefix and the .json
// suffix.
func NewID() (string, error) {
	var b [16]byte
	if _, err := rand.Read(b[:]); err != nil {
		return "", fmt.Errorf("sidebaraction: mint id: %w", err)
	}
	return hex.EncodeToString(b[:]), nil
}
