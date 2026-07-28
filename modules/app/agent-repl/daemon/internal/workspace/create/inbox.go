package create

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

const commandPrefix = "workspace_commands_"

// errRejectedCommandFile marks a command file the daemon cannot ingest at all
// — unreadable content or a payload that does not parse into commands.  Such a
// file is quarantined next to the inbox as durable evidence instead of being
// re-claimed forever, and it never stops the scan.  Store failures deliberately
// do NOT carry this sentinel: a broken JobStore is structural.
var errRejectedCommandFile = errors.New("workspace create: command file rejected")

// Inbox owns the command-file ingress.  It atomically claims files, persists
// every array entry, then deletes the claim only after persistence succeeds.
// Claimed files are scanned on startup, so a crash never converts a command
// into an untracked vanished request.
type Inbox struct {
	Dir      string
	Store    JobStore
	Manager  *Manager
	Logf     func(string, ...any)
	Interval time.Duration
}

func (i *Inbox) validate() error {
	switch {
	case i.Dir == "":
		return fmt.Errorf("workspace create: inbox directory is required")
	case i.Store == nil:
		return fmt.Errorf("workspace create: inbox needs a JobStore")
	case i.Manager == nil:
		return fmt.Errorf("workspace create: inbox needs a Manager")
	case i.Logf == nil:
		return fmt.Errorf("workspace create: inbox needs a logger")
	}
	return nil
}

// Run startup-drains then actively polls for newly emitted command files.
// Polling keeps the core free of an OS-specific watcher while still providing
// a single durable ingress path on every supported daemon host.
func (i *Inbox) Run(ctx context.Context) error {
	if err := i.validate(); err != nil {
		return err
	}
	if err := i.ScanAndDrain(ctx); err != nil {
		return err
	}
	interval := i.Interval
	if interval <= 0 {
		return fmt.Errorf("workspace create: inbox poll interval must be positive")
	}
	ticker := time.NewTicker(interval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
			if err := i.ScanAndDrain(ctx); err != nil {
				return err
			}
		}
	}
}

// ScanAndDrain handles all visible fresh and previously claimed files, then
// resumes durable work and non-create actions.  It is exported for daemon
// startup and focused tests; Run is the continuous watcher entry point.
func (i *Inbox) ScanAndDrain(ctx context.Context) error {
	if err := i.validate(); err != nil {
		return err
	}
	if err := os.MkdirAll(i.Dir, 0o755); err != nil {
		return fmt.Errorf("workspace create: create inbox directory %s: %w", i.Dir, err)
	}
	entries, err := os.ReadDir(i.Dir)
	if err != nil {
		return fmt.Errorf("workspace create: read inbox directory %s: %w", i.Dir, err)
	}
	paths := make([]string, 0, len(entries))
	for _, entry := range entries {
		if entry.IsDir() || !isCommandFile(entry.Name()) {
			continue
		}
		paths = append(paths, filepath.Join(i.Dir, entry.Name()))
	}
	sort.Strings(paths)
	for _, path := range paths {
		if err := i.claimAndPersist(ctx, path); err != nil {
			// A command file the daemon cannot understand is that FILE's
			// failure, not the inbox's.  It has already been quarantined and
			// logged; the remaining files still get their turn.
			if errors.Is(err, errRejectedCommandFile) {
				i.Logf("workspace-create: REJECTED command file path=%s error=%v quarantined=yes inbox=continuing", path, err)
				continue
			}
			return err
		}
	}
	if err := i.Manager.Resume(ctx); err != nil {
		return err
	}
	return nil
}

func isCommandFile(name string) bool {
	if strings.HasPrefix(name, commandPrefix) && strings.HasSuffix(name, ".json") {
		return true
	}
	return strings.HasPrefix(name, "."+commandPrefix) && strings.HasSuffix(name, ".claimed")
}

func (i *Inbox) claimAndPersist(ctx context.Context, path string) error {
	claimed := path
	if !strings.HasSuffix(path, ".claimed") {
		claimed = "." + filepath.Base(path) + ".claimed"
		claimed = filepath.Join(i.Dir, claimed)
		if err := os.Rename(path, claimed); err != nil {
			if os.IsNotExist(err) {
				return nil
			}
			return fmt.Errorf("workspace create: claim %s: %w", path, err)
		}
		i.Logf("workspace-create: claimed command file path=%s claim=%s", path, claimed)
	}
	payload, err := os.ReadFile(claimed)
	if err != nil {
		return i.quarantine(claimed, fmt.Errorf("%w: read %s: %v", errRejectedCommandFile, claimed, err))
	}
	commands, err := parseCommands(payload)
	if err != nil {
		return i.quarantine(claimed, fmt.Errorf("%w: parse %s: %v", errRejectedCommandFile, claimed, err))
	}
	fileID := commandFileID(claimed)
	for index, command := range commands {
		id := fmt.Sprintf("%s:%d", fileID, index)
		if command.Type == "create" {
			job := Job{ID: id, SourceFile: fileID, SourceIndex: index, Request: command.Create, State: StateQueued}
			if _, _, err := i.Store.Enqueue(job); err != nil {
				return fmt.Errorf("workspace create: persist create entry %d from %s: %w", index, claimed, err)
			}
			continue
		}
		action := HostAction{ID: id, SourceFile: fileID, SourceIndex: index, Type: command.Type, Payload: command.Raw}
		if _, _, err := i.Store.EnqueueHostAction(action); err != nil {
			return fmt.Errorf("workspace create: persist host action entry %d from %s: %w", index, claimed, err)
		}
	}
	if err := os.Remove(claimed); err != nil {
		return fmt.Errorf("workspace create: delete durably ingested claim %s: %w", claimed, err)
	}
	i.Logf("workspace-create: ingested command file id=%s entries=%d", fileID, len(commands))
	return nil
}

// quarantine moves an uningestible claim aside and returns CAUSE unchanged.
// The file is preserved, never deleted: it is the only remaining evidence of
// what the emitter asked for.  Renaming it out of the command-file namespace
// is what stops one bad file from being re-claimed on every poll.  A failed
// rename is structural — the returned error then does NOT wrap the rejection
// sentinel, so the caller treats it as fatal rather than silently spinning.
func (i *Inbox) quarantine(claimed string, cause error) error {
	rejected := claimed + ".rejected"
	if err := os.Rename(claimed, rejected); err != nil {
		return fmt.Errorf("workspace create: quarantine rejected command file %s: %w (cause: %v)", claimed, err, cause)
	}
	i.Logf("workspace-create: QUARANTINED command file claim=%s quarantined=%s cause=%v", claimed, rejected, cause)
	return cause
}

func commandFileID(path string) string {
	name := filepath.Base(path)
	name = strings.TrimPrefix(name, ".")
	name = strings.TrimSuffix(name, ".claimed")
	name = strings.TrimSuffix(name, ".json")
	return name
}

type parsedCommand struct {
	Type   string
	Raw    json.RawMessage
	Create Request
}

// parseCommands audits every type currently in Emacs' workspace-command
// dispatch table.  Non-create commands retain their original JSON byte shape
// for HostActionSink; unknown types are rejected before any durable mutation.
func parseCommands(payload []byte) ([]parsedCommand, error) {
	var raw []json.RawMessage
	if err := json.Unmarshal(payload, &raw); err != nil {
		return nil, err
	}
	if len(raw) == 0 {
		return nil, fmt.Errorf("command array must not be empty")
	}
	commands := make([]parsedCommand, 0, len(raw))
	for index, item := range raw {
		var head struct {
			Type string `json:"type"`
		}
		if err := json.Unmarshal(item, &head); err != nil {
			return nil, fmt.Errorf("entry %d: %w", index, err)
		}
		if head.Type == "" {
			return nil, fmt.Errorf("entry %d: type is required", index)
		}
		command := parsedCommand{Type: head.Type, Raw: append(json.RawMessage(nil), item...)}
		switch head.Type {
		case "create":
			var wire struct {
				Type     string `json:"type"`
				SourceWS *struct {
					Name string `json:"name"`
					Path string `json:"path"`
				} `json:"source_ws"`
				Request
			}
			if err := json.Unmarshal(item, &wire); err != nil {
				return nil, fmt.Errorf("entry %d create: %w", index, err)
			}
			var extra map[string]json.RawMessage
			if err := json.Unmarshal(item, &extra); err != nil {
				return nil, fmt.Errorf("entry %d create metadata: %w", index, err)
			}
			if wire.SourceWS != nil {
				if wire.SourceWS.Name == "" || wire.SourceWS.Path == "" {
					return nil, fmt.Errorf("entry %d create: source_ws requires name and path", index)
				}
				if wire.SourceWorkspace != "" || wire.SourceDir != "" {
					return nil, fmt.Errorf("entry %d create: source_ws cannot be combined with source_workspace or source_dir", index)
				}
				wire.SourceWorkspace = wire.SourceWS.Name
				wire.SourceDir = wire.SourceWS.Path
			}
			if wire.SourceDir != "" {
				canonical, err := canonicalSourceDir(wire.SourceDir)
				if err != nil {
					return nil, fmt.Errorf("entry %d create source directory: %w", index, err)
				}
				wire.SourceDir = canonical
			}
			for _, key := range []string{"type", "name", "git_root", "prompt", "priority", "fork_from", "fork_session_id", "source_workspace", "source_dir", "source_ws", "base_commit", "model", "config_dir", "permission_mode", "allow_ungated", "postprocessing_prompt", "before_ws_merge"} {
				delete(extra, key)
			}
			if len(extra) > 0 {
				encoded, err := json.Marshal(extra)
				if err != nil {
					return nil, fmt.Errorf("entry %d create extra metadata: %w", index, err)
				}
				wire.Extra = encoded
			}
			if err := wire.Request.validate(); err != nil {
				return nil, fmt.Errorf("entry %d: %w", index, err)
			}
			command.Create = wire.Request
		case "prompt", "finish", "close", "open", "clipboard", "send", "merge", "eval", "switch", "fold", "set-view", "task-create", "task-toggle-done", "task-open", "task-add-workspace":
			// The host still owns these UI semantics.  Preserve the complete
			// payload, including intentionally false/zero values, in the durable
			// HostAction record rather than reinterpreting it here.
		default:
			return nil, fmt.Errorf("entry %d: unsupported workspace command type %q", index, head.Type)
		}
		commands = append(commands, command)
	}
	return commands, nil
}

// canonicalSourceDir gives the registry and request store one stable identity
// for a source worktree.  Emacs commonly emits a directory-form path ending
// in '/', whereas registry CWD records are clean paths; accepting both without
// canonicalization would make a valid child-from-child create look unknown.
func canonicalSourceDir(path string) (string, error) {
	if strings.HasPrefix(path, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return "", fmt.Errorf("resolve home directory: %w", err)
		}
		path = filepath.Join(home, strings.TrimPrefix(path, "~/"))
	}
	if !filepath.IsAbs(path) {
		return "", fmt.Errorf("must be absolute or begin with ~/")
	}
	return filepath.Clean(path), nil
}
