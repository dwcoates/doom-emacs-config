package commandfile

import (
	"encoding/json"
	"fmt"

	"claude-repld/internal/dirpath"
)

// The entry types the ingress accepts. They are the shapes the managed
// emit-workspace-commands skill and the workspace-dispatch scripts have always
// written, and every one of them still means exactly what it meant, because a
// producer that predates this rebuild must keep working unchanged.
//
// TypeForget is the one ADDITION. It is here rather than only on the wire
// because a registry record the user wants gone is reachable from a shell — the
// same place the register-then-clean-up cycles that stranded those records run
// from — and an added type breaks no existing producer.
const (
	// TypeCreate materializes a workspace and prompts it.
	TypeCreate = "create"
	// TypePrompt sends a prompt to an existing workspace.
	TypePrompt = "prompt"
	// TypeSend is TypePrompt's older spelling and behaves identically.
	TypeSend = "send"
	// TypeMerge enqueues a workspace's merge.
	TypeMerge = "merge"
	// TypeClose closes a workspace.
	TypeClose = "close"
	// TypeForget removes a CLOSED workspace's registry record, and its
	// repository's record when no other workspace references it. It destroys
	// no files.
	TypeForget = "forget"
	// TypeOpen re-opens a closed workspace.
	TypeOpen = "open"
	// TypeSwitch selects a workspace.
	TypeSwitch = "switch"
	// TypeTaskCreate creates a task.
	TypeTaskCreate = "task-create"
	// TypeTaskToggleDone flips a task's done flag.
	TypeTaskToggleDone = "task-toggle-done"
	// TypeTaskAddWorkspace assigns a workspace to a task.
	TypeTaskAddWorkspace = "task-add-workspace"
)

// Entry is one decoded command-file entry. Every field the accepted types use
// lives here, because the file is one JSON array of heterogeneous objects and
// splitting it into per-type structs would need a second decode pass over
// bytes that are already in memory.
//
// Pointer fields distinguish ABSENT from ZERO: "done": false and no "done" at
// all are different requests, and an absent value is refused rather than
// defaulted.
type Entry struct {
	// Type names the command.
	Type string `json:"type"`
	// Name is a create's workspace name.
	Name string `json:"name"`
	// GitRoot is a create's repository.
	GitRoot string `json:"git_root"`
	// Prompt is a create's or a prompt's text.
	Prompt string `json:"prompt"`
	// ProjectDir is THE CANONICAL WORKSPACE KEY: the absolute path of the
	// target workspace's git worktree root.
	//
	// THE PRODUCER'S CONTRACT IS THE SOURCE OF TRUTH, and this reader conforms
	// to it (owner ruling, 2026-09-21). The `/create-or-update-workspace`
	// skill REQUIRES `project_dir` on every entry that targets an existing
	// workspace — merge, prompt, close, open, send — and states that the
	// `workspace` NAME beside it "is retained only as a display/logging field
	// and is never used to resolve the target". This reader used to read the
	// directory from `dir` alone and `workspace` as an ID, so every
	// skill-dispatched merge was refused `unknown_workspace` and quarantined
	// where nobody saw it (a one-shot's merge, 2026-09-21; 33 files by then).
	ProjectDir string `json:"project_dir"`
	// Workspace is the workspace's NAME when ProjectDir (or Dir) is set — for
	// display and logging only, never for resolution. Only an entry that
	// carries NO directory at all is resolved by it, as an id, which is what
	// this module's own producers write.
	Workspace string `json:"workspace"`
	// Dir is the older spelling of ProjectDir, still written by producers that
	// predate the skill's contract. ProjectDir wins when both are set.
	Dir string `json:"dir"`
	// ID is a task id.
	ID string `json:"id"`
	// Title is a task's title.
	Title string `json:"title"`
	// Done is a task's done flag, nil when the entry does not set it.
	Done *bool `json:"done"`
	// OneShot marks a create as the one-shot form.
	OneShot bool `json:"one_shot"`
	// BaseRef is a create's base.
	BaseRef string `json:"base_ref"`
}

// Validate refuses an entry the ingress could only act on by guessing. A file
// with ONE invalid entry applies NOTHING: the array is one request, and half of
// it is not a smaller request.
func (e Entry) Validate() error {
	switch e.Type {
	case "":
		return fmt.Errorf("an entry must name a type")
	case TypeCreate:
		if e.GitRoot == "" {
			return fmt.Errorf("%s: git_root is required", TypeCreate)
		}
		if e.Name == "" && e.Prompt == "" {
			return fmt.Errorf("%s: a name or a prompt is required", TypeCreate)
		}
		return nil
	case TypePrompt, TypeSend:
		if err := e.requireTarget(); err != nil {
			return err
		}
		if e.Prompt == "" {
			return fmt.Errorf("%s: prompt is required", e.Type)
		}
		return nil
	case TypeMerge, TypeClose, TypeForget, TypeOpen, TypeSwitch:
		return e.requireTarget()
	case TypeTaskCreate:
		if e.Title == "" {
			return fmt.Errorf("%s: title is required", TypeTaskCreate)
		}
		return nil
	case TypeTaskToggleDone:
		if e.ID == "" {
			return fmt.Errorf("%s: id is required", TypeTaskToggleDone)
		}
		if e.Done == nil {
			return fmt.Errorf("%s: done is required", TypeTaskToggleDone)
		}
		return nil
	case TypeTaskAddWorkspace:
		if e.ID == "" {
			return fmt.Errorf("%s: id is required", TypeTaskAddWorkspace)
		}
		return e.requireTarget()
	default:
		return fmt.Errorf("unknown entry type %q", e.Type)
	}
}

// requireTarget refuses an entry that names no workspace at all.
func (e Entry) requireTarget() error {
	if e.Workspace == "" && e.TargetDir() == "" {
		return fmt.Errorf("%s: a project_dir (or a dir, or a workspace id) is required", e.Type)
	}
	return nil
}

// TargetDir is the directory that keys the entry's workspace: `project_dir`,
// else the older `dir`, else empty.
func (e Entry) TargetDir() string {
	if e.ProjectDir != "" {
		return e.ProjectDir
	}
	return e.Dir
}

// parse decodes one command file's whole array, all-or-nothing. Trailing bytes
// after the array are a decode failure rather than something to ignore: a file
// that is still being written often ends mid-token, and tolerating a partial
// document is exactly how a half-written file gets ingested.
func parse(data []byte) ([]Entry, error) {
	var entries []Entry
	if err := json.Unmarshal(data, &entries); err != nil {
		return nil, fmt.Errorf("decode the command array: %w", err)
	}
	if len(entries) == 0 {
		return nil, fmt.Errorf("the command array is empty, which asks for nothing")
	}
	for i, entry := range entries {
		if err := entry.Validate(); err != nil {
			return nil, fmt.Errorf("entry %d: %w", i, err)
		}
	}
	return entries, nil
}

// withAbsolutePaths answers the entry with every directory field it carries
// made absolute through dirpath.Absolute: a leading `~` expanded to home, and a
// path still relative afterwards refused. The skill's contract says a leading
// `~` is expanded downstream, and the daemon's working directory is no base to
// resolve anything else against.
func (e Entry) withAbsolutePaths(home string) (Entry, error) {
	fields := []struct {
		name  string
		value *string
	}{
		{name: "git_root", value: &e.GitRoot},
		{name: "project_dir", value: &e.ProjectDir},
		{name: "dir", value: &e.Dir},
	}
	for _, field := range fields {
		if *field.value == "" {
			continue
		}
		abs, err := dirpath.Absolute(*field.value, home)
		if err != nil {
			return Entry{}, fmt.Errorf("%s: %s: %w", e.Type, field.name, err)
		}
		*field.value = abs
	}
	return e, nil
}

// decode parses one command file and resolves every entry's directories,
// all-or-nothing: an entry whose directory cannot be resolved refuses the
// whole array, exactly as an entry that does not validate does.
func decode(data []byte, home string) ([]Entry, error) {
	entries, err := parse(data)
	if err != nil {
		return nil, err
	}
	for i, entry := range entries {
		resolved, err := entry.withAbsolutePaths(home)
		if err != nil {
			return nil, fmt.Errorf("entry %d: %w", i, err)
		}
		entries[i] = resolved
	}
	return entries, nil
}
