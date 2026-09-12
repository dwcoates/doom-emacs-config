package commandfile

import (
	"encoding/json"
	"fmt"
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
	// Workspace names an existing workspace by id.
	Workspace string `json:"workspace"`
	// Dir names an existing workspace by directory, which is what the older
	// producers carry.
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
	if e.Workspace == "" && e.Dir == "" {
		return fmt.Errorf("%s: a workspace or a dir is required", e.Type)
	}
	return nil
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
