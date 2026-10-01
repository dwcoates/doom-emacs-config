package commandfile

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/wsm"
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

	// THE MERGE QUEUE'S CONTROLS, mirroring UpdateMergeQueue's three arms
	// (endpoint_update_merge_queue.proto) and calling the same orchestrator
	// entry points it calls. Each entry's workspace is the REQUESTER, the one
	// whose agent wrote it, exactly as a merge's is.

	// TypeMergeEvict takes one workspace's merge off the queue
	// (UpdateMergeQueueEvict): the requester's own, or the one at evict_dir.
	TypeMergeEvict = "merge_evict"
	// TypeMergePause stops the queue admitting merges (UpdateMergeQueuePause):
	// the repository at repository_dir, or every repository when it is unset.
	TypeMergePause = "merge_pause"
	// TypeMergeResume resumes admitting merges (UpdateMergeQueueResume),
	// scoped exactly as TypeMergePause is.
	TypeMergeResume = "merge_resume"
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

	// THE SKILL'S CREATE FIELDS. The /create-or-update-workspace skill has
	// written every one of these on its creates since before this ingress
	// existed, and the CreateWorkspace rpc has always carried their meaning;
	// this reader used to decode none of them, and encoding/json dropped each
	// without a word (a `--master` create's base, 2026-09-28). Each is
	// create-only, and each maps onto the same CreateSpec field the rpc fills.

	// SourceWS is the workspace the create was spawned from. Its path is the
	// key; its name is for display only, as `workspace` beside `project_dir`
	// is. The repository's main checkout means no parent; any other
	// registered workspace is the create's PARENT, which is its merge target
	// and its roster nesting (CreateSpec.Parent).
	SourceWS *SourceWorkspace `json:"source_ws,omitempty"`
	// BaseCommit is the skill's spelling of BaseRef.
	BaseCommit string `json:"base_commit,omitempty"`
	// ForkFrom NAMES the workspace whose conversation the create forks. A fork
	// is always from the parent (CreateWorkspaceParent.fork), so it is the
	// create's parent too.
	ForkFrom string `json:"fork_from,omitempty"`
	// Model is the model the session starts under; blank is the default.
	Model string `json:"model,omitempty"`
	// Priority is the roster priority: p05, p1, p2 or p3.
	Priority string `json:"priority,omitempty"`
	// BeforeWSMerge is prompt text run in the workspace before its merge
	// (CreateWorkspaceMergeActions.before_ws_merge).
	BeforeWSMerge string `json:"before_ws_merge,omitempty"`
	// PostprocessingPrompt is prompt text run after the merge lands
	// (CreateWorkspaceMergeActions.postprocessing_prompt).
	PostprocessingPrompt string `json:"postprocessing_prompt,omitempty"`

	// THE MERGE'S SOURCE (agentrepl.v1.MergeWorkspaceSource). The entry's
	// workspace is the REQUESTER, which the merge runs in; these name what it
	// merges. None set is the requester's own branch. Each is merge-only.

	// Branch merges a branch that is no workspace (source.branch).
	Branch string `json:"branch,omitempty"`
	// SourceDir merges another workspace's branch, the workspace keyed by its
	// worktree path (source.workspace).
	SourceDir string `json:"source_dir,omitempty"`
	// KeepOpen keeps the requester open once its own branch lands
	// (source.own_branch.keep_open).
	KeepOpen bool `json:"keep_open,omitempty"`
	// PRWasMerged says the requester's own branch already merged upstream
	// through its pull request (source.merged_upstream): the default branch
	// is updated from upstream and the requester closed.
	PRWasMerged bool `json:"pr_was_merged,omitempty"`

	// THE MERGE QUEUE CONTROLS' FIELDS.

	// EvictDir names ANOTHER workspace, by its worktree root, whose merge a
	// merge_evict takes off the queue (UpdateMergeQueueEvict.workspace).
	// Unset evicts the requester's own merge. Evict-only.
	EvictDir string `json:"evict_dir,omitempty"`
	// RepositoryDir names the repository, by its main checkout, whose queue a
	// merge_pause or merge_resume addresses (RepositoryRef.dir). Unset is
	// every repository, the daemon-wide switch the rpc's unset ref means.
	// Pause- and resume-only.
	RepositoryDir string `json:"repository_dir,omitempty"`
}

// SourceWorkspace is a create's `source_ws`: the workspace it was spawned
// from, keyed by its worktree path.
type SourceWorkspace struct {
	// Name is the source workspace's display name, never resolved.
	Name string `json:"name"`
	// Path is the source workspace's worktree root: the key.
	Path string `json:"path"`
}

// priorities spells each `priority` value the skill writes as the registry's.
var priorities = map[string]wsm.Priority{
	"p05": wsm.PriorityP05,
	"p1":  wsm.PriorityP1,
	"p2":  wsm.PriorityP2,
	"p3":  wsm.PriorityP3,
}

// Validate refuses an entry the ingress could only act on by guessing. A file
// with ONE invalid entry applies NOTHING: the array is one request, and half of
// it is not a smaller request.
func (e Entry) Validate() error {
	if e.Type != TypeCreate && e.Type != "" {
		if field := e.createOnly(); field != "" {
			return fmt.Errorf("%s: %s is a create's field, and no %s reads it", e.Type, field, e.Type)
		}
	}
	if e.Type != TypeMerge && e.Type != "" {
		if field := e.mergeOnly(); field != "" {
			return fmt.Errorf("%s: %s is a merge's field, and no %s reads it", e.Type, field, e.Type)
		}
	}
	if e.Type != TypeMergeEvict && e.EvictDir != "" && e.Type != "" {
		return fmt.Errorf("%s: evict_dir is a %s's field, and no %s reads it", e.Type, TypeMergeEvict, e.Type)
	}
	if e.Type != TypeMergePause && e.Type != TypeMergeResume && e.RepositoryDir != "" && e.Type != "" {
		return fmt.Errorf("%s: repository_dir is a %s's or a %s's field, and no %s reads it", e.Type, TypeMergePause, TypeMergeResume, e.Type)
	}
	switch e.Type {
	case "":
		return fmt.Errorf("an entry must name a type")
	case TypeCreate:
		return e.validateCreate()
	case TypePrompt, TypeSend:
		if err := e.requireTarget(); err != nil {
			return err
		}
		if e.Prompt == "" {
			return fmt.Errorf("%s: prompt is required", e.Type)
		}
		return nil
	case TypeMerge:
		if err := e.requireTarget(); err != nil {
			return err
		}
		return e.validateMergeSource()
	case TypeClose, TypeForget, TypeOpen, TypeSwitch, TypeMergeEvict, TypeMergePause, TypeMergeResume:
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

// validateCreate refuses a create whose fields contradict each other or name
// a value no create can take.
func (e Entry) validateCreate() error {
	switch {
	case e.GitRoot == "":
		return fmt.Errorf("%s: git_root is required", TypeCreate)
	case e.Name == "" && e.Prompt == "":
		return fmt.Errorf("%s: a name or a prompt is required", TypeCreate)
	case e.BaseRef != "" && e.BaseCommit != "":
		return fmt.Errorf("%s: base_ref and base_commit are two spellings of one base; name it once", TypeCreate)
	case e.ForkFrom != "" && e.base() != "":
		return fmt.Errorf("%s: a fork_from create takes no base, and this one names %q", TypeCreate, e.base())
	case e.SourceWS != nil && e.SourceWS.Path == "":
		return fmt.Errorf("%s: source_ws.path is required when source_ws is given", TypeCreate)
	}
	if _, ok := priorities[e.Priority]; e.Priority != "" && !ok {
		return fmt.Errorf("%s: priority %q is none of p05, p1, p2, p3", TypeCreate, e.Priority)
	}
	return nil
}

// base is the create's base under whichever spelling it came in.
func (e Entry) base() string {
	if e.BaseCommit != "" {
		return e.BaseCommit
	}
	return e.BaseRef
}

// createOnly names the first create-only field a non-create entry carries,
// empty for none. Such a field would be dropped by every other verb, and a
// dropped field is exactly the silence this reader refuses.
func (e Entry) createOnly() string {
	fields := []struct {
		name string
		set  bool
	}{
		{"git_root", e.GitRoot != ""},
		{"name", e.Name != ""},
		{"one_shot", e.OneShot},
		{"base_ref", e.BaseRef != ""},
		{"source_ws", e.SourceWS != nil},
		{"base_commit", e.BaseCommit != ""},
		{"fork_from", e.ForkFrom != ""},
		{"model", e.Model != ""},
		{"priority", e.Priority != ""},
		{"before_ws_merge", e.BeforeWSMerge != ""},
		{"postprocessing_prompt", e.PostprocessingPrompt != ""},
	}
	for _, field := range fields {
		if field.set {
			return field.name
		}
	}
	return ""
}

// mergeOnly names the first merge-only field a non-merge entry carries, empty
// for none.
func (e Entry) mergeOnly() string {
	fields := []struct {
		name string
		set  bool
	}{
		{"branch", e.Branch != ""},
		{"source_dir", e.SourceDir != ""},
		{"keep_open", e.KeepOpen},
		{"pr_was_merged", e.PRWasMerged},
	}
	for _, field := range fields {
		if field.set {
			return field.name
		}
	}
	return ""
}

// validateMergeSource refuses a merge naming more than one source, or keeping
// open a requester whose own branch it does not merge.
func (e Entry) validateMergeSource() error {
	named := 0
	for _, set := range []bool{e.Branch != "", e.SourceDir != "", e.PRWasMerged} {
		if set {
			named++
		}
	}
	switch {
	case named > 1:
		return fmt.Errorf("%s: branch, source_dir and pr_was_merged each name a different merge; name one", TypeMerge)
	case e.KeepOpen && named > 0:
		return fmt.Errorf("%s: keep_open keeps the requester open after its OWN branch lands, and this merge lands another", TypeMerge)
	}
	return nil
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
//
// AN UNKNOWN FIELD IS A DECODE FAILURE TOO. encoding/json drops one without a
// word, and that is how every field the skill wrote beyond this struct went
// missing for a month; the held-prompt ingress refuses unknown fields for the
// same reason.
func parse(data []byte) ([]Entry, error) {
	var entries []Entry
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&entries); err != nil {
		return nil, fmt.Errorf("decode the command array: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return nil, fmt.Errorf("decode the command array: bytes follow the array")
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
		{name: "source_dir", value: &e.SourceDir},
		{name: "evict_dir", value: &e.EvictDir},
		{name: "repository_dir", value: &e.RepositoryDir},
	}
	if e.SourceWS != nil {
		// A COPY, so resolving the path never writes through to the entry
		// the caller still holds.
		source := *e.SourceWS
		e.SourceWS = &source
		fields = append(fields, struct {
			name  string
			value *string
		}{name: "source_ws.path", value: &e.SourceWS.Path})
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
