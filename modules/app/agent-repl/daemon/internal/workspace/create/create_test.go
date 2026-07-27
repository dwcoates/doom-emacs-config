package create

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"testing"
)

type fakeWorktrees struct {
	calls int
	path  string
	err   error
}

func (f *fakeWorktrees) EnsureWorktree(_ context.Context, _ Job) (string, error) {
	f.calls++
	return f.path, f.err
}

type fakeSessions struct {
	calls int
	id    string
	err   error
}

func (f *fakeSessions) EnsureSession(_ context.Context, _ Job) (string, error) {
	f.calls++
	return f.id, f.err
}

type fakeHealth struct {
	calls int
	err   error
}

func (f *fakeHealth) AwaitHealthy(_ context.Context, _ Job) error {
	f.calls++
	return f.err
}

type fakePrompts struct {
	calls int
	jobs  []string
	err   error
}

func (f *fakePrompts) SubmitInitialPrompt(_ context.Context, job Job) error {
	f.calls++
	f.jobs = append(f.jobs, job.ID)
	return f.err
}

type fakeAvailable struct {
	calls int
	items []Available
	err   error
}

func (f *fakeAvailable) PublishWorkspaceAvailable(_ context.Context, available Available) error {
	f.calls++
	f.items = append(f.items, available)
	return f.err
}

type fakeActions struct {
	calls int
	items []HostAction
	err   error
}

func (f *fakeActions) PublishHostAction(_ context.Context, action HostAction) error {
	f.calls++
	f.items = append(f.items, action)
	return f.err
}

type fixture struct {
	store     *FileJobStore
	manager   *Manager
	worktrees *fakeWorktrees
	sessions  *fakeSessions
	health    *fakeHealth
	prompts   *fakePrompts
	available *fakeAvailable
	actions   *fakeActions
	logs      []string
}

func newFixture(t *testing.T, statePath string) *fixture {
	t.Helper()
	f := &fixture{
		worktrees: &fakeWorktrees{path: "/worktrees/new"},
		sessions:  &fakeSessions{id: "s_new"},
		health:    &fakeHealth{},
		prompts:   &fakePrompts{},
		available: &fakeAvailable{},
		actions:   &fakeActions{},
	}
	logf := func(format string, args ...any) { f.logs = append(f.logs, format) }
	store, err := OpenJobStore(statePath, logf)
	if err != nil {
		t.Fatalf("OpenJobStore: %v", err)
	}
	f.store = store
	f.manager, err = NewManager(Config{
		Store: store, Worktrees: f.worktrees, Sessions: f.sessions, Health: f.health,
		Prompts: f.prompts, Available: f.available, HostActions: f.actions, Logf: logf,
	})
	if err != nil {
		t.Fatalf("NewManager: %v", err)
	}
	return f
}

func (f *fixture) inbox(dir string) *Inbox {
	return &Inbox{Dir: dir, Store: f.store, Manager: f.manager, Logf: func(string, ...any) {}, Interval: 1}
}

func writeCommandFile(t *testing.T, dir, name, body string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("MkdirAll: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}
}

func job(t *testing.T, store JobStore, id string) Job {
	t.Helper()
	got, ok, err := store.Get(id)
	if err != nil {
		t.Fatalf("Get: %v", err)
	}
	if !ok {
		t.Fatalf("job %s missing", id)
	}
	return got
}

func TestInboxCrashRestartResumesClaimedInFlightJob(t *testing.T) {
	root := t.TempDir()
	state := filepath.Join(root, "state", "jobs.json")
	inboxDir := filepath.Join(root, "output")
	// A dot-prefixed claim is exactly the crash residue after rename and before
	// the file's entries were persisted.  A fresh daemon must consume it.
	writeCommandFile(t, inboxDir, ".workspace_commands_recover.json.claimed", `[{"type":"create","name":"DWC/recover","git_root":"/repo"}]`)
	f := newFixture(t, state)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}
	got := job(t, f.store, "workspace_commands_recover:0")
	if got.State != StateAwaitingEmacs || got.WorktreePath != "/worktrees/new" || got.SessionID != "s_new" {
		t.Fatalf("resumed job = %#v", got)
	}
	if f.worktrees.calls != 1 || f.sessions.calls != 1 || f.health.calls != 1 || f.available.calls != 1 {
		t.Fatalf("calls worktrees=%d sessions=%d health=%d available=%d", f.worktrees.calls, f.sessions.calls, f.health.calls, f.available.calls)
	}
	// Reopening the durable store simulates another daemon restart.  Awaiting
	// Emacs is a stable boundary, so no creation effect may run again.
	restarted := newFixture(t, state)
	if err := restarted.manager.Resume(context.Background()); err != nil {
		t.Fatalf("Resume after restart: %v", err)
	}
	if restarted.worktrees.calls != 0 || restarted.sessions.calls != 0 || restarted.available.calls != 0 {
		t.Fatalf("restart repeated effects: worktrees=%d sessions=%d available=%d", restarted.worktrees.calls, restarted.sessions.calls, restarted.available.calls)
	}
}

func TestInboxDuplicateFileMaterializesExactlyOneWorkspace(t *testing.T) {
	root := t.TempDir()
	state, inboxDir := filepath.Join(root, "jobs.json"), filepath.Join(root, "output")
	f := newFixture(t, state)
	body := `[{"type":"create","name":"DWC/duplicate","git_root":"/repo"}]`
	writeCommandFile(t, inboxDir, "workspace_commands_same.json", body)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatal(err)
	}
	// The exact same filename+index is redelivered after a producer retry.
	writeCommandFile(t, inboxDir, "workspace_commands_same.json", body)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatal(err)
	}
	if f.worktrees.calls != 1 || f.sessions.calls != 1 || f.available.calls != 1 {
		t.Fatalf("duplicate caused repeated effects: worktrees=%d sessions=%d available=%d", f.worktrees.calls, f.sessions.calls, f.available.calls)
	}
	jobs, err := f.store.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(jobs) != 1 {
		t.Fatalf("jobs = %d, want 1", len(jobs))
	}
}

func TestInboxBatchRoutesCreatesAndPreservesUIActions(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_batch.json", `[
  {"type":"create","name":"DWC/one","git_root":"/repo","prompt":"one"},
  {"type":"switch","dir":"/worktrees/one"},
  {"type":"create","name":"DWC/two","git_root":"/repo"},
  {"type":"task-toggle-done","id":"t-7"}
]`)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatal(err)
	}
	jobs, err := f.store.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(jobs) != 2 {
		t.Fatalf("jobs = %d, want 2", len(jobs))
	}
	if f.actions.calls != 2 {
		t.Fatalf("host action calls = %d, want 2", f.actions.calls)
	}
	var first map[string]any
	if err := json.Unmarshal(f.actions.items[0].Payload, &first); err != nil {
		t.Fatal(err)
	}
	if first["type"] != "switch" || first["dir"] != "/worktrees/one" {
		t.Fatalf("first action = %#v", first)
	}
	if f.worktrees.calls != 2 || f.sessions.calls != 2 || f.health.calls != 2 {
		t.Fatalf("creation effects were not per create entry")
	}
}

func TestZeroPromptWaitsForMaterializationButNeverSubmits(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	_, inserted, err := f.store.Enqueue(Job{ID: "zero", Request: Request{Name: "DWC/zero", GitRoot: "/repo"}, State: StateQueued})
	if err != nil || !inserted {
		t.Fatalf("Enqueue = %v, %t", err, inserted)
	}
	if err := f.manager.Process(context.Background(), "zero"); err != nil {
		t.Fatal(err)
	}
	if got := job(t, f.store, "zero"); got.State != StateAwaitingEmacs {
		t.Fatalf("state before ack = %s", got.State)
	}
	if err := f.manager.MarkMaterialized(context.Background(), "zero"); err != nil {
		t.Fatal(err)
	}
	got := job(t, f.store, "zero")
	if got.State != StateReady || got.PromptDelivered || f.prompts.calls != 0 {
		t.Fatalf("zero prompt result=%#v calls=%d", got, f.prompts.calls)
	}
}

func TestHealthFailureIsDurablyFailedAndNeverAvailable(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	f.health.err = errors.New("shim unavailable")
	if _, _, err := f.store.Enqueue(Job{ID: "bad-health", Request: Request{Name: "DWC/health", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "bad-health"); err == nil {
		t.Fatal("Process succeeded, want health failure")
	}
	got := job(t, f.store, "bad-health")
	if got.State != StateFailed || got.LastError == "" || f.available.calls != 0 {
		t.Fatalf("health failure job=%#v available=%d", got, f.available.calls)
	}
}

func TestMaterializationAckIsIdempotentAndDeliversPromptOnce(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "prompt", Request: Request{Name: "DWC/prompt", GitRoot: "/repo", Prompt: "build it"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "prompt"); err != nil {
		t.Fatal(err)
	}
	if f.prompts.calls != 0 {
		t.Fatal("prompt submitted before materialization")
	}
	if err := f.manager.MarkMaterialized(context.Background(), "prompt"); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.MarkMaterialized(context.Background(), "prompt"); err != nil {
		t.Fatal(err)
	}
	got := job(t, f.store, "prompt")
	if f.prompts.calls != 1 || !got.PromptDelivered || got.State != StateReady {
		t.Fatalf("prompt calls=%d job=%#v", f.prompts.calls, got)
	}
	if !reflect.DeepEqual(f.prompts.jobs, []string{"prompt"}) {
		t.Fatalf("prompt jobs = %#v", f.prompts.jobs)
	}
}

func TestParseCommandsAuditsEveryKnownCommandType(t *testing.T) {
	payload := `[
 {"type":"create","name":"DWC/a","git_root":"/repo"}, {"type":"prompt"}, {"type":"finish"}, {"type":"close"}, {"type":"open"}, {"type":"clipboard"}, {"type":"send","data":false}, {"type":"merge"}, {"type":"eval"}, {"type":"switch"}, {"type":"fold","folded":false}, {"type":"set-view"}, {"type":"task-create"}, {"type":"task-toggle-done"}, {"type":"task-open"}, {"type":"task-add-workspace"}
]`
	commands, err := parseCommands([]byte(payload))
	if err != nil {
		t.Fatalf("parseCommands: %v", err)
	}
	if len(commands) != 16 {
		t.Fatalf("commands = %d, want 16", len(commands))
	}
	if _, err := parseCommands([]byte(`[{"type":"unknown"}]`)); err == nil {
		t.Fatal("unknown command was accepted")
	}
}
