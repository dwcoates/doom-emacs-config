package create

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"sync"
	"testing"
	"time"
)

type fakeWorktrees struct {
	calls  int
	plans  int
	path   string
	name   string
	branch string
	base   string
	err    error
}

func (f *fakeWorktrees) PlanWorktree(_ context.Context, job Job) (WorktreeResult, error) {
	f.plans++
	return WorktreeResult{
		Path:       f.path,
		FinalName:  firstNonEmpty(f.name, job.Request.Name),
		Branch:     firstNonEmpty(f.branch, job.Request.Name),
		BaseCommit: firstNonEmpty(f.base, job.Request.BaseCommit, "HEAD"),
	}, f.err
}

func (f *fakeWorktrees) EnsureWorktree(_ context.Context, _ Job) error {
	f.calls++
	return f.err
}

func firstNonEmpty(values ...string) string {
	for _, value := range values {
		if value != "" {
			return value
		}
	}
	return ""
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
		Store: store, Planner: f.worktrees, Worktrees: f.worktrees, Sessions: f.sessions, Health: f.health,
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
	if got.State != StateAwaitingEmacs || got.WorktreePath != "/worktrees/new" || got.FinalName != "DWC/recover" || got.SessionID != "s_new" {
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

func TestResolvedWorktreeIdentitySurvivesRestart(t *testing.T) {
	root := t.TempDir()
	state := filepath.Join(root, "jobs.json")
	f := newFixture(t, state)
	f.worktrees.name, f.worktrees.branch, f.worktrees.base = "DWC/requested-abc", "DWC/requested-abc", "origin/main"
	if _, _, err := f.store.Enqueue(Job{ID: "resolved", Request: Request{Name: "DWC/requested", GitRoot: "/repo", BaseCommit: "origin/main"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "resolved"); err != nil {
		t.Fatal(err)
	}
	restarted := newFixture(t, state)
	got := job(t, restarted.store, "resolved")
	if got.FinalName != "DWC/requested-abc" || got.Branch != "DWC/requested-abc" || got.ResolvedBaseCommit != "origin/main" {
		t.Fatalf("persisted identity = %#v", got)
	}
}

func TestCrashAfterIdentityCheckpointResumesWithoutAnotherPlan(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	// This is the exact durable state after planning but before git mutates the
	// repository.  A restarted daemon must reuse it rather than resolve a new
	// suffix after seeing the branch/path its interrupted predecessor created.
	seed := Job{ID: "planned", Request: Request{Name: "DWC/requested", GitRoot: "/repo", BaseCommit: "HEAD"}, State: StateWorktreeCreating, WorktreePath: "/worktrees/requested-abc", FinalName: "DWC/requested-abc", Branch: "DWC/requested-abc", ResolvedBaseCommit: "HEAD"}
	if _, _, err := f.store.Enqueue(seed); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "planned"); err != nil {
		t.Fatal(err)
	}
	if f.worktrees.plans != 0 || f.worktrees.calls != 1 {
		t.Fatalf("resume replanned=%d create_calls=%d", f.worktrees.plans, f.worktrees.calls)
	}
}

type reentrantAvailable struct {
	manager *Manager
	seen    int
}

func (p *reentrantAvailable) PublishWorkspaceAvailable(ctx context.Context, available Available) error {
	p.seen++
	return p.manager.MarkMaterialized(ctx, available.JobID)
}

func TestAvailableStateIsPersistedBeforeAReentrantMaterializationAck(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	publisher := &reentrantAvailable{manager: f.manager}
	f.manager.cfg.Available = publisher
	if _, _, err := f.store.Enqueue(Job{ID: "ack-race", Request: Request{Name: "DWC/ack", GitRoot: "/repo", Prompt: "begin"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "ack-race"); err != nil {
		t.Fatal(err)
	}
	got := job(t, f.store, "ack-race")
	if publisher.seen != 1 || got.State != StateReady || !got.PromptDelivered || f.prompts.calls != 1 {
		t.Fatalf("reentrant result publisher=%d job=%#v prompt_calls=%d", publisher.seen, got, f.prompts.calls)
	}
}

func TestInteractiveCreatePersistsBeforeItsAsyncWork(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	created, inserted, err := f.manager.EnqueueInteractiveCreate("frontend-r1", Request{Name: "DWC/interactive", GitRoot: "/repo"})
	if err != nil || !inserted {
		t.Fatalf("EnqueueInteractiveCreate = %#v, %t, %v", created, inserted, err)
	}
	if created.ID != "interactive:frontend-r1" || created.State != StateQueued {
		t.Fatalf("created = %#v", created)
	}
	if f.worktrees.calls != 0 || f.sessions.calls != 0 {
		t.Fatalf("enqueue performed effects: worktrees=%d sessions=%d", f.worktrees.calls, f.sessions.calls)
	}
	duplicate, inserted, err := f.manager.EnqueueInteractiveCreate("frontend-r1", Request{Name: "DWC/interactive", GitRoot: "/repo"})
	if err != nil || inserted || duplicate.ID != created.ID {
		t.Fatalf("duplicate enqueue = %#v, %t, %v", duplicate, inserted, err)
	}
	if err := f.manager.Process(context.Background(), created.ID); err != nil {
		t.Fatal(err)
	}
	if f.worktrees.calls != 1 || f.sessions.calls != 1 {
		t.Fatalf("process calls worktrees=%d sessions=%d", f.worktrees.calls, f.sessions.calls)
	}
}

func TestHostActionFailureRemainsPendingUntilSuccess(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	action := HostAction{ID: "a1", SourceFile: "file", Type: "switch", Payload: json.RawMessage(`{"type":"switch","dir":"/worktree"}`)}
	if _, _, err := f.store.EnqueueHostAction(action); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.DrainHostActions(context.Background()); err != nil {
		t.Fatal(err)
	}
	if f.actions.calls != 1 {
		t.Fatalf("publish calls=%d", f.actions.calls)
	}
	if err := f.manager.CompleteHostAction("a1", false, "Emacs refused workspace"); err != nil {
		t.Fatal(err)
	}
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 1 || pending[0].Completed || pending[0].Failure != "Emacs refused workspace" {
		t.Fatalf("pending = %#v", pending)
	}
	if err := f.manager.DrainHostActions(context.Background()); err != nil {
		t.Fatal(err)
	}
	if f.actions.calls != 1 {
		t.Fatalf("failed action republished without an explicit retry: calls=%d", f.actions.calls)
	}
	if err := f.manager.CompleteHostAction("a1", true, ""); err != nil {
		t.Fatal(err)
	}
	pending, err = f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 0 {
		t.Fatalf("successful completion still pending: %#v", pending)
	}
}

func TestConcurrentProcessSerializesOneJob(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	entered := make(chan struct{})
	release := make(chan struct{})
	var lock sync.Mutex
	active, maxActive := 0, 0
	f.worktrees = &fakeWorktrees{path: "/worktrees/new"}
	blocking := f.worktrees
	// A function-shaped wrapper is intentionally avoided here: the fake is the
	// external boundary and this test observes only manager serialization.
	worktrees := &blockingWorktrees{fakeWorktrees: blocking, entered: entered, release: release, active: &active, maxActive: &maxActive, lock: &lock}
	f.manager.cfg.Planner = worktrees
	f.manager.cfg.Worktrees = worktrees
	if _, _, err := f.store.Enqueue(Job{ID: "concurrent", Request: Request{Name: "DWC/concurrent", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	first := make(chan error, 1)
	go func() { first <- f.manager.Process(context.Background(), "concurrent") }()
	<-entered
	second := make(chan error, 1)
	go func() { second <- f.manager.Process(context.Background(), "concurrent") }()
	select {
	case err := <-second:
		if err != nil {
			t.Fatal(err)
		}
	case <-time.After(time.Second):
		t.Fatal("second Process did not observe the in-flight job")
	}
	close(release)
	if err := <-first; err != nil {
		t.Fatal(err)
	}
	lock.Lock()
	gotMax := maxActive
	lock.Unlock()
	if gotMax != 1 || blocking.calls != 1 {
		t.Fatalf("max concurrent worktree calls=%d calls=%d", gotMax, blocking.calls)
	}
}

type blockingWorktrees struct {
	*fakeWorktrees
	entered   chan struct{}
	release   chan struct{}
	active    *int
	maxActive *int
	lock      *sync.Mutex
}

func (f *blockingWorktrees) EnsureWorktree(ctx context.Context, job Job) error {
	f.lock.Lock()
	*f.active++
	if *f.active > *f.maxActive {
		*f.maxActive = *f.active
	}
	f.lock.Unlock()
	select {
	case f.entered <- struct{}{}:
	default:
	}
	select {
	case <-f.release:
	case <-ctx.Done():
		return ctx.Err()
	}
	f.lock.Lock()
	*f.active--
	f.lock.Unlock()
	return f.fakeWorktrees.EnsureWorktree(ctx, job)
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

func TestParseCommandsMapsStructuredSourceWorkspace(t *testing.T) {
	commands, err := parseCommands([]byte(`[{"type":"create","name":"DWC/a","git_root":"/repo","source_ws":{"name":"parent","path":"/parent"}}]`))
	if err != nil {
		t.Fatal(err)
	}
	if got := commands[0].Create; got.SourceWorkspace != "parent" || got.SourceDir != "/parent" {
		t.Fatalf("source workspace = %#v", got)
	}
	if _, err := parseCommands([]byte(`[{"type":"create","name":"DWC/a","git_root":"/repo","source_ws":{"name":"parent"}}]`)); err == nil {
		t.Fatal("incomplete source_ws was accepted")
	}
}
