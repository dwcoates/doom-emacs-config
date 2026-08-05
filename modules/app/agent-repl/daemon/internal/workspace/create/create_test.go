package create

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
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

type metadataSessions struct {
	fakeSessions
	resolved Request
}

func (f *metadataSessions) ResolveSessionMetadata(_ context.Context, _ Job) (Request, error) {
	return f.resolved, nil
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

type fakeReleases struct {
	calls     int
	items     []PublicationDecision
	err       error
	onRelease func(PublicationDecision)
}

type fakePublication struct {
	calls int
	jobs  []Job
	err   error
}

func (f *fakePublication) PrepareSessionPublication(_ context.Context, job Job) error {
	f.calls++
	f.jobs = append(f.jobs, job)
	return f.err
}

func (f *fakeReleases) ReleaseSessionPublication(_ context.Context, decision PublicationDecision) error {
	f.calls++
	f.items = append(f.items, decision)
	if f.onRelease != nil {
		f.onRelease(decision)
	}
	return f.err
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

// fakeGeometry stands in for the durable merge-geometry recorder.  It records
// exactly which jobs reached the recording call, which is what the ordering
// assertions (geometry before worktree_ready) read.
type fakeGeometry struct {
	jobs []Job
	err  error
}

func (f *fakeGeometry) RecordWorkspaceGeometry(_ context.Context, job Job) error {
	f.jobs = append(f.jobs, job)
	return f.err
}

// fakeMerges records every merge the router dispatched daemon-side, and can be
// told to reject one the way an unrecorded project_dir is rejected.
type fakeMerges struct {
	calls []MergeCommand
	err   error
}

func (f *fakeMerges) DispatchMerge(_ context.Context, cmd MergeCommand) error {
	f.calls = append(f.calls, cmd)
	return f.err
}

type fixture struct {
	store       *FileJobStore
	manager     *Manager
	geometry    *fakeGeometry
	worktrees   *fakeWorktrees
	sessions    *fakeSessions
	health      *fakeHealth
	prompts     *fakePrompts
	available   *fakeAvailable
	releases    *fakeReleases
	publication *fakePublication
	actions     *fakeActions
	merges      *fakeMerges

	// logs is written by the router goroutine AND by a worker goroutine in the
	// tests that run one, so it carries its own lock rather than relying on the
	// tests that happen to be single-threaded.
	logMu sync.Mutex
	logs  []string
}

func (f *fixture) log(format string, _ ...any) {
	f.logMu.Lock()
	defer f.logMu.Unlock()
	f.logs = append(f.logs, format)
}

func newFixture(t *testing.T, statePath string) *fixture {
	t.Helper()
	f := &fixture{
		geometry:    &fakeGeometry{},
		worktrees:   &fakeWorktrees{path: "/worktrees/new"},
		sessions:    &fakeSessions{id: "s_new"},
		health:      &fakeHealth{},
		prompts:     &fakePrompts{},
		available:   &fakeAvailable{},
		releases:    &fakeReleases{},
		publication: &fakePublication{},
		actions:     &fakeActions{},
		merges:      &fakeMerges{},
	}
	logf := f.log
	store, err := OpenJobStore(statePath, logf)
	if err != nil {
		t.Fatalf("OpenJobStore: %v", err)
	}
	f.store = store
	f.manager, err = NewManager(Config{
		Store: store, Planner: f.worktrees, Worktrees: f.worktrees, Geometry: f.geometry, Sessions: f.sessions, Health: f.health,
		Prompts: f.prompts, Available: f.available, Releases: f.releases, Publication: f.publication, HostActions: f.actions, Logf: logf,
	})
	if err != nil {
		t.Fatalf("NewManager: %v", err)
	}
	return f
}

func (f *fixture) inbox(dir string) *Inbox {
	return &Inbox{Dir: dir, Store: f.store, Manager: f.manager, Merges: f.merges, Logf: f.log, Interval: 1}
}

// drainWorkers runs the creation and host-action workers to quiescence ON THE
// CALLING GOROUTINE. It is the exact loop RunCreationWorker and
// RunHostActionWorker run, minus the parking, so a test observes a routed job's
// effects deterministically instead of waiting on a background worker.
func (f *fixture) drainWorkers(ctx context.Context) {
	for f.manager.drainOnce(ctx) {
	}
	for f.manager.drainHostOnce(ctx) {
	}
}

// ingest routes one scan's worth of command files and then runs the workers,
// which together reproduce what the daemon does across its three goroutines.
func (f *fixture) ingest(ctx context.Context, dir string) error {
	err := f.inbox(dir).ScanAndDrain(ctx)
	f.drainWorkers(ctx)
	return err
}

// loggedFormat reports whether any log line's FORMAT contains want. The
// fixture's logger keeps formats rather than rendered lines, which is what makes
// a log assertion about the code path rather than about one job's data.
func (f *fixture) loggedFormat(want string) bool {
	f.logMu.Lock()
	defer f.logMu.Unlock()
	for _, line := range f.logs {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
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
	if err := f.ingest(context.Background(), inboxDir); err != nil {
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
	restarted.manager.resumeAtBoot()
	restarted.drainWorkers(context.Background())
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
	if err := f.ingest(context.Background(), inboxDir); err != nil {
		t.Fatal(err)
	}
	// The exact same filename+index is redelivered after a producer retry.
	writeCommandFile(t, inboxDir, "workspace_commands_same.json", body)
	if err := f.ingest(context.Background(), inboxDir); err != nil {
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
	if err := f.ingest(context.Background(), inboxDir); err != nil {
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

func TestMaterializationDurablyReleasesPublicationBeforeInitialPrompt(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "publication", Request: Request{Name: "DWC/publication", GitRoot: "/repo", Prompt: "build it"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.Process(context.Background(), "publication"); err != nil {
		t.Fatal(err)
	}
	f.releases.onRelease = func(decision PublicationDecision) {
		if f.prompts.calls != 0 {
			t.Fatalf("initial prompt calls at publication release = %d, want 0", f.prompts.calls)
		}
		if !decision.Materialized || decision.JobID != "publication" || decision.WorktreePath != "/worktrees/new" || decision.SessionID != "s_new" {
			t.Fatalf("publication decision = %#v", decision)
		}
		job := job(t, f.store, "publication")
		if !job.Materialized || job.State != StateEmacsMaterialized {
			t.Fatalf("durable job at publication release = %#v", job)
		}
	}
	if err := f.manager.MarkMaterialized(context.Background(), "publication"); err != nil {
		t.Fatal(err)
	}
	got := job(t, f.store, "publication")
	if !got.Materialized || !got.PublicationReleased || f.releases.calls != 1 || f.prompts.calls != 1 {
		t.Fatalf("materialization result job=%#v releases=%d prompts=%d", got, f.releases.calls, f.prompts.calls)
	}
}

func TestSessionPublicationDecisionHoldsOnlyMatchingUnmaterializedJob(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "held", Request: Request{Name: "DWC/held", GitRoot: "/repo"}, State: StateAwaitingEmacs, WorktreePath: "/worktrees/held", SessionID: "s_held"}); err != nil {
		t.Fatal(err)
	}
	decision, err := SessionPublicationDecision(f.store, "/worktrees/held", "s_held")
	if err != nil || decision.Materialized || decision.JobID != "held" {
		t.Fatalf("held decision=%#v err=%v", decision, err)
	}
	decision, err = SessionPublicationDecision(f.store, "/other", "s_other")
	if err != nil || !decision.Materialized || decision.JobID != "" {
		t.Fatalf("unmanaged decision=%#v err=%v", decision, err)
	}
}

func TestPromptlessCreateReachesReadyWithoutSubmittingAnything(t *testing.T) {
	// Arrange — a create with no initial prompt. Emacs' SPC TAB n emits no
	// `prompt' field at all when the user leaves the prompt blank.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "quiet", Request: Request{Name: "DWC/quiet", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := f.manager.Process(context.Background(), "quiet"); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.MarkMaterialized(context.Background(), "quiet"); err != nil {
		t.Fatal(err)
	}

	// Assert — the workspace is fully ready, and NOTHING was submitted: an
	// empty submit would open the session with a blank turn.
	got := job(t, f.store, "quiet")
	if got.State != StateReady {
		t.Fatalf("promptless job state = %s, want %s", got.State, StateReady)
	}
	if f.prompts.calls != 0 {
		t.Fatalf("promptless job submitted %d prompts, want 0", f.prompts.calls)
	}
	if got.PromptDelivered {
		t.Fatal("promptless job recorded a prompt delivery")
	}
	if f.available.calls != 1 {
		t.Fatalf("available published %d times, want 1", f.available.calls)
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

// drainedHostAction enqueues one action and publishes it once, leaving it in
// the state a real host reports a completion from.
func drainedHostAction(t *testing.T, f *fixture, id, actionType string) {
	t.Helper()
	action := HostAction{ID: id, SourceFile: "file", Type: actionType, Payload: json.RawMessage(`{"type":"` + actionType + `","dir":"/worktree"}`)}
	if _, _, err := f.store.EnqueueHostAction(action); err != nil {
		t.Fatal(err)
	}
	if err := f.manager.DrainHostActions(context.Background()); err != nil {
		t.Fatal(err)
	}
	if f.actions.calls != 1 {
		t.Fatalf("publish calls = %d, want 1", f.actions.calls)
	}
}

// storedHostActions reads the durable records straight off disk, including the
// completed ones no query surfaces.
func storedHostActions(t *testing.T, statePath string) map[string]HostAction {
	t.Helper()
	raw, err := os.ReadFile(statePath)
	if err != nil {
		t.Fatal(err)
	}
	var doc diskShape
	if err := json.Unmarshal(raw, &doc); err != nil {
		t.Fatal(err)
	}
	return doc.HostActions
}

func TestHostActionFailureCompletionLeavesNothingInTheSnapshot(t *testing.T) {
	// Arrange — a published action the host is about to refuse. A refusal that
	// left the action pending made it a poison pill: every reconnect snapshot
	// re-delivered it, and it failed again every time.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	drainedHostAction(t, f, "a1", "switch")

	// Act.
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatal(err)
	}

	// Assert — the reconnect snapshot source is empty.
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 0 {
		t.Fatalf("pending = %#v, want the refused action released", pending)
	}
}

func TestHostActionFailureCompletionStopsRedelivery(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	drainedHostAction(t, f, "a1", "switch")

	// Act.
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatal(err)
	}

	// Assert — the drain tick no longer republishes it.
	if err := f.manager.DrainHostActions(context.Background()); err != nil {
		t.Fatal(err)
	}
	if f.actions.calls != 1 {
		t.Fatalf("refused action was redelivered: calls = %d, want 1", f.actions.calls)
	}
}

func TestHostActionFailureCompletionRetainsItsFailureEvidence(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	statePath := filepath.Join(root, "jobs.json")
	f := newFixture(t, statePath)
	drainedHostAction(t, f, "a1", "switch")

	// Act.
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatal(err)
	}

	// Assert — releasing the action does not forget why it failed.
	stored := storedHostActions(t, statePath)["a1"]
	if !stored.Completed || stored.Failure != "no live workspace for dir" {
		t.Fatalf("stored = %#v, want a completed record carrying its failure", stored)
	}
	if !f.loggedFormat("workspace-create: host action FAILED") {
		t.Fatalf("logs = %v, want one naming the failed host action", f.logs)
	}
}

func TestHostActionSuccessCompletionLeavesNothingInTheSnapshot(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	drainedHostAction(t, f, "a1", "switch")

	// Act.
	if err := f.manager.CompleteHostAction("a1", true, ""); err != nil {
		t.Fatal(err)
	}

	// Assert.
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 0 {
		t.Fatalf("pending = %#v, want the completed action released", pending)
	}
}

func TestHostActionDuplicateFailureCompletionIsIdempotent(t *testing.T) {
	// Arrange — the live shape: one host reported the same refusal twice,
	// hours apart, across a reconnect.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	drainedHostAction(t, f, "a1", "switch")
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatal(err)
	}

	// Act — the late duplicate.
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatalf("duplicate completion = %v, want nil", err)
	}

	// Assert — still released, nothing resurrected.
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 0 {
		t.Fatalf("pending = %#v, want the duplicate completion to change nothing", pending)
	}
}

func TestHostActionDisagreeingLateCompletionIsLoggedLoudly(t *testing.T) {
	// Arrange — a successfully completed action.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	drainedHostAction(t, f, "a1", "switch")
	if err := f.manager.CompleteHostAction("a1", true, ""); err != nil {
		t.Fatal(err)
	}

	// Act — a late verdict that contradicts the recorded one.
	if err := f.manager.CompleteHostAction("a1", false, "no live workspace for dir"); err != nil {
		t.Fatalf("late disagreeing completion = %v, want nil", err)
	}

	// Assert — the contradiction is never silent.
	if !f.loggedFormat("workspace-create: host action DUPLICATE COMPLETION DISAGREES") {
		t.Fatalf("logs = %v, want one naming the disagreeing duplicate completion", f.logs)
	}
}

func TestResolvedSessionMetadataPersistsBeforeCreateAndSurvivesRestart(t *testing.T) {
	root := t.TempDir()
	statePath := filepath.Join(root, "jobs.json")
	f := newFixture(t, statePath)
	sessions := &metadataSessions{fakeSessions: fakeSessions{id: "s_new"}, resolved: Request{Name: "DWC/child", GitRoot: "/repo", SourceWorkspace: "parent", SourceDir: "/parent", ConfigDir: "/cfg", PermissionMode: "plan"}}
	manager, err := NewManager(Config{Store: f.store, Planner: f.worktrees, Worktrees: f.worktrees, Geometry: f.geometry, Sessions: sessions, Health: f.health, Prompts: f.prompts, Available: f.available, Releases: f.releases, Publication: f.publication, HostActions: f.actions, Logf: func(string, ...any) {}})
	if err != nil {
		t.Fatal(err)
	}
	created, _, err := f.store.Enqueue(Job{ID: "meta", Request: Request{Name: "DWC/child", GitRoot: "/repo"}, State: StateQueued})
	if err != nil {
		t.Fatal(err)
	}
	if err := manager.Process(context.Background(), created.ID); err != nil {
		t.Fatal(err)
	}
	stored := job(t, f.store, created.ID)
	if stored.Request.ConfigDir != "/cfg" || stored.Request.PermissionMode != "plan" {
		t.Fatalf("durable metadata = %#v", stored.Request)
	}
	reopened, err := OpenJobStore(statePath, func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}
	afterRestart := job(t, reopened, created.ID)
	if !reflect.DeepEqual(afterRestart.Request, stored.Request) {
		t.Fatalf("metadata after restart = %#v, want %#v", afterRestart.Request, stored.Request)
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
 {"type":"create","name":"DWC/a","git_root":"/repo"}, {"type":"prompt"}, {"type":"finish"}, {"type":"close"}, {"type":"open"}, {"type":"clipboard"}, {"type":"send","data":false}, {"type":"merge","project_dir":"/worktrees/a"}, {"type":"eval"}, {"type":"switch"}, {"type":"fold","folded":false}, {"type":"set-view"}, {"type":"task-create"}, {"type":"task-toggle-done"}, {"type":"task-open"}, {"type":"task-add-workspace"}
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
	commands, err := parseCommands([]byte(`[{"type":"create","name":"DWC/a","git_root":"/repo","source_ws":{"name":"parent","path":"/parent/"}}]`))
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

// selectivePlanner fails PlanWorktree for exactly one requested name so a
// single poisoned job can be observed alongside a healthy sibling.
type selectivePlanner struct {
	fakeWorktrees
	failName string
	failErr  error
}

func (s *selectivePlanner) PlanWorktree(ctx context.Context, job Job) (WorktreeResult, error) {
	if job.Request.Name == s.failName {
		return WorktreeResult{}, s.failErr
	}
	return s.fakeWorktrees.PlanWorktree(ctx, job)
}

// listErrorStore is a JobStore whose List is broken.  That is the structural
// class: nothing about it is one job's fault, so it must keep propagating.
type listErrorStore struct {
	JobStore
	err error
}

func (l listErrorStore) List() ([]Job, error) { return nil, l.err }

// A poisoned job is contained: it is recorded failed, and the sibling job in
// the same batch still reaches the host.  Before this, one failing plan step
// returned out of Resume and stopped the entire inbox.
func TestScanAndDrainContainsJobFailureAndKeepsDrainingSiblings(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	planner := &selectivePlanner{fakeWorktrees: fakeWorktrees{path: "/worktrees/new"}, failName: "DWC/poison", failErr: errors.New("check start tag exit=128")}
	f.manager.cfg.Planner = planner
	f.manager.cfg.Worktrees = planner
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_mixed.json", `[
  {"type":"create","name":"DWC/poison","git_root":"/repo"},
  {"type":"create","name":"DWC/healthy","git_root":"/repo"}
]`)

	err := f.ingest(context.Background(), inboxDir)

	if err != nil {
		t.Fatalf("ScanAndDrain = %v, want nil: a job failure must not stop the inbox", err)
	}
	if failed := job(t, f.store, "workspace_commands_mixed:0"); failed.State != StateFailed || failed.LastError == "" {
		t.Fatalf("poisoned job = %#v, want durably failed with a recorded error", failed)
	}
	if healthy := job(t, f.store, "workspace_commands_mixed:1"); healthy.State != StateAwaitingEmacs {
		t.Fatalf("sibling job = %#v, want it to have reached the host anyway", healthy)
	}
}

// The failure classification itself, stated once.  Neither class may end the
// creation worker — it has no caller to return to — so each is DISTINGUISHED BY
// THE RECORD IT LEAVES: a job-level failure is durable against the job, while a
// structural one names the subsystem and strands nothing.
func TestCreationWorkerClassifiesJobFailureAgainstStructuralFailure(t *testing.T) {
	tests := []struct {
		name       string
		structural bool
		wantLog    string
	}{
		{name: "job failure is contained", structural: false, wantLog: "CONTAINED job failure"},
		{name: "structural failure is named", structural: true, wantLog: "BOOT RESUME FAILED"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			f := newFixture(t, filepath.Join(root, "jobs.json"))
			planner := &selectivePlanner{fakeWorktrees: fakeWorktrees{path: "/worktrees/new"}, failName: "DWC/poison", failErr: errors.New("plan exploded")}
			f.manager.cfg.Planner = planner
			f.manager.cfg.Worktrees = planner
			if _, _, err := f.store.Enqueue(Job{ID: "j0", Request: Request{Name: "DWC/poison", GitRoot: "/repo"}, State: StateQueued}); err != nil {
				t.Fatal(err)
			}
			if test.structural {
				f.manager.cfg.Store = listErrorStore{JobStore: f.store, err: errors.New("store is broken")}
			}

			// Act
			f.manager.resumeAtBoot()
			f.drainWorkers(context.Background())

			// Assert
			if !f.loggedFormat(test.wantLog) {
				t.Fatalf("logs = %v, want one naming %q", f.logs, test.wantLog)
			}
		})
	}
}

// A job-level failure is DURABLE against the job, which is what makes stepping
// over it safe rather than merely quiet.
func TestCreationWorkerRecordsAContainedJobFailureDurably(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	planner := &selectivePlanner{fakeWorktrees: fakeWorktrees{path: "/worktrees/new"}, failName: "DWC/poison", failErr: errors.New("plan exploded")}
	f.manager.cfg.Planner = planner
	f.manager.cfg.Worktrees = planner
	if _, _, err := f.store.Enqueue(Job{ID: "j0", Request: Request{Name: "DWC/poison", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act
	f.manager.resumeAtBoot()
	f.drainWorkers(context.Background())

	// Assert
	if got := job(t, f.store, "j0"); got.State != StateFailed || got.LastError == "" {
		t.Fatalf("job = %#v, want a durably failed job carrying its cause", got)
	}
}

// A failed job must reach the user.  It rides the durable HostAction channel,
// so it survives an Emacs restart exactly like an available workspace does.
func TestFailedJobSurfacesDurableHostActionNamingJobAndError(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	planner := &selectivePlanner{fakeWorktrees: fakeWorktrees{path: "/worktrees/new"}, failName: "DWC/poison", failErr: errors.New("check start tag exit=128")}
	f.manager.cfg.Planner = planner
	f.manager.cfg.Worktrees = planner
	if _, _, err := f.store.Enqueue(Job{ID: "j0", Request: Request{Name: "DWC/poison", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	f.manager.resumeAtBoot()
	f.drainWorkers(context.Background())

	if len(f.actions.items) != 1 {
		t.Fatalf("published host actions = %#v, want exactly the failure notice", f.actions.items)
	}
	notice := f.actions.items[0]
	if notice.ID != "j0:failed" || notice.Type != HostActionTypeWorkspaceCreateFailed {
		t.Fatalf("notice = %#v", notice)
	}
	var failure WorkspaceCreateFailure
	if err := json.Unmarshal(notice.Payload, &failure); err != nil {
		t.Fatal(err)
	}
	if failure.JobID != "j0" || failure.RequestedName != "DWC/poison" {
		t.Fatalf("failure payload = %#v", failure)
	}
	if !strings.Contains(failure.Error, "check start tag exit=128") {
		t.Fatalf("failure error = %q, want the underlying cause verbatim", failure.Error)
	}
	// Durable, not merely broadcast: a reconnecting host must still see it.
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 1 || pending[0].ID != "j0:failed" {
		t.Fatalf("pending host actions = %#v, want the retained failure notice", pending)
	}
}

// One unparseable command file cannot stop service: it is quarantined as
// durable evidence and every other file in the same scan is still ingested.
func TestPoisonedCommandFileIsQuarantinedAndScanContinues(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_a-poison.json", `{not even an array`)
	writeCommandFile(t, inboxDir, "workspace_commands_b-good.json", `[{"type":"create","name":"DWC/good","git_root":"/repo"}]`)

	err := f.ingest(context.Background(), inboxDir)

	if err != nil {
		t.Fatalf("ScanAndDrain = %v, want nil: a poisoned file must not stop the scan", err)
	}
	if got := job(t, f.store, "workspace_commands_b-good:0"); got.State != StateAwaitingEmacs {
		t.Fatalf("good job = %#v, want it to have been ingested anyway", got)
	}
	rejected, err := filepath.Glob(filepath.Join(inboxDir, "*.rejected"))
	if err != nil {
		t.Fatal(err)
	}
	if len(rejected) != 1 {
		t.Fatalf("quarantined files = %v, want exactly the poisoned one preserved", rejected)
	}
	// A second scan must not re-claim the quarantined file.
	if err := f.ingest(context.Background(), inboxDir); err != nil {
		t.Fatalf("second ScanAndDrain: %v", err)
	}
	again, err := filepath.Glob(filepath.Join(inboxDir, "*.rejected"))
	if err != nil {
		t.Fatal(err)
	}
	if !reflect.DeepEqual(again, rejected) {
		t.Fatalf("quarantined files after rescan = %v, want %v", again, rejected)
	}
}

func TestPriorityDecodesToTheBareLabel(t *testing.T) {
	tests := []struct {
		name string
		json string
		want Priority
	}{
		{name: "string", json: `{"priority":"p1"}`, want: "p1"},
		{name: "number", json: `{"priority":1}`, want: "1"},
		{name: "null", json: `{"priority":null}`, want: ""},
		{name: "absent", json: `{}`, want: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var request Request

			// Act
			if err := json.Unmarshal([]byte(tc.json), &request); err != nil {
				t.Fatalf("unmarshal %s: %v", tc.json, err)
			}

			// Assert
			if request.Priority != tc.want {
				t.Fatalf("priority = %q, want %q", request.Priority, tc.want)
			}
		})
	}
}

func TestPriorityRejectsANonScalar(t *testing.T) {
	// Arrange
	var request Request

	// Act
	err := json.Unmarshal([]byte(`{"priority":["p1"]}`), &request)

	// Assert
	if err == nil {
		t.Fatal("expected a decode error for a non-scalar priority")
	}
}

func TestPriorityRoundTripsAsAString(t *testing.T) {
	// Arrange
	request := Request{Name: "ws", GitRoot: "/repo", Priority: "p2"}

	// Act
	encoded, err := json.Marshal(request)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	var decoded Request
	if err := json.Unmarshal(encoded, &decoded); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}

	// Assert
	if decoded.Priority != "p2" {
		t.Fatalf("priority = %q, want %q", decoded.Priority, "p2")
	}
}

// TestGeometryIsRecordedWithThePersistedWorktreeIdentity proves the recorder
// sees the job's checkpointed identity (path + branch), not a bare request:
// those two fields ARE two of the three merge coordinates.
func TestGeometryIsRecordedWithThePersistedWorktreeIdentity(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	f.worktrees.path = "/worktrees/geo"
	f.worktrees.branch = "DWC/geo"
	if _, _, err := f.store.Enqueue(Job{ID: "geo", Request: Request{Name: "DWC/geo", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := f.manager.Process(context.Background(), "geo"); err != nil {
		t.Fatal(err)
	}

	// Assert.
	if len(f.geometry.jobs) != 1 {
		t.Fatalf("geometry recordings = %d, want 1", len(f.geometry.jobs))
	}
	got := f.geometry.jobs[0]
	if got.ID != "geo" || got.WorktreePath != "/worktrees/geo" || got.Branch != "DWC/geo" {
		t.Fatalf("recorded job = %#v", got)
	}
}

// TestGeometryIsRecordedBeforeTheWorktreeStageCompletes pins the ORDERING: a
// workspace must never reach worktree_ready (and from there the user) without
// the geometry that makes it mergeable.
func TestGeometryIsRecordedBeforeTheWorktreeStageCompletes(t *testing.T) {
	// Arrange — the recorder refuses, so the stage must not advance.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	f.geometry.err = errors.New("state store is down")
	if _, _, err := f.store.Enqueue(Job{ID: "order", Request: Request{Name: "DWC/order", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act.
	err := f.manager.Process(context.Background(), "order")

	// Assert.
	if err == nil {
		t.Fatal("Process succeeded, want the geometry failure")
	}
	got := job(t, f.store, "order")
	if got.State != StateFailed {
		t.Fatalf("state = %s, want failed", got.State)
	}
	if f.sessions.calls != 0 || f.available.calls != 0 {
		t.Fatalf("the job advanced past the worktree stage: sessions=%d available=%d", f.sessions.calls, f.available.calls)
	}
}

// TestGeometryRecordingFailureIsSurfacedToTheHost proves the failure is not
// merely returned: it lands durably and reaches the host with its cause.
func TestGeometryRecordingFailureIsSurfacedToTheHost(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	f.geometry.err = errors.New("state store is down")
	if _, _, err := f.store.Enqueue(Job{ID: "surfaced", Request: Request{Name: "DWC/surfaced", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act.
	if err := f.manager.Process(context.Background(), "surfaced"); err == nil {
		t.Fatal("Process succeeded, want the geometry failure")
	}

	// Assert.
	if len(f.actions.items) != 1 {
		t.Fatalf("host actions = %d, want 1", len(f.actions.items))
	}
	var failure WorkspaceCreateFailure
	if err := json.Unmarshal(f.actions.items[0].Payload, &failure); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(failure.Error, "record merge geometry") || !strings.Contains(failure.Error, "state store is down") {
		t.Fatalf("host failure text = %q", failure.Error)
	}
}

// TestManagerRefusesConstructionWithoutAGeometryRecorder keeps the capability
// mandatory: a manager without one would materialize unmergeable workspaces.
func TestManagerRefusesConstructionWithoutAGeometryRecorder(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))

	// Act.
	_, err := NewManager(Config{
		Store: f.store, Planner: f.worktrees, Worktrees: f.worktrees, Sessions: f.sessions, Health: f.health,
		Prompts: f.prompts, Available: f.available, Releases: f.releases, Publication: f.publication, HostActions: f.actions, Logf: func(string, ...any) {},
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "WorkspaceGeometryRecorder") {
		t.Fatalf("NewManager error = %v, want a WorkspaceGeometryRecorder refusal", err)
	}
}

// ---- The router / worker split -------------------------------------------
//
// The inbox goroutine claims, persists, and routes. It never runs a job state
// machine. These tests pin that separation from both sides: what the router
// does NOT do, and what a wedged worker cannot do to it.

// wedgedPlanner blocks inside PlanWorktree until released, which is the shape
// of the production wedge that froze ingestion: a job whose state machine never
// returns.
type wedgedPlanner struct {
	fakeWorktrees
	entered chan struct{}
	release chan struct{}
}

func (w *wedgedPlanner) PlanWorktree(ctx context.Context, job Job) (WorktreeResult, error) {
	select {
	case w.entered <- struct{}{}:
	default:
	}
	select {
	case <-w.release:
	case <-ctx.Done():
		return WorktreeResult{}, ctx.Err()
	}
	return w.fakeWorktrees.PlanWorktree(ctx, job)
}

// THE REGRESSION THIS RESTRUCTURING EXISTS FOR. A job whose state machine never
// returns used to freeze the single goroutine that also claimed command files,
// so every later command vanished until the daemon was bounced.
func TestRouterKeepsIngestingWhileTheCreationWorkerIsWedged(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	wedge := &wedgedPlanner{
		fakeWorktrees: fakeWorktrees{path: "/worktrees/new"},
		entered:       make(chan struct{}, 1),
		release:       make(chan struct{}),
	}
	f.manager.cfg.Planner = wedge
	f.manager.cfg.Worktrees = wedge
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	worker := make(chan struct{})
	go func() {
		defer close(worker)
		f.manager.RunCreationWorker(ctx) //nolint:errcheck // the worker ends only with ctx.Err()
	}()
	inbox := f.inbox(inboxDir)
	writeCommandFile(t, inboxDir, "workspace_commands_wedge.json", `[{"type":"create","name":"DWC/wedge","git_root":"/repo"}]`)
	if err := inbox.ScanAndDrain(ctx); err != nil {
		t.Fatalf("first ScanAndDrain: %v", err)
	}
	<-wedge.entered

	// Act: the worker is now parked inside the first job's state machine.
	writeCommandFile(t, inboxDir, "workspace_commands_later.json", `[{"type":"create","name":"DWC/later","git_root":"/repo"}]`)
	err := inbox.ScanAndDrain(ctx)

	// Assert
	if err != nil {
		t.Fatalf("ScanAndDrain while a job is wedged = %v, want nil", err)
	}
	if got := job(t, f.store, "workspace_commands_later:0"); got.State != StateQueued {
		t.Fatalf("later job = %#v, want it durably ingested behind the wedge", got)
	}
	cancel()
	<-worker
	close(wedge.release)
}

// The router persists and routes; it does not create. Nothing but the worker
// may touch a worktree, a session, or the host.
func TestRouterPersistsACreateWithoutRunningIt(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_routed.json", `[{"type":"create","name":"DWC/routed","git_root":"/repo"}]`)

	// Act
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	if got := job(t, f.store, "workspace_commands_routed:0"); got.State != StateQueued {
		t.Fatalf("routed job = %#v, want it still queued: the router must not advance it", got)
	}
	if f.worktrees.plans != 0 || f.sessions.calls != 0 {
		t.Fatalf("router ran creation effects: plans=%d sessions=%d", f.worktrees.plans, f.sessions.calls)
	}
}

// The routed id reaches the worker, and the worker is what materializes it.
func TestRoutedCreateIsMaterializedByTheCreationWorker(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_routed.json", `[{"type":"create","name":"DWC/routed","git_root":"/repo"}]`)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Act
	f.drainWorkers(context.Background())

	// Assert
	if got := job(t, f.store, "workspace_commands_routed:0"); got.State != StateAwaitingEmacs {
		t.Fatalf("routed job = %#v, want the worker to have materialized it", got)
	}
}

// Host-action publication is off the router too: a host slow to accept an
// action must not delay the next command file.
func TestRouterPersistsAHostActionWithoutPublishingIt(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_action.json", `[{"type":"switch","dir":"/worktrees/one"}]`)

	// Act
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	if f.actions.calls != 0 {
		t.Fatalf("router published %d host actions, want 0: publication belongs to the host-action worker", f.actions.calls)
	}
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 1 {
		t.Fatalf("pending host actions = %#v, want the routed action retained durably", pending)
	}
}

// ...and the host-action worker is what publishes it.
func TestHostActionWorkerPublishesTheRoutedAction(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_action.json", `[{"type":"switch","dir":"/worktrees/one"}]`)
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Act
	f.drainWorkers(context.Background())

	// Assert
	if f.actions.calls != 1 {
		t.Fatalf("host action calls = %d, want exactly one publication", f.actions.calls)
	}
}

// Boot resume travels the SAME channel a fresh command does; the durable store
// is the only thing that survives a bounce, so the ids come from it.
func TestBootResumeRoutesNonTerminalJobsThroughTheCreationChannel(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "resume-me", Request: Request{Name: "DWC/resume", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}
	if _, _, err := f.store.Enqueue(Job{ID: "already-done", Request: Request{Name: "DWC/done", GitRoot: "/repo"}, State: StateReady}); err != nil {
		t.Fatal(err)
	}

	// Act
	f.manager.resumeAtBoot()

	// Assert
	select {
	case id := <-f.manager.jobs:
		if id != "resume-me" {
			t.Fatalf("routed id = %q, want the non-terminal job", id)
		}
	default:
		t.Fatal("boot resume routed nothing: the non-terminal job must reach the worker's channel")
	}
	select {
	case id := <-f.manager.jobs:
		t.Fatalf("boot resume also routed %q, want the terminal job left alone", id)
	default:
	}
}

// A full route buffer must never block the router. The id is recoverable from
// the store, so the route degrades to the coalesced sweep instead.
func TestRouteJobFallsBackToTheSweepWhenTheBufferIsFull(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	for i := 0; i < cap(f.manager.jobs); i++ {
		f.manager.jobs <- "filler"
	}

	// Act
	f.manager.RouteJob("overflow")

	// Assert
	select {
	case <-f.manager.sweep:
	default:
		t.Fatal("a full route buffer did not raise the sweep: the id would be stranded")
	}
}

// ---- The merge verb, routed daemon-side ----------------------------------

// The merge verb no longer round-trips through Emacs: it is dispatched with the
// project_dir the entry states, and nothing else.
func TestRouterDispatchesMergeWithItsProjectDir(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)

	// Act
	err := f.inbox(inboxDir).ScanAndDrain(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}
	want := []MergeCommand{{Workspace: "DWC/feature", ProjectDir: "/worktrees/feature", ID: "workspace_commands_merge:0"}}
	if !reflect.DeepEqual(f.merges.calls, want) {
		t.Fatalf("dispatched merges = %#v, want %#v", f.merges.calls, want)
	}
}

// A merge is NOT a host action any more. Emacs must never be handed one again:
// that is the round trip whose heuristic name resolution this replaces.
func TestRouterNeverPersistsAMergeAsAHostAction(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)

	// Act
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatal(err)
	}
	if len(pending) != 0 {
		t.Fatalf("pending host actions = %#v, want none: the merge verb is daemon-owned", pending)
	}
}

// EVERY unroutable merge is rejected loudly. There is deliberately no name
// resolution, no branch tail, and no fallback of any kind behind these.
func TestRouterRejectsAnUnroutableMerge(t *testing.T) {
	tests := []struct {
		name        string
		entry       string
		dispatchErr error
		wantReason  string
	}{
		{
			name:       "missing project_dir",
			entry:      `{"type":"merge","workspace":"DWC/feature"}`,
			wantReason: "missing-project-dir",
		},
		{
			name:       "relative project_dir",
			entry:      `{"type":"merge","workspace":"DWC/feature","project_dir":"worktrees/feature"}`,
			wantReason: "relative-project-dir",
		},
		{
			name:        "project_dir names no recorded workspace",
			entry:       `{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/ghost"}`,
			dispatchErr: ErrUnknownMergeWorkspace,
			wantReason:  "unknown-project-dir",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			f := newFixture(t, filepath.Join(root, "jobs.json"))
			f.merges.err = test.dispatchErr
			inboxDir := filepath.Join(root, "output")
			writeCommandFile(t, inboxDir, "workspace_commands_bad.json", "["+test.entry+"]")

			// Act
			err := f.inbox(inboxDir).ScanAndDrain(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("ScanAndDrain = %v, want nil: a rejected file must not stop the scan", err)
			}
			rejected, globErr := filepath.Glob(filepath.Join(inboxDir, "*.rejected"))
			if globErr != nil {
				t.Fatal(globErr)
			}
			if len(rejected) != 1 {
				t.Fatalf("quarantined files = %v, want the rejected merge preserved", rejected)
			}
			if !f.loggedFormat(test.wantReason) {
				t.Fatalf("logs = %v, want one naming reason=%s", f.logs, test.wantReason)
			}
		})
	}
}

// A rejection names BOTH identities. The project_dir is the only key, and the
// workspace name is what the human who dispatched it recognizes.
func TestRejectedMergeLogNamesWorkspaceAndProjectDir(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_bad.json", `[{"type":"merge","workspace":"DWC/feature"}]`)

	// Act
	if err := f.inbox(inboxDir).ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	if !f.loggedFormat(`workspace=%q project_dir=%q`) {
		t.Fatalf("logs = %v, want a rejection naming both the workspace and the project_dir", f.logs)
	}
}

// A structural dispatch failure is NOT a rejection: the claim stays standing so
// the next scan retries it rather than the emitter being blamed.
func TestStructuralMergeDispatchFailureKeepsTheClaim(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	f.merges.err = errors.New("merge queue is unreachable")
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)

	// Act
	err := f.inbox(inboxDir).ScanAndDrain(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "merge queue is unreachable") {
		t.Fatalf("ScanAndDrain = %v, want the structural cause propagated", err)
	}
	claims, globErr := filepath.Glob(filepath.Join(inboxDir, "*.claimed"))
	if globErr != nil {
		t.Fatal(globErr)
	}
	if len(claims) != 1 {
		t.Fatalf("claims = %v, want the claim left standing for the next scan", claims)
	}
}

// A long-lived watcher without a merge route could only ever drop merges, so it
// refuses to START rather than shredding them one file at a time.
func TestInboxRefusesToRunWithoutAMergeDispatcher(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inbox := f.inbox(filepath.Join(root, "output"))
	inbox.Merges = nil

	// Act
	err := inbox.Run(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "MergeDispatcher") {
		t.Fatalf("Run = %v, want a MergeDispatcher refusal", err)
	}
}

// A one-shot drain treats the missing route as THAT FILE's failure: the entry
// cannot be routed, so it is quarantined rather than propagated as an ingress
// failure that stops every remaining file.
func TestMergeWithoutADispatcherIsQuarantinedRatherThanAbortingTheDrain(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)
	inbox := f.inbox(inboxDir)
	inbox.Merges = nil

	// Act
	err := inbox.ScanAndDrain(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ScanAndDrain = %v, want nil: an unroutable file must not stop the scan", err)
	}
	rejected, globErr := filepath.Glob(filepath.Join(inboxDir, "*.rejected"))
	if globErr != nil {
		t.Fatal(globErr)
	}
	if len(rejected) != 1 {
		t.Fatalf("quarantined files = %v, want the unroutable merge preserved as evidence", rejected)
	}
}

// The MISCONFIGURATION itself stays loud. Quarantining the casualty without
// naming the cause would tell an operator the emitter was at fault.
func TestMergeWithoutADispatcherLogsTheMisconfiguration(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)
	inbox := f.inbox(inboxDir)
	inbox.Merges = nil

	// Act
	if err := inbox.ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	if !f.loggedFormat("merge route MISCONFIGURED") {
		t.Fatalf("logs = %v, want one naming the missing merge route as a daemon misconfiguration", f.logs)
	}
}

// One wiring bug must not become a total ingress outage: the files SORTED AFTER
// the unroutable one still get their turn.
func TestMergeWithoutADispatcherStillIngestsTheFilesBehindIt(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	inboxDir := filepath.Join(root, "output")
	writeCommandFile(t, inboxDir, "workspace_commands_a_merge.json", `[{"type":"merge","workspace":"DWC/feature","project_dir":"/worktrees/feature"}]`)
	writeCommandFile(t, inboxDir, "workspace_commands_b_create.json", `[{"type":"create","name":"DWC/behind","git_root":"/repo"}]`)
	inbox := f.inbox(inboxDir)
	inbox.Merges = nil

	// Act
	if err := inbox.ScanAndDrain(context.Background()); err != nil {
		t.Fatalf("ScanAndDrain: %v", err)
	}

	// Assert
	jobs, err := f.store.List()
	if err != nil {
		t.Fatal(err)
	}
	if len(jobs) != 1 || jobs[0].Request.Name != "DWC/behind" {
		t.Fatalf("durable jobs = %#v, want the create file behind the unroutable merge ingested", jobs)
	}
}

// The sweep is what makes routing TOTAL: whatever a full buffer could not name
// individually is still recovered from the durable store.
func TestSweepRecoversWorkFromTheDurableStore(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "swept", Request: Request{Name: "DWC/swept", GitRoot: "/repo"}, State: StateQueued}); err != nil {
		t.Fatal(err)
	}

	// Act: no id was ever routed — only the coalesced signal.
	f.manager.RouteSweep()
	f.drainWorkers(context.Background())

	// Assert
	if got := job(t, f.store, "swept"); got.State != StateAwaitingEmacs {
		t.Fatalf("swept job = %#v, want the sweep to have recovered and run it", got)
	}
}

// An interactive create is routed like every other creation rather than
// spawning a goroutine of its own, so creation keeps exactly one owner.
func TestStartInteractiveCreateRoutesInsteadOfRunningItsOwnGoroutine(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))

	// Act
	job, inserted, err := f.manager.StartInteractiveCreate(context.Background(), "req-1", Request{Name: "DWC/interactive", GitRoot: "/repo"})

	// Assert
	if err != nil || !inserted {
		t.Fatalf("StartInteractiveCreate = %v, %t", err, inserted)
	}
	select {
	case id := <-f.manager.jobs:
		if id != job.ID {
			t.Fatalf("routed id = %q, want %q", id, job.ID)
		}
	default:
		t.Fatal("interactive create routed nothing to the creation worker")
	}
	if f.worktrees.plans != 0 {
		t.Fatalf("interactive create ran %d plans on the calling goroutine, want 0", f.worktrees.plans)
	}
}

// signallingActions reports each publication on a channel so a test can observe
// the worker goroutine without polling or sleeping.
type signallingActions struct {
	published chan HostAction
}

func (s *signallingActions) PublishHostAction(_ context.Context, action HostAction) error {
	s.published <- action
	return nil
}

// The host-action worker publishes what the router persisted, on its own
// goroutine, until its context ends.
func TestHostActionWorkerPublishesUntilItsContextEnds(t *testing.T) {
	// Arrange
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	actions := &signallingActions{published: make(chan HostAction, 1)}
	f.manager.cfg.HostActions = actions
	if _, _, err := f.store.EnqueueHostAction(HostAction{ID: "a0", Type: "switch", Payload: []byte(`{"type":"switch"}`)}); err != nil {
		t.Fatal(err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stopped := make(chan error, 1)

	// Act
	go func() { stopped <- f.manager.RunHostActionWorker(ctx) }()

	// Assert
	if got := <-actions.published; got.ID != "a0" {
		t.Fatalf("published = %#v, want the retained action", got)
	}
	cancel()
	if err := <-stopped; !errors.Is(err, context.Canceled) {
		t.Fatalf("RunHostActionWorker = %v, want the context's cancellation", err)
	}
}
