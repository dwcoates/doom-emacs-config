package create

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
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
	calls    int
	id       string
	err      error
	onEnsure func(Job)
}

type metadataSessions struct {
	fakeSessions
	resolved Request
}

func (f *metadataSessions) ResolveSessionMetadata(_ context.Context, _ Job) (Request, error) {
	return f.resolved, nil
}

func (f *fakeSessions) EnsureSession(_ context.Context, job Job) (string, error) {
	f.calls++
	if f.onEnsure != nil {
		f.onEnsure(job)
	}
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
	calls     int
	jobs      []Job
	err       error
	onPrepare func(Job)
}

func (f *fakePublication) PrepareSessionPublication(_ context.Context, job Job) error {
	f.calls++
	f.jobs = append(f.jobs, job)
	if f.onPrepare != nil {
		f.onPrepare(job)
	}
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
	// errorLogs are the records the manager emitted at ERROR severity, kept
	// apart from logs so a test can assert a fault was reported AS a fault.
	errorLogs []string

	// clock is the manager's time source. The awaiting-host re-request cadence
	// is a time-since check, and driving it with a controlled instant is what
	// lets a test cross the interval boundary WITHOUT sleeping.
	clockMu sync.Mutex
	clock   time.Time
}

func (f *fixture) now() time.Time {
	f.clockMu.Lock()
	defer f.clockMu.Unlock()
	return f.clock
}

func (f *fixture) advance(d time.Duration) {
	f.clockMu.Lock()
	defer f.clockMu.Unlock()
	f.clock = f.clock.Add(d)
}

// RECORDS ARE STORED RENDERED, not as their format strings. A record's
// distinguishing detail — which job, which cause — travels in the ARGUMENTS,
// so a fixture holding only formats can assert that an escalation happened but
// never that it named the right fault. Every literal a test matches on is a
// literal of the format too, so rendering strengthens the assertions without
// weakening any.
func (f *fixture) log(format string, args ...any) {
	f.logMu.Lock()
	defer f.logMu.Unlock()
	f.logs = append(f.logs, fmt.Sprintf(format, args...))
}

func (f *fixture) logError(format string, args ...any) {
	f.logMu.Lock()
	defer f.logMu.Unlock()
	rendered := fmt.Sprintf(format, args...)
	f.errorLogs = append(f.errorLogs, rendered)
	f.logs = append(f.logs, rendered)
}

// loggedErrorFormat reports whether any ERROR-severity record's format contains
// want.
func (f *fixture) loggedErrorFormat(want string) bool {
	f.logMu.Lock()
	defer f.logMu.Unlock()
	for _, line := range f.errorLogs {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
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
		clock:       time.Date(2026, 8, 7, 11, 44, 0, 0, time.UTC),
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
		Now: f.now, Errorf: f.logError,
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

func TestOpenJobStoreMigratesOnlyHistoricalMaterializedJobs(t *testing.T) {
	path := filepath.Join(t.TempDir(), "jobs.json")
	legacy := diskShape{Version: 1, Jobs: map[string]Job{}, HostActions: map[string]HostAction{}}
	states := map[string]struct {
		state              JobState
		availablePublished bool
		materialized       bool
	}{
		"queued":                        {state: StateQueued},
		"worktree-creating":             {state: StateWorktreeCreating},
		"worktree-ready":                {state: StateWorktreeReady},
		"session-creating":              {state: StateSessionCreating},
		"session-ready":                 {state: StateSessionReady},
		"session-healthy":               {state: StateSessionHealthy},
		"awaiting":                      {state: StateAwaitingEmacs},
		"materialized":                  {state: StateEmacsMaterialized, materialized: true},
		"prompt-submitting":             {state: StatePromptSubmitting, materialized: true},
		"ready":                         {state: StateReady, materialized: true},
		"failed-before-available":       {state: StateFailed},
		"failed-after-available-pushed": {state: StateFailed, availablePublished: true, materialized: true},
	}
	for id, tc := range states {
		legacy.Jobs[id] = Job{
			ID:                 id,
			Request:            Request{Name: id, GitRoot: "/repo"},
			State:              tc.state,
			AvailablePublished: tc.availablePublished,
		}
	}
	encoded, err := json.Marshal(legacy)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, encoded, 0o600); err != nil {
		t.Fatal(err)
	}
	store, err := OpenJobStore(path, func(string, ...any) {})
	if err != nil {
		t.Fatal(err)
	}
	for id, tc := range states {
		job, ok, err := store.Get(id)
		if err != nil || !ok {
			t.Fatalf("job %q missing after migration: ok=%t err=%v", id, ok, err)
		}
		if job.Materialized != tc.materialized || job.PublicationReleased != tc.materialized {
			t.Errorf("job %q migration materialized=%t released=%t, want both %t", id, job.Materialized, job.PublicationReleased, tc.materialized)
		}
	}
	var persisted diskShape
	persistedBytes, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := json.Unmarshal(persistedBytes, &persisted); err != nil {
		t.Fatal(err)
	}
	if persisted.Version != 2 {
		t.Fatalf("migrated version=%d, want 2", persisted.Version)
	}
}

func TestOpenJobStoreRejectsMalformedV1WithoutRewritingBytes(t *testing.T) {
	path := filepath.Join(t.TempDir(), "jobs.json")
	legacy := []byte(`{"version":1,"jobs":{"bad":{"id":"other","request":{"name":"bad","git_root":"/repo"},"state":"ready"}},"host_actions":{}}`)
	if err := os.WriteFile(path, legacy, 0o600); err != nil {
		t.Fatal(err)
	}
	if _, err := OpenJobStore(path, func(string, ...any) {}); err == nil {
		t.Fatal("OpenJobStore succeeded for malformed v1")
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	if string(got) != string(legacy) {
		t.Fatalf("malformed legacy store was rewritten: got %s", got)
	}
}

func TestPublicationPreparationPrecedesSessionCreation(t *testing.T) {
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "prepare", Request: Request{Name: "prepare", GitRoot: "/repo"}, State: StateSessionCreating, WorktreePath: "/worktrees/prepare"}); err != nil {
		t.Fatal(err)
	}
	var order []string
	f.publication.onPrepare = func(job Job) {
		order = append(order, "prepare:"+job.SessionID)
	}
	f.sessions.onEnsure = func(job Job) {
		order = append(order, "ensure:"+job.SessionID)
	}
	if err := f.manager.Process(context.Background(), "prepare"); err != nil {
		t.Fatal(err)
	}
	want := []string{"prepare:", "ensure:", "prepare:s_new"}
	if !reflect.DeepEqual(order, want) {
		t.Fatalf("publication/session order=%v, want %v", order, want)
	}
	if got := job(t, f.store, "prepare"); got.State != StateAwaitingEmacs || got.SessionID != "s_new" {
		t.Fatalf("prepared job=%#v, want awaiting_emacs bound to s_new", got)
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
	manager, err := NewManager(Config{Store: f.store, Planner: f.worktrees, Worktrees: f.worktrees, Geometry: f.geometry, Sessions: sessions, Health: f.health, Prompts: f.prompts, Available: f.available, Releases: f.releases, Publication: f.publication, HostActions: f.actions, Logf: func(string, ...any) {}, Errorf: func(string, ...any) {}})
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
	if !f.loggedFormat(`workspace="DWC/feature" project_dir=""`) {
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

// parkOnHost drives one job to awaiting_emacs, which is where every
// materialization-cadence test starts.
func parkOnHost(t *testing.T, f *fixture, id string) {
	t.Helper()
	if _, _, err := f.store.Enqueue(Job{ID: id, Request: Request{Name: "DWC/" + id, GitRoot: "/repo", Prompt: "build it"}, State: StateQueued}); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := f.manager.Process(context.Background(), id); err != nil {
		t.Fatalf("Process: %v", err)
	}
	if got := job(t, f.store, id); got.State != StateAwaitingEmacs {
		t.Fatalf("job %s state = %s, want awaiting_emacs", id, got.State)
	}
}

func TestAwaitingHostSweepReRequestsMaterializationAfterTheInterval(t *testing.T) {
	// Arrange: a job parked on the host with its one original request already
	// spent — exactly the state a workspace created while no Emacs host was
	// connected is left in.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "held")
	before := f.available.calls

	// Act: the interval elapses and the host-action worker sweeps.
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.available.calls != before+1 {
		t.Fatalf("available publishes = %d, want %d", f.available.calls, before+1)
	}
	if got := job(t, f.store, "held"); got.MaterializationRequests != 2 {
		t.Fatalf("materialization requests = %d, want 2", got.MaterializationRequests)
	}
	if !f.loggedFormat("RE-REQUESTING materialization") {
		t.Fatal("the re-request was not logged")
	}
}

func TestAwaitingHostSweepDoesNotReRequestInsideTheInterval(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "fresh")
	before := f.available.calls

	// Act: less than the interval has passed since the original request.
	f.advance(defaultMaterializationRequestInterval - time.Millisecond)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (no re-request inside the interval)", f.available.calls, before)
	}
}

func TestAwaitingHostSweepIgnoresJobsThatAreNotWaitingOnTheHost(t *testing.T) {
	// Arrange: an acknowledged job is past the host entirely.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "done")
	if err := f.manager.MarkMaterialized(context.Background(), "done"); err != nil {
		t.Fatalf("MarkMaterialized: %v", err)
	}
	before := f.available.calls

	// Act.
	f.advance(10 * defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (a materialized job is never re-requested)", f.available.calls, before)
	}
}

func TestAwaitingHostSweepStampsAJobPersistedBeforeTheCadenceExisted(t *testing.T) {
	// Arrange: a job whose park predates the durable timestamps, so both are
	// zero — the shape every job in an upgraded store has on its first sweep.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "legacy")
	if _, err := f.store.Update("legacy", func(j *Job) error {
		j.AwaitingEmacsSinceMs = 0
		j.MaterializationLastRequestMs = 0
		return nil
	}); err != nil {
		t.Fatalf("Update: %v", err)
	}

	// Act.
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert: it is dated from this sweep, not from the epoch.
	if got := job(t, f.store, "legacy"); got.AwaitingEmacsSinceMs != f.now().UnixMilli() {
		t.Fatalf("awaiting-since = %d, want %d", got.AwaitingEmacsSinceMs, f.now().UnixMilli())
	}
}

func TestAwaitingHostSweepKeepsAJobHeldWhenTheReRequestFails(t *testing.T) {
	// Arrange: a re-request that cannot be published must not consume the
	// job's attempt, or a transient host fault would end the cadence.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "held")
	spent := job(t, f.store, "held").MaterializationRequests
	f.available.err = errors.New("host channel is gone")

	// Act.
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if got := job(t, f.store, "held"); got.MaterializationRequests != spent {
		t.Fatalf("materialization requests = %d, want the failed re-request not to count (%d)", got.MaterializationRequests, spent)
	}
	if !f.loggedFormat("materialization RE-REQUEST FAILED") {
		t.Fatalf("logs = %v, want the failed re-request named", f.logs)
	}
}

func TestAwaitingHostSweepReportsAListingFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	f.manager.cfg.Store = listFailingStore{JobStore: f.store}

	// Act.
	err := f.manager.SweepAwaitingHost(context.Background())

	// Assert: a broken store is structural and propagates rather than being
	// mistaken for "no jobs are waiting".
	if err == nil || !strings.Contains(err.Error(), "awaiting-host sweep") {
		t.Fatalf("SweepAwaitingHost error = %v, want a listing failure", err)
	}
}

type listFailingStore struct{ JobStore }

func (s listFailingStore) List() ([]Job, error) { return nil, errors.New("store is unreadable") }

func TestHeldMaterializationPastTheDeadlineIsReportedAtErrorSeverity(t *testing.T) {
	// Arrange: a workspace nobody has answered for, which is what two jobs
	// spent DAYS being while every line about them was emitted at info.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")

	// Act.
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if !f.loggedErrorFormat("MATERIALIZATION HELD PAST DEADLINE") {
		t.Fatalf("error records = %v, want the held-materialization escalation", f.errorLogs)
	}
}

func TestHeldMaterializationEscalationSurfacesAUserVisibleFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")

	// Act.
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert: the host is handed one notice naming the job.
	var notice *HostAction
	for i := range f.actions.items {
		if f.actions.items[i].ID == "stuck:materialization-held" {
			notice = &f.actions.items[i]
		}
	}
	if notice == nil {
		t.Fatalf("host actions = %+v, want a held-materialization notice", f.actions.items)
	}
	var failure WorkspaceCreateFailure
	if err := json.Unmarshal(notice.Payload, &failure); err != nil {
		t.Fatalf("Unmarshal: %v", err)
	}
	if failure.JobID != "stuck" || !strings.Contains(failure.Error, "no editor materialized it") {
		t.Fatalf("failure payload = %+v, want it to name the job and the unanswered wait", failure)
	}
}

func TestHeldMaterializationEscalationIsReportedExactlyOnce(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}
	first := len(f.errorLogs)

	// Act: many more sweeps, long past the deadline.
	for i := 0; i < 5; i++ {
		f.advance(defaultMaterializationHeldDeadline)
		if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
			t.Fatalf("SweepAwaitingHost: %v", err)
		}
	}

	// Assert.
	if len(f.errorLogs) != first {
		t.Fatalf("error records = %d after repeated sweeps, want the %d from the single escalation", len(f.errorLogs), first)
	}
}

func TestHeldMaterializationEscalationLeavesTheJobRecoverable(t *testing.T) {
	// Arrange: the escalation is a REPORT, never a cleanup — the worktree,
	// session and shim are all real and all still the user's.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Act: a host finally connects and acknowledges.
	if err := f.manager.MarkMaterialized(context.Background(), "stuck"); err != nil {
		t.Fatalf("MarkMaterialized: %v", err)
	}

	// Assert: the held prompt is still delivered.
	got := job(t, f.store, "stuck")
	if got.State != StateReady || !got.PromptDelivered {
		t.Fatalf("job after a late acknowledgement = %#v, want ready with its prompt delivered", got)
	}
}

func TestEscalatedJobStopsBeingReRequested(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}
	before := f.available.calls

	// Act.
	f.advance(10 * defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (an escalated wait stops re-requesting)", f.available.calls, before)
	}
}

func TestManagerRefusesConstructionWithoutAnErrorLogger(t *testing.T) {
	// Arrange: a manager that can only speak at info cannot report a fault as
	// one, which is exactly how the incident hid.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))

	// Act.
	_, err := NewManager(Config{
		Store: f.store, Planner: f.worktrees, Worktrees: f.worktrees, Geometry: f.geometry,
		Sessions: f.sessions, Health: f.health, Prompts: f.prompts, Available: f.available,
		Releases: f.releases, Publication: f.publication, HostActions: f.actions, Logf: f.log,
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "error-level logger") {
		t.Fatalf("NewManager error = %v, want a refusal naming the missing error-level logger", err)
	}
}

// faultyStore fails one named durable operation, so the escalation's own error
// paths can be exercised instead of assumed.
type faultyStore struct {
	JobStore
	failUpdate        string
	failEnqueueAction bool
}

func (s *faultyStore) Update(id string, change func(*Job) error) (Job, error) {
	if id == s.failUpdate {
		return Job{}, errors.New("store is unwritable")
	}
	return s.JobStore.Update(id, change)
}

func (s *faultyStore) EnqueueHostAction(action HostAction) (HostAction, bool, error) {
	if s.failEnqueueAction {
		return HostAction{}, false, errors.New("store is unwritable")
	}
	return s.JobStore.EnqueueHostAction(action)
}

func TestHeldMaterializationEscalationThatCannotLatchReportsNothingAndRetries(t *testing.T) {
	// Arrange: a latch that cannot be written must not produce a report, or the
	// next sweep repeats it forever.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.manager.cfg.Store = &faultyStore{JobStore: f.store, failUpdate: "stuck"}

	// Act.
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if f.loggedErrorFormat("MATERIALIZATION HELD PAST DEADLINE") {
		t.Fatalf("error records = %v, want no report when the latch could not be written", f.errorLogs)
	}
	if !f.loggedFormat("held-materialization escalation could not latch") {
		t.Fatalf("logs = %v, want the failed latch named", f.logs)
	}
}

func TestHeldMaterializationEscalationThatCannotEnqueueIsLoud(t *testing.T) {
	// Arrange.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.manager.cfg.Store = &faultyStore{JobStore: f.store, failEnqueueAction: true}

	// Act.
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	if !f.loggedErrorFormat("HELD MATERIALIZATION NOT SURFACED") {
		t.Fatalf("error records = %v, want the unsurfaceable escalation reported as a fault", f.errorLogs)
	}
}

func TestHeldMaterializationNoticeIsRetainedWhenTheHostRefusesIt(t *testing.T) {
	// Arrange: a host that cannot take the notice right now must not cause the
	// notice to be lost — it stays pending for the next drain.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "stuck")
	f.actions.err = errors.New("host is gone")

	// Act.
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert.
	pending, err := f.store.PendingHostActions()
	if err != nil {
		t.Fatalf("PendingHostActions: %v", err)
	}
	found := false
	for _, action := range pending {
		if action.ID == "stuck:materialization-held" {
			found = true
		}
	}
	if !found {
		t.Fatalf("pending host actions = %+v, want the retained held-materialization notice", pending)
	}
}

func TestHostActionWorkerRunsTheAwaitingHostSweep(t *testing.T) {
	// Arrange: the sweep must be owned by the worker that owns host
	// publication, not by a caller that happens to remember to run it.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	parkOnHost(t, f, "worker")
	before := f.available.calls
	f.advance(defaultMaterializationRequestInterval)

	// Act: exactly what RunHostActionWorker does on one wake.
	f.manager.RouteHostActions()
	for f.manager.drainHostOnce(context.Background()) {
	}

	// Assert.
	if f.available.calls != before+1 {
		t.Fatalf("available publishes = %d, want %d", f.available.calls, before+1)
	}
}

// ---------------------------------------------------------------------------
// The publication hold: a job the host HAS materialized whose frames are still
// gated because the release never opened. It had no reporter and no retry, so
// one workspace sat behind a closed gate for 25 minutes and 6294 hold records
// with nothing above info severity to say so.
// ---------------------------------------------------------------------------

// gateStuck parks a job on the host, materializes it with a FAILING release,
// and returns it in the exact durable shape the wedge produced: materialized,
// never published-released.
func gateStuck(t *testing.T, f *fixture, id string) Job {
	t.Helper()
	parkOnHost(t, f, id)
	f.releases.err = errors.New("release open lost publication decision")
	if err := f.manager.MarkMaterialized(context.Background(), id); err == nil {
		t.Fatalf("MarkMaterialized with a failing release: err = nil, want the release failure")
	}
	got := job(t, f.store, id)
	if !got.Materialized || got.PublicationReleased {
		t.Fatalf("job after the failed release = %#v, want materialized and unreleased", got)
	}
	return got
}

func TestPublicationStillGatedReportsAMaterializedJobWhoseReleaseNeverOpened(t *testing.T) {
	// Arrange
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	stuck := gateStuck(t, f, "gated")

	// Act / Assert
	if !publicationStillGated(stuck) {
		t.Fatalf("publicationStillGated(%#v) = false, want true", stuck)
	}
}

func TestPublicationStillGatedIgnoresAJobWithNoSessionYet(t *testing.T) {
	// Arrange — a job whose identity is not settled cannot be gate-stuck; it is
	// still on the ordinary bring-up path.
	unsettled := Job{ID: "early", State: StateEmacsMaterialized, Materialized: true}

	// Act / Assert
	if publicationStillGated(unsettled) {
		t.Fatal("publicationStillGated on a session-less job = true, want false")
	}
}

func TestPublicationStillGatedIgnoresAReleasedJob(t *testing.T) {
	// Arrange
	released := Job{ID: "done", State: StateReady, SessionID: "s_1", Materialized: true, PublicationReleased: true}

	// Act / Assert
	if publicationStillGated(released) {
		t.Fatal("publicationStillGated on a released job = true, want false")
	}
}

func TestAwaitingHostSweepRedrivesAHeldPublicationRelease(t *testing.T) {
	// Arrange — the wedge, with the transient release failure now cleared: the
	// sweep must complete the step that did not, rather than leaving the
	// workspace blank behind a gate the durable record says should be open.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	gateStuck(t, f, "gated")
	f.releases.err = nil

	// Act
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert
	if got := job(t, f.store, "gated"); !got.PublicationReleased {
		t.Fatalf("job after the sweep = %#v, want the publication released", got)
	}
}

func TestAwaitingHostSweepNeverReAnnouncesAGatedJobToTheHost(t *testing.T) {
	// Arrange — the host already answered for this workspace, so re-asking it
	// to materialize one it holds is the daemon repeating an answered question.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	gateStuck(t, f, "gated")
	f.releases.err = nil
	before := f.available.calls

	// Act
	f.advance(defaultMaterializationRequestInterval)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert
	if f.available.calls != before {
		t.Fatalf("available publishes = %d, want %d (a gated job is re-released, never re-announced)", f.available.calls, before)
	}
}

func TestHeldPublicationPastTheDeadlineIsReportedAtErrorSeverity(t *testing.T) {
	// Arrange — the release keeps failing, which is the shape that used to hold
	// forever in silence.
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	gateStuck(t, f, "gated")

	// Act
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert
	if !f.loggedErrorFormat("publication_release_never_opened") {
		t.Fatalf("error records = %v, want the held-publication escalation naming its own cause", f.errorLogs)
	}
}

func TestHeldPublicationEscalationSurfacesAUserVisibleFailure(t *testing.T) {
	// Arrange
	f := newFixture(t, filepath.Join(t.TempDir(), "jobs.json"))
	gateStuck(t, f, "gated")

	// Act
	f.advance(defaultMaterializationHeldDeadline)
	if err := f.manager.SweepAwaitingHost(context.Background()); err != nil {
		t.Fatalf("SweepAwaitingHost: %v", err)
	}

	// Assert — the user is told the workspace is blank because its frames are
	// gated, not that no editor ever answered.
	var notice *HostAction
	for i := range f.actions.items {
		if f.actions.items[i].Type == HostActionTypeWorkspaceMaterializationHeld {
			notice = &f.actions.items[i]
		}
	}
	if notice == nil {
		t.Fatalf("host actions = %#v, want a materialization-held notice", f.actions.items)
	}
	if !strings.Contains(string(notice.Payload), "publication release never opened") {
		t.Fatalf("notice payload = %s, want the gated-frames explanation", notice.Payload)
	}
}

func TestSessionPublicationDecisionCarriesTheJobsOwnSession(t *testing.T) {
	// Arrange — a frame that arrives BEFORE the job has a session at all is
	// exactly what used to memoize an empty session id as the job's identity,
	// which the release could then never match.
	root := t.TempDir()
	f := newFixture(t, filepath.Join(root, "jobs.json"))
	if _, _, err := f.store.Enqueue(Job{ID: "held", Request: Request{Name: "DWC/held", GitRoot: "/repo"}, State: StateAwaitingEmacs, WorktreePath: "/worktrees/held", SessionID: "s_held"}); err != nil {
		t.Fatal(err)
	}

	// Act
	decision, err := SessionPublicationDecision(f.store, "/worktrees/held", "")

	// Assert
	if err != nil || decision.SessionID != "s_held" {
		t.Fatalf("decision = %#v err=%v, want the job's own session s_held", decision, err)
	}
}
