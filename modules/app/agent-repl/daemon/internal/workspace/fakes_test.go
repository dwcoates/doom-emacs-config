package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/prompts"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// fixedNow is the instant every stamping assertion is made against.
var fixedNow = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// errFake is what a fake answers when a test arranged a failure without caring
// which one.
var errFake = errors.New("workspace test: arranged failure")

// fakeDB is a wsm.DB whose unused methods panic on use. Embedding the interface
// keeps each test's arrangement to exactly the calls it cares about; a call
// outside that set nil-panics, which is the loud failure the test wants.
type fakeDB struct {
	wsm.DB

	workspaces   map[ids.WorkspaceID]wsm.Workspace
	byDir        map[string]wsm.Workspace
	repositories []wsm.Repository
	tasks        []wsm.Task
	current      *ids.WorkspaceID
	sessions     map[ids.WorkspaceID]wsm.Session
	// sessionErr makes every session read fail, which is the only way to
	// reach the roster's session-read error branch: the fake's own map
	// cannot fail.
	sessionErr error
	jobs       map[ids.WorkspaceID]wsm.CreationJob
	held       map[ids.WorkspaceID][]wsm.HeldPrompt

	registerErr error
	registered  []wsm.RegisterFacts
	registerDir string
	createdNew  bool

	putJobs     []wsm.CreationJob
	putJobErr   error
	putSessions []wsm.Session
	putTurns    []wsm.Turn
	// hasTurnsErr makes the turns existence read fail, which the fake's own
	// slice cannot.
	hasTurnsErr error
	// conversations is what ConversationPrompts answers per workspace, and
	// portedPrompts is what PutPortedPrompts recorded.
	conversations   map[ids.WorkspaceID][]wsm.PortedPrompt
	conversationErr error
	portedPrompts   map[ids.WorkspaceID][]wsm.PortedPrompt
	putPortedErr    error
	closedFlags     map[ids.WorkspaceID]bool
	// setClosedErr makes the closed-flag write fail, which the map cannot.
	setClosedErr error
	priorities   map[ids.WorkspaceID]*wsm.Priority
	attention    map[ids.WorkspaceID]bool
	currentAt    time.Time
	forgotten    []ids.WorkspaceID
	// forgetReport is what Forget answers, and forgetErr makes it fail; the
	// fake's own map cannot do either on its own.
	forgetReport wsm.ForgetReport
	forgetErr    error
	terminals    map[ids.WorkspaceID]wsm.SessionTerminal
	orphanReport wsm.OrphanReport
	createdTasks []string
	taskChanges  map[ids.TaskID]wsm.TaskChange
	assignments  map[ids.WorkspaceID]*ids.TaskID
	taskErr      error

	// dbFaults is the fault table the fleet opens and closes lost-link rows
	// in; dbClosed records the ids CloseFault was called with.
	dbFaults []wsm.Fault
	dbClosed []ids.FaultID
}

func (d *fakeDB) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	id := ids.FaultID(fmt.Sprintf("db-fault-%d", len(d.dbFaults)+1))
	f.ID = id
	d.dbFaults = append(d.dbFaults, f)
	return id, nil
}

func (d *fakeDB) CloseFault(_ context.Context, id ids.FaultID, _ time.Time) error {
	d.dbClosed = append(d.dbClosed, id)
	kept := d.dbFaults[:0]
	for _, f := range d.dbFaults {
		if f.ID != id {
			kept = append(kept, f)
		}
	}
	d.dbFaults = kept
	return nil
}

func (d *fakeDB) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	var out []wsm.Fault
	for _, f := range d.dbFaults {
		if scope.Kind != "" && f.Kind != scope.Kind {
			continue
		}
		if scope.Workspace != nil && (f.Workspace == nil || *f.Workspace != *scope.Workspace) {
			continue
		}
		out = append(out, f)
	}
	return out, nil
}

func newFakeDB() *fakeDB {
	return &fakeDB{
		workspaces:  map[ids.WorkspaceID]wsm.Workspace{},
		byDir:       map[string]wsm.Workspace{},
		sessions:    map[ids.WorkspaceID]wsm.Session{},
		jobs:        map[ids.WorkspaceID]wsm.CreationJob{},
		held:        map[ids.WorkspaceID][]wsm.HeldPrompt{},
		closedFlags: map[ids.WorkspaceID]bool{},
		priorities:  map[ids.WorkspaceID]*wsm.Priority{},
		attention:   map[ids.WorkspaceID]bool{},
		terminals:   map[ids.WorkspaceID]wsm.SessionTerminal{},
		taskChanges: map[ids.TaskID]wsm.TaskChange{},
		assignments: map[ids.WorkspaceID]*ids.TaskID{},
	}
}

// with records one workspace in the fake registry, keyed both ways.
func (d *fakeDB) with(ws wsm.Workspace) *fakeDB {
	d.workspaces[ws.ID] = ws
	d.byDir[ws.Dir] = ws
	return d
}

func (d *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	ws, ok := d.workspaces[id]
	if !ok {
		return wsm.Workspace{}, errors.New("no such workspace")
	}
	return ws, nil
}

func (d *fakeDB) WorkspaceByDir(_ context.Context, dir string) (wsm.Workspace, error) {
	ws, ok := d.byDir[dir]
	if !ok {
		return wsm.Workspace{}, errors.New("no workspace at that dir")
	}
	return ws, nil
}

func (d *fakeDB) RegisterWorkspace(_ context.Context, dir string, facts wsm.RegisterFacts) (wsm.Workspace, bool, error) {
	if d.registerErr != nil {
		return wsm.Workspace{}, false, d.registerErr
	}
	d.registerDir = dir
	d.registered = append(d.registered, facts)
	if existing, ok := d.byDir[dir]; ok {
		return existing, false, nil
	}
	ws := wsm.Workspace{
		ID: ids.WorkspaceID("ws-" + filepath.Base(dir)), Dir: dir, Repo: "repo-1",
		Name: facts.Name, Branch: facts.Branch, ParentBranch: facts.ParentBranch,
		Parent: facts.Parent,
	}
	d.with(ws)
	d.createdNew = true
	return ws, true, nil
}

func (d *fakeDB) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	out := make([]wsm.Workspace, 0, len(d.workspaces))
	for _, ws := range d.workspaces {
		out = append(out, ws)
	}
	return out, nil
}

func (d *fakeDB) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return d.repositories, nil
}

func (d *fakeDB) Tasks(context.Context) ([]wsm.Task, error) { return d.tasks, nil }

func (d *fakeDB) Current(context.Context) (*ids.WorkspaceID, error) { return d.current, nil }

func (d *fakeDB) SetClosed(_ context.Context, id ids.WorkspaceID, closed bool) error {
	if d.setClosedErr != nil {
		return d.setClosedErr
	}
	d.closedFlags[id] = closed
	return nil
}

func (d *fakeDB) SetCurrent(_ context.Context, id ids.WorkspaceID, at time.Time) error {
	d.current, d.currentAt = &id, at
	return nil
}

func (d *fakeDB) SetAttention(_ context.Context, id ids.WorkspaceID, on bool) error {
	d.attention[id] = on
	return nil
}

func (d *fakeDB) SetPriority(_ context.Context, id ids.WorkspaceID, p *wsm.Priority) error {
	d.priorities[id] = p
	return nil
}

func (d *fakeDB) Forget(_ context.Context, id ids.WorkspaceID) (wsm.ForgetReport, error) {
	if d.forgetErr != nil {
		return wsm.ForgetReport{}, d.forgetErr
	}
	d.forgotten = append(d.forgotten, id)
	delete(d.workspaces, id)
	return d.forgetReport, nil
}

func (d *fakeDB) PutCreationJob(_ context.Context, job wsm.CreationJob) error {
	if d.putJobErr != nil {
		return d.putJobErr
	}
	d.putJobs = append(d.putJobs, job)
	d.jobs[job.Workspace] = job
	return nil
}

func (d *fakeDB) CreationJob(_ context.Context, id ids.WorkspaceID) (wsm.CreationJob, bool, error) {
	job, ok := d.jobs[id]
	return job, ok, nil
}

func (d *fakeDB) PutSession(_ context.Context, s wsm.Session) error {
	d.putSessions = append(d.putSessions, s)
	d.sessions[s.Workspace] = s
	return nil
}

func (d *fakeDB) Session(_ context.Context, id ids.WorkspaceID) (wsm.Session, bool, error) {
	if d.sessionErr != nil {
		return wsm.Session{}, false, d.sessionErr
	}
	s, ok := d.sessions[id]
	return s, ok, nil
}

func (d *fakeDB) SetSessionTerminal(_ context.Context, id ids.WorkspaceID, t wsm.SessionTerminal) error {
	d.terminals[id] = t
	return nil
}

func (d *fakeDB) PutTurn(_ context.Context, t wsm.Turn) error {
	d.putTurns = append(d.putTurns, t)
	return nil
}

// HasTurns answers off the same recorded turns PutTurn collects, so a test
// that arranges an engaged workspace does it by recording a turn rather than
// by setting a flag the production store does not have. hasTurnsErr is the
// only way to reach the read-failure branch: the fake's own slice cannot fail.
func (d *fakeDB) HasTurns(_ context.Context, id ids.WorkspaceID) (bool, error) {
	if d.hasTurnsErr != nil {
		return false, d.hasTurnsErr
	}
	for _, t := range d.putTurns {
		if t.Workspace == id {
			return true, nil
		}
	}
	return false, nil
}

// ConversationPrompts answers what a fork of this workspace inherits. The
// fixture holds it per workspace so a fork test states the parent's
// conversation directly rather than driving turns through the fake.
func (d *fakeDB) ConversationPrompts(_ context.Context, id ids.WorkspaceID) ([]wsm.PortedPrompt, error) {
	if d.conversationErr != nil {
		return nil, d.conversationErr
	}
	return d.conversations[id], nil
}

func (d *fakeDB) PortedPrompts(_ context.Context, id ids.WorkspaceID) ([]wsm.PortedPrompt, error) {
	return d.portedPrompts[id], nil
}

func (d *fakeDB) PutPortedPrompts(_ context.Context, id ids.WorkspaceID, rows []wsm.PortedPrompt) error {
	if d.putPortedErr != nil {
		return d.putPortedErr
	}
	if d.portedPrompts == nil {
		d.portedPrompts = map[ids.WorkspaceID][]wsm.PortedPrompt{}
	}
	d.portedPrompts[id] = rows
	return nil
}

func (d *fakeDB) HeldPrompts(_ context.Context, id ids.WorkspaceID) ([]wsm.HeldPrompt, error) {
	return d.held[id], nil
}

func (d *fakeDB) CloseOrphans(context.Context, ids.WorkspaceID, time.Time) (wsm.OrphanReport, error) {
	return d.orphanReport, nil
}

func (d *fakeDB) CreateTask(_ context.Context, title string) (wsm.Task, error) {
	if d.taskErr != nil {
		return wsm.Task{}, d.taskErr
	}
	d.createdTasks = append(d.createdTasks, title)
	return wsm.Task{ID: ids.TaskID("task-1"), Title: title}, nil
}

func (d *fakeDB) UpdateTask(_ context.Context, id ids.TaskID, change wsm.TaskChange) error {
	if d.taskErr != nil {
		return d.taskErr
	}
	d.taskChanges[id] = change
	return nil
}

func (d *fakeDB) AssignWorkspaceTask(_ context.Context, id ids.WorkspaceID, task *ids.TaskID) error {
	d.assignments[id] = task
	return nil
}

// fakeGit is a gitclient.Git whose answers the test arranges.
type fakeGit struct {
	gitclient.Git

	commonDir     string
	mainWorktree  string
	commonDirErr  error
	currentBranch string
	branchErr     error
	defaultBranch string
	defaultErr    error

	resolveErr error

	created   []createdWorktree
	createErr error
	nuked     []nukedWorktree
	nukeErr   error
}

type createdWorktree struct{ RepoDir, Branch, BaseRef, WorktreeDir string }
type nukedWorktree struct{ RepoDir, WorktreeDir, Branch string }

func (g *fakeGit) ResolveRef(_ context.Context, _, ref string) (string, error) {
	if g.resolveErr != nil {
		return "", g.resolveErr
	}
	return "sha-of-" + ref, nil
}

func (g *fakeGit) CommonDir(context.Context, string) (string, error) {
	return g.commonDir, g.commonDirErr
}

// MainWorktree answers what registration derives the repository from. The
// fixture keys it the same way CommonDir is keyed, since a fake repository has
// exactly one of each.
func (g *fakeGit) MainWorktree(context.Context, string) (string, error) {
	return g.mainWorktree, g.commonDirErr
}

func (g *fakeGit) CurrentBranch(context.Context, string) (string, error) {
	return g.currentBranch, g.branchErr
}

func (g *fakeGit) DefaultBranch(context.Context, string) (string, error) {
	return g.defaultBranch, g.defaultErr
}

func (g *fakeGit) CreateWorktree(_ context.Context, repoDir, branch, baseRef, worktreeDir string) error {
	if g.createErr != nil {
		return g.createErr
	}
	g.created = append(g.created, createdWorktree{repoDir, branch, baseRef, worktreeDir})
	return os.MkdirAll(filepath.Join(worktreeDir, ".git"), 0o755)
}

func (g *fakeGit) Nuke(_ context.Context, repoDir, worktreeDir, branch string) error {
	if g.nukeErr != nil {
		return g.nukeErr
	}
	g.nuked = append(g.nuked, nukedWorktree{repoDir, worktreeDir, branch})
	return nil
}

// fakeAccounts is an account.Resolver.
type fakeAccounts struct {
	account.Resolver

	configDir     string
	transcript    account.Transcript
	transcriptErr error
	ported        []portedTranscript
	portErr       error
	// mint is the fork mapping PortTranscript answers with; nil takes a
	// readable default so a test that does not care still gets one mapping.
	mint account.RemintedID
	// email is the signed-in address Read answers with; empty is logged out.
	email string
	// readErr makes Read fail, which registration must surface.
	readErr error
	// moved records MoveTranscript's account switches; moveErr fails them.
	moved   []movedTranscript
	moveErr error
}

type movedTranscript struct{ Path, ToConfigDir, WorkspaceDir string }

type portedTranscript struct{ Path, ConfigDir, WorkspaceDir, VendorSessionID string }

func (a *fakeAccounts) ConfigDirFor(string) string { return a.configDir }

// Read answers the account the fixture holds; an unset email is the logged-out
// arm, which is an answer and not a failure.
func (a *fakeAccounts) Read(_ context.Context, configDir string) (account.Account, error) {
	if a.readErr != nil {
		return account.Account{}, a.readErr
	}
	return account.Account{ConfigDir: configDir, Email: a.email, LoggedIn: a.email != ""}, nil
}

func (a *fakeAccounts) FindTranscript(context.Context, string, string) (account.Transcript, error) {
	return a.transcript, a.transcriptErr
}

func (a *fakeAccounts) PortTranscript(_ context.Context, path, configDir, workspaceDir, vendorSessionID string) (account.RemintedID, error) {
	if a.portErr != nil {
		return nil, a.portErr
	}
	a.ported = append(a.ported, portedTranscript{path, configDir, workspaceDir, vendorSessionID})
	if a.mint != nil {
		return a.mint, nil
	}
	return func(old string) string { return "minted-" + old }, nil
}

// MoveTranscript records the account switch's port, which is what makes a
// resume land under the root the workspace now routes to.
func (a *fakeAccounts) MoveTranscript(_ context.Context, path, toConfigDir, workspaceDir string) error {
	if a.moveErr != nil {
		return a.moveErr
	}
	a.moved = append(a.moved, movedTranscript{path, toConfigDir, workspaceDir})
	return nil
}

// fakeQueue is a promptqueue.Queue that records what it was handed.
type fakeQueue struct {
	promptqueue.Queue

	submissions []promptqueue.Submission
	submitErr   error
	acts        map[ids.WorkspaceID][]promptqueue.Act
	actErr      error
}

func newFakeQueue() *fakeQueue {
	return &fakeQueue{acts: map[ids.WorkspaceID][]promptqueue.Act{}}
}

func (q *fakeQueue) Submit(_ context.Context, sub promptqueue.Submission) (promptqueue.Disposition, error) {
	if q.submitErr != nil {
		return promptqueue.Disposition{}, q.submitErr
	}
	q.submissions = append(q.submissions, sub)
	return promptqueue.Disposition{Delivered: true}, nil
}

func (q *fakeQueue) SubmitSessionAct(_ context.Context, ws ids.WorkspaceID, act promptqueue.Act) error {
	if q.actErr != nil {
		return q.actErr
	}
	q.acts[ws] = append(q.acts[ws], act)
	return nil
}

// fakeMerge is a merge.Orchestrator.
type fakeMerge struct {
	merge.Orchestrator

	facts       map[ids.WorkspaceID]footer.MergeFacts
	interrupted []ids.WorkspaceID
	closed      []ids.WorkspaceID
	enqueued    []ids.WorkspaceID
	enqueueErr  error
}

func newFakeMerge() *fakeMerge {
	return &fakeMerge{facts: map[ids.WorkspaceID]footer.MergeFacts{}}
}

func (m *fakeMerge) Facts(ws ids.WorkspaceID) (footer.MergeFacts, bool) {
	f, ok := m.facts[ws]
	return f, ok
}

func (m *fakeMerge) OnInterrupt(_ context.Context, ws ids.WorkspaceID) {
	m.interrupted = append(m.interrupted, ws)
}

func (m *fakeMerge) OnWorkspaceClosed(_ context.Context, ws ids.WorkspaceID) {
	m.closed = append(m.closed, ws)
}

func (m *fakeMerge) Enqueue(_ context.Context, ws ids.WorkspaceID) error {
	if m.enqueueErr != nil {
		return m.enqueueErr
	}
	m.enqueued = append(m.enqueued, ws)
	return nil
}

// fakeRollout is a rollout.Controller.
type fakeRollout struct {
	rollout.Controller

	mu          sync.Mutex
	relaunches  []rolloutCall
	relaunchErr error
	reloads     []ids.WorkspaceID
	reloadErr   error
	// done fires once per finished relaunch. The restart verb ACCEPTS and
	// runs the engine behind it, so a test synchronizes on this rather than
	// on the verb's return.
	done chan struct{}
}

type rolloutCall struct {
	WS     ids.WorkspaceID
	Reason rollout.RelaunchReason
}

func (r *fakeRollout) RelaunchShim(_ context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason) error {
	r.mu.Lock()
	r.relaunches = append(r.relaunches, rolloutCall{ws, reason})
	err := r.relaunchErr
	r.mu.Unlock()
	if err != nil {
		r.signal()
	}
	return err
}

func (r *fakeRollout) ReloadWebapp(_ context.Context, ws ids.WorkspaceID) error {
	r.mu.Lock()
	r.reloads = append(r.reloads, ws)
	err := r.reloadErr
	r.mu.Unlock()
	r.signal()
	return err
}

func (r *fakeRollout) signal() {
	select {
	case r.done <- struct{}{}:
	default:
	}
}

// relaunchCalls answers the recorded relaunches under the fake's own lock.
func (r *fakeRollout) relaunchCalls() []rolloutCall {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]rolloutCall(nil), r.relaunches...)
}

// reloadCalls answers the recorded webapp reloads under the fake's own lock.
func (r *fakeRollout) reloadCalls() []ids.WorkspaceID {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]ids.WorkspaceID(nil), r.reloads...)
}

// awaitRelaunch blocks until the restart's background engine has run.
func (r *fakeRollout) awaitRelaunch(t *testing.T) {
	t.Helper()
	select {
	case <-r.done:
	case <-time.After(5 * time.Second):
		t.Fatal("the restart's relaunch engine never ran")
	}
}

// awaitRecord blocks until a record with this level and operation reaches the
// captured log, and fails with everything captured when the bound runs out.
//
// It exists for the ASYNCHRONOUS verbs: their failures are recorded on a
// goroutine the verb does not join, so the arrival of a record is an event to
// wait for rather than a state to read. The capture buffer is mutex-guarded
// and Records copies, so polling it races with nothing.
func awaitRecord(t *testing.T, f *fixture, level, operation string) {
	t.Helper()
	deadline := time.After(recordDeadline)
	ticker := time.NewTicker(time.Millisecond)
	defer ticker.Stop()
	for {
		for _, r := range f.log.logger.Records() {
			if r.Level == level && r.Operation == operation {
				return
			}
		}
		select {
		case <-ticker.C:
		case <-deadline:
			t.Fatalf("records = %+v, want a %s record for %s", f.log.logger.Records(), level, operation)
			return
		}
	}
}

// recordDeadline is how long awaitRecord waits for an already-running
// goroutine to reach its next statement. It is a FAILURE bound, never a
// synchronization device: every wait returns the moment its record lands.
const recordDeadline = 5 * time.Second

// fakeFeed is a feed.Resolver that records the rows it was given.
type fakeFeed struct {
	feed.Resolver

	synthesized []*frontendv1.FeedRow
}

func (f *fakeFeed) UpsertSynthesized(_ ids.WorkspaceID, _ feedid.Feed, row *frontendv1.FeedRow) {
	f.synthesized = append(f.synthesized, row)
}

// fakeFooter is a footer.Resolver.
type fakeFooter struct {
	footer.Resolver

	closing      map[ids.WorkspaceID]*footer.CloseBlocked
	closingSet   int
	coldGates    map[ids.WorkspaceID]footer.ColdGate
	interrupting map[ids.WorkspaceID]bool
	// startFailed is the bring-up failure each workspace was told to stand.
	startFailed map[ids.WorkspaceID]*footer.StartFailed
	// dirs is what registration bound, keyed by workspace.
	dirs map[ids.WorkspaceID]string
}

func newFakeFooter() *fakeFooter {
	return &fakeFooter{
		closing:      map[ids.WorkspaceID]*footer.CloseBlocked{},
		coldGates:    map[ids.WorkspaceID]footer.ColdGate{},
		interrupting: map[ids.WorkspaceID]bool{},
		startFailed:  map[ids.WorkspaceID]*footer.StartFailed{},
	}
}

func (f *fakeFooter) SetClosing(ws ids.WorkspaceID, blocked *footer.CloseBlocked) {
	f.closing[ws] = blocked
	f.closingSet++
}

func (f *fakeFooter) SetColdGate(ws ids.WorkspaceID, gate footer.ColdGate) {
	f.coldGates[ws] = gate
}

func (f *fakeFooter) SetInterrupting(ws ids.WorkspaceID, on bool) {
	f.interrupting[ws] = on
}

func (f *fakeFooter) SetStartFailed(ws ids.WorkspaceID, failure *footer.StartFailed) {
	f.startFailed[ws] = failure
}

// fakeSidebar is a sidebar.Resolver.
type fakeSidebar struct {
	sidebar.Resolver

	registries []sidebar.Registry
	selected   []ids.WorkspaceID
	// calls records the ORDER the resolver was told things in, which is what
	// decides whether a push carries a whole view or a half-refreshed one.
	calls []string
}

func (s *fakeSidebar) SetRegistry(reg sidebar.Registry) {
	s.registries = append(s.registries, reg)
	s.calls = append(s.calls, "registry")
}

func (s *fakeSidebar) SetSelected(ws ids.WorkspaceID) {
	s.selected = append(s.selected, ws)
	s.calls = append(s.calls, "selected")
}

// fakeHost is a HostRelay.
type fakeHost struct {
	editorOpens []editorOpen
	reloads     []ids.WorkspaceID
	notes       []hostNote
	// hostPublishes records every host-state republish the verbs asked for.
	hostPublishes []ids.WorkspaceID
}

func (h *fakeHost) PublishHostWorkspace(ws ids.WorkspaceID) {
	h.hostPublishes = append(h.hostPublishes, ws)
}

type editorOpen struct {
	WS   ids.WorkspaceID
	Path string
	Line *uint32
}

type hostNote struct {
	WS                       ids.WorkspaceID
	Text, Kind, Tool, Header string
}

func (h *fakeHost) OpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	h.editorOpens = append(h.editorOpens, editorOpen{ws, path, line})
}

func (h *fakeHost) ReloadWebapp(ws ids.WorkspaceID) { h.reloads = append(h.reloads, ws) }

func (h *fakeHost) Notify(ws ids.WorkspaceID, note sessionwatcher.HostNotification) {
	h.notes = append(h.notes, hostNote{ws, note.Text, string(note.Kind), note.ToolName, note.Header})
}

// fakeSessions is a Sessions fleet.
type fakeSessions struct {
	live     map[ids.WorkspaceID]bool
	started  []ids.WorkspaceID
	startErr error
	stopped  []stopCall
	stopErr  error
	// resumes are the cold-gate re-opens the fleet was asked for, and resumeErr
	// is the refusal it answers with instead.
	resumes   []ColdResume
	resumeErr error
}

type stopCall struct {
	WS    ids.WorkspaceID
	Force bool
}

func newFakeSessions() *fakeSessions {
	return &fakeSessions{live: map[ids.WorkspaceID]bool{}}
}

func (s *fakeSessions) Start(_ context.Context, ws ids.WorkspaceID) error {
	if s.startErr != nil {
		return s.startErr
	}
	s.started = append(s.started, ws)
	s.live[ws] = true
	return nil
}

func (s *fakeSessions) Stop(_ context.Context, ws ids.WorkspaceID, force bool) error {
	if s.stopErr != nil {
		return s.stopErr
	}
	s.stopped = append(s.stopped, stopCall{ws, force})
	delete(s.live, ws)
	return nil
}

func (s *fakeSessions) Live(ws ids.WorkspaceID) bool { return s.live[ws] }

func (s *fakeSessions) ResumeCold(_ context.Context, ws ids.WorkspaceID, resume ColdResume) error {
	if s.resumeErr != nil {
		return s.resumeErr
	}
	s.resumes = append(s.resumes, resume)
	s.live[ws] = true
	return nil
}

// fakeShim is the narrow Shim surface.
type fakeShim struct {
	killedTurns    []killedTurn
	killTurnErr    error
	stoppedAgents  []string
	stopAgentErr   error
	stoppedShells  []string
	stopBashErr    error
	answers        []deliveredAnswer
	answerErr      error
	killedSession  []bool
	killSessionErr error
}

type killedTurn struct {
	Turn  ids.TurnID
	Force bool
}

type deliveredAnswer struct {
	Agent  string
	Answer *conversationv1.AgentAnswer
}

func (s *fakeShim) KillTurn(_ context.Context, turn ids.TurnID, force bool) error {
	if s.killTurnErr != nil {
		return s.killTurnErr
	}
	s.killedTurns = append(s.killedTurns, killedTurn{turn, force})
	return nil
}

func (s *fakeShim) StopAgent(_ context.Context, agent *conversationv1.AgentId) error {
	if s.stopAgentErr != nil {
		return s.stopAgentErr
	}
	s.stoppedAgents = append(s.stoppedAgents, agent.GetValue())
	return nil
}

func (s *fakeShim) StopBash(_ context.Context, work *conversationv1.DetachedWorkId) error {
	if s.stopBashErr != nil {
		return s.stopBashErr
	}
	s.stoppedShells = append(s.stoppedShells, work.GetValue())
	return nil
}

func (s *fakeShim) Answer(_ context.Context, agent *conversationv1.AgentId, answer *conversationv1.AgentAnswer) error {
	if s.answerErr != nil {
		return s.answerErr
	}
	s.answers = append(s.answers, deliveredAnswer{agent.GetValue(), answer})
	return nil
}

func (s *fakeShim) KillSession(_ context.Context, force bool) error {
	if s.killSessionErr != nil {
		return s.killSessionErr
	}
	s.killedSession = append(s.killedSession, force)
	return nil
}

// fakeBrowser is an externalbrowser.Opener.
type fakeBrowser struct {
	opened []string
	err    error
}

func (b *fakeBrowser) Open(_ context.Context, url string) error {
	if b.err != nil {
		return b.err
	}
	b.opened = append(b.opened, url)
	return nil
}

// fakeOwnership answers one standing for every workspace.
type fakeOwnership struct {
	standing Standing
	err      error
}

func (o *fakeOwnership) Standing(context.Context, ids.WorkspaceID) (Standing, error) {
	return o.standing, o.err
}

// fakeCards is the served-card store the answer verbs echo against.
type fakeCards struct {
	permissions map[string]ServedPermission
	questions   map[string]ServedQuestion
	coldGate    *ServedColdGate
	// coldGatesCleared counts the retirements the resolve path made.
	coldGatesCleared int
	modes            []string
	hasModes         bool
	models           []string
	hasModels        bool
}

func newFakeCards() *fakeCards {
	return &fakeCards{
		permissions: map[string]ServedPermission{},
		questions:   map[string]ServedQuestion{},
	}
}

func (c *fakeCards) Permission(_ ids.WorkspaceID, id *conversationv1.AgentPermissionId) (ServedPermission, bool) {
	served, ok := c.permissions[id.GetValue()]
	return served, ok
}

func (c *fakeCards) Question(_ ids.WorkspaceID, id *conversationv1.AgentQuestionId) (ServedQuestion, bool) {
	served, ok := c.questions[id.GetValue()]
	return served, ok
}

func (c *fakeCards) ColdGate(ids.WorkspaceID) (ServedColdGate, bool) {
	if c.coldGate == nil {
		return ServedColdGate{}, false
	}
	return *c.coldGate, true
}

func (c *fakeCards) ClearColdGate(ids.WorkspaceID) {
	c.coldGate = nil
	c.coldGatesCleared++
}

func (c *fakeCards) PermissionModes(ids.WorkspaceID) ([]string, bool) {
	return c.modes, c.hasModes
}

func (c *fakeCards) Models(ids.WorkspaceID) ([]string, bool) {
	return c.models, c.hasModels
}

// fakeSurfaces is a dlog.Surfaces backed by one capturing logger.
type fakeSurfaces struct {
	logger       *dlog.TestLogger
	workspaceErr error
	shimSinkErr  error
	evictErr     error
	evicted      []string
}

func newFakeSurfaces() *fakeSurfaces { return &fakeSurfaces{logger: dlog.NewTestLogger()} }

func (s *fakeSurfaces) Global() dlog.Logger { return s.logger }

func (s *fakeSurfaces) Workspace(dir string) (dlog.Logger, error) {
	if s.workspaceErr != nil {
		return nil, s.workspaceErr
	}
	return s.logger.With(dlog.Context{"dir": dir}), nil
}

// WorkspaceOrCentral implements dlog.Surfaces: the workspace's logger when it
// resolves, and the global one when it does not.
func (s *fakeSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	log, err := s.Workspace(dir)
	if err != nil {
		return s.Global().With(dlog.Context{dlog.KeyUnroutableWorkspace: dir})
	}
	return log
}

func (s *fakeSurfaces) ShimSink(string) (dlog.Borrowed, error) {
	if s.shimSinkErr != nil {
		return nil, s.shimSinkErr
	}
	return &fakeBorrowed{}, nil
}

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (s *fakeSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (s *fakeSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

// fakeBorrowed is a non-closeable sink handle over the null device, which is
// what a spawn is handed as fd 3.
type fakeBorrowed struct {
	file *os.File
}

func (b *fakeBorrowed) File() *os.File {
	if b.file == nil {
		file, err := os.OpenFile(os.DevNull, os.O_WRONLY, 0)
		if err != nil {
			panic(err)
		}
		b.file = file
	}
	return b.file
}

func (b *fakeBorrowed) Close() error { return nil }

func (s *fakeSurfaces) ClientLog(string, dlog.ClientRecord) error { return errFake }

func (s *fakeSurfaces) Close() error { return nil }

// fakeHealth is a health.Reporter that records the faults it was asked to open
// and answers the standing ones from that same list, so a test asserts both
// that a fault was raised and that a second identical one was not.
type fakeHealth struct {
	health.Reporter

	opened []wsm.Fault
	// closed records the faults CloseFault was called with, and a closed
	// fault leaves the standing list.
	closed []ids.FaultID
	// closeErr fails every CloseFault.
	closeErr error
	// openErr fails every OpenFault.
	openErr error
	// listErr fails every OpenFaults.
	listErr error
}

func (h *fakeHealth) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	if h.openErr != nil {
		return "", h.openErr
	}
	id := ids.FaultID(fmt.Sprintf("fault-%d", len(h.opened)+1))
	f.ID = id
	h.opened = append(h.opened, f)
	return id, nil
}

func (h *fakeHealth) CloseFault(_ context.Context, id ids.FaultID) error {
	if h.closeErr != nil {
		return h.closeErr
	}
	h.closed = append(h.closed, id)
	kept := h.opened[:0]
	for _, f := range h.opened {
		if f.ID != id {
			kept = append(kept, f)
		}
	}
	h.opened = kept
	return nil
}

func (h *fakeHealth) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	if h.listErr != nil {
		return nil, h.listErr
	}
	var out []wsm.Fault
	for _, f := range h.opened {
		if scope.Kind != "" && f.Kind != scope.Kind {
			continue
		}
		out = append(out, f)
	}
	return out, nil
}

// fixture is one arranged verb surface plus every fake behind it, so a test
// arranges by mutating fields and asserts by reading them.
type fixture struct {
	verbs   Verbs
	db      *fakeDB
	git     *fakeGit
	account *fakeAccounts
	queue   *fakeQueue
	merge   *fakeMerge
	rollout *fakeRollout
	feed    *fakeFeed
	footer  *fakeFooter
	sidebar *fakeSidebar
	health  *fakeHealth
	host    *fakeHost
	browser *fakeBrowser
	fleet   *fakeSessions
	shim    *fakeShim
	owner   *fakeOwnership
	cards   *fakeCards
	log     *fakeSurfaces

	// running is what the freeness probe answers.
	running Running
	// hasSession gates both the freeness probe and the shim resolver.
	hasSession bool
	// briefs is what the injected prompt loader answers, by brief name.
	briefs map[string]prompts.Prompt
	// briefErr fails every brief load.
	briefErr error
}

// newFixture arranges a verb surface with a live session, an owned workspace
// and no briefs, which is the arrangement most tests start from.
func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{
		db:      newFakeDB(),
		git:     &fakeGit{defaultBranch: "master", currentBranch: "feature", commonDir: "/repo", mainWorktree: "/repo"},
		account: &fakeAccounts{configDir: "/config"},
		queue:   newFakeQueue(),
		merge:   newFakeMerge(),
		rollout: &fakeRollout{done: make(chan struct{}, 8)},
		feed:    &fakeFeed{},
		footer:  newFakeFooter(),
		sidebar: &fakeSidebar{},
		health:  &fakeHealth{},
		host:    &fakeHost{},
		browser: &fakeBrowser{},
		fleet:   newFakeSessions(),
		shim:    &fakeShim{},
		owner:   &fakeOwnership{standing: StandingOwned},
		cards:   newFakeCards(),
		log:     newFakeSurfaces(),
		briefs:  map[string]prompts.Prompt{},
	}
	f.hasSession = true

	verbs, err := New(Deps{
		DB: f.db, Git: f.git, Accounts: f.account, Queue: f.queue, Merge: f.merge,
		Rollout: f.rollout, Feed: f.feed, Footer: f.footer, Topbar: stubTopbar{}, Browser: f.browser,
		Sidebar: f.sidebar, Holds: stubHolds{}, Host: f.host, Sessions: f.fleet,
		Health:     f.health,
		PromptsDir: "/prompts", CheckoutRoot: fixtureCheckoutRoot, Log: f.log,
		// The policy probe answers from the SAME brief table the loader
		// answers from, so a fixture that registers a brief has it in the
		// repository's policy and one that does not has neither.
		Policy: policyOf(f),
		Shim: func(ids.WorkspaceID) (Shim, bool) {
			if !f.hasSession {
				return nil, false
			}
			return f.shim, true
		},
		Freeness: func(ids.WorkspaceID) (Running, bool) {
			return f.running, f.hasSession
		},
		Ownership: f.owner,
		Cards:     f.cards,
		LoadPrompt: func(_ string, name string) (prompts.Prompt, error) {
			if f.briefErr != nil {
				return prompts.Prompt{}, f.briefErr
			}
			brief, ok := f.briefs[name]
			if !ok {
				return prompts.Prompt{}, errors.New("no such brief: " + name)
			}
			return brief, nil
		},
		// The splicer is the real substitution rule, spelled here because
		// prompts.Prompt.Splice is another package's unlanded leaf: every
		// declared placeholder must be supplied and no unknown one may be.
		SplicePrompt: func(prompt prompts.Prompt, values map[string]string) (string, error) {
			for _, name := range prompt.Placeholders {
				if _, ok := values[name]; !ok {
					return "", errors.New("no value for placeholder " + name)
				}
			}
			for name := range values {
				if !containsString(prompt.Placeholders, name) {
					return "", errors.New("unknown placeholder " + name)
				}
			}
			out := prompt.Body
			for name, value := range values {
				out = strings.ReplaceAll(out, "{{"+name+"}}", value)
			}
			return out, nil
		},
		Now: func() time.Time { return fixedNow },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.verbs = verbs
	return f
}

// fixtureCheckoutRoot is the module checkout the fixture's daemon was deployed
// from. It is deliberately OUTSIDE every fixture repository, so a fixture
// repository's one-shot policy is its own `.agent-repl/prompts` — which is
// what every repository but one is. A test that needs the CORPUS source, i.e.
// the one repository the daemon's checkout lives in, calls
// `fixture.ownRepository`.
const fixtureCheckoutRoot = "/checkout/modules/app/agent-repl"

// fixturePolicy is the fixture's policy probe: a directory holds exactly the
// briefs the fixture's loader was given.
type fixturePolicy struct{ f *fixture }

func policyOf(f *fixture) prompts.Files { return fixturePolicy{f: f} }

func (p fixturePolicy) Missing(_ string, names []string) []string {
	var missing []string
	for _, name := range names {
		if _, ok := p.f.briefs[name]; !ok {
			missing = append(missing, name+prompts.Suffix)
		}
	}
	return missing
}

func (p fixturePolicy) Text(_ string, name string) (string, error) {
	brief, ok := p.f.briefs[name]
	if !ok {
		return "", errors.New("no such brief: " + name)
	}
	return brief.Body, nil
}

// ownRepository makes dir the repository the daemon's own checkout lives in,
// so its one-shot policy is the daemon's corpus rather than its own tree.
func (f *fixture) ownRepository(t *testing.T, dir string) {
	t.Helper()
	f.mutable(t).deps.CheckoutRoot = filepath.Join(dir, "modules", "app", "agent-repl")
}

// mutable exposes the concrete verbs so a test can rearrange a collaborator
// New already accepted — the browser's absence, for one, which is a legal
// headless wiring rather than a construction error.
func (f *fixture) mutable(t *testing.T) *verbs {
	t.Helper()
	v, ok := f.verbs.(*verbs)
	if !ok {
		t.Fatalf("verbs = %T, want *verbs", f.verbs)
	}
	return v
}

// workspace registers one workspace in the fixture's fake registry. The
// directory is NORMALIZED first, exactly as the real state client records it,
// so a ref echoing any spelling of the same tree still matches.
func (f *fixture) workspace(id ids.WorkspaceID, dir string) wsm.Workspace {
	normalized, err := normalizeDir(dir)
	if err != nil {
		panic(err)
	}
	dir = normalized
	ws := wsm.Workspace{ID: id, Dir: dir, Repo: "repo-1", Name: "sample", Branch: "DWC/sample"}
	f.db.with(ws)
	// A REAL DIRECTORY, because the roster no longer publishes a repository
	// whose main worktree is gone (`withoutGoneRepositories'). The workspace's
	// own parent is one the caller's `t.TempDir()' already made.
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: filepath.Dir(dir)})
	return ws
}

// stubTopbar and stubHolds satisfy the resolver seams the verbs hold but never
// call, so a test that does call one nil-panics rather than passing quietly.
type stubTopbar struct{ topbar.Resolver }
type stubHolds struct{ holds.Resolver }

// The three resolvers registration BINDS record the directory it bound, which
// is all any verb test needs of them.
func (f *fakeFooter) SetWorkspaceDir(ws ids.WorkspaceID, dir string) error {
	if f.dirs == nil {
		f.dirs = map[ids.WorkspaceID]string{}
	}
	f.dirs[ws] = dir
	return nil
}

func (stubTopbar) SetWorkspaceDir(ids.WorkspaceID, string) error { return nil }
func (stubTopbar) SetNaming(ids.WorkspaceID, topbar.Naming)      {}
func (stubTopbar) SetAccount(ids.WorkspaceID, string)            {}
func (stubHolds) SetWorkspaceDir(ids.WorkspaceID, string) error  { return nil }

// asRefusal fails the test unless err is a refusal naming arm.
func asRefusal(t *testing.T, err error, arm string) *Refusal {
	t.Helper()
	refusal, ok := AsRefusal(err)
	if !ok {
		t.Fatalf("error = %v, want a refusal naming %s", err, arm)
	}
	if refusal.Arm != arm {
		t.Fatalf("refusal arm = %q, want %q", refusal.Arm, arm)
	}
	return refusal
}

// containsString reports membership, which the fake splicer needs to refuse an
// unknown placeholder.
func containsString(set []string, want string) bool {
	for _, s := range set {
		if s == want {
			return true
		}
	}
	return false
}

// Evict records the sinks a close released, and fails when the test arranged
// an eviction failure.
func (s *fakeSurfaces) Evict(dir string) error {
	if s.evictErr != nil {
		return s.evictErr
	}
	s.evicted = append(s.evicted, dir)
	return nil
}
