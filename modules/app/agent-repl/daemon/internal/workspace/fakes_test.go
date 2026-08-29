package workspace

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
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
	jobs         map[ids.WorkspaceID]wsm.CreationJob
	held         map[ids.WorkspaceID][]wsm.HeldPrompt

	registerErr error
	registered  []wsm.RegisterFacts
	registerDir string
	createdNew  bool

	putJobs      []wsm.CreationJob
	putJobErr    error
	putSessions  []wsm.Session
	putTurns     []wsm.Turn
	closedFlags  map[ids.WorkspaceID]bool
	priorities   map[ids.WorkspaceID]*wsm.Priority
	attention    map[ids.WorkspaceID]bool
	currentAt    time.Time
	forgotten    []ids.WorkspaceID
	terminals    map[ids.WorkspaceID]wsm.SessionTerminal
	orphanReport wsm.OrphanReport
	createdTasks []string
	taskChanges  map[ids.TaskID]wsm.TaskChange
	assignments  map[ids.WorkspaceID]*ids.TaskID
	taskErr      error
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

func (d *fakeDB) Forget(_ context.Context, id ids.WorkspaceID) error {
	d.forgotten = append(d.forgotten, id)
	return nil
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
	commonDirErr  error
	currentBranch string
	branchErr     error
	defaultBranch string
	defaultErr    error

	created   []createdWorktree
	createErr error
	nuked     []nukedWorktree
	nukeErr   error
}

type createdWorktree struct{ RepoDir, Branch, BaseRef, WorktreeDir string }
type nukedWorktree struct{ RepoDir, WorktreeDir, Branch string }

func (g *fakeGit) CommonDir(context.Context, string) (string, error) {
	return g.commonDir, g.commonDirErr
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
	transcript    string
	transcriptErr error
	ported        []portedTranscript
	portErr       error
}

type portedTranscript struct{ Path, ConfigDir, WorkspaceDir string }

func (a *fakeAccounts) ConfigDirFor(string) string { return a.configDir }

func (a *fakeAccounts) FindTranscript(context.Context, string, string) (string, error) {
	return a.transcript, a.transcriptErr
}

func (a *fakeAccounts) PortTranscript(_ context.Context, path, configDir, workspaceDir string) error {
	if a.portErr != nil {
		return a.portErr
	}
	a.ported = append(a.ported, portedTranscript{path, configDir, workspaceDir})
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
	enqueued    []ids.WorkspaceID
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

func (m *fakeMerge) Enqueue(_ context.Context, ws ids.WorkspaceID) error {
	m.enqueued = append(m.enqueued, ws)
	return nil
}

// fakeRollout is a rollout.Controller.
type fakeRollout struct {
	rollout.Controller

	relaunches  []rolloutCall
	relaunchErr error
	reloads     []ids.WorkspaceID
	reloadErr   error
}

type rolloutCall struct {
	WS     ids.WorkspaceID
	Reason rollout.RelaunchReason
}

func (r *fakeRollout) RelaunchShim(_ context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason) error {
	r.relaunches = append(r.relaunches, rolloutCall{ws, reason})
	return r.relaunchErr
}

func (r *fakeRollout) ReloadWebapp(_ context.Context, ws ids.WorkspaceID) error {
	r.reloads = append(r.reloads, ws)
	return r.reloadErr
}

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
}

func newFakeFooter() *fakeFooter {
	return &fakeFooter{
		closing:      map[ids.WorkspaceID]*footer.CloseBlocked{},
		coldGates:    map[ids.WorkspaceID]footer.ColdGate{},
		interrupting: map[ids.WorkspaceID]bool{},
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

// fakeSidebar is a sidebar.Resolver.
type fakeSidebar struct {
	sidebar.Resolver

	registries []sidebar.Registry
	selected   []ids.WorkspaceID
}

func (s *fakeSidebar) SetRegistry(reg sidebar.Registry) { s.registries = append(s.registries, reg) }

func (s *fakeSidebar) SetSelected(ws ids.WorkspaceID) { s.selected = append(s.selected, ws) }

// fakeHost is a HostRelay.
type fakeHost struct {
	editorOpens []editorOpen
	reloads     []ids.WorkspaceID
	notes       []hostNote
}

type editorOpen struct {
	WS   ids.WorkspaceID
	Path string
	Line *uint32
}

type hostNote struct {
	WS               ids.WorkspaceID
	Text, Kind, Tool string
}

func (h *fakeHost) OpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	h.editorOpens = append(h.editorOpens, editorOpen{ws, path, line})
}

func (h *fakeHost) ReloadWebapp(ws ids.WorkspaceID) { h.reloads = append(h.reloads, ws) }

func (h *fakeHost) Notify(ws ids.WorkspaceID, text, kind, tool string) {
	h.notes = append(h.notes, hostNote{ws, text, kind, tool})
}

// fakeSessions is a Sessions fleet.
type fakeSessions struct {
	live     map[ids.WorkspaceID]bool
	started  []ids.WorkspaceID
	startErr error
	stopped  []stopCall
	stopErr  error
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
	resumes        []ColdResume
	resumeErr      error
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

func (s *fakeShim) StartSession(_ context.Context, resume ColdResume) error {
	if s.resumeErr != nil {
		return s.resumeErr
	}
	s.resumes = append(s.resumes, resume)
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
	modes       []string
	hasModes    bool
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

func (c *fakeCards) PermissionModes(ids.WorkspaceID) ([]string, bool) {
	return c.modes, c.hasModes
}

// fakeSurfaces is a dlog.Surfaces backed by one capturing logger.
type fakeSurfaces struct {
	logger       *dlog.TestLogger
	workspaceErr error
}

func newFakeSurfaces() *fakeSurfaces { return &fakeSurfaces{logger: dlog.NewTestLogger()} }

func (s *fakeSurfaces) Global() dlog.Logger { return s.logger }

func (s *fakeSurfaces) Workspace(dir string) (dlog.Logger, error) {
	if s.workspaceErr != nil {
		return nil, s.workspaceErr
	}
	return s.logger.With(dlog.Context{"dir": dir}), nil
}

func (s *fakeSurfaces) ShimSink(string) (dlog.Borrowed, error) { return nil, errFake }

func (s *fakeSurfaces) ClientLog(string, dlog.ClientRecord) error { return errFake }

func (s *fakeSurfaces) Close() error { return nil }

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
	// evicted records the log sinks the close verb evicted.
	evicted []string
}

// newFixture arranges a verb surface with a live session, an owned workspace
// and no briefs, which is the arrangement most tests start from.
func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{
		db:      newFakeDB(),
		git:     &fakeGit{defaultBranch: "master", currentBranch: "feature", commonDir: "/repo"},
		account: &fakeAccounts{configDir: "/config"},
		queue:   newFakeQueue(),
		merge:   newFakeMerge(),
		rollout: &fakeRollout{},
		feed:    &fakeFeed{},
		footer:  newFakeFooter(),
		sidebar: &fakeSidebar{},
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
		PromptsDir: "/prompts", Log: f.log,
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
		Now:          func() time.Time { return fixedNow },
		EvictLogSink: func(dir string) error { f.evicted = append(f.evicted, dir); return nil },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.verbs = verbs
	return f
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
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: "/repo"})
	return ws
}

// stubTopbar and stubHolds satisfy the resolver seams the verbs hold but never
// call, so a test that does call one nil-panics rather than passing quietly.
type stubTopbar struct{ topbar.Resolver }
type stubHolds struct{ holds.Resolver }

// initGitRepo creates a real git repository, which the naming rule's worktree
// derivation stats. It skips the test when git is unavailable rather than
// asserting on the state of the machine.
func initGitRepo(t *testing.T) string {
	t.Helper()
	git, err := exec.LookPath("git")
	if err != nil {
		t.Skip("git is not on PATH")
	}
	dir := t.TempDir()
	for _, args := range [][]string{
		{"init", "-q"},
		{"config", "user.email", "test@example.invalid"},
		{"config", "user.name", "test"},
		{"commit", "-q", "--allow-empty", "-m", "root"},
	} {
		cmd := exec.Command(git, args...)
		cmd.Dir = dir
		if out, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("git %v: %v: %s", args, err, out)
		}
	}
	return dir
}

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
