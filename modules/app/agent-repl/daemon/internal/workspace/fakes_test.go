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
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/headless"
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

	workspaces map[ids.WorkspaceID]wsm.Workspace
	byDir      map[string]wsm.Workspace
	// listWorkspacesErr fails the roster read, for the tests about what a
	// republish does with a state client that would not answer.
	listWorkspacesErr error
	repositories      []wsm.Repository
	// listRepositoriesErr fails the registry read, which the create path must
	// surface rather than read as "the repository is not registered".
	listRepositoriesErr error
	tasks               []wsm.Task
	current             *ids.WorkspaceID
	sessions            map[ids.WorkspaceID]wsm.Session
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

	// registeredRepos records every RegisterRepository the verb made, in
	// order, and registerRepoErr fails the write, which the fake's own slice
	// cannot.
	registeredRepos []registeredRepo
	registerRepoErr error

	putJobs     []wsm.CreationJob
	putJobErr   error
	putSessions []wsm.Session
	putTurns    []wsm.Turn
	// putTurnErr fails every turn write, which the fake's own slice cannot.
	putTurnErr error
	// openTurns is what OpenTurns answers, and openTurnsErr fails the read.
	openTurns    []wsm.Turn
	openTurnsErr error
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
	// clearTerminalErr fails the terminal RETIREMENT, which the fake's own map
	// cannot, so the live-shim invariant's failure arm is reachable.
	clearTerminalErr error
	// setTerminalErr fails the terminal write, which the fake's own map cannot.
	// A workspace with no session row surfaces wsm.ErrNotFound here, and any
	// other error stands for a real terminal-recording failure.
	setTerminalErr error
	orphanReport   wsm.OrphanReport
	createdTasks   []string
	taskChanges    map[ids.TaskID]wsm.TaskChange
	assignments    map[ids.WorkspaceID]*ids.TaskID
	taskErr        error

	// dbFaults is the fault table the fleet opens and closes lost-link rows
	// in; dbClosed records the ids CloseFault was called with.
	dbFaults []wsm.Fault
	dbClosed []ids.FaultID

	// spawnedPIDs records every SetSpawnedShimPID in order, nil for a clear,
	// so a test can pin that the fork's pid was made durable and that a failed
	// start retracted it. The workspace row is updated with it, which is what
	// the starting-survivor probe reads back.
	spawnedPIDs []*int
	// spawnedPIDErr fails the write, which the fake's own map cannot.
	spawnedPIDErr error

	// claims records every ClaimServing in order, and claimErr fails the
	// write, which the fake's own slice cannot.
	claims   []servingClaim
	claimErr error
}

// servingClaim is one recorded ClaimServing.
type servingClaim struct {
	ws       ids.WorkspaceID
	instance ids.InstanceID
}

func (d *fakeDB) ClaimServing(_ context.Context, id ids.WorkspaceID, instance ids.InstanceID) error {
	if d.claimErr != nil {
		return d.claimErr
	}
	d.claims = append(d.claims, servingClaim{ws: id, instance: instance})
	return nil
}

func (d *fakeDB) SetSpawnedShimPID(_ context.Context, id ids.WorkspaceID, pid *int) error {
	if d.spawnedPIDErr != nil {
		return d.spawnedPIDErr
	}
	d.spawnedPIDs = append(d.spawnedPIDs, pid)
	ws, ok := d.workspaces[id]
	if !ok {
		return errors.New("no such workspace")
	}
	ws.SpawnedShimPID = pid
	d.workspaces[id] = ws
	return nil
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

// registeredRepo is one RegisterRepository call, as the fake recorded it.
type registeredRepo struct{ Dir, DefaultBranch string }

func (d *fakeDB) RegisterRepository(_ context.Context, dir, defaultBranch string) (wsm.Repository, bool, error) {
	if d.registerRepoErr != nil {
		return wsm.Repository{}, false, d.registerRepoErr
	}
	d.registeredRepos = append(d.registeredRepos, registeredRepo{Dir: dir, DefaultBranch: defaultBranch})
	for _, repo := range d.repositories {
		if repo.Dir == dir {
			return repo, false, nil
		}
	}
	repo := wsm.Repository{
		ID: ids.RepoID("repo-" + filepath.Base(dir)), Dir: dir,
		Name: filepath.Base(dir), DefaultBranch: defaultBranch,
	}
	d.repositories = append(d.repositories, repo)
	return repo, true, nil
}

func (d *fakeDB) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	if d.listWorkspacesErr != nil {
		return nil, d.listWorkspacesErr
	}
	out := make([]wsm.Workspace, 0, len(d.workspaces))
	for _, ws := range d.workspaces {
		out = append(out, ws)
	}
	return out, nil
}

func (d *fakeDB) ListRepositories(context.Context) ([]wsm.Repository, error) {
	if d.listRepositoriesErr != nil {
		return nil, d.listRepositoriesErr
	}
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
	if d.setTerminalErr != nil {
		return d.setTerminalErr
	}
	d.terminals[id] = t
	// THE TERMINAL LANDS ON THE SESSION ROW, as the store's does: every
	// surface that recedes a killed row reads it back off the record, so a
	// fake that kept the cause somewhere else could not tell the retirement
	// apart from the kill never happening.
	if session, ok := d.sessions[id]; ok {
		session.Terminal = &t
		d.sessions[id] = session
	}
	return nil
}

// ClearSessionTerminal retires the fake's terminal record, refusing a deleted
// session exactly as the store does.
func (d *fakeDB) ClearSessionTerminal(_ context.Context, id ids.WorkspaceID) error {
	if d.clearTerminalErr != nil {
		return d.clearTerminalErr
	}
	if t, ok := d.terminals[id]; ok && t.Kind == "deleted" {
		return fmt.Errorf("fake: workspace %s: %w", id, wsm.ErrSessionDeleted)
	}
	delete(d.terminals, id)
	if session, ok := d.sessions[id]; ok {
		session.Terminal = nil
		d.sessions[id] = session
	}
	return nil
}

func (d *fakeDB) PutTurn(_ context.Context, t wsm.Turn) error {
	if d.putTurnErr != nil {
		return d.putTurnErr
	}
	d.putTurns = append(d.putTurns, t)
	return nil
}

func (d *fakeDB) OpenTurns(_ context.Context, id ids.WorkspaceID) ([]wsm.Turn, error) {
	if d.openTurnsErr != nil {
		return nil, d.openTurnsErr
	}
	var out []wsm.Turn
	for _, t := range d.openTurns {
		if t.Workspace == id {
			out = append(out, t)
		}
	}
	return out, nil
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

// RecentConversationPrompts is the bounded fixture form of
// ConversationPrompts: the same scripted rows, truncated to the most recent
// `limit`, ordinals renumbered from zero, so a naming test can assert the
// call reads a tail rather than the whole scripted conversation.
func (d *fakeDB) RecentConversationPrompts(_ context.Context, id ids.WorkspaceID, limit int) ([]wsm.PortedPrompt, error) {
	if d.conversationErr != nil {
		return nil, d.conversationErr
	}
	if limit <= 0 {
		return nil, nil
	}
	all := d.conversations[id]
	tail := all
	if len(tail) > limit {
		tail = tail[len(tail)-limit:]
	}
	out := make([]wsm.PortedPrompt, len(tail))
	for i, row := range tail {
		row.Ordinal = int64(i)
		out[i] = row
	}
	return out, nil
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

	commonDir    string
	mainWorktree string
	commonDirErr error
	// outsideEveryRepository makes the RepositoryOf probe answer "not in a
	// repository", which is an ordinary answer and never an error.
	outsideEveryRepository bool
	// repositoryOfErr fails the probe itself, which is a different fact from
	// a path that is simply outside every repository.
	repositoryOfErr error
	currentBranch   string
	branchErr       error
	defaultBranch   string
	defaultErr      error

	resolveErr error
	// existingBranches are the branches this repository already holds, which
	// is what the naming call's collision probe asks for.
	existingBranches map[string]bool
	branchExistsErr  error

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

func (g *fakeGit) BranchExists(_ context.Context, _, branch string) (bool, error) {
	return g.existingBranches[branch], g.branchExistsErr
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

// RepositoryOf is the PROBE half: `outsideEveryRepository` is the ordinary
// "not in a repository" answer, which carries no error at all, and
// `repositoryOfErr` is the separate case of git failing to be asked.
func (g *fakeGit) RepositoryOf(context.Context, string) (string, bool, error) {
	if g.repositoryOfErr != nil {
		return "", false, g.repositoryOfErr
	}
	if g.outsideEveryRepository {
		return "", false, nil
	}
	return g.mainWorktree, true, nil
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

	configDir string
	// multiRepoDir is the work (multi-repo) account root the fake reports
	// IsMultiRepo true for; empty means the fixture has no work account, so
	// every config dir is personal (IsMultiRepo false).
	multiRepoDir  string
	transcript    account.Transcript
	transcriptErr error
	// newest is the transcript NewestTranscript answers with for the no-record
	// adoption probe; newestErr is its failure. A zero-valued fake (both unset)
	// answers ErrNoTranscripts, so a fixture that does not opt into adoption
	// comes up fresh exactly as a workspace with an empty project dir would.
	newest    account.AdoptableTranscript
	newestErr error
	// newestProbedDir records the workspace dir NewestTranscript was probed for,
	// so a test can assert the recorded-session path never probes at all.
	newestProbedDir string
	newestProbed    bool
	ported          []portedTranscript
	portErr         error
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
	// roster is every root the daemon knows, which is what the topbar's
	// account cell offers. nil takes the routed root alone, so a fixture that
	// does not care still gets a one-root machine.
	roster    []account.Account
	rosterErr error
}

type movedTranscript struct{ Path, ToConfigDir, WorkspaceDir string }

type portedTranscript struct{ Path, ConfigDir, WorkspaceDir, VendorSessionID string }

func (a *fakeAccounts) ConfigDirFor(string) string { return a.configDir }

// IsMultiRepo reports whether the given config dir is the fixture's work
// (multi-repo) account root. An empty multiRepoDir means no work account, so
// every dir is personal.
func (a *fakeAccounts) IsMultiRepo(configDir string) bool {
	return a.multiRepoDir != "" && configDir == a.multiRepoDir
}

// Read answers the account the fixture holds; an unset email is the logged-out
// arm, which is an answer and not a failure.
func (a *fakeAccounts) Read(_ context.Context, configDir string) (account.Account, error) {
	if a.readErr != nil {
		return account.Account{}, a.readErr
	}
	return account.Account{ConfigDir: configDir, Email: a.email, LoggedIn: a.email != ""}, nil
}

// Roster answers every root the fixture holds, defaulting to the routed one
// alone.
func (a *fakeAccounts) Roster(ctx context.Context) ([]account.Account, error) {
	if a.rosterErr != nil {
		return nil, a.rosterErr
	}
	if a.roster != nil {
		return a.roster, nil
	}
	routed, err := a.Read(ctx, a.configDir)
	if err != nil {
		return nil, err
	}
	return []account.Account{routed}, nil
}

func (a *fakeAccounts) FindTranscript(context.Context, string, string) (account.Transcript, error) {
	return a.transcript, a.transcriptErr
}

// NewestTranscript answers the fixture's adoption candidate, recording that it
// was probed and for which dir. An unset fixture answers ErrNoTranscripts, the
// nothing-to-adopt arm, so an unprepared test comes up fresh.
func (a *fakeAccounts) NewestTranscript(_ context.Context, workspaceDir string) (account.AdoptableTranscript, error) {
	a.newestProbed = true
	a.newestProbedDir = workspaceDir
	if a.newestErr != nil {
		return account.AdoptableTranscript{}, a.newestErr
	}
	if a.newest.VendorSessionID == "" {
		return account.AdoptableTranscript{}, account.ErrNoTranscripts
	}
	return a.newest, nil
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
	// db is the fixture's store, which the door closes orphans on.
	db *fakeDB
}

// CloseOrphans is the queue's door, closing on the fixture's own store.
func (q *fakeQueue) CloseOrphans(ctx context.Context, ws ids.WorkspaceID, at time.Time) (wsm.OrphanReport, error) {
	return q.db.CloseOrphans(ctx, ws, at)
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

func (m *fakeMerge) Enqueue(_ context.Context, ws ids.WorkspaceID, _ merge.Requester) error {
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
	// refuseErr is what BounceShim answers synchronously, as the registry
	// refuses a request it will not take; nil takes every request.
	refuseErr error
	checkErr  error
	reloads   []ids.WorkspaceID
	reloadErr error
	// done fires once per finished relaunch. The restart verb ACCEPTS and
	// runs the engine behind it, so a test synchronizes on this rather than
	// on the verb's return.
	done chan struct{}
}

type rolloutCall struct {
	WS     ids.WorkspaceID
	Reason rollout.RelaunchReason
	Force  bool
}

// BounceShim records the bounce and completes it on a goroutine of its own,
// as the registry does: with relaunchErr as the bounce's outcome.
func (r *fakeRollout) BounceShim(ctx context.Context, ws ids.WorkspaceID, reason rollout.RelaunchReason, force bool, done func(error)) (bounce.Decision, error) {
	r.mu.Lock()
	r.relaunches = append(r.relaunches, rolloutCall{WS: ws, Reason: reason, Force: force})
	err, refused := r.relaunchErr, r.refuseErr
	r.mu.Unlock()
	if refused != nil {
		return bounce.Decision{}, refused
	}
	go func() {
		if done != nil {
			done(err)
		}
		if err != nil {
			r.signal()
		}
	}()
	return bounce.Decision{Now: true, Forced: force}, nil
}

// CheckStaleness records the mount's staleness check as a build-stale call,
// failing with checkErr.
func (r *fakeRollout) CheckStaleness(_ context.Context, ws ids.WorkspaceID, force bool) (rollout.StaleCheck, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.relaunches = append(r.relaunches, rolloutCall{WS: ws, Reason: rollout.ReasonBuildStale, Force: force})
	return rollout.StaleCheck{}, r.checkErr
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
	// retired is every row retired, in order.
	retired []*frontendv1.FeedId
	// resets is every workspace whose feed was emptied, in order.
	resets []ids.WorkspaceID
	// onReset runs AT the reset, which is how a test reads the swap's own
	// progress as it stood when the feed was emptied — the ordering assertion
	// with nothing to wait on.
	onReset func()
}

// RetireRow records the retirement.
func (f *fakeFeed) RetireRow(_ ids.WorkspaceID, _ feedid.Feed, id *frontendv1.FeedId) {
	f.retired = append(f.retired, id)
}

// ResetWorkspace records the emptying and lets a test observe the moment.
func (f *fakeFeed) ResetWorkspace(ws ids.WorkspaceID, _ string) {
	f.resets = append(f.resets, ws)
	if f.onReset != nil {
		f.onReset()
	}
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
	// parked records every park state the verbs installed or lifted, in order.
	parked []bool
	// primed records every workspace registration primed the footer for.
	primed []ids.WorkspaceID
	// coldAnswers is every cold-gate answer state the footer was handed, in
	// order, the clearing nil included: the ORDER is the assertion, because
	// the act has to reach the strip before the shim is dialed.
	coldAnswers []*footer.ColdGateAnswer
	// coldEvents is every cold-gate setter call, in order: "gate:standing",
	// "gate:retired", "answer" or "answer:cleared".
	coldEvents []string
}

func (f *fakeFooter) SetParked(_ ids.WorkspaceID, parked bool) {
	f.parked = append(f.parked, parked)
}

// Prime records the workspaces registration primed the footer for, in order,
// which is what the register wiring test asserts.
func (f *fakeFooter) Prime(ws ids.WorkspaceID) {
	f.primed = append(f.primed, ws)
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
	if gate.Standing {
		f.coldEvents = append(f.coldEvents, "gate:standing")
		return
	}
	f.coldEvents = append(f.coldEvents, "gate:retired")
}

func (f *fakeFooter) SetColdGateAnswer(_ ids.WorkspaceID, answer *footer.ColdGateAnswer) {
	f.coldAnswers = append(f.coldAnswers, answer)
	if answer == nil {
		f.coldEvents = append(f.coldEvents, "answer:cleared")
		return
	}
	f.coldEvents = append(f.coldEvents, "answer")
}

// coldAnswerLines is every non-clearing line the footer was handed, in order.
func (f *fakeFooter) coldAnswerLines() []string {
	out := []string{}
	for _, answer := range f.coldAnswers {
		if answer == nil {
			continue
		}
		out = append(out, answer.Text)
	}
	return out
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

	// mu guards every field: concurrent selects tell the roster things from
	// several goroutines at once.
	mu         sync.Mutex
	registries []sidebar.Registry
	selected   []ids.WorkspaceID
	viewed     []ids.WorkspaceID
	// reviving records every REVIVING edge, in order.
	reviving []revivingEdge
	// calls records the ORDER the resolver was told things in, which is what
	// decides whether a push carries a whole view or a half-refreshed one.
	calls []string
}

// revivingEdge is one SetReviving call.
type revivingEdge struct {
	WS       ids.WorkspaceID
	Reviving bool
}

func (s *fakeSidebar) SetRegistry(reg sidebar.Registry) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.registries = append(s.registries, reg)
	s.calls = append(s.calls, "registry")
}

func (s *fakeSidebar) SetSelected(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.selected = append(s.selected, ws)
	s.calls = append(s.calls, "selected")
}

func (s *fakeSidebar) SetViewed(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.viewed = append(s.viewed, ws)
	s.calls = append(s.calls, "viewed")
}

func (s *fakeSidebar) SetReviving(ws ids.WorkspaceID, reviving bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.reviving = append(s.reviving, revivingEdge{ws, reviving})
	if reviving {
		s.calls = append(s.calls, "reviving")
	} else {
		s.calls = append(s.calls, "revived")
	}
}

// snapshot copies the recorded calls and selections under the lock, for a
// test reading them while another goroutine may still be writing.
// snapshotRegistries is every registry the roster was handed, in order, read
// under the fake's own lock.
func (s *fakeSidebar) snapshotRegistries() []sidebar.Registry {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]sidebar.Registry(nil), s.registries...)
}

func (s *fakeSidebar) snapshot() (calls []string, selected []ids.WorkspaceID, reviving []revivingEdge) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.calls...),
		append([]ids.WorkspaceID(nil), s.selected...),
		append([]revivingEdge(nil), s.reviving...)
}

// fakeHost is a HostRelay.
type fakeHost struct {
	editorOpens []editorOpen
	reloads     []ids.WorkspaceID
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

func (h *fakeHost) OpenInEditor(ws ids.WorkspaceID, path string, line *uint32) {
	h.editorOpens = append(h.editorOpens, editorOpen{ws, path, line})
}

func (h *fakeHost) ReloadWebapp(ws ids.WorkspaceID) { h.reloads = append(h.reloads, ws) }

// fakeBanners is a Banners notifier: it records every raised banner.
type fakeBanners struct {
	raised []raisedBanner
}

type raisedBanner struct {
	WS         ids.WorkspaceID
	Kind, Text string
}

func (b *fakeBanners) Raise(ws ids.WorkspaceID, kind, text string) {
	b.raised = append(b.raised, raisedBanner{ws, kind, text})
}

// fakeSessions is a Sessions fleet.
type fakeSessions struct {
	live     map[ids.WorkspaceID]bool
	started  []ids.WorkspaceID
	startErr error
	// onStop runs at the top of Stop; see Stop.
	onStop func()
	// startCtxErr records what the context handed to Start already said, so a
	// test can tell a start that ran on a live context from one that ran on a
	// cancelled one.
	startCtxErr error
	stopped     []stopCall
	stopErr     error
	// resumes are the cold-gate re-opens the fleet was asked for, and resumeErr
	// is the refusal it answers with instead.
	resumes   []ColdResume
	resumeErr error
	// resumePhases are the compaction phases this fake relays back through
	// ColdResume.OnPhase, in order, standing in for the shim's own stream.
	resumePhases []*conversationv1.SessionCompactionProgress
	// observeResume runs at the TOP of ResumeCold, before anything else, so a
	// test can read the surfaces as they stood when the shim was dialed.
	observeResume func()
	// detachEntered is closed when a detached start begins and detachHold is
	// what it then waits on, for the one test whose subject is that the caller
	// does NOT wait. A nil detachHold runs the start inline.
	detachEntered chan struct{}
	detachHold    chan struct{}

	// startEntered, when set, receives every workspace whose Start was
	// entered, BEFORE it waits on startHold; startHold, when set, holds every
	// Start until it is closed. startCalls counts every Start that got past
	// the gate, failed ones included, under startMu.
	startEntered chan ids.WorkspaceID
	startHold    chan struct{}
	startMu      sync.Mutex
	startCalls   []ids.WorkspaceID

	// resumeHold, when set, holds a detached cold re-open until it is closed,
	// for the tests whose subject is that the answer does NOT wait on it;
	// resumeSettled is closed once that held re-open's outcome has been
	// reported. A nil resumeHold runs the re-open inline.
	resumeHold    chan struct{}
	resumeSettled chan struct{}
	// rebound is every workspace whose start was asked for as a REBIND — the
	// start that follows a bind, and the only one that tells the shim to adopt
	// the resumed conversation as the workspace's book.
	rebound []ids.WorkspaceID
}

type stopCall struct {
	WS    ids.WorkspaceID
	Force bool
}

func newFakeSessions() *fakeSessions {
	return &fakeSessions{live: map[ids.WorkspaceID]bool{}}
}

func (s *fakeSessions) Start(ctx context.Context, ws ids.WorkspaceID) error {
	// THE GATE: a test that arranged one learns the start was entered and
	// holds it there until it releases, which is how an interleaving with a
	// start in flight is arranged deterministically.
	if s.startEntered != nil {
		s.startEntered <- ws
	}
	if s.startHold != nil {
		<-s.startHold
	}
	s.startMu.Lock()
	defer s.startMu.Unlock()
	s.startCtxErr = ctx.Err()
	s.startCalls = append(s.startCalls, ws)
	if s.startErr != nil {
		return s.startErr
	}
	s.started = append(s.started, ws)
	s.live[ws] = true
	return nil
}

// StartRebound is Start, remembered separately: the distinction between the
// two IS the behavior under test, so a fake that collapsed them would let a
// bind that started a plain resume pass.
func (s *fakeSessions) StartRebound(ctx context.Context, ws ids.WorkspaceID) error {
	s.startMu.Lock()
	s.rebound = append(s.rebound, ws)
	s.startMu.Unlock()
	return s.Start(ctx, ws)
}

// StartDetached runs the start INLINE and reports its outcome, which is what
// keeps a test's assertions deterministic: the fake stands for the fleet's
// contract (the caller does not wait on the answer), not for its goroutine.
// The one test whose subject IS the detachment drives the real Fleet.
func (s *fakeSessions) StartDetached(ws ids.WorkspaceID, done func(error)) {
	if s.detachHold == nil {
		err := s.Start(context.Background(), ws)
		if done != nil {
			done(err)
		}
		return
	}
	hold, entered := s.detachHold, s.detachEntered
	go func() {
		if entered != nil {
			close(entered)
		}
		<-hold
		err := s.Start(context.Background(), ws)
		if done != nil {
			done(err)
		}
	}()
}

func (s *fakeSessions) Stop(_ context.Context, ws ids.WorkspaceID, force bool) error {
	// Runs BEFORE anything else this fake does, so a test can cancel the
	// caller's context at exactly the point a real client's deadline lapses:
	// after the choice is validated and with the swap under way.
	if s.onStop != nil {
		s.onStop()
	}
	if s.stopErr != nil {
		return s.stopErr
	}
	s.stopped = append(s.stopped, stopCall{ws, force})
	delete(s.live, ws)
	return nil
}

func (s *fakeSessions) Live(ws ids.WorkspaceID) bool { return s.live[ws] }

func (s *fakeSessions) ResumeCold(_ context.Context, ws ids.WorkspaceID, resume ColdResume) error {
	// The observer runs BEFORE anything else this fake does, which is what
	// lets a test read the footer exactly as the shim would first be dialed.
	if s.observeResume != nil {
		s.observeResume()
	}
	if s.resumeErr != nil {
		return s.resumeErr
	}
	s.resumes = append(s.resumes, resume)
	for _, phase := range s.resumePhases {
		if resume.OnPhase != nil {
			resume.OnPhase(phase)
		}
	}
	s.live[ws] = true
	return nil
}

// ResumeColdDetached runs the re-open INLINE and reports its outcome, for the
// reason StartDetached does; a test that set resumeHold drives the detachment
// itself and learns the outcome was reported from resumeSettled.
func (s *fakeSessions) ResumeColdDetached(ws ids.WorkspaceID, resume ColdResume, done func(context.Context, error)) {
	run := func() {
		err := s.ResumeCold(context.Background(), ws, resume)
		if done != nil {
			done(context.Background(), err)
		}
	}
	if s.resumeHold == nil {
		run()
		return
	}
	hold, settled := s.resumeHold, s.resumeSettled
	go func() {
		<-hold
		run()
		if settled != nil {
			close(settled)
		}
	}()
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
	// standDownRefused makes StandDown answer false, the shape a DETACHED
	// client has: this daemon is ordering no teardown of that process.
	standDownRefused bool
	// stoodDown counts the stand-down latch arms, and standDownBeforeKill
	// records whether the latch was armed before KillSession was asked.
	stoodDown           int
	standDownBeforeKill bool
	// transcripts is the answer ReadTranscripts gives, and transcriptsErr the
	// transport failure it gives instead.
	transcripts    *shimv1.ReadTranscriptsResponse
	transcriptsErr error
	// transcriptReads counts the reads, so a bind can be shown to validate
	// against a FRESH listing rather than a remembered one.
	transcriptReads int
}

func (s *fakeShim) ReadTranscripts(context.Context) (*shimv1.ReadTranscriptsResponse, error) {
	s.transcriptReads++
	if s.transcriptsErr != nil {
		return nil, s.transcriptsErr
	}
	if s.transcripts != nil {
		return s.transcripts, nil
	}
	return &shimv1.ReadTranscriptsResponse{
		Result: &shimv1.ReadTranscriptsResponse_Success{Success: &shimv1.ReadTranscriptsSuccess{}},
	}, nil
}

type killedTurn struct {
	Turn        ids.TurnID
	Force       bool
	CommandedBy *conversationv1.AgentInterruptedByUser
}

type deliveredAnswer struct {
	Agent  string
	Answer *conversationv1.AgentAnswer
}

func (s *fakeShim) KillTurn(_ context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	if s.killTurnErr != nil {
		return s.killTurnErr
	}
	s.killedTurns = append(s.killedTurns, killedTurn{turn, force, commandedBy})
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

// StandDown arms the fake's stand-down latch, answering whether it armed.
func (s *fakeShim) StandDown() bool {
	if s.standDownRefused {
		return false
	}
	s.stoodDown++
	return true
}

func (s *fakeShim) KillSession(_ context.Context, force bool) error {
	s.standDownBeforeKill = s.stoodDown > 0
	if s.killSessionErr != nil {
		return s.killSessionErr
	}
	s.killedSession = append(s.killedSession, force)
	return nil
}

// fakeBrowser is an externalbrowser.Opener.
type fakeBrowser struct {
	opened         []string
	openedProfiles []string
	err            error
	// profileByEmail is the routing ProfileForAccount answers with; an email
	// it does not hold takes defaultProfile, exactly as an unmatched account
	// falls back to the pinned default.
	profileByEmail map[string]string
	defaultProfile string
	// askedEmails records every email ProfileForAccount was asked to route, in
	// order, so a test can assert the verb routes by the session's account.
	askedEmails []string
}

func (b *fakeBrowser) Open(_ context.Context, url, profile string) error {
	if b.err != nil {
		return b.err
	}
	b.opened = append(b.opened, url)
	b.openedProfiles = append(b.openedProfiles, profile)
	return nil
}

func (b *fakeBrowser) ProfileForAccount(email string) string {
	b.askedEmails = append(b.askedEmails, email)
	if p, ok := b.profileByEmail[email]; ok {
		return p
	}
	return b.defaultProfile
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
	// ended is every remediated gate retired; reraised every gate stood again
	// after a failed re-open, and reraiseRefused makes ReraiseColdGate answer
	// that the facts are gone.
	ended          []string
	reraised       []string
	reraiseRefused bool
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

// EndColdGate records the remediated gate retired.
func (c *fakeCards) EndColdGate(_ ids.WorkspaceID, vendorSessionID string) {
	c.ended = append(c.ended, vendorSessionID)
}

// ReraiseColdGate records the gate stood again.
func (c *fakeCards) ReraiseColdGate(_ ids.WorkspaceID, vendorSessionID string) bool {
	if c.reraiseRefused {
		return false
	}
	c.reraised = append(c.reraised, vendorSessionID)
	return true
}

func (c *fakeCards) TakeColdGate(_ ids.WorkspaceID, vendorSessionID string) bool {
	if c.coldGate == nil || c.coldGate.VendorSessionID != vendorSessionID {
		return false
	}
	c.coldGate = nil
	c.coldGatesCleared++
	return true
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

// BindRecordTee implements dlog.Surfaces; this double tees nothing.
func (s *fakeSurfaces) BindRecordTee(dlog.RecordTee) {}

func (s *fakeSurfaces) DetachDir(string) error { return nil }

func (s *fakeSurfaces) AttachDir(string) error { return nil }

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
	verbs    Verbs
	db       *fakeDB
	git      *fakeGit
	account  *fakeAccounts
	queue    *fakeQueue
	merge    *fakeMerge
	rollout  *fakeRollout
	feed     *fakeFeed
	footer   *fakeFooter
	sidebar  *fakeSidebar
	health   *fakeHealth
	host     *fakeHost
	banners  *fakeBanners
	browser  *fakeBrowser
	fleet    *fakeSessions
	shim     *fakeShim
	owner    *fakeOwnership
	cards    *fakeCards
	log      *fakeSurfaces
	headless *fakeHeadless
	// topbarParked is every park state the topbar seam was handed, in order.
	topbarParked []bool
	// topbarColdGates is every cold-gate state the topbar seam was handed, in
	// order. The STRIP has its own cold-gate state, and it is retired by the
	// same answer that retires the footer's.
	topbarColdGates []topbar.ColdGate
	// topbarAccounts is every account cell the topbar seam was handed, in
	// order.
	topbarAccounts []topbar.Account

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
		db:       newFakeDB(),
		git:      &fakeGit{defaultBranch: "master", currentBranch: "feature", commonDir: "/repo", mainWorktree: "/repo"},
		account:  &fakeAccounts{configDir: "/config"},
		queue:    newFakeQueue(),
		merge:    newFakeMerge(),
		rollout:  &fakeRollout{done: make(chan struct{}, 8)},
		feed:     &fakeFeed{},
		footer:   newFakeFooter(),
		sidebar:  &fakeSidebar{},
		health:   &fakeHealth{},
		host:     &fakeHost{},
		banners:  &fakeBanners{},
		browser:  &fakeBrowser{},
		fleet:    newFakeSessions(),
		shim:     &fakeShim{},
		owner:    &fakeOwnership{standing: StandingOwned},
		cards:    newFakeCards(),
		log:      newFakeSurfaces(),
		headless: &fakeHeadless{answers: []headlessAnswer{{text: FixtureMintedName}}},
		briefs:   map[string]prompts.Prompt{},
	}
	f.queue.db = f.db
	// EVERY UNNAMED CREATE NAMES THROUGH THE MODEL, so the naming brief is part
	// of the arrangement every create test starts from, exactly as the corpus
	// ships it.
	f.briefs[BriefWorkspaceName] = prompts.Prompt{
		Name: BriefWorkspaceName, Body: "name the work: {{prompt}} {{conversation}} {{correction}}",
		Placeholders: []string{"prompt", "conversation", "correction"},
	}
	f.hasSession = true

	verbs, err := New(Deps{
		DB: f.db, Git: f.git, Accounts: f.account, Queue: f.queue, Merge: f.merge,
		Rollout: f.rollout, Feed: f.feed, Footer: f.footer, Topbar: stubTopbar{parked: &f.topbarParked, coldGates: &f.topbarColdGates, accounts: &f.topbarAccounts}, Browser: f.browser,
		Sidebar: f.sidebar, Holds: stubHolds{}, Host: f.host, Banners: f.banners, Sessions: f.fleet,
		Headless:   f.headless,
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
type stubTopbar struct {
	topbar.Resolver
	// parked records every park state the verbs installed or lifted, in order.
	parked *[]bool
	// coldGates records every cold-gate state the verbs stated, in order.
	coldGates *[]topbar.ColdGate
	// accounts records every account cell the verbs installed, in order.
	accounts *[]topbar.Account
}
type stubHolds struct{ holds.Resolver }

func (s stubTopbar) SetColdGate(_ ids.WorkspaceID, gate topbar.ColdGate) {
	if s.coldGates != nil {
		*s.coldGates = append(*s.coldGates, gate)
	}
}

func (s stubTopbar) SetParked(_ ids.WorkspaceID, parked bool) {
	if s.parked != nil {
		*s.parked = append(*s.parked, parked)
	}
}

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
func (s stubTopbar) SetAccount(_ ids.WorkspaceID, account topbar.Account) {
	if s.accounts != nil {
		*s.accounts = append(*s.accounts, account)
	}
}
func (stubHolds) SetWorkspaceDir(ids.WorkspaceID, string) error { return nil }

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

// FixtureMintedName is the name the fixture's naming call answers. It is the
// SHAPE a real answer has — at most three lowercase hyphenated words — and it
// is deliberately not derivable from any prompt, so a test asserting on it is
// asserting that the MODEL named the workspace and not some leftover
// truncation rule.
const FixtureMintedName = "the-minted-name"

// headlessAnswer is one scripted answer from the fake naming call.
type headlessAnswer struct {
	text string
	err  error
}

// fakeHeadless is the naming call, scripted. NOTHING in these tests execs a
// vendor binary: the whole point of the headless.Runner seam is that the
// workspace verbs are tested against answers, not processes.
type fakeHeadless struct {
	answers []headlessAnswer
	// calls records every request, so a test can assert the model, the site
	// and the prompt the call was made with.
	calls []headless.Request
}

func (h *fakeHeadless) Bin() string { return "fake-claude" }

func (h *fakeHeadless) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	h.calls = append(h.calls, req)
	if len(h.answers) == 0 {
		return headless.Response{}, errors.New("fake headless: no answer scripted")
	}
	answer := h.answers[0]
	if len(h.answers) > 1 {
		h.answers = h.answers[1:]
	}
	if answer.err != nil {
		return headless.Response{}, answer.err
	}
	return headless.Response{Text: answer.text, Model: req.Model, Duration: time.Millisecond}, nil
}

// hibernate records the idle sweep's own terminal on a workspace's session,
// which is what makes it read as asleep to the verbs that ask.
func (f *fixture) hibernate(ws ids.WorkspaceID) {
	f.db.sessions[ws] = wsm.Session{
		Workspace: ws,
		Terminal:  &wsm.SessionTerminal{Kind: wsm.TerminalHibernated, Detail: "idle past the cutoff", At: fixedNow},
	}
}

// receive takes one value off ch, failing the test when none arrives inside
// recordDeadline. A FAILURE bound, never a synchronization device: it returns
// the moment the value lands.
func receive[T any](t *testing.T, ch <-chan T, what string) T {
	t.Helper()
	select {
	case v := <-ch:
		return v
	case <-time.After(recordDeadline):
		t.Fatalf("%s never happened", what)
		var zero T
		return zero
	}
}

// gateStarts arms the fleet's start gate: every Start announces itself on the
// returned channel and then holds until release is closed.
func (f *fixture) gateStarts(buffer int) (entered <-chan ids.WorkspaceID, release chan struct{}) {
	in := make(chan ids.WorkspaceID, buffer)
	release = make(chan struct{})
	f.fleet.startEntered, f.fleet.startHold = in, release
	return in, release
}

// selectAsync runs Select on its own goroutine and answers its outcome.
func (f *fixture) selectAsync(ctx context.Context, ws ids.WorkspaceID) <-chan error {
	done := make(chan error, 1)
	go func() { done <- f.verbs.Select(ctx, ws) }()
	return done
}
