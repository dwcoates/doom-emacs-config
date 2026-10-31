package workspace

import (
	"cmp"
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/account"
	"agent-repld/internal/bounce"
	"agent-repld/internal/claudesettings"
	"agent-repld/internal/creategate"
	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/feedid"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/headless"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/netstate"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/prompts"
	"agent-repld/internal/pullrefs"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/slackpost"
	"agent-repld/internal/wsm"
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

	// queuedMu guards queued, the creates held for the network, by key;
	// queuedPutErr fails holding one and queuedReadErr reading them.
	queuedMu      sync.Mutex
	queued        map[string]wsm.QueuedCreate
	queuedPutErr  error
	queuedReadErr error

	// defaultModels is each login's chosen default model, by account root;
	// defaultModelErr fails its read and putDefaultModelErr its write.
	defaultModels      map[string]string
	defaultModelErr    error
	putDefaultModelErr error

	// workspaceByDirErr fails WorkspaceByDir, at workspaceByDirErrAt alone
	// when that is set.
	workspaceByDirErr   error
	workspaceByDirErrAt string

	// owedMu guards owed, the resumptions a stand-down recorded.
	owedMu sync.Mutex
	owed   map[ids.WorkspaceID][]ids.TurnID

	// swaps is every SwapPlaces call, in order; swapErr fails it.
	swaps   [][2]ids.WorkspaceID
	swapErr error
	// moves is every MoveWorkspace call, in order; moveErr fails it.
	moves   [][2]ids.WorkspaceID
	moveErr error
	// subsections are what ReadArrangement answers beside the workspaces and
	// repositories; arrangements is every arrangement write, spelled
	// "<method> <args>", in order; arrangeErr fails each of them.
	subsections  []wsm.Subsection
	arrangements []string
	arrangeErr   error
	// readArrangementErr fails ReadArrangement.
	readArrangementErr error

	// rolledBack is each workspace's rolled-back turns; rolledBackErr fails
	// the read.
	rolledBack    map[ids.WorkspaceID][]ids.TurnID
	rolledBackErr error

	workspaces map[ids.WorkspaceID]wsm.Workspace
	byDir      map[string]wsm.Workspace
	// reserved are the merge names standing async requests reserved, by
	// name; reservedErr fails their read.
	reserved    map[string]ids.WorkspaceID
	reservedErr error
	// revealErr fails RevealMergeWorkspace; reveals records each reveal.
	revealErr error
	reveals   []ids.WorkspaceID
	// listWorkspacesErr fails the roster read, for the tests about what a
	// republish does with a state client that would not answer.
	listWorkspacesErr error
	repositories      []wsm.Repository
	// setFoldedErr fails the fold write, which the slice cannot.
	setFoldedErr error
	// listRepositoriesErr fails the registry read, which the create path must
	// surface rather than read as "the repository is not registered".
	listRepositoriesErr error
	// mergeQueues is every repository's merge queue; mergeQueuesErr fails its
	// read.
	mergeQueues    map[wsm.RepoKey][]wsm.MergeQueueEntry
	mergeQueuesErr error
	// books are the workspaces' recorded books (SetWorkspaceBook);
	// setBookErr fails the record and bookErr the read.
	books map[ids.WorkspaceID]string
	// gates are the workspaces' recorded cold gates; gateErr fails their
	// read and setGateErr their record and retirement.
	gates      map[ids.WorkspaceID]wsm.ColdGateRecord
	gateErr    error
	setGateErr error
	setBookErr error
	bookErr    error
	tasks      []wsm.Task
	// setTaskFoldedErr fails the task fold write, which the slice cannot.
	// view is the sidebar's recorded view state, nil when nobody changed it
	// (read back as wsm.DefaultSidebarView); setMergedFoldedErr,
	// setMaturityFoldedErr and setGroupingErr fail its writes and viewErr its
	// read.
	view               *wsm.SidebarView
	setMergedFoldedErr error
	setHiddenFoldedErr error
	setOpenFoldedErr   error
	viewErr            error
	// workspaceErr fails the one-workspace read, which the map cannot.
	workspaceErr error
	// setHiddenErr fails the hidden-flag write.
	setHiddenErr error
	// setParentChange is what SetParent answers on success; setParentErr
	// fails it. parentWrites records each retarget asked for.
	setParentChange wsm.ParentChange
	setParentErr    error
	parentWrites    []wsm.Retarget
	// restoreStale and restoreErr are RestoreParent's answer; restores
	// records each change it was asked to undo.
	restoreStale bool
	restoreErr   error
	restores     []wsm.ParentChange
	current      *ids.WorkspaceID
	currentErr   error
	sessions     map[ids.WorkspaceID]wsm.Session
	// sessionsReads counts the bulk session reads, so a test can pin that
	// the roster reads every session at once.
	sessionsReads int
	// sessionErr makes every session read fail, which is the only way to
	// reach the roster's session-read error branch: the fake's own map
	// cannot fail.
	sessionErr error
	// tasksErr is what every Tasks read answers.
	tasksErr error
	jobs     map[ids.WorkspaceID]wsm.CreationJob
	held     map[ids.WorkspaceID][]wsm.HeldPrompt
	// landedWrites is every SetLanded, in order; setLandedErr fails it.
	landedWrites []*wsm.Landing
	setLandedErr error
	// setMergedErr fails SetMergedAt.
	setMergedErr error

	// openChildrenErr fails every OpenChildren read.
	openChildrenErr error

	// openParentErr is what every RequireOpenParent answers; openParentAsked
	// records each parent it was asked about.
	openParentErr   error
	openParentAsked []ids.WorkspaceID

	registerErr error
	registered  []wsm.RegisterFacts
	registerDir string
	createdNew  bool

	// refuseTemporaryErr is what RefuseTemporary answers.
	refuseTemporaryErr error

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
	// conversations is what RecentConversationPrompts answers per workspace.
	conversations   map[ids.WorkspaceID][]wsm.ConversationPrompt
	conversationErr error
	// pendingForks and lineage are the fork records; the errors fail the
	// write or the read that names them.
	pendingForks     map[ids.WorkspaceID]wsm.PendingFork
	putPendingErr    error
	pendingForkErr   error
	lineage          map[ids.WorkspaceID][]wsm.ForkLineage
	lineageErr       error
	resolveForkErr   error
	resolvedForks    []resolvedFork
	bindSessionCalls []wsm.Session
	closedFlags      map[ids.WorkspaceID]bool
	// onMarkSelfClose runs once a self-close mark is written, for a test that
	// moves the session between the mark and the recheck; markSelfCloseErr
	// fails the mark and takeSelfCloseErr the take.
	onMarkSelfClose func()
	setOneShotErr   error
	setAwaitingErr  error
	// links are each workspace's stored links, most recently set first;
	// linksErr fails every change to them and linksReadErr every read.
	links map[ids.WorkspaceID][]wsm.WorkspaceLink
	// slackOrigins are the recorded originating threads; slackOriginErr
	// fails a record and slackOriginReadErr a read.
	slackOrigins       map[ids.WorkspaceID]string
	slackOriginErr     error
	slackOriginReadErr error
	linksErr           error
	linksReadErr       error
	// setNameErr fails SetName; listWorkspacesErrAfterRename becomes
	// listWorkspacesErr once SetName writes, for a test that fails only the
	// reads after a rename is recorded.
	setNameErr                   error
	listWorkspacesErrAfterRename error
	markSelfCloseErr             error
	takeSelfCloseErr             error
	// reopened records every Reopen, in order.
	reopened []ids.WorkspaceID
	// closures is every close record MarkClosed was handed, in order, per
	// workspace.
	closures map[ids.WorkspaceID][]wsm.Closure
	// setClosedErr makes the closed-flag write fail, which the map cannot.
	setClosedErr error
	priorities   map[ids.WorkspaceID]*wsm.Priority
	attention    map[ids.WorkspaceID]bool
	currentAt    time.Time
	// keptHistorySelects are the selections SetCurrentKeepingLastSelected
	// recorded, in order.
	keptHistorySelects []ids.WorkspaceID
	// keepSelectedErr fails SetCurrentKeepingLastSelected.
	keepSelectedErr error
	forgotten       []ids.WorkspaceID
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
	// setVendorErr fails the resume-handle write, which the map cannot.
	setVendorErr error
	orphanReport wsm.OrphanReport
	createdTasks []string
	taskChanges  map[ids.TaskID]wsm.TaskChange
	assignments  map[ids.WorkspaceID]*ids.TaskID
	taskErr      error

	// dbFaults is the fault table the fleet opens and closes lost-link rows
	// in; dbClosed records the ids CloseFault was called with.
	dbFaults []wsm.Fault
	dbClosed []ids.FaultID
	// dbOpened counts every fault ever opened, so an id is never reused
	// after a close.
	dbOpened int

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
	// reopenClaims records the serving instance every Reopen claimed for, in
	// order.
	reopenClaims []servingClaim

	// coldMu guards the cold read payments, which a detached re-open writes
	// off the test's goroutine. coldPayments is each workspace's standing
	// payment, coldDrops every workspace a drop was asked for, in order; the
	// errors fail the write, the drop or the read, which the map cannot.
	coldMu           sync.Mutex
	coldPayments     map[ids.WorkspaceID]wsm.ColdReadPayment
	coldDrops        []ids.WorkspaceID
	putColdErr       error
	dropColdErr      error
	coldPaymentOfErr error
}

func (d *fakeDB) PutColdReadPayment(_ context.Context, id ids.WorkspaceID, p wsm.ColdReadPayment) error {
	d.coldMu.Lock()
	defer d.coldMu.Unlock()
	if d.putColdErr != nil {
		return d.putColdErr
	}
	if d.coldPayments == nil {
		d.coldPayments = map[ids.WorkspaceID]wsm.ColdReadPayment{}
	}
	d.coldPayments[id] = p
	return nil
}

func (d *fakeDB) DropColdReadPayment(_ context.Context, id ids.WorkspaceID) error {
	d.coldMu.Lock()
	defer d.coldMu.Unlock()
	d.coldDrops = append(d.coldDrops, id)
	if d.dropColdErr != nil {
		return d.dropColdErr
	}
	delete(d.coldPayments, id)
	return nil
}

func (d *fakeDB) ColdReadPaymentOf(_ context.Context, id ids.WorkspaceID) (wsm.ColdReadPayment, bool, error) {
	d.coldMu.Lock()
	defer d.coldMu.Unlock()
	if d.coldPaymentOfErr != nil {
		return wsm.ColdReadPayment{}, false, d.coldPaymentOfErr
	}
	p, ok := d.coldPayments[id]
	return p, ok, nil
}

// coldPayment answers the payment standing on ws.
func (d *fakeDB) coldPayment(ws ids.WorkspaceID) (wsm.ColdReadPayment, bool) {
	d.coldMu.Lock()
	defer d.coldMu.Unlock()
	p, ok := d.coldPayments[ws]
	return p, ok
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
	d.dbOpened++
	id := ids.FaultID(fmt.Sprintf("db-fault-%d", d.dbOpened))
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

// RolledBackTurns answers the workspace's scripted rolled-back turns.
func (d *fakeDB) RolledBackTurns(_ context.Context, ws ids.WorkspaceID) ([]ids.TurnID, error) {
	if d.rolledBackErr != nil {
		return nil, d.rolledBackErr
	}
	return d.rolledBack[ws], nil
}

// OweResumption records a stand-down's debt.
func (d *fakeDB) OweResumption(_ context.Context, ws ids.WorkspaceID, turn ids.TurnID, _ time.Time) error {
	d.owedMu.Lock()
	defer d.owedMu.Unlock()
	if d.owed == nil {
		d.owed = map[ids.WorkspaceID][]ids.TurnID{}
	}
	d.owed[ws] = append(d.owed[ws], turn)
	return nil
}

// owedTurns answers the turns a stand-down owed on ws.
func (d *fakeDB) owedTurns(ws ids.WorkspaceID) []ids.TurnID {
	d.owedMu.Lock()
	defer d.owedMu.Unlock()
	return append([]ids.TurnID(nil), d.owed[ws]...)
}

func (d *fakeDB) ColdGate(_ context.Context, id ids.WorkspaceID) (wsm.ColdGateRecord, bool, error) {
	if d.gateErr != nil {
		return wsm.ColdGateRecord{}, false, d.gateErr
	}
	rec, ok := d.gates[id]
	return rec, ok, nil
}

func (d *fakeDB) SetColdGate(_ context.Context, id ids.WorkspaceID, rec wsm.ColdGateRecord) error {
	if d.setGateErr != nil {
		return d.setGateErr
	}
	d.gates[id] = rec
	return nil
}

func (d *fakeDB) ClearColdGate(_ context.Context, id ids.WorkspaceID) (bool, error) {
	if d.setGateErr != nil {
		return false, d.setGateErr
	}
	_, ok := d.gates[id]
	delete(d.gates, id)
	return ok, nil
}

func (d *fakeDB) SetWorkspaceBook(_ context.Context, id ids.WorkspaceID, book string) error {
	if d.setBookErr != nil {
		return d.setBookErr
	}
	d.books[id] = book
	return nil
}

func (d *fakeDB) WorkspaceBook(_ context.Context, id ids.WorkspaceID) (string, bool, error) {
	if d.bookErr != nil {
		return "", false, d.bookErr
	}
	book, ok := d.books[id]
	return book, ok, nil
}

func newFakeDB() *fakeDB {
	return &fakeDB{
		workspaces:  map[ids.WorkspaceID]wsm.Workspace{},
		byDir:       map[string]wsm.Workspace{},
		sessions:    map[ids.WorkspaceID]wsm.Session{},
		books:       map[ids.WorkspaceID]string{},
		gates:       map[ids.WorkspaceID]wsm.ColdGateRecord{},
		jobs:        map[ids.WorkspaceID]wsm.CreationJob{},
		held:        map[ids.WorkspaceID][]wsm.HeldPrompt{},
		closedFlags: map[ids.WorkspaceID]bool{},
		closures:    map[ids.WorkspaceID][]wsm.Closure{},
		priorities:  map[ids.WorkspaceID]*wsm.Priority{},
		attention:   map[ids.WorkspaceID]bool{},
		terminals:   map[ids.WorkspaceID]wsm.SessionTerminal{},
		taskChanges: map[ids.TaskID]wsm.TaskChange{},
		assignments: map[ids.WorkspaceID]*ids.TaskID{},
	}
}

// with records one workspace in the fake registry, keyed both ways.
func (d *fakeDB) ReservedMergeNames(context.Context) (map[string]ids.WorkspaceID, error) {
	if d.reservedErr != nil {
		return nil, d.reservedErr
	}
	out := map[string]ids.WorkspaceID{}
	for name, ws := range d.reserved {
		out[name] = ws
	}
	return out, nil
}

// RevealMergeWorkspace stands for the durable reveal: a merge workspace's
// flag is set once, and any other workspace is refused.
func (d *fakeDB) RevealMergeWorkspace(_ context.Context, id ids.WorkspaceID) (bool, error) {
	if d.revealErr != nil {
		return false, d.revealErr
	}
	ws, ok := d.workspaces[id]
	if !ok {
		return false, wsm.ErrNotFound
	}
	if ws.MergeOf == nil {
		return false, errors.New("fakeDB: the workspace is no merge workspace")
	}
	d.reveals = append(d.reveals, id)
	if ws.MergeRevealed {
		return false, nil
	}
	ws.MergeRevealed = true
	d.with(ws)
	return true, nil
}

func (d *fakeDB) with(ws wsm.Workspace) *fakeDB {
	d.workspaces[ws.ID] = ws
	d.byDir[ws.Dir] = ws
	return d
}

func (d *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	if d.workspaceErr != nil {
		return wsm.Workspace{}, d.workspaceErr
	}
	ws, ok := d.workspaces[id]
	if !ok {
		return wsm.Workspace{}, fmt.Errorf("fake: no such workspace %s: %w", id, wsm.ErrNotFound)
	}
	return ws, nil
}

func (d *fakeDB) WorkspaceByDir(_ context.Context, dir string) (wsm.Workspace, error) {
	if d.workspaceByDirErr != nil && (d.workspaceByDirErrAt == "" || d.workspaceByDirErrAt == dir) {
		return wsm.Workspace{}, d.workspaceByDirErr
	}
	ws, ok := d.byDir[dir]
	if !ok {
		// The store's own contract: a dir nothing is registered at is
		// wsm.ErrNotFound.
		return wsm.Workspace{}, fmt.Errorf("no workspace at that dir: %w", wsm.ErrNotFound)
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
		ID: ids.WorkspaceID("ws-" + filepath.Base(dir)), Dir: dir, Repo: d.repoFor(dir, facts),
		Name: facts.Name, Branch: facts.Branch, ParentBranch: facts.ParentBranch,
		Parent: facts.Parent, Priority: facts.Priority,
	}
	if facts.Closure != nil {
		ws.Closed, ws.Closure = true, facts.Closure
	}
	d.with(ws)
	d.createdNew = true
	return ws, true, nil
}

// RefuseTemporary answers the scripted temporary-directory refusal.
func (d *fakeDB) RefuseTemporary(string) error { return d.refuseTemporaryErr }

// repoFor answers the repository a minted workspace is registered under, as
// the store does: the repository already held at facts.RepoDir, or one minted
// by the main worktree's own registration. A fixture that registers a linked
// worktree of a repository it never states keeps the fixed "repo-1".
func (d *fakeDB) repoFor(dir string, facts wsm.RegisterFacts) ids.RepoID {
	if facts.RepoDir == "" {
		return "repo-1"
	}
	if repo, found := wsm.RepositoryAt(d.repositories, facts.RepoDir); found {
		return repo.ID
	}
	if dir != facts.RepoDir {
		return "repo-1"
	}
	repo := wsm.Repository{
		ID: ids.RepoID("repo-" + dir), Dir: dir,
		Name: filepath.Base(dir), DefaultBranch: facts.DefaultBranch,
	}
	d.repositories = append(d.repositories, repo)
	return repo.ID
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

func (d *fakeDB) AllMergeQueues(context.Context) (map[wsm.RepoKey][]wsm.MergeQueueEntry, error) {
	return d.mergeQueues, d.mergeQueuesErr
}

func (d *fakeDB) Tasks(context.Context) ([]wsm.Task, error) { return d.tasks, d.tasksErr }

func (d *fakeDB) Current(context.Context) (*ids.WorkspaceID, error) {
	if d.currentErr != nil {
		return nil, d.currentErr
	}
	return d.current, nil
}

func (d *fakeDB) ReadArrangement(ctx context.Context) (wsm.Arrangement, error) {
	if d.readArrangementErr != nil {
		return wsm.Arrangement{}, d.readArrangementErr
	}
	workspaces, err := d.ListWorkspaces(ctx)
	if err != nil {
		return wsm.Arrangement{}, err
	}
	repositories, err := d.ListRepositories(ctx)
	if err != nil {
		return wsm.Arrangement{}, err
	}
	return wsm.Arrangement{Workspaces: workspaces, Repositories: repositories, Subsections: d.subsections}, nil
}

// arrange records one arrangement write, or fails it with arrangeErr.
func (d *fakeDB) arrange(call string) error {
	if d.arrangeErr != nil {
		return d.arrangeErr
	}
	d.arrangements = append(d.arrangements, call)
	return nil
}

func (d *fakeDB) CreateSubsection(_ context.Context, repo ids.RepoID, name string) (wsm.Subsection, error) {
	return wsm.Subsection{ID: "sub-new", Repo: repo, Name: name, Place: 1}, d.arrange("CreateSubsection " + string(repo) + " " + name)
}

func (d *fakeDB) CreateAgentSubsection(_ context.Context, creator ids.WorkspaceID, name string, members []ids.WorkspaceID) (wsm.Subsection, error) {
	return wsm.Subsection{ID: "sub-agent", Repo: "repo-1", Name: name, Place: 1, CreatedBy: &creator},
		d.arrange(fmt.Sprintf("CreateAgentSubsection %s %s %v", creator, name, members))
}

func (d *fakeDB) RenameSubsection(_ context.Context, id ids.SubsectionID, name string) error {
	return d.arrange("RenameSubsection " + string(id) + " " + name)
}

func (d *fakeDB) DeleteSubsection(_ context.Context, id ids.SubsectionID) error {
	return d.arrange("DeleteSubsection " + string(id))
}

func (d *fakeDB) MoveSubsection(_ context.Context, id ids.SubsectionID, to wsm.Anchor) error {
	return d.arrange("MoveSubsection " + string(id) + " " + wsm.AnchorField(to))
}

func (d *fakeDB) MoveRepository(_ context.Context, id ids.RepoID, before *ids.RepoID) error {
	return d.arrange("MoveRepository " + string(id) + " " + ids.Optional(before))
}

func (d *fakeDB) PlaceWorkspaces(_ context.Context, ws []ids.WorkspaceID, into *ids.SubsectionID, at wsm.Anchor, marks wsm.OneShotMarks) error {
	return d.arrange(fmt.Sprintf("PlaceWorkspaces %v %s %s %s", ws, ids.Optional(into), wsm.AnchorField(at), marks))
}

func (d *fakeDB) MoveWorkspace(_ context.Context, ws, target ids.WorkspaceID) error {
	if d.moveErr != nil {
		return d.moveErr
	}
	d.moves = append(d.moves, [2]ids.WorkspaceID{ws, target})
	return nil
}

func (d *fakeDB) SwapPlaces(_ context.Context, ws, target ids.WorkspaceID) error {
	if d.swapErr != nil {
		return d.swapErr
	}
	d.swaps = append(d.swaps, [2]ids.WorkspaceID{ws, target})
	return nil
}

// SetOneShot marks the fake's creation job; a workspace with none is
// wsm.ErrNotFound, as the store answers. setOneShotErr fails it.
func (d *fakeDB) SetOneShot(_ context.Context, id ids.WorkspaceID, oneShot bool) error {
	if d.setOneShotErr != nil {
		return d.setOneShotErr
	}
	job, ok := d.jobs[id]
	if !ok {
		return fmt.Errorf("fake: no creation job for %s: %w", id, wsm.ErrNotFound)
	}
	job.OneShot = oneShot
	d.jobs[id] = job
	ws := d.workspaces[id]
	ws.OneShot = oneShot
	d.workspaces[id] = ws
	return nil
}

// SetAwaiting marks the fake's record; setAwaitingErr fails it.
func (d *fakeDB) SetAwaiting(_ context.Context, id ids.WorkspaceID, awaited *ids.WorkspaceID) error {
	if d.setAwaitingErr != nil {
		return d.setAwaitingErr
	}
	ws := d.workspaces[id]
	ws.Awaiting = awaited
	d.workspaces[id] = ws
	return nil
}

// WorkspaceLinks answers the workspace's stored links; linksReadErr fails it.
func (d *fakeDB) WorkspaceLinks(_ context.Context, id ids.WorkspaceID) ([]wsm.WorkspaceLink, error) {
	if d.linksReadErr != nil {
		return nil, d.linksReadErr
	}
	return slices.Clone(d.links[id]), nil
}

// SetWorkspaceLink stores or relabels a link at the front, shown while fewer
// than wsm.ShownLinkLimit are, as the store does; linksErr fails it.
func (d *fakeDB) SetWorkspaceLink(_ context.Context, id ids.WorkspaceID, url, label string, owned bool) ([]wsm.WorkspaceLink, error) {
	if d.linksErr != nil {
		return nil, d.linksErr
	}
	if d.links == nil {
		d.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{}
	}
	links := d.links[id]
	link := wsm.WorkspaceLink{URL: url, Label: label, Owned: owned}
	if i := slices.IndexFunc(links, func(l wsm.WorkspaceLink) bool { return l.URL == url }); i >= 0 {
		link.Shown = links[i].Shown
		links = slices.Delete(links, i, i+1)
	} else {
		shown := 0
		for _, l := range links {
			if l.Shown {
				shown++
			}
		}
		link.Shown = shown < wsm.ShownLinkLimit
	}
	d.links[id] = append([]wsm.WorkspaceLink{link}, links...)
	return slices.Clone(d.links[id]), nil
}

// RemoveWorkspaceLink removes a stored link, answering *wsm.UnknownLinkError
// for one the workspace does not hold; linksErr fails it.
func (d *fakeDB) RemoveWorkspaceLink(_ context.Context, id ids.WorkspaceID, url string) ([]wsm.WorkspaceLink, error) {
	if d.linksErr != nil {
		return nil, d.linksErr
	}
	i := slices.IndexFunc(d.links[id], func(l wsm.WorkspaceLink) bool { return l.URL == url })
	if i < 0 {
		return nil, &wsm.UnknownLinkError{URL: url}
	}
	d.links[id] = slices.Delete(d.links[id], i, i+1)
	return slices.Clone(d.links[id]), nil
}

// ShowWorkspaceLinks shows exactly URLS, answering *wsm.UnknownLinkError for
// one the workspace does not hold and changing nothing; linksErr fails it.
func (d *fakeDB) ShowWorkspaceLinks(_ context.Context, id ids.WorkspaceID, urls []string) ([]wsm.WorkspaceLink, error) {
	if d.linksErr != nil {
		return nil, d.linksErr
	}
	links := slices.Clone(d.links[id])
	for _, url := range urls {
		if !slices.ContainsFunc(links, func(l wsm.WorkspaceLink) bool { return l.URL == url }) {
			return nil, &wsm.UnknownLinkError{URL: url}
		}
	}
	for i := range links {
		links[i].Shown = slices.Contains(urls, links[i].URL)
	}
	d.links[id] = links
	return slices.Clone(links), nil
}

// SetWorkspaceSlackOrigin records the workspace's originating thread;
// slackOriginErr fails it and records nothing.
func (d *fakeDB) SetWorkspaceSlackOrigin(_ context.Context, id ids.WorkspaceID, url string) error {
	if d.slackOriginErr != nil {
		return d.slackOriginErr
	}
	if d.slackOrigins == nil {
		d.slackOrigins = map[ids.WorkspaceID]string{}
	}
	d.slackOrigins[id] = url
	return nil
}

// WorkspaceSlackOrigin answers the recorded originating thread;
// slackOriginReadErr fails it.
func (d *fakeDB) WorkspaceSlackOrigin(_ context.Context, id ids.WorkspaceID) (string, bool, error) {
	if d.slackOriginReadErr != nil {
		return "", false, d.slackOriginReadErr
	}
	url, found := d.slackOrigins[id]
	return url, found, nil
}

// ClearWorkspaceLinks removes every stored link; linksErr fails it.
func (d *fakeDB) ClearWorkspaceLinks(_ context.Context, id ids.WorkspaceID) error {
	if d.linksErr != nil {
		return d.linksErr
	}
	delete(d.links, id)
	return nil
}

// SetName records the name on the fake's record; setNameErr fails it and
// writes nothing.
func (d *fakeDB) SetName(_ context.Context, id ids.WorkspaceID, name string) error {
	if d.setNameErr != nil {
		return d.setNameErr
	}
	ws, ok := d.workspaces[id]
	if !ok {
		return fmt.Errorf("fake: no such workspace %s: %w", id, wsm.ErrNotFound)
	}
	ws.Name = name
	d.workspaces[id] = ws
	if d.listWorkspacesErrAfterRename != nil {
		d.listWorkspacesErr = d.listWorkspacesErrAfterRename
	}
	return nil
}

// MarkCloseWhenTurnEnds marks the fake's record.
func (d *fakeDB) MarkCloseWhenTurnEnds(_ context.Context, id ids.WorkspaceID, at time.Time) error {
	if d.markSelfCloseErr != nil {
		return d.markSelfCloseErr
	}
	ws, ok := d.workspaces[id]
	if !ok {
		return fmt.Errorf("fake: no such workspace %s: %w", id, wsm.ErrNotFound)
	}
	ws.CloseWhenTurnEnds = &at
	d.workspaces[id] = ws
	if d.onMarkSelfClose != nil {
		d.onMarkSelfClose()
	}
	return nil
}

// TakeCloseWhenTurnEnds clears the fake's mark, answering whether one stood.
func (d *fakeDB) TakeCloseWhenTurnEnds(_ context.Context, id ids.WorkspaceID) (bool, error) {
	if d.takeSelfCloseErr != nil {
		return false, d.takeSelfCloseErr
	}
	ws := d.workspaces[id]
	taken := ws.CloseWhenTurnEnds != nil
	ws.CloseWhenTurnEnds = nil
	d.workspaces[id] = ws
	return taken, nil
}

func (d *fakeDB) MarkClosed(_ context.Context, id ids.WorkspaceID, closure wsm.Closure) error {
	if d.setClosedErr != nil {
		return d.setClosedErr
	}
	d.closedFlags[id] = true
	d.closures[id] = append(d.closures[id], closure)
	return nil
}

func (d *fakeDB) Reopen(_ context.Context, id ids.WorkspaceID, serving ids.InstanceID) error {
	if d.setClosedErr != nil {
		return d.setClosedErr
	}
	d.closedFlags[id] = false
	d.reopened = append(d.reopened, id)
	d.reopenClaims = append(d.reopenClaims, servingClaim{ws: id, instance: serving})
	// The record reads open from here on, as the store's does.
	if ws, ok := d.workspaces[id]; ok {
		ws.Closed = false
		d.workspaces[id] = ws
	}
	return nil
}

func (d *fakeDB) SetCurrent(_ context.Context, id ids.WorkspaceID, at time.Time) error {
	d.current, d.currentAt = &id, at
	// As WSM does: the selection stamps the durable last-selected instant
	// the roster row carries.
	if ws, ok := d.workspaces[id]; ok {
		stamped := at
		ws.LastSelectedAt = &stamped
		d.workspaces[id] = ws
	}
	return nil
}

func (d *fakeDB) SetCurrentKeepingLastSelected(_ context.Context, id ids.WorkspaceID) error {
	if d.keepSelectedErr != nil {
		return d.keepSelectedErr
	}
	d.current = &id
	d.keptHistorySelects = append(d.keptHistorySelects, id)
	return nil
}

func (d *fakeDB) SetAttention(_ context.Context, id ids.WorkspaceID, on bool) error {
	d.attention[id] = on
	return nil
}

// SetParent records the retarget asked for and answers the arranged change.
func (d *fakeDB) SetParent(_ context.Context, id ids.WorkspaceID, retarget wsm.Retarget) (wsm.ParentChange, error) {
	d.parentWrites = append(d.parentWrites, retarget)
	if d.setParentErr != nil {
		return wsm.ParentChange{}, d.setParentErr
	}
	return d.setParentChange, nil
}

// RestoreParent records the undo asked for and answers the arranged result.
func (d *fakeDB) RestoreParent(_ context.Context, change wsm.ParentChange) (bool, error) {
	d.restores = append(d.restores, change)
	return d.restoreStale, d.restoreErr
}

// SetHidden records the hidden flag and its instant on the workspace's
// record, as WSM does, so the republished registry carries them.
func (d *fakeDB) SetHidden(_ context.Context, id ids.WorkspaceID, hidden bool, at time.Time) error {
	if d.setHiddenErr != nil {
		return d.setHiddenErr
	}
	ws, ok := d.workspaces[id]
	if !ok {
		return wsm.ErrNotFound
	}
	ws.Hidden = hidden
	ws.HiddenAt = nil
	if hidden {
		stamped := at
		ws.HiddenAt = &stamped
	}
	d.workspaces[id] = ws
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

func (d *fakeDB) AccountDefaultModel(_ context.Context, configDir string) (string, bool, error) {
	if d.defaultModelErr != nil {
		return "", false, d.defaultModelErr
	}
	model, ok := d.defaultModels[configDir]
	return model, ok, nil
}

func (d *fakeDB) PutAccountDefaultModel(_ context.Context, configDir, model string) error {
	if d.putDefaultModelErr != nil {
		return d.putDefaultModelErr
	}
	if d.defaultModels == nil {
		d.defaultModels = map[string]string{}
	}
	d.defaultModels[configDir] = model
	return nil
}

func (d *fakeDB) Session(_ context.Context, id ids.WorkspaceID) (wsm.Session, bool, error) {
	if d.sessionErr != nil {
		return wsm.Session{}, false, d.sessionErr
	}
	s, ok := d.sessions[id]
	return s, ok, nil
}

func (d *fakeDB) Sessions(context.Context) ([]wsm.Session, error) {
	d.sessionsReads++
	if d.sessionErr != nil {
		return nil, d.sessionErr
	}
	out := make([]wsm.Session, 0, len(d.sessions))
	for _, s := range d.sessions {
		out = append(out, s)
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Workspace < out[j].Workspace })
	return out, nil
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

// RecentConversationPrompts answers the scripted rows, truncated to the most
// recent `limit`, ordinals renumbered from zero, so a naming test can assert
// the call reads a tail rather than the whole scripted conversation.
func (d *fakeDB) RecentConversationPrompts(_ context.Context, id ids.WorkspaceID, limit int) ([]wsm.ConversationPrompt, error) {
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
	out := make([]wsm.ConversationPrompt, len(tail))
	for i, row := range tail {
		row.Ordinal = int64(i)
		out[i] = row
	}
	return out, nil
}

// resolvedFork is one ResolveFork the fake recorded.
type resolvedFork struct {
	vendorSessionID string
	lineage         wsm.ForkLineage
}

func (d *fakeDB) PutPendingFork(_ context.Context, fork wsm.PendingFork) error {
	if d.putPendingErr != nil {
		return d.putPendingErr
	}
	if d.pendingForks == nil {
		d.pendingForks = map[ids.WorkspaceID]wsm.PendingFork{}
	}
	d.pendingForks[fork.Workspace] = fork
	return nil
}

// SetLanded records a requester's landing on its record, as the store's
// columns do; setLandedErr fails it.
func (d *fakeDB) SetLanded(_ context.Context, id ids.WorkspaceID, landing *wsm.Landing) error {
	if d.setLandedErr != nil {
		return d.setLandedErr
	}
	d.landedWrites = append(d.landedWrites, landing)
	ws := d.workspaces[id]
	ws.Landed = landing
	d.with(ws)
	return nil
}

// SetMergedAt records the merge stamp on the record; setMergedErr fails it.
func (d *fakeDB) SetMergedAt(_ context.Context, id ids.WorkspaceID, at time.Time, _ *uint32) error {
	if d.setMergedErr != nil {
		return d.setMergedErr
	}
	ws := d.workspaces[id]
	ws.MergedAt = &at
	d.with(ws)
	return nil
}

func (d *fakeDB) PendingFork(_ context.Context, id ids.WorkspaceID) (wsm.PendingFork, bool, error) {
	if d.pendingForkErr != nil {
		return wsm.PendingFork{}, false, d.pendingForkErr
	}
	fork, ok := d.pendingForks[id]
	return fork, ok, nil
}

// ResolveFork records the fork as the store does: the session's vendor id,
// the lineage, and the pending fork retired, or nothing on a failure.
func (d *fakeDB) ResolveFork(_ context.Context, vendorSessionID string, lineage wsm.ForkLineage) error {
	if d.resolveForkErr != nil {
		return d.resolveForkErr
	}
	d.resolvedForks = append(d.resolvedForks, resolvedFork{vendorSessionID: vendorSessionID, lineage: lineage})
	delete(d.pendingForks, lineage.Workspace)
	session := d.sessions[lineage.Workspace]
	session.VendorSessionID = vendorSessionID
	d.sessions[lineage.Workspace] = session
	if d.lineage == nil {
		d.lineage = map[ids.WorkspaceID][]wsm.ForkLineage{}
	}
	d.lineage[lineage.Workspace] = append([]wsm.ForkLineage{lineage}, d.lineage[lineage.Parent]...)
	return nil
}

func (d *fakeDB) Lineage(_ context.Context, id ids.WorkspaceID) ([]wsm.ForkLineage, error) {
	if d.lineageErr != nil {
		return nil, d.lineageErr
	}
	return d.lineage[id], nil
}

// BindSession records the session and forgets the workspace's fork records,
// as the store does in one transaction.
func (d *fakeDB) BindSession(ctx context.Context, s wsm.Session) error {
	d.bindSessionCalls = append(d.bindSessionCalls, s)
	delete(d.pendingForks, s.Workspace)
	delete(d.lineage, s.Workspace)
	return d.PutSession(ctx, s)
}

func (d *fakeDB) HeldPrompts(_ context.Context, id ids.WorkspaceID) ([]wsm.HeldPrompt, error) {
	return d.held[id], nil
}

func (d *fakeDB) CloseOrphans(context.Context, ids.WorkspaceID, time.Time, wsm.OrphanCut) (wsm.OrphanReport, error) {
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
	// mainOf answers MainWorktree per directory, as git does: a worktree
	// CreateWorktree cut belongs to the repository it was cut from, and that
	// repository's main worktree belongs to itself. A directory it does not
	// name answers mainWorktree.
	mainOf       map[string]string
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
	// offPrimary are the base refs IsAncestor answers NOT on the primary
	// branch; every other ref is on it. ancestorErr fails the check itself.
	offPrimary  map[string]bool
	ancestorErr error
	// ancestorChecks records each IsAncestor call as "ancestor..descendant".
	ancestorChecks []string
	// existingBranches are the branches this repository already holds, which
	// is what the naming call's collision probe asks for.
	existingBranches map[string]bool
	// branchExistsErrFor fails BranchExists for one branch alone.
	branchExistsErrFor map[string]error
	branchExistsErr    error

	created   []createdWorktree
	createErr error
	// createHangs makes CreateWorktree run until its context ends, as a git
	// stuck behind a hook that never returns does.
	createHangs bool
	// duringCreate, when set, runs inside CreateWorktree, while the create
	// is mid-flight.
	duringCreate func()
	// fetched records each Fetch as "dir remote"; fetchErr fails every one.
	fetched  []string
	fetchErr error
	nuked    []nukedWorktree
	nukeErr  error

	// gitCalls records the restore path's git, in order, so a test can tell
	// a prune that came BEFORE the add from one that came after it.
	gitCalls []string
	// worktrees is what ListWorktrees answers. An entry whose directory is
	// gone is a STALE registration: RestoreWorktree refuses its path exactly
	// as `git worktree add` does, and UnregisterMissingWorktree retires it.
	worktrees     []gitclient.Worktree
	listErr       error
	unregisterErr error
	restored      []restoredWorktree
	restoreErr    error

	// heads is what ResolveRef answers for HEAD in a directory; a directory
	// absent from it answers the default "sha-of-HEAD".
	heads map[string]string
	// dirty makes IsClean answer false; isCleanErr fails it.
	dirty      bool
	isCleanErr error
	// deletedBranches records every DeleteBranchAt as BRANCH@HEAD;
	// deleteBranchErr fails it.
	deletedBranches []string
	deleteBranchErr error
	// removedClean records every RemoveCleanWorktree; removeCleanErr fails it.
	removedClean   []nukedWorktree
	removeCleanErr error
	// fastForwarded records every FastForward as "dir->commit";
	// fastForwardErr fails it.
	fastForwarded  []string
	fastForwardErr error
}

type restoredWorktree struct{ RepoDir, WorktreeDir, Branch string }

type createdWorktree struct{ RepoDir, Branch, BaseRef, WorktreeDir string }
type nukedWorktree struct{ RepoDir, WorktreeDir, Branch string }

func (g *fakeGit) ResolveRef(_ context.Context, dir, ref string) (string, error) {
	if g.resolveErr != nil {
		return "", g.resolveErr
	}
	if head, ok := g.heads[dir]; ok && ref == "HEAD" {
		return head, nil
	}
	return "sha-of-" + ref, nil
}

func (g *fakeGit) IsClean(context.Context, string) (bool, error) {
	return !g.dirty, g.isCleanErr
}

func (g *fakeGit) RemoveCleanWorktree(_ context.Context, repoDir, worktreeDir string) error {
	if g.removeCleanErr != nil {
		return g.removeCleanErr
	}
	g.removedClean = append(g.removedClean, nukedWorktree{RepoDir: repoDir, WorktreeDir: worktreeDir})
	return nil
}

func (g *fakeGit) IsAncestor(_ context.Context, _, ancestor, descendant string) (bool, error) {
	g.ancestorChecks = append(g.ancestorChecks, ancestor+".."+descendant)
	if g.ancestorErr != nil {
		return false, g.ancestorErr
	}
	return !g.offPrimary[ancestor], nil
}

func (g *fakeGit) FastForward(_ context.Context, dir, commit string) error {
	if g.fastForwardErr != nil {
		return g.fastForwardErr
	}
	g.fastForwarded = append(g.fastForwarded, dir+"->"+commit)
	return nil
}

// DeleteBranchAt records a landed branch's deletion (landedbranch.Retire).
func (g *fakeGit) DeleteBranchAt(_ context.Context, _, branch, head string) error {
	if g.deleteBranchErr != nil {
		return g.deleteBranchErr
	}
	g.deletedBranches = append(g.deletedBranches, branch+"@"+head)
	return nil
}

func (g *fakeGit) BranchExists(_ context.Context, _, branch string) (bool, error) {
	if err := g.branchExistsErrFor[branch]; err != nil {
		return false, err
	}
	return g.existingBranches[branch], g.branchExistsErr
}

func (g *fakeGit) CommonDir(context.Context, string) (string, error) {
	return g.commonDir, g.commonDirErr
}

// MainWorktree answers what registration derives the repository from. The
// fixture keys it the same way CommonDir is keyed, since a fake repository has
// exactly one of each.
func (g *fakeGit) MainWorktree(_ context.Context, dir string) (string, error) {
	if g.commonDirErr != nil {
		return "", g.commonDirErr
	}
	if main, ok := g.mainOf[dir]; ok {
		return main, nil
	}
	return g.mainWorktree, nil
}

// belongsTo records that DIR is a worktree of the repository whose main
// worktree is MAIN, exactly as a cut by CreateWorktree would have.
func (g *fakeGit) belongsTo(t testing.TB, dir, main string) {
	t.Helper()
	if err := g.recordCut(main, dir); err != nil {
		t.Fatalf("record %q as a worktree of %q: %v", dir, main, err)
	}
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

func (g *fakeGit) Fetch(_ context.Context, dir, remote string) error {
	g.fetched = append(g.fetched, dir+" "+remote)
	return g.fetchErr
}

func (g *fakeGit) CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error {
	if g.duringCreate != nil {
		g.duringCreate()
	}
	if g.createHangs {
		<-ctx.Done()
		return ctx.Err()
	}
	if g.createErr != nil {
		return g.createErr
	}
	g.created = append(g.created, createdWorktree{repoDir, branch, baseRef, worktreeDir})
	if err := os.MkdirAll(filepath.Join(worktreeDir, ".git"), 0o755); err != nil {
		return err
	}
	return g.recordCut(repoDir, worktreeDir)
}

// recordCut records what CreateWorktree made true in git: the new worktree
// and the repository's own main worktree both belong to the repository.
func (g *fakeGit) recordCut(repoDir, worktreeDir string) error {
	if g.mainOf == nil {
		g.mainOf = map[string]string{}
	}
	main, err := normalizeDir(repoDir)
	if err != nil {
		return err
	}
	cut, err := normalizeDir(worktreeDir)
	if err != nil {
		return err
	}
	g.mainOf[main], g.mainOf[cut] = main, main
	return nil
}

func (g *fakeGit) ListWorktrees(_ context.Context, repoDir string) ([]gitclient.Worktree, error) {
	g.gitCalls = append(g.gitCalls, "list "+repoDir)
	return g.worktrees, g.listErr
}

func (g *fakeGit) UnregisterMissingWorktree(_ context.Context, _, worktreeDir string) error {
	g.gitCalls = append(g.gitCalls, "unregister "+worktreeDir)
	if g.unregisterErr != nil {
		return g.unregisterErr
	}
	kept := g.worktrees[:0]
	for _, wt := range g.worktrees {
		if wt.Dir != worktreeDir {
			kept = append(kept, wt)
		}
	}
	g.worktrees = kept
	return nil
}

// RestoreWorktree checks the branch out at worktreeDir, materializing the
// directory as the real add does, and refuses a path git still registers,
// with git's own words, as `git worktree add` does.
func (g *fakeGit) RestoreWorktree(_ context.Context, repoDir, worktreeDir, branch string) error {
	g.gitCalls = append(g.gitCalls, "restore "+worktreeDir)
	if g.restoreErr != nil {
		return g.restoreErr
	}
	for _, wt := range g.worktrees {
		if wt.Dir == worktreeDir {
			return fmt.Errorf("fatal: '%s' is a missing but already registered worktree;\nuse 'add -f' to override, or 'prune' or 'remove' to clear", worktreeDir)
		}
	}
	g.restored = append(g.restored, restoredWorktree{repoDir, worktreeDir, branch})
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

	// settingsRevivals is every ReviveForSettings asked; reviveErr its answer.
	settingsRevivals []ids.WorkspaceID
	reviveErr        error

	submissions []promptqueue.Submission
	submitErr   error
	// submitRefusedArm makes Submit answer a refusal disposition.
	submitRefusedArm string
	acts             map[ids.WorkspaceID][]promptqueue.Act
	actErr           error
	// db is the fixture's store, which the door closes orphans on.
	db *fakeDB
	// rollBacks records every RollBack call, in order.
	rollBacks []rollBackCall
	// rollBackErr is what RollBack answers BEFORE perform runs, mirroring the
	// real queue's contract that a refused rollback never performs it: a
	// test wanting perform's own error (a *ShimRefusal or
	// promptqueue.ErrHoldsChanged surfaced from inside perform) scripts that
	// through the fake shim or leaves this nil and lets perform run.
	rollBackErr error
	// withdraws records every WithdrawRevivalTurn call's workspace, in order;
	// withdrawn and withdrawErr are what each answers.
	withdraws   []ids.WorkspaceID
	withdrawn   bool
	withdrawErr error
	// startAwaits records every AwaitTurnStarted call's turn, in order;
	// startAwaitErr is what each answers, and onStartAwait runs as each is
	// made.
	startAwaits   []ids.TurnID
	startAwaitErr error
	onStartAwait  func()
	// gateFates records every ColdGateAnswered fate, in order; gateFateErr is
	// what each answers, and onGateFate runs as each is settled.
	gateFates   []promptqueue.GateFate
	gateFateErr error
	onGateFate  func()
}

// ColdGateAnswered records the fate and answers the scripted outcome.
func (q *fakeQueue) ColdGateAnswered(_ context.Context, _ ids.WorkspaceID, fate promptqueue.GateFate) error {
	q.gateFates = append(q.gateFates, fate)
	if q.onGateFate != nil {
		q.onGateFate()
	}
	return q.gateFateErr
}

// WithdrawRevivalTurn records the call and answers the scripted outcome.
func (q *fakeQueue) WithdrawRevivalTurn(_ context.Context, ws ids.WorkspaceID) (bool, error) {
	q.withdraws = append(q.withdraws, ws)
	return q.withdrawn, q.withdrawErr
}

// AwaitTurnStarted records the call and answers the scripted outcome.
func (q *fakeQueue) AwaitTurnStarted(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) error {
	q.startAwaits = append(q.startAwaits, turn)
	if q.onStartAwait != nil {
		q.onStartAwait()
	}
	return q.startAwaitErr
}

// rollBackCall is one RollBack call the fake queue recorded.
type rollBackCall struct {
	WS       ids.WorkspaceID
	Since    time.Time
	DropHeld []ids.TurnID
}

// RollBack records the call and, absent a scripted refusal, runs perform
// exactly as the real queue does: perform's own error (or success) is what
// RollBack answers with.
func (q *fakeQueue) RollBack(ctx context.Context, ws ids.WorkspaceID, since time.Time, drop []ids.TurnID, perform func(context.Context) error) error {
	q.rollBacks = append(q.rollBacks, rollBackCall{WS: ws, Since: since, DropHeld: drop})
	if q.rollBackErr != nil {
		return q.rollBackErr
	}
	return perform(ctx)
}

// CloseOrphans is the queue's door, closing on the fixture's own store.
func (q *fakeQueue) CloseOrphans(ctx context.Context, ws ids.WorkspaceID, at time.Time, cut wsm.OrphanCut) (wsm.OrphanReport, error) {
	return q.db.CloseOrphans(ctx, ws, at, cut)
}

func newFakeQueue() *fakeQueue {
	return &fakeQueue{acts: map[ids.WorkspaceID][]promptqueue.Act{}}
}

func (q *fakeQueue) Submit(_ context.Context, sub promptqueue.Submission) (promptqueue.Disposition, error) {
	if q.submitErr != nil {
		return promptqueue.Disposition{}, q.submitErr
	}
	if q.submitRefusedArm != "" {
		return promptqueue.Disposition{RefusedArm: q.submitRefusedArm}, nil
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
	// queued reports the workspace's sync merge is queued, so OnInterrupt
	// raises the merge-queue question.
	queued     bool
	closed     []ids.WorkspaceID
	enqueued   []ids.WorkspaceID
	enqueueErr error
	// steps records the restart's suspension and take-back, in order, and
	// the rollout's bounces are appended by the tests that need the order.
	steps      []string
	suspendErr error
	// retired records every RetireConcluded.
	retired []ids.WorkspaceID
	// relabeled records every Relabel; relabelErr fails it.
	relabeled  []ids.WorkspaceID
	relabelErr error
}

// Relabel records the renamed workspace and answers relabelErr.
func (m *fakeMerge) Relabel(_ context.Context, ws ids.WorkspaceID) error {
	m.relabeled = append(m.relabeled, ws)
	return m.relabelErr
}

// RetireConcluded records the retirement.
func (m *fakeMerge) RetireConcluded(_ context.Context, ws ids.WorkspaceID) {
	m.retired = append(m.retired, ws)
}

// SuspendForRestart records the suspension and answers suspendErr.
func (m *fakeMerge) SuspendForRestart(_ context.Context, ws ids.WorkspaceID) error {
	m.steps = append(m.steps, "suspend:"+string(ws))
	return m.suspendErr
}

// ResumeAfterRestart records the take-back.
func (m *fakeMerge) ResumeAfterRestart(_ context.Context, ws ids.WorkspaceID) error {
	m.steps = append(m.steps, "resume:"+string(ws))
	return nil
}

func newFakeMerge() *fakeMerge {
	return &fakeMerge{facts: map[ids.WorkspaceID]footer.MergeFacts{}}
}

func (m *fakeMerge) Facts(ws ids.WorkspaceID) (footer.MergeFacts, bool) {
	f, ok := m.facts[ws]
	return f, ok
}

func (m *fakeMerge) OnInterrupt(_ context.Context, ws ids.WorkspaceID) bool {
	m.interrupted = append(m.interrupted, ws)
	return m.queued
}

func (m *fakeMerge) OnWorkspaceClosed(_ context.Context, ws ids.WorkspaceID) {
	m.closed = append(m.closed, ws)
}

func (m *fakeMerge) Enqueue(_ context.Context, req merge.Request) (merge.Enqueued, error) {
	ws := req.Workspace
	if m.enqueueErr != nil {
		return merge.Enqueued{}, m.enqueueErr
	}
	m.enqueued = append(m.enqueued, ws)
	return merge.Enqueued{}, nil
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
	// hold, when set, keeps every bounce from finishing until it is closed.
	hold chan struct{}
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
	err, refused, hold := r.relaunchErr, r.refuseErr, r.hold
	r.mu.Unlock()
	if refused != nil {
		return bounce.Decision{}, refused
	}
	go func() {
		if hold != nil {
			<-hold
		}
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
		if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: level, Operation: operation}); ok {
			return
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
	// lineages is every lineage NoteLineage stated, per workspace, in order.
	lineages map[ids.WorkspaceID][][]feed.LineageSegment
	// onReset runs AT the reset, which is how a test reads the swap's own
	// progress as it stood when the feed was emptied — the ordering assertion
	// with nothing to wait on.
	onReset func()
	// rolledBackTurns records every RollBackTurns call's turns, in order.
	rolledBackTurns [][]ids.TurnID
	// heldTurns and releasedTurns record every HoldTurns and ReleaseTurns
	// call's turns, in order.
	heldTurns     [][]ids.TurnID
	releasedTurns [][]ids.TurnID
	// withheldDrawn records every WithholdDrawnTurns call's turns, in order.
	withheldDrawn [][]ids.TurnID
	// mainAgents is every main agent the feed was told, in order.
	mainAgents []string
	// freshBooks is every workspace whose session came up fresh, in order.
	freshBooks []ids.WorkspaceID
	// sourcesUp is every workspace a history source came up for, in order.
	sourcesUp []ids.WorkspaceID
	// pullTargets is the latest pull request target installed per workspace,
	// nil for one installed as none.
	pullTargets map[ids.WorkspaceID]*pullrefs.Target
}

// RecentCodeSpans answers no spans.
func (f *fakeFeed) RecentCodeSpans(ids.WorkspaceID) []string { return nil }

// SetPullTarget records the pull request target installed for a workspace.
func (f *fakeFeed) SetPullTarget(ws ids.WorkspaceID, target *pullrefs.Target) {
	if f.pullTargets == nil {
		f.pullTargets = map[ids.WorkspaceID]*pullrefs.Target{}
	}
	f.pullTargets[ws] = target
}

// OnMainAgent records the main agent the feed was told.
func (f *fakeFeed) OnMainAgent(_ ids.WorkspaceID, agent *conversationv1.AgentId) {
	f.mainAgents = append(f.mainAgents, agent.GetValue())
}

// NoteFreshBook records a workspace whose session comes up fresh.
func (f *fakeFeed) NoteFreshBook(ws ids.WorkspaceID) {
	f.freshBooks = append(f.freshBooks, ws)
}

// NoteLineage records each lineage the fleet stated, per workspace, newest last.
func (f *fakeFeed) NoteLineage(ws ids.WorkspaceID, segments []feed.LineageSegment) {
	if f.lineages == nil {
		f.lineages = map[ids.WorkspaceID][][]feed.LineageSegment{}
	}
	f.lineages[ws] = append(f.lineages[ws], segments)
}

// SourceUp records that a history source came up.
func (f *fakeFeed) SourceUp(ws ids.WorkspaceID) {
	f.sourcesUp = append(f.sourcesUp, ws)
}

// RollBackTurns records the turns removed; the real resolver's own durable
// error is deliberately not modeled here, since rollback.go swallows it.
func (f *fakeFeed) RollBackTurns(_ ids.WorkspaceID, turns []ids.TurnID) error {
	f.rolledBackTurns = append(f.rolledBackTurns, turns)
	return nil
}

// HoldTurns records the hold.
func (f *fakeFeed) HoldTurns(_ ids.WorkspaceID, turns []ids.TurnID) {
	f.heldTurns = append(f.heldTurns, turns)
}

// ReleaseTurns records the release.
func (f *fakeFeed) ReleaseTurns(_ ids.WorkspaceID, turns []ids.TurnID) {
	f.releasedTurns = append(f.releasedTurns, turns)
}

// WithholdDrawnTurns records the drawn rows taken off the feed at the cut.
func (f *fakeFeed) WithholdDrawnTurns(_ ids.WorkspaceID, turns []ids.TurnID) {
	f.withheldDrawn = append(f.withheldDrawn, turns)
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

	// statuses is the status each workspace's strip answers.
	statuses map[ids.WorkspaceID]*frontendv1.FooterStatus
	// awaiting is the awaited name each workspace's strip was last told.
	awaiting map[ids.WorkspaceID]string

	// mainAgents is every main agent the footer was told, and historyPages the
	// entry count of every page it was handed, in order.
	mainAgents   []string
	historyPages []int

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
	// faults is every fault the verbs opened on the footer, in order.
	faults []footer.Fault
	// accounts is every account root the verbs bound each workspace to, in
	// order.
	accounts map[ids.WorkspaceID][]string
}

// SetAccount records the account root a workspace was bound to.
func (f *fakeFooter) SetAccount(ws ids.WorkspaceID, root string) {
	if f.accounts == nil {
		f.accounts = map[ids.WorkspaceID][]string{}
	}
	f.accounts[ws] = append(f.accounts[ws], root)
}

// OpenFault records a fault the verbs opened on the footer.
func (f *fakeFooter) OpenFault(_ ids.WorkspaceID, fault footer.Fault) {
	f.faults = append(f.faults, fault)
}

func (f *fakeFooter) SetParked(_ ids.WorkspaceID, park wsm.Park) {
	f.parked = append(f.parked, park != wsm.ParkNone)
}

// Prime records the workspaces registration primed the footer for, in order,
// which is what the register wiring test asserts.
func (f *fakeFooter) Prime(ws ids.WorkspaceID) {
	f.primed = append(f.primed, ws)
}

// OnMainAgent records the main agent the footer was told.
func (f *fakeFooter) OnMainAgent(_ ids.WorkspaceID, agent *conversationv1.AgentId) {
	f.mainAgents = append(f.mainAgents, agent.GetValue())
}

// OnHistoryPage records the entries of every page the footer was handed.
func (f *fakeFooter) OnHistoryPage(_ ids.WorkspaceID, _ *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	f.historyPages = append(f.historyPages, len(page.GetEntries()))
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

// coldAnswerLines is what every non-clearing answer the footer was handed
// says, in order: its request line, or the name of the phase it relays.
func (f *fakeFooter) coldAnswerLines() []string {
	out := []string{}
	for _, answer := range f.coldAnswers {
		switch {
		case answer == nil:
			continue
		case answer.Progress != nil:
			out = append(out, answer.Progress.GetPhase().String())
		default:
			out = append(out, answer.Request)
		}
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

func (s *fakeSidebar) SetRegistrySelected(reg sidebar.Registry, ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.registries = append(s.registries, reg)
	s.selected = append(s.selected, ws)
	s.calls = append(s.calls, "registry+selected")
}

func (s *fakeSidebar) SetSelected(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.selected = append(s.selected, ws)
	s.calls = append(s.calls, "selected")
}

// Selected answers the last selection the roster was told, as the real
// resolver's state would.
func (s *fakeSidebar) Selected() (ids.WorkspaceID, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if len(s.selected) == 0 {
		return "", false
	}
	return s.selected[len(s.selected)-1], true
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
	// commitLogOpens records every relayed open_commit_log target.
	commitLogOpens []*agentreplv1.OpenInEditorCommitLog
	// terminalRuns records every relayed run_in_terminal command.
	terminalRuns []*agentreplv1.OpenInEditorTerminalCommand
	reloads      []ids.WorkspaceID
	// hostPublishes records every host-state republish the verbs asked for.
	hostPublishes []ids.WorkspaceID
	// tailReturns records every feed return-to-tail the verbs asked for.
	tailReturns []ids.WorkspaceID
}

func (h *fakeHost) ReturnFeedToTail(ws ids.WorkspaceID) {
	h.tailReturns = append(h.tailReturns, ws)
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

func (h *fakeHost) OpenCommitLog(_ ids.WorkspaceID, target *agentreplv1.OpenInEditorCommitLog) {
	h.commitLogOpens = append(h.commitLogOpens, target)
}

func (h *fakeHost) RunInTerminal(_ ids.WorkspaceID, target *agentreplv1.OpenInEditorTerminalCommand) {
	h.terminalRuns = append(h.terminalRuns, target)
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
	// held marks a workspace whose shim is held with no session on it.
	held     map[ids.WorkspaceID]bool
	live     map[ids.WorkspaceID]bool
	started  []ids.WorkspaceID
	startErr error
	// standingDown is what StandingDown answers: the supervisor's latch.
	standingDown bool
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
	// startCauses are the start causes the starts' contexts stated.
	startCauses []shimlife.StartCause

	// resumeHold, when set, holds a detached cold re-open until it is closed,
	// for the tests whose subject is that the answer does NOT wait on it;
	// resumeSettled is closed once that held re-open's outcome has been
	// reported. A nil resumeHold runs the re-open inline.
	resumeHold    chan struct{}
	resumeSettled chan struct{}
	// resumeRunCtx is the context a detached re-open reports it ran under;
	// nil reports a live one.
	resumeRunCtx context.Context
	// rebound is every workspace whose start was asked for as a REBIND — the
	// start that follows a bind, and the only one that tells the shim to adopt
	// the resumed conversation as the workspace's book.
	rebound []ids.WorkspaceID
	// efforts is every SetEffort asked of the fleet; effortErr its answer.
	efforts   []effortCall
	effortErr error
	// retries is every RetryVendorError asked of the fleet, answered with
	// retryResponse and retryErr.
	retries       []ids.WorkspaceID
	retryResponse *shimv1.RetryVendorErrorNowResponse
	retryErr      error
	// vendorRunning makes CancelVendorStart report a run it ended, and
	// vendorCancels counts every CancelVendorStart asked.
	vendorRunning bool
	vendorCancels []ids.WorkspaceID
}

// CancelVendorStart records the cancellation and reports vendorRunning.
func (s *fakeSessions) CancelVendorStart(_ context.Context, ws ids.WorkspaceID) bool {
	s.vendorCancels = append(s.vendorCancels, ws)
	return s.vendorRunning
}

type stopCall struct {
	WS    ids.WorkspaceID
	Force bool
}

// SetEffort records the level asked for and answers effortErr.
func (s *fakeSessions) SetEffort(_ context.Context, _ dlog.Logger, ws ids.WorkspaceID, level conversationv1.AgentEffortLevel) error {
	s.efforts = append(s.efforts, effortCall{WS: ws, Level: level})
	return s.effortErr
}

// RetryVendorError records the ask and answers the scripted outcome.
func (s *fakeSessions) RetryVendorError(_ context.Context, _ dlog.Logger, ws ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error) {
	s.retries = append(s.retries, ws)
	return s.retryResponse, s.retryErr
}

type effortCall struct {
	WS    ids.WorkspaceID
	Level conversationv1.AgentEffortLevel
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
	if cause, ok := shimlife.StartOf(ctx); ok {
		s.startCauses = append(s.startCauses, cause)
	}
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

func (s *fakeSessions) StandingDown() bool { return s.standingDown }

// Held answers a live session's shim, or a shim held with no session (held).
func (s *fakeSessions) Held(ws ids.WorkspaceID) bool { return s.live[ws] || s.held[ws] }

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
			runCtx := s.resumeRunCtx
			if runCtx == nil {
				runCtx = context.Background()
			}
			done(runCtx, err)
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
	killedTurns []killedTurn
	killTurnErr error
	// killTurnHangs makes KillTurn wait for its context to end.
	killTurnHangs  bool
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
	// rollBacks records every RollBackSession call, in order.
	rollBacks []rolledBackSession
	// onRollBack, when set, runs as RollBackSession is called, so a test can
	// observe what was true at the moment of the cut.
	onRollBack func()
	// rollBackPaths is the restored-files paths RollBackSession answers with.
	rollBackPaths []string
	// rollBackErr fails every RollBackSession call.
	rollBackErr error
}

// rolledBackSession is one RollBackSession call the fake recorded.
type rolledBackSession struct {
	Turns        []ids.TurnID
	RestoreFiles bool
}

func (s *fakeShim) RollBackSession(_ context.Context, turns []ids.TurnID, restoreFiles bool) ([]string, error) {
	s.rollBacks = append(s.rollBacks, rolledBackSession{Turns: turns, RestoreFiles: restoreFiles})
	if s.onRollBack != nil {
		s.onRollBack()
	}
	if s.rollBackErr != nil {
		return nil, s.rollBackErr
	}
	return s.rollBackPaths, nil
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

func (s *fakeShim) KillTurn(ctx context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	if s.killTurnHangs {
		// A vendor that will not answer: the kill returns only when its
		// caller's bound ends it.
		<-ctx.Done()
		return ctx.Err()
	}
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

// fakeSlack is a slackpost.Poster: it records every reply and answers
// permalink, or err when set.
type fakeSlack struct {
	replies   []slackReply
	permalink string
	err       error
}

// slackReply is one reply fakeSlack was asked to post.
type slackReply struct {
	thread slackpost.Thread
	body   string
}

func (s *fakeSlack) Reply(_ context.Context, thread slackpost.Thread, markdown string) (string, error) {
	s.replies = append(s.replies, slackReply{thread: thread, body: markdown})
	return s.permalink, s.err
}

// fakeBrowser is an externalbrowser.Opener.
type fakeBrowser struct {
	opened []string
	// openedEmails records the account email every Open was handed, in order,
	// so a test can assert the verb routes by the session's account.
	openedEmails []string
	err          error
}

func (b *fakeBrowser) Open(_ context.Context, url, accountEmail string) error {
	if b.err != nil {
		return b.err
	}
	b.opened = append(b.opened, url)
	b.openedEmails = append(b.openedEmails, accountEmail)
	return nil
}

// fakeOwnership answers one standing for every workspace, and records each
// workspace a registration told it was created here.
type fakeOwnership struct {
	standing Standing
	err      error
	created  []ids.WorkspaceID
}

func (o *fakeOwnership) Standing(context.Context, ids.WorkspaceID) (Standing, error) {
	return o.standing, o.err
}

func (o *fakeOwnership) Created(ws ids.WorkspaceID) { o.created = append(o.created, ws) }

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
	efforts          []conversationv1.AgentEffortLevel
	hasEfforts       bool
	// edits is every draft edit applied, in order; draftOutcome and draftErr
	// are what EditQuestionDraft answers. released is every ask whose
	// delivery claim was freed.
	edits        []fakeDraftEdit
	draftOutcome feed.QuestionDraftOutcome
	draftErr     error
	released     []string
	// standing is the ask StandingAsk answers, when hasStanding; targets is
	// every target it was asked about.
	standing    feed.StandingAsk
	hasStanding bool
	targets     []*feedid.Ref
}

// fakeDraftEdit is one EditQuestionDraft call.
type fakeDraftEdit struct {
	ask  string
	edit feed.QuestionEdit
}

func (c *fakeCards) EditQuestionDraft(_ ids.WorkspaceID, ask string, edit feed.QuestionEdit) (feed.QuestionDraftOutcome, error) {
	c.edits = append(c.edits, fakeDraftEdit{ask: ask, edit: edit})
	return c.draftOutcome, c.draftErr
}

func (c *fakeCards) ReleaseQuestionDelivery(_ ids.WorkspaceID, ask string) {
	c.released = append(c.released, ask)
}

func (c *fakeCards) StandingAsk(_ ids.WorkspaceID, target *feedid.Ref) (feed.StandingAsk, bool) {
	c.targets = append(c.targets, target)
	return c.standing, c.hasStanding
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

// SetAwaiting records the awaited name the strip was told.
func (f *fakeFooter) SetAwaiting(ws ids.WorkspaceID, name string) {
	if f.awaiting == nil {
		f.awaiting = map[ids.WorkspaceID]string{}
	}
	f.awaiting[ws] = name
}

// Status answers the status seeded for WS, nil when none was.
func (f *fakeFooter) Status(ws ids.WorkspaceID) *frontendv1.FooterStatus { return f.statuses[ws] }

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

func (c *fakeCards) EffortLevels(ids.WorkspaceID) ([]conversationv1.AgentEffortLevel, bool) {
	return c.efforts, c.hasEfforts
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

func (s *fakeSurfaces) BindRecordCounter(dlog.RecordCounter)      {}
func (s *fakeSurfaces) RetainDirectory(string, func(string) bool) {}

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
func (s *fakeSurfaces) CentralClientLog(dlog.ClientRecord) error  { return errFake }

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
	// gateAnswers records every cold gate answer the verbs counted.
	gateAnswers fakeGateAnswers
	// visits is the settings visits a switch stands down, and visitDrain
	// the drain they are handed to.
	visits     *SettingsVisits
	visitDrain fakeVisitDrain
	// topbarPicked is the effort the topbar seam was handed as picked.
	topbarPicked conversationv1.AgentEffortLevel
	// creates is the create gate the verbs hold a ticket from.
	creates *creategate.Gate
	// network is the network state a failed naming call is classified by.
	network *fakeNetwork
	// recovered are the reporters answered for a held create's ops, by op.
	recovered map[string]*recordingProgress
	// recoveredMu guards recovered.
	recoveredMu sync.Mutex
	// life is the daemon's lifetime, ended at the test's cleanup.
	life context.Context
	// home is the home directory a feed link's leading `~` expands to.
	home    string
	verbs   Verbs
	db      *fakeDB
	git     *fakeGit
	account *fakeAccounts
	// pullOwners is the GitHub owner of each account root; empty, so no
	// workspace's references link anywhere until a test names an owner.
	pullOwners pullrefs.Owners
	queue      *fakeQueue
	merge      *fakeMerge
	rollout    *fakeRollout
	feed       *fakeFeed
	footer     *fakeFooter
	sidebar    *fakeSidebar
	health     *fakeHealth
	host       *fakeHost
	banners    *fakeBanners
	browser    *fakeBrowser
	slack      *fakeSlack
	fleet      *fakeSessions
	// threads is the threads' graphs a nuke deletes.
	threads  *fakeThreadGraphs
	shim     *fakeShim
	owner    *fakeOwnership
	cards    *fakeCards
	log      *fakeSurfaces
	headless *fakeHeadless
	// standDowns is every withheld workspace whose stand-down was asked for.
	standDowns []ids.WorkspaceID
	// visibilityEdges is every workspace whose hide or show the cache keeper
	// was told of.
	visibilityEdges []ids.WorkspaceID
	// coldMarks is every workspace whose answered cold gate the cache keeper
	// was told of (CacheLapses.ColdGateAnswered); coldMarkErr fails it.
	coldMarks   []ids.WorkspaceID
	coldMarkErr error
	// summaries is the summary restorer registration asks.
	summaries *fakeSummaries
	// topbarParked is every park state the topbar seam was handed, in order.
	topbarParked []bool
	// topbarColdGates is every cold-gate state the topbar seam was handed, in
	// order. The STRIP has its own cold-gate state, and it is retired by the
	// same answer that retires the footer's.
	topbarColdGates []topbar.ColdGate
	// topbarAccounts is every account cell the topbar seam was handed, in
	// order.
	topbarAccounts []topbar.Account
	// topbarLoginDefaults is every login default the topbar seam was moved
	// to, as {root, model}, in order.
	topbarLoginDefaults [][2]string
	// topbarEffortSettings is every effort settings read the topbar seam was
	// handed; topbarWarnings every warning line it was asked to raise.
	topbarEffortSettings []claudesettings.Effort
	topbarWarnings       []string
	// topbarLinks is every shown-links list the topbar seam was handed, in
	// order; an empty one is a clear.
	topbarLinks [][]topbar.Link
	topbarNames []string
	// effortReads is every config root whose settings were read; the read
	// answers effortSettings and effortErr.
	effortReads    []string
	effortSettings claudesettings.Effort
	effortErr      error

	// running is what the freeness probe answers.
	running Running
	// hasSession gates both the freeness probe and the shim resolver.
	hasSession bool
	// derived is every derived-workspace event the verbs reported.
	derived *fakeDerived
	// briefs is what the injected prompt loader answers, by brief name.
	briefs map[string]prompts.Prompt
	// briefErr fails every brief load.
	briefErr error
}

// derivedEvent is one reported creation of a derived workspace.
type derivedEvent struct {
	workspace   ids.WorkspaceID
	ownerNotice string
}

// fakeDerived records every reported creation.
type fakeDerived struct{ events []derivedEvent }

func (d *fakeDerived) ReportCreated(ws wsm.Workspace, ownerNotice string) {
	d.events = append(d.events, derivedEvent{workspace: ws.ID, ownerNotice: ownerNotice})
}

// newFixture arranges a verb surface with a live session, an owned workspace
// and no briefs, which is the arrangement most tests start from.
func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{
		creates: creategate.New(),
		home:    t.TempDir(),
		db:      newFakeDB(),
		// The main worktree is a REAL worktree directory: every registration
		// of a linked worktree registers it first.
		git:        &fakeGit{defaultBranch: "master", currentBranch: "feature", commonDir: "/repo", mainWorktree: worktreeDir(t)},
		account:    &fakeAccounts{configDir: "/config"},
		pullOwners: pullrefs.Owners{},
		queue:      newFakeQueue(),
		merge:      newFakeMerge(),
		rollout:    &fakeRollout{done: make(chan struct{}, 8)},
		feed:       &fakeFeed{},
		footer:     newFakeFooter(),
		sidebar:    &fakeSidebar{},
		health:     &fakeHealth{},
		host:       &fakeHost{},
		banners:    &fakeBanners{},
		browser:    &fakeBrowser{},
		slack:      &fakeSlack{},
		fleet:      newFakeSessions(),
		threads:    &fakeThreadGraphs{},
		shim:       &fakeShim{},
		owner:      &fakeOwnership{standing: StandingOwned},
		cards:      newFakeCards(),
		summaries:  &fakeSummaries{},
		log:        newFakeSurfaces(),
		headless:   &fakeHeadless{answers: []headlessAnswer{{text: FixtureMintedName}}},
		network:    newFakeNetwork(),
		recovered:  map[string]*recordingProgress{},
		briefs:     map[string]prompts.Prompt{},
		derived:    &fakeDerived{},
	}
	f.queue.db = f.db
	// EVERY UNNAMED CREATE NAMES THROUGH THE MODEL, so the naming brief is part
	// of the arrangement every create test starts from, exactly as the corpus
	// ships it.
	f.briefs[BriefWorkspaceName] = prompts.Prompt{
		Name: BriefWorkspaceName, Body: "name the work: {{prompt}} {{conversation}} .",
		Placeholders: []string{"prompt", "conversation"},
	}
	f.briefs[BriefWorkspaceNameShorten] = prompts.Prompt{
		Name: BriefWorkspaceNameShorten, Body: "shorten the name: {{answer}} {{problem}} {{limit}}",
		Placeholders: []string{"answer", "problem", "limit"},
	}
	f.hasSession = true

	life, endLife := context.WithCancel(context.Background())
	t.Cleanup(endLife)
	f.life = life
	verbs, err := New(Deps{
		Instance: fixtureInstance,
		Network:  f.network,
		Lifetime: life,
		CreateProgressFor: func(opID string) CreateProgress {
			f.recoveredMu.Lock()
			defer f.recoveredMu.Unlock()
			rec := &recordingProgress{op: opID, ended: make(chan struct{})}
			f.recovered[opID] = rec
			return rec
		},
		DB: f.db, Derived: f.derived, Git: f.git, Creates: f.creates, Accounts: f.account, PullOwners: f.pullOwners, Queue: f.queue, TurnStops: f.queue, Merge: f.merge,
		Rollout: f.rollout, Feed: f.feed, Footer: f.footer, Topbar: stubTopbar{picked: &f.topbarPicked, parked: &f.topbarParked, coldGates: &f.topbarColdGates, accounts: &f.topbarAccounts, loginDefaults: &f.topbarLoginDefaults, effortSettings: &f.topbarEffortSettings, warnings: &f.topbarWarnings, links: &f.topbarLinks, names: &f.topbarNames}, Browser: f.browser, Slack: f.slack,
		Sidebar: f.sidebar, Host: f.host, Banners: f.banners, Sessions: f.fleet, Threads: f.threads, ColdGateAnswers: &f.gateAnswers, SettingsVisits: f.ensureVisits(), Drain: &f.visitDrain,
		Headless:   f.headless,
		Health:     f.health,
		PromptsDir: "/prompts", CheckoutRoot: fixtureCheckoutRoot, HomeDir: f.home, Log: f.log,
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
		StandDowns: standDownsFunc(func(ws ids.WorkspaceID) {
			f.standDowns = append(f.standDowns, ws)
		}),
		CacheLapses: &fakeCacheLapses{
			visibility: func(ws ids.WorkspaceID) { f.visibilityEdges = append(f.visibilityEdges, ws) },
			answered: func(ws ids.WorkspaceID) error {
				f.coldMarks = append(f.coldMarks, ws)
				return f.coldMarkErr
			},
		},
		Cards:     f.cards,
		Summaries: f.summaries,
		ReadEffortSettings: func(configDir string) (claudesettings.Effort, error) {
			f.effortReads = append(f.effortReads, configDir)
			return f.effortSettings, f.effortErr
		},
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
// fakeThreadGraphs records each thread whose graph was deleted, failing with
// err when it is set.
type fakeThreadGraphs struct {
	deleted []ids.WorkspaceID
	err     error
	// opened is every thread Open was handed, in order; openErr fails Open.
	mu      sync.Mutex
	opened  []ids.WorkspaceID
	openErr error
}

func (f *fakeThreadGraphs) Open(_ context.Context, thread ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.opened = append(f.opened, thread)
	return f.openErr
}

func (f *fakeThreadGraphs) Delete(_ context.Context, thread ids.WorkspaceID) error {
	if f.err != nil {
		return f.err
	}
	f.deleted = append(f.deleted, thread)
	return nil
}

func (f *fixture) workspace(id ids.WorkspaceID, dir string) wsm.Workspace {
	normalized, err := normalizeDir(dir)
	if err != nil {
		panic(err)
	}
	dir = normalized
	ws := wsm.Workspace{ID: id, Kind: wsm.WorkspaceKindRepository, Dir: dir, Repo: "repo-1", Name: "sample", Branch: "ABC/sample"}
	f.db.with(ws)
	// A REAL DIRECTORY, because the roster no longer publishes a repository
	// whose main worktree is gone (`withoutGoneRepositories'). The workspace's
	// own parent is one the caller's `t.TempDir()' already made.
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: filepath.Dir(dir)})
	return ws
}

// workspaceInRepo registers workspace ID at a fresh directory, in the
// repository whose main worktree is repoDir (as mainWorktree registered it),
// so a create in that repository may name it as owner, fork source or
// requester.
func (f *fixture) workspaceInRepo(t *testing.T, id ids.WorkspaceID, repoDir string) wsm.Workspace {
	t.Helper()
	repo, found := wsm.RepositoryAt(f.db.repositories, repoDir)
	if !found {
		t.Fatalf("no repository is registered at %s", repoDir)
	}
	dir, err := normalizeDir(t.TempDir())
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	ws := wsm.Workspace{ID: id, Dir: dir, Repo: repo.ID, Name: string(id), Branch: "ABC/" + string(id)}
	f.db.with(ws)
	return ws
}

// stubTopbar satisfies the resolver seam the verbs hold but never
// call, so a test that does call one nil-panics rather than passing quietly.
type stubTopbar struct {
	topbar.Resolver
	// effortSettings records every effort settings read the verbs installed,
	// in order; warnings every warning-strip line they raised, by key.
	effortSettings *[]claudesettings.Effort
	warnings       *[]string
	// picked is the level the topbar holds as picked; nil holds none.
	picked *conversationv1.AgentEffortLevel
	// parked records every park state the verbs installed or lifted, in order.
	parked *[]bool
	// coldGates records every cold-gate state the verbs stated, in order.
	coldGates *[]topbar.ColdGate
	// accounts records every account cell the verbs installed, in order.
	accounts *[]topbar.Account
	// loginDefaults records every login default moved, as {root, model}.
	loginDefaults *[][2]string
	// links records every shown-links list the verbs installed, in order; an
	// empty one is a clear.
	links *[][]topbar.Link
	// names records every workspace name a rename installed, in order.
	names *[]string
	// window is the context window the strip answers; nil answers none and
	// nil-panics, as every seam a test did not wire does.
	window *int64
}

func (s stubTopbar) SetColdGate(_ ids.WorkspaceID, gate topbar.ColdGate) {
	if s.coldGates != nil {
		*s.coldGates = append(*s.coldGates, gate)
	}
}

func (s stubTopbar) ContextWindow(ids.WorkspaceID) int64 {
	return *s.window
}

func (s stubTopbar) SetParked(_ ids.WorkspaceID, park wsm.Park) {
	if s.parked != nil {
		*s.parked = append(*s.parked, park != wsm.ParkNone)
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
func (s stubTopbar) SetName(_ ids.WorkspaceID, name string) {
	if s.names != nil {
		*s.names = append(*s.names, name)
	}
}
func (s stubTopbar) SetLinks(_ ids.WorkspaceID, shown []topbar.Link) {
	if s.links != nil {
		*s.links = append(*s.links, slices.Clone(shown))
	}
}
func (s stubTopbar) SetLoginDefaultModel(configDir, model string) {
	if s.loginDefaults != nil {
		*s.loginDefaults = append(*s.loginDefaults, [2]string{configDir, model})
	}
}

func (s stubTopbar) SetAccount(_ ids.WorkspaceID, account topbar.Account) {
	if s.accounts != nil {
		*s.accounts = append(*s.accounts, account)
	}
}
func (s stubTopbar) SetEffortSettings(_ ids.WorkspaceID, settings claudesettings.Effort) {
	if s.effortSettings != nil {
		*s.effortSettings = append(*s.effortSettings, settings)
	}
}
func (s stubTopbar) PickedEffort(ids.WorkspaceID) (conversationv1.AgentEffortLevel, bool) {
	if s.picked == nil || *s.picked == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED, false
	}
	return *s.picked, true
}
func (s stubTopbar) SetPickedEffort(_ ids.WorkspaceID, level conversationv1.AgentEffortLevel) {
	if s.picked != nil {
		*s.picked = level
	}
}
func (s stubTopbar) RaiseWarning(_ ids.WorkspaceID, key, line string) {
	if s.warnings != nil {
		*s.warnings = append(*s.warnings, key+": "+line)
	}
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
	mu sync.Mutex
	// entered, when set, is sent on by each call before it answers, and the
	// call then waits for release: a test holds a create inside its naming
	// call.
	entered chan struct{}
	release chan struct{}
	answers []headlessAnswer
	// calls records every request, so a test can assert the model, the site
	// and the prompt the call was made with.
	calls []headless.Request
}

func (h *fakeHeadless) Bin() string { return "fake-claude" }

func (h *fakeHeadless) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	h.mu.Lock()
	entered, release := h.entered, h.release
	h.mu.Unlock()
	if entered != nil {
		entered <- struct{}{}
		<-release
	}
	h.mu.Lock()
	defer h.mu.Unlock()
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
	go func() { done <- f.verbs.Select(ctx, ws, RecordSelection) }()
	return done
}

// currentView is the recorded view, or the store's default.
func (d *fakeDB) currentView() wsm.SidebarView {
	if d.view == nil {
		return wsm.DefaultSidebarView
	}
	return *d.view
}

// SetMergedSectionFolded records the band's fold.
func (d *fakeDB) SetMergedSectionFolded(_ context.Context, folded bool) error {
	if d.setMergedFoldedErr != nil {
		return d.setMergedFoldedErr
	}
	view := d.currentView()
	view.MergedFolded = folded
	d.view = &view
	return nil
}

// SetHiddenSectionFolded records the hidden section's fold.
func (d *fakeDB) SetHiddenSectionFolded(_ context.Context, folded bool) error {
	if d.setHiddenFoldedErr != nil {
		return d.setHiddenFoldedErr
	}
	view := d.currentView()
	view.HiddenFolded = folded
	d.view = &view
	return nil
}

// SetOpenSectionFolded records the open section's fold.
func (d *fakeDB) SetOpenSectionFolded(_ context.Context, folded bool) error {
	if d.setOpenFoldedErr != nil {
		return d.setOpenFoldedErr
	}
	view := d.currentView()
	view.OpenFolded = folded
	d.view = &view
	return nil
}

// SidebarView answers the recorded view, or the store's default.
func (d *fakeDB) SidebarView(context.Context) (wsm.SidebarView, error) {
	if d.viewErr != nil {
		return wsm.SidebarView{}, d.viewErr
	}
	return d.currentView(), nil
}

// SetRepositoryFolded records the fold on the fake's repository row; an
// unknown repository is wsm.ErrNotFound, as the store answers it.
func (d *fakeDB) SetRepositoryFolded(_ context.Context, id ids.RepoID, folded bool) error {
	if d.setFoldedErr != nil {
		return d.setFoldedErr
	}
	for i := range d.repositories {
		if d.repositories[i].ID == id {
			d.repositories[i].Folded = folded
			return nil
		}
	}
	return fmt.Errorf("fake: repository %s: %w", id, wsm.ErrNotFound)
}

// SetVendorSessionID records the resume handle on the fake's session row and
// answers the one it replaced; setVendorErr fails it.
func (d *fakeDB) SetVendorSessionID(_ context.Context, id ids.WorkspaceID, vendorSessionID string) (string, error) {
	if d.setVendorErr != nil {
		return "", d.setVendorErr
	}
	session := d.sessions[id]
	replaced := session.VendorSessionID
	session.Workspace, session.VendorSessionID = id, vendorSessionID
	d.sessions[id] = session
	return replaced, nil
}

// SetSessionModel records the workspace's model on the fake's session row; a
// workspace with no row is wsm.ErrNotFound, as the store answers it.
func (d *fakeDB) SetSessionModel(_ context.Context, id ids.WorkspaceID, model string) error {
	session, ok := d.sessions[id]
	if !ok {
		return fmt.Errorf("fake: session for workspace %s: %w", id, wsm.ErrNotFound)
	}
	session.Model = model
	d.sessions[id] = session
	return nil
}

// SetAccount takes the session's account; the fake feed draws nothing from it.
func (f *fakeFeed) SetAccount(ids.WorkspaceID, string) {}

// standDownsFunc adapts a function to StandDowns.
type standDownsFunc func(ws ids.WorkspaceID)

func (f standDownsFunc) StandDownFolded(ws ids.WorkspaceID) { f(ws) }

// fakeSummaries records every workspace whose summary was restored, and
// answers err.
type fakeSummaries struct {
	restored []ids.WorkspaceID
	err      error
}

func (s *fakeSummaries) Restore(_ context.Context, ws ids.WorkspaceID) error {
	s.restored = append(s.restored, ws)
	return s.err
}

// fakeCacheLapses adapts two functions to CacheLapses.
type fakeCacheLapses struct {
	visibility func(ws ids.WorkspaceID)
	answered   func(ws ids.WorkspaceID) error
}

func (f *fakeCacheLapses) VisibilityChanged(ws ids.WorkspaceID) { f.visibility(ws) }

func (f *fakeCacheLapses) ColdGateAnswered(_ context.Context, ws ids.WorkspaceID) error {
	return f.answered(ws)
}

// OpenChildren answers PARENT's open children among the arranged records, in
// place order, as WSM does.
func (f *fakeDB) OpenChildren(_ context.Context, parent ids.WorkspaceID) ([]wsm.FamilyRef, error) {
	if f.openChildrenErr != nil {
		return nil, f.openChildrenErr
	}
	var children []wsm.Workspace
	for _, ws := range f.workspaces {
		if ws.Parent != nil && *ws.Parent == parent && ws.ID != parent && !ws.Closed && !ws.MergeHidden() {
			children = append(children, ws)
		}
	}
	slices.SortFunc(children, func(a, b wsm.Workspace) int { return cmp.Compare(a.Place, b.Place) })
	refs := make([]wsm.FamilyRef, len(children))
	for i, child := range children {
		refs[i] = wsm.FamilyRef{ID: child.ID, Dir: child.Dir}
	}
	return refs, nil
}

// RequireOpenParent answers openParentErr, recording the parent asked about.
func (f *fakeDB) RequireOpenParent(_ context.Context, parent ids.WorkspaceID) error {
	f.openParentAsked = append(f.openParentAsked, parent)
	return f.openParentErr
}

// errOpenChildren is a family rule refusing a parent with an open child.
var errOpenChildren = &wsm.HasOpenChildrenError{
	Parent:   wsm.FamilyRef{ID: "w1", Dir: "/w/w1"},
	Children: []wsm.FamilyRef{{ID: "w2", Dir: "/w/w2"}},
}

// errParentNotOpen is a family rule refusing a child of a closed parent.
var errParentNotOpen = &wsm.ParentNotOpenError{Parent: wsm.FamilyRef{ID: "parent", Dir: "/w/parent"}, State: wsm.ParentClosed}

// requireFamilyAnswer fails unless ERR is WANT, recorded under OPERATION at
// INFO and never at ERROR: a family refusal is an answer, not a failure.
func requireFamilyAnswer(t *testing.T, f *fixture, err, want error, operation string) {
	t.Helper()
	if !errors.Is(err, want) {
		t.Fatalf("error = %v, want the family refusal %v", err, want)
	}
	if loggedAtError(f, operation) {
		t.Fatalf("the family refusal was recorded at ERROR under %s", operation)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: operation}); !ok {
		t.Fatalf("the family refusal was not recorded at INFO under %s", operation)
	}
}

// SetSubsectionFolded records the fold on the fake's subsection, as WSM does;
// an unknown subsection is WSM's UnknownSubsectionError.
func (d *fakeDB) SetSubsectionFolded(_ context.Context, id ids.SubsectionID, folded bool) error {
	for i := range d.subsections {
		if d.subsections[i].ID == id {
			d.subsections[i].Folded = folded
			return nil
		}
	}
	return &wsm.UnknownSubsectionError{ID: id}
}

// ExpandSubsectionHolding expands the subsection the fake's workspace sits in,
// answering whether it was collapsed, as WSM does.
func (d *fakeDB) ExpandSubsectionHolding(_ context.Context, id ids.WorkspaceID) (bool, error) {
	ws := d.workspaces[id]
	if ws.Subsection == nil {
		return false, nil
	}
	for i := range d.subsections {
		if d.subsections[i].ID == *ws.Subsection && d.subsections[i].Folded {
			d.subsections[i].Folded = false
			return true, nil
		}
	}
	return false, nil
}

// fakeNetwork is the network state a failed naming call is classified by. It
// answers VERDICT to every classification and records the words it was asked
// about; restore closes the restored latch.
type fakeNetwork struct {
	mu         sync.Mutex
	verdict    netstate.Verdict
	classified []string
	restored   chan struct{}
}

func newFakeNetwork() *fakeNetwork { return &fakeNetwork{restored: make(chan struct{})} }

func (n *fakeNetwork) Classify(_ context.Context, words string) netstate.Verdict {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.classified = append(n.classified, words)
	return n.verdict
}

// Restored answers the latch, closed already while the network is up, as
// netstate.Monitor's is.
func (n *fakeNetwork) Restored() <-chan struct{} {
	n.mu.Lock()
	defer n.mu.Unlock()
	if !n.verdict.Network {
		up := make(chan struct{})
		close(up)
		return up
	}
	return n.restored
}

// signalRestored closes the latch while classifications still say the
// network is down: a restoration that does not last.
func (n *fakeNetwork) signalRestored() {
	n.mu.Lock()
	defer n.mu.Unlock()
	close(n.restored)
	n.restored = make(chan struct{})
}

// down makes every later classification say the network is down.
func (n *fakeNetwork) down(evidence netstate.Evidence) {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.verdict = netstate.Verdict{Network: true, Evidence: evidence, Detail: "the network is down"}
}

// restore says the network is back: the latch closes, the next one is armed,
// and classifications say it is up.
func (n *fakeNetwork) restore() {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.verdict = netstate.Verdict{}
	close(n.restored)
	n.restored = make(chan struct{})
}

func (n *fakeNetwork) classifications() []string {
	n.mu.Lock()
	defer n.mu.Unlock()
	return append([]string(nil), n.classified...)
}

// PutQueuedCreate holds a create in memory, or fails with queuedPutErr.
func (d *fakeDB) PutQueuedCreate(_ context.Context, q wsm.QueuedCreate) error {
	d.queuedMu.Lock()
	defer d.queuedMu.Unlock()
	if d.queuedPutErr != nil {
		return d.queuedPutErr
	}
	if d.queued == nil {
		d.queued = map[string]wsm.QueuedCreate{}
	}
	d.queued[q.Key] = q
	return nil
}

// QueuedCreates answers every held create, or fails with queuedReadErr.
func (d *fakeDB) QueuedCreates(context.Context) ([]wsm.QueuedCreate, error) {
	d.queuedMu.Lock()
	defer d.queuedMu.Unlock()
	if d.queuedReadErr != nil {
		return nil, d.queuedReadErr
	}
	out := make([]wsm.QueuedCreate, 0, len(d.queued))
	for _, q := range d.queued {
		out = append(out, q)
	}
	return out, nil
}

// DeleteQueuedCreate releases a held create.
func (d *fakeDB) DeleteQueuedCreate(_ context.Context, key string) error {
	d.queuedMu.Lock()
	defer d.queuedMu.Unlock()
	delete(d.queued, key)
	return nil
}

// heldCreates answers the held creates' rows.
func (d *fakeDB) heldCreates() []wsm.QueuedCreate {
	d.queuedMu.Lock()
	defer d.queuedMu.Unlock()
	out := make([]wsm.QueuedCreate, 0, len(d.queued))
	for _, q := range d.queued {
		out = append(out, q)
	}
	return out
}

// fakeStoreHistory is the store's read-only face: it answers every read with
// PAGE or ERR and records how it was asked.
type fakeStoreHistory struct {
	page  *conversationv1.HistoryPage
	err   error
	asked []string
}

func (s *fakeStoreHistory) Newest(_ context.Context, book string, stated bool) (*conversationv1.HistoryPage, error) {
	s.asked = append(s.asked, fmt.Sprintf("newest %s stated=%t", book, stated))
	return s.page, s.err
}

func (s *fakeStoreHistory) Before(_ context.Context, book string, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	s.asked = append(s.asked, fmt.Sprintf("before %s %s", book, after.GetValue()))
	return s.page, s.err
}

func (s *fakeStoreHistory) Through(_ context.Context, book string, throughMs int64) (*conversationv1.HistoryPage, error) {
	s.asked = append(s.asked, fmt.Sprintf("through %s %d", book, throughMs))
	return s.page, s.err
}

// fakeShimLife records the shim starts and stand-downs the fleet counts.
type fakeShimLife struct {
	mu         sync.Mutex
	starts     []string
	standDowns []string
}

func (f *fakeShimLife) ObserveShimStart(ws ids.WorkspaceID, cause, outcome string, _ time.Duration) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.starts = append(f.starts, string(ws)+" "+cause+" "+outcome)
}

func (f *fakeShimLife) CountShimStandDown(ws ids.WorkspaceID, reason string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.standDowns = append(f.standDowns, string(ws)+" "+reason)
}

// fakeGateAnswers records the cold gate answers the verbs count.
type fakeGateAnswers struct {
	mu      sync.Mutex
	answers []string
}

func (f *fakeGateAnswers) CountColdGateAnswer(ws ids.WorkspaceID, choice, outcome string) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.answers = append(f.answers, string(ws)+" "+choice+" "+outcome)
}

// ensureVisits answers the fixture's settings visits, made on first use.
func (f *fixture) ensureVisits() *SettingsVisits {
	if f.visits == nil {
		f.visits = NewSettingsVisits()
	}
	return f.visits
}

// ReviveForSettings records the ask and answers reviveErr.
func (q *fakeQueue) ReviveForSettings(_ context.Context, ws ids.WorkspaceID) error {
	q.settingsRevivals = append(q.settingsRevivals, ws)
	return q.reviveErr
}

// visitDown is one VisitDown asked of the drain.
type visitDown struct {
	WS    ids.WorkspaceID
	Since time.Time
}

// fakeVisitDrain records every settings visit's stand-down asked.
type fakeVisitDrain struct {
	drain.Controller
	visits []visitDown
}

// VisitDown records the ask.
func (d *fakeVisitDrain) VisitDown(ws ids.WorkspaceID, since time.Time) {
	d.visits = append(d.visits, visitDown{WS: ws, Since: since})
}
