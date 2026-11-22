package harness

import (
	"context"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/grantedclock"

	"connectrpc.com/connect"
)

// A VIEW IS ONE TEST'S DAEMON OVER A PROCESS THE TEST DOES NOT OWN.
//
// A fixture that costs more than the tests it serves (the e2e suite's World)
// may be started once and shared, under AGENTS.md "A case may share an
// expensive fixture, never a slice another case reads". The daemon that
// started the process is its OWNER; every test that uses it takes a View. A
// view shares the owner's process, client and locks, and carries its own
// test, run budget and expectations.
//
// WHAT A VIEW SWEEPS IS WHAT IT CLAIMED. Register claims a workspace for the
// view that registered it, so the view's warning sweep reads exactly the
// records that name a workspace it claimed, against its own ExpectWarnings.
// The owner's sweep reads every record no view claimed. Each record is swept
// once, by whoever answers for it, so one test's declared warning can never
// license another test's record.
//
// A RECORD WRITTEN AFTER ITS VIEW'S SWEEP IS STILL SWEPT. A daemon goes on
// logging about a workspace after the test that used it has ended (the
// shim's exit, the process's own shutdown). Each view leaves its sweep time
// and its expectations with the process, and the owner's final sweep checks
// every later record of a claimed workspace against the expectations of the
// view that claimed it, naming that view's test.

// claim is one view's answer for the workspaces it registered, kept on the
// process so the owner can sweep what the view could not see.
type claim struct {
	test     string
	dirs     map[string]bool
	expected map[string]bool
	sweptAt  time.Time
}

// View is this daemon as the test t sees it. It fails t, rather than the
// owner, for everything t asks of it; it is bounded by its own run budget,
// released when t ends; and its sweep answers for the workspaces t
// registered through it.
//
// A view may not stop or kill the process: a test that does either owns its
// own daemon. A view of a view is refused, so every view answers to the one
// owner.
func (d *Daemon) View(t testing.TB) *Daemon {
	t.Helper()
	if d.view {
		t.Fatalf("harness: View of a view; take every view from the daemon that owns the process")
	}
	v := *d
	v.t = t
	v.view = true
	v.expected = map[string]bool{}
	v.required = map[string]bool{}
	v.expectedFor = nil
	v.claims = map[string]bool{}
	// The repositories this test mints must be ones the shared daemon's git
	// can see, so the test's git world is the daemon's.
	bindWorld(t, d.Git)
	requireGrantedClock(t)
	ctx, cancel := grantedclock.WithBound(v.watchRecords(t, d.fault.ctx), v.waitBound*runBudgetWaits)
	// A stream the view opens settles against the view (Stream.settle), as
	// an owner's does against its owner.
	v.ctx = context.WithValue(ctx, daemonKey{}, &v)
	// Registered first so it runs last: the sweeps below read the logs, and
	// the budget must outlive them.
	t.Cleanup(cancel)
	t.Cleanup(v.leaveClaim)
	// The sweeps answer for what this view claimed, and A FAILED TEST KEEPS ITS
	// LOGS exactly as an owned daemon's does: the logs directory is the
	// shared process's, so it holds other tests' records beside this one's,
	// and each record names its workspace.
	registerLogChecks(t, func() { keepFailureLogs(t, v.StateDir, v.stderrPath) }, v.assertNoUnexpectedWarnings, v.assertNoUnproducedRequiredWarnings, v.countLogRecords)
	// Registered after the sweeps, so it runs before them: they judge every
	// record the retirement writes, as the test's own.
	t.Cleanup(v.retireSlice)
	return &v
}

// retireSlice takes every workspace the view registered out of the shared
// process when its test ends, latest first (a repository's main worktree is
// forgotten last, taking the repository with it): KillWorkspace, which
// closes it and ends its session whatever it was doing, then
// ForgetWorkspace, which removes its record. A workspace the test itself
// already removed is gone, which is what the retirement wants.
//
// A CASE'S SLICE ENDS WITH THE CASE. A shared process that kept every
// finished case's workspace held the whole suite's by its end, and its
// roster, recomputed whole on every change, cost more CPU than a fresh
// process per case (2026-11-20, batch 1 of the canonical daemon: 83.5 CPU-s
// against 68.3).
func (d *Daemon) retireSlice() {
	d.t.Helper()
	d.mu.Lock()
	slice := append([]*workspacev1.WorkspaceRef(nil), d.slice...)
	d.mu.Unlock()
	if len(slice) == 0 {
		return
	}
	// Read before anything is forgotten: a forgotten workspace no longer
	// names its repository.
	mains := d.unclaimedMainWorktrees(slice)
	for i := len(slice) - 1; i >= 0; i-- {
		d.retire(slice[i], true)
	}
	for _, main := range mains {
		d.retire(main, true)
	}
}

// unclaimedMainWorktrees answers the main worktree of every repository a
// workspace of SLICE is in, where that main worktree is not itself in SLICE:
// the one registering a linked worktree registers beside it, closed. The
// case minted the repository, so its main worktree is the case's too, and a
// main worktree left behind keeps its repository on every later case's
// sidebar.
func (d *Daemon) unclaimedMainWorktrees(slice []*workspacev1.WorkspaceRef) []*workspacev1.WorkspaceRef {
	d.t.Helper()
	return mainWorktreesBeside(d.everyWorkspace("retire the slice"), slice)
}

// mainWorktreesBeside is unclaimedMainWorktrees' choice among FOUND, every
// workspace the daemon holds.
func mainWorktreesBeside(found []*agentreplv1.FoundWorkspace, slice []*workspacev1.WorkspaceRef) []*workspacev1.WorkspaceRef {
	inSlice := make(map[string]bool, len(slice))
	for _, ws := range slice {
		inSlice[ws.GetId()] = true
	}
	repos := map[string]bool{}
	for _, w := range found {
		if inSlice[w.GetWorkspace().GetId()] {
			repos[canonicalDir(w.GetRepository().GetDir())] = true
		}
	}
	var mains []*workspacev1.WorkspaceRef
	for _, w := range found {
		repo := canonicalDir(w.GetRepository().GetDir())
		if repos[repo] && !inSlice[w.GetWorkspace().GetId()] && canonicalDir(w.GetWorkspace().GetDir()) == repo {
			mains = append(mains, w.GetWorkspace())
		}
	}
	return mains
}

// retire kills and forgets one workspace of the slice. A workspace with open
// children (a fork, a create the case made through the daemon) has them
// retired first, as the kill names them, and is killed again once.
func (d *Daemon) retire(ws *workspacev1.WorkspaceRef, childrenFirst bool) {
	d.t.Helper()
	kill, err := d.Client().KillWorkspace(d.ctx, connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ws}))
	if err != nil {
		d.t.Errorf("harness: retire %s from the shared daemon: KillWorkspace = error %v", ws.GetDir(), err)
		return
	}
	switch {
	case kill.Msg.GetError().GetUnknownWorkspace() != nil:
		return
	case childrenFirst && kill.Msg.GetError().GetHasOpenChildren() != nil:
		for _, child := range kill.Msg.GetError().GetHasOpenChildren().GetChildren() {
			d.retire(child, true)
		}
		d.retire(ws, false)
		return
	case kill.Msg.GetSuccess() == nil:
		d.t.Errorf("harness: retire %s from the shared daemon: KillWorkspace = %v, want a success", ws.GetDir(), kill.Msg)
		return
	}
	d.forgetRetired(ws, true)
}

// forgetRetired forgets a killed workspace of the slice. A forget blocked by
// held prompts (undelivered user intent the kill does not touch) drops them
// and forgets again, once; one whose closed children stand is forgotten after
// them, once; a repository's main worktree that other workspaces
// of its repository still stand beside (ones the case created through the
// daemon) has them retired first, and is forgotten again, once.
func (d *Daemon) forgetRetired(ws *workspacev1.WorkspaceRef, dropHeld bool) {
	d.t.Helper()
	forget, err := d.Client().ForgetWorkspace(d.ctx, connect.NewRequest(&agentreplv1.ForgetWorkspaceRequest{Workspace: ws}))
	if err != nil {
		d.t.Errorf("harness: retire %s from the shared daemon: ForgetWorkspace = error %v", ws.GetDir(), err)
		return
	}
	switch {
	case forget.Msg.GetSuccess() != nil, forget.Msg.GetError().GetUnknownWorkspace() != nil:
	case dropHeld && forget.Msg.GetError().GetBlocked().GetHeldPrompts() > 0:
		d.dropHeld(ws)
		d.forgetRetired(ws, false)
	case dropHeld && forget.Msg.GetError().GetHasChildren() != nil:
		// A CLOSED CHILD is no open child, so the kill went through, but a
		// forget would flatten its lineage: it is retired first.
		for _, child := range forget.Msg.GetError().GetHasChildren().GetChildren() {
			d.retire(&workspacev1.WorkspaceRef{Id: child}, true)
		}
		d.forgetRetired(ws, false)
	case dropHeld && forget.Msg.GetError().GetMainWorktree() != nil:
		for _, mate := range d.repositoryMates(ws) {
			d.retire(mate, true)
		}
		d.forgetRetired(ws, false)
	default:
		d.t.Errorf("harness: retire %s from the shared daemon: ForgetWorkspace = %v, want a success", ws.GetDir(), forget.Msg)
	}
}

// repositoryMates answers every other workspace registered under the
// repository whose main worktree MAIN is (FindWorkspaces), by its directory's
// canonical form.
func (d *Daemon) repositoryMates(main *workspacev1.WorkspaceRef) []*workspacev1.WorkspaceRef {
	d.t.Helper()
	found := d.everyWorkspace("retire " + main.GetDir())
	repo := canonicalDir(main.GetDir())
	var mates []*workspacev1.WorkspaceRef
	for _, w := range found {
		if w.GetWorkspace().GetId() != main.GetId() && canonicalDir(w.GetRepository().GetDir()) == repo {
			mates = append(mates, w.GetWorkspace())
		}
	}
	return mates
}

// everyWorkspace answers every workspace the daemon holds, in one answer: a
// partial one could omit a workspace the retirement must take. WHAT names the
// retirement step a failure is reported against.
func (d *Daemon) everyWorkspace(what string) []*agentreplv1.FoundWorkspace {
	d.t.Helper()
	limit := uint32(100000)
	resp, err := d.Client().FindWorkspaces(d.ctx, connect.NewRequest(&agentreplv1.FindWorkspacesRequest{Limit: &limit}))
	if err != nil || resp.Msg.GetSuccess().GetComplete() == nil {
		d.t.Errorf("harness: %s from the shared daemon: FindWorkspaces = (%v, %v), want every workspace in one answer", what, resp, err)
		return nil
	}
	return resp.Msg.GetSuccess().GetComplete().GetWorkspaces()
}

// canonicalDir is DIR with its symlinks resolved, or DIR as given when it no
// longer resolves (a worktree already removed).
func canonicalDir(dir string) string {
	if resolved, err := filepath.EvalSymlinks(dir); err == nil {
		return resolved
	}
	return filepath.Clean(dir)
}

// dropHeld drops every prompt the workspace's held section holds.
func (d *Daemon) dropHeld(ws *workspacev1.WorkspaceRef) {
	d.t.Helper()
	resp, err := d.Client().OpenFeed(d.ctx, connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		d.t.Errorf("harness: retire %s from the shared daemon: OpenFeed for its held section = (%v, %v)", ws.GetDir(), resp, err)
		return
	}
	for _, item := range resp.Msg.GetSuccess().GetHeld().GetItems() {
		prompt := item.GetPrompt()
		if prompt == nil {
			continue
		}
		drop, err := d.Client().UpdateHeldPrompt(d.ctx, connect.NewRequest(&agentreplv1.UpdateHeldPromptRequest{
			Workspace: ws, Turn: prompt.GetTurn(), Action: &agentreplv1.UpdateHeldPromptRequest_Drop{Drop: &agentreplv1.UpdateHeldPromptDrop{}},
		}))
		if err != nil || drop.Msg.GetSuccess() == nil {
			d.t.Errorf("harness: retire %s from the shared daemon: drop held prompt %s = (%v, %v)", ws.GetDir(), prompt.GetTurn().GetValue(), drop, err)
		}
	}
}

// refuseOnView fails a view's test that asks to end the shared process.
func (d *Daemon) refuseOnView(verb string) {
	d.t.Helper()
	if d.view {
		d.t.Fatalf("harness: %s on a view; the process is shared by the whole suite, and a test that ends its daemon owns its own", verb)
	}
}

// IsView reports whether d is a view of a daemon another fixture owns.
func (d *Daemon) IsView() bool { return d.view }

// Claim makes the workspace at dir this view's: its records are swept by this
// view, against this view's expectations, and by no one else. Register claims
// every workspace it registers through a view; Claim is for a workspace that
// reached the daemon some other way. Claiming on an owner is refused, since an
// owner already answers for every record no view claimed.
func (d *Daemon) Claim(dir string) {
	d.t.Helper()
	if !d.view {
		d.t.Fatalf("harness: Claim(%s) on the daemon that owns the process; only a view claims a workspace", dir)
	}
	forms := dirForms(d.t, dir)
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.claimed == nil {
		d.claimed = map[string]string{}
	}
	for _, form := range forms {
		if owner, ok := d.claimed[form]; ok && owner != d.t.Name() {
			d.t.Fatalf("harness: workspace %s is already claimed by %s; two tests never share a workspace", dir, owner)
		}
		d.claimed[form] = d.t.Name()
		d.claims[form] = true
	}
}

// claimID makes the workspace with this id this view's too, for the records
// that name a workspace by its id alone (recordKey). Register calls it beside
// Claim.
func (d *Daemon) claimID(id string) {
	d.t.Helper()
	key := idKey(id)
	d.mu.Lock()
	defer d.mu.Unlock()
	if owner, ok := d.claimed[key]; ok && owner != d.t.Name() {
		d.t.Fatalf("harness: workspace %s is already claimed by %s; two tests never share a workspace", id, owner)
	}
	d.claimed[key] = d.t.Name()
	d.claims[key] = true
}

// idKey is the claim key of a workspace named by its id alone.
func idKey(id string) string { return "id:" + id }

// recordKey is the claim key a record names its workspace by: its directory,
// or its id when it names no directory (a central record stamped with the
// workspace's id alone, such as a desktop banner's). A record naming neither
// is no workspace's.
func recordKey(r LogRecord) string {
	switch {
	case r.WorkspaceDir != "":
		return r.WorkspaceDir
	case r.WorkspaceID != "":
		return idKey(r.WorkspaceID)
	}
	return ""
}

// Owns reports whether r is a record this Daemon answers for: a view's, one
// naming a workspace it claimed; the owner's, every record. A case on a
// shared daemon reads the run log through it, so another case's record never
// satisfies its wait.
func (d *Daemon) Owns(r LogRecord) bool {
	if !d.view {
		return true
	}
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.claims[recordKey(r)]
}

// ExpectWarningsFor declares the operations whose WARN or ERROR records the
// workspace ws is meant to produce. Unlike ExpectWarnings, it licenses them
// for that workspace's records only, so a test with several workspaces states
// which one each warning belongs to.
func (d *Daemon) ExpectWarningsFor(ws *workspacev1.WorkspaceRef, operations ...string) {
	d.t.Helper()
	forms := append(dirForms(d.t, ws.GetDir()), idKey(ws.GetId()))
	d.mu.Lock()
	defer d.mu.Unlock()
	if d.expectedFor == nil {
		d.expectedFor = map[string]map[string]bool{}
	}
	for _, form := range forms {
		ops := d.expectedFor[form]
		if ops == nil {
			ops = map[string]bool{}
			d.expectedFor[form] = ops
		}
		for _, op := range operations {
			ops[op] = true
		}
	}
}

// dirForms is a workspace directory as the daemon may write it: as given, and
// with its symlinks resolved (macOS's /var is /private/var). The directory
// must exist: a claim or a declaration names a workspace the test just made.
func dirForms(t testing.TB, dir string) []string {
	t.Helper()
	if dir == "" {
		t.Fatalf("harness: a workspace with no directory cannot be claimed or declared for")
	}
	resolved, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatalf("harness: resolve workspace dir %s: %v", dir, err)
	}
	if resolved == dir {
		return []string{dir}
	}
	return []string{dir, resolved}
}

// leaveClaim hands this view's claim to the process, stamped with the moment
// its own sweep ran, so the owner's final sweep can check what came after.
func (d *Daemon) leaveClaim() {
	d.mu.Lock()
	defer d.mu.Unlock()
	if len(d.claims) == 0 {
		return
	}
	expected := make(map[string]bool, len(d.expected))
	for op := range d.expected {
		expected[op] = true
	}
	for dir, ops := range d.expectedFor {
		if d.claims[dir] {
			for op := range ops {
				expected[op] = true
			}
		}
	}
	dirs := make(map[string]bool, len(d.claims))
	for dir := range d.claims {
		dirs[dir] = true
	}
	d.leftClaims = append(d.leftClaims, claim{test: d.t.Name(), dirs: dirs, expected: expected, sweptAt: time.Now()})
}

// sweptRecords is every record of this process that this Daemon's sweep
// answers for: a view's claimed workspaces, or, for the owner, every record
// no view claimed.
func (d *Daemon) sweptRecords() []LogRecord {
	d.t.Helper()
	all := append(d.RunLog(), d.WorkspaceLogRecords()...)
	d.mu.Lock()
	defer d.mu.Unlock()
	out := make([]LogRecord, 0, len(all))
	for _, r := range all {
		key := recordKey(r)
		_, claimed := d.claimed[key]
		if d.view && d.claims[key] || !d.view && !claimed {
			out = append(out, r)
		}
	}
	return out
}

// expects reports whether r is a record this Daemon's declarations license.
// The caller holds d.mu.
func (d *Daemon) expects(r LogRecord) bool {
	return d.expected[r.Operation] || d.expectedFor[recordKey(r)][r.Operation]
}

// lateClaimedWarnings is every WARN or worse record a view claimed that was
// written after that view's own sweep and that its expectations do not
// license, each named with the view's test. Only the owner asks.
func (d *Daemon) lateClaimedWarnings() []string {
	d.t.Helper()
	all := append(d.RunLog(), d.WorkspaceLogRecords()...)
	d.mu.Lock()
	claims := append([]claim(nil), d.leftClaims...)
	d.mu.Unlock()
	var out []string
	for _, r := range all {
		if !warningLevels[strings.ToLower(r.Level)] {
			continue
		}
		for _, c := range claims {
			if !c.dirs[recordKey(r)] || c.expected[r.Operation] {
				continue
			}
			at, err := time.Parse(time.RFC3339Nano, r.Timestamp)
			if err != nil {
				d.t.Errorf("harness: record %q carries an unparseable timestamp %q: %v", r.Operation, r.Timestamp, err)
				continue
			}
			if at.After(c.sweptAt) {
				out = append(out, c.test+": "+r.Level+" "+r.Operation+": "+r.Message+contextSuffix(r))
			}
		}
	}
	sort.Strings(out)
	return out
}
