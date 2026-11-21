//go:build integration

package integration

import (
	"sync"
	"testing"

	"agent-repld/integration/harness"
)

// THE CANONICAL DAEMON AND ITS GROUPS: one daemon per test process, which
// every case shares on its own slice (its own repository and workspace), and
// one daemon per process-wide precondition a case changes, per AGENTS.md
// "Integration tests share one canonical process, each case on its own
// slice". The suite's mechanics are daemon/AGENTS.md "The integration suite's
// canonical daemon". A case that keeps a daemon of its own says at its
// fixture which of that section's reasons it has.

// sharedDaemon is one daemon the suite starts once and how its start ended.
type sharedDaemon struct {
	once   sync.Once
	daemon *harness.Daemon
	fatal  *harness.SuiteFatal
	skip   *harness.SuiteSkip
	// toggle runs the cases that change the group's precondition one at a
	// time; the canonical daemon's is never taken.
	toggle sync.Mutex
}

// owner answers the daemon, started on the suite at the first call. A start
// that failed fails every case that asks; a precondition it lacked skips
// every case that asks, with the same reason.
func (s *sharedDaemon) owner(t *testing.T, what string) *harness.Daemon {
	t.Helper()
	s.once.Do(func() {
		s.fatal, s.skip = harness.RecoverSuiteBuild(func() { s.daemon = harness.StartDaemon(harness.Suite(), harness.Opts{}) })
	})
	switch {
	case s.fatal != nil:
		t.Fatalf("integration: %s did not start: %s", what, s.fatal.Message)
	case s.skip != nil:
		t.Skipf("integration: %s did not start: %s", what, s.skip.Reason)
	}
	return s.daemon
}

var theCanonicalDaemon sharedDaemon

// canonicalOwner answers the suite's canonical daemon. A case never uses the
// owner itself: it takes a view (canonicalView, newCanonical,
// newCanonicalRegistered).
func canonicalOwner(t *testing.T) *harness.Daemon {
	t.Helper()
	return theCanonicalDaemon.owner(t, "the canonical daemon")
}

// canonicalView is t's view of the canonical daemon, for a case that
// registers its slice itself.
func canonicalView(t *testing.T) *harness.Daemon {
	t.Helper()
	return canonicalOwner(t).View(t)
}

// newCanonical is newOpened on the canonical daemon: a fresh repository and
// opened workspace that are t's own.
func newCanonical(t *testing.T) *fixture {
	t.Helper()
	return newOpenedOn(t, canonicalOwner(t))
}

// newCanonicalRegistered is newRegistered on the canonical daemon: a fresh
// repository's worktree registered as t's own, with no session opened.
func newCanonicalRegistered(t *testing.T) *fixture {
	t.Helper()
	d := canonicalView(t)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	return &fixture{d: d, repo: repo, ws: ws, t: t}
}

// A daemon group is the daemon of one process-wide precondition: every case
// that changes it shares the group's daemon, one at a time, and never the
// canonical daemon, whose other cases' contracts the change would move.
type daemonGroup string

const (
	// groupShutdownSchedule is the daemon whose shutdown schedule a case
	// sets or cancels: a schedule holds every workspace's intake.
	groupShutdownSchedule daemonGroup = "the shutdown schedule"
	// groupBrowserLauncher is the daemon whose external browser launcher a
	// case makes fail: the launcher is the daemon's one stub.
	groupBrowserLauncher daemonGroup = "the browser launcher's exit code"
	// groupAccountUsage is the daemon whose account usage a case moves with
	// a rate-limit event: every workspace spends as the daemon's one
	// account, whose allowances (and a refusal's block) are the account's.
	groupAccountUsage daemonGroup = "the account's usage"
	// groupSelection is the daemon whose selected workspace a case changes:
	// the selection is one per daemon, and every roster says which it is.
	groupSelection daemonGroup = "the selected workspace"
	// groupUnservedCatalog is the daemon whose login has never served a model
	// catalog: a workspace with no shim draws the catalog its login last
	// served, and every case's workspace signs in as the daemon's one login.
	// No case of the group starts a session.
	groupUnservedCatalog daemonGroup = "a login that never served a model catalog"
	// groupHiddenKeepalives is the daemon whose hidden workspaces a case
	// keeps alive: SendHiddenKeepalives asks every hidden shim of the
	// daemon, so one case's send reaches every other's.
	groupHiddenKeepalives daemonGroup = "the hidden workspaces' keep-alives"
	// groupTabOrder is the daemon whose sidebar a case arranges or reads
	// whole: tab numbers run across every repository the daemon holds, and
	// the arrangement lists every one of them.
	groupTabOrder daemonGroup = "the sidebar's tab order"
	// groupDesktopBanner is the daemon whose desktop banners a case raises or
	// reads: whether a banner shows turns on the one editor's focus, and a
	// focus gain claims the newest banner held for any workspace.
	groupDesktopBanner daemonGroup = "the desktop banner"
	// groupLoginTerminal is the daemon whose login terminal a case opens:
	// there is one per account root, and every case's workspace signs in as
	// the same default account.
	groupLoginTerminal daemonGroup = "the login terminal"
	// groupDefaultModel is the daemon whose login's default model a case
	// moves: every workspace signs in as the one login, and its topbar
	// offers that login's default.
	groupDefaultModel daemonGroup = "the login's default model"
	// groupEditorSession is the daemon an Emacs stream connects to as a new
	// editor: agent-repl's session is begun by the editor, one per daemon.
	groupEditorSession daemonGroup = "the editor's session"
)

var (
	groupsMu sync.Mutex
	groups   = map[daemonGroup]*sharedDaemon{}
)

// newInGroup is newCanonical on GROUP's daemon, holding the group's toggle
// for t's whole life, so no two cases change the group's precondition at
// once.
func newInGroup(t *testing.T, group daemonGroup) *fixture {
	t.Helper()
	return newOpenedOn(t, groupOwner(t, group))
}

// newRegisteredInGroup is newCanonicalRegistered on GROUP's daemon, holding
// the group's toggle as newInGroup does.
func newRegisteredInGroup(t *testing.T, group daemonGroup) *fixture {
	t.Helper()
	d := viewInGroup(t, group)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	return &fixture{d: d, repo: repo, ws: ws, t: t}
}

// viewInGroup is t's view of GROUP's daemon, for a case that registers its
// slice itself, holding the group's toggle as newInGroup does.
func viewInGroup(t *testing.T, group daemonGroup) *harness.Daemon {
	t.Helper()
	return groupOwner(t, group).View(t)
}

// groupOwner answers GROUP's daemon, started at its first case, and takes the
// group's toggle for t's whole life.
func groupOwner(t *testing.T, group daemonGroup) *harness.Daemon {
	t.Helper()
	groupsMu.Lock()
	g, ok := groups[group]
	if !ok {
		g = &sharedDaemon{}
		groups[group] = g
	}
	groupsMu.Unlock()
	owner := g.owner(t, "the daemon of "+string(group))
	g.toggle.Lock()
	// Registered first, so it runs last: the case's own cleanups end its
	// slice before the next case of the group starts.
	t.Cleanup(g.toggle.Unlock)
	return owner
}

// A CASE THAT CREATES A CHILD OR A FORK KEEPS A DAEMON OF ITS OWN until the
// owner rules on e34f0cb693 ("a close or forget is blocked only by holds the
// held section draws"). The derived workspace's notice to its source is held
// while the source revives, and the held section draws no card for it, so a
// slice's retirement can neither see nor drop it, and its forget stays
// blocked. Once the ruling lands, these share the canonical daemon again.

// newDerivingDaemon is a daemon of its own for a case that registers its
// slice itself and creates a child or a fork from it.
func newDerivingDaemon(t *testing.T) *harness.Daemon {
	t.Helper()
	return newDaemon(t, harness.Opts{})
}

// newDerivingOpened is newOpened on a daemon of its own, for a case whose
// opened workspace creates a child or a fork.
func newDerivingOpened(t *testing.T) *fixture {
	t.Helper()
	return newOpened(t, harness.Opts{})
}
