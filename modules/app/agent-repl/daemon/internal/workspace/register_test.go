package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// worktreeDir makes a directory that looks like a git worktree to the
// registration check, which is all Register stats.
func worktreeDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	normalized, err := normalizeDir(dir)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	return normalized
}

func TestRegisterRefusesADirectoryThatIsNotAWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.Register(context.Background(), t.TempDir(), wsm.RegisterFacts{})

	// Assert.
	asRefusal(t, err, ArmNotAWorktree)
}

func TestRegisterIsIdempotentByDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)

	// Act.
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	second, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("second Register: %v", err)
	}

	// Assert.
	if first.ID != second.ID {
		t.Fatalf("Register minted %q then %q, want one identity", first.ID, second.ID)
	}
}

// TestRegisterDerivesTheRepositoryFromGitsMainWorktree covers what a
// repository IS to the contract: RepositoryRef.dir is "the repository's
// normalized main-worktree directory", and it is what a top-level workspace's
// merge targets -- never the common dir, which no git verb should be aimed at.
func TestRegisterDerivesTheRepositoryFromGitsMainWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.mainWorktree = "/canonical/repo"
	f.git.commonDir = "/canonical/repo/.git"

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 1 || f.db.registered[0].RepoDir != "/canonical/repo" {
		t.Fatalf("registered facts = %+v, want the repository's main worktree", f.db.registered)
	}
}

func TestRegisterDerivesTheBranchFromGit(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.currentBranch = "ABC/derived"

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if f.db.registered[0].Branch != "ABC/derived" {
		t.Fatalf("registered branch = %q, want ABC/derived", f.db.registered[0].Branch)
	}
}

func TestRegisterDerivesTheParentBranchFromTheRepositoryDefault(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.defaultBranch = "main"

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if f.db.registered[0].ParentBranch != "main" {
		t.Fatalf("registered parent branch = %q, want main", f.db.registered[0].ParentBranch)
	}
}

func TestRegisterKeepsTheSuppliedFacts(t *testing.T) {
	// Arrange: supplied facts are never re-derived, because the announcer knows
	// its own tree.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{
		Name: "given", Branch: "given-branch", ParentBranch: "given-parent", RepoDir: "/given",
	}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	got := f.db.registered[0]
	if got.Name != "given" || got.Branch != "given-branch" || got.ParentBranch != "given-parent" || got.RepoDir != "/given" {
		t.Fatalf("registered facts = %+v, want the supplied ones kept", got)
	}
}

func TestRegisterDerivesTheDisplayNameFromTheDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if f.db.registered[0].Name != filepath.Base(dir) {
		t.Fatalf("registered name = %q, want %q", f.db.registered[0].Name, filepath.Base(dir))
	}
}

func TestRegisterSurfacesTheCommonDirFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.commonDirErr = errors.New("not a repository")

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	if err == nil {
		t.Fatal("Register() = nil error, want the git failure surfaced")
	}
}

func TestRegisterRepublishesTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

// TestPublishRegistryPublishesTheEmptyRoster covers the opening truth a booted
// daemon owes its first client: an empty roster is a roster, and nothing else
// publishes one until a verb happens to run.
func TestPublishRegistryPublishesTheEmptyRoster(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("SetRegistry calls = %d, want exactly one opening publication", len(f.sidebar.registries))
	}
}

// TestPublishRegistryCarriesEachWorkspacesSessionRecord pins the roster's
// durable session half. sidebar.Registry.Sessions is what lets the roster tell
// a PARK from a fault — a session carrying the idle sweep's `hibernated`
// terminal keeps an idle arm rather than the link's `dead` — and for as long
// as nothing populated it, every one of those answers was resolved from a nil
// record and a parked workspace was painted as broken.
func TestPublishRegistryCarriesEachWorkspacesSessionRecord(t *testing.T) {
	// Arrange
	f := newFixture(t)
	record, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.db.sessions[record.ID] = wsm.Session{
		Workspace: record.ID,
		Terminal:  &wsm.SessionTerminal{Kind: "hibernated"},
	}

	// Act
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert
	published := f.sidebar.registries[len(f.sidebar.registries)-1]
	if len(published.Sessions) != 1 {
		t.Fatalf("published sessions = %+v, want the one registered workspace's record", published.Sessions)
	}
	if got := published.Sessions[0].Terminal; got == nil || got.Kind != "hibernated" {
		t.Fatalf("published session terminal = %+v, want the hibernated stand-down the roster reads", got)
	}
}

// TestPublishRegistryCarriesNoRecordForASessionlessWorkspace is the other
// half: an absent record is the roster's `none` assertion, so a workspace that
// has never had a session must contribute nothing rather than a zero record
// that would read as a session.
func TestPublishRegistryCarriesNoRecordForASessionlessWorkspace(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Act
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert
	published := f.sidebar.registries[len(f.sidebar.registries)-1]
	if len(published.Sessions) != 0 {
		t.Fatalf("published sessions = %+v, want none for a workspace that never had one", published.Sessions)
	}
}

// TestPublishRegistryRefusesWhenASessionRecordCannotBeRead keeps the read on
// the same footing as the roster's other durable reads: a roster published
// from records the daemon could not read would assert `none` for workspaces
// whose sessions it simply failed to see.
func TestPublishRegistryRefusesWhenASessionRecordCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.db.sessionErr = errors.New("the state client is closed")
	before := len(f.sidebar.registries)

	// Act
	err := f.verbs.PublishRegistry(context.Background())

	// Assert
	if err == nil {
		t.Fatalf("PublishRegistry = nil, want the session read's failure surfaced")
	}
	if len(f.sidebar.registries) != before {
		t.Fatalf("roster publications = %d, want no roster published from records that could not be read", len(f.sidebar.registries))
	}
}

// registeredWithAConversation registers a directory once and files the
// conversation the first daemon recorded for it, so a SECOND registration of
// the same directory is the announcement a relaunched daemon receives.
func registeredWithAConversation(t *testing.T, f *fixture, terminal *wsm.SessionTerminal) (string, wsm.Workspace) {
	t.Helper()
	dir := worktreeDir(t)
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("the first Register: %v", err)
	}
	f.db.sessions[record.ID] = wsm.Session{
		Workspace:       record.ID,
		HostSessionID:   "host-1",
		VendorSessionID: "vendor-1",
		Terminal:        terminal,
	}
	f.fleet.started = nil
	delete(f.fleet.live, record.ID)
	return dir, record
}

// TestRegisterRevivesAKnownWorkspacesRecordedConversation is THE RELAUNCH.
// Emacs re-announces every workspace it holds when the link comes up, and for
// a workspace whose panel was already mounted that announcement is the only
// edge a fresh daemon gets — no OpenWorkspace follows it. Without the revival
// the session never comes up, no watcher opens, and the feed serves nothing
// for a conversation the store still holds whole.
func TestRegisterRevivesAKnownWorkspacesRecordedConversation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, record := registeredWithAConversation(t, f, nil)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 1 || got[0] != record.ID {
		t.Fatalf("sessions started by the re-announcement = %v, want the known workspace %q brought back up", got, record.ID)
	}
}

// TestRegisterLeavesALiveSessionAlone is the OTHER half of the relaunch rule.
// The revival exists for a daemon that came back to a conversation nobody was
// serving; a re-announcement of a workspace whose session is ALREADY UP on
// THIS daemon is a no-op for the session. Emacs re-announces on every link-up,
// so a revival that did not check would roll a live shim out from under a
// mounted panel mid-turn -- which reads to the user as a restart nothing asked
// for.
func TestRegisterLeavesALiveSessionAlone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, record := registeredWithAConversation(t, f, nil)
	f.fleet.live[record.ID] = true

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started by a re-announcement of a LIVE workspace = %v, want none: the live session was rolled", got)
	}
}

// TestRegisterSpawnsNothingForAFirstAnnouncement holds SPAWN ON MOUNT: a
// directory this daemon has never seen has no conversation to lose, so its
// announcement mints a roster row and nothing else. A registration that
// spawned would put a shim behind every workspace Emacs happens to know about.
func TestRegisterSpawnsNothingForAFirstAnnouncement(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started by a first announcement = %v, want none", got)
	}
}

// TestRegisterSpawnsNothingForAWorkspaceThatNeverHadAConversation covers the
// known workspace with a session row but no vendor id: there is no
// conversation to resume, and starting one FRESH is exactly the abandonment
// the fresh-conversation invariant forbids being done unasked.
func TestRegisterSpawnsNothingForAWorkspaceThatNeverHadAConversation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, record := registeredWithAConversation(t, f, nil)
	f.db.sessions[record.ID] = wsm.Session{Workspace: record.ID, HostSessionID: "host-1"}

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started for a workspace with no conversation = %v, want none", got)
	}
}

// TestRegisterDoesNotReviveADeletedSession covers the one death that refuses
// resurrection: an announcement must not resurrect what a delete ended.
func TestRegisterDoesNotReviveADeletedSession(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, _ := registeredWithAConversation(t, f, &wsm.SessionTerminal{Kind: "deleted", Detail: "NukeWorkspace"})

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started for a deleted session = %v, want none", got)
	}
}

// TestRegisterDoesNotReviveAHibernatedSession covers the deliberate
// stand-down: the idle sweep parked this session on purpose and a PROMPT is
// what revives it, so an announcement must not undo the policy.
func TestRegisterDoesNotReviveAHibernatedSession(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, _ := registeredWithAConversation(t, f, &wsm.SessionTerminal{Kind: "hibernated", Detail: "idle past the cutoff"})

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}

	// Assert.
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started for a hibernated session = %v, want none", got)
	}
}

// TestRegisterStillAnswersWhenTheRevivalFails covers the announcement's own
// contract: the roster row is durable and must land whatever the bring-up
// does, and the failure is recorded rather than absorbed.
func TestRegisterStillAnswersWhenTheRevivalFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, record := registeredWithAConversation(t, f, nil)
	f.fleet.startErr = errors.New("the shim would not spawn")

	// Act.
	again, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})

	// Assert.
	if err != nil {
		t.Fatalf("Register = %v, want the roster row despite a failed revival", err)
	}
	if again.ID != record.ID {
		t.Fatalf("Register = %q, want the known workspace %q", again.ID, record.ID)
	}
	recorded := false
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && r.Operation == opRegister {
			recorded = true
		}
	}
	if !recorded {
		t.Fatalf("records = %+v, want the failed revival recorded at error", f.log.logger.Records())
	}
}

// TestPublishRegistryClosesAWorkspaceWhoseDirectoryIsGone pins the owner's
// ruling on the roster walk. The boot reconciliation closes these rows, but a
// JOINING SUCCESSOR reconciles nothing, so this walk -- the one that publishes
// the opening roster -- is what keeps a successor from handing Emacs a live
// row for a directory that is not there.
func TestPublishRegistryClosesAWorkspaceWhoseDirectoryIsGone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert.
	if !f.db.closedFlags[record.ID] {
		t.Fatalf("workspace %v is still open; a directory that is gone must close the row", record.ID)
	}
}

// TestPublishRegistryPushesTheMissingDirectoryRowAsClosed is the frontend's
// half: the roster Emacs receives must already carry the row as closed, or it
// opens a tab on a path that is not there.
func TestPublishRegistryPushesTheMissingDirectoryRowAsClosed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert.
	published := f.sidebar.registries[len(f.sidebar.registries)-1]
	var found bool
	for _, ws := range published.Workspaces {
		if ws.ID != record.ID {
			continue
		}
		found = true
		if !ws.Closed {
			t.Fatalf("the published row for %v is open; the roster must carry it closed", record.ID)
		}
	}
	if !found {
		t.Fatalf("the published roster has no row for %v at all", record.ID)
	}
}

// TestPublishRegistryLeavesAPresentDirectoryOpen is the negative arm: the walk
// closes nothing it was not asked to.
func TestPublishRegistryLeavesAPresentDirectoryOpen(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	record, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Act.
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert.
	if f.db.closedFlags[record.ID] {
		t.Fatalf("workspace %v was closed though its directory is there", record.ID)
	}
}

// TestRegisterReopensAClosedWorkspace pins the fix for a register that
// produced a workspace with NO TAB.
//
// Registration is idempotent by dir, and the row it answers with is the row
// that is already there. When a previous CLOSE had marked that row closed,
// handing it back untouched left the roster carrying `closed = true`, which is
// the editor's whole tab-membership rule: no tab was drawn, the minted ref's
// landing waited on a tab that never came, and the workspace could not be
// resolved by name to close it again.
func TestRegisterReopensAClosedWorkspace(t *testing.T) {
	// Arrange: a directory whose registry row is already closed.
	f := newFixture(t)
	dir := worktreeDir(t)
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	closed := first
	closed.Closed = true
	f.db.with(closed)

	// Act.
	again, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("second Register: %v", err)
	}

	// Assert.
	if again.Closed {
		t.Fatalf("Register answered workspace %v still closed; announcing a directory must re-open it", again.ID)
	}
}

// TestRegisterClearsTheClosedFlagOnTheRecord is the durable half: the answer
// being open is worth nothing if the row it came from stays closed, because
// the roster is published from the row.
func TestRegisterClearsTheClosedFlagOnTheRecord(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	closed := first
	closed.Closed = true
	f.db.with(closed)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("second Register: %v", err)
	}

	// Assert.
	if f.db.closedFlags[first.ID] {
		t.Fatalf("workspace %v is still recorded closed after being announced again", first.ID)
	}
}

// TestRegisterWritesNoClosedFlagForAWorkspaceThatIsAlreadyOpen is the negative
// arm: the re-open is a repair of one state, not a write every announcement
// makes. The link-up walk re-announces every held workspace on every
// reconnect.
func TestRegisterWritesNoClosedFlagForAWorkspaceThatIsAlreadyOpen(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("second Register: %v", err)
	}

	// Assert.
	if _, written := f.db.closedFlags[first.ID]; written {
		t.Fatalf("announcing the open workspace %v wrote its closed flag; nothing needed repairing", first.ID)
	}
}

// TestRegisterSurfacesAFailedReopen keeps the failure path loud: a re-open
// that did not land would answer an open workspace over a row that is still
// closed, and the tab would never come.
func TestRegisterSurfacesAFailedReopen(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	closed := first
	closed.Closed = true
	f.db.with(closed)
	f.db.setClosedErr = errors.New("the registry is read-only")

	// Act.
	_, err = f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})

	// Assert.
	if err == nil {
		t.Fatalf("Register answered success though the re-open failed")
	}
}

// TestRegisterLevelsACancelledRevivalAtInfo is the other side of the test
// above: a bring-up that ended because the DAEMON'S OWN CONTEXT was cancelled
// is the process leaving, not a session that failed to come up. Nothing is
// left to serve the revived conversation, and the next boot revives it again.
func TestRegisterLevelsACancelledRevivalAtInfo(t *testing.T) {
	tests := []struct {
		name      string
		startErr  error
		wantLevel string
	}{
		{
			name:      "the bring-up was cancelled",
			startErr:  context.Canceled,
			wantLevel: "info",
		},
		{
			name:      "the shim would not spawn",
			startErr:  errors.New("the shim would not spawn"),
			wantLevel: "error",
		},
		{
			// The same answer reached from the other side: the bring-up
			// refused before it spawned because this daemon is standing down.
			// Measured at realtest 2026-09-13T18:32:16, where it was recorded
			// as a conversation that did not come back up.
			name:      "this daemon is standing down",
			startErr:  shimclient.ErrStandingDown,
			wantLevel: "info",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			dir, _ := registeredWithAConversation(t, f, nil)
			f.fleet.startErr = tt.startErr

			// Act.
			if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
				t.Fatalf("Register = %v, want the roster row despite the revival", err)
			}

			// Assert.
			var level string
			for _, r := range f.log.logger.Records() {
				if r.Operation == opRegister && strings.Contains(r.Message, "revival") ||
					r.Operation == opRegister && strings.Contains(r.Message, "did not come back up") {
					level = r.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("the revival record is %q, want %q: %+v", level, tt.wantLevel, f.log.logger.Records())
			}
		})
	}
}

// TestRegisterAnswersWhileTheRevivalIsStillStarting is the whole point of
// detaching the revival: the announcement's answer is the ROSTER ROW, and the
// row is written before the session start is anywhere near done.
//
// MEASURED, realtest run 2026-09-13T16:20:34. The revival ran inline, so the
// register waited on `Sessions.Start` — which takes the workspace's start gate
// and therefore waited on the boot's own bring-up of the SAME workspace, whose
// StartSession the shim never answered. Emacs abandoned RegisterWorkspace at
// its 10s bound on three daemon generations running and reported
// `link-up-register-failed`, then `call-on-closed-connection SelectWorkspace`,
// for a workspace whose row the daemon had already written and could have
// answered from immediately.
func TestRegisterAnswersWhileTheRevivalIsStillStarting(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir, record := registeredWithAConversation(t, f, nil)
	entered := make(chan struct{})
	release := make(chan struct{})
	t.Cleanup(func() { close(release) })
	f.fleet.detachEntered = entered
	f.fleet.detachHold = release

	// Act.
	answered, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})

	// Assert.
	if err != nil {
		t.Fatalf("the re-announcement: %v", err)
	}
	if answered.ID != record.ID {
		t.Fatalf("Register answered %q, want the known workspace %q", answered.ID, record.ID)
	}
	<-entered
}

// TestRegisterPrimesTheFooter pins the reconnect fix's wiring: registration —
// the edge every workspace passes through when the client reconnects (Emacs
// re-announces every workspace it holds) — primes the per-workspace footer
// topic, so a reconnecting subscriber replays a current view even for an idle
// session that produces no fresh live edge. Unlike the global roster, the
// footer topic is per-workspace and empty after a daemon restart, so this
// prime is what keeps it current.
func TestRegisterPrimesTheFooter(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)

	// Act.
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if !slices.Contains(f.footer.primed, record.ID) {
		t.Fatalf("footer was not primed for %q at registration; primed = %v", record.ID, f.footer.primed)
	}
}

func TestRegisterBindsTheFooterToTheAccountRoot(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.account.configDir = "/config-work"

	// Act.
	record, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if got := f.footer.accounts[record.ID]; !slices.Equal(got, []string{"/config-work"}) {
		t.Fatalf("footer account roots for %q = %v, want the root the session spends from", record.ID, got)
	}
}

// TestBindViewsBindsAnInheritedWorkspacesDirectory pins the boot's first step:
// a restarted daemon never ran Register for the rows it inherited, and the
// reconciliation publishes faults for them before PublishRegistry runs.
func TestBindViewsBindsAnInheritedWorkspacesDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	record, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.footer.dirs = nil

	// Act.
	if err := f.verbs.BindViews(context.Background()); err != nil {
		t.Fatalf("BindViews: %v", err)
	}

	// Assert.
	if f.footer.dirs[record.ID] != record.Dir {
		t.Fatalf("footer bound %q for %v, want %q", f.footer.dirs[record.ID], record.ID, record.Dir)
	}
}

func TestBindViewsPassesOverAWorkspaceWhoseDirectoryIsGone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}
	f.footer.dirs = nil

	// Act.
	if err := f.verbs.BindViews(context.Background()); err != nil {
		t.Fatalf("BindViews: %v", err)
	}

	// Assert.
	if _, bound := f.footer.dirs[record.ID]; bound {
		t.Fatalf("a workspace whose directory is gone was bound")
	}
}

func TestBindViewsWritesNothing(t *testing.T) {
	// Arrange: a gone directory is exactly what PublishRegistry closes, and a
	// joining successor's read-only handle must not be asked to.
	f := newFixture(t)
	dir := worktreeDir(t)
	record, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove the workspace directory: %v", err)
	}

	// Act.
	if err := f.verbs.BindViews(context.Background()); err != nil {
		t.Fatalf("BindViews: %v", err)
	}

	// Assert.
	if f.db.closedFlags[record.ID] {
		t.Fatalf("BindViews closed workspace %v; it must write nothing", record.ID)
	}
}
