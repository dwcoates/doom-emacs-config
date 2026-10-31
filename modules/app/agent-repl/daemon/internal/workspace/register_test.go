package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/wsm"
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

// TestRegisterTellsTheOwnershipOnlyOfAWorkspaceItCreated covers the
// creation half of the 2026-10-30 stranding: a workspace is owned by the
// daemon that minted it, and only a minting registration says so. A
// re-registration (an editor announcing a workspace it already had) must not,
// or a successor would claim a manifested workspace before adopting it.
func TestRegisterTellsTheOwnershipOnlyOfAWorkspaceItCreated(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)

	// Act.
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	mintedByFirst := slices.Clone(f.owner.created)
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("second Register: %v", err)
	}

	// Assert. The first registration minted the repository's main worktree
	// (born closed) and the workspace itself, both by this daemon; the
	// second minted nothing.
	if len(mintedByFirst) != 2 || mintedByFirst[1] != first.ID {
		t.Fatalf("first Register told created %v, want [<main worktree> %s]", mintedByFirst, first.ID)
	}
	if !slices.Equal(f.owner.created, mintedByFirst) {
		t.Fatalf("a re-registration told created %v, want nothing beyond %v", f.owner.created, mintedByFirst)
	}
}

// TestRegisterDerivesTheRepositoryFromGitsMainWorktree covers what a
// repository IS to the contract: RepositoryRef.dir is "the repository's
// normalized main-worktree directory", and it is what a top-level workspace's
// merge targets -- never the common dir, which no git verb should be aimed at.
func TestRegisterDerivesTheRepositoryFromGitsMainWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	main := worktreeDir(t)
	f.git.mainWorktree = main
	f.git.commonDir = filepath.Join(main, ".git")

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert: the main worktree's own registration, then the linked one's.
	if len(f.db.registered) != 2 || f.db.registered[1].RepoDir != main {
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
	// its own tree. The repository is the one git agrees with.
	f := newFixture(t)
	given := worktreeDir(t)
	dir := worktreeDir(t)
	f.git.belongsTo(t, dir, given)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{
		Name: "given", Branch: "given-branch", ParentBranch: "given-parent", RepoDir: given,
	}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	got := f.db.registered[len(f.db.registered)-1]
	if got.Name != "given" || got.Branch != "given-branch" || got.ParentBranch != "given-parent" || got.RepoDir != given {
		t.Fatalf("registered facts = %+v, want the supplied ones kept", got)
	}
}

// TestRegisterRefusesARepositoryGitDisagreesWith covers the roster's
// grouping invariant at its source: a workspace is listed under a repository
// only if its worktree belongs to that repository, so a registration naming
// any other repository fails with nothing written and an ERROR naming both.
func TestRegisterRefusesARepositoryGitDisagreesWith(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	claimed, actual, dir := worktreeDir(t), worktreeDir(t), worktreeDir(t)
	f.git.belongsTo(t, dir, actual)

	// Act.
	_, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{RepoDir: claimed})

	// Assert.
	if !errors.Is(err, ErrRepositoryMismatch) {
		t.Fatalf("Register = %v, want ErrRepositoryMismatch", err)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none", f.db.registered)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opRegister})
	if !ok || rec.Context["claimed_repo_dir"] != claimed || rec.Context["git_repo_dir"] != actual || rec.Context["invariant_violation"] == nil {
		t.Fatalf("ERROR record = %+v, want one naming the claimed and git's repositories", rec)
	}
}

// TestRegisterAsksGitEvenWhenTheRepositoryIsNamed covers that a named
// repository never skips git: a git that cannot answer fails the registration.
func TestRegisterAsksGitEvenWhenTheRepositoryIsNamed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.commonDirErr = errors.New("git: not a repository")

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{RepoDir: worktreeDir(t)})

	// Assert.
	if !errors.Is(err, f.git.commonDirErr) {
		t.Fatalf("Register = %v, want git's failure", err)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none", f.db.registered)
	}
	if countRecords(f, "error", opRegister) != 1 {
		t.Fatalf("ERROR records = %d, want exactly one", countRecords(f, "error", opRegister))
	}
}

// TestRegisterMatchesTheNamedRepositoryInAnySpelling covers that the check
// compares canonical directories: a named repository reached through a
// symlink is the repository git names.
func TestRegisterMatchesTheNamedRepositoryInAnySpelling(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	main, dir := worktreeDir(t), worktreeDir(t)
	f.git.belongsTo(t, dir, main)
	link := filepath.Join(t.TempDir(), "repo-link")
	if err := os.Symlink(main, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{RepoDir: link}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if got := f.db.registered[len(f.db.registered)-1].RepoDir; got != main {
		t.Fatalf("registered repository = %q, want git's %q", got, main)
	}
}

func TestRegisterNamesTheMainWorktreeAfterItsRepository(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := f.git.mainWorktree

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	want := wsm.MainWorkspaceName(filepath.Base(dir))
	if f.db.registered[0].Name != want {
		t.Fatalf("registered name = %q, want %q", f.db.registered[0].Name, want)
	}
}

func TestRegisterDerivesALinkedWorktreesNameFromItsDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.belongsTo(t, dir, f.git.mainWorktree)

	// Act.
	if _, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	got := f.db.registered[len(f.db.registered)-1].Name
	if got != filepath.Base(dir) {
		t.Fatalf("registered name = %q, want %q", got, filepath.Base(dir))
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

func TestPublishRegistryRefusesWhenTheTasksCannotBeRead(t *testing.T) {
	// Arrange: a task row that does not decode is a failed read.
	f := newFixture(t)
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.db.tasksErr = errors.New("decode task row: invalid json")
	before := len(f.sidebar.registries)

	// Act
	err := f.verbs.PublishRegistry(context.Background())

	// Assert
	if err == nil || !strings.Contains(err.Error(), "list the tasks") {
		t.Fatalf("PublishRegistry = %v, want the task read's failure surfaced", err)
	}
	if len(f.sidebar.registries) != before {
		t.Fatalf("roster publications = %d, want no roster published from records that could not be read", len(f.sidebar.registries))
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opRegister}); !ok {
		t.Fatalf("records = %+v, want the failed task read at ERROR", f.log.logger.Records())
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

// TestRegisterStartsNoShim pins the owner ruling of 2026-10-28: Emacs's
// announcement of a workspace starts no shim, whatever its record holds. The
// conversation is read from the store, and the next prompt brings the shim
// up.
func TestRegisterStartsNoShim(t *testing.T) {
	cases := []struct {
		name     string
		terminal *wsm.SessionTerminal
	}{
		{name: "a recorded conversation with no terminal"},
		{name: "a hibernated session", terminal: &wsm.SessionTerminal{Kind: "hibernated", Detail: "idle past the cutoff"}},
		{name: "a deleted session", terminal: &wsm.SessionTerminal{Kind: "deleted", Detail: "NukeWorkspace"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			dir, _ := registeredWithAConversation(t, f, tc.terminal)

			// Act.
			_, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})

			// Assert.
			if err != nil || len(f.fleet.started) != 0 || len(f.fleet.startCalls) != 0 {
				t.Fatalf("Register = %v after starts %v; want the row and no start", err, f.fleet.startCalls)
			}
		})
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
	if closures := f.db.closures[record.ID]; len(closures) != 1 || closures[0].By != wsm.CloseByDaemon {
		t.Fatalf("close records = %+v, want one by the daemon", closures)
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

func TestRegisterReopenClaimsServingForThisDaemon(t *testing.T) {
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
	want := []servingClaim{{ws: first.ID, instance: fixtureInstance}}
	if !reflect.DeepEqual(f.db.reopenClaims, want) {
		t.Fatalf("reopen claims = %+v, want %+v", f.db.reopenClaims, want)
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

func TestRegisterRestoresTheWorkspaceSummary(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	record, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if !slices.Equal(f.summaries.restored, []ids.WorkspaceID{record.ID}) {
		t.Fatalf("restored summaries = %v, want %q's", f.summaries.restored, record.ID)
	}
}

func TestRegisterSurfacesAFailedSummaryRestore(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.summaries.err = errors.New("database is locked")

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "restore the summary") || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Register = %v, want the restore's failure", err)
	}
}

func TestPublishRegistryRestoresEveryInheritedWorkspacesSummary(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	record, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	f.summaries.restored = nil

	// Act.
	if err := f.verbs.PublishRegistry(context.Background()); err != nil {
		t.Fatalf("PublishRegistry: %v", err)
	}

	// Assert.
	if !slices.Equal(f.summaries.restored, []ids.WorkspaceID{record.ID}) {
		t.Fatalf("restored summaries = %v, want %q's", f.summaries.restored, record.ID)
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

func TestRegisterAnswersARefusedReopen(t *testing.T) {
	// Arrange: a closed workspace whose parent is not open.
	f := newFixture(t)
	dir := worktreeDir(t)
	first, err := f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("first Register: %v", err)
	}
	closed := first
	closed.Closed = true
	f.db.with(closed)
	f.db.setClosedErr = errParentNotOpen

	// Act.
	_, err = f.verbs.Register(context.Background(), dir, wsm.RegisterFacts{})

	// Assert.
	requireFamilyAnswer(t, f, err, errParentNotOpen, opRegister)
}

func TestRegisterAnswersARefusedChild(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.registerErr = errParentNotOpen

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	requireFamilyAnswer(t, f, err, errParentNotOpen, opRegister)
}

// registeredAt answers the workspace the fake registry holds at dir.
func registeredAt(f *fixture, dir string) (wsm.Workspace, bool) {
	ws, ok := f.db.byDir[dir]
	return ws, ok
}

func TestRegisteringALinkedWorktreeRegistersTheMainWorktreeFirst(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.defaultBranch = "trunk"
	linked := worktreeDir(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), linked, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 2 {
		t.Fatalf("registrations = %+v, want the main worktree's and then the linked one's", f.db.registered)
	}
	if main := f.db.registered[0]; main.RepoDir != f.git.mainWorktree || main.DefaultBranch != "trunk" {
		t.Fatalf("first registration = %+v, want the main worktree of %s with its default branch", main, f.git.mainWorktree)
	}
	if f.db.registerDir != linked {
		t.Fatalf("last registered dir = %q, want the linked worktree %q", f.db.registerDir, linked)
	}
}

func TestTheEnsuredMainWorktreeIsBornClosedByTheDaemon(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	main, ok := registeredAt(f, f.git.mainWorktree)
	if !ok {
		t.Fatalf("no workspace is registered at the main worktree %s", f.git.mainWorktree)
	}
	if !main.Closed || main.Closure == nil || main.Closure.By != wsm.CloseByDaemon {
		t.Fatalf("main worktree workspace = closed %v closure %+v, want born closed by the daemon", main.Closed, main.Closure)
	}
}

func TestTheEnsuredMainWorktreeClaimsNoServingAndStartsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.verbs.(*verbs).deps.Instance = "instance-1"

	// Act.
	linked, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})
	if err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if len(f.db.claims) != 1 || f.db.claims[0].ws != linked.ID {
		t.Fatalf("serving claims = %+v, want the linked worktree's alone", f.db.claims)
	}
	if got := f.fleet.started; len(got) != 0 {
		t.Fatalf("sessions started = %v, want none", got)
	}
}

func TestRegisteringALinkedWorktreePublishesOneRosterCarryingBothRows(t *testing.T) {
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
	if _, ok := registeredAt(f, f.git.mainWorktree); !ok {
		t.Fatalf("the roster was published without the main worktree registered")
	}
}

func TestAnAlreadyRegisteredMainWorktreeIsLeftAsItStands(t *testing.T) {
	tests := []struct {
		name   string
		closed bool
	}{
		{name: "an open main worktree", closed: false},
		{name: "a closed main worktree stays closed", closed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			main := wsm.Workspace{ID: "ws-main", Dir: f.git.mainWorktree, Repo: "repo-1", Closed: tt.closed}
			// The row's repository is held, as the schema's foreign key
			// requires of every workspace row.
			f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: f.git.mainWorktree})
			f.db.with(main)

			// Act.
			if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
				t.Fatalf("Register: %v", err)
			}

			// Assert.
			if len(f.db.registered) != 1 {
				t.Fatalf("registrations = %+v, want the linked worktree's alone", f.db.registered)
			}
			if len(f.db.reopened) != 0 {
				t.Fatalf("reopened = %v, want the main worktree left as it stands", f.db.reopened)
			}
		})
	}
}

func TestRegisteringTheMainWorktreeItselfRegistersItOnce(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	record, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})

	// Assert.
	if err != nil {
		t.Fatalf("Register: %v", err)
	}
	if len(f.db.registered) != 1 || record.Closed {
		t.Fatalf("registrations = %+v, closed = %v; want the main worktree registered once, open", f.db.registered, record.Closed)
	}
}

func TestABornClosedRegistrationOfAClosedRowReopensNothing(t *testing.T) {
	// Arrange: the row appeared between the lookup and the registration.
	f := newFixture(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-1", Dir: f.git.mainWorktree})
	f.db.with(wsm.Workspace{ID: "ws-main", Dir: f.git.mainWorktree, Repo: "repo-1", Closed: true})

	// Act.
	_, _, err := f.verbs.(*verbs).register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{
		RepoDir: f.git.mainWorktree, Closure: &wsm.Closure{At: time.Unix(1, 0), By: wsm.CloseByDaemon},
	})

	// Assert.
	if err != nil {
		t.Fatalf("register: %v", err)
	}
	if len(f.db.reopened) != 0 {
		t.Fatalf("reopened = %v, want nothing reopened", f.db.reopened)
	}
}

func TestAMainWorktreeThatCannotBeRegisteredRegistersNoLinkedWorktree(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.branchErr = errors.New("git: HEAD is unreadable")

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	if !errors.Is(err, f.git.branchErr) {
		t.Fatalf("Register = %v, want the main worktree's failure", err)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none", f.db.registered)
	}
	if n := countRecords(f, "error", opRegister); n != 1 {
		t.Fatalf("ERROR records under %s = %d, want the failure recorded once", opRegister, n)
	}
}

func TestAMainWorktreeLookupFailureIsSurfacedAndRecorded(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.workspaceByDirErr = errors.New("sql: database is locked")
	f.db.workspaceByDirErrAt = f.git.mainWorktree

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	if !errors.Is(err, f.db.workspaceByDirErr) {
		t.Fatalf("Register = %v, want the lookup's failure", err)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none", f.db.registered)
	}
	rec, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opRegister})
	if !ok || rec.Context["repo_dir"] != f.git.mainWorktree {
		t.Fatalf("ERROR record = %+v, want one naming the main worktree", rec)
	}
}

func TestARefusedMainWorktreeIsAnsweredAndNotRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.registerErr = insideTemp

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	assertTemporaryRefusal(t, f, err, "RegisterWorkspace")
}

func TestEnsureMainWorktreesRegistersEveryMissingOneBornClosed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	missing := worktreeDir(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories,
		wsm.Repository{ID: "repo-missing", Dir: missing, Name: "missing", DefaultBranch: "trunk"},
		wsm.Repository{ID: "repo-held", Dir: held, Name: "held", DefaultBranch: "master"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "main (held)"})
	f.git.belongsTo(t, missing, missing)

	// Act.
	if err := f.verbs.EnsureMainWorktrees(context.Background()); err != nil {
		t.Fatalf("EnsureMainWorktrees: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 1 || f.db.registerDir != missing || f.db.registered[0].DefaultBranch != "trunk" {
		t.Fatalf("registrations = %+v at %q, want the missing main worktree's alone", f.db.registered, f.db.registerDir)
	}
	if ws, _ := registeredAt(f, missing); !ws.Closed {
		t.Fatalf("the ensured main worktree is open, want it born closed")
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestEnsureMainWorktreesPublishesNothingWhenNothingWasMissing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-held", Dir: held, Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "main (held)"})

	// Act.
	if err := f.verbs.EnsureMainWorktrees(context.Background()); err != nil {
		t.Fatalf("EnsureMainWorktrees: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 0 || len(f.sidebar.registries) != 0 {
		t.Fatalf("registrations %+v, publications %d; want neither", f.db.registered, len(f.sidebar.registries))
	}
}

func TestEnsureMainWorktreesPassesOverAGoneRepository(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	gone := filepath.Join(t.TempDir(), "gone")
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-gone", Dir: gone})

	// Act.
	if err := f.verbs.EnsureMainWorktrees(context.Background()); err != nil {
		t.Fatalf("EnsureMainWorktrees: %v", err)
	}

	// Assert.
	if len(f.db.registered) != 0 {
		t.Fatalf("registrations = %+v, want none for a gone repository", f.db.registered)
	}
}

func TestEnsureMainWorktreesSurfacesAnUnreadableRepositoryDirectory(t *testing.T) {
	// Arrange: a path through a FILE stats as "not a directory", which is not
	// "not exist" and is never read as gone.
	f := newFixture(t)
	file := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatalf("write %s: %v", file, err)
	}
	unreadable := filepath.Join(file, "repo")
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-unreadable", Dir: unreadable})

	// Act.
	err := f.verbs.EnsureMainWorktrees(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), unreadable) {
		t.Fatalf("EnsureMainWorktrees = %v, want the stat failure naming %s", err, unreadable)
	}
	if !hasRecord(f, "error", opRegister) || len(f.db.registered) != 0 {
		t.Fatalf("want the failure recorded at ERROR and nothing registered (registrations %+v)", f.db.registered)
	}
}

func TestEnsureMainWorktreesSurfacesAnUnreadableRegistry(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.listRepositoriesErr = errors.New("the state database is unreachable")

	// Act.
	err := f.verbs.EnsureMainWorktrees(context.Background())

	// Assert.
	if !errors.Is(err, f.db.listRepositoriesErr) {
		t.Fatalf("EnsureMainWorktrees = %v, want the registry's failure", err)
	}
	if !hasRecord(f, "error", opRegister) {
		t.Fatal("the failure was not recorded at ERROR")
	}
}

func TestEnsureMainWorktreesSurfacesARegistrationThatFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	missing := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-missing", Dir: missing})
	f.git.belongsTo(t, missing, missing)
	f.db.registerErr = errFake

	// Act.
	err := f.verbs.EnsureMainWorktrees(context.Background())

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("EnsureMainWorktrees = %v, want the registration's failure", err)
	}
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none for a failed walk", len(f.sidebar.registries))
	}
}

// countRecords counts the fixture's captured records at level under operation.
func countRecords(f *fixture, level, operation string) int {
	n := 0
	for _, rec := range f.log.logger.Records() {
		if rec.Level == level && rec.Operation == operation {
			n++
		}
	}
	return n
}

func TestRegistrationHandsTheTitleTheRecordedShownLinks(t *testing.T) {
	doc := wsm.WorkspaceLink{URL: "https://example.com/design", Label: "The design doc", Shown: true}
	unshown := wsm.WorkspaceLink{URL: "https://example.com/old", Label: "An older draft"}
	tests := []struct {
		name  string
		links []wsm.WorkspaceLink
		want  [][]topbar.Link
	}{
		{name: "the recorded shown links are restored", links: []wsm.WorkspaceLink{doc, unshown},
			want: [][]topbar.Link{{{Label: doc.Label, URL: doc.URL}}}},
		{name: "a workspace with none is handed none", links: nil, want: [][]topbar.Link{nil}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the workspace is already registered, as across a restart.
			f := newFixture(t)
			f.workspace("w1", f.git.mainWorktree)
			f.db.links = map[ids.WorkspaceID][]wsm.WorkspaceLink{"w1": tt.links}

			// Act.
			if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
				t.Fatalf("Register: %v", err)
			}

			// Assert.
			if !reflect.DeepEqual(f.topbarLinks, tt.want) {
				t.Fatalf("topbar links = %v, want %v", f.topbarLinks, tt.want)
			}
		})
	}
}

func TestRegistrationRefusesWhenTheLinksCannotBeRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", f.git.mainWorktree)
	f.db.linksReadErr = errors.New("disk gone")

	// Act.
	_, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})

	// Assert: the failure is surfaced, recorded, and no title links published.
	if err == nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("Register = %v, want the links read failure", err)
	}
	if len(f.topbarLinks) != 0 {
		t.Errorf("topbar links = %v, want none published", f.topbarLinks)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRegister,
		Message: "could not read the workspace's links for its title"})
	if !ok || r.Context["workspace"] != "w1" {
		t.Errorf("record = %+v, want an ERROR naming the workspace", r)
	}
}

func TestEnsureMainWorktreesRenamesAMainWorkspaceRegisteredBeforeTheRule(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-held", Dir: held, Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "held"})

	// Act.
	if err := f.verbs.EnsureMainWorktrees(context.Background()); err != nil {
		t.Fatalf("EnsureMainWorktrees: %v", err)
	}

	// Assert.
	if got := f.db.workspaces["ws-held"].Name; got != "main (held)" {
		t.Fatalf("main workspace name = %q, want %q", got, "main (held)")
	}
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

func TestEnsureMainWorktreesLeavesAnAlreadyRuleNamedMainWorkspaceAlone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-held", Dir: held, Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "main (held)"})

	// Act.
	if err := f.verbs.EnsureMainWorktrees(context.Background()); err != nil {
		t.Fatalf("EnsureMainWorktrees: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 0 {
		t.Fatalf("roster republications = %d, want none", len(f.sidebar.registries))
	}
}

func TestEnsureMainWorktreesFailsWhenTheRuleNameIsHeldByAnotherWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-held", Dir: held, Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-squatter", Dir: worktreeDir(t), Repo: "repo-held", Name: "main (held)"})

	// Act.
	err := f.verbs.EnsureMainWorktrees(context.Background())

	// Assert.
	if err == nil || !hasRecord(f, "error", opRegister) {
		t.Fatalf("EnsureMainWorktrees = %v, want a failure recorded at ERROR", err)
	}
	if got := f.db.workspaces["ws-held"].Name; got != "held" {
		t.Fatalf("main workspace name = %q, want it untouched", got)
	}
}

func TestEnsureMainWorktreesSurfacesARenameThatFailed(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	held := worktreeDir(t)
	f.db.repositories = append(f.db.repositories, wsm.Repository{ID: "repo-held", Dir: held, Name: "held"})
	f.db.with(wsm.Workspace{ID: "ws-held", Dir: held, Repo: "repo-held", Name: "held"})
	f.db.setNameErr = errFake

	// Act.
	err := f.verbs.EnsureMainWorktrees(context.Background())

	// Assert.
	if !errors.Is(err, errFake) || !hasRecord(f, "error", opRegister) {
		t.Fatalf("EnsureMainWorktrees = %v, want the rename's failure recorded at ERROR", err)
	}
}
