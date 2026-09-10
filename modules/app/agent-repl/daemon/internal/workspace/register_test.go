package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

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
	f.git.currentBranch = "DWC/derived"

	// Act.
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if f.db.registered[0].Branch != "DWC/derived" {
		t.Fatalf("registered branch = %q, want DWC/derived", f.db.registered[0].Branch)
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
