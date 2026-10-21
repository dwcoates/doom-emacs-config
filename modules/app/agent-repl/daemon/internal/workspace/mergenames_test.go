package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// claimRepo answers a main worktree on disk that F's git resolves every
// directory to, so a name's worktree directory can be derived and probed.
func claimRepo(t *testing.T, f *fixture) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	f.git.mainWorktree = dir
	return dir
}

func TestClaimMergeNameClaimsAFreeName(t *testing.T) {
	// Arrange
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")

	// Act
	branch, release, taken, err := f.verbs.ClaimMergeName(context.Background(), claimRepo(t, f), "reconnect-fix")

	// Assert
	if err != nil || taken || branch != "ABC/reconnect-fix" || release == nil {
		t.Fatalf("ClaimMergeName = %q, %v, %v; want the prefixed branch claimed", branch, taken, err)
	}
	release()
}

func TestClaimMergeNameAnswersTakenForWhatIsAlreadyTaken(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
	}{
		{name: "a registered workspace", arrange: func(_ *testing.T, f *fixture) {
			f.db.with(wsm.Workspace{ID: "ws-other", Name: "ABC/reconnect-fix", Dir: "/tmp/other"})
		}},
		{name: "a branch", arrange: func(_ *testing.T, f *fixture) { f.git.existingBranches = map[string]bool{"ABC/reconnect-fix": true} }},
		{name: "a reservation", arrange: func(_ *testing.T, f *fixture) {
			f.db.reserved = map[string]ids.WorkspaceID{"reconnect-fix": "ws-other"}
		}},
		// Another branch's worktree under the same bare name: the 2026-10-20
		// collision, "fix/outage-recovery" standing where "outage-recovery"
		// would be materialized.
		{name: "an occupied worktree directory", arrange: func(t *testing.T, f *fixture) {
			occupied := filepath.Join(filepath.Dir(f.git.mainWorktree), filepath.Base(f.git.mainWorktree)+WorktreeDirSuffix, "reconnect-fix")
			if err := os.MkdirAll(occupied, 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			t.Setenv(PrefixEnv, "ABC")
			t.Setenv(LegacyPrefixEnv, "")
			repo := claimRepo(t, f)
			tt.arrange(t, f)

			// Act
			branch, release, taken, err := f.verbs.ClaimMergeName(context.Background(), repo, "reconnect-fix")

			// Assert
			if err != nil || !taken || branch != "ABC/reconnect-fix" || release != nil {
				t.Fatalf("ClaimMergeName = %q, %v, %v; want taken with no claim", branch, taken, err)
			}
		})
	}
}

func TestClaimMergeNameAnswersTakenWhileAnotherClaimStands(t *testing.T) {
	// Arrange
	f := newFixture(t)
	dir := claimRepo(t, f)
	_, release, _, err := f.verbs.ClaimMergeName(context.Background(), dir, "reconnect-fix")
	if err != nil {
		t.Fatalf("first claim: %v", err)
	}

	// Act
	_, _, taken, err := f.verbs.ClaimMergeName(context.Background(), dir, "reconnect-fix")
	release()
	_, again, takenAfter, errAfter := f.verbs.ClaimMergeName(context.Background(), dir, "reconnect-fix")

	// Assert
	if err != nil || !taken {
		t.Fatalf("second claim = %v, %v; want taken while the first stands", taken, err)
	}
	if errAfter != nil || takenAfter {
		t.Fatalf("claim after release = %v, %v; want free again", takenAfter, errAfter)
	}
	again()
}

func TestClaimMergeNameFailsLoudlyWhenTheReservationsCannotBeRead(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.reservedErr = errors.New("database is locked")

	// Act
	_, release, _, err := f.verbs.ClaimMergeName(context.Background(), t.TempDir(), "reconnect-fix")

	// Assert
	if err == nil || release != nil {
		t.Fatalf("ClaimMergeName = %v with a claim, want the failure and no claim", err)
	}
	if !loggedAtError(f, opNaming) {
		t.Fatal("the failed read was not recorded at ERROR")
	}
}

func TestClaimMergeNameFailsLoudlyWhenTheWorktreeDirectoryCannotBeDerived(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.git.commonDirErr = errors.New("not a git repository")

	// Act
	_, release, _, err := f.verbs.ClaimMergeName(context.Background(), t.TempDir(), "reconnect-fix")

	// Assert
	if err == nil || release != nil {
		t.Fatalf("ClaimMergeName = %v with a claim, want the failure and no claim", err)
	}
	if !loggedAtError(f, opNaming) {
		t.Fatal("the failed probe was not recorded at ERROR")
	}
}

func TestFreeNameSkipsAReservedName(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.reserved = map[string]ids.WorkspaceID{"ABC/fix": "ws-other"}

	// Act
	branch, release, err := f.verbs.(*verbs).freeName(context.Background(), f.log.logger, claimRepo(t, f), "ABC/fix")

	// Assert
	if err != nil || branch != "ABC/fix-2" {
		t.Fatalf("freeName = %q, %v; want the reservation skipped", branch, err)
	}
	release()
}

func TestFreeNameClaimsTheNameItChoseUntilReleased(t *testing.T) {
	// Arrange
	f := newFixture(t)
	dir := claimRepo(t, f)

	// Act
	first, release, err := f.verbs.(*verbs).freeName(context.Background(), f.log.logger, dir, "ABC/fix")
	second, releaseSecond, err2 := f.verbs.(*verbs).freeName(context.Background(), f.log.logger, dir, "ABC/fix")

	// Assert
	if err != nil || err2 != nil || first != "ABC/fix" || second != "ABC/fix-2" {
		t.Fatalf("freeName = %q, %q (%v, %v); want the second allocation to skip the first's claim", first, second, err, err2)
	}
	release()
	releaseSecond()
}

func TestFreeNameSkipsAnOccupiedWorktreeDirectory(t *testing.T) {
	// Arrange
	f := newFixture(t)
	repo := claimRepo(t, f)
	occupied := filepath.Join(filepath.Dir(repo), filepath.Base(repo)+WorktreeDirSuffix, "fix")
	if err := os.MkdirAll(occupied, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act
	branch, release, err := f.verbs.(*verbs).freeName(context.Background(), f.log.logger, repo, "ABC/fix")

	// Assert
	if err != nil || branch != "ABC/fix-2" {
		t.Fatalf("freeName = %q, %v; want the occupied directory skipped", branch, err)
	}
	release()
}

func TestFreeNameFailsWhenTheWorktreeDirectoryCannotBeDerived(t *testing.T) {
	// Arrange: a directory with no .git, whose worktree directory has no
	// parent to derive.
	f := newFixture(t)

	// Act
	_, release, err := f.verbs.(*verbs).freeName(context.Background(), f.log.logger, t.TempDir(), "ABC/fix")

	// Assert
	if err == nil || release != nil {
		t.Fatalf("freeName = %v with a claim, want the failure and no claim", err)
	}
}

func TestCreateRefusesASuppliedNameAnAsyncMergeReserved(t *testing.T) {
	// Arrange
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")
	f.db.reserved = map[string]ids.WorkspaceID{"reconnect-fix": "ws-other"}

	// Act
	_, err := f.verbs.Create(context.Background(), CreateSpec{RepoDir: mainWorktree(t, f), Name: "reconnect-fix"})

	// Assert
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != ArmNameReserved || refusal.Fields["branch"] != "ABC/reconnect-fix" {
		t.Fatalf("Create = %v, want the name_reserved refusal naming the branch", err)
	}
	if len(f.git.created) != 0 {
		t.Fatalf("created = %+v, want nothing built", f.git.created)
	}
}

func TestRevealMergeWorkspaceRevealsAndRepublishesTheRegistry(t *testing.T) {
	// Arrange
	f := newFixture(t)
	requester := ids.WorkspaceID("ws-requester")
	f.db.with(wsm.Workspace{ID: "ws-merge", Name: "reconnect-fix", Dir: t.TempDir(), Repo: "repo-1", MergeOf: &wsm.MergeOrigin{Requester: requester, Request: "r-1"}})

	// Act
	revealed, err := f.verbs.RevealMergeWorkspace(context.Background(), "ws-merge")

	// Assert
	if err != nil || !revealed {
		t.Fatalf("RevealMergeWorkspace = %v, %v; want revealed", revealed, err)
	}
	if ws, _ := f.db.Workspace(context.Background(), "ws-merge"); !ws.MergeRevealed {
		t.Fatal("the merge workspace was not recorded revealed")
	}
}

func TestRevealMergeWorkspaceAnswersFalseForOneAlreadyRevealed(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.with(wsm.Workspace{ID: "ws-merge", Name: "reconnect-fix", Dir: t.TempDir(), Repo: "repo-1", MergeRevealed: true,
		MergeOf: &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}})

	// Act
	revealed, err := f.verbs.RevealMergeWorkspace(context.Background(), "ws-merge")

	// Assert
	if err != nil || revealed {
		t.Fatalf("RevealMergeWorkspace = %v, %v; want no change", revealed, err)
	}
}

func TestRevealMergeWorkspaceThatCannotBeRecordedIsAnError(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.db.with(wsm.Workspace{ID: "ws-merge", Name: "reconnect-fix", Dir: t.TempDir(), Repo: "repo-1", MergeOf: &wsm.MergeOrigin{Requester: "ws-requester", Request: "r-1"}})
	f.db.revealErr = errors.New("database is locked")

	// Act
	_, err := f.verbs.RevealMergeWorkspace(context.Background(), "ws-merge")

	// Assert
	if err == nil {
		t.Fatal("RevealMergeWorkspace = nil, want the failure")
	}
	if !loggedAtError(f, opRevealMergeWorkspace) {
		t.Fatal("the failed reveal was not recorded at ERROR")
	}
}
