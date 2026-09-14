//go:build integration

package integration

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// registerRepository calls the rpc and answers the whole message, so a test can
// read either arm.
func registerRepository(t *testing.T, d *harness.Daemon, path string) *agentreplv1.RegisterRepositoryResponse {
	t.Helper()
	resp, err := d.Client().RegisterRepository(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{Path: path}))
	if err != nil {
		t.Fatalf("RegisterRepository(%s) = transport error %v, want an in-band answer", path, err)
	}
	return resp.Msg
}

// TestRegisterRepositoryFromAFileInsideItMintsTheRepository drives the whole
// path the command takes: a FILE is picked, the scripted git resolves the
// repository it is in, and the ref comes back.
func TestRegisterRepositoryFromAFileInsideItMintsTheRepository(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)

	// Act
	msg := registerRepository(t, d, filepath.Join(repo.Dir, "README.md"))

	// Assert
	success := msg.GetSuccess()
	if success == nil {
		t.Fatalf("RegisterRepository = %v, want a success", msg)
	}
	if success.GetRepository().GetId() == "" {
		t.Fatalf("RegisterRepository = %v, want a minted repository id", msg)
	}
	if got := success.GetRepository().GetDir(); got != repo.Dir {
		t.Fatalf("RegisterRepository = dir %q, want the repository's main worktree %q", got, repo.Dir)
	}
	if success.GetAlreadyKnown() {
		t.Fatalf("RegisterRepository reported already_known for a repository nothing had registered")
	}
}

// TestRegisterRepositorySaysWhenItAlreadyHadTheRepository pins the answer that
// makes the command reportable: re-registering is success, and the caller is
// told which of the two it was.
func TestRegisterRepositorySaysWhenItAlreadyHadTheRepository(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	first := registerRepository(t, d, repo.Dir).GetSuccess()

	// Act
	again := registerRepository(t, d, filepath.Join(repo.Dir, "README.md")).GetSuccess()

	// Assert
	if !again.GetAlreadyKnown() {
		t.Fatalf("the second RegisterRepository = already_known false, want true")
	}
	if again.GetRepository().GetId() != first.GetRepository().GetId() {
		t.Fatalf("the second RegisterRepository = id %q, want the first mint's id %q",
			again.GetRepository().GetId(), first.GetRepository().GetId())
	}
}

// TestRegisterRepositoryAdoptsARepositoryAWorkspaceAlreadyMinted pins the two
// mint paths on ONE row: RegisterWorkspace's own ensureRepo and this verb
// answer the same repository for one directory.
func TestRegisterRepositoryAdoptsARepositoryAWorkspaceAlreadyMinted(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	msg := registerRepository(t, f.d, filepath.Join(f.repo.Dir, "README.md"))

	// Assert
	if !msg.GetSuccess().GetAlreadyKnown() {
		t.Fatalf("RegisterRepository = already_known false, want the repository the workspace minted")
	}
	if got := msg.GetSuccess().GetRepository().GetDir(); got != f.repo.Dir {
		t.Fatalf("RegisterRepository = dir %q, want %q", got, f.repo.Dir)
	}
}

// TestRegisterRepositoryPutsAnEmptySectionOnTheRoster is the whole visible
// consequence: a repository with no workspace draws a section, or the user has
// no evidence the registration happened.
func TestRegisterRepositoryPutsAnEmptySectionOnTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()

	// Act
	id := registerRepository(t, d, repo.Dir).GetSuccess().GetRepository().GetId()

	// Assert
	got := awaitRoster(t, d, roster, "a roster carrying the newly registered repository",
		func(r *frontendv1.WorkspaceRoster) bool {
			return repoSectionByID(r, id) != nil
		})
	section := repoSectionByID(got, id)
	if rows := section.GetRows().GetRows(); len(rows) != 0 {
		t.Fatalf("the new repository's section carries %d rows, want none", len(rows))
	}
	if section.GetHeader().GetLabel().GetText() == "" {
		t.Fatalf("the new repository's section has no header label: %v", section)
	}
}

func TestRegisterRepositoryRefusesAPathOutsideAnyRepository(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	plain := t.TempDir()
	path := filepath.Join(plain, "loose.txt")
	if err := os.WriteFile(path, []byte("loose\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}

	// Act
	msg := registerRepository(t, d, path)

	// Assert
	if msg.GetError().GetNotInARepository() == nil {
		t.Fatalf("RegisterRepository(%s) = %v, want RegisterRepositoryError.not_in_a_repository", path, msg)
	}
}

func TestRegisterRepositoryRefusesAPathThatIsNotThere(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	absent := filepath.Join(t.TempDir(), "absent.txt")

	// Act
	msg := registerRepository(t, d, absent)

	// Assert
	if msg.GetError().GetUnreadablePath() == nil {
		t.Fatalf("RegisterRepository(%s) = %v, want RegisterRepositoryError.unreadable_path", absent, msg)
	}
}

func TestRegisterRepositoryRefusesABlankPathAsAValidationFailure(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	_, err := d.Client().RegisterRepository(context.Background(), connect.NewRequest(&agentreplv1.RegisterRepositoryRequest{}))

	// Assert: a blank string is not a path at all, so it is InvalidArgument
	// rather than one of the two arms, which are about what a real path is.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("RegisterRepository(\"\") = %v, want InvalidArgument", err)
	}
}

// repoSectionByID answers the repository grouping's section for one repository.
func repoSectionByID(r *frontendv1.WorkspaceRoster, id string) *frontendv1.RosterRepoSection {
	for _, section := range r.GetRepository().GetSections() {
		if section.GetKey().GetRepository().GetId() == id {
			return section
		}
	}
	return nil
}
