//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/fakegit"
	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// scratchRepo mints a fake repository in a temporary folder OUTSIDE this run's
// root -- a scratch folder under /tmp the run does not own, exactly what the
// 2026-09-02 capture run had registered -- and answers it with the temporary
// root it lies inside.
//
// CALL IT BEFORE newDaemon. Cleanups run last first, so a daemon started
// after it stops before the folder goes; the other way round the folder is
// removed under a daemon still running, and any later look the daemon takes
// at the path (an instrumented, slower daemon took one) logs an error that
// fails the test at the warning sweep.
func scratchRepo(t *testing.T) (*harness.Repo, string) {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "arscratch")
	if err != nil {
		t.Fatalf("make the scratch folder: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	return harness.NewRepoAt(t, filepath.Join(fakegit.Canon(dir), "cwd")), fakegit.Canon("/tmp")
}

// awaitTypedRefusal waits for the INFO record of the inside_temporary_directory
// refusal: an answer, so the warning sweep at cleanup also proves nothing
// warned. The shared registration body records it before an rpc names it, so
// the record is matched by its arm.
func awaitTypedRefusal(t *testing.T, d *harness.Daemon) {
	t.Helper()
	d.AwaitLogRecord(d.RunLogPath(), "the typed temporary-directory refusal at INFO", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.refusal.typed" && r.Level == "info" &&
			r.Context["arm"] == "inside_temporary_directory"
	})
}

func TestRegisterRepositoryRefusesARepositoryInsideATemporaryFolder(t *testing.T) {
	t.Parallel()
	// Arrange
	repo, root := scratchRepo(t)
	d := newDaemon(t, harness.Opts{})

	// Act
	msg := registerRepository(t, d, filepath.Join(repo.Dir, "README.md"))

	// Assert
	refused := msg.GetError().GetInsideTemporaryDirectory()
	if refused == nil {
		t.Fatalf("RegisterRepository = %v, want RegisterRepositoryError.inside_temporary_directory", msg)
	}
	if refused.GetDir() != repo.Dir || refused.GetTemporaryRoot() != root {
		t.Fatalf("refusal = dir %q root %q, want dir %q root %q", refused.GetDir(), refused.GetTemporaryRoot(), repo.Dir, root)
	}
	awaitTypedRefusal(t, d)
}

func TestRegisterWorkspaceRefusesAWorktreeInsideATemporaryFolder(t *testing.T) {
	t.Parallel()
	// Arrange
	repo, root := scratchRepo(t)
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.Client().RegisterWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.RegisterWorkspaceRequest{Dir: repo.Dir}))

	// Assert
	if err != nil {
		t.Fatalf("RegisterWorkspace = transport error %v, want an in-band refusal", err)
	}
	refused := resp.Msg.GetError().GetInsideTemporaryDirectory()
	if refused == nil {
		t.Fatalf("RegisterWorkspace = %v, want RegisterWorkspaceError.inside_temporary_directory", resp.Msg)
	}
	if refused.GetDir() != repo.Dir || refused.GetTemporaryRoot() != root {
		t.Fatalf("refusal = dir %q root %q, want dir %q root %q", refused.GetDir(), refused.GetTemporaryRoot(), repo.Dir, root)
	}
	awaitTypedRefusal(t, d)
}
