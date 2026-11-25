//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/integration/harness"

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

// TestRegisterRepositoryPutsTheMainWorktreeRowOnTheRoster is the whole visible
// consequence of the 2026-09-14 ruling: the repository's section carries the
// main worktree as a workspace row, which is what makes it selectable at all.
// The section used to be drawn EMPTY, and an empty section is exactly what the
// owner reported as "not available for selection".
func TestRegisterRepositoryPutsTheMainWorktreeRowOnTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	d := canonicalView(t)
	repo := harness.NewRepo(t)
	roster := d.WatchRoster()

	// Act
	id := registerRepository(t, d, repo.Dir).GetSuccess().GetRepository().GetId()

	// Assert
	got := awaitRoster(t, d, roster, "a roster carrying the newly registered repository's workspace row",
		func(r *frontendv1.RosterPush) bool {
			return len(rosterwalk.UnsectionedRows(repoSectionByID(r, id))) == 1
		})
	section := repoSectionByID(got, id)
	if section.GetHeader().GetLabel().GetText() == "" {
		t.Fatalf("the new repository's section has no header label: %v", section)
	}
}

// repoSectionByID answers the repository grouping's section for one repository.
func repoSectionByID(r *frontendv1.RosterPush, id string) *frontendv1.RosterRepoSection {
	for _, section := range r.GetWorkspaces().GetRepository().GetSections() {
		if section.GetKey().GetRepository().GetId() == id {
			return section
		}
	}
	return nil
}
