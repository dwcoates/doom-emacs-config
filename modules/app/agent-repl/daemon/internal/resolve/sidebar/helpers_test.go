package sidebar_test

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/vocab"
	"claude-repld/internal/wsm"
)

// epoch is the instant every test's timestamps are relative to, so no
// assertion depends on wall-clock.
var epoch = time.UnixMilli(1_700_000_000_000)

// at is epoch plus d, addressable so it can be assigned to a *time.Time field.
func at(d time.Duration) *time.Time {
	t := epoch.Add(d)
	return &t
}

// testColors is the render-colors double: every roster_status arm painted and
// every merge arm glyphed, which is exactly what the resolver asserts. The
// values are irrelevant — the resolver reads the tables' KEYS, never a color.
func testColors() vocab.RenderColors {
	status := map[string]string{}
	for _, arm := range []string{
		"submitting", "thinking", "clearing", "compacting", "permission", "done",
		"interrupted", "turn_failed", "ready", "idle_async", "vendor_blocked", "init", "severed",
		"start_failed", "degraded", "dead", "merge_enqueuing", "merging",
		"merge_queued", "merge_conflict", "merge_failed", "merged", "none",
		"inactive",
	} {
		status[arm] = "grey"
	}
	glyphs := map[string]string{}
	for _, arm := range []string{
		"merge_enqueuing", "merging", "merge_queued", "merge_conflict",
		"merge_failed", "merged",
	} {
		glyphs[arm] = "recycle"
	}
	return vocab.RenderColors{RosterStatus: status, MergeGlyphs: glyphs}
}

// newResolver builds a resolver plus the surfaces its records land in.
func newResolver(t *testing.T) (sidebar.Resolver, *dlog.TestSurfaces) {
	t.Helper()
	surfaces := dlog.NewTestSurfaces()
	r, err := sidebar.New(testColors(), surfaces)
	if err != nil {
		t.Fatalf("sidebar.New: %v", err)
	}
	return r, surfaces
}

// latest reads the roster the resolver last published, failing when it
// published none.
func latest(t *testing.T, r sidebar.Resolver) *frontendv1.WorkspaceRoster {
	t.Helper()
	roster, ok := r.Topic().Latest()
	if !ok {
		t.Fatal("the roster published nothing")
	}
	return roster
}

// subscribe opens a subscription that is torn down with the test.
func subscribe(t *testing.T, r sidebar.Resolver) <-chan *frontendv1.WorkspaceRoster {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	return r.Topic().Subscribe(ctx)
}

// repo is the repository every test workspace belongs to unless it names
// another.
var repo = wsm.Repository{
	ID:            ids.RepoID("repo-1"),
	Dir:           "/repos/one",
	Name:          "alpha",
	DefaultBranch: "master",
}

// workspace builds a registered workspace record with a distinct dir and
// branch derived from its id, cut from the repository's default branch.
func workspace(id, name string) wsm.Workspace {
	return wsm.Workspace{
		ID:           ids.WorkspaceID(id),
		Repo:         repo.ID,
		Dir:          "/repos/one/" + id,
		Name:         name,
		Branch:       "branch/" + id,
		ParentBranch: repo.DefaultBranch,
		CreatedAt:    epoch,
	}
}

// registry builds a snapshot carrying one repository and the given workspaces.
func registry(workspaces ...wsm.Workspace) sidebar.Registry {
	return sidebar.Registry{
		Workspaces:   workspaces,
		Repositories: []wsm.Repository{repo},
	}
}

// prioritized stamps a priority on a workspace.
func prioritized(ws wsm.Workspace, p wsm.Priority) wsm.Workspace {
	ws.Priority = &p
	return ws
}

// repoRows reads the rows of the repository grouping's only section.
func repoRows(t *testing.T, roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	t.Helper()
	sections := roster.GetRepository().GetSections()
	if len(sections) != 1 {
		t.Fatalf("the repository view carried %d sections, want exactly 1", len(sections))
	}
	return sections[0].GetRows().GetRows()
}

// rowNames reads a row list's display names, in the order they render.
func rowNames(rows []*frontendv1.RosterRow) []string {
	out := make([]string, 0, len(rows))
	for _, row := range rows {
		out = append(out, row.GetName().GetText())
	}
	return out
}

// rowFor finds the row for a workspace id anywhere in a row list's tree.
func rowFor(rows []*frontendv1.RosterRow, id string) *frontendv1.RosterRow {
	for _, row := range rows {
		if row.GetWorkspace().GetWorkspace().GetId() == id {
			return row
		}
		if found := rowFor(row.GetChildren(), id); found != nil {
			return found
		}
	}
	return nil
}

// onlyRow reads the repository grouping's single row.
func onlyRow(t *testing.T, r sidebar.Resolver) *frontendv1.RosterRow {
	t.Helper()
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section carried %d rows, want exactly 1", len(rows))
	}
	return rows[0]
}

// statusName names the status arm a row carries, empty when the oneof is unset
// — which is itself a contract breach a test asserts against.
func statusName(row *frontendv1.RosterRow) string {
	switch row.GetStatus().(type) {
	case *frontendv1.RosterRow_Submitting:
		return "submitting"
	case *frontendv1.RosterRow_Thinking:
		return "thinking"
	case *frontendv1.RosterRow_Clearing:
		return "clearing"
	case *frontendv1.RosterRow_Compacting:
		return "compacting"
	case *frontendv1.RosterRow_Permission:
		return "permission"
	case *frontendv1.RosterRow_Done:
		return "done"
	case *frontendv1.RosterRow_Interrupted:
		return "interrupted"
	case *frontendv1.RosterRow_TurnFailed:
		return "turn_failed"
	case *frontendv1.RosterRow_Ready:
		return "ready"
	case *frontendv1.RosterRow_IdleAsync:
		return "idle_async"
	case *frontendv1.RosterRow_VendorBlocked:
		return "vendor_blocked"
	case *frontendv1.RosterRow_Init:
		return "init"
	case *frontendv1.RosterRow_Severed:
		return "severed"
	case *frontendv1.RosterRow_StartFailed:
		return "start_failed"
	case *frontendv1.RosterRow_Degraded:
		return "degraded"
	case *frontendv1.RosterRow_Dead:
		return "dead"
	case *frontendv1.RosterRow_MergeEnqueuing:
		return "merge_enqueuing"
	case *frontendv1.RosterRow_Merging:
		return "merging"
	case *frontendv1.RosterRow_MergeQueued:
		return "merge_queued"
	case *frontendv1.RosterRow_MergeConflict:
		return "merge_conflict"
	case *frontendv1.RosterRow_MergeFailed:
		return "merge_failed"
	case *frontendv1.RosterRow_Merged:
		return "merged"
	case *frontendv1.RosterRow_None:
		return "none"
	case *frontendv1.RosterRow_Inactive:
		return "inactive"
	default:
		return ""
	}
}

// agent is the agent id every session frame in these tests carries.
func agent(id string) *conversationv1.AgentId {
	return &conversationv1.AgentId{Value: id}
}

// permissionAsk builds an OPEN consent ask with the given identity.
func permissionAsk(id string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: id},
		Result: &conversationv1.AgentPermission_Start{
			Start: &conversationv1.AgentPermissionStart{}},
	}
}

// permissionDecided builds the same ask once the gate has been decided.
func permissionDecided(id string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id: &conversationv1.AgentPermissionId{Value: id},
		Result: &conversationv1.AgentPermission_Success{
			Success: &conversationv1.AgentPermissionSuccess{}},
	}
}

// detachedWork builds a detached-work announcement with the given handle.
func detachedWork(id string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: id},
	}
}

// hasError reports whether any captured record was an error under operation.
func hasError(records []dlog.Record, operation string) bool {
	for _, rec := range records {
		if rec.Level == "error" && rec.Operation == operation {
			return true
		}
	}
	return false
}
