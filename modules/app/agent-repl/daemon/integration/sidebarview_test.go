//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// updateSidebarView sends one UpdateSidebarView and answers its response,
// failing the test on a transport error.
func updateSidebarView(t *testing.T, f *fixture, req *agentreplv1.UpdateSidebarViewRequest) *agentreplv1.UpdateSidebarViewResponse {
	t.Helper()
	resp, err := f.d.Client().UpdateSidebarView(f.d.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("UpdateSidebarView = error %v, want an in-band answer", err)
	}
	return resp.Msg
}

func foldSection(section *agentreplv1.SidebarViewFoldSection) *agentreplv1.UpdateSidebarViewRequest {
	return &agentreplv1.UpdateSidebarViewRequest{
		Change: &agentreplv1.UpdateSidebarViewRequest_FoldSection{FoldSection: section},
	}
}

func collapseMergedBand() *agentreplv1.UpdateSidebarViewRequest {
	return foldSection(&agentreplv1.SidebarViewFoldSection{
		Section: &agentreplv1.SidebarViewFoldSection_RecentlyMerged{RecentlyMerged: &agentreplv1.SidebarViewRecentlyMerged{}},
		Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
	})
}

func mergedBandCollapsed(r *frontendv1.WorkspaceRoster) bool {
	return r.GetRecentlyMerged().GetCollapsed() != nil
}

// The band opens expanded until someone folds it (owner ruling, 2026-10-08).
func TestTheRecentlyMergedBandStartsExpanded(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	first := harness.AwaitNext(t, f.d.Ctx(), f.d.WatchRoster(), "the opening roster")

	// Assert
	if first.GetRecentlyMerged().GetExpanded() == nil {
		t.Fatalf("the band's fold before anyone folded it = %v, want expanded", first.GetRecentlyMerged().GetFold())
	}
}

func TestTheSidebarViewSurvivesADaemonRestart(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{DurableState: true})
	roster := f.d.WatchRoster()
	updateSidebarView(t, f, collapseMergedBand())
	awaitRoster(t, f.d, roster, "the folded band, before the restart", mergedBandCollapsed)

	// Act
	nd := promptRestartDaemon(t, f)

	// Assert
	got := harness.AwaitNext(t, nd.Ctx(), nd.WatchRoster(), "the roster after the restart")
	if !mergedBandCollapsed(got) {
		t.Fatalf("band fold after restart = %v, want collapsed", got.GetRecentlyMerged().GetFold())
	}
}
