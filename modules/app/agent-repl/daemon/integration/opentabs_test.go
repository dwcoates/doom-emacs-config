//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
)

// noPageOpen and loadingPageOpen name one workspace of an open.
func noPageOpen(ws *workspacev1.WorkspaceRef) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{Workspace: ws, Page: &agentreplv1.OpenTabsWorkspace_None{None: &agentreplv1.OpenTabsPageNone{}}}
}

func loadingPageOpen(ws *workspacev1.WorkspaceRef) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{Workspace: ws, Page: &agentreplv1.OpenTabsWorkspace_Loading{Loading: &agentreplv1.OpenTabsPageLoading{}}}
}

// openLog reads an open until its finish, answering every frame in order.
func openLog(t *testing.T, d *harness.Daemon, s *harness.Stream[*agentreplv1.OpenTabsResponse]) []*agentreplv1.OpenTabsResponse {
	t.Helper()
	var frames []*agentreplv1.OpenTabsResponse
	ctx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, ctx, s, "the open's finish", func(r *agentreplv1.OpenTabsResponse) bool {
		frames = append(frames, r)
		return r.GetFinished() != nil
	})
	return frames
}

// openedOutcomes maps each answered workspace to its outcome arm's name.
func openedOutcomes(frames []*agentreplv1.OpenTabsResponse) map[string]string {
	out := map[string]string{}
	for _, f := range frames {
		o := f.GetOpened()
		if o == nil {
			continue
		}
		switch {
		case o.GetReady() != nil:
			out[o.GetWorkspace().GetId()] = "ready"
		case o.GetFailed() != nil:
			out[o.GetWorkspace().GetId()] = "failed"
		case o.GetHidden() != nil:
			out[o.GetWorkspace().GetId()] = "hidden"
		case o.GetRefused() != nil:
			out[o.GetWorkspace().GetId()] = "refused"
		}
	}
	return out
}

// hide moves a workspace into the hidden section.
func hide(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	if _, err := d.Client().SetWorkspaceHidden(d.Ctx(), connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{
		Workspace: ws, Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Hide{Hide: &agentreplv1.SetWorkspaceHiddenHide{}},
	})); err != nil {
		t.Fatalf("SetWorkspaceHidden(hide) = error %v", err)
	}
}

func TestOpenTabsBringsEachWorkspaceUpAndAnswersItReady(t *testing.T) {
	t.Parallel()
	// Arrange: two registered workspaces, neither opened.
	d := newDaemon(t, harness.Opts{})
	first := harness.Register(t, d, harness.NewRepo(t).Dir)
	second := harness.Register(t, d, harness.NewRepo(t).Dir)

	// Act
	frames := openLog(t, d, d.OpenTabs(noPageOpen(first), noPageOpen(second)))

	// Assert
	want := map[string]string{first.GetId(): "ready", second.GetId(): "ready"}
	if got := openedOutcomes(frames); len(got) != 2 || got[first.GetId()] != "ready" || got[second.GetId()] != "ready" {
		t.Fatalf("outcomes = %v, want %v", got, want)
	}
	if done := frames[len(frames)-1].GetFinished(); done.GetReady() != 2 || done.GetTotal() != 2 || len(done.GetFailed()) != 0 {
		t.Fatalf("finished = %v, want 2 of 2 ready", done)
	}
}

func TestOpenTabsAnswersAHiddenWorkspaceHiddenAndStartsNothing(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	hide(t, d, ws)

	// Act
	frames := openLog(t, d, d.OpenTabs(loadingPageOpen(ws)))

	// Assert
	if got := openedOutcomes(frames); got[ws.GetId()] != "hidden" {
		t.Fatalf("outcomes = %v, want hidden", got)
	}
	for _, f := range frames {
		if f.GetStep() != nil {
			t.Fatalf("a hidden workspace was told a step %v; nothing should have started", f.GetStep())
		}
	}
}

func TestOpenTabsWaitsForTheWorkspacesPageToDraw(t *testing.T) {
	t.Parallel()
	// Arrange: an open whose editor is loading the workspace's page.
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)
	ctx := d.Ctx()
	pageStream, err := d.Client().WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-1"}))
	if err != nil {
		t.Fatalf("WatchPage = error %v", err)
	}
	frames := pageFrames(t, ctx, pageStream)
	awaitPageFrame(t, ctx, frames, "the attachment latch")
	open := d.OpenTabs(loadingPageOpen(ws))
	waitCtx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, waitCtx, open, "the workspace served", func(r *agentreplv1.OpenTabsResponse) bool {
		if r.GetOpened() != nil {
			t.Fatalf("the workspace opened %v before its page drew", r.GetOpened())
		}
		return r.GetStep().GetServed() != nil
	})

	// Act: the page watches the workspace's footer, then reports it drawn.
	if _, err := d.Client().SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer",
		Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ws}},
	})); err != nil {
		t.Fatalf("SubscribePage = error %v", err)
	}
	harness.AwaitView(t, waitCtx, open, "the page connected", func(r *agentreplv1.OpenTabsResponse) bool {
		return r.GetStep().GetPageConnected() != nil
	})
	drawn, err := d.Client().ReportPageDrawn(ctx, connect.NewRequest(&agentreplv1.ReportPageDrawnRequest{Workspace: ws, Page: "page-1"}))
	if err != nil || drawn.Msg.GetSuccess() == nil {
		t.Fatalf("ReportPageDrawn = (%v, %v), want success", drawn, err)
	}

	// Assert
	harness.AwaitView(t, waitCtx, open, "the page drawn", func(r *agentreplv1.OpenTabsResponse) bool {
		return r.GetStep().GetPageDrawn() != nil
	})
	opened := harness.AwaitView(t, waitCtx, open, "the workspace opened", func(r *agentreplv1.OpenTabsResponse) bool {
		return r.GetOpened() != nil
	})
	if opened.GetOpened().GetReady() == nil {
		t.Fatalf("opened = %v, want ready", opened.GetOpened())
	}
}

func TestARosterRowIsOpeningWhileItsOpenIsInFlight(t *testing.T) {
	t.Parallel()
	// Arrange: an open waiting on a page that never draws.
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)
	roster := d.WatchRoster()

	// Act
	open := d.OpenTabs(loadingPageOpen(ws))

	// Assert
	awaitRoster(t, d, roster, "the row opening", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, ws.GetId()).GetOpening() != nil
	})
	open.Close()
	awaitRoster(t, d, roster, "the row no longer opening once the open ended", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, ws.GetId())
		return row != nil && row.GetOpening() == nil
	})
}

// standDownWarnings are the records a stand-down's KillSession leaves, which
// the fake shim answers by exiting (as the idle sweep's own tests declare).
var standDownWarnings = []string{
	"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
	"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
	"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
}

func TestHidingAWorkspaceStandsItsSessionDown(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(standDownWarnings...)
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})

	// Act
	hide(t, f.d, f.ws)

	// Assert: the idle sweep's stand-down, graceful.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if killed.GetForce() {
		t.Fatal("the hidden workspace's KillSession was forced, want a graceful stand-down")
	}
}

func TestCollapsingARepositoryStandsItsWorkspacesSessionDown(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(standDownWarnings...)
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})

	// Act
	if _, err := f.d.Client().UpdateSidebarView(f.d.Ctx(), connect.NewRequest(foldSection(&agentreplv1.SidebarViewFoldSection{
		Section: &agentreplv1.SidebarViewFoldSection_Repository{Repository: &workspacev1.RepositoryRef{Dir: f.repo.Dir}},
		Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
	}))); err != nil {
		t.Fatalf("UpdateSidebarView(collapse) = error %v", err)
	}

	// Assert
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})
}

func TestANewEmacsStreamStartsNoWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace nothing has opened, its roster drawn.
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the registered workspace's row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, ws.GetId()) != nil
	})

	// Act: a new Emacs connects.
	stream := d.WatchDaemonStream()
	harness.AwaitView(t, d.Ctx(), stream, "the persistent-wifi standing", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetPersistentWifi() != nil
	})

	// Assert: nothing is brought up; the editor asks with OpenTabs.
	harness.ExpectNoPush(t, roster, harness.ProbeWindow, "a roster change from a bring-up nobody asked for")
}

func TestANetworkTheShimCannotReachIsANetworkFaultOnTheFooterAndTheRoster(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpenedWithProfile(t, harness.Opts{}, harness.ShimProfile{
		OpeningNetworkUnreachable: "cannot reach api.anthropic.com: no route to host",
	})

	// Act
	footer := f.d.WatchFooter(f.ws)

	// Assert
	view := awaitFooter(t, f, footer, "the footer's network fault", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetNetworkFault().GetOffline() != nil
	})
	if got := view.GetStrip().GetStatus().GetNetworkFault().GetActivity().GetSalient().GetOffline().GetText(); got != "cannot reach api.anthropic.com: no route to host" {
		t.Fatalf("offline line = %q, want the shim's own observation", got)
	}
	awaitRoster(t, f.d, f.d.WatchRoster(), "the roster's network fault", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetNetworkFault() != nil
	})
}
