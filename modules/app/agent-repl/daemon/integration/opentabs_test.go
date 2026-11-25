//go:build integration

package integration

import (
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/integration/harness"
)

func loadingPageOpen(ws *workspacev1.WorkspaceRef) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{Workspace: ws, Page: &agentreplv1.OpenTabsWorkspace_Loading{Loading: &agentreplv1.OpenTabsPageLoading{}}}
}

// hide moves a workspace into the backlog.
func hide(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	if _, err := d.Client().SetWorkspaceBacklogged(d.Ctx(), connect.NewRequest(&agentreplv1.SetWorkspaceBackloggedRequest{
		Workspace: ws, Visibility: &agentreplv1.SetWorkspaceBackloggedRequest_Hide{Hide: &agentreplv1.SetWorkspaceBackloggedHide{}},
	})); err != nil {
		t.Fatalf("SetWorkspaceBacklogged(hide) = error %v", err)
	}
}

func TestOpenTabsWaitsForTheWorkspacesPageToDraw(t *testing.T) {
	t.Parallel()
	// Arrange: an open whose editor is loading the workspace's page.
	d := canonicalView(t)
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)
	ctx := d.Ctx()
	pageStream, err := d.Client().WatchPage(ctx, connect.NewRequest(&agentreplv1.WatchPageRequest{Page: "page-1"}))
	if err != nil {
		t.Fatalf("WatchPage = error %v", err)
	}
	frames := pageFrames(t, ctx, pageStream)
	attachment := awaitPageFrame(t, ctx, frames, "the attachment latch").GetAttached().GetAttachment()
	open := d.OpenTabs(loadingPageOpen(ws))
	waitCtx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, waitCtx, open, "the conversation read", func(r *agentreplv1.OpenTabsResponse) bool {
		if r.GetOpened() != nil {
			t.Fatalf("the workspace opened %v before its page drew", r.GetOpened())
		}
		return r.GetStep().GetConversationRead() != nil
	})

	// Act: the page watches the workspace's footer, then reports it drawn.
	if _, err := d.Client().SubscribePage(ctx, connect.NewRequest(&agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer", Attachment: attachment,
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

// standDownWarnings are the records a stand-down's KillSession leaves, which
// the fake shim answers by exiting (as the idle sweep's own tests declare).
var standDownWarnings = []string{
	"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
	"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
	"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
}

// collapseRepository collapses F's repository section, as the sidebar's fold
// gesture asks it.
func collapseRepository(t *testing.T, f *fixture) {
	t.Helper()
	if _, err := f.d.Client().UpdateSidebarView(f.d.Ctx(), connect.NewRequest(foldSection(&agentreplv1.SidebarViewFoldSection{
		Section: &agentreplv1.SidebarViewFoldSection_Repository{Repository: &workspacev1.RepositoryRef{Dir: f.repo.Dir}},
		Fold:    &agentreplv1.SidebarViewFoldSection_Collapse{Collapse: &agentreplv1.SidebarViewCollapse{}},
	}))); err != nil {
		t.Fatalf("UpdateSidebarView(collapse) = error %v", err)
	}
}

func TestCollapsingARepositoryStandsItsWorkspacesSessionDown(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{FoldStandDownDelay: time.Millisecond})
	f.d.ExpectWarnings(standDownWarnings...)
	f.remoteControlOff()
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})

	// Act
	collapseRepository(t, f)

	// Assert
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})
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
	awaitRoster(t, f.d, f.d.WatchRoster(), "the roster's network fault", func(r *frontendv1.RosterPush) bool {
		return rosterRow(r, f.ws.GetId()).GetNetworkFault() != nil
	})
}

// cutWhileBacklogged runs a turn, hides the workspace under it (the stand-down
// waits for the turn), and stands the daemon down, which cuts the turn: the
// successor over the same state boots with a backlogged workspace owed a cut turn.
func cutWhileBacklogged(t *testing.T) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{DurableState: true})
	// BACKLOGGED FIRST: a backlog interrupts a running turn (owner ruling,
	// 2026-11-24), so the turn the shutdown cuts is one a prompt started on
	// the already backlogged workspace.
	hide(t, f.d, f.ws)
	f.shim.ExpectStartSession()
	f.submit("do the long thing", "k-cut", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	shutdownNow(t, f.d)
	// The fake shim writes no vendor transcript, so the successor's resume
	// finds none and says so; a real session's transcript is on disk.
	f.d = harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExpectWarnings: []string{"daemon.workspace.bring_up"}})
	return f
}

// TestABackloggedWorkspacesCutTurnIsResumedByTheSuccessorsBoot: an open starts no
// shim, and the backlogged workspace's shim was up when its turn was cut, so the
// successor's boot brings it back (bringup.OnlyWasUp) and the resumption is
// the turn it starts.
func TestABackloggedWorkspacesCutTurnIsResumedByTheSuccessorsBoot(t *testing.T) {
	t.Parallel()
	// Arrange
	f := cutWhileBacklogged(t)

	// Act: nothing opens it.
	st := &shimv1.StartTurnRequest{}

	// Assert: the successor's shim is asked to start the resumption.
	f.d.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCStartTurn, "the resumption's StartTurn", st, func() bool {
		return st.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART
	})
}
