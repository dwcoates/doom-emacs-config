//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
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

// stepIndex answers the index in FRAMES of the first step of WS that IS
// reports, -1 when there is none.
func stepIndex(frames []*agentreplv1.OpenTabsResponse, ws string, is func(*agentreplv1.OpenTabsStep) bool) int {
	for i, f := range frames {
		if step := f.GetStep(); step != nil && step.GetWorkspace().GetId() == ws && is(step) {
			return i
		}
	}
	return -1
}

// openedIndex answers the index in FRAMES of WS's `opened`, -1 when none.
func openedIndex(frames []*agentreplv1.OpenTabsResponse, ws string) int {
	for i, f := range frames {
		if f.GetOpened().GetWorkspace().GetId() == ws {
			return i
		}
	}
	return -1
}

func TestOpenTabsAnswersReadyOnlyAfterTheConversationIsRead(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)

	// Act
	frames := openLog(t, d, d.OpenTabs(noPageOpen(ws)))

	// Assert: served, then read, then ready.
	served := stepIndex(frames, ws.GetId(), func(s *agentreplv1.OpenTabsStep) bool { return s.GetServed() != nil })
	read := stepIndex(frames, ws.GetId(), func(s *agentreplv1.OpenTabsStep) bool { return s.GetConversationRead() != nil })
	opened := openedIndex(frames, ws.GetId())
	if served < 0 || read < served || opened < read || frames[opened].GetOpened().GetReady() == nil {
		t.Fatalf("served at %d, conversation_read at %d, ready at %d; want them in that order", served, read, opened)
	}
}

func TestOpenTabsReadyCarriesTheWorkspacesRosterRow(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)

	// Act
	frames := openLog(t, d, d.OpenTabs(noPageOpen(ws)))

	// Assert
	ready := frames[openedIndex(frames, ws.GetId())].GetOpened().GetReady()
	if ready.GetRow().GetWorkspace().GetWorkspace().GetId() != ws.GetId() || ready.GetRosterRevision() == 0 {
		t.Fatalf("ready = %v, want the workspace's row with its roster's revision", ready)
	}
	if ready.GetRow().GetOpening() != nil {
		t.Fatal("the answer's row still draws the workspace opening")
	}
}

func TestOpenTabsSendsNoResumeStepForAWorkspaceOwedNothing(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	ws := harness.Register(t, d, harness.NewRepo(t).Dir)

	// Act
	frames := openLog(t, d, d.OpenTabs(noPageOpen(ws)))

	// Assert
	if at := stepIndex(frames, ws.GetId(), func(s *agentreplv1.OpenTabsStep) bool { return s.GetResumingCutTurn() != nil }); at >= 0 {
		t.Fatalf("frame %d told a cut turn resuming; nothing was owed", at)
	}
}

// show moves a hidden workspace back into its section.
func show(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	if _, err := d.Client().SetWorkspaceHidden(d.Ctx(), connect.NewRequest(&agentreplv1.SetWorkspaceHiddenRequest{
		Workspace: ws, Visibility: &agentreplv1.SetWorkspaceHiddenRequest_Show{Show: &agentreplv1.SetWorkspaceHiddenShow{}},
	})); err != nil {
		t.Fatalf("SetWorkspaceHidden(show) = error %v", err)
	}
}

// cutWhileHidden runs a turn, hides the workspace under it (the stand-down
// waits for the turn), and stands the daemon down, which cuts the turn: the
// successor over the same state boots with a hidden workspace owed a cut turn.
func cutWhileHidden(t *testing.T) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	f.submit("do the long thing", "k-cut", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	hide(t, f.d, f.ws)
	shutdownNow(t, f.d)
	f.d = harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	// The fake shim writes no vendor transcript, so the successor's resume
	// finds none and says so; a real session's transcript is on disk.
	f.d.ExpectWarnings("daemon.workspace.bring_up")
	return f
}

func TestAHiddenWorkspacesCutTurnIsResumedInsideItsLaterOpen(t *testing.T) {
	t.Parallel()
	// Arrange: the successor leaves the hidden workspace down; it is shown.
	f := cutWhileHidden(t)
	show(t, f.d, f.ws)

	// Act
	frames := openLog(t, f.d, f.d.OpenTabs(noPageOpen(f.ws)))

	// Assert: the resume is told, and ready follows it.
	resuming := stepIndex(frames, f.ws.GetId(), func(s *agentreplv1.OpenTabsStep) bool { return s.GetResumingCutTurn() != nil })
	opened := openedIndex(frames, f.ws.GetId())
	if resuming < 0 || opened < resuming || frames[opened].GetOpened().GetReady() == nil {
		t.Fatalf("resuming_cut_turn at %d, ready at %d; want the resume told before ready", resuming, opened)
	}
}

func TestTheResumedTurnHasStartedWhenTheOpenAnswersReady(t *testing.T) {
	t.Parallel()
	// Arrange
	f := cutWhileHidden(t)
	show(t, f.d, f.ws)

	// Act
	openLog(t, f.d, f.d.OpenTabs(noPageOpen(f.ws)))

	// Assert: the shim already holds the resumption's StartTurn.
	shim := f.d.OpenedShim(f.ws)
	st := &shimv1.StartTurnRequest{}
	if n := shim.Count(harness.RPCStartTurn); n != 1 {
		t.Fatalf("the shim was asked to start %d turns by the time the open answered, want the resumption", n)
	}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartTurn, st)
	if st.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("the started turn has origin %v, want the resumption after restart", st.GetOrigin())
	}
}
