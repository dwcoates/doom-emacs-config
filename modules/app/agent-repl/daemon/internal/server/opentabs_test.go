package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// openBound is how long a test waits on one open's next frame: a small
// multiple of a loopback round trip.
const openBound = 5 * time.Second

// noPage and loadingPage build one workspace of an open.
func noPage(r *workspacev1.WorkspaceRef) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{Workspace: r, Page: &agentreplv1.OpenTabsWorkspace_None{None: &agentreplv1.OpenTabsPageNone{}}}
}

func loadingPage(r *workspacev1.WorkspaceRef) *agentreplv1.OpenTabsWorkspace {
	return &agentreplv1.OpenTabsWorkspace{Workspace: r, Page: &agentreplv1.OpenTabsWorkspace_Loading{Loading: &agentreplv1.OpenTabsPageLoading{}}}
}

// openTabs opens one OpenTabs stream.
func openTabs(t *testing.T, h *harness, ctx context.Context, workspaces ...*agentreplv1.OpenTabsWorkspace) *connect.ServerStreamForClient[agentreplv1.OpenTabsResponse] {
	t.Helper()
	stream, err := h.Client.OpenTabs(ctx, connect.NewRequest(&agentreplv1.OpenTabsRequest{Workspaces: workspaces}))
	if err != nil {
		t.Fatalf("OpenTabs: %v", err)
	}
	return stream
}

// nextOpenFrame reads the open's next frame, failing when the stream ends.
func nextOpenFrame(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.OpenTabsResponse]) *agentreplv1.OpenTabsResponse {
	t.Helper()
	if !stream.Receive() {
		t.Fatalf("the open ended early: %v", stream.Err())
	}
	return stream.Msg()
}

// openedOf reads frames until the first `opened`, answering it.
func openedOf(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.OpenTabsResponse]) *agentreplv1.OpenTabsOpened {
	t.Helper()
	for {
		if opened := nextOpenFrame(t, stream).GetOpened(); opened != nil {
			return opened
		}
	}
}

// openError answers the error an open ends with before any frame.
func openError(t *testing.T, h *harness, workspaces ...*agentreplv1.OpenTabsWorkspace) error {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), openBound)
	defer cancel()
	stream := openTabs(t, h, ctx, workspaces...)
	if stream.Receive() {
		t.Fatalf("the open sent %v, want it refused", stream.Msg())
	}
	return stream.Err()
}

func TestOpenTabsRefusesAMalformedRequest(t *testing.T) {
	cases := []struct {
		name       string
		workspaces []*agentreplv1.OpenTabsWorkspace
	}{
		{name: "no workspace", workspaces: nil},
		{name: "a workspace with no ref", workspaces: []*agentreplv1.OpenTabsWorkspace{noPage(nil)}},
		{name: "a workspace with no page arm", workspaces: []*agentreplv1.OpenTabsWorkspace{{Workspace: ref()}}},
		{name: "a workspace named twice", workspaces: []*agentreplv1.OpenTabsWorkspace{noPage(ref()), noPage(ref())}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			err := openError(t, h, tc.workspaces...)

			// Assert
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
		})
	}
}

func TestOpenTabsRefusesAWorkspaceItWillNotOpen(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness) *workspacev1.WorkspaceRef
		want    func(*agentreplv1.OpenTabsOpenedRefused) bool
	}{
		{
			name:    "an unknown workspace",
			arrange: func(*harness) *workspacev1.WorkspaceRef { return &workspacev1.WorkspaceRef{Id: "nobody"} },
			want:    func(r *agentreplv1.OpenTabsOpenedRefused) bool { return r.GetUnknownWorkspace() != nil },
		},
		{
			name: "a ref whose dir disagrees",
			arrange: func(*harness) *workspacev1.WorkspaceRef {
				return &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"}
			},
			want: func(r *agentreplv1.OpenTabsOpenedRefused) bool {
				return r.GetWorkspaceRefMismatch().GetRegistryDir() == testWorkspaceDir
			},
		},
		{
			name: "a workspace handed to a successor",
			arrange: func(h *harness) *workspacev1.WorkspaceRef {
				h.Ownership.standing = workspace.StandingTransferringAway
				return ref()
			},
			want: func(r *agentreplv1.OpenTabsOpenedRefused) bool {
				return r.GetTransferringAway().GetAddress() == "127.0.0.1:9999"
			},
		},
		{
			name: "a workspace not yet adopted",
			arrange: func(h *harness) *workspacev1.WorkspaceRef {
				h.Ownership.standing = workspace.StandingNotYetAdopted
				return ref()
			},
			want: func(r *agentreplv1.OpenTabsOpenedRefused) bool { return r.GetNotYetAdopted() != nil },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			asked := tc.arrange(h)
			ctx, cancel := context.WithTimeout(context.Background(), openBound)
			defer cancel()

			// Act
			opened := openedOf(t, openTabs(t, h, ctx, noPage(asked)))

			// Assert
			if !tc.want(opened.GetRefused()) {
				t.Fatalf("opened = %v, want the refusal", opened)
			}
		})
	}
}

func TestOpenTabsAnswersAWorkspaceThatIsNotVisibleHidden(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "a hidden workspace", arrange: func(h *harness) {
			record := h.DB.workspaces[testWorkspaceID]
			hiddenAt := time.Unix(1_700_000_000, 0)
			record.Hidden, record.HiddenAt = true, &hiddenAt
			h.DB.workspaces[testWorkspaceID] = record
		}},
		{name: "a workspace in a collapsed section", arrange: func(h *harness) {
			record := h.DB.workspaces[testWorkspaceID]
			record.Repo = "r"
			h.DB.workspaces[testWorkspaceID] = record
			h.DB.repositories = []wsm.Repository{{ID: "r", Folded: true}}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tc.arrange(h)
			ctx, cancel := context.WithTimeout(context.Background(), openBound)
			defer cancel()

			// Act
			opened := openedOf(t, openTabs(t, h, ctx, loadingPage(ref())))

			// Assert
			if opened.GetHidden() == nil {
				t.Fatalf("opened = %v, want hidden", opened)
			}
		})
	}
}

func TestOpenTabsEndsWithTheErrorWhenTheFoldsCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.DB.listRepositoriesErr = errors.New("arranged: the store is gone")

	// Act
	err := openError(t, h, noPage(ref()))

	// Assert
	if code := connectCode(t, err); code != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", code)
	}
}

func TestOpenTabsOpensAWorkspaceWithNoPageOnceRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithTimeout(context.Background(), openBound)
	defer cancel()
	stream := openTabs(t, h, ctx, noPage(ref()))

	// Act
	read := nextOpenFrame(t, stream).GetStep().GetConversationRead()
	opened := nextOpenFrame(t, stream).GetOpened()
	finished := nextOpenFrame(t, stream).GetFinished()

	// Assert
	if read == nil || opened.GetReady() == nil || finished.GetReady() != 1 || finished.GetTotal() != 1 {
		t.Fatalf("frames = conversation read %v, opened %v, finished %v", read, opened, finished)
	}
	if stream.Receive() {
		t.Fatalf("a frame followed the finish: %v", stream.Msg())
	}
}

func TestOpenTabsWaitsForTheWorkspacesPageToConnectAndDraw(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithTimeout(context.Background(), openBound)
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	stream := openTabs(t, h, ctx, loadingPage(ref()))
	if nextOpenFrame(t, stream).GetStep().GetConversationRead() == nil {
		t.Fatal("the first frame was not the conversation read")
	}

	// Act: the page's footer subscription names the workspace, then it draws.
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "footer-1",
		Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()}},
	})
	connected := nextOpenFrame(t, stream).GetStep().GetPageConnected()
	if _, err := h.Client.ReportPageDrawn(ctx, connect.NewRequest(&agentreplv1.ReportPageDrawnRequest{Workspace: ref(), Page: "page-1"})); err != nil {
		t.Fatalf("ReportPageDrawn: %v", err)
	}

	// Assert
	drawn := nextOpenFrame(t, stream).GetStep().GetPageDrawn()
	opened := nextOpenFrame(t, stream).GetOpened()
	if connected == nil || drawn == nil || opened.GetReady() == nil {
		t.Fatalf("frames = connected %v, drawn %v, opened %v", connected, drawn, opened)
	}
}

func TestATopbarSubscriptionServesItsWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithTimeout(context.Background(), openBound)
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")
	stream := openTabs(t, h, ctx, loadingPage(ref()))
	nextOpenFrame(t, stream) // conversation read

	// Act
	subscribePage(t, ctx, h, &agentreplv1.SubscribePageRequest{
		Page: "page-1", Subscription: "topbar-1",
		Request: &agentreplv1.SubscribePageRequest_Topbar{Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()}},
	})

	// Assert
	if nextOpenFrame(t, stream).GetStep().GetPageConnected() == nil {
		t.Fatal("a topbar subscription did not connect the workspace's page")
	}
}

func TestPageSubscriptionWorkspaceNamesOnlyAWorkspacesConversationSurfaces(t *testing.T) {
	cases := []struct {
		name string
		req  *agentreplv1.SubscribePageRequest
		want bool
	}{
		{"web workspace", &agentreplv1.SubscribePageRequest{Request: &agentreplv1.SubscribePageRequest_WebWorkspace{WebWorkspace: &agentreplv1.WatchWebWorkspaceRequest{Workspace: ref()}}}, true},
		{"topbar", &agentreplv1.SubscribePageRequest{Request: &agentreplv1.SubscribePageRequest_Topbar{Topbar: &agentreplv1.WatchTopbarRequest{Workspace: ref()}}}, true},
		{"footer", &agentreplv1.SubscribePageRequest{Request: &agentreplv1.SubscribePageRequest_Footer{Footer: &agentreplv1.WatchFooterRequest{Workspace: ref()}}}, true},
		{"roster", &agentreplv1.SubscribePageRequest{Request: &agentreplv1.SubscribePageRequest_Roster{Roster: &agentreplv1.WatchWorkspaceRosterRequest{}}}, false},
		{"login terminal", &agentreplv1.SubscribePageRequest{Request: &agentreplv1.SubscribePageRequest_LoginTerminal{LoginTerminal: &agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}}}, false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			ws, ok := pageSubscriptionWorkspace(tc.req)

			// Assert
			if ok != tc.want || (ok && ws != testWorkspaceID) {
				t.Fatalf("pageSubscriptionWorkspace = %q, %v; want %v", ws, ok, tc.want)
			}
		})
	}
}

func TestReportPageDrawnRefusesAMalformedRequest(t *testing.T) {
	cases := []struct {
		name string
		req  *agentreplv1.ReportPageDrawnRequest
	}{
		{name: "no workspace", req: &agentreplv1.ReportPageDrawnRequest{Page: "page-1"}},
		{name: "a blank page", req: &agentreplv1.ReportPageDrawnRequest{Workspace: ref()}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.ReportPageDrawn(context.Background(), connect.NewRequest(tc.req))

			// Assert
			if code := connectCode(t, err); code != connect.CodeInvalidArgument {
				t.Fatalf("code = %v, want InvalidArgument", code)
			}
		})
	}
}

func TestReportPageDrawnAnswersItsRefusals(t *testing.T) {
	cases := []struct {
		name string
		ref  *workspacev1.WorkspaceRef
		want func(*agentreplv1.ReportPageDrawnError) bool
	}{
		{
			name: "an unknown workspace",
			ref:  &workspacev1.WorkspaceRef{Id: "nobody"},
			want: func(e *agentreplv1.ReportPageDrawnError) bool { return e.GetUnknownWorkspace() != nil },
		},
		{
			name: "a ref whose dir disagrees",
			ref:  &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"},
			want: func(e *agentreplv1.ReportPageDrawnError) bool {
				return e.GetWorkspaceRefMismatch().GetRegistryDir() == testWorkspaceDir
			},
		},
		{
			name: "a page no stream holds",
			ref:  ref(),
			want: func(e *agentreplv1.ReportPageDrawnError) bool { return e.GetUnknownPage() != nil },
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			resp, err := h.Client.ReportPageDrawn(context.Background(), connect.NewRequest(&agentreplv1.ReportPageDrawnRequest{Workspace: tc.ref, Page: "page-1"}))

			// Assert
			if err != nil {
				t.Fatalf("ReportPageDrawn: %v", err)
			}
			if !tc.want(resp.Msg.GetError()) {
				t.Fatalf("answer = %v, want the refusal", resp.Msg)
			}
		})
	}
}

func TestReportPageDrawnRecordsAnAttachedPage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithTimeout(context.Background(), openBound)
	defer cancel()
	attachPageStream(t, h, ctx, "page-1")

	// Act
	resp, err := h.Client.ReportPageDrawn(ctx, connect.NewRequest(&agentreplv1.ReportPageDrawnRequest{Workspace: ref(), Page: "page-1"}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("ReportPageDrawn = %v, %v; want success", resp, err)
	}
}
