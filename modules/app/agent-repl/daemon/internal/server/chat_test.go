package server

import (
	"context"
	"errors"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	zettelv1 "agentrepl/proto/zettel/v1"

	"agent-repld/internal/chat"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/publish"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// THE CHAT VERBS, over the real transport.

const testThreadID = ids.WorkspaceID("thread-1")

// fakeChat is the chat service, scripted.
type fakeChat struct {
	topics    map[ids.WorkspaceID]*publish.Topic[*frontendv1.ChatView]
	openErr   error
	parent    *zettelv1.NodeParent
	askErr    error
	gestErr   error
	asked     []*zettelv1.NodeParent
	switches  []string
	selects   []*chat.Highlight
	sightings [][]chat.NodeID
}

func (f *fakeChat) Topic(ws ids.WorkspaceID) *publish.Topic[*frontendv1.ChatView] {
	if f.topics == nil {
		f.topics = map[ids.WorkspaceID]*publish.Topic[*frontendv1.ChatView]{}
	}
	if f.topics[ws] == nil {
		f.topics[ws] = publish.NewLatestOnly[*frontendv1.ChatView]()
	}
	return f.topics[ws]
}

func (f *fakeChat) Open(_ context.Context, ws ids.WorkspaceID) error {
	if f.openErr != nil {
		return f.openErr
	}
	f.Topic(ws).Publish(&frontendv1.ChatView{State: &frontendv1.ChatView_Empty{Empty: &frontendv1.ChatViewEmpty{
		Hint: &frontendv1.ChatViewEmptyHint{Text: chat.EmptyHint},
	}}})
	return nil
}

func (f *fakeChat) Ask(context.Context, ids.WorkspaceID) (*zettelv1.NodeParent, error) {
	return f.parent, f.askErr
}

func (f *fakeChat) Asked(_ context.Context, _ ids.WorkspaceID, asked *zettelv1.NodeParent) error {
	f.asked = append(f.asked, asked)
	return nil
}

func (f *fakeChat) SwitchSibling(_ context.Context, _ ids.WorkspaceID, shown chat.NodeID, way chat.Direction) error {
	word := "next"
	if way == chat.Previous {
		word = "previous"
	}
	f.switches = append(f.switches, string(shown)+":"+word)
	return f.gestErr
}

func (f *fakeChat) Select(_ context.Context, _ ids.WorkspaceID, h *chat.Highlight) error {
	f.selects = append(f.selects, h)
	return f.gestErr
}

func (f *fakeChat) Sightings(_ context.Context, _ ids.WorkspaceID, nodes []chat.NodeID) error {
	f.sightings = append(f.sightings, nodes)
	return f.gestErr
}

// withThread registers a thread beside the harness's agent workspace.
func withThread(h *harness) *workspacev1.WorkspaceRef {
	h.DB.workspaces[testThreadID] = wsm.Workspace{ID: testThreadID, Kind: wsm.WorkspaceKindThread, Dir: "/zettel/threads/why", Name: "why"}
	return &workspacev1.WorkspaceRef{Id: string(testThreadID), Dir: "/zettel/threads/why"}
}

func nodeParent(node, quote string) *zettelv1.NodeParent {
	of := &zettelv1.NodeParentNode{Node: &zettelv1.NodeId{Id: node}, Revision: &zettelv1.RevisionNumber{Number: 1}}
	if quote != "" {
		of.Quote = &zettelv1.NodeQuote{Text: quote}
	}
	return &zettelv1.NodeParent{Origin: &zettelv1.NodeParent_Node{Node: of}}
}

func TestCreateThreadCreatesAndSubmitsTheFirstQuestion(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Verbs.createThread = wsm.Workspace{ID: testThreadID, Kind: wsm.WorkspaceKindThread, Dir: "/zettel/threads/why", Name: "why"}
	h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-1"}

	// Act
	resp, err := h.Client.CreateThread(context.Background(), connect.NewRequest(&agentreplv1.CreateThreadRequest{
		Question: &agentreplv1.CreateThreadQuestion{Text: "why is the sky blue?"},
	}))

	// Assert
	if err != nil {
		t.Fatalf("CreateThread: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetThread().GetId(); got != string(testThreadID) {
		t.Fatalf("thread = %q, want %q", got, testThreadID)
	}
	subs := h.Prompts.threadSubmissions
	if len(subs) != 1 || subs[0].ask.Parent.GetThreadStart() == nil || subs[0].key != "create-thread:"+string(testThreadID) ||
		subs[0].origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT {
		t.Fatalf("submissions = %+v, want one thread-start question", subs)
	}
}

func TestCreateThreadRefusals(t *testing.T) {
	cases := []struct {
		name     string
		question string
		verbErr  error
		code     connect.Code
		arm      bool
	}{
		{name: "a blank question is invalid", question: " ", code: connect.CodeInvalidArgument},
		{name: "a refused creation answers its arm", question: "why?", verbErr: &workspace.Refusal{Arm: workspace.ArmCreationFailed, Reason: "no name"}, arm: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.createThreadErr = tc.verbErr

			// Act
			resp, err := h.Client.CreateThread(context.Background(), connect.NewRequest(&agentreplv1.CreateThreadRequest{
				Question: &agentreplv1.CreateThreadQuestion{Text: tc.question},
			}))

			// Assert
			if tc.arm {
				if err != nil || resp.Msg.GetError().GetCreationFailed().GetDetail() != "no name" {
					t.Fatalf("resp = %v, err = %v, want creation_failed", resp, err)
				}
			} else if connect.CodeOf(err) != tc.code {
				t.Fatalf("err = %v, want %v", err, tc.code)
			}
			if len(h.Prompts.threadSubmissions) != 0 {
				t.Fatal("a refused thread submitted a question")
			}
		})
	}
}

func TestRequestNewThreadWithNoEditorIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.RequestNewThread(context.Background(), connect.NewRequest(&agentreplv1.RequestNewThreadRequest{}))

	// Assert
	if err != nil || resp.Msg.GetError().GetNoEditor() == nil {
		t.Fatalf("resp = %v, err = %v, want no_editor", resp, err)
	}
}

func TestRequestNewThreadReachesTheEmacsStream(t *testing.T) {
	// Arrange
	h := newHarness(t)
	emacs := openDaemonWatch(t, h, emacsWatch("b"), "sync-emacs")

	// Act
	resp, err := h.Client.RequestNewThread(context.Background(), connect.NewRequest(&agentreplv1.RequestNewThreadRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("resp = %v, err = %v, want success", resp, err)
	}
	for emacs.Receive() {
		if emacs.Msg().GetNewThreadRequested() != nil {
			return
		}
	}
	t.Fatalf("the Emacs stream ended without the request: %v", emacs.Err())
}

func TestChatGesturesRefuseAWorkspaceThatIsNotAThread(t *testing.T) {
	// Arrange
	h := newHarness(t)
	agent := &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: testWorkspaceDir}

	// Act
	resp, err := h.Client.SwitchChatSibling(context.Background(), connect.NewRequest(&agentreplv1.SwitchChatSiblingRequest{
		Thread: agent, Shown: &zettelv1.NodeId{Id: "n1"},
		Direction: &agentreplv1.SwitchChatSiblingDirection{Way: &agentreplv1.SwitchChatSiblingDirection_Next{Next: &agentreplv1.SwitchChatSiblingNext{}}},
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetNotAThread() == nil {
		t.Fatalf("resp = %v, err = %v, want not_a_thread", resp, err)
	}
	if len(h.Chat.switches) != 0 {
		t.Fatal("the gesture reached the chat service")
	}
}

func TestSwitchChatSiblingCarriesTheDirection(t *testing.T) {
	cases := []struct {
		name string
		way  *agentreplv1.SwitchChatSiblingDirection
		want string
	}{
		{name: "next", way: &agentreplv1.SwitchChatSiblingDirection{Way: &agentreplv1.SwitchChatSiblingDirection_Next{Next: &agentreplv1.SwitchChatSiblingNext{}}}, want: "n2:next"},
		{name: "previous", way: &agentreplv1.SwitchChatSiblingDirection{Way: &agentreplv1.SwitchChatSiblingDirection_Previous{Previous: &agentreplv1.SwitchChatSiblingPrevious{}}}, want: "n2:previous"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			thread := withThread(h)

			// Act
			resp, err := h.Client.SwitchChatSibling(context.Background(), connect.NewRequest(&agentreplv1.SwitchChatSiblingRequest{
				Thread: thread, Shown: &zettelv1.NodeId{Id: "n2"}, Direction: tc.way,
			}))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil || len(h.Chat.switches) != 1 || h.Chat.switches[0] != tc.want {
				t.Fatalf("resp = %v, err = %v, switches = %v", resp, err, h.Chat.switches)
			}
		})
	}
}

func TestChatGestureRefusalsAnswerTheirArms(t *testing.T) {
	cases := []struct {
		name string
		call func(h *harness, thread *workspacev1.WorkspaceRef) (bool, error)
	}{
		{name: "switch", call: func(h *harness, thread *workspacev1.WorkspaceRef) (bool, error) {
			resp, err := h.Client.SwitchChatSibling(context.Background(), connect.NewRequest(&agentreplv1.SwitchChatSiblingRequest{
				Thread: thread, Shown: &zettelv1.NodeId{Id: "nope"},
				Direction: &agentreplv1.SwitchChatSiblingDirection{Way: &agentreplv1.SwitchChatSiblingDirection_Next{Next: &agentreplv1.SwitchChatSiblingNext{}}},
			}))
			return resp.Msg.GetError().GetUnknownNode() != nil, err
		}},
		{name: "sightings", call: func(h *harness, thread *workspacev1.WorkspaceRef) (bool, error) {
			resp, err := h.Client.ReportChatSightings(context.Background(), connect.NewRequest(&agentreplv1.ReportChatSightingsRequest{
				Thread: thread, Nodes: []*zettelv1.NodeId{{Id: "nope"}},
			}))
			return resp.Msg.GetError().GetUnknownNode() != nil, err
		}},
		{name: "select", call: func(h *harness, thread *workspacev1.WorkspaceRef) (bool, error) {
			resp, err := h.Client.SelectChatText(context.Background(), connect.NewRequest(&agentreplv1.SelectChatTextRequest{
				Thread: thread, Selection: &agentreplv1.SelectChatTextSelection{State: &agentreplv1.SelectChatTextSelection_Highlighted{
					Highlighted: &agentreplv1.SelectChatTextHighlighted{Node: &zettelv1.NodeId{Id: "nope"}, Quote: &zettelv1.NodeQuote{Text: "w"}},
				}},
			}))
			return resp.Msg.GetError().GetUnknownNode() != nil, err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			thread := withThread(h)
			h.Chat.gestErr = chat.ErrUnknownNode

			// Act
			armed, err := tc.call(h, thread)

			// Assert
			if err != nil || !armed {
				t.Fatalf("armed = %v, err = %v, want unknown_node", armed, err)
			}
		})
	}
}

func TestChatGesturesValidateTheirRequests(t *testing.T) {
	cases := []struct {
		name string
		call func(h *harness, thread *workspacev1.WorkspaceRef) error
	}{
		{name: "a switch with no direction", call: func(h *harness, thread *workspacev1.WorkspaceRef) error {
			_, err := h.Client.SwitchChatSibling(context.Background(), connect.NewRequest(&agentreplv1.SwitchChatSiblingRequest{Thread: thread, Shown: &zettelv1.NodeId{Id: "n1"}}))
			return err
		}},
		{name: "sightings of no node", call: func(h *harness, thread *workspacev1.WorkspaceRef) error {
			_, err := h.Client.ReportChatSightings(context.Background(), connect.NewRequest(&agentreplv1.ReportChatSightingsRequest{Thread: thread}))
			return err
		}},
		{name: "a selection with no state", call: func(h *harness, thread *workspacev1.WorkspaceRef) error {
			_, err := h.Client.SelectChatText(context.Background(), connect.NewRequest(&agentreplv1.SelectChatTextRequest{Thread: thread, Selection: &agentreplv1.SelectChatTextSelection{}}))
			return err
		}},
		{name: "a highlight of no words", call: func(h *harness, thread *workspacev1.WorkspaceRef) error {
			_, err := h.Client.SelectChatText(context.Background(), connect.NewRequest(&agentreplv1.SelectChatTextRequest{
				Thread: thread, Selection: &agentreplv1.SelectChatTextSelection{State: &agentreplv1.SelectChatTextSelection_Highlighted{
					Highlighted: &agentreplv1.SelectChatTextHighlighted{Node: &zettelv1.NodeId{Id: "n1"}, Quote: &zettelv1.NodeQuote{Text: " "}},
				}},
			}))
			return err
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			thread := withThread(h)

			// Act
			err := tc.call(h, thread)

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want invalid_argument", err)
			}
		})
	}
}

func TestSelectChatTextHoldsAndClearsTheHighlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	thread := withThread(h)
	highlighted := &agentreplv1.SelectChatTextSelection{State: &agentreplv1.SelectChatTextSelection_Highlighted{
		Highlighted: &agentreplv1.SelectChatTextHighlighted{Node: &zettelv1.NodeId{Id: "n1"}, Quote: &zettelv1.NodeQuote{Text: "words"}},
	}}
	cleared := &agentreplv1.SelectChatTextSelection{State: &agentreplv1.SelectChatTextSelection_Cleared{Cleared: &agentreplv1.SelectChatTextCleared{}}}

	// Act
	for _, sel := range []*agentreplv1.SelectChatTextSelection{highlighted, cleared} {
		if _, err := h.Client.SelectChatText(context.Background(), connect.NewRequest(&agentreplv1.SelectChatTextRequest{Thread: thread, Selection: sel})); err != nil {
			t.Fatal(err)
		}
	}

	// Assert
	if len(h.Chat.selects) != 2 || h.Chat.selects[0].Quote.GetText() != "words" || h.Chat.selects[1] != nil {
		t.Fatalf("selects = %+v, want the highlight then the clear", h.Chat.selects)
	}
}

func TestSubmitPromptToAThreadAsksAQuestion(t *testing.T) {
	cases := []struct {
		name      string
		parent    *zettelv1.NodeParent
		wantQuote bool
	}{
		{name: "asked of the path's end", parent: nodeParent("n2", "")},
		{name: "asked of a highlight quotes its words", parent: nodeParent("n1", "the blue words"), wantQuote: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			thread := withThread(h)
			h.Chat.parent = tc.parent
			h.Prompts.outcome = prompthandler.Outcome{Turn: "turn-2"}

			// Act
			resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
				Workspace: thread, Said: workspace.SaidText("why?"), IdempotencyKey: "k1",
				Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			}))

			// Assert
			if err != nil || resp.Msg.GetSuccess().GetTurn() == nil {
				t.Fatalf("resp = %v, err = %v, want the minted turn", resp, err)
			}
			subs := h.Prompts.threadSubmissions
			if len(subs) != 1 || subs[0].ask.Parent != tc.parent || h.Prompts.submits != 0 {
				t.Fatalf("submissions = %+v, plain submits = %d", subs, h.Prompts.submits)
			}
			_, quoted := subs[0].said.GetContent().GetBlocks()[0].GetBlock().(*conversationv1.UserContentBlock_Quote)
			if quoted != tc.wantQuote {
				t.Fatalf("quoted = %v, want %v", quoted, tc.wantQuote)
			}
			if len(h.Chat.asked) != 1 || h.Chat.asked[0] != tc.parent {
				t.Fatalf("asked = %v, want the chat told", h.Chat.asked)
			}
		})
	}
}

func TestSubmitPromptToAThreadAwaitingItsFirstAnswerIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)
	thread := withThread(h)
	h.Chat.askErr = chat.ErrAwaitingFirstAnswer

	// Act
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
		Workspace: thread, Said: workspace.SaidText("and?"), IdempotencyKey: "k1",
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetThreadAwaitingFirstAnswer() == nil {
		t.Fatalf("resp = %v, err = %v, want thread_awaiting_first_answer", resp, err)
	}
	if len(h.Prompts.threadSubmissions) != 0 {
		t.Fatal("a refused question was submitted")
	}
}

func TestWatchChatServesTheThreadsView(t *testing.T) {
	// Arrange
	h := newHarness(t)
	thread := withThread(h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	stream, err := h.Client.WatchChat(ctx, connect.NewRequest(&agentreplv1.WatchChatRequest{Thread: thread}))

	// Assert
	if err != nil || !stream.Receive() {
		t.Fatalf("WatchChat: %v / %v", err, stream.Err())
	}
	if got := stream.Msg().GetView().GetEmpty().GetHint().GetText(); got != chat.EmptyHint {
		t.Fatalf("view = %v, want the empty hint", stream.Msg().GetView())
	}
}

func TestWatchChatEndsWhenTheGraphCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	thread := withThread(h)
	h.Chat.openErr = errors.New("zettel down")

	// Act
	stream, err := h.Client.WatchChat(context.Background(), connect.NewRequest(&agentreplv1.WatchChatRequest{Thread: thread}))
	err = firstStreamErr(stream, err)

	// Assert
	if connect.CodeOf(err) != connect.CodeUnavailable {
		t.Fatalf("err = %v, want unavailable", err)
	}
}

func TestShowSidebarAppSelectsTheAppsMostRecentWorkspace(t *testing.T) {
	cases := []struct {
		name string
		app  *frontendv1.SidebarApp
		want ids.WorkspaceID
	}{
		{name: "chat", app: &frontendv1.SidebarApp{App: &frontendv1.SidebarApp_Chat{Chat: &frontendv1.SidebarAppChat{}}}, want: testThreadID},
		{name: "workspaces", app: &frontendv1.SidebarApp{App: &frontendv1.SidebarApp_Workspaces{Workspaces: &frontendv1.SidebarAppWorkspaces{}}}, want: testWorkspaceID},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			withThread(h)

			// Act
			resp, err := h.Client.ShowSidebarApp(context.Background(), connect.NewRequest(&agentreplv1.ShowSidebarAppRequest{App: tc.app}))

			// Assert
			if err != nil || resp.Msg.GetSuccess() == nil {
				t.Fatalf("resp = %v, err = %v", resp, err)
			}
			if len(h.Verbs.selected) != 1 || h.Verbs.selected[0] != tc.want {
				t.Fatalf("selected = %v, want %s", h.Verbs.selected, tc.want)
			}
		})
	}
}

func TestShowSidebarAppWithNothingToShowIsRefused(t *testing.T) {
	// Arrange: no thread is registered.
	h := newHarness(t)

	// Act
	resp, err := h.Client.ShowSidebarApp(context.Background(), connect.NewRequest(&agentreplv1.ShowSidebarAppRequest{
		App: &frontendv1.SidebarApp{App: &frontendv1.SidebarApp_Chat{Chat: &frontendv1.SidebarAppChat{}}},
	}))

	// Assert
	if err != nil || resp.Msg.GetError().GetNothingToShow() == nil {
		t.Fatalf("resp = %v, err = %v, want nothing_to_show", resp, err)
	}
}

func TestMostRecentOfPicksTheLastInteractedOpenWorkspace(t *testing.T) {
	at := func(s int64) *time.Time { v := time.Unix(s, 0); return &v }
	records := []wsm.Workspace{
		{ID: "old-thread", Kind: wsm.WorkspaceKindThread, LastSelectedAt: at(10)},
		{ID: "new-thread", Kind: wsm.WorkspaceKindThread, LastActivityAt: at(30)},
		{ID: "hidden-thread", Kind: wsm.WorkspaceKindThread, LastSelectedAt: at(50), Hidden: true},
		{ID: "agent", Kind: wsm.WorkspaceKindRepository, LastSelectedAt: at(40)},
		{ID: "closed-agent", Kind: wsm.WorkspaceKindRepository, LastSelectedAt: at(60), Closed: true},
	}
	cases := []struct {
		name    string
		threads bool
		want    ids.WorkspaceID
	}{
		{name: "threads", threads: true, want: "new-thread"},
		{name: "workspaces", threads: false, want: "agent"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := mostRecentOf(records, tc.threads)

			// Assert
			if !ok || got.ID != tc.want {
				t.Fatalf("mostRecentOf = %s, %v, want %s", got.ID, ok, tc.want)
			}
		})
	}
}

func TestAdoptWebWorkspaceNamesThePageItMounts(t *testing.T) {
	cases := []struct {
		name   string
		ref    func(h *harness) *workspacev1.WorkspaceRef
		thread bool
	}{
		{name: "an agent workspace mounts the feed", ref: func(*harness) *workspacev1.WorkspaceRef {
			return &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: testWorkspaceDir}
		}},
		{name: "a thread mounts the chat", ref: withThread, thread: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ref := tc.ref(h)

			// Act
			resp, err := h.Client.AdoptWebWorkspace(context.Background(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref}))

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			page := resp.Msg.GetSuccess().GetPage()
			if (page.GetThread() != nil) != tc.thread || (page.GetAgent() != nil) == tc.thread {
				t.Fatalf("page = %v, want thread %v", page, tc.thread)
			}
		})
	}
}
