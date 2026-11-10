package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// withAskable seeds the cards a question may name and its prompt quotes:
// row-1 and row-2 final responses, row-3 a prompt, row-4 a tool call card.
func withAskable(h *harness) {
	h.Feed.cards = map[string]feed.CardText{
		"row-1": {Kind: feed.CardResponse, Arm: "activity.response", Text: "1 🔎 the answer"},
		"row-2": {Kind: feed.CardResponse, Arm: "activity.response", Text: "2 🔎 another answer"},
		"row-3": {Kind: feed.CardPrompt, Arm: "user_prompt", Text: "fix the footer"},
		"row-4": {Kind: feed.CardRow, Arm: "activity.simple_tool_call", Text: `{"name": "Bash"}`},
	}
}

// withHomeMain has the fake verbs answer the home repository's main
// workspace as every question's recipient.
func withHomeMain(h *harness) {
	h.Verbs.homeMain = wsm.Workspace{ID: "ws-main", Dir: "/tmp/agent-repl-test-repo", Name: "main (doom)"}
}

// ask toggles the workspace's question about ROW.
func ask(t *testing.T, h *harness, row string) *agentreplv1.AskAboutResponseResponse {
	t.Helper()
	resp, err := h.Client.AskAboutResponse(context.Background(), connect.NewRequest(&agentreplv1.AskAboutResponseRequest{
		Workspace: ref(), Row: &frontendv1.FeedId{Value: row},
	}))
	if err != nil {
		t.Fatalf("AskAboutResponse: %v", err)
	}
	return resp.Msg
}

// receiveQuestion reads the watch stream until a response-question frame arrives.
func receiveQuestion(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.WatchFeedResponse]) *frontendv1.FeedResponseQuestion {
	t.Helper()
	return receivePush(t, stream, "a response question", (*agentreplv1.WatchFeedResponse).GetResponseQuestion)
}

// receiveHostQuestion reads the host stream until a response-question event arrives.
func receiveHostQuestion(t *testing.T, stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse]) *agentreplv1.HostWorkspaceResponseQuestion {
	t.Helper()
	return receivePush(t, stream, "a response question", (*agentreplv1.WatchHostWorkspaceResponse).GetResponseQuestion)
}

func TestAskAboutResponseToggles(t *testing.T) {
	tests := []struct {
		name      string
		clicks    []string
		wantAsked string // "" for withdrawn
	}{
		{name: "a first click asks", clicks: []string{"row-1"}, wantAsked: "row-1"},
		{name: "a second click on the asked response withdraws", clicks: []string{"row-1", "row-1"}},
		{name: "a click on another response moves the question", clicks: []string{"row-1", "row-2"}, wantAsked: "row-2"},
		{name: "a click on a tool card asks about it", clicks: []string{"row-4"}, wantAsked: "row-4"},
		{name: "a click on a tool card moves a question about a response", clicks: []string{"row-1", "row-4"}, wantAsked: "row-4"},
		{name: "a second click on the asked tool card withdraws", clicks: []string{"row-4", "row-4"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			withAskable(h)
			for _, row := range tc.clicks[:len(tc.clicks)-1] {
				ask(t, h, row)
			}

			// Act.
			got := ask(t, h, tc.clicks[len(tc.clicks)-1]).GetSuccess()

			// Assert.
			if tc.wantAsked == "" {
				if got.GetWithdrawn() == nil {
					t.Fatalf("success = %v, want withdrawn", got)
				}
				return
			}
			if row := got.GetAsking().GetRow().GetValue(); row != tc.wantAsked {
				t.Fatalf("success = %v, want asking %s", got, tc.wantAsked)
			}
		})
	}
}

func TestAskAboutResponseRefusesARowThatIsNoCard(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	ask(t, h, "row-1")

	// Act.
	got := ask(t, h, "row-9")

	// Assert.
	if got.GetError().GetNotACard() == nil {
		t.Fatalf("AskAboutResponse = %v, want not_a_card", got)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Operation: "AskAboutResponse"}); !ok {
		t.Fatalf("no AskAboutResponse record names the refusal; records = %v", log.Records())
	}
	if row, ok := h.Server.(*server).questions.take(testWorkspaceID); !ok || row.GetValue() != "row-1" {
		t.Fatalf("held question = %v (%v), want row-1 left standing", row, ok)
	}
}

func TestAskAboutResponseRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.AskAboutResponseRequest
	}{
		{name: "no workspace", req: &agentreplv1.AskAboutResponseRequest{Row: &frontendv1.FeedId{Value: "row-1"}}},
		{name: "no row", req: &agentreplv1.AskAboutResponseRequest{Workspace: ref()}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.AskAboutResponse(context.Background(), connect.NewRequest(tc.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("AskAboutResponse = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestAskAboutResponseTellsTheHostEachTransition(t *testing.T) {
	tests := []struct {
		name   string
		clicks []string
		want   func(*agentreplv1.HostWorkspaceResponseQuestion) bool
	}{
		{name: "asked", clicks: []string{"row-1"}, want: func(q *agentreplv1.HostWorkspaceResponseQuestion) bool { return q.GetAsked() != nil }},
		{name: "moved", clicks: []string{"row-1", "row-2"}, want: func(q *agentreplv1.HostWorkspaceResponseQuestion) bool { return q.GetMoved() != nil }},
		{name: "withdrawn", clicks: []string{"row-1", "row-1"}, want: func(q *agentreplv1.HostWorkspaceResponseQuestion) bool { return q.GetWithdrawn() != nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			withAskable(h)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := dialHostStream(t, h, ctx)

			// Act.
			var last *agentreplv1.HostWorkspaceResponseQuestion
			for _, row := range tc.clicks {
				ask(t, h, row)
				last = receiveHostQuestion(t, stream)
			}

			// Assert.
			if !tc.want(last) {
				t.Fatalf("last host event = %v, want %s", last, tc.name)
			}
		})
	}
}

func TestTheRootFeedWatchDrawsTheQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := openRootWatch(t, h, ctx)

	// Act.
	ask(t, h, "row-1")
	asked := receiveQuestion(t, stream)
	ask(t, h, "row-1")
	withdrawn := receiveQuestion(t, stream)

	// Assert.
	if asked.GetAsking().GetRow().GetValue() != "row-1" {
		t.Fatalf("first frame = %v, want asking row-1", asked)
	}
	if withdrawn.GetNone() == nil {
		t.Fatalf("second frame = %v, want none", withdrawn)
	}
}

func TestALateRootWatchIsToldTheStandingQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	ask(t, h, "row-2")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	got := receiveQuestion(t, openRootWatch(t, h, ctx))

	// Assert.
	if got.GetAsking().GetRow().GetValue() != "row-2" {
		t.Fatalf("frame = %v, want asking row-2", got)
	}
}

func TestAPromptSentWhileAQuestionStandsIsSentToTheHomeMainWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	withHomeMain(h)
	ask(t, h, "row-1")

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	sent := resp.Msg.GetSuccess().GetQuestionSent()
	if sent.GetRow().GetValue() != "row-1" || sent.GetRecipient() != "main (doom)" {
		t.Fatalf("SubmitPrompt answered %v, want question_sent naming row-1 to main (doom)", resp.Msg)
	}
	asked := h.Verbs.homeQuestions
	if len(asked) != 1 || asked[0].asker != testWorkspaceID {
		t.Fatalf("home questions = %+v, want one from %s", asked, testWorkspaceID)
	}
	if !strings.Contains(asked[0].question, "1 🔎 the answer") || !strings.HasSuffix(asked[0].question, "hello") {
		t.Fatalf("question = %q, want the quoted response then the words", asked[0].question)
	}
}

func TestAQuestionPromptQuotesTheAskedCardByItsKind(t *testing.T) {
	tests := []struct {
		name string
		row  string
		want string
	}{
		{name: "a response under the response preamble", row: "row-1", want: replyquote.QuestionText("1 🔎 the answer", "hello")},
		{name: "a prompt under the prompt preamble", row: "row-3", want: replyquote.PromptQuestionText("fix the footer", "hello")},
		{name: "a tool card as its JSON under its kind", row: "row-4", want: replyquote.CardQuestionText("activity.simple_tool_call", `{"name": "Bash"}`, "hello")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			withAskable(h)
			withHomeMain(h)
			ask(t, h, tc.row)

			// Act.
			if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
				t.Fatalf("SubmitPrompt: %v", err)
			}

			// Assert.
			if len(h.Verbs.homeQuestions) != 1 || h.Verbs.homeQuestions[0].question != tc.want {
				t.Fatalf("home questions = %+v, want one whose text is %q", h.Verbs.homeQuestions, tc.want)
			}
		})
	}
}

func TestAQuestionPromptMintsNoTurn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	withHomeMain(h)
	ask(t, h, "row-1")

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if h.Prompts.submits != 0 {
		t.Fatalf("the prompt handler took %d submissions, want none", h.Prompts.submits)
	}
}

func TestTheQuestionPromptTakesTheQuestion(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	withHomeMain(h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	feedStream := openRootWatch(t, h, ctx)
	hostStream := dialHostStream(t, h, ctx)
	ask(t, h, "row-1")
	receiveQuestion(t, feedStream)
	receiveHostQuestion(t, hostStream)

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest())); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if q := receiveQuestion(t, feedStream); q.GetNone() == nil {
		t.Fatalf("feed frame = %v, want none", q)
	}
	if q := receiveHostQuestion(t, hostStream); q.GetTaken() == nil {
		t.Fatalf("host event = %v, want taken", q)
	}
	if _, ok := h.Server.(*server).questions.take(testWorkspaceID); ok {
		t.Fatalf("a question still stands after the prompt took it")
	}
}

func TestAQuestionAboutAGoneResponseRefusesThePrompt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	withHomeMain(h)
	ask(t, h, "row-1")
	delete(h.Feed.cards, "row-1")

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if connect.CodeOf(err) != connect.CodeNotFound {
		t.Fatalf("SubmitPrompt = %v, want NotFound for the gone response", err)
	}
	if len(h.Verbs.homeQuestions) != 0 || h.Prompts.submits != 0 {
		t.Fatalf("home questions=%d submits=%d, want nothing sent or delivered", len(h.Verbs.homeQuestions), h.Prompts.submits)
	}
	if _, ok := h.Server.(*server).questions.take(testWorkspaceID); ok {
		t.Fatalf("the refused prompt left the question standing")
	}
}

func TestAQuestionPromptTheHomeMainWorkspaceCannotTakeFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	h.Verbs.homeMainErr = workspace.ErrNoHomeMainWorkspace
	ask(t, h, "row-1")

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(submitRequest()))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("SubmitPrompt = %v, want Internal", err)
	}
	if _, ok := h.Server.(*server).questions.take(testWorkspaceID); ok {
		t.Fatalf("the failed send left the question standing")
	}
}

func TestAnAgentsPromptLeavesTheQuestionStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	withAskable(h)
	withHomeMain(h)
	sender := ids.WorkspaceID("ws-sender")
	h.DB.workspaces[sender] = wsm.Workspace{ID: sender, Dir: "/tmp/ws-sender", Name: "sender"}
	ask(t, h, "row-1")
	req := submitRequest()
	req.SentBy = &workspacev1.WorkspaceRef{Id: string(sender), Dir: "/tmp/ws-sender"}
	req.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT

	// Act.
	resp, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}
	if resp.Msg.GetSuccess().GetQuestionSent() != nil {
		t.Fatalf("an agent's prompt was sent as the user's question: %v", resp.Msg)
	}
	if _, ok := h.Server.(*server).questions.take(testWorkspaceID); !ok {
		t.Fatalf("an agent's prompt took the user's question")
	}
}
