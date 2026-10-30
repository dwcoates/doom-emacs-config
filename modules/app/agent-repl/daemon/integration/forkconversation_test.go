//go:build integration

package integration

import (
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"agent-repld/integration/harness"
)

// A FORK READS ITS INHERITED PAST FROM ITS PARENT'S BOOK. The child's own book
// holds only what the fork itself said; its feed walks that book to its start
// and then the parent's, bounded to the fork point and without the parent's
// turns that were open when the fork was taken. The fake store keeps one book
// per fake shim, so each case files the parent's book on the CHILD's shim,
// which is where a real store serves it from too.

// The sentences the fork tests read back off the feed. They are distinctive so
// a substring match cannot confuse one for another.
const (
	forkParentQuestion = "what did the parent ask"
	forkParentAnswer   = "what the parent was told"
	forkInFlight       = "what the parent was still doing"
	forkOwnQuestion    = "what the fork asks for itself"
)

// TestAForksFeedReadsItsParentsTurnAboveItsOwn is the owner's principle as an
// assertion: the fork's feed carries the parent's question and its answer,
// the answer as the parent's feed draws it, above the fork's own question.
func TestAForksFeedReadsItsParentsTurnAboveItsOwn(t *testing.T) {
	t.Parallel()
	// Arrange: a parent asked something, then forked.
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	parentTurn := f.submit(forkParentQuestion, "fork-parent-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).
		GetSuccess().GetTurn().GetTurn().GetValue()
	kid, book := forkedChild(t, f)
	fileInheritedTurn(kid, book, parentTurn, forkParentQuestion, "parent-answer", forkParentAnswer)

	// Act
	kid.submit(forkOwnQuestion, "fork-child-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	rows := walkFeedToStart(kid)

	// Assert
	texts := forkRowTexts(rows)
	question, answer, own := forkIndexOf(texts, forkParentQuestion), forkIndexOf(texts, forkParentAnswer), forkIndexOf(texts, forkOwnQuestion)
	if question < 0 || answer < 0 || own < 0 || !(question < answer && answer < own) {
		t.Fatalf("the fork's feed reads %v: want the parent's question, then its answer, then the fork's own question", texts)
	}
	for _, row := range rows {
		if response := row.GetActivity().GetResponse(); response != nil && strings.Contains(response.GetSuccess().GetProse().GetProse().GetSource().GetMarkdown(), forkParentAnswer) && !response.GetFinalAnswer() {
			t.Fatalf("the inherited answer %v is not drawn as its turn's final answer", row.GetId())
		}
	}
}

// forkedChild forks the fixture's workspace, brings the child up on the fork
// its shim made, and answers the child's fixture and the parent's book.
func forkedChild(t *testing.T, f *fixture) (*fixture, string) {
	t.Helper()
	parentVendorID := f.shim.Info().VendorSessionID
	child := forkOf(t, f)
	childShim := f.d.Shim(child)
	if childShim.ExpectForkSession().GetParentVendorSessionId() != parentVendorID {
		t.Fatal("the child's shim was asked for a fork of another conversation")
	}
	if resume := childShim.ExpectStartSession().GetResume(); resume == nil {
		t.Fatal("the forked child's StartSession carries no resume")
	}
	// The fake shim's fork names the parent's book after the parent's vendor
	// session (fakeshim ForkSession).
	return &fixture{d: f.d, repo: f.repo, ws: child, shim: childShim, t: t}, parentVendorID
}

// fileInheritedTurn files one finished turn of BOOK on the child's fake store:
// its prompt, its answer UNIT and the terminal naming that answer, all of TURN.
func fileInheritedTurn(kid *fixture, book, turn, question, unit, answer string) {
	kid.t.Helper()
	kid.shim.FileUserPrompt(book, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  &conversationv1.AgentId{Value: book},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
		Said:   said(question),
	})
	kid.shim.FileAgentFrame(book, turn, activityFrame(book, &conversationv1.AgentActivity{
		ActivityId: activityID(unit),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: answer}},
		}}},
	}))
	kid.shim.FileAgentFrame(book, turn, &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: book},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{Answer: activityID(unit)}},
		}},
	})
}

// walkFeedToStart serves the root feed's newest page and every older one to
// the conversation's start, answering every row served, oldest first.
func walkFeedToStart(f *fixture) []*frontendv1.FeedRow {
	f.t.Helper()
	page := f.feedPage(true)
	rows := page.GetSuccess().GetRows()
	for walks := 0; page.GetSuccess().GetHasMore() != nil; walks++ {
		if walks > 16 {
			f.t.Fatal("the walk never reached the conversation's start")
		}
		page = f.feedPage(false)
		rows = append(append([]*frontendv1.FeedRow{}, page.GetSuccess().GetRows()...), rows...)
	}
	return rows
}

// forkRowTexts renders rows as the sentences a reader would see, in order.
func forkRowTexts(rows []*frontendv1.FeedRow) []string {
	var out []string
	for _, row := range rows {
		switch {
		case row.GetUserPrompt() != nil:
			for _, block := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
				out = append(out, block.GetText().GetProse().GetSource().GetMarkdown())
			}
		case row.GetActivity().GetResponse() != nil:
			out = append(out, row.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown())
		}
	}
	return out
}

// forkIndexOf answers where a sentence sits among the drawn texts, -1 when it
// is on none of them.
func forkIndexOf(texts []string, want string) int {
	for i, text := range texts {
		if strings.Contains(text, want) {
			return i
		}
	}
	return -1
}

// forkOf forks the fixture's workspace and answers the child.
func forkOf(t *testing.T, f *fixture) *workspacev1.WorkspaceRef {
	t.Helper()
	resp, err := f.d.Client().CreateWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository:    createRepositoryRef(t, f.d, f.repo),
		Requester:     harness.User(),
		ContextOrigin: harness.Fork(f.ws),
		Form:          &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Relationship: harness.Child(f.ws), OwnerNotice: harness.Notice("")}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(fork) = (%v, %v), want a success", resp, err)
	}
	return resp.Msg.GetSuccess().GetWorkspace()
}
