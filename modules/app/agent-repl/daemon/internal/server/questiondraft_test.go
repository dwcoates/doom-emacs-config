package server

import (
	"context"
	"os"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
)

// UpdateQuestionDraft: one click on a standing question card, decoded to the
// typed edit the verbs apply; its refusals land on the endpoint's own arms.

func draftRequest(edit any) *agentreplv1.UpdateQuestionDraftRequest {
	req := &agentreplv1.UpdateQuestionDraftRequest{
		Workspace: ref(),
		Question:  questionIDFor(testWorkspaceID, "ask-2"),
	}
	switch e := edit.(type) {
	case *agentreplv1.UpdateQuestionDraftShow:
		req.Edit = &agentreplv1.UpdateQuestionDraftRequest_Show{Show: e}
	case *agentreplv1.UpdateQuestionDraftPick:
		req.Edit = &agentreplv1.UpdateQuestionDraftRequest_Pick{Pick: e}
	case *agentreplv1.UpdateQuestionDraftConfirm:
		req.Edit = &agentreplv1.UpdateQuestionDraftRequest_Confirm{Confirm: e}
	}
	return req
}

func TestUpdateQuestionDraftDecodesEachArmIntoItsEdit(t *testing.T) {
	tests := []struct {
		name string
		edit any
		want feed.QuestionEdit
	}{
		{"show", &agentreplv1.UpdateQuestionDraftShow{QuestionText: "which?"},
			feed.QuestionEdit{Kind: feed.EditShow, Question: "which?"}},
		{"pick", &agentreplv1.UpdateQuestionDraftPick{QuestionText: "which?", Label: "a"},
			feed.QuestionEdit{Kind: feed.EditPick, Question: "which?", Label: "a"}},
		{"confirm", &agentreplv1.UpdateQuestionDraftConfirm{QuestionText: "which?"},
			feed.QuestionEdit{Kind: feed.EditConfirm, Question: "which?"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			resp, err := h.Client.UpdateQuestionDraft(context.Background(), connect.NewRequest(draftRequest(tc.edit)))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateQuestionDraft: %v", err)
			}
			if resp.Msg.GetSuccess() == nil {
				t.Fatalf("result = %v, want success", resp.Msg.GetResult())
			}
			got := h.Verbs.draftEdits
			if len(got) != 1 || got[0].ask != "ask-2" || got[0].edit != tc.want {
				t.Fatalf("edits = %+v, want %+v on ask-2", got, tc.want)
			}
		})
	}
}

func TestUpdateQuestionDraftRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.UpdateQuestionDraftRequest
	}{
		{"no edit", draftRequest(nil)},
		{"a show naming no question", draftRequest(&agentreplv1.UpdateQuestionDraftShow{})},
		{"a pick naming no option", draftRequest(&agentreplv1.UpdateQuestionDraftPick{QuestionText: "which?"})},
		{"a pick naming no question", draftRequest(&agentreplv1.UpdateQuestionDraftPick{Label: "a"})},
		{"a confirm naming no question", draftRequest(&agentreplv1.UpdateQuestionDraftConfirm{})},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.UpdateQuestionDraft(context.Background(), connect.NewRequest(tc.req))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
			if len(h.Verbs.draftEdits) != 0 {
				t.Fatalf("edits = %+v, want none for a malformed request", h.Verbs.draftEdits)
			}
		})
	}
}

func TestUpdateQuestionDraftNamesEachRefusalByItsArm(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		check func(*agentreplv1.UpdateQuestionDraftError) bool
	}{
		{"an ask that is not standing", &workspace.Refusal{Arm: workspace.ArmAskNotStanding, Reason: "gone", NotFound: true},
			func(e *agentreplv1.UpdateQuestionDraftError) bool { return e.GetAskNotStanding() != nil }},
		{"the shim's closed ask", &workspace.Refusal{Arm: workspace.ArmShimNoOpenAsk, Reason: "closed"},
			func(e *agentreplv1.UpdateQuestionDraftError) bool { return e.GetAskNotStanding() != nil }},
		{"an unserved value", &workspace.Refusal{Arm: workspace.ArmUnservedAnswer, Reason: "never offered", Fields: map[string]any{"text": "c"}},
			func(e *agentreplv1.UpdateQuestionDraftError) bool { return e.GetUnservedValue().GetText() == "c" }},
		{"no session", &workspace.Refusal{Arm: workspace.ArmNoSession, Reason: "down"},
			func(e *agentreplv1.UpdateQuestionDraftError) bool { return e.GetNoSession() != nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.draftErr = tc.err

			// Act.
			resp, err := h.Client.UpdateQuestionDraft(context.Background(),
				connect.NewRequest(draftRequest(&agentreplv1.UpdateQuestionDraftShow{QuestionText: "which?"})))

			// Assert.
			if err != nil {
				t.Fatalf("UpdateQuestionDraft: %v", err)
			}
			if !tc.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tc.name)
			}
		})
	}
}

func TestUpdateQuestionDraftRefusesARowOfAnotherWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := draftRequest(&agentreplv1.UpdateQuestionDraftShow{QuestionText: "which?"})
	req.Question = questionIDFor("another-workspace", "ask-2")

	// Act.
	resp, err := h.Client.UpdateQuestionDraft(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateQuestionDraft: %v", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("result = %v, want ask_not_standing", resp.Msg.GetResult())
	}
	if len(h.Verbs.draftEdits) != 0 {
		t.Fatal("an edit was applied for another workspace's row")
	}
}

// THE CARD VERBS SHARE ONE PROLOGUE (askRow): a handler that decodes its row by
// hand drifts from the others' refusals, so the source is held to the helper.
func TestEveryCardVerbDecodesItsRowThroughAskRow(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile("answers.go")
	if err != nil {
		t.Fatalf("read answers.go: %v", err)
	}
	src := string(raw)

	// Act / Assert: each card verb calls askRow, and askIDFrom is called only
	// by askRow and by interruptRow, whose refusal arm is the target's own.
	for _, rpc := range []string{"AnswerPermission", "AnswerQuestion", "UpdateQuestionDraft"} {
		body := funcBody(t, src, "func (s *server) "+rpc+"(")
		if !strings.Contains(body, "s.askRow(") || strings.Contains(body, "askIDFrom(") {
			t.Errorf("%s does not decode its row through askRow", rpc)
		}
	}
	if n := strings.Count(src, ":= askIDFrom(rpc"); n != 2 {
		t.Errorf("askIDFrom is called %d times, want 2 (askRow and interruptRow)", n)
	}
}

// funcBody answers the text of the top-level function starting at header, up
// to the next top-level declaration.
func funcBody(t *testing.T, src, header string) string {
	t.Helper()
	start := strings.Index(src, header)
	if start < 0 {
		t.Fatalf("no %q in the source", header)
	}
	rest := src[start+len(header):]
	if end := strings.Index(rest, "\n}\n"); end >= 0 {
		return rest[:end]
	}
	return rest
}

func TestAskRowAnswersARowOfAnotherWorkspaceAsNotStandingOnEveryCardVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	elsewhere := questionIDFor("another-workspace", "ask-2")

	// Act.
	perm, permErr := h.Client.AnswerPermission(context.Background(), connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
		Workspace: ref(), Permission: elsewhere,
		Answer: &agentreplv1.AnswerPermissionRequest_AllowOnce{AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{}},
	}))
	question, questionErr := h.Client.AnswerQuestion(context.Background(), connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
		Workspace: ref(), Question: elsewhere,
		Answers: []*agentreplv1.AnswerQuestionAnswer{{QuestionText: "which?", Chosen: []string{"a"}}},
	}))

	// Assert.
	if permErr != nil || perm.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("AnswerPermission = %v, %v; want ask_not_standing", perm.Msg.GetResult(), permErr)
	}
	if questionErr != nil || question.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("AnswerQuestion = %v, %v; want ask_not_standing", question.Msg.GetResult(), questionErr)
	}
}

func TestAskRowAnswersAnUnknownWorkspaceByItsOwnArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	req := draftRequest(&agentreplv1.UpdateQuestionDraftShow{QuestionText: "which?"})
	req.Workspace = &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: "/nowhere"}

	// Act.
	resp, err := h.Client.UpdateQuestionDraft(context.Background(), connect.NewRequest(req))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateQuestionDraft: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
	if len(h.Verbs.draftEdits) != 0 {
		t.Fatal("an edit was applied for an unknown workspace")
	}
}
