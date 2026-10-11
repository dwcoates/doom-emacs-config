package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/workspace"
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
