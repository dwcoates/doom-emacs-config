package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// openFeedLink clicks one feed link in the source row.
func openFeedLink(t *testing.T, h *harness, href, source string) *agentreplv1.OpenInEditorResponse {
	t.Helper()
	resp, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_FeedLink{FeedLink: &agentreplv1.OpenInEditorFeedLink{
			Href: href, SourceRow: &frontendv1.FeedId{Value: source},
		}},
	}))
	if err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}
	return resp.Msg
}

// unresolvedLink seeds the verb's answer for a link that named no file.
func unresolvedLink(h *harness, href string) {
	h.Verbs.feedLinkUnresolved = &workspace.UnresolvedLink{Href: href, Question: "which file is " + href + "?"}
	h.Verbs.feedLinkErr = &workspace.Refusal{Rpc: "OpenInEditor", Arm: workspace.ArmLinkUnresolved,
		Reason: "no file", Fields: map[string]any{"href": href}}
}

// saidTextOf flattens a said's text blocks.
func saidTextOf(said *conversationv1.UserSaid) string {
	var out []string
	for _, block := range said.GetContent().GetBlocks() {
		out = append(out, block.GetText().GetText())
	}
	return strings.Join(out, "\n")
}

func TestOpenInEditorRelaysAResolvedFeedLinkThroughTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	msg := openFeedLink(t, h, "AGENTS.md", "row-1")

	// Assert.
	if msg.GetSuccess() == nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "link:AGENTS.md" {
		t.Fatalf("result = %v, opens %v, want success through the feed-link verb", msg.GetResult(), h.Verbs.editorOpens)
	}
}

func TestOpenInEditorAnswersLinkUnresolvedNamingTheHref(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unresolvedLink(h, "nowhere.md")

	// Act.
	msg := openFeedLink(t, h, "nowhere.md", "row-1")

	// Assert.
	if got := msg.GetError().GetLinkUnresolved().GetHref(); got != "nowhere.md" {
		t.Fatalf("result = %v, want link_unresolved naming nowhere.md", msg.GetResult())
	}
}

func TestAnUnresolvedFeedLinkAsksTheWorkspaceWithTheLinkOrigin(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unresolvedLink(h, "nowhere.md")

	// Act.
	openFeedLink(t, h, "nowhere.md", "row-1")

	// Assert.
	if h.Prompts.submits != 1 || h.Prompts.lastOrigin != conversationv1.PromptOrigin_PROMPT_ORIGIN_LINK_UNRESOLVED {
		t.Fatalf("submits = %d origin = %v, want one LINK_UNRESOLVED question", h.Prompts.submits, h.Prompts.lastOrigin)
	}
}

func TestAnUnresolvedFeedLinksQuestionNeverInterrupts(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unresolvedLink(h, "nowhere.md")

	// Act.
	openFeedLink(t, h, "nowhere.md", "row-1")

	// Assert: deferred, so it runs after the running turn and is never
	// classified into an interjection.
	if h.Prompts.lastDelivery != wsm.DeliveryDeferred {
		t.Fatalf("delivery = %v, want deferred", h.Prompts.lastDelivery)
	}
}

func TestAnUnresolvedFeedLinksQuestionQuotesTheSourceBubble(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Feed.markdown = map[string]string{"row-1": "see [AGENTS](nowhere.md)"}
	unresolvedLink(h, "nowhere.md")

	// Act.
	openFeedLink(t, h, "nowhere.md", "row-1")

	// Assert: quoted exactly as a reply to a selected response is.
	got := saidTextOf(h.Prompts.lastSaid)
	want := replyPrefixOpening + "see [AGENTS](nowhere.md)" + replyPrefixMessage + "which file is nowhere.md?"
	if got != want {
		t.Fatalf("question = %q, want %q", got, want)
	}
}

func TestAnUnresolvedFeedLinksQuestionIsSentUnquotedForAnUnselectableRow(t *testing.T) {
	// Arrange: the row resolves to no selectable bubble.
	h := newHarness(t)
	unresolvedLink(h, "nowhere.md")

	// Act.
	openFeedLink(t, h, "nowhere.md", "row-unknown")

	// Assert.
	if got := saidTextOf(h.Prompts.lastSaid); got != "which file is nowhere.md?" {
		t.Fatalf("question = %q, want the question alone", got)
	}
}

func TestAFailedQuestionSubmissionFailsTheClickLoudly(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	unresolvedLink(h, "nowhere.md")
	h.Prompts.err = errors.New("the queue is closed")

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_FeedLink{FeedLink: &agentreplv1.OpenInEditorFeedLink{
			Href: "nowhere.md", SourceRow: &frontendv1.FeedId{Value: "row-1"},
		}},
	}))

	// Assert: never a link_unresolved claiming the question was sent.
	if err == nil {
		t.Fatal("OpenInEditor succeeded, want the submission's failure surfaced")
	}
}

func TestOpenInEditorRefusesAFeedLinkWithNoHref(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: ref(),
		Target:    &agentreplv1.OpenInEditorRequest_FeedLink{FeedLink: &agentreplv1.OpenInEditorFeedLink{}},
	}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want invalid_argument", err)
	}
}
