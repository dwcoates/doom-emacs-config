package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/replyquote"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// openFeedLink clicks one feed link in the source row.
func openFeedLink(t *testing.T, h *harness, href, source string) *agentreplv1.OpenInEditorResponse {
	t.Helper()
	return openFeedLinkOn(t, h, href, source, true)
}

// reportArm is on_unresolved's `report` arm.
func reportArm() *agentreplv1.OpenInEditorFeedLink_Report {
	return &agentreplv1.OpenInEditorFeedLink_Report{Report: &agentreplv1.OpenInEditorFeedLinkReport{}}
}

// openFeedLinkOn clicks one feed link carrying on_unresolved's `report` arm
// when report is set, its `web_fallback` arm otherwise.
func openFeedLinkOn(t *testing.T, h *harness, href, source string, report bool) *agentreplv1.OpenInEditorResponse {
	t.Helper()
	link := &agentreplv1.OpenInEditorFeedLink{Href: href, SourceRow: &frontendv1.FeedId{Value: source}}
	if report {
		link.OnUnresolved = reportArm()
	} else {
		link.OnUnresolved = &agentreplv1.OpenInEditorFeedLink_WebFallback{WebFallback: &agentreplv1.OpenInEditorFeedLinkWebFallback{}}
	}
	resp, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: ref(),
		Target:    &agentreplv1.OpenInEditorRequest_FeedLink{FeedLink: link},
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

	// Assert: quoted exactly as a reply to a selected response is — the
	// quote its own leading block, the question the block after it.
	want := replyquote.Quote(said("which file is nowhere.md?"), "see [AGENTS](nowhere.md)", false)
	if !proto.Equal(h.Prompts.lastSaid, want) {
		t.Fatalf("question = %v, want %v", h.Prompts.lastSaid, want)
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
			Href: "nowhere.md", SourceRow: &frontendv1.FeedId{Value: "row-1"}, OnUnresolved: reportArm(),
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

func TestAReportFeedLinkAsksTheVerbToReport(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	openFeedLinkOn(t, h, "AGENTS.md", "row-1", true)

	// Assert.
	if len(h.Verbs.feedLinkReport) != 1 || !h.Verbs.feedLinkReport[0] {
		t.Fatalf("report flags = %v, want one report", h.Verbs.feedLinkReport)
	}
}

func TestAWebFallbackFeedLinkAsksTheVerbNotToReport(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	openFeedLinkOn(t, h, "notes.org", "row-1", false)

	// Assert.
	if len(h.Verbs.feedLinkReport) != 1 || h.Verbs.feedLinkReport[0] {
		t.Fatalf("report flags = %v, want one silent call", h.Verbs.feedLinkReport)
	}
}

func TestAnUnresolvedWebFallbackFeedLinkAnswersLinkUnresolvedAndAsksNothing(t *testing.T) {
	// Arrange: the verb answers the silent refusal, with no question.
	h := newHarness(t)
	h.Verbs.feedLinkErr = &workspace.Refusal{Rpc: "OpenInEditor", Arm: workspace.ArmLinkUnresolved,
		Reason: "no file", Fields: map[string]any{"href": "notes.org"}}

	// Act.
	msg := openFeedLinkOn(t, h, "notes.org", "row-1", false)

	// Assert.
	if msg.GetError().GetLinkUnresolved().GetHref() != "notes.org" || h.Prompts.submits != 0 {
		t.Fatalf("result = %v, submits = %d; want link_unresolved and no question", msg.GetResult(), h.Prompts.submits)
	}
}

func TestOpenInEditorRefusesAFeedLinkWithNoOnUnresolvedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_FeedLink{FeedLink: &agentreplv1.OpenInEditorFeedLink{
			Href: "AGENTS.md", SourceRow: &frontendv1.FeedId{Value: "row-1"},
		}},
	}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Verbs.editorOpens) != 0 {
		t.Fatalf("err = %v, opens = %v; want invalid_argument and nothing opened", err, h.Verbs.editorOpens)
	}
}
