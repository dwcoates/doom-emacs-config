package feed

import (
	"fmt"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// THE PROMPT ROWS. The author label is the origin's business, the drawn text
// has the host's sentinel spans stripped, and an image reference is resolved to
// a src by the DAEMON.

// promptWith sends one prompt with the given origin and blocks.
func (h *harness) promptWith(turn string, origin conversationv1.PromptOrigin, blocks ...*conversationv1.UserContentBlock) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  mainAgent(),
		Origin: origin,
		Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}},
	}, noAddress())
}

// textBlock is one typed block.
func textBlock(text string) *conversationv1.UserContentBlock {
	return &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
	}
}

func TestTheAuthorLabelComesFromTheOrigin(t *testing.T) {
	tests := []struct {
		name   string
		origin conversationv1.PromptOrigin
		want   string
	}{
		{
			name:   "a person typed it",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
			want:   "You",
		},
		{
			name:   "a merge conflict repair is the merge's, not the user's",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
			want:   "Merge",
		},
		{
			name:   "a displaced turn resumed by the merge is the merge's",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
			want:   "Merge",
		},
		{
			name:   "a restart re-drive says so rather than posing as a fresh turn",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART,
			want:   "Resumed after restart",
		},
		{
			name:   "a workspace's initial brief is labelled as one",
			origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED,
			want:   "Workspace brief",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			h.promptWith("turn-1", tc.origin, textBlock("do the thing"))

			// Assert.
			got := h.only(rootFeed()).GetUserPrompt().GetAuthor().GetLabel()
			if got != tc.want {
				t.Fatalf("author = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTheDrawnPromptHasItsSentinelSpansStripped(t *testing.T) {
	// Arrange: a resolver whose injected stripper removes the host's spans.
	h := newHarness(t)
	h.resolver.deps.StripSentinels = func(text string) string {
		return "fix the flaky test"
	}

	// Act.
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT_WITH_METAPROMPT,
		textBlock("<<META>>house rules<<END>>fix the flaky test"))

	// Assert: the DRAWN text alone is stripped; the client never sniffs a
	// sentinel, and the full text stays on the durable record elsewhere.
	blocks := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if got := blocks[0].GetText().GetText(); got != "fix the flaky test" {
		t.Fatalf("drawn text = %q, want the stripped text", got)
	}
}

func TestAnImageReferenceIsResolvedToADrawableSrc(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
				Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: "/tmp/shot.png"}},
				MediaType: "image/png",
			}},
		})

	// Assert: that resolution is the daemon's, never the client's.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0].GetImage()
	if block.GetSrc() != "https://host/img" || block.GetAlt() != "screenshot.png" {
		t.Fatalf("image block = %+v, want the resolved src and alt", block)
	}
}

func TestAnUnresolvableImageIsDrawnAsUnsupportedAndWarned(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.resolver.deps.ResolveImage = func(*conversationv1.ImageBlock) (string, string, error) {
		return "", "", fmt.Errorf("the file is gone")
	}

	// Act.
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{MediaType: "image/png"}},
		})

	// Assert: the block is named rather than dropped, and the failure is loud.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0]
	if block.GetUnsupported().GetKind() != "image" {
		t.Fatalf("block = %+v, want an unsupported image", block)
	}
	if !h.hasRecord("warn", "daemon.feed.image_unresolved") {
		t.Fatalf("records = %+v, want a WARN daemon.feed.image_unresolved", h.records())
	}
}

func TestAnUnmodeledBlockIsNamedRatherThanDropped(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Unsupported{
				Unsupported: &conversationv1.UnsupportedBlock{Kind: "vendor_widget"},
			},
		})

	// Assert.
	block := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()[0]
	if block.GetUnsupported().GetKind() != "vendor_widget" {
		t.Fatalf("block = %+v, want the kind named", block)
	}
}

func TestABlockSequenceKeepsTheOrderThePersonComposed(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.promptWith("turn-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		textBlock("look at this"),
		&conversationv1.UserContentBlock{
			Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{MediaType: "image/png"}},
		},
		textBlock("and fix it"))

	// Assert.
	blocks := h.only(rootFeed()).GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 3 || blocks[0].GetText() == nil || blocks[1].GetImage() == nil || blocks[2].GetText() == nil {
		t.Fatalf("blocks = %+v, want text, image, text in order", blocks)
	}
}

func TestThePromptRowIsStampedWithItsTurn(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	h.deliverPrompt("turn-42", "hello")

	// Assert: what a client matches its own SubmitPromptSuccess against.
	if got := h.only(rootFeed()).GetTurn().GetValue(); got != "turn-42" {
		t.Fatalf("turn stamp = %q, want turn-42", got)
	}
}

func TestAnAgentAddressedPromptIsDrawnAtBothEnds(t *testing.T) {
	// Arrange: a subagent whose bubble the resolver has seen created.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: a prompt delivered to that subagent.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, noAddress())

	// Assert: the outgoing send on the sender's feed…
	var outgoing, delivered string
	for _, row := range h.rows(rootFeed()) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			outgoing = prompt.GetAddress().GetText()
		}
	}
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			delivered = prompt.GetAddress().GetText()
		}
	}
	if outgoing != "→ map the daemon" {
		t.Fatalf("sender address = %q, want the outgoing form", outgoing)
	}
	// …and the delivered prompt on the recipient's, differing ONLY in the line.
	if delivered != "from the main agent" {
		t.Fatalf("recipient address = %q, want the delivered form", delivered)
	}
}

func TestAnAgentAddressedPromptCarriesTheSameBodyAtBothEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, noAddress())

	// Assert: ONE component, both ends.
	var senderBody, recipientBody string
	for _, row := range h.rows(rootFeed()) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			senderBody = prompt.GetBody().GetBlocks()[0].GetText().GetText()
		}
	}
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if prompt := row.GetAgentPrompt(); prompt != nil {
			recipientBody = prompt.GetBody().GetBlocks()[0].GetText().GetText()
		}
	}
	if senderBody != "also check the shim" || recipientBody != senderBody {
		t.Fatalf("bodies = (%q, %q), want the same content at both ends", senderBody, recipientBody)
	}
}

func TestAPromptWhileAnOutputAddressIsInForceStaysAUserPrompt(t *testing.T) {
	// Arrange: a merge lease holds the session, and the recipient is a
	// subagent the resolver knows.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")
	lease := ids.LeaseID("lease-7")
	h.resolver.SetOutputAddress(testWorkspace, &sessionwatcher.OutputAddress{
		Feed: feedid.Feed{Merge: &lease},
	})

	// Act.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR,
		Said:   &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: []*conversationv1.UserContentBlock{textBlock("resolve it")}}},
	}, noAddress())

	// Assert: the address wins — the row lands on the merge feed as a labelled
	// user prompt rather than being split across two agent feeds.
	rows := h.rows(feedid.Feed{Merge: &lease})
	if len(rows) != 1 || rows[0].GetUserPrompt() == nil {
		t.Fatalf("merge feed rows = %+v, want one labelled user prompt", rows)
	}
	if got := rows[0].GetUserPrompt().GetAuthor().GetLabel(); got != "Merge" {
		t.Fatalf("author = %q, want Merge", got)
	}
}
