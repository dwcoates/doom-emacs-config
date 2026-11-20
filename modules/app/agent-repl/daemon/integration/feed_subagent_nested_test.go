//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
)

func pageHasResponseProse(page *frontendv1.FeedPage, markdown string) bool {
	return findRow(page, func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == markdown
	}) != nil
}

func TestASyncSubagentIsOneSettledBubbleOnTheRootFeedAndItsNestedActivityLivesOnItsSubFeed(t *testing.T) {
	t.Parallel()
	// Arrange: the frames the shim files for a synchronous Agent spawn: the
	// spawn and its settle on the MAIN agent's book, the subagent's own
	// activity on the created agent's.
	f := newCanonical(t)
	f.submit("go", "k-subagent-nested", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const sub = "sub-nested-1"
	const commission = "Read the module's AGENTS.md and report the test command."
	const nestedProse = "Reading the module's conventions."
	const report = "The module's test command is `npm test`."
	prompt := &conversationv1.AgentSubagentPrompt{
		Text:         commission,
		Description:  proto.String("Explore the module"),
		SubagentType: proto.String("general-purpose"),
	}
	spawn := func(result *conversationv1.AgentSubagent) *conversationv1.AgentFrame {
		return activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID(sub),
			Item:       &conversationv1.AgentActivity_Subagent{Subagent: result},
		})
	}

	// Act
	f.shim.PushAgentFrame(mainAgent, spawn(&conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
		CreatedAgentId: &conversationv1.AgentId{Value: sub},
		Prompt:         prompt,
		StartedAt:      startedAt(1),
	}}}))
	f.shim.PushAgentFrame(sub, activityFrame(sub, &conversationv1.AgentActivity{
		ActivityId: activityID("sub-nested-prose"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: nestedProse}},
		}}},
	}))
	f.shim.PushAgentFrame(sub, activityFrame(sub, &conversationv1.AgentActivity{
		ActivityId: activityID("sub-nested-report"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: report}},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, spawn(&conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Success{Success: &conversationv1.AgentSubagentSuccess{
		Prompt: prompt,
		Report: &conversationv1.AgentSubagentReport{Prose: &conversationv1.AgentResponseProse{Markdown: report}},
	}}}))

	// Assert: the bubble's head, settled succeeded.
	bubble := awaitRow(t, f, tail, "the spawn's settled bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSubagent().GetSettled().GetSucceeded() != nil
	})
	head := bubble.GetActivity().GetSubagent()
	if head.GetLabel().GetText() != "general-purpose" || head.GetDescription().GetText() != "Explore the module" {
		t.Fatalf("bubble head = label %q description %q, want %q and %q", head.GetLabel().GetText(), head.GetDescription().GetText(), "general-purpose", "Explore the module")
	}

	// Assert: all of the subagent's own activity is reachable on the bubble's
	// own sub-feed. The sub-feed's frames ride a watch of their own, so each
	// row is awaited on the sub-feed's tail when its page lacks it.
	subPage, subToken := f.openFeed(bubble.GetId())
	subTail := f.d.WatchFeed(subToken)
	subRow := func(what string, pred func(*frontendv1.FeedRow) bool) {
		t.Helper()
		if findRow(subPage, pred) == nil {
			awaitRow(t, f, subTail, what, pred)
		}
	}
	prose := func(markdown string) func(*frontendv1.FeedRow) bool {
		return func(r *frontendv1.FeedRow) bool {
			return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == markdown
		}
	}
	subRow("the subagent's own prose", prose(nestedProse))
	subRow("the subagent's own report", prose(report))
	subRow("the commission as an agent_prompt row", func(r *frontendv1.FeedRow) bool {
		prompt := r.GetAgentPrompt()
		if prompt == nil || prompt.GetAddress().GetReceived() == nil {
			return false
		}
		for _, block := range prompt.GetBody().GetBlocks() {
			if block.GetText().GetProse().GetSource().GetMarkdown() == commission {
				return true
			}
		}
		return false
	})

	// Assert: and, the sub-feed having drawn them, none of it is a second row
	// on the root feed, which carries the one bubble.
	rootPage, _ := f.openFeed(nil)
	bubbles := 0
	for _, row := range rootPage.GetSuccess().GetRows() {
		if row.GetActivity().GetSubagent() != nil {
			bubbles++
		}
	}
	if bubbles != 1 {
		t.Fatalf("root feed carries %d subagent bubbles, want exactly one for the one spawn", bubbles)
	}
	if pageHasResponseProse(rootPage, nestedProse) || pageHasResponseProse(rootPage, report) {
		t.Fatalf("the root feed carried the subagent's own prose; it belongs on the sub-feed only")
	}
}
