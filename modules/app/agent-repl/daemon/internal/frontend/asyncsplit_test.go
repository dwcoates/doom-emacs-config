package frontend

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
)

// transcriptEvent wraps a transcript line as the store event a consumer sees.
func transcriptEvent(t *testing.T, tl *datav1.TranscriptLine) *corev1.Event {
	t.Helper()
	return &corev1.Event{
		Seq:          7,
		ProducedAtMs: producedMs,
		SessionId:    "s1",
		Payload:      &corev1.Event_Vendor{Vendor: mustAny(t, tl)},
		DedupKey:     "d1",
	}
}

// assistantLine builds one assistant transcript record with the given envelope
// facts and a single text block, which is what an agent's spoken turn is.
func assistantLine(uuid, text string, sidechain bool, sourceToolUseID, agentID string) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{
		Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{
				Uuid:            uuid,
				IsSidechain:     sidechain,
				SourceToolUseId: sourceToolUseID,
				AgentId:         agentID,
			},
			Message: &datav1.ApiAssistantMessage{
				Id: "msg_" + uuid,
				Content: []*datav1.ContentBlock{
					{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}},
				},
			},
		},
	}}
}

// --- THE ACCEPTANCE CRITERION ----------------------------------------------

func TestADetachedAgentsEmissionReachesItsBubble(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "subagent speaking", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Detached) != 1 || len(c.Detached[0].Emissions) != 1 {
		t.Fatalf("a detached agent's response must be routed to its bubble, got %d folds", len(c.Detached))
	}
}

func TestADetachedAgentsEmissionIsNotAFeedItem(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "subagent speaking", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if got := len(c.Feed.GetItems()); got != 0 {
		t.Fatalf("a detached agent's emissions must never reach a frontend as top-level ConversationItems, got %d feed items", got)
	}
}

func TestAnAllDetachedEventStillCarriesItsReplayCursor(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "subagent speaking", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if c.Feed == nil || c.Feed.GetThroughSeq() != 7 {
		t.Fatal("swallowing an all-detached event would strand every client's replay cursor behind it")
	}
}

func TestTheMainAgentsEmissionStaysOnTheFeed(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "main agent speaking", false, "", "")))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Feed.GetItems()) != 1 {
		t.Fatalf("the main conversation must stay on the feed, got %d items", len(c.Feed.GetItems()))
	}
}

func TestTheMainAgentsEmissionIsNotRoutedToAnyBubble(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "main agent speaking", false, "", "")))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Detached) != 0 {
		t.Fatalf("nothing the main agent said is detached work, got %d folds", len(c.Detached))
	}
}

// --- fold identity ---------------------------------------------------------

func TestADetachedFoldNamesTheCallThatLaunchedIt(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "x", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if c.Detached[0].SourceToolUseID != "tu_task" {
		t.Fatalf("want the launching call named, got %q", c.Detached[0].SourceToolUseID)
	}
}

func TestADetachedFoldNamesItsAgent(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "x", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if c.Detached[0].AgentID != "agent_1" {
		t.Fatalf("want the agent named, got %q", c.Detached[0].AgentID)
	}
}

func TestADetachedFoldCarriesTheSameEmissionTheFeedWouldHave(t *testing.T) {
	detached, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "hello", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	top, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "hello", false, "", "")))
	if err != nil {
		t.Fatal(err)
	}
	want := top.Feed.GetItems()[0].GetAgent().GetResponse().GetBody().GetContent()[0].GetText().GetText()
	got := detached.Detached[0].Emissions[0].GetResponse().GetBody().GetContent()[0].GetText().GetText()
	if got != want {
		t.Fatalf("a bubble's emissions come from the same curation the feed uses: want %q, got %q", want, got)
	}
}

func TestTwoDetachedAgentsFoldSeparately(t *testing.T) {
	first := assistantLine("u1", "a", true, "tu_a", "agent_a")
	second := assistantLine("u2", "b", true, "tu_b", "agent_b")
	c1, _ := CurateEvent("/ws", "f1", transcriptEvent(t, first))
	c2, _ := CurateEvent("/ws", "f1", transcriptEvent(t, second))
	if c1.Detached[0].SourceToolUseID == c2.Detached[0].SourceToolUseID {
		t.Fatal("two detached agents must not fold into one bubble")
	}
}

func TestADetachedRecordIsAbsentFromTheFeedsEnvelopes(t *testing.T) {
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, assistantLine("u1", "x", true, "tu_task", "agent_1")))
	if err != nil {
		t.Fatal(err)
	}
	if _, present := c.Envelopes["u1"]; present {
		t.Fatal("a detached record left the feed, so the feed's own curators must not see its envelope")
	}
}

// --- detached user records -------------------------------------------------

func sidechainUserLine(uuid string, blocks []*datav1.ContentBlock) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{
		User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid, IsSidechain: true, SourceToolUseId: "tu_task", AgentId: "agent_1"},
			Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
				ContentBlocks: &datav1.ApiContentBlocks{Blocks: blocks},
			}},
		},
	}}
}

func TestADetachedToolResultFoldsAsAToolResultEmission(t *testing.T) {
	line := sidechainUserLine("u1", []*datav1.ContentBlock{
		{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "tu_inner"}}},
	})
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if c.Detached[0].Emissions[0].GetToolResult().GetResult().GetToolUseId() != "tu_inner" {
		t.Fatal("a subagent's tool results belong in its bubble, on the same arm the feed's tool cards use")
	}
}

func TestADetachedLaunchPromptIsWithheldRatherThanPromotedToTheFeed(t *testing.T) {
	line := sidechainUserLine("u1", []*datav1.ContentBlock{
		{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "go do the thing"}}},
	})
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Feed.GetItems()) != 0 {
		t.Fatalf("a record with no emission arm is withheld, never promoted to the feed, got %d items", len(c.Feed.GetItems()))
	}
}

func TestADetachedLaunchPromptIsReportedWithheldRatherThanDroppedSilently(t *testing.T) {
	line := sidechainUserLine("u1", []*datav1.ContentBlock{
		{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "go do the thing"}}},
	})
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.WithheldDetached) != 1 {
		t.Fatalf("a bubble quietly missing records looks exactly like a quiet agent; want it reported, got %v", c.WithheldDetached)
	}
}

// --- tool names ------------------------------------------------------------

func TestCurateEventHarvestsATopLevelToolName(t *testing.T) {
	line := &datav1.TranscriptLine{Line: &datav1.TranscriptLine_Assistant{
		Assistant: &datav1.AssistantLine{
			Envelope: &datav1.LineEnvelope{Uuid: "u1"},
			Message: &datav1.ApiAssistantMessage{Content: []*datav1.ContentBlock{
				{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: "tu_1", Name: "Frobnicate"}}},
			}},
		},
	}}
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if c.ToolNames["tu_1"] != "Frobnicate" {
		t.Fatalf("the tool name is the only source for an unclassified bubble's tool_name, got %v", c.ToolNames)
	}
}

// --- typed outcomes --------------------------------------------------------

func outcomeLine(uuid string, blocks []*datav1.ContentBlock, res *datav1.ToolUseResult) *datav1.TranscriptLine {
	return &datav1.TranscriptLine{Line: &datav1.TranscriptLine_User{
		User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiUserMessage{Content: &datav1.ApiUserMessage_ContentBlocks{
				ContentBlocks: &datav1.ApiContentBlocks{Blocks: blocks},
			}},
			ToolUseResult:    res,
			HasToolUseResult: true,
		},
	}}
}

func TestCurateEventCorrelatesAToolOutcomeToItsCall(t *testing.T) {
	line := outcomeLine("u1",
		[]*datav1.ContentBlock{{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "tu_1"}}}},
		&datav1.ToolUseResult{Result: &datav1.ToolUseResult_Bash{Bash: &datav1.BashResult{BackgroundTaskId: "task_1"}}})
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Outcomes) != 1 || c.Outcomes[0].ToolUseID != "tu_1" {
		t.Fatalf("want the outcome correlated to tu_1, got %v", c.Outcomes)
	}
}

func TestCurateEventLeavesAnAmbiguousOutcomeUncorrelated(t *testing.T) {
	line := outcomeLine("u1", []*datav1.ContentBlock{
		{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "tu_1"}}},
		{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "tu_2"}}},
	}, &datav1.ToolUseResult{Result: &datav1.ToolUseResult_Bash{Bash: &datav1.BashResult{BackgroundTaskId: "task_1"}}})
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if len(c.Outcomes) != 0 {
		t.Fatal("a launch attributed to a guessed call would open a bubble under the wrong card")
	}
}

func TestCurateEventMarksAnOutcomeProducedInsideADetachedAgent(t *testing.T) {
	line := outcomeLine("u1",
		[]*datav1.ContentBlock{{Block: &datav1.ContentBlock_ToolResult{ToolResult: &datav1.ToolResultBlock{ToolUseId: "tu_1"}}}},
		&datav1.ToolUseResult{Result: &datav1.ToolUseResult_Bash{Bash: &datav1.BashResult{BackgroundTaskId: "task_1"}}})
	line.GetUser().GetEnvelope().IsSidechain = true
	line.GetUser().GetEnvelope().SourceToolUseId = "tu_parent"
	c, err := CurateEvent("/ws", "f1", transcriptEvent(t, line))
	if err != nil {
		t.Fatal(err)
	}
	if !c.Outcomes[0].FromDetachedAgent {
		t.Fatal("a detachment launched inside a bubble is a nested dispatch and must be marked as one")
	}
}
