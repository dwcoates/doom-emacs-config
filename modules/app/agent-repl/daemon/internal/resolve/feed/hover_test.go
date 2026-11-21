package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"

	"google.golang.org/protobuf/proto"
)

// A HOVERED BUBBLE SHOWS WHAT IT IS, HOW LONG IT TOOK AND HOW LONG AGO IT WAS
// SENT OR SETTLED (owner request, 2026-11-19): FeedRow.hover, by one rule over
// the row's kind.

func activity(act *frontendv1.FeedTurnActivity) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: act}}
}

func subagentCard(label string, started, ended int64) *frontendv1.FeedSubagent {
	sub := &frontendv1.FeedSubagent{
		Label:   &frontendv1.FeedSubagentLabel{Text: label},
		Runtime: &frontendv1.FeedSubagentRuntime{StartedAtMs: started},
		State:   &frontendv1.FeedSubagent_Live{Live: &frontendv1.FeedSubagentLive{}},
	}
	if ended != 0 {
		sub.State = &frontendv1.FeedSubagent_Settled{Settled: &frontendv1.FeedSubagentSettled{EndedAtMs: ended}}
	}
	return sub
}

func mergeCard(started int64, result any) *frontendv1.FeedMerge {
	merge := &frontendv1.FeedMerge{Head: &frontendv1.FeedMergeHead{Runtime: &frontendv1.FeedMergeRuntime{StartedAtMs: started}}}
	switch r := result.(type) {
	case *frontendv1.FeedMergeSuccess:
		merge.Result = &frontendv1.FeedMerge_Success{Success: r}
	case *frontendv1.FeedMergeError:
		merge.Result = &frontendv1.FeedMerge_Error{Error: r}
	default:
		merge.Result = &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}
	}
	return merge
}

func TestHoverOfStampsEveryHoverableKind(t *testing.T) {
	heat := &frontendv1.TokenHeat{Position: 0.25}
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
		sent int64
		want *frontendv1.FeedHoverStamp
	}{
		{
			name: "a user prompt, at the instant it was sent",
			row:  &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}}},
			sent: 7_000,
			want: hoverStamp("Prompt", 0, 7_000),
		},
		{
			name: "an agent prompt, at the instant it was sent",
			row:  &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{}}},
			sent: 7_000,
			want: hoverStamp("Prompt", 0, 7_000),
		},
		{
			name: "a prompt whose sent instant is not yet known",
			row:  &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}}},
			want: hoverStamp("Prompt", 0, 0),
		},
		{
			name: "a settled response, restating its usage stamp",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Response{Response: &frontendv1.FeedResponse{
				Usage: &frontendv1.FeedResponseUsageStamp{Text: "2.1k", AtMs: 9_000, StartedAtMs: 3_000, Heat: heat},
			}}}),
			want: &frontendv1.FeedHoverStamp{
				Label:       &frontendv1.FeedHoverStampLabel{Text: "Response"},
				StartedAtMs: 3_000, AtMs: 9_000,
				Tokens: &frontendv1.FeedHoverStampTokens{Text: "2.1k", Heat: heat},
			},
		},
		{
			name: "a response that has stated no usage yet",
			row:  activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Response{Response: &frontendv1.FeedResponse{}}}),
			want: hoverStamp("Response", 0, 0),
		},
		{
			name: "a settled tool call, named by its tool",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_SimpleToolCall{SimpleToolCall: &frontendv1.FeedSimpleToolCall{
				Name: &frontendv1.FeedToolCallName{Text: "Bash"}, Clock: stoppedClock(1_000, 4_000),
			}}}),
			want: hoverStamp("Tool call Bash", 1_000, 4_000),
		},
		{
			name: "a running tool call",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_SimpleToolCall{SimpleToolCall: &frontendv1.FeedSimpleToolCall{
				Name: &frontendv1.FeedToolCallName{Text: "Read"}, Clock: runningClock(1_000),
			}}}),
			want: hoverStamp("Tool call Read", 1_000, 0),
		},
		{
			name: "a tool call whose producer stated no clock",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_SimpleToolCall{SimpleToolCall: &frontendv1.FeedSimpleToolCall{
				Name: &frontendv1.FeedToolCallName{Text: "Grep"},
			}}}),
			want: hoverStamp("Tool call Grep", 0, 0),
		},
		{
			name: "a skill, named by its invocation",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Skill{Skill: &frontendv1.FeedSkill{
				Invocation: &frontendv1.FeedSkillInvocation{Text: "/graphify"}, Clock: stoppedClock(1_000, 2_000),
			}}}),
			want: hoverStamp("Skill /graphify", 1_000, 2_000),
		},
		{
			name: "a hook",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Hook{Hook: &frontendv1.FeedHook{
				Clock: stoppedClock(1_000, 1_500),
			}}}),
			want: hoverStamp("Hook", 1_000, 1_500),
		},
		{
			name: "a permission",
			row:  &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Permission{Permission: &frontendv1.FeedPermission{Clock: runningClock(1_000)}}},
			want: hoverStamp("Permission", 1_000, 0),
		},
		{
			name: "a question",
			row:  &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Question{Question: &frontendv1.FeedQuestion{Clock: stoppedClock(1_000, 8_000)}}},
			want: hoverStamp("Question", 1_000, 8_000),
		},
		{
			name: "a settled shell",
			row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_ShellHead{ShellHead: &frontendv1.FeedShell{
				Runtime: &frontendv1.FeedShellRuntime{StartedAtMs: 1_000},
				State:   &frontendv1.FeedShell_Settled{Settled: &frontendv1.FeedShellSettled{EndedAtMs: 6_000}},
			}}},
			want: hoverStamp("Shell", 1_000, 6_000),
		},
		{
			name: "a live shell",
			row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_ShellHead{ShellHead: &frontendv1.FeedShell{
				Runtime: &frontendv1.FeedShellRuntime{StartedAtMs: 1_000},
				State:   &frontendv1.FeedShell_Live{Live: &frontendv1.FeedShellLive{}},
			}}},
			want: hoverStamp("Shell", 1_000, 0),
		},
		{
			name: "a settled sync subagent, named by its label",
			row:  activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Subagent{Subagent: subagentCard("Explore", 1_000, 9_000)}}),
			want: hoverStamp("Subagent Explore", 1_000, 9_000),
		},
		{
			name: "a live detached subagent",
			row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_DetachedSubagent{DetachedSubagent: &frontendv1.FeedDetachedSubagent{
				Subagent: subagentCard("Plan", 1_000, 0),
			}}},
			want: hoverStamp("Subagent Plan", 1_000, 0),
		},
		{
			name: "a merge under way",
			row:  activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Merge{Merge: mergeCard(1_000, nil)}}),
			want: hoverStamp("Merge", 1_000, 0),
		},
		{
			name: "a merge that landed",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Merge{Merge: mergeCard(1_000,
				&frontendv1.FeedMergeSuccess{EndedAtMs: 5_000})}}),
			want: hoverStamp("Merge", 1_000, 5_000),
		},
		{
			name: "a merge that failed",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Merge{Merge: mergeCard(1_000,
				&frontendv1.FeedMergeError{EndedAtMs: 4_000})}}),
			want: hoverStamp("Merge", 1_000, 4_000),
		},
		{
			name: "a fix gate testing the fix",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_FixGate{FixGate: &frontendv1.FeedFixGate{
				State: &frontendv1.FeedFixGate_Live{Live: &frontendv1.FeedMergeTabLive{StartedAtMs: 2_000}},
			}}}),
			want: hoverStamp("Testing the fix", 2_000, 0),
		},
		{
			name: "a fix gate whose run settled",
			row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_FixGate{FixGate: &frontendv1.FeedFixGate{
				State: &frontendv1.FeedFixGate_Settled{Settled: &frontendv1.FeedMergeTabSettled{StartedAtMs: 2_000, EndedAtMs: 7_000}},
			}}}),
			want: hoverStamp("Testing the fix", 2_000, 7_000),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := hoverOf(tc.row, tc.sent)

			// Assert.
			if !proto.Equal(got, tc.want) {
				t.Fatalf("hover = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestHoverOfStampsNothingOnAKindThatShowsNothing(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
	}{
		{name: "a turn's ending", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_TurnEnded{TurnEnded: &frontendv1.FeedTurnEnded{EndedAtMs: 5}}}},
		{name: "a separation", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Separation{Separation: &frontendv1.FeedSessionSeparation{}}}},
		{name: "an artifact", row: activity(&frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Artifact{Artifact: &frontendv1.FeedArtifact{}}})},
		{name: "a merge tab", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}}}},
		{name: "a removal", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Removed{Removed: &frontendv1.FeedRowRemoved{}}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert.
			if got := hoverOf(tc.row, 7_000); got != nil {
				t.Fatalf("hover = %v, want none", got)
			}
		})
	}
}

func TestRecordPromptSentKeepsTheFirstKnownInstant(t *testing.T) {
	tests := []struct {
		name string
		ats  []int64
		want int64
	}{
		{name: "one instant", ats: []int64{5_000}, want: 5_000},
		{name: "a restated prompt keeps its first instant", ats: []int64{5_000, 9_000}, want: 5_000},
		{name: "an unknown instant records nothing", ats: []int64{0}, want: 0},
		{name: "a known instant follows an unknown one", ats: []int64{0, 9_000}, want: 9_000},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := &feedState{sentAtMs: map[string]int64{}}

			// Act.
			for _, at := range tc.ats {
				recordPromptSent(f, "p", at)
			}

			// Assert.
			if got := f.sentAtMs["p"]; got != tc.want {
				t.Fatalf("sent = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestAUserPromptRowIsHoverStampedWithTheInstantItWasSent(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.deliverPromptAt("turn-1", "hello", 42_000)

	// Assert.
	want := hoverStamp("Prompt", 0, 42_000)
	if got := h.only(rootFeed()).GetHover(); !proto.Equal(got, want) {
		t.Fatalf("hover = %v, want %v", got, want)
	}
}

func TestARepublishedPromptRowKeepsTheInstantItWasSent(t *testing.T) {
	// Arrange: a prompt, then its turn's prompts restated.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "hello", 42_000)

	// Act.
	h.resolver.mu.Lock()
	h.resolver.restateTurnPrompts(h.resolver.state(testWorkspace), &conversationv1.TurnId{Value: "turn-1"})
	h.resolver.mu.Unlock()

	// Assert.
	if got := h.only(rootFeed()).GetHover().GetAtMs(); got != 42_000 {
		t.Fatalf("sent = %d, want 42000", got)
	}
}

func TestAnAgentAddressedPromptIsHoverStampedAtBothEnds(t *testing.T) {
	// Arrange: a subagent whose bubble the resolver has seen created.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}
	h.spawnSubagent("spawn-1", created, "Explore", "map the daemon")

	// Act: a prompt delivered to that subagent, its entry at 77s.
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-2"},
		Agent:  created,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock("also check the shim")},
		}},
	}, placeAt(77_000))

	// Assert: the outgoing send and the delivered prompt both carry the instant.
	var sent []int64
	for _, feed := range []feedid.Feed{rootFeed(), {Agent: created}} {
		for _, row := range h.rows(feed) {
			if row.GetAgentPrompt() != nil && row.GetTurn().GetValue() == "turn-2" {
				sent = append(sent, row.GetHover().GetAtMs())
			}
		}
	}
	if len(sent) != 2 || sent[0] != 77_000 || sent[1] != 77_000 {
		t.Fatalf("sent instants = %v, want 77000 at both ends", sent)
	}
}

func TestARetiredRowForgetsItsSentInstant(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "hello", 42_000)
	id := h.only(rootFeed()).GetId().GetValue()

	// Act.
	h.resolver.mu.Lock()
	s := h.resolver.state(testWorkspace)
	h.resolver.retire(s, rootFeed(), id)
	_, kept := h.resolver.feed(s, rootFeed()).sentAtMs[id]
	h.resolver.mu.Unlock()

	// Assert.
	if kept {
		t.Fatalf("the retired row %s kept its sent instant", id)
	}
}

func TestASubagentsCommissionIsHoverStampedWithTheInstantItsSpawnWasSent(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "spawn an explorer")
	created := &conversationv1.AgentId{Value: "agent-explore"}

	// Act: the spawn's entry sits at 55s.
	h.sendAt(&conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: "spawn-1"},
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
			Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: created,
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "go and look"},
				StartedAt:      &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
			}},
		}},
	}, 55_000)

	// Assert.
	var sent []int64
	for _, row := range h.rows(feedid.Feed{Agent: created}) {
		if row.GetAgentPrompt() != nil {
			sent = append(sent, row.GetHover().GetAtMs())
		}
	}
	if len(sent) != 1 || sent[0] != 55_000 {
		t.Fatalf("commission sent instants = %v, want [55000]", sent)
	}
}

func TestAPromptDrawnOutsideAnyEntryHasNoSentInstantUntilItsEntryDrawsIt(t *testing.T) {
	// Arrange: the prompt queue's mirror of an accepted prompt, drawn before
	// the conversation has the prompt's entry.
	h := newHarness(t)
	row := PromptRow(conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, nil, SaidText("hello"), nil)
	row.Turn = &conversationv1.TurnId{Value: "turn-1"}
	h.resolver.UpsertAtTurnAddress(testWorkspace, "turn-1", feedid.RowKey{Kind: feedid.KindPrompt, ID: "turn-1"}, row)
	mirrored := h.only(rootFeed()).GetHover()

	// Act: the conversation's own entry draws the same row.
	h.deliverPromptAt("turn-1", "hello", 42_000)

	// Assert.
	got := []int64{mirrored.GetAtMs(), h.only(rootFeed()).GetHover().GetAtMs()}
	if got[0] != 0 || got[1] != 42_000 {
		t.Fatalf("sent instants (mirrored, drawn) = %v, want [0 42000]", got)
	}
}
