package holds_test

import (
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/wsm"
)

func TestTrayConvertsEachClassificationArm(t *testing.T) {
	tests := []struct {
		name string
		held wsm.HeldPrompt
		want string
	}{
		{
			name: "unjudged draws classifying",
			held: hold("t1", "fix the flaky test"),
			want: "classifying",
		},
		{
			name: "classifying",
			held: verdict(hold("t1", "fix it"), wsm.ArmClassifying, ""),
			want: "classifying",
		},
		{
			name: "interject",
			held: verdict(hold("t1", "stop and rebase"), wsm.ArmInterject, "explicit stop"),
			want: "interject",
		},
		{
			name: "after this tool call",
			held: verdict(hold("t1", "also cover the edge case"), wsm.ArmAfterToolCall, "adds to the running work"),
			want: "after_tool_call",
		},
		{
			name: "hold for turn end",
			held: verdict(hold("t1", "then run the tests"), wsm.ArmHoldForTurnEnd, "no urgency"),
			want: "hold_for_turn_end",
		},
		{
			name: "uninterruptible turn",
			held: uninterruptible(hold("t1", "wait for me"), conversationv1.SessionCommand_SESSION_COMMAND_COMPACT),
			want: "uninterruptible_turn",
		},
		{
			name: "classification error",
			held: verdict(hold("t1", "who knows"), wsm.ArmClassificationError, "the judge answered neither token"),
			want: "classification_error",
		},
		{
			name: "unjudged under a daemon hold draws daemon_held",
			held: func() wsm.HeldPrompt { h := hold("t1", "after the merge"); h.Hold = kindOf(wsm.HoldMerge); return h }(),
			want: "daemon_held",
		},
		{
			name: "a verdict under a daemon hold keeps its verdict",
			held: func() wsm.HeldPrompt {
				h := verdict(hold("t1", "then run the tests"), wsm.ArmHoldForTurnEnd, "no urgency")
				h.Hold = kindOf(wsm.HoldMerge)
				return h
			}(),
			want: "hold_for_turn_end",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetHeldPrompts(testWS, []wsm.HeldPrompt{tc.held})

			// Assert.
			if got := classificationName(onlyPrompt(t, latest(t, r))); got != tc.want {
				t.Fatalf("classification arm = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTrayCarriesTheAfterToolCallRationale(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := verdict(hold("t1", "also cover the edge case"), wsm.ArmAfterToolCall, "adds to the running work")

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if got := onlyPrompt(t, latest(t, r)).GetAfterToolCall().GetRationale(); got != "adds to the running work" {
		t.Fatalf("rationale = %q, want the classifier's reason", got)
	}
}

func TestTrayCarriesTheInterjectRationale(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := verdict(hold("t1", "stop"), wsm.ArmInterject, "the user said stop")

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	got := onlyPrompt(t, latest(t, r)).GetInterject().GetRationale()
	if got != "the user said stop" {
		t.Fatalf("rationale = %q, want the classifier's stated reason", got)
	}
}

func TestTrayCarriesTheHoldForTurnEndAcceptance(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := verdict(hold("t1", "later"), wsm.ArmHoldForTurnEnd, "no urgency")
	held.Accepted = true

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if !onlyPrompt(t, latest(t, r)).GetHoldForTurnEnd().GetAccepted().GetAccepted() {
		t.Fatal("the accepted flag did not reach the tray")
	}
}

func TestTrayNamesTheUninterruptibleCommand(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := uninterruptible(hold("t1", "wait"), conversationv1.SessionCommand_SESSION_COMMAND_CLEAR)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	got := onlyPrompt(t, latest(t, r)).GetUninterruptibleTurn().GetCommand()
	if got != conversationv1.SessionCommand_SESSION_COMMAND_CLEAR {
		t.Fatalf("command = %v, want the recognized cut", got)
	}
}

func TestTrayRecordsAnUninterruptibleVerdictThatNamesNoCommand(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	held := uninterruptible(hold("t1", "wait"), conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if !hasError(surfaces.Records(), "daemon.holds.classification") {
		t.Fatal("an uninterruptible verdict with no command was not recorded as an invariant violation")
	}
}

func TestTrayStillDrawsAnUninterruptibleVerdictThatNamesNoCommand(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := uninterruptible(hold("t1", "wait"), conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert: the prompt is really held, so hiding it would hide pending work.
	if len(prompts(t, latest(t, r))) != 1 {
		t.Fatal("a defective verdict dropped the prompt from the tray")
	}
}

func TestTrayConvertsEachHoldArm(t *testing.T) {
	tests := []struct {
		name     string
		kind     *wsm.HoldKind
		schedule string
		want     string
	}{
		{name: "no condition leaves the oneof unset", kind: nil, want: ""},
		{name: "shutdown", kind: kindOf(wsm.HoldShutdown), schedule: "sched-1", want: "shutdown"},
		{name: "reconnect", kind: kindOf(wsm.HoldReconnect), want: "reconnect"},
		{name: "build refresh", kind: kindOf(wsm.HoldBuildRefresh), want: "build_refresh"},
		{name: "merge", kind: kindOf(wsm.HoldMerge), want: "merge"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			held := hold("t1", "queued")
			held.Hold = tc.kind
			held.ScheduleID = tc.schedule

			// Act.
			r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

			// Assert.
			if got := holdName(onlyPrompt(t, latest(t, r))); got != tc.want {
				t.Fatalf("hold arm = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestTrayCarriesTheShutdownSchedule(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := hold("t1", "queued")
	held.Hold = kindOf(wsm.HoldShutdown)
	held.ScheduleID = "sched-7"

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if got := onlyPrompt(t, latest(t, r)).GetShutdown().GetScheduleId(); got != "sched-7" {
		t.Fatalf("schedule id = %q, want the drain schedule the hold waits on", got)
	}
}

func TestTrayRecordsAShutdownHoldThatNamesNoSchedule(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	held := hold("t1", "queued")
	held.Hold = kindOf(wsm.HoldShutdown)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if !hasError(surfaces.Records(), "daemon.holds.hold") {
		t.Fatal("a shutdown hold with no schedule was not recorded as an invariant violation")
	}
}

func TestTrayCarriesTheWholeSaid(t *testing.T) {
	// Arrange: an image block is exactly what a text-only conversion drops.
	r, _ := newResolver(t)
	held := hold("t1", "look at this")
	held.Said.Content.Blocks = append(held.Said.Content.Blocks, &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Unsupported{
			Unsupported: &conversationv1.UnsupportedBlock{}},
	})

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if got := len(onlyPrompt(t, latest(t, r)).GetSaid().GetContent().GetBlocks()); got != 2 {
		t.Fatalf("the tray carried %d blocks, want the whole said's 2", got)
	}
}

func TestTrayCarriesTheMintedTurn(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("turn-42", "queued")})

	// Assert.
	if got := onlyPrompt(t, latest(t, r)).GetTurn().GetValue(); got != "turn-42" {
		t.Fatalf("turn = %q, want the minted turn a force or cancel names", got)
	}
}

func TestTrayStampsWhenThePromptWasQueued(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "queued")})

	// Assert.
	if got := onlyPrompt(t, latest(t, r)).GetQueuedAt().GetAtMs(); got != queuedAt.UnixMilli() {
		t.Fatalf("queued_at = %d, want %d", got, queuedAt.UnixMilli())
	}
}

func TestTraySkipsARetiredHold(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	retired := hold("t1", "already delivered")
	retired.Tombstone = &wsm.Tombstone{Kind: "delivered", At: queuedAt}

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{retired, hold("t2", "still standing")})

	// Assert.
	got := prompts(t, latest(t, r))
	if len(got) != 1 || got[0].GetTurn().GetValue() != "t2" {
		t.Fatalf("the tray drew %d prompts, want only the standing one", len(got))
	}
}

func TestTrayDrawsHoldsOldestFirst(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	newer := hold("t-newer", "second")
	newer.QueuedAt = queuedAt.Add(time.Second)
	older := hold("t-older", "first")

	// Act: handed to the tray newest-first, so only the resolver's order can fix it.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{newer, older})

	// Assert.
	got := prompts(t, latest(t, r))
	if len(got) != 2 || got[0].GetTurn().GetValue() != "t-older" {
		t.Fatalf("first drawn = %q, want the oldest hold", got[0].GetTurn().GetValue())
	}
}

func TestTrayBreaksAnOrderTieOnTheTurnId(t *testing.T) {
	// Arrange: two prompts queued in the same millisecond.
	r, _ := newResolver(t)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t-b", "second"), hold("t-a", "first")})

	// Assert.
	got := prompts(t, latest(t, r))
	if got[0].GetTurn().GetValue() != "t-a" {
		t.Fatalf("first drawn = %q, want the lower turn id", got[0].GetTurn().GetValue())
	}
}

func TestTrayNeverReordersTheCallersSlice(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	newer := hold("t-newer", "second")
	newer.QueuedAt = queuedAt.Add(time.Second)
	given := []wsm.HeldPrompt{newer, hold("t-older", "first")}

	// Act.
	r.SetHeldPrompts(testWS, given)

	// Assert: the slice belongs to the prompt queue.
	if given[0].Turn != ids.TurnID("t-newer") {
		t.Fatal("the resolver sorted its caller's memory")
	}
}

// uninterruptible stamps the uninterruptible-turn verdict with its command.
func uninterruptible(h wsm.HeldPrompt, command conversationv1.SessionCommand) wsm.HeldPrompt {
	h.Classification = &wsm.Classification{
		Arm:     wsm.ArmUninterruptibleTurn,
		Command: command,
		At:      queuedAt,
	}
	return h
}

// kindOf takes a hold kind's address, which the record stores as a pointer.
func kindOf(k wsm.HoldKind) *wsm.HoldKind { return &k }

// classificationName names the verdict arm the entry carries.
func classificationName(p *frontendv1.HeldPrompt) string {
	switch p.GetClassification().(type) {
	case *frontendv1.HeldPrompt_Classifying:
		return "classifying"
	case *frontendv1.HeldPrompt_Interject:
		return "interject"
	case *frontendv1.HeldPrompt_AfterToolCall:
		return "after_tool_call"
	case *frontendv1.HeldPrompt_HoldForTurnEnd:
		return "hold_for_turn_end"
	case *frontendv1.HeldPrompt_UninterruptibleTurn:
		return "uninterruptible_turn"
	case *frontendv1.HeldPrompt_ClassificationError:
		return "classification_error"
	case *frontendv1.HeldPrompt_DaemonHeld:
		return "daemon_held"
	default:
		return ""
	}
}

// holdName names the hold arm the entry carries, empty when none is set.
func holdName(p *frontendv1.HeldPrompt) string {
	switch p.GetHold().(type) {
	case *frontendv1.HeldPrompt_Shutdown:
		return "shutdown"
	case *frontendv1.HeldPrompt_Reconnect:
		return "reconnect"
	case *frontendv1.HeldPrompt_BuildRefresh:
		return "build_refresh"
	case *frontendv1.HeldPrompt_Merge:
		return "merge"
	default:
		return ""
	}
}

// wantBadge is one expected badge; an empty detail means none is carried.
type wantBadge struct {
	label, detail string
}

// wantStatus is a card's one badge, the fact it stands for, and its notes.
type wantStatus struct {
	badge wantBadge
	fact  string
	notes []string
}

// statusOf flattens a composed badge and its notes for comparison.
func statusOf(b *frontendv1.HeldPromptBadge, notes []*frontendv1.HeldPromptStatusNote) wantStatus {
	out := wantStatus{badge: wantBadge{label: b.GetLabel(), detail: b.GetDetail()}, notes: []string{}}
	if b.GetStandsFor() != nil {
		out.fact = string(b.ProtoReflect().WhichOneof(b.ProtoReflect().Descriptor().Oneofs().ByName("stands_for")).Name())
	}
	for _, n := range notes {
		out.notes = append(out.notes, n.GetSentence())
	}
	return out
}

// promptStatus is statusOf for a projected entry.
func promptStatus(p *frontendv1.HeldPrompt) wantStatus {
	return statusOf(p.GetBadge(), p.GetNotes())
}

func equalStatus(a, b wantStatus) bool {
	if a.badge != b.badge || a.fact != b.fact || len(a.notes) != len(b.notes) {
		return false
	}
	for i := range a.notes {
		if a.notes[i] != b.notes[i] {
			return false
		}
	}
	return true
}

func TestHeldStatusComposesEveryStatus(t *testing.T) {
	turn := &conversationv1.TurnId{Value: "t1"}
	merge := &frontendv1.HeldPrompt_Merge{Merge: &frontendv1.HeldPromptMergeHold{}}
	tests := []struct {
		name string
		p    *frontendv1.HeldPrompt
		want wantStatus
	}{
		{
			name: "classifying",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}},
			want: wantStatus{badge: wantBadge{"classifying", "queued — classifying"}, fact: "classifying", notes: []string{}},
		},
		{
			name: "interject",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_Interject{Interject: &frontendv1.HeldPromptInterject{}}},
			want: wantStatus{badge: wantBadge{"interrupting", "interjects"}, fact: "interject", notes: []string{}},
		},
		{
			name: "after this tool call",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_AfterToolCall{AfterToolCall: &frontendv1.HeldPromptAfterToolCall{}}},
			want: wantStatus{badge: wantBadge{"after this tool call", "joins the running turn after its current tool call"}, fact: "after_tool_call", notes: []string{}},
		},
		{
			name: "hold for turn end",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(false)},
			want: wantStatus{badge: wantBadge{"after this turn", ""}, fact: "hold_for_turn_end", notes: []string{}},
		},
		{
			name: "a confirmation is a note",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(true)},
			want: wantStatus{badge: wantBadge{"after this turn", ""}, fact: "hold_for_turn_end", notes: []string{"confirmed"}},
		},
		{
			name: "uninterruptible compact",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)},
			want: wantStatus{badge: wantBadge{"after /compact", "waits for /compact to finish"}, fact: "uninterruptible_turn", notes: []string{}},
		},
		{
			name: "uninterruptible clear",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_CLEAR)},
			want: wantStatus{badge: wantBadge{"after /clear", "waits for /clear to finish"}, fact: "uninterruptible_turn", notes: []string{}},
		},
		{
			name: "classification error",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_ClassificationError{ClassificationError: &frontendv1.HeldPromptClassificationError{Detail: "x"}}},
			want: wantStatus{badge: wantBadge{"unclassified", ""}, fact: "classification_error", notes: []string{}},
		},
		{
			name: "editing outranks the verdict",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(false), Editing: &frontendv1.HeldPromptEditing{}},
			want: wantStatus{badge: wantBadge{"editing", ""}, fact: "editing", notes: []string{"after this turn"}},
		},
		{
			name: "every fact at once: the edit claims the badge, the rest are notes in rank order",
			p: &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(true), Editing: &frontendv1.HeldPromptEditing{},
				Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}, Coalesced: &frontendv1.HeldPromptCoalesced{}},
			want: wantStatus{badge: wantBadge{"editing", ""}, fact: "editing",
				notes: []string{"held for the build refresh", "after this turn", "confirmed", "later prompts were folded into this one"}},
		},
		{
			name: "a hold outranks the verdict",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{ScheduleId: "s-1"}}},
			want: wantStatus{badge: wantBadge{"restart hold", "held for the scheduled restart (s-1)"}, fact: "shutdown", notes: []string{"after this turn"}},
		},
		{
			name: "build refresh hold",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}},
			want: wantStatus{badge: wantBadge{"build refresh", "held for the build refresh"}, fact: "build_refresh", notes: []string{"after this turn"}},
		},
		{
			name: "the reported card: a merge hold over a turn-end verdict shows the merge",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: merge},
			want: wantStatus{badge: wantBadge{"after the merge", "held until the merge ends; the workspace stays open for it"}, fact: "merge", notes: []string{"after this turn"}},
		},
		{
			name: "merge hold under daemon_held draws the hold's badge alone",
			p:    &frontendv1.HeldPrompt{Classification: &frontendv1.HeldPrompt_DaemonHeld{DaemonHeld: &frontendv1.HeldPromptDaemonHeld{}}, Hold: merge},
			want: wantStatus{badge: wantBadge{"after the merge", "held until the merge ends; the workspace stays open for it"}, fact: "merge", notes: []string{}},
		},
		{
			name: "reconnect hold",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Reconnect{Reconnect: &frontendv1.HeldPromptReconnectHold{}}},
			want: wantStatus{badge: wantBadge{"after reconnect", "held until the session reconnects"}, fact: "reconnect", notes: []string{"after this turn"}},
		},
		{
			name: "coalescence is a note",
			p:    &frontendv1.HeldPrompt{Classification: &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}, Coalesced: &frontendv1.HeldPromptCoalesced{}},
			want: wantStatus{badge: wantBadge{"classifying", "queued — classifying"}, fact: "classifying", notes: []string{"later prompts were folded into this one"}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			got := statusOf(holds.HeldStatus(tc.p, log))

			// Assert.
			if !equalStatus(got, tc.want) {
				t.Fatalf("status = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestHeldStatusLabelsAreOneToThreeWords(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	all := []*frontendv1.HeldPrompt{
		{Classification: &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}},
		{Classification: &frontendv1.HeldPrompt_Interject{Interject: &frontendv1.HeldPromptInterject{}}},
		{Classification: holdForTurnEnd(true)},
		{Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)},
		{Classification: &frontendv1.HeldPrompt_ClassificationError{ClassificationError: &frontendv1.HeldPromptClassificationError{}}},
		{Classification: holdForTurnEnd(false), Editing: &frontendv1.HeldPromptEditing{}},
		{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{ScheduleId: "s"}}},
		{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}},
		{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Reconnect{Reconnect: &frontendv1.HeldPromptReconnectHold{}}},
		{Classification: &frontendv1.HeldPrompt_DaemonHeld{DaemonHeld: &frontendv1.HeldPromptDaemonHeld{}}, Hold: &frontendv1.HeldPrompt_Merge{Merge: &frontendv1.HeldPromptMergeHold{}}},
	}

	for _, p := range all {
		// Act.
		b, _ := holds.HeldStatus(p, log)

		// Assert.
		if n := len(strings.Fields(b.GetLabel())); n < 1 || n > 3 {
			t.Fatalf("label %q has %d words, want 1 to 3", b.GetLabel(), n)
		}
	}
}

func TestTruncateLabel(t *testing.T) {
	tests := []struct {
		name, in, want string
	}{
		{name: "short literal is kept whole", in: "/compact", want: "/compact"},
		{name: "exactly the limit is kept whole", in: strings.Repeat("a", 24), want: strings.Repeat("a", 24)},
		{name: "a longer literal is cut to the limit with an ellipsis", in: "/" + strings.Repeat("x", 40), want: "/" + strings.Repeat("x", 22) + "…"},
		{name: "multibyte runes are counted as characters", in: strings.Repeat("é", 30), want: strings.Repeat("é", 23) + "…"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := holds.TruncateLabel(tc.in, 24)

			// Assert.
			if got != tc.want {
				t.Fatalf("TruncateLabel(%q) = %q, want %q", tc.in, got, tc.want)
			}
		})
	}
}

func TestHeldStatusRecordsAnUninterruptibleBadgeThatNamesNoCommand(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED)}

	// Act.
	got := statusOf(holds.HeldStatus(p, log))

	// Assert: the prompt stays visible, and the defect is recorded loudly.
	want := wantStatus{badge: wantBadge{"after a context cut", "waits for a context cut to finish"}, fact: "uninterruptible_turn", notes: []string{}}
	if !equalStatus(got, want) {
		t.Fatalf("status = %+v, want %+v", got, want)
	}
	if !hasError(log.Records(), "daemon.holds.badge") {
		t.Fatal("a badge with no command literal was not recorded as an invariant violation")
	}
}

func TestHeldStatusRecordsAMissingVerdict(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{}

	// Act.
	got, _ := holds.HeldStatus(p, log)

	// Assert: no words are invented; the frontend refuses the badgeless entry.
	if got != nil {
		t.Fatalf("badge = %+v, want none for a verdict that does not exist", got)
	}
	if !hasError(log.Records(), "daemon.holds.badge") {
		t.Fatal("a verdict with no badge was not recorded as an invariant violation")
	}
}

func TestHeldStatusShutdownWithNoScheduleOmitsTheId(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{}}}

	// Act.
	got := statusOf(holds.HeldStatus(p, log))

	// Assert: the projection already logged the missing schedule.
	want := wantStatus{badge: wantBadge{"restart hold", "held for the scheduled restart"}, fact: "shutdown", notes: []string{"after this turn"}}
	if !equalStatus(got, want) {
		t.Fatalf("status = %+v, want %+v", got, want)
	}
}

func TestHeldStatusNeverEmitsAnEmptyLabel(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := []wsm.HeldPrompt{
		hold("t0", "a"),
		verdict(hold("t1", "b"), wsm.ArmInterject, ""),
		verdict(hold("t2", "c"), wsm.ArmHoldForTurnEnd, ""),
		verdict(hold("t3", "d"), wsm.ArmClassificationError, ""),
		uninterruptible(hold("t4", "e"), conversationv1.SessionCommand_SESSION_COMMAND_COMPACT),
	}

	// Act.
	r.SetHeldPrompts(testWS, held)

	// Assert.
	for _, p := range prompts(t, latest(t, r)) {
		if p.GetBadge().GetLabel() == "" {
			t.Fatalf("turn %s carried an empty badge label", p.GetTurn().GetValue())
		}
	}
}

func TestTrayCarriesTheComposedBadge(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := verdict(hold("t1", "then run the tests"), wsm.ArmHoldForTurnEnd, "no urgency")
	held.Accepted = true
	held.Hold = kindOf(wsm.HoldShutdown)
	held.ScheduleID = "s-7"

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	got := promptStatus(onlyPrompt(t, latest(t, r)))
	want := wantStatus{badge: wantBadge{"restart hold", "held for the scheduled restart (s-7)"}, fact: "shutdown", notes: []string{"after this turn", "confirmed"}}
	if !equalStatus(got, want) {
		t.Fatalf("status = %+v, want %+v", got, want)
	}
}

func holdForTurnEnd(accepted bool) *frontendv1.HeldPrompt_HoldForTurnEnd {
	return &frontendv1.HeldPrompt_HoldForTurnEnd{HoldForTurnEnd: &frontendv1.HeldPromptHoldForTurnEnd{
		Accepted: &frontendv1.HeldPromptAccepted{Accepted: accepted}}}
}

func uninterruptibleArm(command conversationv1.SessionCommand) *frontendv1.HeldPrompt_UninterruptibleTurn {
	return &frontendv1.HeldPrompt_UninterruptibleTurn{UninterruptibleTurn: &frontendv1.HeldPromptUninterruptibleTurn{Command: command}}
}

func TestTrayCarriesAHeldSessionAct(t *testing.T) {
	tests := []struct {
		name string
		act  wsm.HeldAct
		want func(*frontendv1.HeldSessionAct) string
	}{
		{name: "a model change", act: wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"},
			want: func(a *frontendv1.HeldSessionAct) string { return a.GetModel().GetModel() }},
		{name: "a permission-mode change", act: wsm.HeldAct{Kind: wsm.ActPermissionMode, Value: "plan"},
			want: func(a *frontendv1.HeldSessionAct) string { return a.GetPermissionMode().GetMode() }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			held := hold("t1", "/model opus")
			act := tc.act
			held.Act = &act

			// Act.
			r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

			// Assert.
			if got := tc.want(onlyPrompt(t, latest(t, r)).GetAct()); got != tc.act.Value {
				t.Fatalf("act value = %q, want %q", got, tc.act.Value)
			}
		})
	}
}

func TestTrayCarriesNoActForAPrompt(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{hold("t1", "fix it")})

	// Assert.
	if act := onlyPrompt(t, latest(t, r)).GetAct(); act != nil {
		t.Fatalf("act = %v, want none on a prompt", act)
	}
}

func TestTrayRecordsAnUnknownActLoudly(t *testing.T) {
	// Arrange.
	r, sink := newResolver(t)
	held := hold("t1", "theme dark")
	held.Act = &wsm.HeldAct{Kind: "theme", Value: "dark"}

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if act := onlyPrompt(t, latest(t, r)).GetAct(); act != nil {
		t.Fatalf("act = %v, want none drawn for an unknown kind", act)
	}
	if !hasError(sink.Records(), "daemon.holds.act") {
		t.Fatalf("the unknown act was not recorded at error: %v", sink.Records())
	}
}

func TestTrayMarksACoalescedHold(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := hold("t1", "a\nb")
	held.Coalesced = true

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	if onlyPrompt(t, latest(t, r)).GetCoalesced() == nil {
		t.Fatal("the coalesced mark did not reach the tray")
	}
}

func TestTrayComposesTheCoalescedNote(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := hold("t1", "a\nb")
	held.Coalesced = true

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	got := promptStatus(onlyPrompt(t, latest(t, r)))
	if got.badge.label != "classifying" || len(got.notes) != 1 || got.notes[0] != "later prompts were folded into this one" {
		t.Fatalf("status = %+v, want the verdict's badge and the coalesced note", got)
	}
}

// queuedSecond is a hold queued one second after queuedAt, behind hold().
func queuedSecond(turn, text string) wsm.HeldPrompt {
	h := hold(turn, text)
	h.QueuedAt = queuedAt.Add(time.Second)
	return h
}

// foldAboveOf answers the turn an entry's fold button names, "" when the entry
// offers none.
func foldAboveOf(p *frontendv1.HeldPrompt) string {
	if p.FoldAbove == nil {
		return ""
	}
	return p.GetFoldAbove().GetAbove().GetValue()
}

func TestTrayOffersFoldAbove(t *testing.T) {
	modelChange := func(turn string) wsm.HeldPrompt {
		h := hold(turn, "model")
		h.Act = &wsm.HeldAct{Kind: wsm.ActModel, Value: "opus"}
		return h
	}
	tests := []struct {
		name    string
		holds   []wsm.HeldPrompt
		editing ids.TurnID
		// want is each drawn entry's fold target, in display order.
		want []string
	}{
		{
			name:  "the second of two prompts folds into the first, and the first offers nothing",
			holds: []wsm.HeldPrompt{hold("t1", "first"), queuedSecond("t2", "second")},
			want:  []string{"", "t1"},
		},
		{
			name:  "nothing is offered behind a held model change",
			holds: []wsm.HeldPrompt{modelChange("t1"), queuedSecond("t2", "second")},
			want:  []string{"", ""},
		},
		{
			name:  "nothing is offered behind a held /compact",
			holds: []wsm.HeldPrompt{hold("t1", "/compact"), queuedSecond("t2", "second")},
			want:  []string{"", ""},
		},
		{
			name:  "a held model change offers nothing itself",
			holds: []wsm.HeldPrompt{hold("t1", "first"), func() wsm.HeldPrompt { h := modelChange("t2"); h.QueuedAt = queuedAt.Add(time.Second); return h }()},
			want:  []string{"", ""},
		},
		{
			name:    "nothing is offered while the entry ahead is being edited",
			holds:   []wsm.HeldPrompt{hold("t1", "first"), queuedSecond("t2", "second")},
			editing: "t1",
			want:    []string{"", ""},
		},
		{
			name:    "nothing is offered while the entry itself is being edited",
			holds:   []wsm.HeldPrompt{hold("t1", "first"), queuedSecond("t2", "second")},
			editing: "t2",
			want:    []string{"", ""},
		},
		{
			name: "a retired entry between two prompts is skipped: the fold names the standing one",
			holds: []wsm.HeldPrompt{
				hold("t1", "first"),
				func() wsm.HeldPrompt {
					h := queuedSecond("t2", "gone")
					h.Tombstone = &wsm.Tombstone{Kind: "dropped", At: queuedAt}
					return h
				}(),
				func() wsm.HeldPrompt { h := hold("t3", "third"); h.QueuedAt = queuedAt.Add(2 * time.Second); return h }(),
			},
			want: []string{"", "t1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetEditing(testWS, tt.editing)

			// Act.
			r.SetHeldPrompts(testWS, tt.holds)

			// Assert.
			got := prompts(t, latest(t, r))
			if len(got) != len(tt.want) {
				t.Fatalf("the tray drew %d prompts, want %d", len(got), len(tt.want))
			}
			for i, p := range got {
				if foldAboveOf(p) != tt.want[i] {
					t.Fatalf("entry %d (%s) folds above %q, want %q", i, p.GetTurn().GetValue(), foldAboveOf(p), tt.want[i])
				}
			}
		})
	}
}
