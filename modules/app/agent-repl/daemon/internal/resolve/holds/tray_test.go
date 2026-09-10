package holds_test

import (
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
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
		{name: "session starting", kind: kindOf(wsm.HoldSessionStarting), want: "session_starting"},
		{name: "build refresh", kind: kindOf(wsm.HoldBuildRefresh), want: "build_refresh"},
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
	case *frontendv1.HeldPrompt_HoldForTurnEnd:
		return "hold_for_turn_end"
	case *frontendv1.HeldPrompt_UninterruptibleTurn:
		return "uninterruptible_turn"
	case *frontendv1.HeldPrompt_ClassificationError:
		return "classification_error"
	default:
		return ""
	}
}

// holdName names the hold arm the entry carries, empty when none is set.
func holdName(p *frontendv1.HeldPrompt) string {
	switch p.GetHold().(type) {
	case *frontendv1.HeldPrompt_Shutdown:
		return "shutdown"
	case *frontendv1.HeldPrompt_SessionStarting:
		return "session_starting"
	case *frontendv1.HeldPrompt_BuildRefresh:
		return "build_refresh"
	default:
		return ""
	}
}
