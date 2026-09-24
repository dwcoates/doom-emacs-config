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

// wantBadge is one expected badge; an empty detail means none is carried.
type wantBadge struct {
	label, detail string
}

// badgesOf flattens a badge list for comparison.
func badgesOf(bs []*frontendv1.HeldPromptBadge) []wantBadge {
	out := make([]wantBadge, 0, len(bs))
	for _, b := range bs {
		out = append(out, wantBadge{label: b.GetLabel(), detail: b.GetDetail()})
	}
	return out
}

func equalBadges(a, b []wantBadge) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func TestHeldBadgesComposeEveryStatus(t *testing.T) {
	turn := &conversationv1.TurnId{Value: "t1"}
	tests := []struct {
		name string
		p    *frontendv1.HeldPrompt
		want []wantBadge
	}{
		{
			name: "classifying",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}},
			want: []wantBadge{{"classifying", "queued — classifying"}},
		},
		{
			name: "interject",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_Interject{Interject: &frontendv1.HeldPromptInterject{}}},
			want: []wantBadge{{"interrupting", "interjects"}},
		},
		{
			name: "hold for turn end",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(false)},
			want: []wantBadge{{"after this turn", ""}},
		},
		{
			name: "accepted hold for turn end",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(true)},
			want: []wantBadge{{"after this turn", ""}, {"confirmed", ""}},
		},
		{
			name: "uninterruptible compact",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)},
			want: []wantBadge{{"after /compact", "waits for /compact to finish"}},
		},
		{
			name: "uninterruptible clear",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_CLEAR)},
			want: []wantBadge{{"after /clear", "waits for /clear to finish"}},
		},
		{
			name: "classification error",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: &frontendv1.HeldPrompt_ClassificationError{ClassificationError: &frontendv1.HeldPromptClassificationError{Detail: "x"}}},
			want: []wantBadge{{"unclassified", ""}},
		},
		{
			name: "editing",
			p:    &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(false), Editing: &frontendv1.HeldPromptEditing{}},
			want: []wantBadge{{"after this turn", ""}, {"editing", ""}},
		},
		{
			name: "every fact at once, in the proto's order",
			p: &frontendv1.HeldPrompt{Turn: turn, Classification: holdForTurnEnd(true), Editing: &frontendv1.HeldPromptEditing{},
				Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}},
			want: []wantBadge{{"after this turn", ""}, {"editing", ""}, {"confirmed", ""}, {"build refresh", "held for the build refresh"}},
		},
		{
			name: "shutdown hold",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{ScheduleId: "s-1"}}},
			want: []wantBadge{{"after this turn", ""}, {"restart hold", "held for the scheduled restart (s-1)"}},
		},
		{
			name: "build refresh hold",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}},
			want: []wantBadge{{"after this turn", ""}, {"build refresh", "held for the build refresh"}},
		},
		{
			name: "session starting hold",
			p:    &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_SessionStarting{SessionStarting: &frontendv1.HeldPromptSessionStartingHold{}}},
			want: []wantBadge{{"after this turn", ""}, {"starting up", "held until the session is up"}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			got := badgesOf(holds.HeldBadges(tc.p, log))

			// Assert.
			if !equalBadges(got, tc.want) {
				t.Fatalf("badges = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestHeldBadgesLabelsAreOneToThreeWords(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	all := []*frontendv1.HeldPrompt{
		{Classification: &frontendv1.HeldPrompt_Classifying{Classifying: &frontendv1.HeldPromptClassifying{}}},
		{Classification: &frontendv1.HeldPrompt_Interject{Interject: &frontendv1.HeldPromptInterject{}}},
		{Classification: holdForTurnEnd(true)},
		{Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_COMPACT)},
		{Classification: &frontendv1.HeldPrompt_ClassificationError{ClassificationError: &frontendv1.HeldPromptClassificationError{}}},
		&frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{ScheduleId: "s"}}},
		&frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_BuildRefresh{BuildRefresh: &frontendv1.HeldPromptBuildRefreshHold{}}},
		&frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_SessionStarting{SessionStarting: &frontendv1.HeldPromptSessionStartingHold{}}},
	}

	for _, p := range all {
		// Act.
		for _, b := range holds.HeldBadges(p, log) {
			// Assert.
			if n := len(strings.Fields(b.GetLabel())); n < 1 || n > 3 {
				t.Fatalf("label %q has %d words, want 1 to 3", b.GetLabel(), n)
			}
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

func TestHeldBadgesRecordAnUninterruptibleBadgeThatNamesNoCommand(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{Classification: uninterruptibleArm(conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED)}

	// Act.
	got := badgesOf(holds.HeldBadges(p, log))

	// Assert: the prompt stays visible, and the defect is recorded loudly.
	want := []wantBadge{{"after a context cut", "waits for a context cut to finish"}}
	if !equalBadges(got, want) {
		t.Fatalf("badges = %+v, want %+v", got, want)
	}
	if !hasError(log.Records(), "daemon.holds.badges") {
		t.Fatal("a badge with no command literal was not recorded as an invariant violation")
	}
}

func TestHeldBadgesRecordAMissingVerdict(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{}

	// Act.
	got := holds.HeldBadges(p, log)

	// Assert: no words are invented; the frontend refuses the short list.
	if len(got) != 0 {
		t.Fatalf("badges = %+v, want none for a verdict that does not exist", badgesOf(got))
	}
	if !hasError(log.Records(), "daemon.holds.badges") {
		t.Fatal("a verdict with no badge was not recorded as an invariant violation")
	}
}

func TestHeldBadgesShutdownWithNoScheduleOmitsTheId(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	p := &frontendv1.HeldPrompt{Classification: holdForTurnEnd(false), Hold: &frontendv1.HeldPrompt_Shutdown{Shutdown: &frontendv1.HeldPromptShutdownHold{}}}

	// Act.
	got := badgesOf(holds.HeldBadges(p, log))

	// Assert: the projection already logged the missing schedule.
	want := []wantBadge{{"after this turn", ""}, {"restart hold", "held for the scheduled restart"}}
	if !equalBadges(got, want) {
		t.Fatalf("badges = %+v, want %+v", got, want)
	}
}

func TestHeldBadgesNeverEmitAnEmptyLabel(t *testing.T) {
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
		for _, b := range p.GetBadges() {
			if b.GetLabel() == "" {
				t.Fatalf("turn %s carried an empty badge label", p.GetTurn().GetValue())
			}
		}
	}
}

func TestTrayCarriesTheComposedBadges(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	held := verdict(hold("t1", "then run the tests"), wsm.ArmHoldForTurnEnd, "no urgency")
	held.Accepted = true
	held.Hold = kindOf(wsm.HoldShutdown)
	held.ScheduleID = "s-7"

	// Act.
	r.SetHeldPrompts(testWS, []wsm.HeldPrompt{held})

	// Assert.
	got := badgesOf(onlyPrompt(t, latest(t, r)).GetBadges())
	want := []wantBadge{{"after this turn", ""}, {"confirmed", ""}, {"restart hold", "held for the scheduled restart (s-7)"}}
	if !equalBadges(got, want) {
		t.Fatalf("badges = %+v, want %+v", got, want)
	}
}

func holdForTurnEnd(accepted bool) *frontendv1.HeldPrompt_HoldForTurnEnd {
	return &frontendv1.HeldPrompt_HoldForTurnEnd{HoldForTurnEnd: &frontendv1.HeldPromptHoldForTurnEnd{
		Accepted: &frontendv1.HeldPromptAccepted{Accepted: accepted}}}
}

func uninterruptibleArm(command conversationv1.SessionCommand) *frontendv1.HeldPrompt_UninterruptibleTurn {
	return &frontendv1.HeldPrompt_UninterruptibleTurn{UninterruptibleTurn: &frontendv1.HeldPromptUninterruptibleTurn{Command: command}}
}
