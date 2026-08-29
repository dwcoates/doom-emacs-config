package convert

// detached_test.go — spool deltas, the exit marker, and the LOST verdict's
// stability.

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestBashDeltaFromOffsetIsAGapDetector(t *testing.T) {
	// Arrange. from_offset MUST equal the bytes the consumer already holds;
	// anything else means bytes were lost and the consumer REFUSES the frame
	// rather than concatenating across a hole and drawing output that never
	// existed.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	entry := c.BashDelta(at, "toolu_run", "more bytes", 4096)

	// Assert.
	update := entry.GetAgentUpdate().GetBash().GetFrame().GetUpdate()
	if got := update.GetFromOffset(); got != 4096 {
		t.Fatalf("from_offset = %d, want 4096", got)
	}
	if got := update.GetNewOutput(); got != "more bytes" {
		t.Fatalf("new_output = %q, want the delta verbatim, unparsed and not line-split", got)
	}
}

func TestEverySpoolDerivedWriteIsItsOwnRow(t *testing.T) {
	// Arrange. THE STORE SUPERSEDES A ROW WHOLE, so one key for the whole run
	// would leave it holding only its most recent delta — every earlier chunk of
	// output erased by the next. store.v1 WatchBashRun replays a run's rows in
	// write order, which is only possible if each write IS a row.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	first := c.BashDelta(at, "toolu_run", "out", 0)
	second := c.BashDelta(at, "toolu_run", "more", 3)
	terminal := c.BashExited(at, "toolu_run", "outmore", 0, 0)

	// Assert.
	keys := []string{first.GetUpsertKey(), second.GetUpsertKey(), terminal.GetUpsertKey()}
	seen := map[string]bool{}
	for _, key := range keys {
		if seen[key] {
			t.Fatalf("two spool-derived writes share the key %q; the later would erase the earlier", key)
		}
		seen[key] = true
	}
	if got := terminal.GetUpsertKey(); got != BashTerminalKey("toolu_run") {
		t.Fatalf("terminal key = %q, want the run's single terminal key", got)
	}
}

func TestARereadDeltaSupersedesItsOwnRowRatherThanAppendingACopy(t *testing.T) {
	// Arrange. The delta's key is its from_offset, which IS the delta's identity:
	// the same bytes re-read after a restart must land on the row they already
	// own, or a replay grows a second copy of the run's output.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	first := c.BashDelta(at, "toolu_run", "out", 512)
	replayed := c.BashDelta(at, "toolu_run", "out", 512)

	// Assert.
	if first.GetUpsertKey() != replayed.GetUpsertKey() {
		t.Fatalf("a re-read delta keyed %q vs %q; it must supersede its own row",
			first.GetUpsertKey(), replayed.GetUpsertKey())
	}
	if first.GetWriteId() != replayed.GetWriteId() {
		t.Fatalf("a re-read delta minted write_ids %q and %q; the digest is of file coordinates and must be identical",
			first.GetWriteId(), replayed.GetWriteId())
	}
}

func TestATerminalAndADeltaAtTheSameOffsetDoNotCollide(t *testing.T) {
	// Arrange. A command that produced no output at all settles from offset 0,
	// where its only delta also lives — so the terminal's key must not be
	// derivable from an offset at all.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	delta := c.BashDelta(at, "toolu_run", "EXIT=0\n", 0)
	terminal := c.BashExited(at, "toolu_run", "EXIT=0\n", 0, 0)

	// Assert.
	if delta.GetUpsertKey() == terminal.GetUpsertKey() {
		t.Fatalf("the terminal and the offset-0 delta share the key %q", delta.GetUpsertKey())
	}
}

func TestLostVerdictIsStableSoAReEmissionIsANoOp(t *testing.T) {
	// Arrange. A run is lost ONCE however many sweeps observe it, so the write
	// identity must be stable and a re-emission absorbed rather than appending a
	// second terminal.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	first := c.BashLost(at, "toolu_run", "", 0, LostWentSilent)
	second := c.BashLost(at, "toolu_run", "", 0, LostWentSilent)

	// Assert.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatalf("write ids differ: %q vs %q", first.GetWriteId(), second.GetWriteId())
	}
}

func TestLostAndExitedShareTheTerminalDiscriminator(t *testing.T) {
	// Arrange. A run reaches exactly ONE terminal. Whichever way we conclude it,
	// the terminal is the same write, so a LOST verdict that arrives after an
	// observed exit cannot append a second, contradictory ending.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	exited := c.BashExited(at, "toolu_run", "out", 0, 0)
	lost := c.BashLost(at, "toolu_run", "out", 0, LostSweptUp)

	// Assert.
	if exited.GetWriteId() != lost.GetWriteId() {
		t.Fatal("a run's terminal is one write: a later LOST must be absorbed, never appended beside an observed exit")
	}
}

func TestBashOutputIsAlwaysSetEvenWhenSilent(t *testing.T) {
	// Arrange. An empty output still draws its header, so a reader can tell "ran
	// and was silent" from "has not run".
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	entry := c.BashExited(at, "toolu_run", "", 0, 0)

	// Assert.
	completed := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetCompleted()
	if completed.GetOutput() == nil {
		t.Fatal("output must always be set")
	}
	if completed.GetOutput().GetText() == nil {
		t.Fatal("a silent command's output must still name its form")
	}
}

func TestWorkflowJournalRecordsAreResidueThisWave(t *testing.T) {
	// Arrange. WORKFLOW IS KICKED: the vocabulary stays in the contract and the
	// files are tailed so nothing is lost, but no workflow frame is produced.
	// Filing them as `unknown` would say "we do not model this", which is false —
	// the model exists and the FEATURE does not.
	cases := []string{"started", "result"}
	for _, kind := range cases {
		t.Run(kind, func(t *testing.T) {
			c := newTestConverter(t)
			record := decode(t, `{"type":"`+kind+`","key":"v2:abc","agentId":"ace45c0b3342cf275"}`)

			// Act.
			entries := c.JournalRecord(record, testAttribution(0), "wf_1")

			// Assert.
			if len(entries) != 1 {
				t.Fatalf("entries = %d, want 1", len(entries))
			}
			if got := vendorKindOf(entries[0]); got != "workflow_journal/"+kind {
				t.Fatalf("kind = %q, want workflow_journal/%s", got, kind)
			}
		})
	}
}

func TestUnknownJournalShapeIsUnknownResidue(t *testing.T) {
	// Arrange. A journal holds exactly TWO record shapes; a third is a modelling
	// gap, not a withholding decision.
	c := newTestConverter(t)
	record := decode(t, `{"type":"something-else","key":"v2:abc"}`)

	// Act.
	entries := c.JournalRecord(record, testAttribution(0), "wf_1")

	// Assert.
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown == nil || unknown.GetDiscriminator() != "something-else" {
		t.Fatalf("want unknown naming the shape, got %v", unknown)
	}
}

func TestATerminalOverTheOutputBoundStatesPartialRatherThanWhole(t *testing.T) {
	// Arrange. A producer that kept only part of what a run said must SAY so:
	// a consumer handed `whole` over a prefix has no way to find out it was
	// lied to, and the count is what makes the loss investigable.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashExited(at, "toolu_run", "the first megabyte", 4096, 0)

	// Assert.
	text := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetCompleted().GetOutput().GetText()
	if text.GetWhole() != nil {
		t.Fatal("a terminal that dropped bytes must not claim to carry the whole output")
	}
	if got := text.GetPartial().GetBytesOmitted(); got != 4096 {
		t.Fatalf("bytes_omitted = %d, want 4096", got)
	}
}

func TestATerminalThatDroppedNothingStatesWhole(t *testing.T) {
	// Arrange. The other half: a run inside the bound is carried entire, and
	// saying so is what saves a consumer from comparing lengths against totals.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashExited(at, "toolu_run", "all of it", 0, 0)

	// Assert.
	text := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetCompleted().GetOutput().GetText()
	if text.GetWhole() == nil {
		t.Fatalf("a terminal that dropped nothing must state whole: %v", text.GetExtent())
	}
}

func TestALostRunStatesTheArmItConcludedOn(t *testing.T) {
	// Arrange. Landing 3 gave DetachedLost a home on the interrupted cause, so
	// HOW the reader stopped seeing a run is a statement the WIRE carries rather
	// than a fact surviving only in this reader's log.
	tests := []struct {
		name   string
		reason LostReason
		want   func(*conversationv1.DetachedLost) bool
	}{
		{
			name:   "the file disappeared",
			reason: LostFileVanished,
			want:   func(l *conversationv1.DetachedLost) bool { return l.GetFileVanished() != nil },
		},
		{
			name:   "the file stopped growing",
			reason: LostWentSilent,
			want:   func(l *conversationv1.DetachedLost) bool { return l.GetWentSilent() != nil },
		},
		{
			name:   "a boot sweep found it open",
			reason: LostSweptUp,
			want:   func(l *conversationv1.DetachedLost) bool { return l.GetSweptUp() != nil },
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			c := newTestConverter(t)
			at := testAttribution(0)

			// Act.
			entry := c.BashLost(at, "toolu_run", "so far", 0, test.reason)

			// Assert.
			cut := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
			if cut.GetLost() == nil {
				t.Fatalf("a LOST run must state the lost cause: %v", cut.GetCause())
			}
			if !test.want(cut.GetLost()) {
				t.Fatalf("the lost cause names the wrong arm: %v", cut.GetLost().GetHow())
			}
		})
	}
}

func TestALostRunIsNeverBlamedOnAPersonOrATimeout(t *testing.T) {
	// Arrange. by_user and timed_out name DECISIONS, and no decision was
	// observed — the reader merely stopped seeing the file.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashLost(at, "toolu_run", "so far", 0, LostWentSilent)

	// Assert.
	cut := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
	if cut.GetByUser() != nil {
		t.Fatal("a LOST run must not be drawn as a person's cancel")
	}
	if cut.GetTimedOut() != nil {
		t.Fatal("a LOST run must not be drawn as a timeout")
	}
}

func TestAnUnknownLostReasonFailsRatherThanPickingAnArm(t *testing.T) {
	// Arrange. The three arms ARE the reader's vocabulary, so a fourth string
	// means this package and the staleness policy have drifted — and choosing an
	// arm to keep going would have the wire assert something nobody observed.
	defer func() {
		if recover() == nil {
			t.Fatal("an unknown LOST reason must fail hard, not resolve to an arm")
		}
	}()

	// Act.
	DetachedLostArm(LostReason("invented"))
}

func TestACancelledRunCarriesTheOutputItHadProducedAndBlamesThePerson(t *testing.T) {
	// Arrange. A TaskStop result IS evidence of a decision, which is the one
	// thing `by_user` may be set on — and the terminal owes what the run said.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	entry := c.BashCancelled(at, "toolu_run", "partial work\n", 0, 1700000000000)

	// Assert.
	cut := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted()
	if cut.GetByUser() == nil {
		t.Fatalf("a stop is a person's decision and must state by_user: %v", cut.GetCause())
	}
	if got := cut.GetOutput().GetText().GetStdout(); got != "partial work\n" {
		t.Fatalf("cancelled stdout = %q, want the output the run had produced", got)
	}
	if got := entry.GetUpsertKey(); got != BashTerminalKey("toolu_run") {
		t.Fatalf("cancelled terminal keyed %q, want the run's terminal key", got)
	}
}
