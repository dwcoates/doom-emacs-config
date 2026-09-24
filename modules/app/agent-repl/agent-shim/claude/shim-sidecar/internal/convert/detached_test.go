package convert

// detached_test.go — the spool's rendered tail, the exit marker, and the LOST verdict's
// stability.

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func TestBashTailCarriesTheRenderedWindowAndItsOmittedCounts(t *testing.T) {
	// Arrange. The tail is a SNAPSHOT of what is drawn: the text verbatim and
	// the bytes and lines before it, never an offset a consumer must join on.
	cases := []struct {
		name         string
		text         string
		bytesOmitted uint64
		linesOmitted uint64
	}{
		{name: "the whole output so far", text: "compiling\n", bytesOmitted: 0, linesOmitted: 0},
		{name: "a window past the cap", text: "the latest line\n", bytesOmitted: 20480, linesOmitted: 312},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := newTestConverter(t)
			at := testAttribution(4096)
			at.TaskID = "b1"

			// Act.
			entry := c.BashTail(at, "toolu_run", tc.text, tc.bytesOmitted, tc.linesOmitted)

			// Assert.
			got := entry.GetAgentUpdate().GetBash().GetFrame().GetTail()
			if got == nil {
				t.Fatal("spool output must land on the bash tail arm")
			}
			if got.GetText() != tc.text || got.GetBytesOmitted() != tc.bytesOmitted || got.GetLinesOmitted() != tc.linesOmitted {
				t.Fatalf("tail = {%q, %d, %d}, want {%q, %d, %d}", got.GetText(), got.GetBytesOmitted(), got.GetLinesOmitted(),
					tc.text, tc.bytesOmitted, tc.linesOmitted)
			}
		})
	}
}

func TestARunsTailSupersedesItsOneRow(t *testing.T) {
	// Arrange. Output beyond what is rendered is not stored, so every batch
	// upserts the run's ONE tail row rather than adding a row per chunk.
	c := newTestConverter(t)
	first, second := testAttribution(3), testAttribution(7)
	first.TaskID, second.TaskID = "b1", "b1"

	// Act.
	earlier := c.BashTail(first, "toolu_run", "out", 0, 0)
	later := c.BashTail(second, "toolu_run", "outmore", 0, 0)

	// Assert.
	if earlier.GetUpsertKey() != later.GetUpsertKey() || later.GetUpsertKey() != BashTailKey("toolu_run") {
		t.Fatalf("tail keys = %q, %q, want both the run's single tail key %q",
			earlier.GetUpsertKey(), later.GetUpsertKey(), BashTailKey("toolu_run"))
	}
}

func TestATailThroughANewPositionIsANewWrite(t *testing.T) {
	// Arrange. The tail through a file position is identified by that
	// position: a later window must not be absorbed as a replay of an earlier.
	c := newTestConverter(t)
	first, second := testAttribution(3), testAttribution(7)
	first.TaskID, second.TaskID = "b1", "b1"

	// Act.
	earlier := c.BashTail(first, "toolu_run", "out", 0, 0)
	later := c.BashTail(second, "toolu_run", "outmore", 0, 0)

	// Assert.
	if earlier.GetWriteId() == later.GetWriteId() {
		t.Fatalf("two windows through different positions share write_id %q; the store would absorb the later", later.GetWriteId())
	}
}

func TestATailReReadThroughTheSamePositionIsTheSameWrite(t *testing.T) {
	// Arrange. A batch re-read after an unacknowledged write rebuilds the same
	// window through the same position, so it must mint the same identity and
	// be absorbed rather than written twice.
	c := newTestConverter(t)
	at := testAttribution(512)
	at.TaskID = "b1"

	// Act.
	first := c.BashTail(at, "toolu_run", "out", 0, 0)
	replayed := c.BashTail(at, "toolu_run", "out", 0, 0)

	// Assert.
	if first.GetWriteId() != replayed.GetWriteId() {
		t.Fatalf("a re-read tail minted write_ids %q and %q; the digest is of file coordinates and must be identical",
			first.GetWriteId(), replayed.GetWriteId())
	}
}

func TestATerminalAndTheTailDoNotCollide(t *testing.T) {
	// Arrange. A command that produced no output settles in the same batch as
	// its only tail, so the terminal's key must be its own.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	tailEntry := c.BashTail(at, "toolu_run", "EXIT=0\n", 0, 0)
	terminal := c.BashExited(at, "toolu_run", "EXIT=0\n", 0, 0)

	// Assert.
	if tailEntry.GetUpsertKey() == terminal.GetUpsertKey() {
		t.Fatalf("the terminal and the tail share the key %q", terminal.GetUpsertKey())
	}
	if got := terminal.GetUpsertKey(); got != BashTerminalKey("toolu_run") {
		t.Fatalf("terminal key = %q, want the run's single terminal key", got)
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
	first := c.BashLost(at, "toolu_run", "", 0, LostWentSilent, true, false)
	second := c.BashLost(at, "toolu_run", "", 0, LostWentSilent, true, false)

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
	lost := c.BashLost(at, "toolu_run", "out", 0, LostSweptUp, true, false)

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
			entry := c.BashLost(at, "toolu_run", "so far", 0, test.reason, true, false)

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
	entry := c.BashLost(at, "toolu_run", "so far", 0, LostWentSilent, true, false)

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
	entry := c.BashCancelled(at, "toolu_run", "partial work\n", 0, 1700000000000, true)

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

// ---- landing 5: a terminal whose producer holds NO BYTES says not_observed ----

func TestALostTerminalWithNoBytesObservedStatesNotObserved(t *testing.T) {
	// Arrange. THREE DIFFERENT FACTS, and only one of them is true here:
	// text{stdout:"", whole{}} claims the COMMAND printed nothing;
	// partial{bytes_omitted:0} claims nothing was CUT; not_observed claims the
	// producer does not know. A run swept up at boot supports only the third.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashLost(at, "toolu_run", "", 0, LostSweptUp, false, false)

	// Assert.
	output := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if output.GetNotObserved() == nil {
		t.Fatalf("output = %v, want the not_observed arm", output)
	}
}

func TestALostTerminalWithNoBytesNeverClaimsTheCommandPrintedNothing(t *testing.T) {
	// Arrange. The distinction is the whole point: an empty text arm is a
	// positive claim about the command, and the producer is not entitled to it.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashLost(at, "toolu_run", "", 0, LostSweptUp, false, false)

	// Assert.
	output := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if output.GetText() != nil {
		t.Fatalf("output states text %v; a producer holding no bytes must not claim the command printed nothing", output.GetText())
	}
}

func TestALostTerminalThatDidObserveOutputStillCarriesIt(t *testing.T) {
	// Arrange. not_observed is for the case the spool was never read; a run we
	// DID read still owes its bytes.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashLost(at, "toolu_run", "what it managed to say", 0, LostWentSilent, true, false)

	// Assert.
	output := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if got := output.GetText().GetStdout(); got != "what it managed to say" {
		t.Fatalf("stdout = %q, want the bytes the reader observed", got)
	}
}

func TestACancelledTerminalWithNoBytesObservedStatesNotObserved(t *testing.T) {
	// Arrange. A stop is evidence about a PERSON's decision, never about what
	// the command printed — so a stop for a run whose spool was never readable
	// says not_observed rather than inventing an empty output for it.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashCancelled(at, "toolu_run", "", 0, 1700000000000, false)

	// Assert.
	output := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if output.GetNotObserved() == nil {
		t.Fatalf("output = %v, want the not_observed arm", output)
	}
}

func TestAnObservedButGenuinelySilentRunStatesEmptyTextNotNotObserved(t *testing.T) {
	// Arrange. THE CONVERSE, and it matters as much: a command we READ that
	// printed nothing is a real fact about the command, and downgrading it to
	// not_observed would throw away something we do know.
	c := newTestConverter(t)
	at := testAttribution(0)

	// Act.
	entry := c.BashLost(at, "toolu_run", "", 0, LostWentSilent, true, false)

	// Assert.
	output := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess().GetInterrupted().GetOutput()
	if output.GetNotObserved() != nil {
		t.Fatal("a run we DID read that printed nothing must state empty text, not not_observed")
	}
	if output.GetText().GetWhole() == nil {
		t.Fatalf("output = %v, want text with the whole extent", output)
	}
}

func TestALostSubagentSettleAssertsNoError(t *testing.T) {
	// Arrange. We observed SILENCE, never an error. Filling in an AgentToolFailure
	// would have this producer assert the run died, which is exactly the claim
	// `lost` exists to avoid making.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "a15b5267244c1360e"

	// Act.
	entry := c.SubagentLost(at, "toolu_spawn", "owner-agent", LostWentSilent, false)

	// Assert.
	failure := entry.GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().
		GetUpdate().GetActivity().GetSubagent().GetFailure()
	if failure.GetError() != nil {
		t.Fatalf("a LOST subagent's settle carries an error %v; we observed only silence", failure.GetError())
	}
}

func TestALostSubagentVerdictIsStableSoAReEmissionIsANoOp(t *testing.T) {
	// Arrange. A subagent is lost ONCE however many sweeps observe it, so the
	// write identity must be stable and a re-emission absorbed rather than
	// appending a second settle.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "a15b5267244c1360e"

	// Act.
	first := c.SubagentLost(at, "toolu_spawn", "owner-agent", LostSweptUp, false)
	second := c.SubagentLost(at, "toolu_spawn", "owner-agent", LostSweptUp, false)

	// Assert.
	if first.GetWriteId() != second.GetWriteId() {
		t.Fatalf("write ids differ: %q vs %q", first.GetWriteId(), second.GetWriteId())
	}
}

func TestASpoolTerminalLeavesTheCommandUnsetRatherThanRestatingTheTaskID(t *testing.T) {
	// Arrange. AgentBashSuccess.command is a RESTATEMENT of what was run, and a
	// spool terminal is minted from bytes on disk plus a run handle — the line
	// is in neither, nor in shim-store's detached_work row, which holds the join
	// and nothing else. Filling the field with the vendor TASK id would have
	// this producer assert a command nobody ran, so every spool-minted terminal
	// leaves it unset and lets the origin unit's own call supply the true line.
	tests := []struct {
		name     string
		terminal func(*Converter, Attribution) *storev1.StoreEntry
	}{
		{
			name: "exited",
			terminal: func(c *Converter, at Attribution) *storev1.StoreEntry {
				return c.BashExited(at, "toolu_run", "out", 0, 0)
			},
		},
		{
			name: "lost",
			terminal: func(c *Converter, at Attribution) *storev1.StoreEntry {
				return c.BashLost(at, "toolu_run", "out", 0, LostWentSilent, true, false)
			},
		},
		{
			name: "cancelled",
			terminal: func(c *Converter, at Attribution) *storev1.StoreEntry {
				return c.BashCancelled(at, "toolu_run", "out", 0, 1700000000000, true)
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			c := newTestConverter(t)
			at := testAttribution(0)
			at.TaskID = "b1"

			// Act.
			entry := test.terminal(c, at)

			// Assert.
			success := entry.GetAgentUpdate().GetBash().GetFrame().GetSuccess()
			if command := success.GetCommand(); command != nil {
				t.Fatalf("command = %v; a spool terminal does not know the line and must leave it unset rather than restating the task id", command)
			}
		})
	}
}

// TestABacklogLostVerdictIsNotAWarning is the 161-warning finding: realtest 5's
// harvest was 475 records and 161 of them were `bash-lost`, every one a run that
// had already been stale for hours before that sidecar started. The policy one
// layer up already refuses to state those individually and rolls them into one
// informational summary per class; this record must follow the same
// classification, or the flood simply reappears under a different operation name.
func TestABacklogLostVerdictIsNotAWarning(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	c.BashLost(at, "toolu_run", "", 0, LostSweptUp, false, true)

	// Assert.
	if strings.Contains(sink.String(), `"level":"warn"`) {
		t.Fatalf("a startup catch-up conclusion was recorded as a warning: %s", sink.String())
	}
}

// TestALostTerminalTakesTheSameLevelAsThePolicyThatConcludedIt keeps the two
// layers in step. A conclusion reached while watching is a newly-arising
// condition and stays loud for a file that VANISHED under us; a run that merely
// went SILENT is not a fault this reader can attribute to anything — the file
// plane cannot tell a quiet dead run from a quiet live one — and warning here
// under an informational `lost-policy` would put the flood straight back one
// layer down.
func TestALostTerminalTakesTheSameLevelAsThePolicyThatConcludedIt(t *testing.T) {
	cases := []struct {
		name      string
		reason    LostReason
		wantLevel string
	}{
		{
			name:      "the file vanished under us, which is an anomaly to act on",
			reason:    LostFileVanished,
			wantLevel: "warn",
		},
		{
			name:      "the run went silent, which a healthy poll loop also does",
			reason:    LostWentSilent,
			wantLevel: "info",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			c, sink := loggedConverter(t)
			at := testAttribution(0)
			at.TaskID = "b1"

			// Act.
			c.BashLost(at, "toolu_run", "", 0, tc.reason, false, false)

			// Assert.
			if got := levelForMessage(t, sink, "the detached run is LOST"); got != tc.wantLevel {
				t.Fatalf("the terminal was recorded at %q, want %q", got, tc.wantLevel)
			}
		})
	}
}

// TestABacklogSubagentLostVerdictIsNotAWarning covers the other detached kind:
// a backgrounded subagent's spool is re-derived from disk on every restart
// exactly as a shell spool is, so its terminal owes the same classification.
func TestABacklogSubagentLostVerdictIsNotAWarning(t *testing.T) {
	// Arrange.
	c, sink := loggedConverter(t)
	at := testAttribution(0)
	at.TaskID = "a1"

	// Act.
	c.SubagentLost(at, "toolu_spawn", "owner-agent", LostSweptUp, true)

	// Assert.
	if strings.Contains(sink.String(), `"level":"warn"`) {
		t.Fatalf("a startup catch-up subagent conclusion was recorded as a warning: %s", sink.String())
	}
}
