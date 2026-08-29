package convert

// detached_test.go — spool deltas, the exit marker, and the LOST verdict's
// stability.

import "testing"

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

func TestBashFramesUpsertTheRunsOneRow(t *testing.T) {
	// Arrange. A delta and the terminal are frames of ONE unit, not two rows.
	c := newTestConverter(t)
	at := testAttribution(0)
	at.TaskID = "b1"

	// Act.
	delta := c.BashDelta(at, "toolu_run", "out", 0)
	terminal := c.BashExited(at, "toolu_run", "out", 0)

	// Assert.
	if delta.GetUpsertKey() != terminal.GetUpsertKey() {
		t.Fatalf("keys differ: %q vs %q; the terminal must upsert the run's own row",
			delta.GetUpsertKey(), terminal.GetUpsertKey())
	}
	if delta.GetWriteId() == terminal.GetWriteId() {
		t.Fatal("two frames of one unit must have DIFFERENT write ids, or the store absorbs one as a replay of the other")
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
	first := c.BashLost(at, "toolu_run", "", LostWentSilent)
	second := c.BashLost(at, "toolu_run", "", LostWentSilent)

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
	exited := c.BashExited(at, "toolu_run", "out", 0)
	lost := c.BashLost(at, "toolu_run", "out", LostSweptUp)

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
	entry := c.BashExited(at, "toolu_run", "", 0)

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
