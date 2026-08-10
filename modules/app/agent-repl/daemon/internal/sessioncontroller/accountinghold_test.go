package sessioncontroller

import (
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE INVARIANT: the enrichment release predicate reads a LEVEL, never an edge.
//
// A correction that arrives before its hold and one that arrives after it must
// be indistinguishable to the release. The production wedge was exactly the
// first ordering — the correction for msg_011CdswkkMZyUqE6amdCWSjJ landed 52ms
// ahead of the hold — and an edge-triggered release cannot see a transition
// that already happened.
// ---------------------------------------------------------------------------

// correctionUsage is one response's final vendor usage, distinguishable by its
// output count so a test can prove WHICH figure a hold released against.
func correctionUsage(outputTokens int64) *frontendv1.VendorTokenUsage {
	return &frontendv1.VendorTokenUsage{OutputTokens: outputTokens}
}

// releaseRecorder captures every enrichment release, so ordering assertions are
// made on observed calls rather than on a sleep.
type releaseRecorder struct {
	mu    sync.Mutex
	turns []string
	late  []bool
}

func (r *releaseRecorder) release(turnID string, late bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.turns = append(r.turns, turnID)
	r.late = append(r.late, late)
}

func (r *releaseRecorder) calls() ([]string, []bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.turns...), append([]bool(nil), r.late...)
}

// THE PRODUCTION INTERLEAVING, AT THE LEDGER. Each case files the same
// correction and installs the same hold, differing only in which came first.
// Both must release, or the ordering is load-bearing and the wedge is one
// scheduling accident away.
func TestAnEnrichmentHoldReleasesUnderEitherCorrectionOrdering(t *testing.T) {
	tests := []struct {
		name             string
		correctionFirst  bool
		wantReleaseCount int
	}{
		{name: "correction arrives before the hold is installed", correctionFirst: true, wantReleaseCount: 1},
		{name: "correction arrives after the hold is installed", correctionFirst: false, wantReleaseCount: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ledger := newAccountingCorrections(t.Logf)
			recorder := &releaseRecorder{}

			// Act.
			if tc.correctionFirst {
				ledger.Record("msg_1", correctionUsage(720))
				ledger.Install("turn_1", []string{"msg_1"}, recorder.release)
			} else {
				ledger.Install("turn_1", []string{"msg_1"}, recorder.release)
				ledger.Record("msg_1", correctionUsage(720))
			}

			// Assert.
			turns, _ := recorder.calls()
			if len(turns) != tc.wantReleaseCount {
				t.Fatalf("releases = %v, want %d — the arrival order of a correction may not decide whether its turn's stamp ever settles", turns, tc.wantReleaseCount)
			}
		})
	}
}

// A hold whose correction was already on file must release SYNCHRONOUSLY, on
// the install call itself. Releasing eventually is not the guarantee: nothing
// else is coming for that response, so a hold that returns still waiting waits
// forever.
func TestAHoldInstalledOverAFiledCorrectionReleasesOnTheInstallCall(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	recorder := &releaseRecorder{}
	ledger.Record("msg_1", correctionUsage(720))

	// Act.
	ledger.Install("turn_1", []string{"msg_1"}, recorder.release)

	// Assert.
	if turns, _ := recorder.calls(); len(turns) != 1 {
		t.Fatalf("releases during Install = %v, want the predicate satisfied at install time from the ledger", turns)
	}
}

// A turn naming NO responses has an empty predicate. It must release rather
// than wait, because there is no correction that could ever arrive for it.
func TestAHoldOverNoResponsesReleasesImmediately(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	recorder := &releaseRecorder{}

	// Act.
	ledger.Install("turn_empty", nil, recorder.release)

	// Assert.
	if turns, _ := recorder.calls(); len(turns) != 1 {
		t.Fatalf("releases = %v, want a turn with no responses to settle at once", turns)
	}
}

// A hold with one of two corrections on file stays outstanding. The predicate
// is every member, not any member.
func TestAHoldStillOwedOneCorrectionDoesNotRelease(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	recorder := &releaseRecorder{}
	ledger.Record("msg_1", correctionUsage(720))

	// Act.
	ledger.Install("turn_1", []string{"msg_1", "msg_2"}, recorder.release)

	// Assert.
	if turns, _ := recorder.calls(); len(turns) != 0 {
		t.Fatalf("releases = %v, want the hold to stay outstanding while one response is still uncorrected", turns)
	}
}

// The outstanding set is reported for the record, so a stamp that has not
// settled can say WHAT it is waiting on rather than merely that it waits.
func TestAnOutstandingHoldNamesTheResponsesItAwaits(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	ledger.Record("msg_1", correctionUsage(720))

	// Act.
	ledger.Install("turn_1", []string{"msg_1", "msg_2"}, func(string, bool) {})

	// Assert.
	awaiting := ledger.Awaiting("turn_1")
	if len(awaiting) != 1 || awaiting[0] != "msg_2" {
		t.Fatalf("awaiting = %v, want only the uncorrected response named", awaiting)
	}
}

// A CORRECTION ARRIVING AFTER THE RELEASE IS A LATE UPDATE, NOT A NO-OP. The
// hold stays registered precisely so the settled stamp can be revised.
func TestACorrectionAfterTheReleaseFiresTheHoldAgainAsLate(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	recorder := &releaseRecorder{}
	ledger.Install("turn_1", []string{"msg_1"}, recorder.release)
	ledger.Record("msg_1", correctionUsage(720))

	// Act — the vendor revises the same response.
	ledger.Record("msg_1", correctionUsage(9001))

	// Assert.
	turns, late := recorder.calls()
	if len(turns) != 2 {
		t.Fatalf("releases = %v, want the late correction to re-fire the hold", turns)
	}
	if late[0] || !late[1] {
		t.Fatalf("late flags = %v, want the second release reported as late and the first as not", late)
	}
}

// A VALUELESS DEPENDENCY HOLDS THE STAMP EXACTLY AS A CORRECTION DOES. The
// turn-end usage observation is read from the reducer rather than from this
// ledger, but it is owed on the same terms, so the hold must wait for it.
func TestAHoldStillOwedItsTurnEndObservationDoesNotRelease(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	recorder := &releaseRecorder{}
	ledger.Record("msg_1", correctionUsage(720))

	// Act — every response is corrected; the end boundary is not yet observed.
	ledger.Install("turn_1", []string{"msg_1", turnEndUsageKey("turn_1")}, recorder.release)

	// Assert.
	if turns, _ := recorder.calls(); len(turns) != 0 {
		t.Fatalf("releases = %v, want the stamp to wait for the end-boundary usage its reconciliation is measured against", turns)
	}
}

// The end-boundary observation releases the hold when it lands, and the same
// hold releases at install time when it landed first: the level is what is
// read, exactly as for a correction.
func TestAHoldReleasesUnderEitherTurnEndObservationOrdering(t *testing.T) {
	tests := []struct {
		name             string
		observationFirst bool
	}{
		{name: "the end boundary is observed before the hold is installed", observationFirst: true},
		{name: "the end boundary is observed after the hold is installed", observationFirst: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ledger := newAccountingCorrections(t.Logf)
			recorder := &releaseRecorder{}

			// Act.
			if tc.observationFirst {
				ledger.RecordFact(turnEndUsageKey("turn_1"))
				ledger.Install("turn_1", []string{turnEndUsageKey("turn_1")}, recorder.release)
			} else {
				ledger.Install("turn_1", []string{turnEndUsageKey("turn_1")}, recorder.release)
				ledger.RecordFact(turnEndUsageKey("turn_1"))
			}

			// Assert.
			if turns, _ := recorder.calls(); len(turns) != 1 {
				t.Fatalf("releases = %v, want the arrival order of the end boundary not to decide whether the stamp settles", turns)
			}
		})
	}
}

// The ledger hands out COPIES. A reducer patching a response record in place
// must not be able to reach back into the filed correction.
func TestAFiledCorrectionIsHandedOutAsACopy(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	ledger.Record("msg_1", correctionUsage(720))

	// Act.
	first := ledger.Correction("msg_1")
	first.OutputTokens = 1

	// Assert.
	if second := ledger.Correction("msg_1"); second.GetOutputTokens() != 720 {
		t.Fatalf("filed correction = %d, want the ledger's own entry untouched by a caller's mutation", second.GetOutputTokens())
	}
}

// A response with no correction on file is reported as such rather than as a
// zero-filled one, which would read as a free response.
func TestAnUnfiledCorrectionIsReportedAbsent(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)

	// Act.
	got := ledger.Correction("msg_never_filed")

	// Assert.
	if got != nil {
		t.Fatalf("correction = %+v, want nil for a response the vendor never corrected", got)
	}
}

// ApplyTo is how a late correction reaches a turn whose reducer entry is gone:
// the durable record is the only remaining statement of its responses.
func TestApplyToPatchesASettledRecordsResponseUsage(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	ledger.Record("msg_1", correctionUsage(720))
	settled := &frontendv1.TurnAccounting{TurnId: "turn_1", Responses: []*frontendv1.TokenUtilization{
		{ApiMessageId: "msg_1", Usage: correctionUsage(563)},
	}}

	// Act.
	changed := ledger.ApplyTo(settled)

	// Assert.
	if !changed || settled.GetResponses()[0].GetUsage().GetOutputTokens() != 720 {
		t.Fatalf("changed = %v output_tokens = %d, want the settled record carrying the vendor's final figure", changed, settled.GetResponses()[0].GetUsage().GetOutputTokens())
	}
}

// A record already carrying the filed figure is NOT reported as changed, so a
// late release cannot churn the store re-persisting an identical row.
func TestApplyToReportsNoChangeWhenTheRecordAlreadyAgrees(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	ledger.Record("msg_1", correctionUsage(720))
	settled := &frontendv1.TurnAccounting{TurnId: "turn_1", Responses: []*frontendv1.TokenUtilization{
		{ApiMessageId: "msg_1", Usage: correctionUsage(720)},
	}}

	// Act.
	changed := ledger.ApplyTo(settled)

	// Assert.
	if changed {
		t.Fatal("ApplyTo reported a change over a record that already carries the filed correction")
	}
}

// THE MEMORY BOUND IS REAL AND IT IS LOUD. The ledger must outlive each turn's
// settlement, so it cannot be cleared per turn; what it can do is retire its
// oldest holds, and a turn past that window is one whose stamp can no longer be
// revised.
func TestTheLedgerRetiresItsOldestHoldPastTheRetentionWindow(t *testing.T) {
	// Arrange.
	ledger := newAccountingCorrections(t.Logf)
	ledger.Install("turn_oldest", []string{"msg_oldest"}, func(string, bool) {})

	// Act — install one more hold than the window admits.
	for i := 0; i < enrichmentRetention; i++ {
		ledger.Install(newerTurnID(i), []string{newerMessageID(i)}, func(string, bool) {})
	}

	// Assert — the oldest hold's correction can no longer be filed against it.
	if awaiting := ledger.Awaiting("turn_oldest"); awaiting != nil {
		t.Fatalf("awaiting for the retired turn = %v, want the hold retired past the window", awaiting)
	}
}

func newerTurnID(i int) string    { return "turn_newer_" + string(rune('a'+i)) }
func newerMessageID(i int) string { return "msg_newer_" + string(rune('a'+i)) }
