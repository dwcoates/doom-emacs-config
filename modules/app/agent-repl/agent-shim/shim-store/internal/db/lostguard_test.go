package db

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- a LOST terminal never lands on a run whose ending is on record ----

func lostArm() *conversationv1.DetachedLost {
	return &conversationv1.DetachedLost{How: &conversationv1.DetachedLost_WentSilent{WentSilent: &conversationv1.DetachedLostWentSilent{}}}
}

// bashLost is the sidecar's LOST terminal for a shell run.
func bashLost() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
		Outcome: &conversationv1.AgentBashSuccess_Interrupted{Interrupted: &conversationv1.AgentBashInterrupted{
			Cause: &conversationv1.AgentBashInterrupted_Lost{Lost: lostArm()},
		}},
	}}}
}

// fileBashLost is bashLost on the file plane, keyed as the run's terminal.
func fileBashLost(writeID, run string) *storev1.StoreEntry {
	return onFilePlane(bashEntry(writeID, "bash:"+run+":terminal", run, bashLost()))
}

// fileSubagentLost is the sidecar's LOST terminal for a backgrounded subagent:
// its spawn activity, keyed by the run, settles failed with cause lost.
func fileSubagentLost(writeID, run string) *storev1.StoreEntry {
	failure := &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Failure{Failure: &conversationv1.AgentSubagentFailure{
		Cause: &conversationv1.AgentSubagentFailure_Lost{Lost: lostArm()},
	}}}
	return onFilePlane(pageEntry(writeID, "activity:"+run, "main", frameItem(activityFrame("main", run, failure))))
}

// endedRun seeds a run's detached row as ended at endedAtMs, the way a real
// terminal leaves it.
func endedRun(t *testing.T, d *DB, run string, endedAtMs int64) {
	t.Helper()
	if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, announced_at_ms, ended_at_ms, terminal)
	  VALUES (?, 'bash', ?, 1, ?, x'01')`, run, run, endedAtMs); err != nil {
		t.Fatalf("seed ended run: %v", err)
	}
}

func TestWriteBatchRefusesAFileBashLostOverAnEndedRun(t *testing.T) {
	// Arrange: the run ended on its own spool's terminator.
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(bashEntry("w-exit", "bash:r:terminal", "r", bashSuccess())))
	endedAt := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE origin_unit = 'r'`)

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(fileBashLost("w-lost", "r")), nil)

	// Assert
	if !errors.Is(err, ErrInvalid) || RefusalSite(err) != SiteLostOverSettled || RefusalField(err) != "entries[0]" {
		t.Fatalf("err = %v (site %q field %q), want ErrInvalid at %s naming entries[0]", err, RefusalSite(err), RefusalField(err), SiteLostOverSettled)
	}
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'bash:r:terminal'`); got != "w-exit" {
		t.Fatalf("stored terminal = %q, want the original w-exit", got)
	}
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE origin_unit = 'r'`); got != endedAt {
		t.Fatalf("ended_at_ms = %d, want the original %d", got, endedAt)
	}
}

func TestWriteBatchRefusesAFileSubagentLostOverAnEndedRun(t *testing.T) {
	// Arrange: the subagent's run ended (its notification closed the row).
	d, _ := newStore(t)
	endedRun(t, d, "toolu_sub", 5)

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(fileSubagentLost("w-lost", "toolu_sub")), nil)

	// Assert
	if !errors.Is(err, ErrInvalid) || RefusalSite(err) != SiteLostOverSettled {
		t.Fatalf("err = %v (site %q), want ErrInvalid at %s", err, RefusalSite(err), SiteLostOverSettled)
	}
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE origin_unit = 'toolu_sub'`); got != 5 {
		t.Fatalf("ended_at_ms = %d, want the original 5", got)
	}
}

func TestWriteBatchAppliesAFileBashLostOverALiveRun(t *testing.T) {
	// Arrange: the run started and never ended; LOST is its honest conclusion.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:r:start", "r", bashStart()))

	// Act
	writeOK(t, d, fileBashLost("w-lost", "r"))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE origin_unit = 'r'`); got != testNow {
		t.Fatalf("ended_at_ms = %d, want the LOST's %d", got, testNow)
	}
}

func TestWriteBatchLeavesAStreamPlaneLostToThePlanePrecedence(t *testing.T) {
	// Arrange: the guard is the file plane's; a stream-plane terminal over a
	// stream-plane ending supersedes as it always has.
	d, _ := newStore(t)
	writeOK(t, d, streamBashTerminal("s1"))

	// Act
	writeOK(t, d, bashEntry("s2", "bash:r:terminal", "r", bashLost()))

	// Assert
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'bash:r:terminal'`); got != "s2" {
		t.Fatalf("stored terminal = %q, want s2", got)
	}
}

func TestWriteBatchStillLetsAFileTerminalThatIsNotLostSupersede(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, fileBashTerminal("f1", "exit 3"))

	// Act
	writeOK(t, d, fileBashTerminal("f2", "exit 4"))

	// Assert
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'bash:r:terminal'`); got != "f2" {
		t.Fatalf("stored terminal = %q, want f2", got)
	}
}

func TestLostTerminalRunNamesOnlyAFilePlaneLostTerminal(t *testing.T) {
	tests := []struct {
		name    string
		entry   *storev1.StoreEntry
		wantRun string
		wantOK  bool
	}{
		{name: "file bash lost", entry: fileBashLost("w", "r"), wantRun: "r", wantOK: true},
		{name: "file subagent lost", entry: fileSubagentLost("w", "toolu_sub"), wantRun: "toolu_sub", wantOK: true},
		{name: "stream bash lost", entry: bashEntry("w", "bash:r:terminal", "r", bashLost()), wantOK: false},
		{name: "file bash exit", entry: onFilePlane(bashEntry("w", "bash:r:terminal", "r", bashSuccess())), wantOK: false},
		{name: "file prose line", entry: onFilePlane(pageEntry("w", "activity:x", "main", frameItem(activityFrame("main", "x", prose())))), wantOK: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			r, err := classify(test.entry, 0)
			if err != nil {
				t.Fatalf("classify: %v", err)
			}

			// Act
			run, ok := lostTerminalRun(r)

			// Assert
			if ok != test.wantOK || run != test.wantRun {
				t.Fatalf("lostTerminalRun = (%q, %t), want (%q, %t)", run, ok, test.wantRun, test.wantOK)
			}
		})
	}
}

func TestRefuseLostOverSettledReportsAStorageFailure(t *testing.T) {
	// Arrange: a transaction already ended fails the lookup.
	d, _ := newStore(t)
	r, err := classify(fileBashLost("w", "r"), 0)
	if err != nil {
		t.Fatalf("classify: %v", err)
	}
	tx, err := d.sql.Begin()
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	if err := tx.Rollback(); err != nil {
		t.Fatalf("rollback: %v", err)
	}

	// Act
	err = d.refuseLostOverSettled(ctx(), tx, r)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("err = %v, want ErrStorage", err)
	}
}

func TestTheLostGuardLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, lostOverSettledSQL, "r")

	// Assert
	assertNoAutomaticIndex(t, "the LOST guard lookup", plan)
}
