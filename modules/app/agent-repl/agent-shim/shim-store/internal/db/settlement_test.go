package db

import (
	"errors"
	"fmt"
	"testing"
)

// ---- which runs the record holds as ended ----

func TestRunSettlementsAnswersAnEndedRunWithItsEndInstant(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:r:start", "r", bashStart()))
	writeOK(t, d, bashEntry("w-end", "bash:r:terminal", "r", terminalBash()))

	// Act
	settled, err := d.RunSettlements(ctx(), []string{"r"})

	// Assert
	if err != nil {
		t.Fatalf("RunSettlements: %v", err)
	}
	if len(settled) != 1 || settled[0].RunID != "r" || settled[0].EndedAtMs != testNow {
		t.Fatalf("settled = %+v, want r ended at %d", settled, testNow)
	}
}

func TestRunSettlementsLeavesOutALiveRun(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:r:start", "r", bashStart()))

	// Act
	settled, err := d.RunSettlements(ctx(), []string{"r"})

	// Assert
	if err != nil || len(settled) != 0 {
		t.Fatalf("settled = %+v err = %v, want nothing: a live row is not settled", settled, err)
	}
}

func TestRunSettlementsLeavesOutARunWithNoRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	settled, err := d.RunSettlements(ctx(), []string{"never-seen"})

	// Assert
	if err != nil || len(settled) != 0 {
		t.Fatalf("settled = %+v err = %v, want nothing: no row is not settled", settled, err)
	}
}

func TestRunSettlementsLeavesOutARunOneOfWhoseRowsIsLive(t *testing.T) {
	// Arrange: two rows sharing one origin unit, which resolveDetachedRowKey
	// never mints, written straight to the table.
	d, _ := newStore(t)
	if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, announced_at_ms, ended_at_ms) VALUES
	  ('w1', 'bash', 'r', 1, 5), ('w2', 'bash', 'r', 1, NULL)`); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	settled, err := d.RunSettlements(ctx(), []string{"r"})

	// Assert
	if err != nil || len(settled) != 0 {
		t.Fatalf("settled = %+v err = %v, want nothing while any row of the run is live", settled, err)
	}
}

func TestRunSettlementsAnswersOnlyTheAskedRuns(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	for _, run := range []string{"a", "b"} {
		writeOK(t, d, bashEntry("w-end-"+run, "bash:"+run+":terminal", run, terminalBash()))
	}

	// Act
	settled, err := d.RunSettlements(ctx(), []string{"b", "c"})

	// Assert
	if err != nil || len(settled) != 1 || settled[0].RunID != "b" {
		t.Fatalf("settled = %+v err = %v, want only b", settled, err)
	}
}

func TestRunSettlementsRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name  string
		ids   []string
		field string
	}{
		{name: "no ids", ids: nil, field: "run_ids"},
		{name: "an empty id", ids: []string{"r", ""}, field: "run_ids[1]"},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, err := d.RunSettlements(ctx(), test.ids)

			// Assert
			if !errors.Is(err, ErrInvalid) || RefusalField(err) != test.field {
				t.Fatalf("err = %v (field %q), want ErrInvalid naming %s", err, RefusalField(err), test.field)
			}
		})
	}
}

func TestTheSettlementLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, fmt.Sprintf(runSettlementsSQL, "?"), "r")

	// Assert
	assertNoAutomaticIndex(t, "the settlement lookup", plan)
}

func TestRunSettlementsReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.RunSettlements(ctx(), []string{"r"})

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "reading the settlements of 1 run(s)")
}
