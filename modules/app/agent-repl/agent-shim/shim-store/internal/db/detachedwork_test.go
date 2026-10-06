package db

import (
	"errors"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// ---- the detached work a unit left ----

func TestDetachedWorkByUnitAnswersAnEndedBashRunWrittenByItsFrames(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:r:start", "r", bashStart()))
	writeOK(t, d, bashEntry("w-end", "bash:r:terminal", "r", terminalBash()))

	// Act
	work, found, err := d.DetachedWorkByUnit(ctx(), "r")

	// Assert
	if err != nil || !found {
		t.Fatalf("DetachedWorkByUnit = (%v, %t, %v), want a found row", work, found, err)
	}
	if work.GetKind().GetBash() == nil || work.GetEnded().GetEndedAtMs() != testNow {
		t.Fatalf("work = %v, want a bash run ended at %d", work, testNow)
	}
}

func TestDetachedWorkByUnitAnswersALiveBashRunAsLive(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:r:start", "r", bashStart()))

	// Act
	work, found, err := d.DetachedWorkByUnit(ctx(), "r")

	// Assert
	if err != nil || !found || work.GetLive() == nil {
		t.Fatalf("DetachedWorkByUnit = (%v, %t, %v), want a live row", work, found, err)
	}
}

func TestDetachedWorkByUnitServesEachRecordedKindAsItsArm(t *testing.T) {
	tests := []struct {
		name string
		kind string
		arm  func(*storev1.GetDetachedWorkKind) bool
	}{
		{name: "subagent", kind: "subagent", arm: func(k *storev1.GetDetachedWorkKind) bool { return k.GetSubagent() != nil }},
		{name: "bash", kind: "bash", arm: func(k *storev1.GetDetachedWorkKind) bool { return k.GetBash() != nil }},
		{name: "workflow", kind: "workflow", arm: func(k *storev1.GetDetachedWorkKind) bool { return k.GetWorkflow() != nil }},
		{name: "monitor", kind: "monitor", arm: func(k *storev1.GetDetachedWorkKind) bool { return k.GetMonitor() != nil }},
		{name: "the detached marker is unstated", kind: "detached", arm: func(k *storev1.GetDetachedWorkKind) bool { return k.GetUnstated() != nil }},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)
			if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, announced_at_ms) VALUES ('w', ?, 'u', 1)`, test.kind); err != nil {
				t.Fatalf("seed: %v", err)
			}

			// Act
			work, found, err := d.DetachedWorkByUnit(ctx(), "u")

			// Assert
			if err != nil || !found || !test.arm(work.GetKind()) {
				t.Fatalf("DetachedWorkByUnit = (%v, %t, %v), want the %s arm", work, found, err, test.name)
			}
		})
	}
}

func TestDetachedWorkByUnitAnswersNotFoundForAUnitNoRowLocates(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	work, found, err := d.DetachedWorkByUnit(ctx(), "never-seen")

	// Assert
	if err != nil || found || work != nil {
		t.Fatalf("DetachedWorkByUnit = (%v, %t, %v), want not found with no error", work, found, err)
	}
	s.assertLogged(t, "info", "no detached work on record left unit never-seen")
}

func TestDetachedWorkByUnitRefusesTwoRowsLocatedByOneUnit(t *testing.T) {
	// Arrange: two rows sharing one origin unit, which resolveDetachedRowKey
	// never mints, written straight to the table.
	d, s := newStore(t)
	if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, announced_at_ms) VALUES
	  ('w1', 'bash', 'u', 1), ('w2', 'bash', 'u', 1)`); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, found, err := d.DetachedWorkByUnit(ctx(), "u")

	// Assert
	if !errors.Is(err, ErrStorage) || !errors.Is(err, errAmbiguousDetachedUnit) || found {
		t.Fatalf("err = %v found = %t, want an ambiguous-unit storage failure", err, found)
	}
	s.assertLogged(t, "error", "unit u locates 2 detached work rows")
}

func TestDetachedWorkByUnitRefusesAKindTheStoreNeverWrites(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, announced_at_ms) VALUES ('w', 'teleport', 'u', 1)`); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, found, err := d.DetachedWorkByUnit(ctx(), "u")

	// Assert
	if !errors.Is(err, ErrStorage) || !errors.Is(err, errUnknownDetachedKind) || found {
		t.Fatalf("err = %v found = %t, want an unknown-kind storage failure", err, found)
	}
	s.assertLogged(t, "error", `records kind "teleport"`)
}

func TestDetachedWorkByUnitRefusesAnEmptyUnit(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, _, err := d.DetachedWorkByUnit(ctx(), "")

	// Assert
	if !errors.Is(err, ErrInvalid) || RefusalField(err) != "unit" {
		t.Fatalf("err = %v (field %q), want ErrInvalid naming unit", err, RefusalField(err))
	}
}

func TestTheDetachedWorkLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, detachedWorkByUnitSQL, "u")

	// Assert
	assertNoAutomaticIndex(t, "the detached work lookup", plan)
}

func TestDetachedWorkByUnitReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, _, err := d.DetachedWorkByUnit(ctx(), "u")

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "reading the detached work of unit u")
}
