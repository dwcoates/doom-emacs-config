package db

import (
	"errors"
	"testing"
)

// seedDetached writes detached_work rows straight to the table.
func seedDetached(t *testing.T, d *DB, sql string) {
	t.Helper()
	if _, err := d.sql.Exec(`INSERT INTO detached_work (work_id, kind, origin_unit, owner_agent, announced_at_ms, ended_at_ms) VALUES ` + sql); err != nil {
		t.Fatalf("seed: %v", err)
	}
}

func TestLiveShellRunsAnnouncedBeforeAnswersOnlyLiveShellsFromBefore(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedDetached(t, d, `
	  ('old-live',  'bash',     'r-old',   'agent-a', 100, NULL),
	  ('older',     'bash',     'r-older', 'agent-b', 50,  NULL),
	  ('old-ended', 'bash',     'r-ended', 'agent-a', 100, 150),
	  ('new-live',  'bash',     'r-new',   'agent-a', 900, NULL),
	  ('at-edge',   'bash',     'r-edge',  'agent-a', 500, NULL),
	  ('subagent',  'subagent', 'r-sub',   'agent-a', 100, NULL),
	  ('no-owner',  'bash',     'r-none',  NULL,      100, NULL),
	  ('no-unit',   'bash',     NULL,      'agent-a', 100, NULL)`)

	// Act
	runs, err := d.LiveShellRunsAnnouncedBefore(ctx(), 500)

	// Assert
	if err != nil {
		t.Fatalf("LiveShellRunsAnnouncedBefore: %v", err)
	}
	want := []LiveShellRun{
		{WorkID: "older", Run: "r-older", Owner: "agent-b", AnnouncedAtMs: 50},
		{WorkID: "old-live", Run: "r-old", Owner: "agent-a", AnnouncedAtMs: 100},
	}
	if len(runs) != len(want) {
		t.Fatalf("runs = %+v, want %+v", runs, want)
	}
	for i := range want {
		if runs[i] != want[i] {
			t.Fatalf("runs[%d] = %+v, want %+v", i, runs[i], want[i])
		}
	}
}

func TestLiveShellRunsAnnouncedBeforeRefusesANonPositiveInstant(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.LiveShellRunsAnnouncedBefore(ctx(), 0)

	// Assert
	if !errors.Is(err, ErrInvalid) || RefusalField(err) != "announced_before_ms" {
		t.Fatalf("err = %v (field %q), want ErrInvalid naming announced_before_ms", err, RefusalField(err))
	}
}

func TestTheLiveShellLookupBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, liveShellRunsAnnouncedBeforeSQL, detachedKindBash, 1)

	// Assert
	assertNoAutomaticIndex(t, "the live shell lookup", plan)
}

func TestLiveShellRunsAnnouncedBeforeReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.LiveShellRunsAnnouncedBefore(ctx(), 1)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "reading the live shell runs announced before 1")
}
