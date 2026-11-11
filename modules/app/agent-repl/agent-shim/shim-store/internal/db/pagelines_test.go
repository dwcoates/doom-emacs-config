package db

import (
	"context"
	"database/sql"
	"errors"
	"strings"
	"testing"
)

// inTx runs FN in a write transaction on D, rolled back afterwards.
func inTx(t *testing.T, d *DB, fn func(*sql.Tx) error) error {
	t.Helper()
	tx, err := d.sql.BeginTx(ctx(), nil)
	if err != nil {
		t.Fatalf("begin: %v", err)
	}
	defer tx.Rollback() //nolint:errcheck // the test reads, and every write is thrown away
	return fn(tx)
}

func TestStoredPageLinesDecodesEveryBookedPageLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, questionLine("q1", "agent-main", true), deliveredLine("u1", "session-b", "m-1"))

	// Act
	var lines []storedPageLine
	err := inTx(t, d, func(tx *sql.Tx) error {
		var err error
		lines, err = storedPageLines(context.Background(), tx, "the test's rows")
		return err
	})

	// Assert
	if err != nil || len(lines) != 2 {
		t.Fatalf("storedPageLines = (%d lines, %v), want both stored lines", len(lines), err)
	}
	if lines[0].book != "agent-main" || lines[0].entry.GetAgentUpdate().GetServeableFrame() == nil {
		t.Fatalf("first line = %+v, want agent-main's decoded page line", lines[0])
	}
}

func TestStoredPageLinesAnswersNothingForAnEmptyStore(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	var lines []storedPageLine
	err := inTx(t, d, func(tx *sql.Tx) error {
		var err error
		lines, err = storedPageLines(context.Background(), tx, "the test's rows")
		return err
	})

	// Assert
	if err != nil || len(lines) != 0 {
		t.Fatalf("storedPageLines = (%d lines, %v), want none", len(lines), err)
	}
}

// TestEveryFrameDecodingBackfillReadsThroughStoredPageLines pins that each
// backfill whose bookkeeping lives inside the stored frames fails a frame it
// cannot decode exactly as storedPageLines does, which is the read they share.
func TestEveryFrameDecodingBackfillReadsThroughStoredPageLines(t *testing.T) {
	tests := []struct {
		name     string
		backfill func(context.Context, *sql.Tx) error
	}{
		{"peer_delivery", backfillPeerDeliveries},
		{"ask_line", backfillAskLines},
		{"turn_prompt", backfillTurnPrompts},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: one undecodable page line.
			d, _ := newStore(t)
			writeOK(t, d, questionLine("q1", "agent-main", true))
			if _, err := d.sql.ExecContext(ctx(), `UPDATE entry SET frame = x'ff' WHERE kind = ?`, kindPageLine); err != nil {
				t.Fatalf("damaging the frames: %v", err)
			}

			// Act
			err := inTx(t, d, func(tx *sql.Tx) error { return tt.backfill(context.Background(), tx) })

			// Assert
			if !errors.Is(err, ErrStorage) || !strings.Contains(err.Error(), "stored frame at position") {
				t.Fatalf("%s backfill = %v, want the shared read's decode failure", tt.name, err)
			}
		})
	}
}
