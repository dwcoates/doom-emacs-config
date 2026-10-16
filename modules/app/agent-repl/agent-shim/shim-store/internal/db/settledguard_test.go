package db

import (
	"errors"
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

// ---- a concluded unit is never reopened by a late write of it ----

const settledKey = "activity:unit-1"

// unitWrite is one write of the unit under settledKey: settled, with
// MARKDOWN as its content, or merely started when MARKDOWN is empty.
func unitWrite(writeID, markdown string) *storev1.StoreEntry {
	item := prose()
	if markdown != "" {
		item = proseSaying(markdown)
	}
	return pageEntry(writeID, settledKey, "agent-1", frameItem(activityFrame("agent-1", "unit-1", item)))
}

// storedMarkdown is the stored unit's content, or "" while it is not settled.
func storedMarkdown(t *testing.T, d *DB) string {
	t.Helper()
	activity := storedEntry(t, d, settledKey).GetAgentUpdate().GetServeableFrame().GetAgentItem().GetAgentFrame().GetUpdate().GetActivity()
	return activity.GetResponse().GetSuccess().GetProse().GetMarkdown()
}

func TestASettledUnitIsNeverReopenedByALateWrite(t *testing.T) {
	tests := []struct {
		name   string
		writes []*storev1.StoreEntry
		want   string
	}{
		{"the file plane's conclusion stands over the stream's late start",
			[]*storev1.StoreEntry{onFilePlane(unitWrite("file-1", "loaded")), unitWrite("stream-1", "")}, "loaded"},
		{"a start before the conclusion is superseded by it",
			[]*storev1.StoreEntry{unitWrite("stream-1", ""), onFilePlane(unitWrite("file-1", "loaded"))}, "loaded"},
		{"a later conclusion supersedes an earlier one",
			[]*storev1.StoreEntry{unitWrite("stream-1", "first"), unitWrite("stream-2", "second")}, "second"},
		{"a later start supersedes an earlier start",
			[]*storev1.StoreEntry{unitWrite("stream-1", ""), unitWrite("stream-2", "")}, ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			for _, write := range tc.writes {
				writeOK(t, d, write)
			}

			// Assert
			if got := storedMarkdown(t, d); got != tc.want {
				t.Fatalf("stored unit content = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestALateStartStillStampsTheSettledRowWithItsTurn(t *testing.T) {
	// Arrange: the sidecar's conclusion carries no turn; the shim's start does.
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(unitWrite("file-1", "loaded")))

	// Act
	writeOK(t, d, stampedTurn(unitWrite("stream-1", ""), "turn-7"))

	// Assert
	stored := storedEntry(t, d, settledKey)
	if stored.GetTurn().GetValue() != "turn-7" || storedMarkdown(t, d) != "loaded" {
		t.Fatalf("stored row = turn %q content %q, want turn-7 and the conclusion", stored.GetTurn().GetValue(), storedMarkdown(t, d))
	}
}

func TestALateStartReDeliversTheSettledRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(unitWrite("file-1", "loaded")))
	before := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = ?`, settledKey)

	// Act
	writeOK(t, d, stampedTurn(unitWrite("stream-1", ""), "turn-7"))

	// Assert: a standing watch sees the stamp land, with the conclusion.
	if after := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = ?`, settledKey); after <= before {
		t.Fatalf("write_seq = %d after %d, want the row written again", after, before)
	}
}

func TestASettledRowThatCannotBeDecodedRefusesTheLateWrite(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(unitWrite("file-1", "loaded")))
	if _, err := d.sql.ExecContext(ctx(), `UPDATE entry SET frame = x'ff' WHERE upsert_key = ?`, settledKey); err != nil {
		t.Fatalf("corrupt the stored frame: %v", err)
	}

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(unitWrite("stream-1", "")), nil)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("WriteBatch error = %v, want a storage failure", err)
	}
}
