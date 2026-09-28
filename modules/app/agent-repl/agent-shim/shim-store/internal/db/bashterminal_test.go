package db

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// ---- a bash run's terminal: the file plane outranks the stream plane ----

func onFilePlane(entry *storev1.StoreEntry) *storev1.StoreEntry {
	entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	entry.ConversionVersion = fileVersion()
	return entry
}

// fileBashTerminal is the sidecar's terminal: it carries the spool's output.
func fileBashTerminal(writeID, output string) *storev1.StoreEntry {
	return onFilePlane(bashEntry(writeID, "bash:r:terminal", "r", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: output},
		}},
	}))
}

// streamBashTerminal is the shim's terminal: it says only that the run ended.
func streamBashTerminal(writeID string) *storev1.StoreEntry {
	return bashEntry(writeID, "bash:r:terminal", "r", bashFailure())
}

func TestWriteBatchBashTerminalPlanePrecedence(t *testing.T) {
	tests := []struct {
		name        string
		writes      []*storev1.StoreEntry
		wantWriteID string
	}{
		{
			name:        "file then stream keeps the file terminal",
			writes:      []*storev1.StoreEntry{fileBashTerminal("f1", "exit 3"), streamBashTerminal("s1")},
			wantWriteID: "f1",
		},
		{
			name:        "stream then file lets the file terminal win",
			writes:      []*storev1.StoreEntry{streamBashTerminal("s1"), fileBashTerminal("f1", "exit 3")},
			wantWriteID: "f1",
		},
		{
			name:        "file then file lets the latest file terminal win",
			writes:      []*storev1.StoreEntry{fileBashTerminal("f1", "exit 3"), fileBashTerminal("f2", "exit 4")},
			wantWriteID: "f2",
		},
		{
			name:        "stream then stream lets the latest stream terminal win",
			writes:      []*storev1.StoreEntry{streamBashTerminal("s1"), streamBashTerminal("s2")},
			wantWriteID: "s2",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			for _, w := range tt.writes {
				writeOK(t, d, w)
			}

			// Assert
			if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'bash:r:terminal'`); got != tt.wantWriteID {
				t.Fatalf("stored write_id = %q, want %q", got, tt.wantWriteID)
			}
		})
	}
}

func TestWriteBatchAbsorbsAStreamBashTerminalOverAFileTerminal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, fileBashTerminal("f1", "exit 3"))

	// Act
	result := writeOK(t, d, streamBashTerminal("s1"))

	// Assert
	if result.Absorbed != 1 || result.Written != 0 {
		t.Fatalf("absorbed=%d written=%d, want 1 and 0", result.Absorbed, result.Written)
	}
}

func TestWriteBatchLedgerRecordsNoRefusedStreamBashTerminal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, fileBashTerminal("f1", "exit 3"))

	// Act
	writeOK(t, d, streamBashTerminal("s1"))

	// Assert: the key's one ledger row is the stored row's own write.
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry e JOIN write_ledger l ON l.write_id = e.write_id AND l.write_seq = e.write_seq WHERE e.upsert_key = 'bash:r:terminal'`); got != 1 {
		t.Fatalf("entry rows matching their ledger row = %d, want 1", got)
	}
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM write_ledger WHERE write_id = 's1'`); got != 0 {
		t.Fatalf("ledger rows for the refused write = %d, want 0", got)
	}
}

func TestWriteBatchPublishesNoWatcherRowForARefusedStreamBashTerminal(t *testing.T) {
	// Arrange: a watcher sees the file terminal once and never the stream one.
	d, _ := newStore(t)
	first := writeOK(t, d, fileBashTerminal("f1", "exit 3"))

	// Act
	second := writeOK(t, d, streamBashTerminal("s1"))

	// Assert
	if len(first.BashRows) != 1 || first.BashRows[0].Row.GetFrame().GetSuccess().GetCommand().GetLine() != "exit 3" {
		t.Fatalf("file terminal watcher rows = %v, want the one file terminal", first.BashRows)
	}
	if len(second.BashRows) != 0 {
		t.Fatalf("refused stream terminal watcher rows = %d, want 0", len(second.BashRows))
	}
}

func TestWriteBatchRecordsARefusedStreamBashTerminalOnceAtInfo(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	writeOK(t, d, fileBashTerminal("f1", "exit 3"))

	// Act
	writeOK(t, d, streamBashTerminal("s1"))

	// Assert
	count := 0
	for _, record := range s.records(t) {
		if strings.Contains(record["message"].(string), "stream-plane bash terminal not applied") {
			count++
			if record["level"] != "info" {
				t.Fatalf("level = %v, want info", record["level"])
			}
		}
	}
	if count != 1 {
		t.Fatalf("records = %d, want 1", count)
	}
}

func TestWriteBatchLetsAStreamBashNonTerminalSupersedeAFileRow(t *testing.T) {
	// Arrange: only a terminal is guarded; a tail supersedes as before.
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(bashEntry("f1", "bash:r:tail", "r", bashTail("a"))))

	// Act
	writeOK(t, d, bashEntry("s1", "bash:r:tail", "r", bashTail("b")))

	// Assert
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'bash:r:tail'`); got != "s1" {
		t.Fatalf("stored write_id = %q, want s1", got)
	}
}

func TestWriteBatchLetsAStreamNonBashRowSupersedeAFileRow(t *testing.T) {
	// Arrange: the rule is confined to bash rows.
	d, _ := newStore(t)
	writeOK(t, d, onFilePlane(workflowRunEntry("f1", "wf", "run")))

	// Act
	writeOK(t, d, workflowRunEntry("s1", "wf", "run"))

	// Assert
	if got := scalar[string](t, d, `SELECT write_id FROM entry WHERE upsert_key = 'wf'`); got != "s1" {
		t.Fatalf("stored write_id = %q, want s1", got)
	}
}
