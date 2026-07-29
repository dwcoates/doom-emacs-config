package ssm

import (
	"path/filepath"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestApplyRepairsAnOrphanEndOnce(t *testing.T) {
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))

	mustApply(t, m, evTaskEnded("s1", 2, "orphan", corev1.TerminalStatus_TERMINAL_STATUS_LOST))
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 0 {
		t.Fatalf("live_task_count after orphan end = %d, want 0", got)
	}
	if got := cl.count("repaired orphan task_ended"); got != 1 {
		t.Fatalf("orphan repair log count = %d, want 1", got)
	}

	mustApply(t, m, evTaskEnded("s1", 3, "orphan", corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	if got := cl.count("repaired orphan task_ended"); got != 1 {
		t.Fatalf("duplicate terminal edge repeated orphan repair log: count=%d, want 1", got)
	}
}

func TestOpenRepairsPersistedOrphanEndsOnce(t *testing.T) {
	path := filepath.Join(t.TempDir(), "state.db")
	db, err := openDB(path)
	if err != nil {
		t.Fatalf("open seed db: %v", err)
	}
	seedSignal(t, db, "ws1", "s1", sigIdle, causeSessionStarted, 1, 1)
	seedTaskSignal(t, db, "ws1", "s1", sigTaskEnded, causeTaskEnded, 2, 2, "orphan-a")
	seedTaskSignal(t, db, "ws1", "s1", sigTaskEnded, causeTaskEnded, 3, 3, "orphan-b")
	if err := db.Close(); err != nil {
		t.Fatalf("close seed db: %v", err)
	}

	firstLog := &capLog{}
	m1, err := Open(Options{DBPath: path, Logf: firstLog.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("first Open: %v", err)
	}
	if got := mustCurrent(t, m1, "ws1").GetLiveTaskCount(); got != 0 {
		t.Fatalf("live_task_count after persisted repair = %d, want 0", got)
	}
	if got := firstLog.count("repaired persisted orphan task ends ws=ws1 count=2"); got != 1 {
		t.Fatalf("persisted repair log count = %d, want 1; lines=%v", got, firstLog.lines)
	}
	if err := m1.Close(); err != nil {
		t.Fatalf("close first manager: %v", err)
	}

	secondLog := &capLog{}
	m2, err := Open(Options{DBPath: path, Logf: secondLog.logf, Resolver: fakeResolver{"s1": "ws1"}})
	if err != nil {
		t.Fatalf("second Open: %v", err)
	}
	defer m2.Close()
	if got := secondLog.count("repaired persisted orphan task ends"); got != 0 {
		t.Fatalf("second Open repeated persisted repair: count=%d lines=%v", got, secondLog.lines)
	}

	var reconciled int
	if err := m2.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE workspace = ? AND state = ? AND cause_kind = ?`,
		"ws1", sigTaskStarted, causeTaskReconciled).Scan(&reconciled); err != nil {
		t.Fatalf("count reconciled starts: %v", err)
	}
	if reconciled != 2 {
		t.Fatalf("reconciled task_started rows = %d, want 2", reconciled)
	}
}

func TestApplyRejectsTaskEndWithoutIdentity(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	err := m.Apply(evTaskEnded("s1", 1, "", corev1.TerminalStatus_TERMINAL_STATUS_LOST))
	if err == nil {
		t.Fatal("TaskEnded without task_id must fail")
	}
	var rows int
	if scanErr := m.db.QueryRow(
		`SELECT COUNT(*) FROM workspace_state WHERE state NOT IN ('wired','starting','dormant')`).Scan(&rows); scanErr != nil {
		t.Fatalf("count rows: %v", scanErr)
	}
	if rows != 0 {
		t.Fatalf("failed TaskEnded appended %d row(s), want 0", rows)
	}
}
