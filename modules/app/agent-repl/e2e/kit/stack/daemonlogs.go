package stack

import (
	"agent-repld/integration/harness"
	"fmt"
	"path/filepath"
	"strings"
	"testing"
)

// The daemon's own idle-sweep log operation and the two message strings this
// area asserts on (daemon/internal/drain/sweep.go's opSweep = "daemon.drain.
// sweep"; the exact wording is independently confirmed by daemon/integration/
// session_lifecycle_test.go's own header comment on
// TestHibernateTurnInFlightRefusalDefersTheStandDown, which quotes "a turn is
// in flight; deferring the hibernation" verbatim). This is the daemon's OWN
// structured log — unlike harness.AwaitShimLoggedRequest (which reads the
// FAKE shim's control-socket recorder and does not exist for the real shim
// this suite spawns), it is produced regardless of which shim is real.
const (
	HibernateSweepOp = "daemon.drain.sweep"
	// The SWEEP's own freeness pre-check, which runs BEFORE any Hibernate
	// directive is sent (daemon/internal/drain/sweep.go:64-66). A session
	// with live work never reaches the shim at all, so this — not the
	// shim's own refusal — is the record a turn-in-flight deferral leaves.
	hibernateNotFreeMsg   = "the idle session is not free; deferring its park"
	HibernateSucceededMsg = "parked a session idle past the cutoff"
)

// sweepMark is a position in a workspace's own daemon sink: the number of
// records it already held when the mark was taken. A wait carrying a mark
// reads only what the daemon wrote AFTER it, so a test can never be satisfied
// by a record that predates the act it is waiting on.
//
// THIS AREA NEEDS ONE, and the need is a measurement rather than a caution.
// The daemon spawns a session ON MOUNT (daemon.md's SPAWN ON MOUNT), and a
// mount-spawned session that is never prompted goes idle at
// hibernationIdleCutoffMS like any other — so on a quiet box the sweep stands
// that session down BEFORE the test has submitted anything. Observed, from
// testRevivalAfterHibernate's own sink on an idle host: "hibernated an idle
// session" at T+0.368s, "the session was engaged inside the cutoff" (the
// prompt queue's engagement stamp, promptqueue/deliver.go's touchEngagement)
// at T+0.522s. An unmarked wait for that message therefore matched a
// stand-down of a session the test's first turn had not yet run on, and only
// a SATURATED box — where the first prompt lands inside the 50ms cutoff, so
// no such record exists — made the test wait for the hibernation it actually
// names. Waiting on the wrong fact is what made the pass load-dependent.
type SweepMark int

// sweepRecordSince answers the first hibernateSweepOp record with the given
// message that the sink holds AT OR AFTER mark. It is a pure function so the
// marking rule itself is tested rather than only exercised.
func SweepRecordSince(records []harness.LogRecord, mark SweepMark, message string) (harness.LogRecord, bool) {
	if int(mark) < 0 {
		mark = 0
	}
	for i := int(mark); i < len(records); i++ {
		if records[i].Operation == HibernateSweepOp && records[i].Message == message {
			return records[i], true
		}
	}
	return harness.LogRecord{}, false
}

// conclusionBudgetSpent is what the shim logs, at ERROR, when its teardown
// waited a tail's whole conclusion budget (`WATCHER_CONCLUSION_BUDGET_MS`, 1s)
// instead of ending it (agent-shim/claude/shim/src/engine/session.ts).
//
// THE STALL IS ASSERTED BY ITS OWN RECORD, NOT BY THE STOP'S WALL TIME. The
// shim's teardown concludes every open `WatchAgent` tail through its book's
// head and waits for the tail to serve it. A tail that can never reach that
// head — the store streams an upsert of an old row at its ORIGINAL pointer, so
// the newest pointer a tail has served walks BACKWARD — spends the whole
// budget, and the daemon's stop waits inside it. The shim states exactly that
// when it happens; a wall-clock bound on the stop stood in for the record and
// failed on a saturated host whose healthy stop simply ran slow (292ms against
// a 250ms bound, load average 54, 2026-10-13).
const conclusionBudgetSpent = "did not end within its conclusion budget"

// conclusionBudgetRecords names every record in LOGSDIR's logs stating that a
// teardown waited out a conclusion budget.
func ConclusionBudgetRecords(t *testing.T, logsDir string) []string {
	t.Helper()
	pattern := filepath.Join(logsDir, "*.log")
	paths, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob %s: %v", pattern, err)
	}
	if len(paths) == 0 {
		t.Fatalf("no log matches %s, so this assertion could pass reading nothing", pattern)
	}
	var found []string
	for _, path := range paths {
		for _, r := range harness.ReadLog(t, path) {
			if strings.Contains(r.Message, conclusionBudgetSpent) {
				found = append(found, fmt.Sprintf("%s: %s %s: %s", filepath.Base(path), r.Level, r.Operation, r.Message))
			}
		}
	}
	return found
}
