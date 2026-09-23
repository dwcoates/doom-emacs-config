package db

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// The handle and the run identity are DELIBERATELY DIFFERENT STRINGS in every
// subject below. They are different types in the protocol
// (DetachedWorkId vs AgentActivityId), the producers are entitled to mint them
// independently, and the old join only worked because the fixtures happened to
// use the same value for both.
const (
	bashHandle = "work-handle-1"
	bashRunID  = "run-unit-1"
)

// detachedBashRun is the announcement: a bash run that DETACHED from the
// in-turn unit `bashRunID`, addressed by the handle `bashHandle`.
func detachedBashRun(handle, originUnit string) *conversationv1.AgentDetachedWork {
	return detachedWork(handle, originUnit, &conversationv1.DetachedWorkDetached_Requested{
		Requested: &conversationv1.DetachedCauseRequested{},
	})
}

func TestABashRunFrameJoinsTheRowItsAnnouncementAlreadyOpened(t *testing.T) {
	// Arrange: the announcement first (the ordinary stream-before-file order).
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Act: the run's own frame, which knows only the run identity.
	writeOK(t, d, bashEntry("w2", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Assert: ONE row, not one per identity.
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work`); got != 1 {
		t.Fatalf("detached_work rows = %d, want 1 — the run has one row, not one per identity", got)
	}
	if got := scalar[string](t, d, `SELECT work_id FROM detached_work`); got != bashHandle {
		t.Fatalf("work_id = %q, want the announcement's handle %q to keep the row", got, bashHandle)
	}
}

func TestAnAnnouncementJoinsTheRowItsRunFrameAlreadyOpened(t *testing.T) {
	// Arrange: the FILE plane observed the spool before the stream plane
	// announced it, which is the reverse order and just as legal.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Act
	writeOK(t, d, pageEntry("w2", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work`); got != 1 {
		t.Fatalf("detached_work rows = %d, want 1 — a later announcement upserts, never duplicates", got)
	}
	if got := scalar[string](t, d, `SELECT work_id FROM detached_work`); got != bashRunID {
		t.Fatalf("work_id = %q, want the run's own row %q to be joined", got, bashRunID)
	}
}

func TestABashRunFrameCreatesItsOwnRowWhenNothingAnnouncedIt(t *testing.T) {
	// Arrange: nothing has announced the run at all.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, bashEntry("w1", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Assert
	if got := scalar[string](t, d, `SELECT work_id FROM detached_work`); got != bashRunID {
		t.Fatalf("work_id = %q, want the run's own identity %q", got, bashRunID)
	}
	if got := scalar[string](t, d, `SELECT origin_unit FROM detached_work`); got != bashRunID {
		t.Fatalf("origin_unit = %q, want the run to be its own origin unit", got)
	}
}

func TestABashTerminalEndsTheRowTheAnnouncementOpened(t *testing.T) {
	// Arrange: the terminal arrives on the run's identity but must close the
	// row the announcement's handle keys.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))
	writeOK(t, d, bashEntry("w2", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Act
	writeOK(t, d, bashEntry("w3", "bash:"+bashRunID+":terminal", bashRunID, bashSuccess()))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE work_id = ?`, bashHandle); got != testNow {
		t.Fatalf("ended_at_ms = %d, want %d", got, testNow)
	}
}

func TestAFailingBashRunEndsItsRowToo(t *testing.T) {
	// Arrange: a call that could not be performed is still a terminal, so the
	// obligation must not stay open on it.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Act
	writeOK(t, d, bashEntry("w2", "bash:"+bashRunID+":terminal", bashRunID, bashFailure()))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE ended_at_ms IS NULL`); got != 0 {
		t.Fatalf("open detached rows = %d, want 0 after a failure terminal", got)
	}
}

func TestTheOriginUnitsTerminalClosesTheDetachedRow(t *testing.T) {
	// Arrange: the run detached from an in-turn unit, and that unit reaching a
	// terminal on the SPAWNING stream is what ends the run's row — one indexed
	// lookup, never a lineage walk.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Act: the origin unit concludes in the announcer's own book.
	writeOK(t, d, pageEntry("w2", "u-origin-terminal", "agent-1",
		frameItem(activityFrame("agent-1", bashRunID, terminalBash()))))

	// Assert
	if got := scalar[int64](t, d, `SELECT ended_at_ms FROM detached_work WHERE work_id = ?`, bashHandle); got != testNow {
		t.Fatalf("ended_at_ms = %d, want the origin unit's terminal to close the row", got)
	}
}

func TestANonTerminalOriginFrameLeavesTheDetachedRowOpen(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Act
	writeOK(t, d, pageEntry("w2", "u-origin-progress", "agent-1",
		frameItem(activityFrame("agent-1", bashRunID, bashStart()))))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE ended_at_ms IS NULL`); got != 1 {
		t.Fatalf("open detached rows = %d, want the run still live", got)
	}
}

func TestADetachedOriginArmRecordsTheUnitItLeft(t *testing.T) {
	// Arrange: only the `created` arm carries a DetachableWork, so a `detached`
	// announcement states no kind — the store refuses to guess one and records
	// the unit it left, which is the join that matters.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Assert
	if got := scalar[string](t, d, `SELECT origin_unit FROM detached_work WHERE work_id = ?`, bashHandle); got != bashRunID {
		t.Fatalf("origin_unit = %q, want %q", got, bashRunID)
	}
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = ?`, bashHandle); got != detachedKindDetached {
		t.Fatalf("kind = %q, want %q — the announcement did not say what kind of work left", got, detachedKindDetached)
	}
}

func TestARunFrameNeverDowngradesTheKindAnAnnouncementSet(t *testing.T) {
	// Arrange: a `detached` announcement records the unspecific marker; the
	// run's own frames know it is a shell run.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", detachedBashRun(bashHandle, bashRunID)))))

	// Act
	writeOK(t, d, bashEntry("w2", "bash:"+bashRunID+":start", bashRunID, bashStart()))

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = ?`, bashHandle); got != detachedKindBash {
		t.Fatalf("kind = %q, want %q", got, detachedKindBash)
	}
}

func TestABashTerminalNeverRelabelsASubagentsRow(t *testing.T) {
	// Arrange: a detached subagent, announced as such under its spawn handle.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:work-1", "agent-1",
		frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2"))))))

	// Act: a producer closes the same handle with a SHELL terminal.
	writeOK(t, d, bashEntry("w2", "bash:work-1:terminal", "work-1", bashSuccess()))

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM detached_work WHERE work_id = 'work-1'`); got != detachedKindSubagent {
		t.Fatalf("kind = %q, want the announced %q to stand", got, detachedKindSubagent)
	}
}

func TestABashTerminalOnASubagentsRowStillClosesIt(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:work-1", "agent-1",
		frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2"))))))

	// Act
	writeOK(t, d, bashEntry("w2", "bash:work-1:terminal", "work-1", bashSuccess()))

	// Assert: the obligation the write closes is closed.
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM detached_work WHERE ended_at_ms IS NULL`); got != 0 {
		t.Fatalf("open detached rows = %d, want 0", got)
	}
}

func TestAKindConflictIsRecordedAtError(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:work-1", "agent-1",
		frameItem(detachedFrame("agent-1", createdWork("work-1", subagentWork("agent-2"))))))

	// Act
	writeOK(t, d, bashEntry("w2", "bash:work-1:terminal", "work-1", bashSuccess()))

	// Assert
	s.assertLogged(t, "error", "the recorded kind stands")
}

func TestAMatchingKindWritesNoConflict(t *testing.T) {
	// Arrange: an announced shell run.
	d, s := newStore(t)
	writeOK(t, d, pageEntry("w1", "detached:"+bashHandle, "agent-1",
		frameItem(detachedFrame("agent-1", createdWork(bashHandle, bashWork())))))

	// Act: its own terminal, under the same handle.
	writeOK(t, d, bashEntry("w2", "bash:"+bashHandle+":terminal", bashHandle, bashSuccess()))

	// Assert
	if errs := recordsAtLevel(t, s, "error"); len(errs) != 0 {
		t.Fatalf("error records = %v, want none", errs)
	}
}
