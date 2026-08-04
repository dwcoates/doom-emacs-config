package sessioncontroller

import (
	"strings"
	"testing"

	"claude-repld/internal/shim"
)

// ---------------------------------------------------------------------------
// THE CLOSED VOCABULARY, proved at the record.
//
// The defect these pin is not "a stop happened": it is that every stop the
// daemon issued reached the shim under ONE coarse attribution, so an idle
// sweep, a merged teardown and a drain execution were indistinguishable in the
// evidence trail they left behind. Each case below drives one cause through the
// one funnel and asserts on what the stop func was actually handed.
// ---------------------------------------------------------------------------

// TestEachStopCauseReachesTheShimAsItsOwnAttribution — one row per cause in the
// vocabulary, each asserting the funnel token and the shim.Stop that cause
// renders. Adding a cause without a row here leaves the table incomplete, which
// the completeness test below catches.
func TestEachStopCauseReachesTheShimAsItsOwnAttribution(t *testing.T) {
	cases := []struct {
		name          string
		cause         StopCause
		wantPath      string
		wantInitiator string
	}{
		{"idle sweep", StopCauseHibernateIdleSweep(), "hibernate", "idle_sweep"},
		{"merged teardown", StopCauseMergedTeardown(), "hibernate", "merged_teardown"},
		{"hard restart live", StopCauseHardRestartLive(), "hibernate", "hard_restart"},
		{"hard restart orphan", StopCauseHardRestartOrphan(), "restart_session_orphan", "hard_restart"},
		{"drain execution", StopCauseDrainExecution(), "hibernate", "scheduled_shutdown"},
		{"daemon shutdown", StopCauseDaemonShutdown(), "hibernate", "daemon_shutdown"},
		{"session deleted", StopCauseSessionDeleted(), "hibernate_session", "session_delete"},
		{"session superseded", StopCauseSessionSuperseded(), "hibernate_session", "session_supersede"},
		{"account switch", StopCauseAccountSwitch(), "hibernate", "account_switch"},
		{"bring-up failed", StopCauseBringUpFailed(), "bringup_failed", "bringup_failure"},
		{"controller exit", StopCauseControllerExit(), "session_controller_exit", "session_controller_exit"},
		{"superseded record", StopCauseSessionDeleted().supersededRecord(), "hibernate_session_superseded", "session_stop_superseded_record"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			m, spawner, applier, _ := newTurnStopRig(t)
			applier.staleTurnClosed = true

			// Act.
			if err := m.stopShimSettlingTurn("ws", "s1", tc.cause, true); err != nil {
				t.Fatalf("stopShimSettlingTurn: %v", err)
			}

			// Assert — the record the shim was handed names this cause and only
			// this cause.
			got := spawner.stopAttributions()
			if len(got) != 1 {
				t.Fatalf("stop attributions = %+v, want exactly one", got)
			}
			if got[0].Initiator != tc.wantInitiator {
				t.Fatalf("stop initiator = %q, want %q", got[0].Initiator, tc.wantInitiator)
			}
			if got[0].Reason == "" {
				t.Fatalf("stop reason is empty for %s; an unattributed half is refused by shim.Stop.Validate", tc.name)
			}
			// And the funnel token the SSM's stale-turn close records is this
			// cause's token, unchanged from the string the call site used before
			// the vocabulary existed.
			closes := applier.staleTurnClosesApplied()
			if len(closes) != 1 || closes[0].reason != tc.wantPath {
				t.Fatalf("stale-turn closes = %+v, want the single path token %q", closes, tc.wantPath)
			}
		})
	}
}

// EVERY CAUSE IS RENDERED. A constructor added without a table row would hand
// the shim a zero attribution, which the funnel refuses — so the omission would
// surface as a production stop that never happens. This catches it here.
func TestEveryStopCauseConstructorIsRendered(t *testing.T) {
	// Arrange.
	causes := []StopCause{
		StopCauseHibernateIdleSweep(), StopCauseHibernateForced(), StopCauseHibernateCacheExpired(),
		StopCauseMergedTeardown(), StopCauseHardRestartLive(),
		StopCauseHardRestartOrphan(), StopCauseDrainExecution(), StopCauseDaemonShutdown(),
		StopCauseSessionDeleted(), StopCauseSessionSuperseded(), StopCauseAccountSwitch(),
		StopCauseBringUpFailed(), StopCauseControllerExit(),
	}

	// Act / Assert.
	if len(causes)+1 != len(stopCauseTable) {
		t.Fatalf("%d exported constructors against %d table rows; the table carries exactly the exported causes plus the internal superseded-record refinement",
			len(causes), len(stopCauseTable))
	}
	for _, cause := range causes {
		if !cause.valid() {
			t.Fatalf("cause %v is not in the table", cause)
		}
		if err := cause.stop().Validate(); err != nil {
			t.Fatalf("cause %v renders an invalid stop: %v", cause, err)
		}
	}
}

// A MERGED TEARDOWN AND AN IDLE SWEEP ARE THE SAME TEARDOWN AND NOT THE SAME
// EVENT. They shared one attribution before the vocabulary existed, which is
// precisely what made the log unable to explain a reclaimed workspace.
func TestMergedTeardownIsDistinguishableFromAnIdleSweepAtTheRecord(t *testing.T) {
	// Arrange.
	m, spawner, applier, _ := newTurnStopRig(t)
	applier.staleTurnClosed = true

	// Act.
	if err := m.stopShimSettlingTurn("ws", "s1", StopCauseMergedTeardown(), true); err != nil {
		t.Fatalf("merged teardown stop: %v", err)
	}
	if err := m.stopShimSettlingTurn("ws", "s1", StopCauseHibernateIdleSweep(), true); err != nil {
		t.Fatalf("idle sweep stop: %v", err)
	}

	// Assert.
	got := spawner.stopAttributions()
	if len(got) != 2 {
		t.Fatalf("stop attributions = %+v, want two", got)
	}
	if got[0] == got[1] {
		t.Fatalf("both stops carried the identical attribution %+v; the merge and the sweep are not the same event", got[0])
	}
}

// A DRAIN EXECUTION IS NOT AN ORDINARY SHUTDOWN either: a deploy is waiting on
// the first and nobody is waiting on the second.
func TestDrainExecutionIsDistinguishableFromADaemonShutdownAtTheRecord(t *testing.T) {
	// Arrange.
	m, spawner, applier, _ := newTurnStopRig(t)
	applier.staleTurnClosed = true

	// Act.
	if err := m.stopShimSettlingTurn("ws", "s1", StopCauseDrainExecution(), true); err != nil {
		t.Fatalf("drain execution stop: %v", err)
	}
	if err := m.stopShimSettlingTurn("ws", "s1", StopCauseDaemonShutdown(), true); err != nil {
		t.Fatalf("daemon shutdown stop: %v", err)
	}

	// Assert.
	got := spawner.stopAttributions()
	if len(got) != 2 {
		t.Fatalf("stop attributions = %+v, want two", got)
	}
	if got[0] == got[1] {
		t.Fatalf("both stops carried the identical attribution %+v; a scheduled bounce and a plain shutdown are not the same event", got[0])
	}
}

// THE REFINED RECORD STILL NAMES WHO ASKED. A stop that finds a replacement
// session driving the workspace reports both the request and the finding.
func TestASupersededRecordStopRetainsTheRequestingCause(t *testing.T) {
	// Arrange.
	cause := StopCauseSessionDeleted().supersededRecord()

	// Act.
	got := cause.stop()

	// Assert.
	if !strings.Contains(got.Reason, "session_delete") {
		t.Fatalf("refined reason %q does not name the requesting cause", got.Reason)
	}
}

// THE ZERO CAUSE IS REFUSED, and the refusal reaches the caller: no stop is
// issued, and the axis is still closed because a refused stop leaves the turn
// exactly as unreportable as a failed one.
func TestTheFunnelRefusesAnUnmintedCause(t *testing.T) {
	// Arrange.
	m, spawner, applier, cl := newTurnStopRig(t)
	applier.staleTurnClosed = true

	// Act.
	err := m.stopShimSettlingTurn("ws", "s1", StopCause{}, true)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "unattributed shim stop") {
		t.Fatalf("err = %v, want the unattributed-stop refusal", err)
	}
	if got := spawner.stoppedSessions(); len(got) != 0 {
		t.Fatalf("a refused stop still reached the spawner: %v", got)
	}
	if len(applier.staleTurnClosesApplied()) != 1 {
		t.Fatalf("closes = %+v, want exactly one — a refused stop must not skip the close", applier.staleTurnClosesApplied())
	}
	if !cl.contains("SHIM STOP REFUSED") {
		t.Fatalf("missing the canonical refusal record; log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// THE SEALED SPAWNER IS THE COMPILE-TIME SHAPE OF THE FUNNEL. What the Manager
// retains cannot stop a shim, so a teardown that reaches for the spawner's stop
// half instead of the funnel fails loudly rather than stopping a shim without
// closing its turn.
func TestTheRetainedSpawnerRefusesOffFunnelStops(t *testing.T) {
	// Arrange.
	m, spawner, _, _ := newTurnStopRig(t)

	// Act.
	err := m.cfg.Spawner.StopShim("s1", 0, shim.Stop{Initiator: "rogue", Reason: "bypassing the funnel"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "off-funnel shim stop") {
		t.Fatalf("err = %v, want the off-funnel refusal", err)
	}
	if got := spawner.stoppedSessions(); len(got) != 0 {
		t.Fatalf("an off-funnel stop reached the real spawner: %v", got)
	}
}
