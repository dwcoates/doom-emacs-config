// detached_test.go — SUBJECT: the detached run's ONE row, and the two identities
// that address it.
//
// The announcement knows a DetachedWorkId; the run's own frames know an
// AgentActivityId. They are different types in the protocol and the producers
// mint them independently, so keying each writer by the identity it happens to
// hold opened TWO rows for one run — which GetLiveWork then reported as two open
// obligations, leaving the shim to resolve a run that did not exist. Every
// subject here uses DELIBERATELY DIFFERENT strings for the two.
package integration

import (
	"agentrepl/shim-store/internal/testclose"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

const (
	itestBashHandle = "work-handle-itest"
	itestBashRunID  = "run-unit-itest"
)

// detachedRunFrame announces that a run DETACHED from the in-turn unit
// originActID, addressed by the handle workID. It is the `detached` origin arm:
// the announcement states no kind, because the consumer already has the unit.
func detachedRunFrame(owner, workID, originActID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: agentID(owner),
		Result: &conversationv1.AgentFrame_DetachedWork{
			DetachedWork: &conversationv1.AgentDetachedWork{
				Work: detachedWorkID(workID),
				Origin: &conversationv1.AgentDetachedWork_Detached{
					Detached: &conversationv1.DetachedWorkDetached{
						DetachedFromId: activityID(originActID),
						Cause: &conversationv1.DetachedWorkDetached_Requested{
							Requested: &conversationv1.DetachedCauseRequested{},
						},
					},
				},
			},
		},
	}
}

// bashFailureFrame is a run that could not be performed at all — still a
// terminal, so the obligation must not stay open on it.
func bashFailureFrame() *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Failure{Failure: &conversationv1.AgentBashFailure{}},
	}
}

func TestARunAnnouncedThenObservedIsOneOpenObligation(t *testing.T) {
	// Arrange: the stream plane announces, then the file plane observes the
	// spool — the ordinary order.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-d-1", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-d-2", "bash:"+itestBashRunID+":start",
		bashRun(agentID("main"), itestBashRunID, bashStart("sleep 60", 1000))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 1 {
		t.Fatalf("live_detached = %v, want exactly one entry for one run", got)
	}
	store.assertNoErrorRecords()
}

func TestARunObservedBeforeItIsAnnouncedIsStillOneOpenObligation(t *testing.T) {
	// Arrange: the file plane can reach the spool before the stream plane
	// announces the detachment, and that order is just as legal.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	sidecar.write(ctx, t, sidecar.agentEntry("w-d-1", "bash:"+itestBashRunID+":start",
		bashRun(agentID("main"), itestBashRunID, bashStart("sleep 60", 1000))))

	// Act
	shim.write(ctx, t, shim.agentEntry("w-d-2", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 1 {
		t.Fatalf("live_detached = %v, want exactly one entry for one run", got)
	}
	store.assertNoErrorRecords()
}

func TestARunsTerminalClosesTheObligationItsAnnouncementOpened(t *testing.T) {
	// Arrange: the terminal arrives on the RUN's identity and must close the
	// row the HANDLE keys.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-d-1", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-d-2", "bash:"+itestBashRunID+":start",
		bashRun(agentID("main"), itestBashRunID, bashStart("sleep 60", 1000))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-d-3", "bash:"+itestBashRunID+":terminal",
		bashRun(agentID("main"), itestBashRunID, bashSuccess("sleep 60", 0))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 0 {
		t.Fatalf("live_detached = %v, want empty after the run concluded", got)
	}
	store.assertNoErrorRecords()
}

func TestAFailedRunClosesItsObligationToo(t *testing.T) {
	// Arrange: a call that could not be performed is still a terminal.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	sidecar := fileProducer(cli)
	sidecar.write(ctx, t, sidecar.agentEntry("w-f-1", "bash:"+itestBashRunID+":start",
		bashRun(agentID("main"), itestBashRunID, bashStart("make test", 1000))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-f-2", "bash:"+itestBashRunID+":terminal",
		bashRun(agentID("main"), itestBashRunID, bashFailureFrame())))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 0 {
		t.Fatalf("live_detached = %v, want empty after a failure terminal", got)
	}
	store.assertNoErrorRecords()
}

func TestTheOriginUnitsTerminalOnTheSpawningStreamClosesTheRun(t *testing.T) {
	// Arrange: the run detached from an in-turn unit. That unit concluding in
	// the announcer's own book is what ends the run — one indexed lookup on
	// origin_unit, never a lineage walk.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-o-1", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))

	// Act: the origin unit reaches a terminal arm on the spawning stream.
	shim.write(ctx, t, shim.agentEntry("w-o-2", "u-origin-terminal",
		frameLine(agentID("main"), bashActivityFrame("main", itestBashRunID, bashSuccess("sleep 60", 0)))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 0 {
		t.Fatalf("live_detached = %v, want the origin unit's terminal to have closed the run", got)
	}
	store.assertNoErrorRecords()
}

func TestADetachedRunFrameIsServedAsAPageLineOfTheAnnouncersBook(t *testing.T) {
	// Arrange: the `detached` origin arm states no kind, and is still the
	// handoff the book's reader must see.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act
	shim.write(ctx, t, shim.agentEntry("w-d-arm", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))

	// Assert
	page := openSession(ctx, t, cli, "main", nil)
	assertTexts(t, "the announcer's book", pageTexts(page.GetPage()), []string{"detached:" + itestBashHandle})
	store.assertNoErrorRecords()
}

// TestACoincidentHandleAndUnitIdIsOneObligation is the convergence edge of the
// two identities: nothing stops a producer from minting a DetachedWorkId and an
// AgentActivityId with the SAME opaque value, and when it does the run must
// still be ONE obligation rather than two rows that happen to look alike.
func TestACoincidentHandleAndUnitIdIsOneObligation(t *testing.T) {
	// Arrange: the handle and the unit id are deliberately the same string.
	const coincident = "work-and-run-coincide"
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-c-1", "detached:"+coincident,
		frameLine(agentID("main"), detachedRunFrame("main", coincident, coincident))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-c-2", "bash:"+coincident+":start",
		bashRun(agentID("main"), coincident, bashStart("sleep 60", 1000))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 1 {
		t.Fatalf("live_detached = %v, want exactly one entry for one run", got)
	}
	store.assertNoErrorRecords()
}

// TestACoincidentHandleAndUnitIdIsClosedByOneTerminal is the other half: one
// terminal on the shared string closes the one obligation, leaving nothing
// open that a reconciler would have to resolve against a run that never was.
func TestACoincidentHandleAndUnitIdIsClosedByOneTerminal(t *testing.T) {
	// Arrange
	const coincident = "work-and-run-coincide"
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	sidecar := fileProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-c-1", "detached:"+coincident,
		frameLine(agentID("main"), detachedRunFrame("main", coincident, coincident))))
	sidecar.write(ctx, t, sidecar.agentEntry("w-c-2", "bash:"+coincident+":start",
		bashRun(agentID("main"), coincident, bashStart("sleep 60", 1000))))

	// Act
	sidecar.write(ctx, t, sidecar.agentEntry("w-c-3", "bash:"+coincident+":terminal",
		bashRun(agentID("main"), coincident, bashSuccess("sleep 60", 0))))

	// Assert
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); len(got) != 0 {
		t.Fatalf("live_detached = %v, want empty after the one run concluded", got)
	}
	store.assertNoErrorRecords()
}

// TestAnAnnouncedRunWithNoRowsIsARefusedWatchButAnOpenObligation is the edge
// the shim's reconciliation has to survive.
//
// The announcement and the run's rows come from DIFFERENT producers, so there
// is a real window in which the obligation exists and the run has no row at
// all. GetLiveWork answers from the lifecycle table and lists it; WatchBashRun
// answers from the entry spine and has nothing to serve, so it refuses the open
// with CodeNotFound rather than standing open on a run it cannot replay.
func TestAnAnnouncedRunWithNoRowsIsARefusedWatchButAnOpenObligation(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)

	// Act: only the announcement is ever written.
	shim.write(ctx, t, shim.agentEntry("w-announced-only", "detached:"+itestBashHandle,
		frameLine(agentID("main"), detachedRunFrame("main", itestBashHandle, itestBashRunID))))

	// Assert: the obligation is open...
	if got := workValues(liveWork(ctx, t, cli, "main").GetLiveDetached()); !contains(got, itestBashHandle) {
		t.Fatalf("live_detached = %v, want the announced run", got)
	}
	// ...and the run's own stream is refused, because there is no row.
	stream := watchBashRun(ctx, t, cli, itestBashRunID)
	defer testclose.OrFail(t, stream)
	assertBashRunRefused(t, stream)
}
