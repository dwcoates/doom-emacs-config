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
	if got := workValues(liveWork(ctx, t, cli).GetLiveDetached()); len(got) != 1 {
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
	if got := workValues(liveWork(ctx, t, cli).GetLiveDetached()); len(got) != 1 {
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
	if got := workValues(liveWork(ctx, t, cli).GetLiveDetached()); len(got) != 0 {
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
	if got := workValues(liveWork(ctx, t, cli).GetLiveDetached()); len(got) != 0 {
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
	if got := workValues(liveWork(ctx, t, cli).GetLiveDetached()); len(got) != 0 {
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
	page := openSession(ctx, t, cli, "main", 10, nil)
	assertTexts(t, "the announcer's book", pageTexts(page.GetPage()), []string{"detached:" + itestBashHandle})
	store.assertNoErrorRecords()
}
