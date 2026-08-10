package ssm

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// publicationlock_test.go covers the LOCK-ORDER half of the synchronous
// publication barrier: a caller's publisher must run with mu released.
//
// The publisher is the frontend's Broadcast in production, and the frontend's
// materialization release calls back into this manager while holding its own
// lock. Publishing from under mu therefore closed a cycle that left mu held
// forever — and mu is what every session controller and the merge queue's
// status ingest need, so all of them wedged behind it, twice.

// THE REGRESSION, in its most direct form: a publisher that asks the manager a
// question. Under the old arrangement the publisher ran under mu and this call
// could never return, Go mutexes not being reentrant.
func TestASynchronousPublisherMayReadTheManagerItIsPublishingFor(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	read := 0

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {
		states, err := m.Snapshot()
		if err != nil {
			t.Errorf("Snapshot from inside the publisher: %v", err)
			return
		}
		read = len(states)
	})

	// Assert.
	if err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if read != 1 {
		t.Fatalf("the publisher read %d workspace state(s), want the one it is publishing", read)
	}
}

// The barrier the lock hold bought is unchanged: publications reach the caller
// in the order the transitions were committed.
func TestPublicationsReachTheCallerInTransitionOrder(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	var seen []frontendv1.RenderState
	record := func(state *frontendv1.WorkspaceState) { seen = append(seen, state.GetState()) }

	// Act — the accepted edge claims the turn, the terminal result settles it.
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, record); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if _, err := m.SettleTurnFromTerminalResult("ws1", "s1", record); err != nil {
		t.Fatalf("SettleTurnFromTerminalResult: %v", err)
	}

	// Assert.
	if len(seen) != 2 {
		t.Fatalf("published %d states (%v), want the accept and the settle", len(seen), seen)
	}
	if seen[0] != frontendv1.RenderState_RENDER_STATE_SUBMITTING {
		t.Fatalf("first publication = %s, want SUBMITTING ahead of the settle", seen[0])
	}
	if seen[1] == frontendv1.RenderState_RENDER_STATE_SUBMITTING {
		t.Fatalf("second publication = %s, want the settled state after it", seen[1])
	}
}

// A publisher never observes mu held. TryLock is the probe because it needs no
// timing at all — the answer is a fact about this instant.
func TestASynchronousPublisherNeverObservesTheManagerMutexHeld(t *testing.T) {
	// Arrange.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := applyTest(m, evSessionStarted("s1", 1)); err != nil {
		t.Fatalf("session started: %v", err)
	}
	var held []bool

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {
		if m.mu.TryLock() {
			m.mu.Unlock()
			held = append(held, false)
			return
		}
		held = append(held, true)
	})

	// Assert.
	if err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	if len(held) != 1 || held[0] {
		t.Fatalf("publisher observed mu held = %v, want one publication with mu FREE", held)
	}
}
