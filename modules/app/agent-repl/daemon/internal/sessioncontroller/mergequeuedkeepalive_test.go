package sessioncontroller

import (
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/registry"
	"claude-repld/internal/ssm"
)

// mergequeuedkeepalive_test.go — A QUEUED WORKSPACE IS PENDING WORK, NOT A QUIET
// ONE.
//
// The live failure these cover: a workspace sat in RENDER_STATE_MERGE_QUEUED for
// an hour, every cache keep-alive ping was refused by the merge axis, the prompt
// cache expired, the cache-cold arm hibernated it, and the conflict resolution
// that eventually ran re-ingested the whole conversation at the uncached rate.

// setQueued puts the rig's workspace on the merge queue as far as every gate
// that reads the resolved state is concerned.
func setQueued(applier *fakeApplier) {
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State: frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
	})
}

func TestKeepAlivePingAsksForTheIdleMachineryAdmission(t *testing.T) {
	// Arrange: the exemption is claimed by SUBMITTER, so what the ping asks for
	// is the fact the whole fix turns on.
	// Act.
	got := submitterKeepAlive.admission()
	// Assert.
	if got != ssm.PromptAdmissionIdleMachinery {
		t.Fatalf("submitterKeepAlive.admission() = %v, want idle machinery", got)
	}
}

func TestWarmCompactionAsksForTheIdleMachineryAdmission(t *testing.T) {
	// Arrange: the warm compaction is blocked by the same merge axis and for the
	// same reason, so it claims the same exemption.
	// Act.
	got := submitterWarmCompaction.admission()
	// Assert.
	if got != ssm.PromptAdmissionIdleMachinery {
		t.Fatalf("submitterWarmCompaction.admission() = %v, want idle machinery", got)
	}
}

func TestOrdinarySubmittersAskForTheUserAdmission(t *testing.T) {
	// Arrange: everything with a person behind it keeps the pre-existing
	// behavior, and the merge lease holder takes no accepted edge at all.
	tests := []struct {
		name string
		who  submitter
	}{
		{"user", submitterUser},
		{"merge lease holder", submitterMergeLeaseHolder},
		{"revival", submitterRevival},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := tt.who.admission()
			// Assert.
			if got != ssm.PromptAdmissionUser {
				t.Fatalf("%s.admission() = %v, want user", tt.who, got)
			}
		})
	}
}

func TestQueuedWorkspaceIsImmuneToTheCacheColdHibernation(t *testing.T) {
	// Arrange: the cache-cold arm's own cause, aimed at a workspace resting on
	// the merge queue. The ping measured a dead cache; the workspace is still
	// owed a merge.
	m, applier, _ := newHibernationRig(t)
	setQueued(applier)

	// Act.
	err := m.hibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseCacheExpired, TTLMs: 3_600_000,
	}, evidenceObserved)

	// Assert.
	if !errors.Is(err, ErrHibernationMergeQueued) {
		t.Fatalf("cache-cold hibernation of a queued workspace = %v, want %v", err, ErrHibernationMergeQueued)
	}
}

func TestQueuedWorkspaceIsImmuneToTheIdleSweepHibernation(t *testing.T) {
	// Arrange: the idle sweep's cause reaches the SAME one transition, so the
	// gate placed there covers it without the sweep having to learn about the
	// merge queue.
	m, applier, _ := newHibernationRig(t)
	setQueued(applier)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	})

	// Assert.
	if !errors.Is(err, ErrHibernationMergeQueued) {
		t.Fatalf("idle-sweep hibernation of a queued workspace = %v, want %v", err, ErrHibernationMergeQueued)
	}
}

func TestQueuedWorkspaceStillHonorsAForcedHibernation(t *testing.T) {
	// Arrange: a forced sleep is the user's own decision about a workspace they
	// are looking at, not a stale clock reading, so the queue does not overrule
	// it.
	m, applier, _ := newHibernationRig(t)
	setQueued(applier)

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{Cause: registry.HibernationCauseForced})

	// Assert.
	if err != nil {
		t.Fatalf("forced hibernation of a queued workspace = %v, want the sleep the user asked for", err)
	}
}

func TestEvictionFromTheQueueRestoresTheAutomaticHibernation(t *testing.T) {
	// Arrange: a failed merge run EVICTS the entry, and the workspace stops
	// being queued. The keep-alive cadence — pings and, at its far end, the
	// hibernation the policy owes — must resume exactly as for any live session.
	m, applier, _ := newHibernationRig(t)
	setQueued(applier)
	if err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	}); !errors.Is(err, ErrHibernationMergeQueued) {
		t.Fatalf("arrangement: queued hibernation = %v, want it refused", err)
	}
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State: frontendv1.RenderState_RENDER_STATE_MERGE_FAILED,
	})

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	})

	// Assert.
	if errors.Is(err, ErrHibernationMergeQueued) {
		t.Fatalf("hibernation after eviction = %v, want the queue gate no longer to hold it", err)
	}
	if err != nil {
		t.Fatalf("hibernation after eviction = %v, want it taken", err)
	}
}

func TestAnUnreadableStateRefusesTheAutomaticHibernation(t *testing.T) {
	// Arrange: the gate is the only thing standing between a queued workspace
	// and a teardown, so a hibernation taken because the daemon could not find
	// out whether it was queued is the very outcome the gate exists to prevent.
	m, applier, _ := newHibernationRig(t)
	applier.currentErr = errors.New("state store unavailable")

	// Act.
	err := m.HibernateWithCause("ws", registry.HibernationDetail{
		Cause: registry.HibernationCauseIdleCutoff, CutoffMs: 21_600_000,
	})

	// Assert.
	if err == nil || !errors.Is(err, applier.currentErr) {
		t.Fatalf("hibernation over an unreadable state = %v, want the read failure surfaced", err)
	}
}
