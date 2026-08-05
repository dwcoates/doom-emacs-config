// WHEN THE DAEMON PINGS, and — far more of this file — when it must not.
//
// The cache keep-alive exists to keep a vendor prompt cache warm across a
// user's idle stretch, and its entire value depends on a single number being
// respected: the ping fires inside the window between the cache TTL minus the
// leeway and the TTL itself. Early is waste (a model call bought nothing);
// late is worse than waste (the ping pays a full cold re-ingest for a cache
// that had already gone).
//
// Every timing statement here is an EVENT the test causes: the daemon's wall
// clock is moved (h.clock) and the check it feeds is fired (h.sweepIdle).
// Nothing waits.
//
// See hibernationharness_test.go for why a ping is observed in the shim-store
// rather than on the frontend, and for the two interpretations this file rests
// on.
package e2e

import (
	"fmt"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// --- (1) the ping fires at TTL minus leeway ----------------------------------

// TestE2EAKeepAlivePingFiresAtTheCacheTtlLessLeeway is the feature's positive
// edge: a session idle for exactly as long as the policy allows before the
// cache is at risk gets a keep-alive turn, submitted by the daemon under its
// own attribution.
func TestE2EAKeepAlivePingFiresAtTheCacheTtlLessLeeway(t *testing.T) {
	// Arrange — one real turn ended, and nothing since.
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act — the session has now been idle for exactly the ping window's open.
	s.idleFor(t, policy.pingAt())

	// Assert
	ping := s.store.await(t, "a TurnStarted attributed to the cache keep-alive", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
	if got := keepAlivePing(ping).GetTurnId(); got == "" {
		t.Error("the keep-alive turn carries no turn_id: the queue hold, the rewind's dropped-turn list and the cost alert all name a ping by that id, so an unnamed one is unjoinable")
	}
}

// TestE2EAKeepAlivePingCarriesTheContractText covers WHAT IS SENT. The wording
// is contract rather than decoration: it is what makes the vendor answer in one
// token, and what makes the turn recognizable as plumbing to a human reading a
// retained transcript.
func TestE2EAKeepAlivePingCarriesTheContractText(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act
	s.idleFor(t, policy.pingAt())

	// Assert — TurnStarted.prompt_preview is the durable copy of the submitted
	// text's first line, and the ping is exactly one line.
	ping := s.store.await(t, "the keep-alive TurnStarted", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
	if got := keepAlivePing(ping).GetPromptPreview(); got != keepAlivePingText {
		t.Errorf("the keep-alive prompt is %q, want the contract text %q", got, keepAlivePingText)
	}
}

// TestE2ENoKeepAlivePingBeforeTheCacheTtlLessLeeway covers THE EARLY EDGE: one
// minute short of the window, the cache is still comfortably warm and a ping
// would be a model call bought for nothing.
//
// The negative is proved against a SENTINEL — a real user turn submitted after
// the early check — because the store preserves arrival order on every hop, so
// an early ping would necessarily have been written before it.
func TestE2ENoKeepAlivePingBeforeTheCacheTtlLessLeeway(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act — a check one minute BEFORE the window opens.
	s.idleFor(t, policy.pingAt()-time.Minute)
	writeCmd(t, s.conn, `{"requestId":"r-sentinel","submitPrompt":{"text":"sentinel","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	s.store.awaitSentinel(t, "the sentinel user turn",
		noKeepAlivePing(fmt.Sprintf("the session had been idle for only %s and the ping window opens at %s", policy.pingAt()-time.Minute, policy.pingAt())),
		func(ev *corev1.Event) bool {
			started := userTurnStart(ev)
			return started != nil && started.GetPromptPreview() == "sentinel"
		})
}

// TestE2EAKeepAlivePingIsStillTriedInsideTheRetryWindow covers the WINDOW'S
// FAR END: the ping is due from TTL-leeway onward and may still be attempted up
// to TTL-60s, because a ping that lands even a minute before expiry still saves
// the whole re-ingest. This checks at the last moment that is still allowed.
func TestE2EAKeepAlivePingIsStillTriedInsideTheRetryWindow(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act — the last elapsed idleness at which a ping is still worth trying.
	s.idleFor(t, policy.retryUntil())

	// Assert
	s.store.await(t, "a keep-alive ping at the retry window's last moment", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
}

// TestE2ENoKeepAlivePingWhileATurnIsLive covers THE BUSY SESSION: a workspace
// with a turn in flight is not idle, whatever the clock says, and a ping
// submitted into it would interleave a daemon prompt with the user's work.
//
// The turn is held open by a `!tool` prompt parked on a permission request
// (see interrupt_e2e_test.go's header). The sentinel is that turn's own end,
// which cannot arrive before a ping that had been submitted during it.
func TestE2ENoKeepAlivePingWhileATurnIsLive(t *testing.T) {
	// Arrange — a genuinely in-flight turn.
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)
	holdTurnOpen(t, s.conn, s.cwd, "r-hold", "sleep e2e-keepalive-live-turn")

	// Act — long past the ping window, with the turn still running.
	s.idleFor(t, policy.pingAt())
	writeCmd(t, s.conn, `{"requestId":"r-interrupt","interrupt":{}}`)

	// Assert — the held turn ends and no ping preceded its end.
	s.store.awaitSentinel(t, "the held turn's end",
		noKeepAlivePing("a turn was in flight, so the workspace was not idle at all"),
		func(ev *corev1.Event) bool { return ev.GetTurnEnded() != nil })
}

// TestE2ENoKeepAlivePingWhileHeldPromptsWaitOnAPausedQueue covers THE PENDING
// PROMPT: a user's own prompt is already waiting to run, so the cache is about
// to be used for real. Spending a model call to warm it first is pure waste,
// and the ping would additionally push the user's prompt behind a rewind.
//
// The state is reached the way a user reaches it: prompts queued behind a live
// turn, then a stop, which PAUSES the queue while retaining every held prompt
// (interrupt_e2e_test.go). That leaves a workspace with no live turn and real
// prompts pending — the only stable form of this condition.
func TestE2ENoKeepAlivePingWhileHeldPromptsWaitOnAPausedQueue(t *testing.T) {
	// Arrange — two prompts retained on a paused queue, no turn running.
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)
	holdTurnOpen(t, s.conn, s.cwd, "r-hold", "sleep e2e-keepalive-paused-queue")
	writeCmd(t, s.conn, `{"requestId":"r-q1","submitPrompt":{"text":"queued-one","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a QueueView holding the queued prompt": func(frame *frontendv1.FrontendFrame) bool {
			return len(queueViewFor(frame, s.cwd).GetEntries()) == 1
		},
	})
	writeCmd(t, s.conn, `{"requestId":"r-interrupt","interrupt":{}}`)
	awaitAll(t, s.conn, nil, map[string]func(*frontendv1.FrontendFrame) bool{
		"a ProgressView carrying an OPEN interrupt window": func(frame *frontendv1.FrontendFrame) bool {
			return progressFor(frame, s.cwd).GetInterrupt().GetActive()
		},
	})

	// Act
	s.idleFor(t, policy.pingAt())
	writeCmd(t, s.conn, `{"requestId":"r-sentinel","submitPrompt":{"text":"sentinel","promptOrigin":"PROMPT_ORIGIN_USER_SENT"}}`)

	// Assert
	s.store.awaitSentinel(t, "the sentinel user turn",
		noKeepAlivePing("the user's own prompt was already queued, so the cache was about to be used for real"),
		func(ev *corev1.Event) bool {
			started := userTurnStart(ev)
			return started != nil && started.GetPromptPreview() == "sentinel"
		})
}

// TestE2ENoKeepAlivePingWhileTheMergeLeaseHoldsTheWorkspace covers THE BORROWED
// SHIM: while merge.Coordinator holds the exclusivity claim, the workspace's
// shim belongs to the merge, and the user themself cannot prompt it. A daemon
// prompt injected under that claim would be doing exactly what the lease exists
// to prevent.
func TestE2ENoKeepAlivePingWhileTheMergeLeaseHoldsTheWorkspace(t *testing.T) {
	// Arrange — a live session on a conflicting worktree, merging, with the
	// lease provably held. (mergelease_e2e_test.go's shape, on a harness that
	// also carries the keep-alive policy and a movable clock.)
	policy := testKeepAlivePolicy()
	h := keepAliveHarness(t, policy)
	repo := newMergeRepo(t)
	wsDir := repo.conflictingWorktree("feature-keepalive-lease")
	s := adoptKeepAliveSession(t, h, policy, wsDir)
	w := newMergeWatch(t, s.conn)
	sendMerge(t, s.conn, "r-merge-keepalive", mergeCmdFor(t, h.geometry, repo, wsDir, "feature-keepalive-lease"))
	w.awaitOKAck("r-merge-keepalive")
	w.awaitLeaseHeld(wsDir)

	// Act
	s.idleFor(t, policy.pingAt())
	// The sentinel rides the MERGE's own conversation, because a user prompt is
	// refused outright under the lease. The merge borrows the shim to resolve
	// the conflict, so a turn under merge attribution is what necessarily
	// follows the lease being taken.
	s.store.awaitSentinel(t, "a turn submitted by the merge that holds the lease",
		noKeepAlivePing("the merge lease holds the workspace's shim, which the user themself cannot prompt"),
		func(ev *corev1.Event) bool {
			started := ev.GetTurnStarted()
			return started != nil &&
				started.GetPromptOrigin() != corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT &&
				started.GetPromptOrigin() != corev1.PromptOrigin_PROMPT_ORIGIN_CACHE_KEEP_ALIVE
		})
}

// --- (2) the overslept cache ---------------------------------------------------

// TestE2EAnOversleptCacheHibernatesInsteadOfPinging covers THE LAPTOP LID: the
// check runs at an elapsed idleness at or beyond the whole TTL, because the
// machine slept or the daemon was down. The cache is already gone, so a ping
// would pay a full cold re-ingest to warm nothing — the discovery IS the
// hibernate.
func TestE2EAnOversleptCacheHibernatesInsteadOfPinging(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act — the whole TTL has passed since the last turn ended.
	s.idleFor(t, policy.ttl)

	// Assert — hibernated, and with no ping submitted on the way there. The
	// frontend assertion comes first because it is the one that establishes the
	// transition happened at all.
	detail := awaitHibernationDetail(t, s.conn, s.sessionID)
	if detail.GetCacheExpired() == nil {
		t.Fatalf("hibernation cause is %T, want the cache_expired arm: the session went to sleep because its prompt cache was already cold, not because it had been idle past the cutoff", detail.GetCause())
	}
}

// TestE2EAnOversleptCacheReportsTheElapsedAndTtlItMeasured covers WHAT THE USER
// IS TOLD. The cause carries its own evidence so the gate can say "asleep — the
// cache expired N minutes into a M-minute TTL" without the frontend knowing any
// daemon configuration.
func TestE2EAnOversleptCacheReportsTheElapsedAndTtlItMeasured(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)
	overslept := policy.ttl + 3*time.Minute

	// Act
	s.idleFor(t, overslept)

	// Assert
	expired := awaitHibernationDetail(t, s.conn, s.sessionID).GetCacheExpired()
	if expired == nil {
		t.Fatal("no cache_expired cause to read evidence from")
	}
	if got, want := expired.GetTtlMs(), policy.ttl.Milliseconds(); got != want {
		t.Errorf("cache_expired ttl_ms = %d, want the configured TTL %d", got, want)
	}
	// The elapsed is a MEASUREMENT, so it is bounded rather than pinned: it is
	// however long the daemon's clock says has passed since the durable last
	// turn end, which is at least what this test claimed and no more than that
	// plus the real time the warm-up turn itself took.
	if got := expired.GetElapsedMs(); got < overslept.Milliseconds() {
		t.Errorf("cache_expired elapsed_ms = %d, want at least the %d ms of idleness this test claimed", got, overslept.Milliseconds())
	}
	if got, floor := expired.GetElapsedMs(), policy.ttl.Milliseconds(); got < floor {
		t.Errorf("cache_expired elapsed_ms = %d is BELOW the ttl_ms it is supposed to have exceeded (%d): this cause is only reachable when the cache is already cold", got, floor)
	}
}

// TestE2EAnOversleptCacheSubmitsNoPingOnItsWayToHibernation covers the other
// half of the same edge, stated as the negative it is: the discovery and the
// hibernation are ONE transition, so no ping is submitted first and none is
// submitted after.
func TestE2EAnOversleptCacheSubmitsNoPingOnItsWayToHibernation(t *testing.T) {
	// Arrange
	policy := testKeepAlivePolicy()
	s := newKeepAliveSession(t, policy)

	// Act
	s.idleFor(t, policy.ttl)
	awaitHibernationDetail(t, s.conn, s.sessionID)

	// Assert — the session's whole durable record, read to its end, contains no
	// keep-alive turn. The hibernation stopped the shim, so the seq space is
	// closed and the tail cannot be outrun by a late write.
	forbidden := noKeepAlivePing("the cache had already expired, so the check hibernated instead of pinging")
	for {
		select {
		case ev, ok := <-s.store.events:
			if !ok {
				return
			}
			if why := forbidden(ev); why != "" {
				t.Fatalf("forbidden durable event: %s", why)
			}
		default:
			return
		}
	}
}

// --- the documented defaults ----------------------------------------------------

// TestE2EWithNoConfigurationThePingFiresAtTheDocumentedDefaults covers the
// numbers a real deployment actually runs: nobody sets these variables, so the
// defaults ARE the policy, and a test that only ever configured them would
// never notice one of them changing.
func TestE2EWithNoConfigurationThePingFiresAtTheDocumentedDefaults(t *testing.T) {
	// Arrange — no keep-alive configuration at all.
	cwd := t.TempDir()
	for _, key := range []string{envKeepAliveCacheTTL, envKeepAliveLeeway, envHibernateIdleCutoff, envUncachedCostAlertToks} {
		unsetEnv(t, key)
	}
	defaults := keepAlivePolicy{
		ttl:        defaultKeepAliveCacheTTL,
		leeway:     defaultKeepAliveLeeway,
		idleCutoff: defaultHibernateIdleCutoff,
		costAlert:  defaultUncachedCostAlertToks,
	}
	h := newUDSHarness(t, withIdleSweeper(), withTestClock(), withIdleCutoff(defaults.idleCutoff))
	s := adoptKeepAliveSession(t, h, defaults, cwd)

	// Act — 58 minutes idle: one hour of TTL less two minutes of leeway.
	s.idleFor(t, defaults.pingAt())

	// Assert
	s.store.await(t, "a keep-alive ping at the default TTL less the default leeway", func(ev *corev1.Event) bool {
		return keepAlivePing(ev) != nil
	})
}
