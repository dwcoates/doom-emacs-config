package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/errclass"
	"claude-repld/internal/keepalive"
)

// compactioncold.go — THE COMPACTION THAT READ THE CONVERSATION COLD.
//
// A compaction the DAEMON submits is supposed to be the cheapest turn of the
// session: it reads the whole conversation, which is exactly the standing
// cached prefix, so a compaction taken while the prompt cache is alive pays
// cache-read rates for essentially all of it. Taken after the cache has died it
// pays full freight for the same read — the single most expensive turn the
// session can produce, bought for nothing, since the user asked for none of it.
//
// THAT IS NOT A HYPOTHETICAL. Compaction used to happen only at REVIVAL, which
// by construction is after the cache expired, and one live revival compaction
// read 1.5 MILLION uncached input tokens. Warm compaction (warmcompact.go)
// exists to make that case unreachable; this file is what says so out loud when
// it happens anyway.
//
// IT IS A REPORT, NEVER A LIFECYCLE DECISION. Nothing here stops a session,
// refuses a prompt, or retries anything: the compaction already ran and the
// money is already spent. What is owed is evidence — an error record with the
// full usage breakdown, and a card on the conversation the cost was charged to
// — because a cost defect that only ever appears in a monthly bill is
// indistinguishable from no defect at all. This is the difference from
// keepalivecold.go, whose measurement DOES take a transition and therefore has
// to be ordered against that transition's teardown; a report has no such
// ordering to keep, so it is taken at the observation itself.

// compactionKind names which daemon-initiated compaction a claim belongs to.
// The two are reported differently because they mean different things: a cold
// WARM compaction is the new scheduling being wrong, and a cold REVIVE
// compaction is the old behavior doing what it always did.
type compactionKind string

const (
	// compactionWarm is the pre-expiry compaction warmcompact.go schedules.
	compactionWarm compactionKind = "warm"
	// compactionRevive is the compact-first revival's own `/compact`.
	compactionRevive compactionKind = "revive"
)

// daemonCompaction is the DAEMON-INITIATED compaction currently in flight for
// one session. Read and written only under Manager.mu.
//
// IT IS A CLAIM, NOT A NAMING CONVENTION. The tripwire could have matched the
// `warm-compact:`/`revive-compact:` prefixes a turn id already carries, but a
// prefix says what an id was MEANT for, and a claim says what this session is
// actually running right now — the same distinction submitter draws against
// sniffing the free-form origin string (mergelease.go).
type daemonCompaction struct {
	// turnID is the request id the compaction was submitted under, which is the
	// submitted turn's own identity on the wire.
	turnID string
	// kind is which of the two compactions this is.
	kind compactionKind
	// anchorTurnEndMs is the durable last-turn-end the decision to compact was
	// taken against. It is the warm path's exactly-once guarantee: a second
	// attempt measured from the same anchor is the same decision taken twice,
	// and is declined rather than submitted (warmcompact.go). Zero on the
	// revive path, which is driven by a user command and needs no anchor.
	anchorTurnEndMs int64
}

// claimDaemonCompactionLocked registers an in-flight daemon compaction, and
// refuses when one is already claimed. Caller holds m.mu.
//
// A SECOND CLAIM IS AN INVARIANT VIOLATION, not a queue. Both submitters run
// under an exclusivity claim of their own — the warm path's eligibility check
// and the revival's own claim — so reaching here twice means one of those
// failed, and overwriting would silently hand the first compaction's cost to
// the second one's record.
func (m *Manager) claimDaemonCompactionLocked(d *sessionController, claim daemonCompaction) error {
	if d.daemonCompaction != nil {
		return fmt.Errorf("session-controller: refusing to claim %s compaction %s for workspace %q: the %s compaction %s is already in flight",
			claim.kind, claim.turnID, d.workspace, d.daemonCompaction.kind, d.daemonCompaction.turnID)
	}
	d.daemonCompaction = &claim
	return nil
}

// releaseDaemonCompactionLocked retires the claim when turnID owns it, and
// reports whether it did. Caller holds m.mu.
//
// IT MATCHES ON THE TURN ID for noteKeepAliveTurnEndedLocked's reason: a turn
// end or an abandonment belonging to some other turn must not retire a claim
// this compaction still owns.
func (m *Manager) releaseDaemonCompactionLocked(d *sessionController, turnID string) bool {
	if d.daemonCompaction == nil || d.daemonCompaction.turnID != turnID {
		return false
	}
	d.daemonCompaction = nil
	return true
}

// noteDaemonCompactionCost trips the cold-read alarm when the result just
// reduced belongs to the daemon compaction now in flight and it read the
// conversation at the uncached rate.
//
// TWO INDEPENDENT FACTS MUST AGREE, the discipline noteKeepAlivePingCost
// follows: the accounting reducer attributed this result to cost.turnID, and
// this session's own claim says cost.turnID is the compaction running right
// now. Either alone would be an attribution by elimination.
//
// Called on the shim read-loop goroutine with no manager mutex held.
func (m *Manager) noteDaemonCompactionCost(d *sessionController, cost turnResultCost) {
	if d == nil || cost.turnID == "" {
		return
	}
	m.mu.Lock()
	claim := d.daemonCompaction
	if claim == nil || claim.turnID != cost.turnID {
		m.mu.Unlock()
		return
	}
	kind := claim.kind
	m.mu.Unlock()

	if !keepalive.ColdCompaction(cost.usage) {
		// THE WARM CASE IS RECORDED TOO, and deliberately. The alarm's silence is
		// otherwise indistinguishable from the alarm never having been asked, and
		// this line is the evidence that the feature is working — a compaction
		// that read a whole conversation for a few hundred uncached tokens.
		m.logf("session-controller: daemon compaction read WARM ws=%q session=%s turn_id=%s kind=%s threshold=%d %s — the compaction read the conversation from the prompt cache, which is what it is scheduled before the cache's expiry to do",
			d.workspace, d.sessionID, cost.turnID, kind, keepalive.ColdCompactionUncachedTokens, cost.breakdown())
		return
	}

	// THE ERROR RECORD IS THE LOUDEST THING THIS PACKAGE EMITS, and it carries
	// every identity and every bucket: a cost defect found in a bill weeks later
	// is reconstructed from exactly these fields, and a reader who has to
	// hand-correlate the workspace against another line has already lost.
	m.errorf("session-controller: DAEMON COMPACTION READ COLD ws=%q session=%s turn_id=%s kind=%s threshold=%d %s — a compaction reads the ENTIRE conversation, so running it against a live prompt cache costs cache-read rates and running it against a dead one costs full freight for the same read; this one paid full freight, which is precisely the cost the compaction scheduling exists to avoid, and the user was billed for it having asked for nothing",
		d.workspace, d.sessionID, cost.turnID, kind, keepalive.ColdCompactionUncachedTokens, cost.breakdown())

	if d.consumer == nil {
		m.errorf("session-controller: cold-compaction card NOT PUSHED ws=%q session=%s turn_id=%s kind=%s — the session has no consumer, so the finding reaches the log and no conversation surface",
			d.workspace, d.sessionID, cost.turnID, kind)
		return
	}
	detail := fmt.Sprintf("turn_id=%s kind=%s threshold_uncached_input_tokens=%d %s",
		cost.turnID, kind, keepalive.ColdCompactionUncachedTokens, cost.breakdown())
	d.consumer.pushFailure(d.consumer.coldCompactionUUID(cost.turnID), errclass.ColdCompaction(detail))
}
