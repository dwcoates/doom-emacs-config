package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/ssm"
)

// compactiongate.go — THE ONE QUESTION BOTH DAEMON COMPACTIONS ASK.
//
// The daemon initiates a compaction from two places that know nothing about
// each other: the pre-expiry WARM compaction, which fires on the cache clock
// (warmcompact.go), and the compact-first REVIVAL, which fires when a user
// picks it off the hibernation gate (revive.go). Each has its own exclusivity
// claim over ITS OWN path, and neither can see the other's — so the ordinary
// sequence of warm-compact, hibernate, revive-with-compaction runs two
// whole-conversation compactions back to back with nothing said between them.
//
// The durable half of the answer lives in the SSM, which is where a fact about
// a workspace belongs and is the only place both paths already reach
// (ssm/compactiongate.go). This is the reading half: ONE helper, so the two
// call sites cannot disagree about what redundant means or about how loudly a
// declined compaction is reported, and so a third daemon-initiated compaction
// added later inherits both by asking the same question.

// compactionRedundant reports whether a compaction the daemon would submit for
// workspace right now would be the SECOND compaction of a conversation nothing
// has been added to since the first.
//
// THE READ CAN FAIL AND THE FAILURE IS RETURNED, never absorbed into a
// permissive "not redundant". A caller that cannot read the gate does not know
// whether it is about to duplicate a whole-conversation model call, and
// guessing in the caller's favor is exactly how the duplicate this exists to
// prevent gets submitted anyway. Both callers decline on the error and say so.
func (m *Manager) compactionRedundant(workspace string) (bool, ssm.CompactionGate, error) {
	gate, err := m.cfg.SSM.CompactionGateOf(workspace)
	if err != nil {
		return false, ssm.CompactionGate{}, fmt.Errorf("session-controller: reading the compaction gate for workspace %q: %w", workspace, err)
	}
	return gate.Redundant(), gate, nil
}

// compactionRedundantDetail renders one gate for a log line, so a declined
// compaction reports the two timestamps its verdict was taken from rather than
// only the verdict.
func compactionRedundantDetail(gate ssm.CompactionGate) string {
	return fmt.Sprintf("last_compacted_at_ms=%d last_prompt_at_ms=%d", gate.CompactedAtMs, gate.PromptAtMs)
}
