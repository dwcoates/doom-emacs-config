package cachelapse

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// Telemetry is where the Keeper reports what happens to every workspace's
// prompt cache, so how often caches stay warm, lapse or go cold under a user
// is a series with a version and a day on it (runtimemetrics.Surface), per
// workspace. Every method is a single counter or histogram update and never
// blocks.
type Telemetry interface {
	// ObserveBeat records one keep-alive the Keeper asked for WS, by how the
	// ask ended (the Beat* outcomes), and how long after it came due it went
	// out.
	ObserveBeat(ws ids.WorkspaceID, outcome string, late time.Duration)
	// CountLapse records one recorded cache lapse of WS coming due, by what
	// became of it (the Lapse* outcomes).
	CountLapse(ws ids.WorkspaceID, outcome string)
	// ObserveColdGate records one cold gate a shim judged for WS, by its kind
	// (the ColdGate* outcomes), and how long the cache sat idle before it. A
	// gate raised again from its record is the same gate, never counted twice.
	ObserveColdGate(ws ids.WorkspaceID, outcome string, idle time.Duration)
}

// How an asked-for keep-alive ended (Telemetry.ObserveBeat).
const (
	BeatOutcomeSent             = "sent"
	BeatOutcomeMainAgentBusy    = "main_agent_busy"
	BeatOutcomeDetachedWorkLive = "detached_work_live"
	BeatOutcomeCacheLapsed      = "cache_lapsed"
	BeatOutcomeSubmitFailed     = "submit_failed"
	BeatOutcomeShimUnreachable  = "shim_unreachable"
	BeatOutcomeNoSession        = "no_session"
	BeatOutcomeUnnamedJudgment  = "unnamed_judgment"
	// BeatOutcomeSessionParked is a beat due while its session is still parked, and
	// BeatOutcomeNotServed one due while no session is served at all.
	BeatOutcomeSessionParked = "session_parked"
	BeatOutcomeNotServed     = "not_served"
)

// What became of a lapse that came due (Telemetry.CountLapse).
const (
	LapseStoodDown   = "stood_down"
	LapseShownFirst  = "shown_first"
	LapseMergeQueued = "merge_queued"
	LapseMovedLater  = "moved_later"
)

// A cold gate's kind (Telemetry.ObserveColdGate).
const (
	ColdGateLapsedFiveMin  = "lapsed_5m"
	ColdGateLapsedOneHour  = "lapsed_1h"
	ColdGateLapsedOtherTTL = "lapsed_other_ttl"
	ColdGateModelSwitch    = "model_switch"
	ColdGateUnnamedReason  = "unnamed_reason"
)

const (
	fiveMinuteTTL = 5 * time.Minute
	oneHourTTL    = time.Hour
)

// coldGateOutcome names a cold gate's kind: a lapse by the cache lifetime the
// vendor sold it at, or a model switch.
func coldGateOutcome(cold *conversationv1.SessionCold) string {
	switch {
	case cold.GetModelSwitch() != nil:
		return ColdGateModelSwitch
	case cold.GetLapsed() != nil:
		switch time.Duration(cold.GetLapsed().GetCacheTtlMs()) * time.Millisecond {
		case fiveMinuteTTL:
			return ColdGateLapsedFiveMin
		case oneHourTTL:
			return ColdGateLapsedOneHour
		}
		return ColdGateLapsedOtherTTL
	}
	return ColdGateUnnamedReason
}
