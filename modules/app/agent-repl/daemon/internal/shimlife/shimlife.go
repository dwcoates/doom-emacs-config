// Package shimlife names why a workspace's shim starts and why it is stood
// down, the two vocabularies the shim lifecycle series count by
// (agentrepl_shim_start_seconds, agentrepl_shim_stand_downs_total). The caller
// that decides a start or a stand-down states its reason on the context it
// hands the fleet; the fleet reads it where it starts or tears a shim down,
// so no start path or stand-down path has to grow a parameter for it.
//
// docs/developer/shim-lifecycle.md says when each one happens.
package shimlife

import "context"

// StartCause is why a shim starts.
type StartCause string

// The start causes.
const (
	// StartPrompt is a prompt, the user's or another workspace's.
	StartPrompt StartCause = "prompt"
	// StartRelaunch is a shim that died being brought back.
	StartRelaunch StartCause = "relaunch"
	// StartKeepalive is a parked session restored for its next keep-alive.
	StartKeepalive StartCause = "keepalive"
	// StartColdGateAnswer is a cold gate's answer re-opening its session.
	StartColdGateAnswer StartCause = "cold_gate_answer"
	// StartBoot is a daemon's boot bringing back a shim that was up.
	StartBoot StartCause = "boot"
	// StartTakeover is a handover successor bringing back a shim that was up.
	StartTakeover StartCause = "takeover"
	// StartRestart is the restart verb starting a shim none was up for.
	StartRestart StartCause = "restart"
	// StartCreate is a workspace created with an initial prompt.
	StartCreate StartCause = "create"
	// StartFork is a fork's creation.
	StartFork StartCause = "fork"
	// StartThread is a chat thread created from its question.
	StartThread StartCause = "thread"
	// StartMerge is a merge workspace's session a merge needs.
	StartMerge StartCause = "merge"
	// StartRebind is a workspace rebound to another conversation.
	StartRebind StartCause = "rebind"
	// StartSettings is a model, effort or permission-mode pick on a
	// workspace with no shim: the pick is applied once the shim is up, and
	// the shim is stood down on the switch away unless a prompt ran meanwhile.
	StartSettings StartCause = "settings"
	// StartUnstated is a start no caller stated a cause for: a wiring gap,
	// which the fleet records at INFO so it is seen.
	StartUnstated StartCause = "unstated"
)

// StandDownReason is why a shim is stood down on purpose.
type StandDownReason string

// The stand-down reasons.
const (
	// StandDownKeepaliveCycle is an idle session parked between keep-alives.
	StandDownKeepaliveCycle StandDownReason = "keepalive_cycle"
	// StandDownHibernate is a session idle past the cutoff.
	StandDownHibernate StandDownReason = "hibernate"
	// StandDownCacheLapse is a hidden or closed workspace whose cache lapsed.
	StandDownCacheLapse StandDownReason = "cache_lapse"
	// StandDownColdGate is a shim whose session stands at its cold gate.
	StandDownColdGate StandDownReason = "cold_gate"
	// StandDownKill is the kill verb, or a workspace closed or deleted.
	StandDownKill StandDownReason = "kill"
	// StandDownRebind is a workspace rebound to another conversation.
	StandDownRebind StandDownReason = "rebind"
	// StandDownHandover is a shim handed to, or stopped for, a successor.
	StandDownHandover StandDownReason = "handover"
	// StandDownShutdown is the daemon's own exit stopping its shims.
	StandDownShutdown StandDownReason = "shutdown"
	// StandDownSettingsVisit is a shim a settings pick started, stood down on
	// the switch away from its workspace with no prompt run since.
	StandDownSettingsVisit StandDownReason = "settings_visit"
	// StandDownReplaced is a held shim replaced by a fresh one inside a start
	// (it runs another account's root).
	StandDownReplaced StandDownReason = "replaced"
	// StandDownUnstated is a stand-down no caller stated a reason for: a
	// wiring gap, which the fleet records at INFO so it is seen.
	StandDownUnstated StandDownReason = "unstated"
)

type startKey struct{}

type standDownKey struct{}

// WithStart answers CTX carrying the start cause C.
func WithStart(ctx context.Context, c StartCause) context.Context {
	return context.WithValue(ctx, startKey{}, c)
}

// StartOf answers the start cause CTX carries, false when it carries none.
func StartOf(ctx context.Context) (StartCause, bool) {
	c, ok := ctx.Value(startKey{}).(StartCause)
	return c, ok
}

// WithStandDown answers CTX carrying the stand-down reason R.
func WithStandDown(ctx context.Context, r StandDownReason) context.Context {
	return context.WithValue(ctx, standDownKey{}, r)
}

// StandDownOf answers the stand-down reason CTX carries, false when it
// carries none.
func StandDownOf(ctx context.Context) (StandDownReason, bool) {
	r, ok := ctx.Value(standDownKey{}).(StandDownReason)
	return r, ok
}
