package workspace

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/shimlife"
)

// ShimLifeTelemetry is where the fleet counts its shims' lives
// (runtimemetrics.Surface): every start by why it started, and every
// deliberate stand-down by why. Every method is a single counter or histogram
// update and never blocks.
type ShimLifeTelemetry interface {
	// ObserveShimStart records one start of WS's shim by CAUSE and OUTCOME,
	// and how long it took.
	ObserveShimStart(ws ids.WorkspaceID, cause, outcome string, took time.Duration)
	// CountShimStandDown records one stand-down of WS's shim by REASON.
	CountShimStandDown(ws ids.WorkspaceID, reason string)
}

// observeShimStart records a start that ran from BEGAN until now and ended in
// ERR, by the cause its caller stated on CTX. A start no caller stated a cause
// for is a wiring gap, recorded at INFO and counted as unstated.
func (f *Fleet) observeShimStart(ctx context.Context, ws ids.WorkspaceID, began time.Time, err error) {
	cause, stated := shimlife.StartOf(ctx)
	if !stated {
		cause = shimlife.StartUnstated
		f.logFor(ctx, ws).Info(opBringUp, "a shim start named no cause; it is counted as unstated", nil)
	}
	f.deps.ShimLife.ObserveShimStart(ws, string(cause), string(restarttiming.OutcomeOf(err)), f.now().Sub(began))
}

// countStandDown records a deliberate stand-down of WS's shim, by the reason
// its caller stated on CTX. A stand-down no caller stated a reason for is a
// wiring gap, recorded at INFO and counted as unstated.
func (f *Fleet) countStandDown(ctx context.Context, ws ids.WorkspaceID) {
	reason, stated := shimlife.StandDownOf(ctx)
	if !stated {
		reason = shimlife.StandDownUnstated
		f.logFor(ctx, ws).Info(opBringUp, "a shim stand-down named no reason; it is counted as unstated", dlog.Context{"op": "stand_down"})
	}
	f.deps.ShimLife.CountShimStandDown(ws, string(reason))
}

// ColdGateAnswerTelemetry counts cold gates answered (runtimemetrics.Surface).
// It is a single counter update and never blocks.
type ColdGateAnswerTelemetry interface {
	CountColdGateAnswer(ws ids.WorkspaceID, choice, outcome string)
}

// How an answered cold gate ended (ColdGateAnswerTelemetry).
const (
	// GateAnswerResumed is the session up on the answer.
	GateAnswerResumed = "resumed"
	// GateAnswerFailed is a re-open that failed; the gate stands again.
	GateAnswerFailed = "failed"
	// GateAnswerAbandoned is a re-open the daemon's stand-down cut.
	GateAnswerAbandoned = "abandoned"
	// GateAnswerRefused is an answer refused before its re-open started (the
	// workspace could not be shown, or its held prompts settled); the gate
	// stands again.
	GateAnswerRefused = "refused"
)
