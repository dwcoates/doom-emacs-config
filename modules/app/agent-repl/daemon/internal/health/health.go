package health

import (
	"context"
	"errors"
	"fmt"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The operation names every record in this package carries.
const (
	opDaemon     = "daemon.health.daemon"
	opSession    = "daemon.health.session"
	opOpenFault  = "daemon.health.open_fault"
	opCloseFault = "daemon.health.close_fault"
	opOpenFaults = "daemon.health.open_faults"
	opSelfCheck  = "daemon.health.self_check"
)

// reporter is the Reporter. It snapshots immutable process identity at boot;
// every fault lives in WSM, and liveness is answered by the injected fleet
// probe.
type reporter struct {
	db       wsm.DB
	live     LiveFunc
	log      dlog.Surfaces
	now      func() time.Time
	instance ids.InstanceID
	pid      int
	buildSHA string
}

// Daemon answers DaemonHealth. UNHEALTHY IS AN ANSWER: the only error this
// returns is one it cannot turn into a report at all, and even then the
// liveness self-check turns a failing state client into an unhealthy answer
// rather than an rpc failure.
func (r *reporter) Daemon(ctx context.Context) (*agentreplv1.DaemonHealthResponse, error) {
	identity := &agentreplv1.DaemonIdentity{
		InstanceId: string(r.instance),
		Pid:        int64(r.pid),
		BuildSha:   r.buildSHA,
	}
	log := r.log.Global().With(dlog.Context{
		"instance": string(r.instance), "pid": r.pid, "build_sha": r.buildSHA,
	})
	var faults []*agentreplv1.DaemonFault

	// The liveness self-check: the daemon's own state client must answer. A
	// state client that will not read is the one fault the daemon can always
	// detect about itself, and it is reported, never returned as an error.
	open, err := r.db.OpenFaults(ctx, wsm.FaultScope{})
	if err != nil {
		log.Error(opSelfCheck, "state client refused the open-fault read", dlog.Context{
			"scope": "daemon",
			"cause": err.Error(),
		})
		faults = append(faults, selfCheckFault(err))
		return unhealthyDaemon(identity, faults), nil
	}
	log.Debug(opSelfCheck, "state client answered the open-fault read", dlog.Context{
		"scope": "daemon", "open_faults": len(open),
	})

	// Daemon scope is the faults with no workspace: a workspace-bound fault is
	// SessionHealth's answer, never the daemon's.
	for _, f := range open {
		if f.Workspace != nil {
			continue
		}
		faults = append(faults, daemonFault(f))
	}
	if len(faults) > 0 {
		log.Warn(opDaemon, "daemon is unhealthy", dlog.Context{"faults": len(faults)})
		return unhealthyDaemon(identity, faults), nil
	}
	log.Debug(opDaemon, "daemon is healthy", dlog.Context{"faults": 0})
	return &agentreplv1.DaemonHealthResponse{
		Result: &agentreplv1.DaemonHealthResponse_Success{
			Success: &agentreplv1.DaemonHealthSuccess{
				Health:   &agentreplv1.DaemonHealthSuccess_Healthy{Healthy: &agentreplv1.DaemonHealthy{}},
				Identity: identity,
			},
		},
	}, nil
}

// Session answers SessionHealth for one workspace. An unhealthy or absent
// session ANSWERS; only an UNKNOWN WORKSPACE is an error, because there is
// nothing to report health about.
func (r *reporter) Session(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.SessionHealthResponse, error) {
	log := r.log.Global().With(dlog.Context{"workspace": string(ws)})

	record, err := r.db.Workspace(ctx, ws)
	if err != nil {
		log.Error(opSession, "unknown workspace", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("health: unknown workspace %q: %w", ws, err)
	}

	// RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION. A workspace whose
	// directory cannot host a durable sink -- a scratch path, a deleted
	// worktree -- still gets its records, on the central sink and carrying
	// `unroutable_workspace'. Answering a workspace's HEALTH must not fail over
	// where the answer is WRITTEN.
	wsLog := r.log.WorkspaceOrCentral(record.Dir)

	var faults []*agentreplv1.SessionFault

	// THE RECORDED FAULTS ARE READ FIRST, because they OUTRANK the liveness
	// probe's own observation. The probe answers two booleans and can neither
	// carry an exit code nor tell a reaped process from a broken stream; the
	// records opened at the reap and at the severance can, and they survive a
	// redial that walked the link back into place.
	open, err := r.db.OpenFaults(ctx, wsm.FaultScope{Workspace: &ws})
	if err != nil {
		wsLog.Error(opSession, "state client refused the open-fault read", dlog.Context{"cause": err.Error()})
		faults = appendSessionFault(wsLog, faults, wsm.Fault{
			Kind: KindStateUnreadable, Detail: err.Error(),
		})
		return unhealthySession(faults), nil
	}
	for _, f := range open {
		faults = appendSessionFault(wsLog, faults, f)
	}
	// UNHEALTHY IS THE RECORDS' VERDICT, NOT THE RENDERER'S. A standing fault
	// whose kind the wire has no arm for is withheld from the answer, and the
	// session is still unhealthy because it STANDS.
	unrenderable := len(open) > 0 && len(faults) == 0

	exists, connected := r.live(ws)
	switch {
	case !exists:
		wsLog.Warn(opSession, "no live session", dlog.Context{"kind": KindSessionAbsent})
		faults = appendSessionFault(wsLog, faults, wsm.Fault{
			Kind: KindSessionAbsent, Detail: "the workspace has no live session",
		})
		unrenderable = true
	case !connected && !hasLostLinkFault(open):
		// The probe's observation is only reported when NOTHING recorded the
		// loss — a session parked behind a cold gate has no watcher and so no
		// link truth. A recorded loss already says it, with its evidence.
		wsLog.Warn(opSession, "the daemon-to-shim link is not serving", dlog.Context{
			"kind": KindLinkSevered,
		})
		faults = appendSessionFault(wsLog, faults, wsm.Fault{
			Kind: KindLinkSevered, Detail: "the daemon-to-shim link is not serving",
		})
	default:
		wsLog.Debug(opSession, "the session's link needs no probe-derived fault", nil)
	}

	if len(faults) > 0 || unrenderable {
		wsLog.Warn(opSession, "the session is unhealthy", dlog.Context{"faults": len(faults)})
		return unhealthySession(faults), nil
	}
	wsLog.Debug(opSession, "the session is healthy", dlog.Context{"faults": 0})
	return &agentreplv1.SessionHealthResponse{
		Result: &agentreplv1.SessionHealthResponse_Success{
			Success: &agentreplv1.SessionHealthSuccess{
				Health: &agentreplv1.SessionHealthSuccess_Healthy{Healthy: &agentreplv1.SessionHealthy{}},
			},
		},
	}, nil
}

// OpenFault records a fault and returns its id.
func (r *reporter) OpenFault(ctx context.Context, f wsm.Fault) (ids.FaultID, error) {
	log := r.log.Global()
	if f.Kind == "" {
		log.Error(opOpenFault, "refusing a fault with no kind", nil)
		return "", fmt.Errorf("health: a fault must name its kind")
	}
	if f.OpenedAt.IsZero() {
		f.OpenedAt = r.now()
	}
	id, err := r.db.OpenFault(ctx, f)
	if err != nil {
		// A FAULT ABOUT A WORKSPACE THAT IS GONE HAS NOWHERE TO STAND, and
		// that is an ordinary end for one: the link watcher and this reporter
		// both outlive the registry row, so a shim dying after its workspace
		// was forgotten arrives here about a row nothing can carry. The error
		// is still returned -- the caller decides what a lost fault means to
		// it -- but it is not this layer's ERROR.
		if errors.Is(err, wsm.ErrNotFound) {
			log.Debug(opOpenFault, "the fault names a workspace that is no longer registered", dlog.Context{
				"kind": f.Kind, "cause": err.Error(),
			})
			return "", fmt.Errorf("health: open fault %q: %w", f.Kind, err)
		}
		log.Error(opOpenFault, "could not record the fault", dlog.Context{
			"kind": f.Kind, "cause": err.Error(),
		})
		return "", fmt.Errorf("health: open fault %q: %w", f.Kind, err)
	}
	fields := dlog.Context{"fault": string(id), "kind": f.Kind, "detail": f.Detail}
	if EnvironmentKind(f.Kind) {
		log.Info(opOpenFault, "fault opened: this machine's environment, not agent-repl, is what failed", fields)
	} else {
		log.Warn(opOpenFault, "fault opened", fields)
	}
	return id, nil
}

// EnvironmentKind reports a fault kind that records THIS MACHINE'S
// ENVIRONMENT rather than anything agent-repl or the vendor did wrong: an
// unreachable network. It is stated loudly where the user reads it (the
// footer's and the roster's network_fault) and recorded at INFO, because
// nothing in the system failed and nobody reading the logs has anything to fix.
func EnvironmentKind(kind string) bool {
	return kind == KindNetworkUnreachable
}

// CloseFault stamps a fault's persisted resolved-at.
func (r *reporter) CloseFault(ctx context.Context, id ids.FaultID) error {
	log := r.log.Global()
	if id == "" {
		log.Error(opCloseFault, "refusing to close an unnamed fault", nil)
		return fmt.Errorf("health: a fault to close must be named")
	}
	at := r.now()
	if err := r.db.CloseFault(ctx, id, at); err != nil {
		log.Error(opCloseFault, "could not close the fault", dlog.Context{
			"fault": string(id), "cause": err.Error(),
		})
		return fmt.Errorf("health: close fault %q: %w", id, err)
	}
	log.Debug(opCloseFault, "fault closed", dlog.Context{"fault": string(id), "at": at.UTC()})
	return nil
}

// OpenFaults lists the open faults in scope.
func (r *reporter) OpenFaults(ctx context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	log := r.log.Global()
	out, err := r.db.OpenFaults(ctx, scope)
	if err != nil {
		if ctx.Err() != nil && errors.Is(err, ctx.Err()) {
			// THE CALLER WITHDREW THE ASK — its request ended, or the daemon is
			// stopping — so the read was abandoned, not failed. An ordinary
			// outcome that carries no defect; the error still reaches the
			// caller, which is the party that cancelled.
			log.Debug(opOpenFaults, "the caller cancelled the open-faults read before it finished", dlog.Context{"cause": err.Error()})
			return nil, fmt.Errorf("health: read open faults: %w", err)
		}
		log.Error(opOpenFaults, "could not read the open faults", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("health: read open faults: %w", err)
	}
	log.Debug(opOpenFaults, "read the open faults", dlog.Context{"count": len(out)})
	return out, nil
}

// appendSessionFault appends one recorded fault's rendered SessionFault, or
// notes at DEBUG that the wire has no arm to carry it. It is DEBUG because the
// fault is already recorded, once, by the layer that opened it; a second voice
// beside every render would say nothing new and would say it on every poll.
func appendSessionFault(log dlog.Logger, faults []*agentreplv1.SessionFault, f wsm.Fault) []*agentreplv1.SessionFault {
	rendered, ok := sessionFault(f)
	if !ok {
		log.Debug(opSession, "a standing fault has no SessionFault arm; it is withheld from the answer",
			dlog.Context{"kind": f.Kind, "armless_by_design": ArmlessSessionKind(f.Kind)})
		return faults
	}
	return append(faults, rendered)
}

func unhealthyDaemon(identity *agentreplv1.DaemonIdentity, faults []*agentreplv1.DaemonFault) *agentreplv1.DaemonHealthResponse {
	return &agentreplv1.DaemonHealthResponse{
		Result: &agentreplv1.DaemonHealthResponse_Success{
			Success: &agentreplv1.DaemonHealthSuccess{
				Health: &agentreplv1.DaemonHealthSuccess_Unhealthy{
					Unhealthy: &agentreplv1.DaemonUnhealthy{Faults: faults},
				},
				Identity: identity,
			},
		},
	}
}

func unhealthySession(faults []*agentreplv1.SessionFault) *agentreplv1.SessionHealthResponse {
	return &agentreplv1.SessionHealthResponse{
		Result: &agentreplv1.SessionHealthResponse_Success{
			Success: &agentreplv1.SessionHealthSuccess{
				Health: &agentreplv1.SessionHealthSuccess_Unhealthy{
					Unhealthy: &agentreplv1.SessionUnhealthy{Faults: faults},
				},
			},
		},
	}
}

// hasLostLinkFault reports whether a lost daemon-to-shim link is already on
// the record, which is what makes the liveness probe's own observation
// redundant rather than a second voice on the same condition.
func hasLostLinkFault(open []wsm.Fault) bool {
	for _, f := range open {
		if f.Kind == KindShimDied || f.Kind == KindLinkSevered {
			return true
		}
	}
	return false
}
