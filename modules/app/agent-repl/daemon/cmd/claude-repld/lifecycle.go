package main

import (
	"context"
	"errors"
	"strconv"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// THE LIFECYCLE SINK.
//
// The session watcher routes three facts at the daemon's own machinery rather
// than at a view, and they belong to two different components: the prompt
// queue drains on a turn's end, and the workspace verbs raise a host
// notification. The sink is the two of them side by side, and it is here
// because it is nothing but the two of them side by side.
type lifecycleSink struct {
	queue  promptqueue.Queue
	verbs  *verbsForwarder
	relay  *relayForwarder
	health *healthForwarder
	// builds is the rollout's staleness judge: every diagnostics frame carries
	// the build its shim runs.
	builds *rolloutForwarder
	log    dlog.Logger
}

// OnLinkChanged republishes the workspace's HOST view: `shim_attached` is part
// of it, and the server cannot see a link edge for itself.
func (s *lifecycleSink) OnLinkChanged(ws ids.WorkspaceID, attached bool) {
	s.log.Debug("daemon.cmd.lifecycle", "the shim link's attachment changed", dlog.Context{
		"workspace": string(ws), "attached": attached,
	})
	s.relay.PublishHostWorkspace(ws)
}

// OnLinkFault records a LOST shim link as the session's own fault, which is
// what SessionHealth answers with. The liveness probe cannot do this job: its
// two booleans carry no exit code, and a shim that redialed back into place
// would erase the evidence that it ever died.
//
// At most ONE fault of a kind stands per workspace: a redial ladder severing
// the same link a dozen times is one condition, not a dozen. The faults are
// closed on the next healthy attach (internal/workspace/sessions.go).
func (s *lifecycleSink) OnLinkFault(ws ids.WorkspaceID, fault sessionwatcher.LinkFault) {
	reporter, ok := s.health.reporter()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a link fault arrived before the health reporter existed", dlog.Context{
			"workspace": string(ws), "kind": string(fault.Kind),
		})
		return
	}
	kind := health.KindLinkSevered
	if fault.Kind == sessionwatcher.LinkFaultDead {
		kind = health.KindShimDied
	}
	ctx := context.Background()
	scoped := wsm.WorkspaceID(ws)
	open, err := reporter.OpenFaults(ctx, wsm.FaultScope{Workspace: &scoped, Kind: kind})
	if err != nil {
		s.log.Error("daemon.cmd.lifecycle", "the standing link faults could not be read", dlog.Context{
			"workspace": string(ws), "kind": kind, "cause": err.Error(),
		})
		return
	}
	if len(open) > 0 {
		s.log.Debug("daemon.cmd.lifecycle", "a link fault of this kind already stands", dlog.Context{
			"workspace": string(ws), "kind": kind,
		})
		return
	}
	record := wsm.Fault{
		Workspace: &scoped,
		Kind:      kind,
		Detail:    fault.Detail,
		Evidence:  map[string]string{},
	}
	if fault.ExitCode != nil {
		record.Evidence["exit_code"] = strconv.FormatInt(int64(*fault.ExitCode), 10)
	}
	if _, err := reporter.OpenFault(ctx, record); err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			// The workspace was forgotten while its shim was still dying. There
			// is no surface left to carry the fault and nothing to remediate.
			s.log.Debug("daemon.cmd.lifecycle", "the link fault names a workspace that is no longer registered",
				dlog.Context{"workspace": string(ws), "kind": kind})
			return
		}
		s.log.Error("daemon.cmd.lifecycle", "the link fault could not be recorded", dlog.Context{
			"workspace": string(ws), "kind": kind, "cause": err.Error(),
		})
		return
	}
	// DEATH OUTRANKS SEVERING, exactly as the watcher's own link rule has it:
	// every standing stream breaks of the same cause the process died of, so
	// the severing recorded moments earlier is a CONSEQUENCE of the death and
	// not a second condition. It is retracted only AFTER the stronger record
	// stands, so no read ever finds the session with neither.
	if kind == health.KindShimDied {
		health.CloseOnEdge(ctx, health.ReporterFaults(reporter), s.log, health.EdgeShimDeathRecorded,
			health.EdgeScope{Workspace: &ws}, time.Now())
	}
}

// OnWatchOpenRefused records a watch open the shim refused for a handle
// nothing announced. It is a SESSION fault of its own kind, never a severed
// link: the shim answered the open, so the hop is serving and a redial would
// change nothing. At most one stands per workspace, as every session fault
// does.
func (s *lifecycleSink) OnWatchOpenRefused(ws ids.WorkspaceID, refusal sessionwatcher.WatchOpenRefusal) {
	record := wsm.Fault{
		Kind:   health.KindWatchOpenRefused,
		Detail: refusal.Detail,
		Evidence: map[string]string{
			"operation": refusal.Operation,
			"handle":    refusal.Handle,
		},
	}
	s.recordSessionFault(ws, record)
}

// recordSessionFault opens one workspace-scoped fault unless one of its kind
// already stands. Every read and write failure is surfaced rather than
// swallowed.
func (s *lifecycleSink) recordSessionFault(ws ids.WorkspaceID, record wsm.Fault) {
	reporter, ok := s.health.reporter()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a session fault arrived before the health reporter existed", dlog.Context{
			"workspace": string(ws), "kind": record.Kind,
		})
		return
	}
	ctx := context.Background()
	scoped := wsm.WorkspaceID(ws)
	record.Workspace = &scoped
	open, err := reporter.OpenFaults(ctx, wsm.FaultScope{Workspace: &scoped, Kind: record.Kind})
	if err != nil {
		s.log.Error("daemon.cmd.lifecycle", "the standing session faults could not be read", dlog.Context{
			"workspace": string(ws), "kind": record.Kind, "cause": err.Error(),
		})
		return
	}
	if len(open) > 0 {
		s.log.Debug("daemon.cmd.lifecycle", "a session fault of this kind already stands", dlog.Context{
			"workspace": string(ws), "kind": record.Kind,
		})
		return
	}
	if _, err := reporter.OpenFault(ctx, record); err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			s.log.Debug("daemon.cmd.lifecycle", "the session fault names a workspace that is no longer registered",
				dlog.Context{"workspace": string(ws), "kind": record.Kind})
			return
		}
		s.log.Error("daemon.cmd.lifecycle", "the session fault could not be recorded", dlog.Context{
			"workspace": string(ws), "kind": record.Kind, "cause": err.Error(),
		})
	}
}

// reportBuild hands one shim's reported build to the rollout, which bounces a
// stale shim through the prompt queue's bounce registry.
func (s *lifecycleSink) reportBuild(ws ids.WorkspaceID, build string) {
	controller, ok := s.builds.controller()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a shim reported its build before the rollout controller existed", dlog.Context{
			"workspace": string(ws), "build": build,
		})
		return
	}
	controller.ShimReported(ws, build)
}

// OnTurnAdopted records, through the queue that owns the turn rows, a turn
// the vendor started on its own.
func (s *lifecycleSink) OnTurnAdopted(ws ids.WorkspaceID, turn ids.TurnID) {
	s.queue.OnTurnAdopted(ws, turn)
}

// OnTurnsEndedUnobserved closes, through the queue that owns the turn rows,
// the turns an adoption found open that the adopted shim no longer runs.
func (s *lifecycleSink) OnTurnsEndedUnobserved(ws ids.WorkspaceID, turns []ids.TurnID) {
	s.queue.OnTurnsEndedUnobserved(ws, turns)
}

// OnFree is the freeness edge: the queue bounces a shim registered for it.
func (s *lifecycleSink) OnFree(ws ids.WorkspaceID) {
	s.queue.OnFree(ws)
}

// OnDeparted is the departure edge: a dead shim's registered bounce is decided
// by the queue at once rather than waiting for a revival.
func (s *lifecycleSink) OnDeparted(ws ids.WorkspaceID, departed sessionwatcher.Watcher, departure sessionwatcher.Departure) {
	s.queue.OnDeparted(ws, departed, departure)
}

// OnTurnEnded pops the queue and releases a hold-for-turn-end.
func (s *lifecycleSink) OnTurnEnded(ws ids.WorkspaceID, turn ids.TurnID, how sessionwatcher.TurnClose) {
	s.queue.OnTurnEnded(ws, turn, how)
}

// OnLiveWorkChanged has no consumer beyond the watcher itself: FREENESS is
// answered inside the watcher, from the same set, and the roster hears the set
// on its own sink. It is recorded rather than dropped silently, so a future
// consumer is added here rather than discovered to be missing.
func (s *lifecycleSink) OnLiveWorkChanged(ws ids.WorkspaceID, live sessionwatcher.LiveWorkSet) {
	s.log.Debug("daemon.cmd.lifecycle", "the live-work set changed", dlog.Context{
		"workspace": string(ws),
		"agents":    len(live.Agents),
		"shells":    len(live.Shells),
		"monitors":  len(live.Monitors),
	})
}

// OnNotification raises the host notification and the roster's attention
// marker through the verbs, which own both.
func (s *lifecycleSink) OnNotification(ws ids.WorkspaceID, note sessionwatcher.HostNotification) {
	verbs, ok := s.verbs.verbs()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a notification arrived before the workspace verbs existed", dlog.Context{
			"workspace": string(ws), "kind": string(note.Kind),
		})
		return
	}
	if err := verbs.Notify(context.Background(), ws, note); err != nil {
		s.log.Error("daemon.cmd.lifecycle", "the host notification could not be raised", dlog.Context{
			"workspace": string(ws), "kind": string(note.Kind), "cause": err.Error(),
		})
	}
}

// OnAsksSettled clears the roster's attention marker through the verbs, which
// own it. It is OnNotification's counterpart: the notification an ask raised is
// retired when the ask it was about is decided.
func (s *lifecycleSink) OnAsksSettled(ws ids.WorkspaceID) {
	verbs, ok := s.verbs.verbs()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "an ask settled before the workspace verbs existed", dlog.Context{
			"workspace": string(ws),
		})
		return
	}
	if err := verbs.AsksSettled(context.Background(), ws); err != nil {
		s.log.Error("daemon.cmd.lifecycle", "the attention marker could not be cleared", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
	}
}

// OnSessionDiagnostics folds the shim's health verdict into the health
// reporter's per-session faults, which is what SessionHealth answers with.
//
// The push is the WHOLE verdict, so the standing shim-reported faults are
// closed first and the pushed ones opened afresh: a healthy verdict is a
// retraction, exactly as the topbar's warning strip treats it.
func (s *lifecycleSink) OnSessionDiagnostics(ws ids.WorkspaceID, diagnostics *conversationv1.SessionDiagnostics) {
	// EVERY FRAME STATES THE SHIM'S BUILD, and the rollout judges it against
	// the installed bundle before anything else here can return early.
	s.reportBuild(ws, diagnostics.GetShimBuild())
	reporter, ok := s.health.reporter()
	if !ok {
		s.log.Error("daemon.cmd.lifecycle", "a diagnostics push arrived before the health reporter existed", dlog.Context{
			"workspace": string(ws),
		})
		return
	}
	ctx := context.Background()
	scoped := wsm.WorkspaceID(ws)
	// THE PUSH IS THE WHOLE VERDICT: it is the recovery edge of every
	// shim-reported fault standing. A standing set that cannot be read is not
	// replaced, or the verdict would be stacked on top of itself.
	closed, err := health.CloseOnEdge(ctx, health.ReporterFaults(reporter), s.log, health.EdgeShimDiagnostics,
		health.EdgeScope{Workspace: &ws}, time.Now())
	if err != nil {
		return
	}

	opened := 0
	unhealthy, isUnhealthy := diagnostics.GetHealth().(*conversationv1.SessionDiagnostics_Unhealthy)
	if isUnhealthy {
		for _, fault := range unhealthy.Unhealthy.GetFaults() {
			if _, err := reporter.OpenFault(ctx, wsm.Fault{
				Workspace: &scoped,
				Kind:      health.KindShimReported,
				Detail:    fault.GetDetail(),
				Evidence: map[string]string{
					"component": fault.GetComponent(),
					"kind":      shimFaultKind(fault),
				},
			}); err != nil {
				s.log.Error("daemon.cmd.lifecycle", "a shim-reported fault could not be recorded", dlog.Context{
					"workspace": string(ws), "component": fault.GetComponent(), "cause": err.Error(),
				})
				continue
			}
			opened++
		}
	}
	s.log.Debug("daemon.cmd.lifecycle", "the shim's health verdict was folded into the session faults", dlog.Context{
		"workspace": string(ws), "unhealthy": isUnhealthy, "opened": opened, "retracted": closed,
	})
}

// shimFaultKind names the SessionFault kind arm the shim set, kept as the
// recorded fault's evidence so SessionFaultShimReported.kind carries the
// shim's own classification rather than a re-derivation of it. The naming
// itself lives in shimclient, which is the one module that reads shim frames,
// so the adoption record and this evidence can never spell an arm differently.
func shimFaultKind(fault *conversationv1.SessionFault) string {
	return shimclient.FaultKind(fault)
}
