package rollout

import (
	"context"
	"errors"
	"fmt"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// FaultRelaunchFailed is the fault kind a relaunch that could not resume
// records. Its remediation is as-it-comes-up, per the ruling.
//
// It is health.KindResumeFailed itself, not a second spelling of it. A relaunch
// whose resume failed IS a resume that failed, and the surfaces render it
// through `SessionFault.resume_failed'; a private spelling reached no arm at
// all and put a HostFault with an unset oneof on the wire.
const FaultRelaunchFailed = health.KindResumeFailed

// BounceShim implements Controller: the ONE shim bounce, for every reason —
// a stale build, the operator's restart verb, a shim log at its hard ceiling.
//
// WHEN IS THE BOUNCE REGISTRY'S DECISION, never this package's: the prompt
// queue owns dispatch, so it decides "free" and "bounce now" as one step and
// drains the workspace while the engine below runs. What this package owns is
// the engine itself (shimBounce).
func (c *controller) BounceShim(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason, force bool, done func(error)) (bounce.Decision, error) {
	fields := dlog.Context{"workspace": string(ws), "reason": string(reason), "force": force}
	decision, err := c.deps.Bounces.RequestBounce(ctx, ws, bounce.Request{
		Reason:       string(reason),
		Force:        force,
		Run:          c.shimBounce(reason, force),
		ReplacesShim: true,
		Done: func(err error) {
			switch bounce.OutcomeOf(err) {
			case bounce.OutcomeUnregistered:
				// AN OUTCOME, NOT A FAILURE: the shim this bounce would have
				// replaced departed, and nothing is left to replace -- this
				// daemon ended the session itself, or the workspace is closed,
				// or a newer shim already runs the installed build.
				c.log.Info(opBounce, "the shim bounce was unregistered: the shim it would replace is gone and nothing is left to replace", fields)
			case bounce.OutcomeHandedAcross:
				// AN OUTCOME, NOT A FAILURE: a dispatch-quiet move carried the
				// replacement to the daemon the workspace moved to, which runs
				// it after its adoption. MEASURED, deploy 2026-09-29T17:15:28:
				// recorded here at ERROR for every busy workspace a deploy
				// handed over.
				c.log.Info(opBounce, "the shim bounce was handed across: the daemon the workspace moved to runs the replacement after its adoption", fields)
			case bounce.OutcomeDeferred:
				// THE REGISTRY KEEPS A DEFERRED BOUNCE'S Done FOR THE RERUN and
				// tells it only that rerun's outcome, so this is the registry
				// breaking its own contract: said loudly, never taken as an end.
				c.log.Error(opBounce, "the bounce registry told a deferral to the shim bounce's completion; it owes only the rerun's outcome", withCause(fields, err))
			case bounce.OutcomeFailed:
				c.log.Error(opBounce, "the shim bounce failed; the workspace is served as it was", withCause(fields, err))
			case bounce.OutcomeFinished:
				c.log.Info(opBounce, "the shim bounce finished; the workspace runs the installed build", fields)
			}
			if done != nil {
				done(err)
			}
		},
	})
	if err != nil {
		c.log.Error(opBounce, "the bounce registry refused the shim bounce", withCause(fields, err))
		return bounce.Decision{}, fmt.Errorf("rollout: bounce the shim of %q: %w", ws, err)
	}
	switch {
	case decision.Now && decision.Forced:
		c.log.Info(opBounce, "bouncing the shim now over its work in flight (forced)", merge(fields, dlog.Context{
			"turn_in_flight": decision.TurnInFlight, "detached_work": decision.DetachedWork,
		}))
	case decision.Now:
		c.log.Info(opBounce, "bouncing the shim now; nothing was in flight", fields)
	default:
		c.log.Info(opBounce, "registered the shim bounce behind the workspace's work in flight", merge(fields, dlog.Context{
			"turn_in_flight": decision.TurnInFlight, "detached_work": decision.DetachedWork,
			"already_registered": decision.AlreadyPending,
		}))
	}
	return decision, nil
}

// shimBounce is the bounce engine, run by the registry once it has DRAINED the
// workspace. Every step is load-bearing:
//
//  1. PRELAUNCH the new shim, INERT BY CONSTRUCTION — no session started, so no
//     vendor process, no store writes, no keep-alives. A prelaunch that fails
//     leaves the old shim untouched and serving.
//  2. TAKE THE RESTART-PENDING HOLD, which is what the tray and the composer's
//     `restarting` arm draw.
//  3. STAND THE OLD SHIM DOWN — gracefully, or FORCED (which ends its turn and
//     every detached item in its vendor child) — force-killing only when a
//     stand-down the shim ACCEPTED (or a forced one) outlives the window, and
//     saying so loudly. AN UNFORCED REPLACEMENT NEVER ENDS LIVE WORK: a shim
//     that refuses the graceful stand-down as `live`, or never answers it and
//     does not leave, keeps serving untouched; the prelaunch is retired, the
//     hold released, and the run answers bounce.ErrDeferred, on which the
//     registry re-registers the bounce behind that work.
//  4. THE REAP IS THE GATE: the old process is confirmed GONE before anything
//     else, which is the guarantee that at most one vendor binary ever touches
//     the session's transcript.
//  5. GREEDY REATTACH: StartSession(resume) at once, the cold gate only on a
//     genuinely lapsed TTL.
//  6. RELEASE THE HOLD. The registry's finish then delivers what was queued.
//
// It deliberately does NOT use Hibernate: hibernation is the IDLE SWEEP's
// directive and compacts the transcript, which is exactly wrong for a bounce
// that means to resume the same context moments later.
func (c *controller) shimBounce(reason RelaunchReason, force bool) bounce.Func {
	return func(ctx context.Context, ws ids.WorkspaceID) error {
		fields := dlog.Context{"workspace": string(ws), "reason": string(reason), "force": force}

		old, hasOld := c.deps.Shims.Client(ws)
		var lease *wsm.Lease
		// THE HOLD'S LIFETIME IS THIS BOUNCE'S SCOPE: it is released on every
		// way out of here, success and each failure alike, and on a context the
		// caller's cancellation cannot reach -- a stand-down that ended with its
		// context used to release through that same cancelled context, the
		// write was refused, and the restart hold outlived the bounce.
		defer func() {
			if lease != nil {
				c.release(context.WithoutCancel(ctx), ws, lease.ID, fields)
			}
		}()
		for {
			fresh, resumed, err := c.relaunchOnce(ctx, ws, reason, force, old, hasOld, &lease, fields)
			if errors.Is(err, ErrResumeRestarted) {
				// A RESTART ENDED THE RESUME'S VENDOR-START RUN. The restart
				// wants this workspace relaunched at once, so the shim this
				// bounce just installed is stood down FORCED and relaunched
				// over, with a fresh run; the restart's own request joined this
				// bounce and hears its outcome.
				c.log.Info(opRelaunch, "a restart ended the resume's vendor-start run; relaunching over the installed shim", fields)
				old, hasOld, force = fresh, true, true
				fields["force"] = true
				continue
			}
			if err != nil {
				return err
			}
			return c.settleRelaunch(ctx, ws, fresh, resumed, fields)
		}
	}
}

// relaunchOnce is one pass of the bounce engine: prelaunch, the
// restart-pending hold (taken once per bounce, into *lease), the stand-down
// and reap of `old`, the install and the resume. It answers the installed
// shim and the resume's outcome.
func (c *controller) relaunchOnce(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason, force bool, old shimclient.Client, hasOld bool, lease **wsm.Lease, fields dlog.Context) (shimclient.Client, Resumed, error) {
	// THE PRELAUNCH COEXISTS WITH THE LIVE SHIM. An inert shim holds NEITHER
	// kernel lock -- by ruling the shim takes both inside StartSession, not
	// at startup -- so a second process for one workspace comes up beside
	// the first, costing nothing until the swap.
	fresh, err := c.deps.Shims.Prelaunch(ctx, ws)
	if err != nil {
		c.log.Error(opRelaunch, "the inert prelaunch failed; the old shim is untouched", withCause(fields, err))
		return nil, Resumed{}, fmt.Errorf("rollout: relaunch %q: prelaunch: %w", ws, err)
	}
	c.log.Debug(opRelaunch, "prelaunched an inert shim beside the running one", fields)

	if *lease == nil {
		taken, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderRestart, wsm.PolicyHold)
		if err != nil {
			c.log.Error(opRelaunch, "could not take the restart-pending hold", withCause(fields, err))
			c.retirePrelaunch(ctx, fresh, reason, fields)
			return nil, Resumed{}, fmt.Errorf("rollout: relaunch %q: take the restart hold: %w", ws, err)
		}
		*lease = &taken
		fields["lease"] = string(taken.ID)
		c.deps.LeaseChanged(ws)
		c.publishHost(ws)
		c.log.Debug(opRelaunch, "took the restart-pending hold; the tray draws it now", fields)
	}

	if hasOld {
		// THE STAND-DOWN IS THE POINT OF NO RETURN, so a replacement that
		// is already dead stops the bounce here, with the old shim still
		// serving.
		if err := c.replacementAlive(fresh, ws, "before the old shim was stood down; the old shim keeps serving", fields); err != nil {
			return nil, Resumed{}, err
		}
		if err := c.standDown(ctx, old, ws, reason, force, fields); err != nil {
			c.retirePrelaunch(ctx, fresh, reason, fields)
			if bounce.OutcomeOf(err) == bounce.OutcomeDeferred {
				c.log.Info(opRelaunch, "the bounce is deferred: the old shim keeps serving untouched, and the registry takes the bounce again at the workspace's next freeness", fields)
			}
			return nil, Resumed{}, err
		}
	} else {
		c.log.Debug(opRelaunch, "the workspace had no running shim to stand down", fields)
	}
	// THE OLD SHIM'S REPORTED BUILD LEAVES WITH IT. The reap gate has
	// passed, so whatever build is on record was the stood-down shim's;
	// the replacement's own report is the one judged, whenever it arrives.
	c.forgetReported(ws)

	// A REPLACEMENT THAT DIED DURING THE STAND-DOWN IS REPLACED, never
	// installed: the old shim is gone, so the workspace is otherwise left
	// linked to nothing. One more prelaunch, and a refusal of that one is
	// the bounce's failure; the workspace then has no live client, which
	// sends its next prompt down the revival path.
	if err := c.replacementAlive(fresh, ws, "while the old shim stood down; prelaunching another", fields); err != nil {
		fresh, err = c.deps.Shims.Prelaunch(ctx, ws)
		if err != nil {
			c.log.Error(opRelaunch, "the second prelaunch failed; the workspace has no shim until it is revived", withCause(fields, err))
			return nil, Resumed{}, fmt.Errorf("rollout: relaunch %q: prelaunch after the replacement died: %w", ws, err)
		}
	}

	// THE REAP HAS PASSED, so both of the old shim's kernel locks are free
	// and the prelaunched one takes them at its StartSession.
	if err := c.deps.Shims.Install(ctx, ws, fresh); err != nil {
		c.log.Error(opRelaunch, "could not install the prelaunched shim", withCause(fields, err))
		c.retirePrelaunch(ctx, fresh, reason, fields)
		return nil, Resumed{}, fmt.Errorf("rollout: relaunch %q: install the new shim: %w", ws, err)
	}

	resumed, err := c.deps.Shims.Resume(ctx, ws, fresh)
	if errors.Is(err, ErrResumeRestarted) {
		return fresh, Resumed{}, err
	}
	if err != nil {
		c.recordRelaunchFault(ctx, ws, err, fields)
		return nil, Resumed{}, fmt.Errorf("rollout: relaunch %q: resume: %w", ws, err)
	}
	return fresh, resumed, nil
}

// settleRelaunch finishes a bounce whose resume answered: the cold gate when
// the resume answered cold, the new shim's pid, and the recovery edges.
func (c *controller) settleRelaunch(ctx context.Context, ws ids.WorkspaceID, fresh shimclient.Client, resumed Resumed, fields dlog.Context) error {
	if resumed.Cold != nil {
		// THE ORDINARY COLD GATE. A fast swap stays warm because the context
		// cache is server-side, so this fires only on a genuinely lapsed TTL.
		c.log.Info(opRelaunch, "the resume answered cold; raising the ordinary cold gate", fields)
		if err := c.deps.ColdGate(ctx, ws, resumed.Cold); err != nil {
			c.log.Error(opRelaunch, "could not raise the cold gate", withCause(fields, err))
			return fmt.Errorf("rollout: relaunch %q: cold gate: %w", ws, err)
		}
	}

	if pid := fresh.PID(); pid > 0 {
		if err := c.deps.DB.SetShimPID(ctx, ws, &pid); err != nil {
			c.log.Warn(opRelaunch, "could not record the new shim's pid", withCause(fields, err))
		}
	}

	// THE INSTALLED REPLACEMENT IS A HEALTHY ATTACH, and a warm resume a
	// started session: both are recovery edges (health/lifetime.go). A
	// cold answer started nothing yet; the gate's re-open is its start.
	workspace := ws
	scope := health.EdgeScope{Workspace: &workspace}
	health.CloseOnEdge(ctx, c.deps.DB, c.log.With(fields), health.EdgeHealthyAttach, scope, c.deps.Clock.Now())
	if resumed.Cold == nil {
		health.CloseOnEdge(ctx, c.deps.DB, c.log.With(fields), health.EdgeSessionStarted, scope, c.deps.Clock.Now())
	}

	c.log.Info(opRelaunch, "relaunched the workspace's shim", fields)
	return nil
}

// replacementAlive answers an error when the prelaunched replacement has
// already exited, recording it at ERROR with `when`.
func (c *controller) replacementAlive(fresh shimclient.Client, ws ids.WorkspaceID, when string, fields dlog.Context) error {
	info, dead := fresh.Reaped()
	if !dead {
		return nil
	}
	c.log.Error(opRelaunch, "the prelaunched replacement shim died "+when, merge(fields, dlog.Context{
		"pid": info.PID, "exit_code": info.Code, "signal": info.Signal,
	}))
	return fmt.Errorf("rollout: relaunch %q: the prelaunched replacement (pid %d) exited with code %d", ws, info.PID, info.Code)
}

// retirePrelaunch stops an inert prelaunched shim a failed or deferred bounce
// will never install, so it does not leave an orphan process holding a socket.
// It holds no session and no lock, so the kill costs nothing.
func (c *controller) retirePrelaunch(ctx context.Context, fresh shimclient.Client, reason RelaunchReason, fields dlog.Context) {
	if err := fresh.Kill(ctx, shimclient.KillAttribution{
		Actor:  "rollout.relaunch",
		Reason: fmt.Sprintf("a %s bounce failed or was deferred before the prelaunched shim was installed", reason),
		Force:  true,
	}); err != nil {
		c.log.Error(opRelaunch, "could not stop the prelaunched shim the failed bounce leaves behind", withCause(fields, err))
		return
	}
	c.log.Info(opRelaunch, "stopped the prelaunched shim the failed or deferred bounce will not install", fields)
}

// publishHost republishes the workspace's host view when a surface is wired.
// The restart-pending hold IS the composer's `restarting` arm.
func (c *controller) publishHost(ws ids.WorkspaceID) {
	if c.deps.PublishHost == nil {
		return
	}
	c.deps.PublishHost(ws)
}

// ErrStandDownLive is the run's answer when the shim REFUSED an unforced
// stand-down as `live`: the registry judged the workspace free, but the vendor
// started a turn on its own (a concluding subagent's notification wakes the
// main agent) between that judgement and the stand-down. The shim's refusal is
// atomic and authoritative about its own liveness, so it wins: nothing is
// forced, and the bounce is deferred (bounce.ErrDeferred).
//
// Regression, 2026-10-01 (footer-activity-updates): the refusal was waited out
// for the 30s window and the shim force-killed, ending the running turn and a
// detached subagent resumed inside the window.
var ErrStandDownLive = fmt.Errorf("rollout: the shim refused the unforced stand-down because work is live in it: %w", bounce.ErrDeferred)

// ErrStandDownUnanswered is the run's answer when an unforced stand-down call
// FAILED and the shim did not leave inside the stand-down window. A shim that
// never answered never vouched that nothing is live in it, so it is not forced
// either: the bounce is deferred (bounce.ErrDeferred), at ERROR.
var ErrStandDownUnanswered = fmt.Errorf("rollout: the shim did not answer the unforced stand-down and did not leave: %w", bounce.ErrDeferred)

// standDown ends the old shim and PASSES THE REAP GATE. A FORCED stand-down
// asks the shim to end everything live at once; an unforced one asks it to end
// a session with nothing live (the registry decided that it is free).
//
// AN UNFORCED STAND-DOWN NEVER ENDS LIVE WORK. The shim's `live` refusal is
// answered at once with ErrStandDownLive (no window, no kill); a call that
// failed waits the window for the shim to leave -- the evidence that it did
// take the stand-down and only its answer was lost -- and answers
// ErrStandDownUnanswered if it did not, never killing it. A stand-down the shim
// ACCEPTED (or a forced one, or another refusal) whose process outlives the
// window is a hung shim, not live work: it is force-killed and logged LOUDLY,
// and the stream-only residue of the window is lost, which is accepted rather
// than an invariant.
func (c *controller) standDown(ctx context.Context, old shimclient.Client, ws ids.WorkspaceID, reason RelaunchReason, force bool, fields dlog.Context) error {
	exited := old.Exited()

	answer, err := old.KillSession(ctx, &shimv1.KillSessionRequest{Force: force})
	switch {
	case err != nil && !force:
		return c.awaitUnansweredStandDown(ctx, exited, ws, err, fields)
	case answer.GetFailure().GetLive() != nil && !force:
		live := answer.GetFailure().GetLive()
		c.log.Info(opRelaunch, "the shim refused the unforced stand-down: work is live in it. The replacement is never forced over it; it is deferred behind that work",
			merge(fields, dlog.Context{
				"refusal":        "live",
				"turn_in_flight": live.GetTurnInFlight() != nil,
				"live_work":      len(live.GetLiveWork()),
				"detail":         answer.GetFailure().GetDetail(),
			}))
		return fmt.Errorf("rollout: relaunch %q: stand down: %w", ws, ErrStandDownLive)
	case err != nil:
		c.log.Warn(opRelaunch, "the stand-down call failed; waiting out the window before forcing",
			withCause(fields, err))
	case answer.GetFailure().GetNoSession() != nil:
		// A SHIM HOLDING NO SESSION HAS NOTHING TO END: its vendor never
		// started (a refused or retried start, a restart of a stuck bring-up),
		// so no transcript or turn is at stake. It does not leave on its own
		// after this refusal, so waiting the window out would only delay the
		// restart; it is stopped at once.
		c.log.Info(opRelaunch, "the old shim holds no session; stopping it at once", fields)
		return c.killAndReap(ctx, old, exited, ws, reason, "the old shim held no session to stand down", fields)
	case answer.GetFailure() != nil:
		c.log.Warn(opRelaunch, "the shim refused the stand-down; waiting out the window before forcing",
			merge(fields, dlog.Context{"refusal": killRefusal(answer.GetFailure())}))
	default:
		c.log.Debug(opRelaunch, "the shim accepted the stand-down", fields)
	}

	select {
	case info := <-exited:
		c.log.Debug(opRelaunch, "the old shim is reaped; the gate is passed",
			merge(fields, dlog.Context{"pid": info.PID, "exit_code": info.Code, "signal": info.Signal}))
		return nil
	case <-c.deps.Clock.After(c.deps.StandDownWindow):
	case <-ctx.Done():
		c.log.Error(opRelaunch, "the stand-down ended with its context", withCause(fields, ctx.Err()))
		return fmt.Errorf("rollout: relaunch %q: stand down: %w", ws, ctx.Err())
	}

	c.log.Error(opRelaunch, "the old shim did not exit inside the stand-down window; force-killing it. "+
		"The stream-only residue of the window is lost",
		merge(fields, dlog.Context{"stand_down_window": c.deps.StandDownWindow.String()}))
	if err := c.killAndReap(ctx, old, exited, ws, reason, "the stand-down window expired", fields); err != nil {
		return err
	}
	c.log.Warn(opRelaunch, "the force-killed shim is reaped; the gate is passed", fields)
	return nil
}

// killAndReap force-kills the old shim and waits for the reap gate: the
// kernel's exit event, so at most one vendor binary ever touches the
// session's transcript.
func (c *controller) killAndReap(ctx context.Context, old shimclient.Client, exited <-chan shimclient.ExitInfo, ws ids.WorkspaceID, reason RelaunchReason, why string, fields dlog.Context) error {
	if err := old.Kill(ctx, shimclient.KillAttribution{
		Actor:  "rollout.relaunch",
		Reason: fmt.Sprintf("%s during a %s relaunch", why, reason),
		Force:  true,
	}); err != nil {
		c.log.Error(opRelaunch, "the force-kill failed", withCause(fields, err))
		return fmt.Errorf("rollout: relaunch %q: force-kill the old shim: %w", ws, err)
	}
	select {
	case info := <-exited:
		c.log.Debug(opRelaunch, "the killed shim is reaped; the gate is passed",
			merge(fields, dlog.Context{"pid": info.PID, "exit_code": info.Code, "signal": info.Signal}))
		return nil
	case <-ctx.Done():
		c.log.Error(opRelaunch, "the reap ended with its context", withCause(fields, ctx.Err()))
		return fmt.Errorf("rollout: relaunch %q: reap the old shim: %w", ws, ctx.Err())
	}
}

// awaitUnansweredStandDown decides an UNFORCED stand-down whose call failed.
// The shim may have taken it and lost only the answer (it exits after
// answering), so the window is waited out for its exit, which passes the reap
// gate. A shim still running at the window's end never vouched that nothing
// is live in it, so it is left serving untouched and the bounce is deferred,
// at ERROR: the shim did not answer.
func (c *controller) awaitUnansweredStandDown(ctx context.Context, exited <-chan shimclient.ExitInfo, ws ids.WorkspaceID, callErr error, fields dlog.Context) error {
	c.log.Debug(opRelaunch, "the unforced stand-down call failed; waiting out the window for the shim to leave, and never forcing it",
		withCause(fields, callErr))
	select {
	case info := <-exited:
		// THE CALL STILL FAILED, so it is a fault; the shim's exit is the
		// evidence that it took the stand-down, and the gate is passed.
		c.log.Warn(opRelaunch, "the unforced stand-down call failed, but the shim left inside the window; the gate is passed",
			merge(withCause(fields, callErr), dlog.Context{"pid": info.PID, "exit_code": info.Code, "signal": info.Signal}))
		return nil
	case <-c.deps.Clock.After(c.deps.StandDownWindow):
	case <-ctx.Done():
		c.log.Error(opRelaunch, "the stand-down ended with its context", withCause(fields, ctx.Err()))
		return fmt.Errorf("rollout: relaunch %q: stand down: %w", ws, ctx.Err())
	}
	c.log.Error(opRelaunch, "the shim did not answer the unforced stand-down and did not leave inside the window; it is left serving untouched and the replacement is deferred, never forced",
		merge(withCause(fields, callErr), dlog.Context{"stand_down_window": c.deps.StandDownWindow.String()}))
	return fmt.Errorf("rollout: relaunch %q: stand down: %w: %w", ws, ErrStandDownUnanswered, callErr)
}

// killRefusal names the shim's stand-down refusal arm for a record.
func killRefusal(failure *shimv1.KillSessionFailure) string {
	switch {
	case failure.GetLive() != nil:
		return "live"
	case failure.GetNoSession() != nil:
		return "no_session"
	case failure.GetQueryRefusedToEnd() != nil:
		return "query_refused_to_end"
	default:
		return "unspecified"
	}
}

// release drops the restart-pending hold and TELLS THE QUEUE, which is what
// un-stamps the held intake: the release alone changes a row the queue is not
// watching, so a bounce without this leaves the intake held forever.
func (c *controller) release(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, fields dlog.Context) {
	if err := c.deps.DB.ReleaseLease(ctx, lease); err != nil {
		c.log.Error(opRelaunch, "could not release the restart-pending hold", withCause(fields, err))
		return
	}
	c.deps.LeaseChanged(ws)
	c.publishHost(ws)
	c.log.Debug(opRelaunch, "released the restart-pending hold", fields)
}

// recordRelaunchFault records a resume that failed hard as the WORKSPACE'S OWN
// error. It is remediated as it comes up: there is no retry machinery.
func (c *controller) recordRelaunchFault(ctx context.Context, ws ids.WorkspaceID, cause error, fields dlog.Context) {
	c.log.Error(opRelaunch, "the resume failed; the workspace carries its own error", withCause(fields, cause))
	workspace := ws
	if _, err := c.deps.DB.OpenFault(ctx, wsm.Fault{
		Workspace: &workspace,
		Kind:      FaultRelaunchFailed,
		Detail:    "the relaunched shim could not resume the session",
		Evidence:  map[string]string{"cause": cause.Error()},
		OpenedAt:  c.deps.Clock.Now(),
	}); err != nil {
		c.log.Error(opRelaunch, "could not record the failed relaunch", withCause(fields, err))
	}
}

// ShimReported implements Controller. The judgement runs OFF the caller's
// goroutine — the caller is a stream router holding its own lock, and the
// registry reads that same router under the queue's — and is joinable through
// staleChecks.
func (c *controller) ShimReported(ws ids.WorkspaceID, build string) {
	c.mu.Lock()
	before, known := c.reported[ws]
	c.reported[ws] = build
	c.mu.Unlock()
	if known && before == build {
		return
	}
	c.logTransition(opStaleness, ws, "reported_shim_build", before, build, nil)
	if build == "" {
		// A SHIM THAT REPORTS NO BUILD CANNOT BE PROVEN CURRENT. The field is
		// required on every diagnostics frame, so this is a shim from before
		// the contract said so — exactly the shim a deploy must not leave
		// alone. It is said out loud, and judged stale below.
		c.log.Error(opStaleness, "the shim reported no build; it cannot be proven current, so it is judged stale",
			dlog.Context{"workspace": string(ws)})
	}
	c.judgeStaleAsync(ws)
}

// judgeStaleAsync runs the staleness judgement off the caller's goroutine,
// joinable through staleChecks.
func (c *controller) judgeStaleAsync(ws ids.WorkspaceID) {
	c.staleChecks.Add(1)
	go func() {
		defer c.staleChecks.Done()
		ctx := c.lifetime(context.Background())
		if _, err := c.checkStale(ctx, ws, c.takeoverForce(ws)); err != nil {
			c.log.Error(opStaleness, "the reported shim build could not be judged", withCause(dlog.Context{"workspace": string(ws)}, err))
		}
	}()
}

// forgetReported drops a workspace's reported shim build once the shim that
// reported it is gone, so the next report is judged as the new shim's.
func (c *controller) forgetReported(ws ids.WorkspaceID) {
	c.mu.Lock()
	before, known := c.reported[ws]
	delete(c.reported, ws)
	c.mu.Unlock()
	if known {
		c.logTransition(opStaleness, ws, "reported_shim_build", before, "", dlog.Context{"cause": "the reporting shim was stood down"})
	}
}

// takeoverForce reports whether a stale shim in this workspace is bounced at
// once because the handover that brought it here was forced.
func (c *controller) takeoverForce(ws ids.WorkspaceID) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.forcedTakeover && c.joining[ws]
}

// CheckStaleness implements Controller.
func (c *controller) CheckStaleness(ctx context.Context, ws ids.WorkspaceID, force bool) (StaleCheck, error) {
	return c.checkStale(ctx, ws, force)
}

// checkStale judges a workspace's last reported shim build against the
// installed bundle and bounces a stale shim through the registry.
//
// ONLY A WORKSPACE THIS DAEMON SERVES IS BOUNCED. A joining successor learns a
// shim's build the moment it attaches — before its adoption has finished — and
// a bounce started then would race the adoption for the shim; the adoption's
// own end re-runs this check.
func (c *controller) checkStale(ctx context.Context, ws ids.WorkspaceID, force bool) (StaleCheck, error) {
	fields := dlog.Context{"workspace": string(ws), "force": force}
	if standing := c.Standing(ws); standing != StandingOwned {
		c.log.Debug(opStaleness, "the workspace is not this daemon's to bounce yet; its adoption re-checks it",
			merge(fields, dlog.Context{"standing": standing.String()}))
		return StaleCheck{}, nil
	}
	if _, live := c.deps.Shims.Client(ws); !live {
		c.log.Debug(opStaleness, "the workspace has no live shim; its next spawn runs the installed build", fields)
		return StaleCheck{}, nil
	}
	c.mu.Lock()
	reported, known := c.reported[ws]
	c.mu.Unlock()
	if !known {
		c.log.Debug(opStaleness, "the live shim has not reported its build yet; its report is judged when it arrives", fields)
		return StaleCheck{}, nil
	}
	installed, err := c.deps.ShimBuild()
	if err != nil {
		c.log.Error(opStaleness, "could not read the installed shim build; nothing is bounced on a guess", withCause(fields, err))
		return StaleCheck{}, fmt.Errorf("rollout: judge the shim build of %q: %w", ws, err)
	}
	check := StaleCheck{Reported: reported, Installed: installed}
	fields["reported_build"] = reported
	fields["installed_build"] = installed
	if reported != "" && reported == installed {
		c.log.Debug(opStaleness, "the shim runs the installed build", fields)
		return check, nil
	}
	check.Stale = true
	switch c.claimStaleBounce(ws, reported) {
	case staleBounceInFlight:
		// THE REPORT IS FROM THE SHIM BEING REPLACED. A takeover re-judges
		// every adopted shim, and the bounce its adoption started may still be
		// standing that very shim down; the relaunched shim reports its own
		// build once installed, and that report is the one judged.
		check.Skipped = SkippedBounceInFlight
		c.log.Debug(opStaleness, "the workspace's stale-build bounce is registered or running; the relaunched shim is judged when it reports", fields)
		return check, nil
	case staleBounceAlreadyRan:
		// THE BOUNCE FIRES ONCE PER OBSERVED BUILD. A shim that comes back
		// still reporting the build it was bounced for cannot be fixed by
		// bouncing it again, and re-bouncing spawns a process per report
		// forever. The disagreement is stated loudly, and the session is
		// served on the build it has.
		check.Skipped = SkippedAlreadyBounced
		c.log.Error(opStaleness, "the shim still reports the build it was already bounced for; not bouncing it again", fields)
		return check, nil
	}
	c.log.Info(opStaleness, "the shim runs an older build than the installed one; bouncing it", fields)
	// THE BOUNCE'S SETTLE IS PART OF THIS JUDGEMENT, and staleChecks counts
	// it from here until it has settled. The settle re-judges on a fresh
	// counted goroutine; counted only from THERE, the count could fall to zero
	// between this judgement's end and the re-judge's Add, and a join already
	// waiting would race the Add (the WaitGroup contract; -race reported it on
	// three tests).
	c.staleChecks.Add(1)
	decision, err := c.BounceShim(ctx, ws, ReasonBuildStale, force, func(err error) {
		defer c.staleChecks.Done()
		c.settleStaleBounce(ws, err)
	})
	if err != nil {
		// A refused request never calls its Done, so the count is ended here.
		c.settleStaleBounce(ws, err)
		c.staleChecks.Done()
		return check, err
	}
	check.Bounce = decision
	return check, nil
}

// The StaleCheck.Skipped reasons.
const (
	SkippedAlreadyBounced = "already_bounced_for_this_build"
	SkippedBounceInFlight = "stale_bounce_in_flight"
)

// staleClaim is what claimStaleBounce answers.
type staleClaim int

const (
	staleBounceClaimed staleClaim = iota
	staleBounceInFlight
	staleBounceAlreadyRan
)

// claimStaleBounce records that this workspace is being bounced for `reported`
// and answers whether it may be. A bounce still registered or running answers
// staleBounceInFlight; a build already bounced for answers
// staleBounceAlreadyRan, which is what makes the build-staleness bounce fire
// once per build rather than once per report. Both are decided under ONE lock
// with the claim, so two concurrent reports cannot both claim.
func (c *controller) claimStaleBounce(ws ids.WorkspaceID, reported string) staleClaim {
	c.mu.Lock()
	if c.staleInFlight[ws] {
		c.mu.Unlock()
		return staleBounceInFlight
	}
	previous, seen := c.bouncedStamp[ws]
	if seen && previous == reported {
		c.mu.Unlock()
		c.logTransition(opStaleness, ws, "bounced_build_stamp", previous, previous,
			dlog.Context{"changed": false})
		return staleBounceAlreadyRan
	}
	c.bouncedStamp[ws] = reported
	c.staleInFlight[ws] = true
	c.mu.Unlock()
	c.logTransition(opStaleness, ws, "bounced_build_stamp", previous, reported,
		dlog.Context{"changed": true})
	return staleBounceClaimed
}

// settleStaleBounce ends a claimed stale-build bounce, however it ended: from
// here a report is the relaunched shim's own and is judged.
//
// A BOUNCE THAT FINISHED RE-JUDGES AT ONCE. The relaunched shim reports its
// build while the bounce is still resuming it, so that report met the
// in-flight skip and, being unchanged, is not reported again; judging here is
// what makes a relaunched shim still on the old build the ERROR it is. A
// failed or unregistered bounce is not re-judged: its own record says why.
func (c *controller) settleStaleBounce(ws ids.WorkspaceID, err error) {
	c.mu.Lock()
	delete(c.staleInFlight, ws)
	c.mu.Unlock()
	if err == nil {
		c.judgeStaleAsync(ws)
	}
}

// ReloadWebapp pushes the EMPTY reload_webapp arm: Emacs reloads this
// workspace's xwidget against the SAME daemon, and the reloaded page's default
// first-page load is the whole of the recovery. No address rides it, because
// the daemon is not changing — and a deploy that hands the daemon over never
// sends it at all, since the handover's fresh attach pulls the new assets as
// a side effect.
func (c *controller) ReloadWebapp(_ context.Context, ws ids.WorkspaceID) error {
	c.deps.Pusher.PushReloadWebapp(ws)
	c.log.Info(opReloadWebap, "pushed the webapp reload", dlog.Context{"workspace": string(ws)})
	return nil
}
