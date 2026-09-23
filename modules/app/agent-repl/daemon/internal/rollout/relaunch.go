package rollout

import (
	"context"
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
		Reason: string(reason),
		Force:  force,
		Run:    c.shimBounce(reason, force),
		Done: func(err error) {
			if err != nil {
				c.log.Error(opBounce, "the shim bounce failed; the workspace is served as it was", withCause(fields, err))
			} else {
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
//     every detached item in its vendor child) — force-killing only when the
//     stand-down window expires, and saying so loudly.
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

		// THE PRELAUNCH COEXISTS WITH THE LIVE SHIM. An inert shim holds NEITHER
		// kernel lock -- by ruling the shim takes both inside StartSession, not
		// at startup -- so a second process for one workspace comes up beside
		// the first, costing nothing until the swap.
		fresh, err := c.deps.Shims.Prelaunch(ctx, ws)
		if err != nil {
			c.log.Error(opRelaunch, "the inert prelaunch failed; the old shim is untouched", withCause(fields, err))
			return fmt.Errorf("rollout: relaunch %q: prelaunch: %w", ws, err)
		}
		c.log.Debug(opRelaunch, "prelaunched an inert shim beside the running one", fields)

		lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderRestart, wsm.PolicyHold)
		if err != nil {
			c.log.Error(opRelaunch, "could not take the restart-pending hold", withCause(fields, err))
			c.retirePrelaunch(ctx, fresh, reason, fields)
			return fmt.Errorf("rollout: relaunch %q: take the restart hold: %w", ws, err)
		}
		fields["lease"] = string(lease.ID)
		c.deps.LeaseChanged(ws)
		c.publishHost(ws)
		c.log.Debug(opRelaunch, "took the restart-pending hold; the tray draws it now", fields)

		if hasOld {
			if err := c.standDown(ctx, old, ws, reason, force, fields); err != nil {
				c.retirePrelaunch(ctx, fresh, reason, fields)
				c.release(ctx, ws, lease.ID, fields)
				return err
			}
		} else {
			c.log.Debug(opRelaunch, "the workspace had no running shim to stand down", fields)
		}

		// THE REAP HAS PASSED, so both of the old shim's kernel locks are free
		// and the prelaunched one takes them at its StartSession.
		if err := c.deps.Shims.Install(ctx, ws, fresh); err != nil {
			c.log.Error(opRelaunch, "could not install the prelaunched shim", withCause(fields, err))
			c.retirePrelaunch(ctx, fresh, reason, fields)
			c.release(ctx, ws, lease.ID, fields)
			return fmt.Errorf("rollout: relaunch %q: install the new shim: %w", ws, err)
		}

		resumed, err := c.deps.Shims.Resume(ctx, ws, fresh)
		if err != nil {
			c.recordRelaunchFault(ctx, ws, err, fields)
			c.release(ctx, ws, lease.ID, fields)
			return fmt.Errorf("rollout: relaunch %q: resume: %w", ws, err)
		}
		if resumed.Cold != nil {
			// THE ORDINARY COLD GATE. A fast swap stays warm because the context
			// cache is server-side, so this fires only on a genuinely lapsed TTL.
			c.log.Info(opRelaunch, "the resume answered cold; raising the ordinary cold gate", fields)
			if err := c.deps.ColdGate(ctx, ws, resumed.Cold); err != nil {
				c.log.Error(opRelaunch, "could not raise the cold gate", withCause(fields, err))
				c.release(ctx, ws, lease.ID, fields)
				return fmt.Errorf("rollout: relaunch %q: cold gate: %w", ws, err)
			}
		}

		if pid := fresh.PID(); pid > 0 {
			if err := c.deps.DB.SetShimPID(ctx, ws, &pid); err != nil {
				c.log.Warn(opRelaunch, "could not record the new shim's pid", withCause(fields, err))
			}
		}

		c.release(ctx, ws, lease.ID, fields)
		c.log.Info(opRelaunch, "relaunched the workspace's shim", fields)
		return nil
	}
}

// retirePrelaunch stops an inert prelaunched shim a failed bounce will never
// install, so the failure does not leave an orphan process holding a socket.
// It holds no session and no lock, so the kill costs nothing.
func (c *controller) retirePrelaunch(ctx context.Context, fresh shimclient.Client, reason RelaunchReason, fields dlog.Context) {
	if err := fresh.Kill(ctx, shimclient.KillAttribution{
		Actor:  "rollout.relaunch",
		Reason: fmt.Sprintf("a %s bounce failed before the prelaunched shim was installed", reason),
		Force:  true,
	}); err != nil {
		c.log.Error(opRelaunch, "could not stop the prelaunched shim the failed bounce leaves behind", withCause(fields, err))
		return
	}
	c.log.Info(opRelaunch, "stopped the prelaunched shim the failed bounce will not install", fields)
}

// publishHost republishes the workspace's host view when a surface is wired.
// The restart-pending hold IS the composer's `restarting` arm.
func (c *controller) publishHost(ws ids.WorkspaceID) {
	if c.deps.PublishHost == nil {
		return
	}
	c.deps.PublishHost(ws)
}

// standDown ends the old shim and PASSES THE REAP GATE. A FORCED stand-down
// asks the shim to end everything live at once; an unforced one asks it to end
// a session with nothing live (the registry decided that it is free). A
// stand-down window that expires is force-killed and logged LOUDLY: the
// stream-only residue of the window is lost, which is accepted rather than an
// invariant.
func (c *controller) standDown(ctx context.Context, old shimclient.Client, ws ids.WorkspaceID, reason RelaunchReason, force bool, fields dlog.Context) error {
	exited := old.Exited()

	answer, err := old.KillSession(ctx, &shimv1.KillSessionRequest{Force: force})
	switch {
	case err != nil:
		c.log.Warn(opRelaunch, "the stand-down call failed; waiting out the window before forcing",
			withCause(fields, err))
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
	if err := old.Kill(ctx, shimclient.KillAttribution{
		Actor:  "rollout.relaunch",
		Reason: fmt.Sprintf("the stand-down window expired during a %s relaunch", reason),
		Force:  true,
	}); err != nil {
		c.log.Error(opRelaunch, "the force-kill failed", withCause(fields, err))
		return fmt.Errorf("rollout: relaunch %q: force-kill the old shim: %w", ws, err)
	}
	select {
	case info := <-exited:
		c.log.Warn(opRelaunch, "the force-killed shim is reaped; the gate is passed",
			merge(fields, dlog.Context{"pid": info.PID, "exit_code": info.Code, "signal": info.Signal}))
		return nil
	case <-ctx.Done():
		c.log.Error(opRelaunch, "the reap ended with its context", withCause(fields, ctx.Err()))
		return fmt.Errorf("rollout: relaunch %q: reap the old shim: %w", ws, ctx.Err())
	}
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
	c.staleChecks.Add(1)
	go func() {
		defer c.staleChecks.Done()
		ctx := c.lifetime(context.Background())
		if _, err := c.checkStale(ctx, ws, c.takeoverForce(ws)); err != nil {
			c.log.Error(opStaleness, "the reported shim build could not be judged", withCause(dlog.Context{"workspace": string(ws)}, err))
		}
	}()
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
	if !c.claimStaleBounce(ws, reported) {
		// THE BOUNCE FIRES ONCE PER OBSERVED BUILD. A shim that comes back
		// still reporting the build it was bounced for cannot be fixed by
		// bouncing it again, and re-bouncing spawns a process per report
		// forever. The disagreement is stated loudly, and the session is
		// served on the build it has.
		check.Skipped = "already_bounced_for_this_build"
		c.log.Error(opStaleness, "the shim still reports the build it was already bounced for; not bouncing it again", fields)
		return check, nil
	}
	c.log.Info(opStaleness, "the shim runs an older build than the installed one; bouncing it", fields)
	decision, err := c.BounceShim(ctx, ws, ReasonBuildStale, force, nil)
	if err != nil {
		return check, err
	}
	check.Bounce = decision
	return check, nil
}

// claimStaleBounce records that this workspace is being bounced for `reported`
// and answers whether that build is NEW. A build already bounced for answers
// false, which is what makes the build-staleness bounce fire once per build
// rather than once per report.
func (c *controller) claimStaleBounce(ws ids.WorkspaceID, reported string) bool {
	c.mu.Lock()
	previous, seen := c.bouncedStamp[ws]
	if seen && previous == reported {
		c.mu.Unlock()
		c.logTransition(opStaleness, ws, "bounced_build_stamp", previous, previous,
			dlog.Context{"changed": false})
		return false
	}
	c.bouncedStamp[ws] = reported
	c.mu.Unlock()
	c.logTransition(opStaleness, ws, "bounced_build_stamp", previous, reported,
		dlog.Context{"changed": true})
	return true
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
