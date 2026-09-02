package rollout

import (
	"context"
	"fmt"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// FaultRelaunchFailed is the fault kind a relaunch that could not resume
// records. Its remediation is as-it-comes-up, per the ruling.
const FaultRelaunchFailed = "relaunch_resume_failed"

// RelaunchShim bounces one workspace's shim. It is the ONE engine for a
// self-merge shim change, the build-staleness bounce and the operator's restart
// verb — one path, three reasons.
//
// It deliberately does NOT use Hibernate: hibernation is the IDLE SWEEP's
// directive and compacts the transcript, which is exactly wrong for a bounce
// that means to resume the same context moments later.
//
// The order is the settled flow and every step is load-bearing:
//
//  1. PRELAUNCH the new shim, INERT BY CONSTRUCTION — no session started, so no
//     vendor process, no store writes, no keep-alives — so it coexists with the
//     old one indefinitely.
//  2. WAIT FOR FREENESS, forever if need be. Freeness at the kill is an
//     INVARIANT of the design: nothing is running under the vendor process when
//     it dies, so killing the CLI is inconsequential by construction.
//  3. TAKE THE RESTART-PENDING HOLD, which is what the tray draws.
//  4. STAND THE OLD SHIM DOWN gracefully; force-kill only when the stand-down
//     window expires, and say so loudly.
//  5. THE REAP IS THE GATE: the old process is confirmed GONE before anything
//     else, which is the guarantee that at most one vendor binary ever touches
//     the session's transcript.
//  6. GREEDY REATTACH: StartSession(resume) at once, the cold gate only on a
//     genuinely lapsed TTL.
//  7. RELEASE THE HOLD, which drains the held intake.
func (c *controller) RelaunchShim(ctx context.Context, ws ids.WorkspaceID, reason RelaunchReason) error {
	fields := dlog.Context{"workspace": string(ws), "reason": string(reason)}

	if reason == ReasonBuildStale {
		stale, err := c.stale(ctx, ws, fields)
		if err != nil {
			return err
		}
		if !stale {
			c.log.Debug(opRelaunch, "the shim is on the deployed build; nothing to bounce", fields)
			return nil
		}
	}

	old, hasOld := c.deps.Shims.Client(ws)

	// THE LAUNCH IS SEQUENTIAL, NOT A PRELAUNCH -- an interim order, recorded.
	//
	// THE CONFLICT: this engine was specified to launch the new shim BESIDE
	// the running one and swap at freeness, but the shim takes
	// workspace-<hash>.lock with flock(LOCK_EX) AT STARTUP, so a second
	// process for one workspace blocks in flock and never listens; the
	// daemon's dial ladder then retries forever and the restart never
	// answers. The two ways out are (a) the shim takes the workspace lock
	// inside StartSession, which restores the overlap, or (b) this order:
	// wait for freeness, hold intake, stand the old shim DOWN and reap it,
	// and only then launch. The project lead has ruled (a), and this is (b)
	// until the shim carries it. FLIPPING BACK IS ONE SITE: move the
	// Prelaunch call above awaitFreeForever again and delete this comment.
	// THE HOLD IS TAKEN BEFORE THE WAIT, not after it. The wait is the window
	// the hold exists for: a graceful restart asked for while a turn runs
	// waits out that turn, and every prompt arriving meanwhile would otherwise
	// be delivered to the very shim about to be stood down. Held first, the
	// intake queues under `build_refresh` for the whole wait and drains when
	// the replacement is ready.
	lease, err := c.deps.DB.AcquireLease(ctx, ws, wsm.HolderRestart, wsm.PolicyHold)
	if err != nil {
		c.log.Error(opRelaunch, "could not take the restart-pending hold", withCause(fields, err))
		return fmt.Errorf("rollout: relaunch %q: take the restart hold: %w", ws, err)
	}
	fields["lease"] = string(lease.ID)
	c.log.Debug(opRelaunch, "took the restart-pending hold; the tray draws it now", fields)

	if err := c.awaitFreeForever(ctx, ws, opRelaunch, fields); err != nil {
		c.release(ctx, ws, lease.ID, fields)
		return err
	}

	if hasOld {
		if err := c.standDown(ctx, old, ws, reason, fields); err != nil {
			c.release(ctx, ws, lease.ID, fields)
			return err
		}
	} else {
		c.log.Debug(opRelaunch, "the workspace had no running shim to stand down", fields)
	}

	// THE REAP HAS PASSED, so the workspace lock is free and the new shim can
	// take it. Nothing of the old process survives this line.
	fresh, err := c.deps.Shims.Prelaunch(ctx, ws)
	if err != nil {
		c.release(ctx, ws, lease.ID, fields)
		c.log.Error(opRelaunch, "the replacement shim did not come up", withCause(fields, err))
		return fmt.Errorf("rollout: relaunch %q: launch the replacement: %w", ws, err)
	}
	c.log.Debug(opRelaunch, "launched the replacement shim once the old one was reaped", fields)

	if err := c.deps.Shims.Install(ctx, ws, fresh); err != nil {
		c.release(ctx, ws, lease.ID, fields)
		c.log.Error(opRelaunch, "could not install the prelaunched shim", withCause(fields, err))
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

	// RELEASING THE HOLD IS WHAT DRAINS THE INTAKE: the queue re-evaluates its
	// holds against the lease that is no longer there.
	c.release(ctx, ws, lease.ID, fields)
	c.log.Info(opRelaunch, "relaunched the workspace's shim", fields)
	return nil
}

// standDown ends the old shim and PASSES THE REAP GATE. A stand-down window
// that expires is force-killed and logged LOUDLY: the stream-only residue of
// the window is lost, which is accepted rather than an invariant.
func (c *controller) standDown(ctx context.Context, old shimclient.Client, ws ids.WorkspaceID, reason RelaunchReason, fields dlog.Context) error {
	exited := old.Exited()

	answer, err := old.KillSession(ctx, &shimv1.KillSessionRequest{Force: false})
	switch {
	case err != nil:
		c.log.Warn(opRelaunch, "the graceful stand-down call failed; waiting out the window before forcing",
			withCause(fields, err))
	case answer.GetFailure() != nil:
		c.log.Warn(opRelaunch, "the shim refused the graceful stand-down; waiting out the window before forcing",
			merge(fields, dlog.Context{"refusal": killRefusal(answer.GetFailure())}))
	default:
		c.log.Debug(opRelaunch, "the shim accepted the graceful stand-down", fields)
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
	if err := old.Kill(shimclient.KillAttribution{
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
// lets the held intake drain: the release alone changes a row the queue is not
// watching, so a bounce without this leaves the intake held forever.
func (c *controller) release(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, fields dlog.Context) {
	if err := c.deps.DB.ReleaseLease(ctx, lease); err != nil {
		c.log.Warn(opRelaunch, "could not release the restart-pending hold", withCause(fields, err))
		return
	}
	c.deps.LeaseChanged(ws)
	c.log.Debug(opRelaunch, "released the restart-pending hold; the held intake drains", fields)
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

// CheckStaleness compares a session's reported shim build against the deploy
// stamp and bounces it at freeness when they disagree. It is the second trigger
// of the ONE relaunch engine; the first is a self-merge that landed a shim
// change.
func (c *controller) CheckStaleness(ctx context.Context, ws ids.WorkspaceID, reportedSHA string) error {
	fields := dlog.Context{"workspace": string(ws), "reported_sha": reportedSHA}
	deployed, err := c.deployStamp()
	if err != nil {
		c.log.Warn(opStaleness, "could not read the deploy stamp; leaving the shim alone", withCause(fields, err))
		return nil
	}
	fields["deployed_sha"] = deployed
	if deployed == "" || reportedSHA == "" || deployed == reportedSHA {
		c.log.Debug(opStaleness, "the shim is on the deployed build", fields)
		return nil
	}
	c.log.Info(opStaleness, "the shim is on an older build; bouncing it at freeness", fields)
	return c.RelaunchShim(ctx, ws, ReasonBuildStale)
}

// stale reports whether the workspace's recorded session is on an older build
// than the deploy stamp. A stamp that cannot be read leaves the shim ALONE:
// bouncing a session on a guess is worse than serving it on an older build.
func (c *controller) stale(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) (bool, error) {
	deployed, err := c.deployStamp()
	if err != nil {
		c.log.Warn(opStaleness, "could not read the deploy stamp; leaving the shim alone", withCause(fields, err))
		return false, nil
	}
	if deployed == "" {
		c.log.Debug(opStaleness, "the deploy stamp is empty; leaving the shim alone", fields)
		return false, nil
	}
	if c.deps.SessionBuildSHA == nil {
		c.log.Debug(opStaleness, "no session build is reported; leaving the shim alone", fields)
		return false, nil
	}
	reported, known := c.deps.SessionBuildSHA(ws)
	fields["reported_sha"] = reported
	fields["deployed_sha"] = deployed
	if !known || reported == "" {
		c.log.Debug(opStaleness, "the session reports no build; leaving the shim alone", fields)
		return false, nil
	}
	if reported == deployed {
		c.log.Debug(opStaleness, "the session is on the deployed build", fields)
		return false, nil
	}
	c.log.Info(opStaleness, "the session is on an older build than the deploy stamp", fields)
	_ = ctx
	return true, nil
}

// deployStamp reads the deployed build's sha, or an empty answer when no reader
// is wired.
func (c *controller) deployStamp() (string, error) {
	if c.deps.DeployStamp == nil {
		return "", nil
	}
	return c.deps.DeployStamp()
}

// ReloadWebapp pushes the EMPTY reload_webapp arm: Emacs reloads this
// workspace's xwidget against the SAME daemon, and the reloaded page's default
// first-page load is the whole of the recovery. No address rides it, because
// the daemon is not changing — and a combined daemon-and-webapp rollout never
// sends it at all, since the handover's fresh attach pulls the new assets as a
// side effect.
func (c *controller) ReloadWebapp(_ context.Context, ws ids.WorkspaceID) error {
	c.deps.Pusher.PushReloadWebapp(ws)
	c.log.Info(opReloadWebap, "pushed the webapp reload", dlog.Context{"workspace": string(ws)})
	return nil
}
