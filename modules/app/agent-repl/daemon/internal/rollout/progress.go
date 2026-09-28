package rollout

import (
	"context"
	"fmt"

	"claude-repld/internal/deployprogress"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE ROLLOUT'S SHARE OF THE UPDATE LINE (owner request, 2026-09-27). The
// deploy states every phase up to the handover; the rollout owns the two
// endings the deploy cannot see:
//
//   - the SUCCESSOR says `updated`, because the outgoing daemon's footer
//     streams end at the transfer. It learns that it came from a deploy from
//     the intent manifest (Manifest.Deploy), the only thing the outgoing
//     daemon leaves it.
//   - an incumbent whose handover or restart CANNOT FINISH takes the line
//     down: it keeps serving, and a line saying "handing over" for the rest
//     of its life would be a lie. What went wrong is already on the fault
//     line (an expired adoption window) or in the ERROR records.
//
// A successor that will not start is a FAULT, `successor_spawn_failed`, the
// daemon-scoped kind that exists for exactly that and that the footer draws
// as `blocked · daemon_impaired`. The next handover whose successor proves it
// is serving closes it.

// opProgress is the operation the rollout's update-line statements are
// recorded under.
const opProgress = "daemon.rollout.progress"

// recordDeployTakeover latches that the handover this successor joins was a
// deploy's, so it says `updated` once it has taken over.
func (c *controller) recordDeployTakeover(deploy bool) {
	if !deploy {
		return
	}
	c.mu.Lock()
	before := c.deployTakeover
	c.deployTakeover = true
	c.mu.Unlock()
	if !before {
		c.log.Info(opProgress, "the handover being joined is a deploy's; this daemon says updated once it has taken over", nil)
	}
}

// shimDeferred reports whether a staleness judgement left the shim for later:
// stale, not skipped, and registered behind its work rather than bounced now.
func shimDeferred(check StaleCheck) bool {
	return check.Stale && check.Skipped == "" && !check.Bounce.Now
}

// finishDeployStory says `updated` on every strip, with each workspace's
// deferred shim noted, when this daemon took over from a deploy. It says it
// ONCE per takeover.
func (c *controller) finishDeployStory(deferred map[ids.WorkspaceID][]deployprogress.Note, fields dlog.Context) {
	c.mu.Lock()
	deploy := c.deployTakeover
	c.deployTakeover = false
	c.mu.Unlock()
	if !deploy {
		c.log.Debug(opProgress, "this takeover was no deploy's; there is no deploy story to finish", fields)
		return
	}
	c.log.Info(opProgress, "took over from a deploy's handover; saying updated on every strip",
		merge(fields, dlog.Context{"shims_when_idle": len(deferred)}))
	c.deps.Progress.SetDeployProgress(&deployprogress.Progress{Phase: deployprogress.Updated, Notes: deferred})
}

// clearDeployLine takes the deploy's line down from an incumbent whose
// rollout cannot finish.
func (c *controller) clearDeployLine(why string, fields dlog.Context) {
	c.log.Info(opProgress, "the rollout cannot finish; taking the deploy's line down", merge(fields, dlog.Context{"why": why}))
	c.deps.Progress.SetDeployProgress(nil)
}

// openSuccessorFault records a successor that would not start (or never
// proved it was serving) as the daemon-scoped `successor_spawn_failed` fault,
// which the footer draws on every strip. A fault that cannot be recorded is
// ERROR: the handover's own failure is still the caller's answer.
func (c *controller) openSuccessorFault(ctx context.Context, cause error, fields dlog.Context) {
	_, err := c.deps.DB.OpenFault(context.WithoutCancel(ctx), wsm.Fault{
		Kind:     health.KindSuccessorSpawnFailed,
		Detail:   fmt.Sprintf("the deploy's successor daemon did not come up; this daemon keeps serving: %v", cause),
		Evidence: map[string]string{"detail": cause.Error()},
		OpenedAt: c.deps.Clock.Now(),
	})
	if err != nil {
		c.log.Error(opProgress, "could not record the successor that would not start as a fault", withCause(fields, err))
		return
	}
	c.log.Info(opProgress, "recorded the successor that would not start as a fault", withCause(fields, cause))
}

// closeSuccessorFaults fires the successor-serving recovery edge once a
// successor has proved it is serving: every daemon-scoped fault whose lifetime
// ends there (`successor_spawn_failed`, an unclaimed handover's
// `adoption_window_expired`) records a condition that is over. A read or a
// close that fails is ERROR and leaves the fault standing.
func (c *controller) closeSuccessorFaults(ctx context.Context, fields dlog.Context) {
	health.CloseOnEdge(context.WithoutCancel(ctx), c.deps.DB, c.log.With(fields), health.EdgeSuccessorServing,
		health.EdgeScope{DaemonOnly: true}, c.deps.Clock.Now())
}
