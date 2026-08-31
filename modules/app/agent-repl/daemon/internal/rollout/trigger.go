package rollout

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
)

// DeployNoBounce is the flag the rollout always passes. THE ROLLOUT INVOKES THE
// ONE DEPLOY CHAIN AND NEVER A SECOND BUILD PATH — and it invokes it without
// the bounce, because the bounce is the rollout's own job and the script's
// version would restart this daemon out from under the handover.
const DeployNoBounce = "--no-bounce"

// Trigger is the merge orchestrator's self-reload hook: classify what landed,
// run the ONE deploy chain exactly once, and then take the per-subsystem
// action.
//
// The action is chosen ONCE for the whole landed range, not once per subsystem:
//
//	daemon (or proto, which touches every consumer) → a blue-green HANDOVER,
//	  and nothing else. A combined daemon-and-webapp rollout is a handover
//	  alone: the fresh attach pulls the new assets as a side effect. The shims
//	  the successor adopts are bounced onto the new build by the ordinary
//	  build-staleness check, at each session's own quiet moment.
//	shim alone → a per-workspace preemptive RELAUNCH.
//	webapp alone → a reload_webapp PUSH per workspace with a webview.
//	elisp → LOGGED and nothing else: hot-loading elisp is Emacs's.
//	store or sidecar → UNHANDLED by design, and named in a WARNING so the
//	  deliberate gap is visible rather than silent.
func (c *controller) Trigger(ctx context.Context, landed []gitclient.Commit) error {
	if len(landed) == 0 {
		c.log.Debug(opTrigger, "nothing landed; there is nothing to roll out", nil)
		return nil
	}
	rangeSpec := landedRange(landed)
	fields := dlog.Context{"commits": len(landed), "range": rangeSpec}

	paths, err := c.deps.Git.ChangedPaths(ctx, c.deps.SelfRepoDir, rangeSpec)
	if err != nil {
		c.log.Error(opTrigger, "could not read the landed range's changed paths", withCause(fields, err))
		return fmt.Errorf("rollout: trigger: changed paths for %s: %w", rangeSpec, err)
	}
	subsystems := Classify(paths)
	fields["subsystems"] = names(subsystems)
	fields["paths"] = len(paths)

	if unhandled := Unhandled(subsystems); len(unhandled) > 0 {
		c.log.Warn(opClassify, "the landed range touches subsystems this rollout deliberately does not handle; "+
			"a user-initiated full restart is what deploys them",
			merge(fields, dlog.Context{"unhandled": names(unhandled)}))
	}
	if len(subsystems) == 0 {
		c.log.Debug(opClassify, "the landed range touches no deployable subsystem", fields)
		return nil
	}
	c.log.Info(opClassify, "classified the landed range", fields)

	if err := c.deploy(ctx, fields); err != nil {
		return err
	}

	if contains(subsystems, SubsystemElisp) {
		// LOGGED, NEVER ACTED ON. Emacs hot-loads its own elisp; the daemon has
		// no route into the editor's runtime and inventing one would be a
		// second deploy path.
		c.log.Info(opTrigger, "the landed range touches elisp; Emacs hot-loads it, not the daemon", fields)
	}

	switch {
	case contains(subsystems, SubsystemDaemon):
		c.log.Info(opTrigger, "the daemon changed; handing over", fields)
		return c.Handover(ctx)
	case contains(subsystems, SubsystemShim):
		return c.relaunchFleet(ctx, fields)
	case contains(subsystems, SubsystemWebapp):
		return c.reloadFleet(ctx, fields)
	default:
		c.log.Debug(opTrigger, "no subsystem in the landed range has a rollout action", fields)
		return nil
	}
}

// deploy runs the ONE deploy chain exactly once and archives its output. A
// failing chain STOPS the rollout: acting on a build that did not happen would
// bounce every session onto the build that is already there.
func (c *controller) deploy(ctx context.Context, fields dlog.Context) error {
	argv := []string{c.deps.DeployScript, DeployNoBounce}
	run := merge(fields, dlog.Context{"script": c.deps.DeployScript, "argv": argv})

	output, code, err := c.deps.Deploy.Run(ctx, c.deps.SelfRepoDir, argv)
	archive, archiveErr := c.archiveDeploy(output)
	if archiveErr != nil {
		c.log.Warn(opDeploy, "could not archive the deploy run's output", withCause(run, archiveErr))
	} else {
		run["archive"] = archive
	}
	if err != nil {
		c.log.Error(opDeploy, "the deploy chain could not be run", withCause(run, err))
		return fmt.Errorf("rollout: deploy: %w", err)
	}
	if code != 0 {
		run["exit_code"] = code
		c.log.Error(opDeploy, "the deploy chain failed; nothing is rolled out", run)
		return fmt.Errorf("rollout: deploy: %s exited %d", c.deps.DeployScript, code)
	}
	c.log.Info(opDeploy, "the deploy chain succeeded", run)
	return nil
}

// archiveDeploy keeps the whole deploy run under the state root, so a rollout
// that went wrong is readable afterwards rather than only in the run log's
// tail.
func (c *controller) archiveDeploy(output string) (string, error) {
	if c.deps.StateDir == "" {
		return "", fmt.Errorf("rollout: no state root is configured to archive the deploy run in")
	}
	dir := filepath.Join(c.deps.StateDir, "merge-logs")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("rollout: create the deploy archive directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, fmt.Sprintf("deploy-%d.log", c.deps.Clock.Now().UnixNano()))
	if err := os.WriteFile(path, []byte(output), 0o644); err != nil {
		return "", fmt.Errorf("rollout: archive the deploy run to %s: %w", path, err)
	}
	return path, nil
}

// relaunchFleet bounces every live workspace's shim, independently: one
// workspace that will not bounce is that workspace's own failure and must not
// stop the others from getting the new build.
func (c *controller) relaunchFleet(ctx context.Context, fields dlog.Context) error {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opTrigger, "could not list the workspaces to relaunch", withCause(fields, err))
		return fmt.Errorf("rollout: trigger: %w", err)
	}
	var bounced []ids.WorkspaceID
	for _, ws := range workspaces {
		if _, live := c.deps.Shims.Client(ws.ID); !live {
			// A PARKED WORKSPACE NEEDS NOTHING: the next implicit revival
			// spawns the new binary.
			c.log.Debug(opTrigger, "the workspace has no live shim to relaunch",
				merge(fields, dlog.Context{"workspace": string(ws.ID)}))
			continue
		}
		if err := c.RelaunchShim(ctx, ws.ID, ReasonShimChanged); err != nil {
			c.log.Error(opTrigger, "a workspace's shim relaunch failed; the others still bounce",
				withCause(merge(fields, dlog.Context{"workspace": string(ws.ID)}), err))
			continue
		}
		bounced = append(bounced, ws.ID)
	}
	c.log.Info(opTrigger, "bounced the fleet onto the new shim build",
		merge(fields, dlog.Context{"bounced": len(bounced)}))
	return nil
}

// reloadFleet pushes reload_webapp to every workspace whose webview is open. A
// workspace with no webview needs nothing: there is no page to reload.
func (c *controller) reloadFleet(ctx context.Context, fields dlog.Context) error {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opTrigger, "could not list the workspaces to reload", withCause(fields, err))
		return fmt.Errorf("rollout: trigger: %w", err)
	}
	pushed := 0
	for _, ws := range workspaces {
		if !c.deps.Participants.Participants(ws.ID).Web {
			c.log.Debug(opTrigger, "the workspace has no webview to reload",
				merge(fields, dlog.Context{"workspace": string(ws.ID)}))
			continue
		}
		if err := c.ReloadWebapp(ctx, ws.ID); err != nil {
			c.log.Error(opTrigger, "could not push a workspace's webapp reload",
				withCause(merge(fields, dlog.Context{"workspace": string(ws.ID)}), err))
			continue
		}
		pushed++
	}
	c.log.Info(opTrigger, "pushed the webapp reload to every open webview",
		merge(fields, dlog.Context{"pushed": pushed}))
	return nil
}

// landedRange is the diff range the landed commits describe. LandedRange
// answers OLDEST FIRST, so the range runs from before the first to the last.
func landedRange(landed []gitclient.Commit) string {
	first, last := landed[0].SHA, landed[len(landed)-1].SHA
	return first + "^.." + last
}
