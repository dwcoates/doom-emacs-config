package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/scriptrunner"
)

// A repository's DEPLOY HOLD (agentrepl.v1.UpdateDeployHold) skips its
// deploys' deploy hook while their pre- and post-deploy stages run as they do:
// for agent-repl, merges keep landing on master and the runtime is not
// redeployed, so a hand-deployed build (a profiling build) is kept. It is
// DURABLE and per repository (wsm's merge_queue_repos), read at each deploy
// stage rather than cached, so a successor answers the hold its predecessor
// took.

const (
	opHoldDeploys    = "daemon.merge.hold_deploys"
	opReleaseDeploys = "daemon.merge.release_deploys"
)

// DeployReleaseKind is what a release ran for the deploys its hold skipped.
type DeployReleaseKind int

const (
	// ReleaseNothingSkipped is a hold no deploy skipped its hook under.
	ReleaseNothingSkipped DeployReleaseKind = iota
	// ReleaseScriptRan is the script deploy hook run once, exiting 0.
	ReleaseScriptRan
	// ReleaseScriptFailed is the script deploy hook run once and failing.
	ReleaseScriptFailed
	// ReleasePromptNotRun is a prompt deploy hook, which has no owner to run
	// in at a release.
	ReleasePromptNotRun
	// ReleaseNoDeployHook is a repository whose config has no deploy hook any
	// more, or cannot be read.
	ReleaseNoDeployHook
)

// DeployRelease is a release's answer.
type DeployRelease struct {
	// Kind is what ran for the skipped deploys.
	Kind DeployReleaseKind
	// Skipped is how many deploys skipped their deploy hook during the hold.
	Skipped int
	// Path is the script, for the two script kinds.
	Path string
	// LogPath is the script's log, for the two script kinds.
	LogPath string
	// Detail is how the script failed, or why there was no hook to run.
	Detail string
}

// HoldDeploys implements Orchestrator.
func (o *orchestrator) HoldDeploys(ctx context.Context, scope RepositoryScope) error {
	log := o.deps.Log.Global()
	repository, key, err := o.scopedRepository(ctx, opHoldDeploys, "a deploy hold", scope)
	if err != nil {
		return err
	}
	repoDir := repository.Dir
	fields := dlog.Context{"repo": string(key), "repository": repoDir}
	hold, err := o.deps.DB.DeployHoldOf(ctx, key)
	if err != nil {
		log.Error(opHoldDeploys, "could not read the repository's deploy hold", withField(fields, "error", err.Error()))
		return err
	}
	if hold.Held {
		log.Info(opHoldDeploys, "refused a hold of a repository whose deploys are already held", withField(fields, "held_since", hold.Since))
		return refuse(ArmDeploysAlreadyHeld, "", "the repository %s's deploy stage is already held", repoDir)
	}
	if err := o.deps.DB.SetDeployHold(ctx, key, true, o.deps.Now()); err != nil {
		log.Error(opHoldDeploys, "could not hold the repository's deploys", withField(fields, "error", err.Error()))
		return err
	}
	log.Info(opHoldDeploys, "held the repository's deploy stage: its deploys still land, and skip their deploy hook", fields)
	return nil
}

// ReleaseDeploys implements Orchestrator.
func (o *orchestrator) ReleaseDeploys(ctx context.Context, scope RepositoryScope) (DeployRelease, error) {
	log := o.deps.Log.Global()
	repository, key, err := o.scopedRepository(ctx, opReleaseDeploys, "a deploy release", scope)
	if err != nil {
		return DeployRelease{}, err
	}
	repoDir := repository.Dir
	fields := dlog.Context{"repo": string(key), "repository": repoDir}
	hold, err := o.deps.DB.DeployHoldOf(ctx, key)
	if err != nil {
		log.Error(opReleaseDeploys, "could not read the repository's deploy hold", withField(fields, "error", err.Error()))
		return DeployRelease{}, err
	}
	if !hold.Held {
		log.Info(opReleaseDeploys, "refused a release of a repository whose deploys are not held", fields)
		return DeployRelease{}, refuse(ArmDeploysNotHeld, "", "the repository %s's deploy stage is not held", repoDir)
	}
	if err := o.deps.DB.SetDeployHold(ctx, key, false, o.deps.Now()); err != nil {
		log.Error(opReleaseDeploys, "could not release the repository's deploys", withField(fields, "error", err.Error()))
		return DeployRelease{}, err
	}
	fields["skipped"] = hold.Skipped
	release := DeployRelease{Skipped: hold.Skipped}
	if hold.Skipped == 0 {
		log.Info(opReleaseDeploys, "released the repository's deploy stage; no deploy skipped its hook during the hold", fields)
		return release, nil
	}
	cfg, err := o.deps.DeployConfig.Load(repoDir)
	if err != nil {
		release.Kind, release.Detail = ReleaseNoDeployHook, err.Error()
		log.Warn(opReleaseDeploys, "released the repository's deploy stage, but its deploy config cannot be read; the skipped deploy hook is not run", withField(fields, "cause", err.Error()))
		return release, nil
	}
	if cfg.GetDeploy() == nil || !deployconfig.RunsFor(cfg.GetDeploy(), true) {
		release.Kind, release.Detail = ReleaseNoDeployHook, "the repository's deploy config has no deploy hook for its main branch"
		log.Info(opReleaseDeploys, "released the repository's deploy stage; its config has no deploy hook to run for the skipped deploys", fields)
		return release, nil
	}
	script := cfg.GetDeploy().GetHook().GetScript()
	if script == nil {
		release.Kind = ReleasePromptNotRun
		log.Info(opReleaseDeploys, "released the repository's deploy stage; its deploy hook is a prompt, which has no owner to run in now, so the skipped deploys are not deployed", fields)
		return release, nil
	}
	return o.runReleasedScript(ctx, repoDir, script, release, fields), nil
}

// runReleasedScript runs a repository's script deploy hook once for the
// deploys its hold skipped, with no owner: AGENT_REPL_CALLING_WORKSPACE and
// AGENT_REPL_DEPLOY_LEDGER are unset, so a script that calls back into the
// daemon names no deploy.
func (o *orchestrator) runReleasedScript(ctx context.Context, repoDir string, script *repoconfigv1.DeployHookScript, release DeployRelease, fields dlog.Context) DeployRelease {
	log := o.deps.Log.Global()
	release.Path = script.GetPath()
	release.LogPath = filepath.Join(o.deps.StateDir, "deploy-logs", fmt.Sprintf("release-%d.log", o.deps.Now().UnixNano()))
	fields["script"], fields["log"] = release.Path, release.LogPath
	if err := os.MkdirAll(filepath.Dir(release.LogPath), 0o755); err != nil {
		release.Kind, release.Detail = ReleaseScriptFailed, err.Error()
		log.Error(opReleaseDeploys, "could not make the released deploy hook's log directory", withField(fields, "error", err.Error()))
		return release
	}
	run, err := o.deps.HookRunner.RunLinesWith(ctx, repoDir, []string{"bash", filepath.Join(repoDir, script.GetPath())},
		scriptrunner.Options{Env: []string{EnvDeployStage + "=deploy"}, Timeout: time.Duration(script.GetTimeoutSeconds()) * time.Second}, nil)
	if writeErr := os.WriteFile(release.LogPath, []byte(run.Output), 0o644); writeErr != nil {
		log.Error(opReleaseDeploys, "could not write the released deploy hook's log", withField(fields, "error", writeErr.Error()))
	}
	switch {
	case err != nil:
		release.Kind, release.Detail = ReleaseScriptFailed, err.Error()
		log.Error(opReleaseDeploys, "the released deploy hook could not be run", withField(fields, "error", err.Error()))
	case run.TimedOut:
		release.Kind, release.Detail = ReleaseScriptFailed, fmt.Sprintf("it ran past its %ds timeout", script.GetTimeoutSeconds())
		log.Warn(opReleaseDeploys, "the released deploy hook ran past its timeout", fields)
	case run.ExitCode != 0:
		release.Kind, release.Detail = ReleaseScriptFailed, fmt.Sprintf("it exited %d", run.ExitCode)
		log.Warn(opReleaseDeploys, "the released deploy hook exited non-zero", withField(fields, "exit_code", run.ExitCode))
	default:
		release.Kind = ReleaseScriptRan
		log.Info(opReleaseDeploys, "released the repository's deploy stage and ran its deploy hook once for the skipped deploys", fields)
	}
	return release
}

// recordInheritedHolds records, at boot, every repository whose deploy stage
// the previous daemon left held, so a reader of this daemon's log knows its
// landings there will not deploy until the hold is released.
func (o *orchestrator) recordInheritedHolds(ctx context.Context) error {
	repositories, err := o.deps.DB.ListRepositories(ctx)
	if err != nil {
		return err
	}
	for _, repository := range repositories {
		key, err := o.repoKeyFor(ctx, repository.Dir)
		if err != nil {
			o.deps.Log.Global().Warn("daemon.merge.recover", "could not resolve a registered repository's queue to read its deploy hold", dlog.Context{
				"repository": repository.Dir, "error": err.Error()})
			continue
		}
		hold, err := o.deps.DB.DeployHoldOf(ctx, key)
		if err != nil {
			return err
		}
		if hold.Held {
			o.deps.Log.Global().Info("daemon.merge.recover", "a repository's deploy stage is held: its deploys land, and skip their deploy hook until it is released", dlog.Context{
				"repository": repository.Dir, "held_since": hold.Since, "skipped": hold.Skipped})
		}
	}
	return nil
}
