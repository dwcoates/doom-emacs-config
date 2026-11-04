package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/merge"
	"agent-repld/internal/wsm"
)

// This file is a create's part in its repository's LIFECYCLE
// (repoconfig.v1.LifecycleConfig): the facts the hooks of every later stage
// read (who created the workspace, and the Slack message it was created for),
// and the CREATION STAGE, run once the workspace is registered and before its
// session's first prompt.
//
// THE DAEMON NEVER POSTS TO SLACK. A workspace created for a Slack message
// records the message and stores it among its links as a reference; what the
// thread is told is the repository's hooks' to say, the emoji-ack defaults
// included (lifecycleconfig/stages.go).
//
// A CREATION SCRIPT THAT FAILS FAILS THE CREATE, which is undone (create's
// unmaterialize) and reported to the repository's main workspace like every
// failed create (reportCreateFailure): a workspace whose repository could not
// set it up is never handed over half-made.

// SlackOriginLabel is the label the originating thread is stored under among
// the workspace's links.
const SlackOriginLabel = "Originating Slack thread"

// ArmLifecycleConfigInvalid is a create into a repository whose lifecycle
// config is unusable, so its creation stage cannot be read
// (agentrepl.v1.CreateWorkspaceError.lifecycle_config_invalid). Its `path` and
// `detail` fields name the file and why.
const ArmLifecycleConfigInvalid = "lifecycle_config_invalid"

// ArmCreationHookFailed is a create whose creation-stage hook failed
// (agentrepl.v1.CreateWorkspaceError.creation_hook_failed). Its `path`,
// `detail` and `log_path` fields name the hook, how it failed, and its log.
const ArmCreationHookFailed = "creation_hook_failed"

// creationStage is the creation stage one create runs: the repository's
// config as the create read it, and the hooks it chose for the workspace's
// kind and target.
type creationStage struct {
	// repoRoot is the repository's main checkout root, where a script runs.
	repoRoot string
	// hooks are the stage's hooks, in order.
	hooks []*repoconfigv1.Hook
}

// readLifecycle reads the lifecycle config a create into REPODIR runs its
// creation stage from, before anything is named or built: nil for a
// repository with none, which runs the built-in defaults alone, and a refusal
// for an unusable one.
func (v *verbs) readLifecycle(log dlog.Logger, repoDir string) (*repoconfigv1.LifecycleConfig, error) {
	cfg, err := v.deps.Lifecycle.Load(repoDir)
	var missing *lifecycleconfig.MissingError
	var invalid *lifecycleconfig.InvalidError
	switch {
	case errors.As(err, &missing):
		log.Debug(opCreate, "the repository states no lifecycle config; the creation stage runs the built-in defaults alone", dlog.Context{"path": missing.Path})
		return nil, nil
	case errors.As(err, &invalid):
		return nil, refuseWith(log, "CreateWorkspace", ArmLifecycleConfigInvalid,
			fmt.Sprintf("the repository's lifecycle config %s is unusable: %s", invalid.Path, invalid.Detail), false,
			map[string]any{"path": invalid.Path, "detail": invalid.Detail})
	case err != nil:
		log.Error(opCreate, "could not read the repository's lifecycle config for the creation stage", dlog.Context{"repo_dir": repoDir, "cause": err.Error()})
		return nil, fmt.Errorf("create: read the lifecycle config of %q: %w", repoDir, err)
	}
	return cfg, nil
}

// creationStageOf is the creation stage SPEC runs under CFG, its workspace
// landing into TARGETDIR of the repository at REPODIR. A merge workspace runs
// none: the queue makes it, and its request named every hook it runs.
func (v *verbs) creationStageOf(cfg *repoconfigv1.LifecycleConfig, spec CreateSpec, repoDir, targetDir string) creationStage {
	if spec.MergeOf != nil {
		return creationStage{}
	}
	builtins := lifecycleconfig.Builtins{CheckoutRoot: v.deps.CheckoutRoot}
	hooks := builtins.Hooks(cfg, lifecycleconfig.StageCreation, spec.Originator, dirpath.Same(targetDir, repoDir))
	return creationStage{repoRoot: repoDir, hooks: hooks}
}

// recordOrigin records who created RECORD and, for a workspace created for a
// Slack message, the message, stored among its links as a reference. Every
// later stage's hooks read both.
func (v *verbs) recordOrigin(ctx context.Context, log dlog.Logger, record wsm.Workspace, spec CreateSpec) error {
	if err := v.deps.DB.SetWorkspaceOriginator(ctx, record.ID, spec.Originator.String()); err != nil {
		log.Error(opCreate, "could not record who created the workspace", dlog.Context{"originator": spec.Originator.String(), "cause": err.Error()})
		return fmt.Errorf("create %q: record its originator: %w", record.Name, err)
	}
	if spec.SlackThread == "" {
		return nil
	}
	fields := dlog.Context{"thread_url": spec.SlackThread}
	if err := v.deps.DB.SetWorkspaceSlackOrigin(ctx, record.ID, spec.SlackThread); err != nil {
		log.Error(opCreate, "could not record the Slack message the workspace was created for", dlog.WithCause(fields, err))
		return fmt.Errorf("create %q: record its Slack origin: %w", record.Name, err)
	}
	if _, err := v.SetLink(ctx, record.ID, spec.SlackThread, SlackOriginLabel, false, ""); err != nil {
		log.Error(opCreate, "could not store the originating Slack thread among the links", dlog.WithCause(fields, err))
		return fmt.Errorf("create %q: store its originating thread: %w", record.Name, err)
	}
	return nil
}

// runCreationScripts runs the stage's script hooks, in order, from the
// repository's main checkout root, and answers the first failure as the
// create's refusal. Prompt hooks are the initial prompt's (creationPrompts).
func (v *verbs) runCreationScripts(ctx context.Context, log dlog.Logger, record wsm.Workspace, spec CreateSpec, stage creationStage) error {
	env := lifecycleconfig.HookEnv(record.Dir, lifecycleconfig.StageCreation, spec.SlackThread)
	for i, hook := range stage.hooks {
		script := hook.GetScript()
		if script == nil {
			continue
		}
		logPath := merge.LifecycleLogPath(v.deps.StateDir, lifecycleconfig.StageCreation, v.now(), i)
		fields := dlog.Context{"hook": i, "script": script.GetPath(), "log": logPath, "repo_root": stage.repoRoot,
			"originator": spec.Originator.String(), "timeout_seconds": script.GetTimeoutSeconds()}
		log.Info(opCreate, "a creation script hook started", fields)
		ran := merge.RunKeptScript(ctx, v.deps.HookRunner, stage.repoRoot, script, env, logPath)
		failure := ran.Failure(script)
		if failure == "" && ran.LogErr != nil {
			failure = fmt.Sprintf("it could not keep its log: %v", ran.LogErr)
		}
		if failure != "" {
			fields["failure"], fields["exit_code"] = failure, ran.Run.ExitCode
			log.Error(opCreate, "a creation script hook failed; the create is undone", fields)
			return refuseWith(log, "CreateWorkspace", ArmCreationHookFailed,
				fmt.Sprintf("the creation script %s failed: %s (its output is in %s)", script.GetPath(), failure, logPath), false,
				map[string]any{"path": script.GetPath(), "detail": failure, "log_path": logPath})
		}
		fields["wall_ms"] = ran.Run.Wall.Milliseconds()
		log.Info(opCreate, "a creation script hook succeeded", fields)
	}
	return nil
}

// creationPrompts are the stage's prompt hooks, their placeholder replaced,
// as the text the initial prompt carries after its commission. A prompt
// naming the originating thread of a workspace created for none fails the
// create: the hook cannot be what its author wrote.
func creationPrompts(log dlog.Logger, spec CreateSpec, stage creationStage) (string, error) {
	var texts []string
	for i, hook := range stage.hooks {
		prompt := hook.GetPrompt()
		if prompt == nil {
			continue
		}
		text, err := lifecycleconfig.ExpandPrompt(prompt.GetText(), spec.SlackThread)
		if err != nil {
			log.Error(opCreate, "a creation prompt hook names the originating Slack thread of a workspace created for none; the create is undone", dlog.Context{
				"hook": i, "originator": spec.Originator.String(), "cause": err.Error()})
			return "", refuseWith(log, "CreateWorkspace", ArmCreationHookFailed,
				fmt.Sprintf("creation prompt hook %d: %v", i, err), false,
				map[string]any{"path": "", "detail": err.Error(), "log_path": ""})
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n\n"), nil
}

// hasCreationPrompts reports whether the stage carries any prompt hook.
func (s creationStage) hasCreationPrompts() bool {
	for _, hook := range s.hooks {
		if hook.GetPrompt() != nil {
			return true
		}
	}
	return false
}
