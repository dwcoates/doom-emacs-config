package merge

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/wsm"
)

// This file is a deploy's FINISH STAGE (repoconfig.v1.LifecycleConfig
// .finish): the hooks a finishing workspace runs once the turn that asked for
// its deploy has ended, and BEFORE the deploy is put in line. Nothing about the
// deploy is drawn while they run, since it has no ledger yet (request.go), and
// they never wait on another deploy: an explanation-engine workspace opens its
// pull request here, while every deploy ahead of it in the queue is still
// waiting on its own.
//
// ONE WORKSPACE, ITS OWN KIND. The stage runs in the workspace whose deploy it
// is, with its kind's hooks (lifecycleconfig.Builtins.Hooks). An async merge
// has none: its merge workspace is created by the queue, never finished.
//
// DURABLE. The stage records the hook it stands at, and a prompt hook's turn,
// before running it (wsm.FinishProgress), so a restart resumes at that hook
// and reattaches to its turn rather than sending the prompt again. A script
// cut short runs again, as every script hook must bear.
//
// A FAILED HOOK WITHDRAWS THE DEPLOY: the request leaves the queue before it
// was ever in line, and the failure is handed back to the workspace's
// conversation as a failed deploy's is.

// opFinishStage is the operation every finish-stage record is made under.
const opFinishStage = "daemon.merge.finish_stage"

// finishTarget is what a finish stage runs against: the workspace's dir, its
// repository's config and its own kind, and the hooks they choose.
type finishTarget struct {
	dir    string
	target deployTarget
	hooks  []*repoconfigv1.Hook
}

// finishTargetOf reads the finish stage WS's deploy runs: none for an async
// merge (a merge workspace's own merge).
func (o *orchestrator) finishTargetOf(ctx context.Context, ws ids.WorkspaceID, source wsm.MergeSource) (finishTarget, error) {
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return finishTarget{}, fmt.Errorf("merge: reading the finishing workspace %s: %w", ws, err)
	}
	if record.MergeOf != nil {
		return finishTarget{}, nil
	}
	dir, err := o.targetDirOf(ctx, ws, source)
	if err != nil {
		return finishTarget{}, fmt.Errorf("merge: resolving where %s's deploy lands: %w", ws, err)
	}
	target, err := o.deployTargetFor(ctx, ws, dir)
	if err != nil {
		return finishTarget{}, err
	}
	hooks := o.deps.Builtins.Hooks(target.cfg, lifecycleconfig.StageFinish, target.owner.kind, target.toMain)
	return finishTarget{dir: record.Dir, target: target, hooks: hooks}, nil
}

// hasFinishStage reports whether a deploy of RECORD into DEPLOY's target runs
// any finish-stage hook: never an async merge's merge workspace, and
// otherwise whenever its kind's finish hooks are not empty.
func (o *orchestrator) hasFinishStage(deploy deployTarget, record wsm.Workspace) bool {
	if record.MergeOf != nil {
		return false
	}
	return len(o.deps.Builtins.Hooks(deploy.cfg, lifecycleconfig.StageFinish, deploy.owner.kind, deploy.toMain)) > 0
}

// runFinishStage runs WS's finish-stage hooks from where its recorded progress
// stands, and answers the failure's one-line account, empty when every hook
// succeeded (or there were none). An error is a stage that could not be run
// at all; the request then stays recorded for the next boot, as any wait that
// could not finish does.
func (o *orchestrator) runFinishStage(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey) (string, error) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	entry, found, err := o.requestOf(ctx, repo, ws)
	if err != nil {
		log.Error(opFinishStage, "could not read the finishing workspace's request", withField(fields, "error", err.Error()))
		return "", err
	}
	if !found {
		err := fmt.Errorf("merge: %s has no recorded request to run the finish stage of", ws)
		log.Error(opFinishStage, "a finish stage has no recorded request", withField(fields, "error", err.Error()))
		return "", err
	}
	ft, err := o.finishTargetOf(ctx, ws, entry.Source)
	if err != nil {
		if refusal, refused := Refused(err); refused {
			// THE CONFIG WENT AWAY OR BROKE between the request and its turn's
			// end: the deploy cannot run, so it is withdrawn like a failed hook.
			log.Info(opFinishStage, "the repository's lifecycle config cannot be read for the finish stage; the deploy is withdrawn", withField(fields, "cause", refusal.Error()))
			return refusal.Error(), nil
		}
		log.Error(opFinishStage, "could not read what the finish stage runs", withField(fields, "error", err.Error()))
		return "", err
	}
	if len(ft.hooks) == 0 {
		log.Debug(opFinishStage, "the workspace's kind runs no finish-stage hook", fields)
		return "", o.clearFinish(ctx, log, fields, ws)
	}
	fields["originator"], fields["hooks"] = ft.target.owner.kind.String(), len(ft.hooks)
	progress, resuming, err := o.deps.DB.WorkspaceFinishProgress(ctx, ws)
	if err != nil {
		log.Error(opFinishStage, "could not read where the finish stage stands", withField(fields, "error", err.Error()))
		return "", err
	}
	start := 0
	if resuming {
		start = progress.HookIndex
		log.Info(opFinishStage, "resumes the finish stage where a restart left it", withField(fields, "hook", start))
	} else {
		log.Info(opFinishStage, "the finish stage starts; the deploy is put in line once it succeeds", fields)
	}
	for i := start; i < len(ft.hooks); i++ {
		var resumed *wsm.FinishProgress
		if resuming && i == start {
			resumed = &progress
		}
		failure, err := o.runFinishHook(ctx, log, withField(fields, "hook", i), ws, ft, i, resumed)
		if err != nil || failure != "" {
			return failure, err
		}
	}
	log.Info(opFinishStage, "every finish-stage hook succeeded", fields)
	return "", o.clearFinish(ctx, log, fields, ws)
}

// runFinishHook runs finish hook I, recording its progress first; RESUMED is
// the progress a restart left at this hook, nil for a fresh run.
func (o *orchestrator) runFinishHook(ctx context.Context, log dlog.Logger, fields dlog.Context, ws ids.WorkspaceID, ft finishTarget, i int, resumed *wsm.FinishProgress) (string, error) {
	switch h := ft.hooks[i].GetHook().(type) {
	case *repoconfigv1.Hook_Prompt:
		text, err := lifecycleconfig.ExpandPrompt(h.Prompt.GetText(), ft.target.owner.slackThread)
		if err != nil {
			log.Error(opFinishStage, "a finish prompt hook names the originating Slack thread of a workspace created for none; the deploy is withdrawn", withField(fields, "cause", err.Error()))
			return fmt.Sprintf("its finish prompt hook %v", err), nil
		}
		origin := conversationv1.PromptOrigin_PROMPT_ORIGIN_FINISH_HOOK
		var close wsm.TurnClose
		if resumed != nil && resumed.Turn != "" {
			close, err = o.reattachTurn(ctx, ws, fields, resumed.Turn, text, origin)
		} else {
			turn := wsm.NewTurnID()
			if err := o.deps.DB.PutFinishProgress(ctx, ws, wsm.FinishProgress{HookIndex: i, Turn: turn}); err != nil {
				log.Error(opFinishStage, "could not record the finish stage's progress; its prompt is not sent", withField(fields, "error", err.Error()))
				return "", err
			}
			if err := o.ensureSession(ctx, ws); err != nil {
				log.Error(opFinishStage, "could not bring up the session a finish prompt runs in", withField(fields, "error", err.Error()))
				return "", err
			}
			close, err = o.submit(ctx, ws, turn, text, origin)
		}
		if err != nil {
			if ctx.Err() == nil {
				log.Error(opFinishStage, "a finish prompt hook could not be run", withField(fields, "error", err.Error()))
			}
			return "", err
		}
		if close.Failed() {
			log.Info(opFinishStage, "a finish prompt hook did not complete; the deploy is withdrawn", withField(fields, "close", close.String()))
			return "its finish prompt hook did not complete", nil
		}
		log.Info(opFinishStage, "a finish prompt hook succeeded", withField(fields, "close", close.String()))
		return "", nil
	case *repoconfigv1.Hook_Script:
		if err := o.deps.DB.PutFinishProgress(ctx, ws, wsm.FinishProgress{HookIndex: i}); err != nil {
			log.Error(opFinishStage, "could not record the finish stage's progress; its script is not run", withField(fields, "error", err.Error()))
			return "", err
		}
		logPath := LifecycleLogPath(o.deps.StateDir, lifecycleconfig.StageFinish, o.deps.Now(), i)
		fields = withField(withField(withField(fields, "script", h.Script.GetPath()), "log", logPath), "repo_root", ft.target.repoRoot)
		ran := RunKeptScript(ctx, o.deps.HookRunner, ft.target.repoRoot, h.Script, ft.target.owner.env(ft.dir, lifecycleconfig.StageFinish), logPath)
		failure := ran.Failure(h.Script)
		if failure == "" && ran.LogErr != nil {
			failure = fmt.Sprintf("it could not keep its log: %v", ran.LogErr)
		}
		if failure != "" {
			if ran.Err != nil && errors.Is(ctx.Err(), context.Canceled) {
				return "", ran.Err
			}
			log.Info(opFinishStage, "a finish script hook failed; the deploy is withdrawn", withField(fields, "failure", failure))
			return fmt.Sprintf("its finish script %s failed: %s (its output is in %s)", h.Script.GetPath(), failure, logPath), nil
		}
		log.Info(opFinishStage, "a finish script hook succeeded", withField(fields, "wall_ms", ran.Run.Wall.Milliseconds()))
		return "", nil
	}
	panic("merge: a finish hook the config resolved is neither a script nor a prompt")
}

// clearFinish forgets WS's finish progress once its stage has nothing left to
// run.
func (o *orchestrator) clearFinish(ctx context.Context, log dlog.Logger, fields dlog.Context, ws ids.WorkspaceID) error {
	if err := o.deps.DB.ClearFinishProgress(ctx, ws); err != nil {
		log.Error(opFinishStage, "could not clear the finish stage's progress", withField(fields, "error", err.Error()))
		return err
	}
	return nil
}

// withdrawFinished takes WS's request out of REPO's queue after its finish
// stage failed with FAILURE, before it was ever in line, and hands the failure
// back to its conversation. Nothing of the deploy was drawn, so there is no
// bubble to settle.
func (o *orchestrator) withdrawFinished(ctx context.Context, ws ids.WorkspaceID, repo wsm.RepoKey, failure string) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "failure": failure}
	if err := o.clearFinish(ctx, log, fields, ws); err != nil {
		return
	}
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, causeFinishFailed, nil); err != nil {
		log.Error(opFinishStage, "could not withdraw the deploy whose finish stage failed; it stays recorded", withField(fields, "error", err.Error()))
		return
	}
	o.mu.Lock()
	delete(o.repoOf, ws)
	delete(o.displaces, ws)
	o.mu.Unlock()
	log.Info(opFinishStage, "withdrew the deploy whose finish stage failed; the workspace is told", fields)
	o.handBackInPlace(ctx, ws, DetachedOutcome{Kind: DetachedFailed, Summary: "it was withdrawn before it was put in the queue: " + failure})
}
