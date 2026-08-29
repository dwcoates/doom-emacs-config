package workspace

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// opOneShotFinish is the operation the finish hook's records carry.
const opOneShotFinish = "daemon.workspace.one_shot_finish"

// OnOneShotTurnConcluded takes a one-shot workspace's FINISH ACTION.
//
// The prompt queue owns detecting that a turn concluded with the success
// marker; this package does not. This verb owns what such a conclusion MEANS,
// which is the action recorded in the creation job before the worktree ever
// existed:
//
//   - self_merge enqueues the workspace's own merge;
//   - open_pr submits the CICD-gated wrap-up as a POST-PROMPT.
//
// The action is SPENT as it runs — the creation job's finish is cleared in the
// same call — so a second conclusion (a follow-up turn, a re-drive after a
// restart) takes it exactly once. A workspace that is not a one-shot, or whose
// finish is already spent, is a NO-OP rather than an error: the queue calls
// this on every conclusion and only some of them owe an action.
func (v *verbs) OnOneShotTurnConcluded(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) error {
	record, log, err := v.owned(ctx, "SubmitPrompt", ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"turn": string(turn)})

	job, ok, err := v.deps.DB.CreationJob(ctx, ws)
	if err != nil {
		log.Error(opOneShotFinish, "could not read the creation job", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("one-shot finish on %q: read the creation job: %w", ws, err)
	}
	if !ok || !job.OneShot {
		log.Debug(opOneShotFinish, "the workspace owes no one-shot finish", dlog.Context{
			"has_creation_job": ok, "one_shot": ok && job.OneShot,
		})
		return nil
	}
	if job.Finish == "" {
		log.Debug(opOneShotFinish, "the one-shot finish was already spent", nil)
		return nil
	}

	finish, err := parseFinishOrigin(job.Finish)
	if err != nil {
		log.Error(opOneShotFinish, "the recorded finish does not parse", dlog.Context{
			"finish": job.Finish, "cause": err.Error(),
		})
		return fmt.Errorf("one-shot finish on %q: %w", ws, err)
	}

	// The finish is SPENT BEFORE it is taken. Acting first and clearing second
	// would re-enqueue the merge if the clear failed, and one merge enqueued
	// twice is worse than one finish the user has to ask for again.
	spent := job
	spent.Finish = ""
	if err := v.deps.DB.PutCreationJob(ctx, spent); err != nil {
		log.Error(opOneShotFinish, "could not spend the recorded finish", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("one-shot finish on %q: spend the finish: %w", ws, err)
	}

	switch {
	case finish.SelfMerge:
		if err := v.deps.Merge.Enqueue(ctx, ws); err != nil {
			log.Error(opOneShotFinish, "could not enqueue the self merge", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("one-shot finish on %q: enqueue the merge: %w", ws, err)
		}
		log.Info(opOneShotFinish, "enqueued the one-shot self merge", nil)
		return nil

	case finish.OpenPr != nil:
		text, err := v.openPrFollowup(finish.OpenPr)
		if err != nil {
			log.Error(opOneShotFinish, "could not compose the pull-request follow-up", dlog.Context{"cause": err.Error()})
			return refuse(log, "SubmitPrompt", ArmBriefMissing, err.Error(), false)
		}
		if err := v.submitFollowup(ctx, log, record, text); err != nil {
			return err
		}
		log.Info(opOneShotFinish, "submitted the one-shot pull-request follow-up", nil)
		return nil

	default:
		return fmt.Errorf("one-shot finish on %q: the recorded finish names no action", ws)
	}
}

// submitFollowup sends one daemon-composed post-prompt down the ONE delivery
// path. Its origin is the deferred-prompt origin, because that is exactly what
// it is: a prompt the daemon held until the turn ended. The whole text is
// meta-wrapped — the user typed none of it.
func (v *verbs) submitFollowup(ctx context.Context, log dlog.Logger, record wsm.Workspace, text string) error {
	turn := wsm.NewTurnID()
	origin := conversationv1.PromptOrigin_PROMPT_ORIGIN_DEFERRED_PROMPT
	if err := v.deps.DB.PutTurn(ctx, wsm.Turn{
		ID:        turn,
		Workspace: record.ID,
		Text:      text,
		Origin:    origin.String(),
		StartedAt: v.now(),
	}); err != nil {
		log.Error(opOneShotFinish, "could not record the follow-up turn", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("one-shot finish on %q: record the follow-up turn: %w", record.ID, err)
	}
	if _, err := v.deps.Queue.Submit(ctx, daemonSubmission(record.ID, turn, SaidText(metaWrap(text)), origin)); err != nil {
		log.Error(opOneShotFinish, "the follow-up prompt was not accepted", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("one-shot finish on %q: submit the follow-up: %w", record.ID, err)
	}
	return nil
}
