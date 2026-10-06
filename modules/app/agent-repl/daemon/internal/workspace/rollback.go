package workspace

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
)

const opRollBack = "daemon.workspace.roll_back"

// RollbackRequest is a rollback the user confirmed.
type RollbackRequest struct {
	// Turns is the prompt's turn and every later turn the rollback drops,
	// oldest first.
	Turns []ids.TurnID
	// Since is when the prompt's turn was opened: held prompts queued at or
	// after it are dropped.
	Since time.Time
	// DropHeld is the held prompts the confirmed plan said would be dropped.
	DropHeld []ids.TurnID
	// RestoreFiles restores the files and stops the dropped turns' detached
	// work.
	RestoreFiles bool
}

// RollbackResult is a rollback that happened.
type RollbackResult struct {
	// FilesRestored is how many files the restore changed back; zero when
	// files were kept.
	FilesRestored int
}

// RollBack performs a confirmed rollback as ONE operation owned by the prompt
// queue (promptqueue.RollBack): no prompt is delivered while it runs. The
// shim rewinds the vendor conversation (interrupting the open turn, and when
// restoring files, stopping the dropped turns' detached work and restoring
// the files); only then are the planned held prompts dropped and the dropped
// turns removed from the feed for good.
//
// A shim refusal comes back as a *ShimRefusal naming its arm, and a queue
// that changed since the plan as promptqueue.ErrHoldsChanged; either leaves
// the conversation, the queue and the feed as they were.
func (v *verbs) RollBack(ctx context.Context, ws ids.WorkspaceID, req RollbackRequest) (RollbackResult, error) {
	_, log, err := v.owned(ctx, "RollBack", ws)
	if err != nil {
		return RollbackResult{}, err
	}
	log = log.With(dlog.Context{"turns": len(req.Turns), "to_before": firstTurnOf(req.Turns), "restore_files": req.RestoreFiles, "drop_held": len(req.DropHeld)})
	if len(req.Turns) == 0 {
		err := errors.New("workspace: a rollback names no turn")
		log.Error(opRollBack, "a rollback naming no turn reached the verb", dlog.Context{"cause": err.Error()})
		return RollbackResult{}, err
	}
	shim, live := v.deps.Shim(ws)
	if !live {
		refusal := &ShimRefusal{Verb: "RollBackSession", Arm: ArmShimNoSession, Detail: "the workspace has no running session"}
		log.Info(opRollBack, "the rollback was refused: no session is running", nil)
		return RollbackResult{}, refusal
	}
	var restored []string
	err = v.deps.Queue.RollBack(ctx, ws, req.Since, req.DropHeld, func(ctx context.Context) error {
		paths, err := shim.RollBackSession(ctx, req.Turns, req.RestoreFiles)
		restored = paths
		return err
	})
	if err != nil {
		if refusal, ok := AsShimRefusal(err); ok {
			log.Info(opRollBack, "the shim refused the rollback; nothing was rolled back",
				dlog.Context{"arm": refusal.Arm, "detail": refusal.Detail})
			return RollbackResult{}, err
		}
		if errors.Is(err, promptqueue.ErrHoldsChanged) {
			return RollbackResult{}, err
		}
		log.Error(opRollBack, "the rollback failed", dlog.Context{"cause": err.Error()})
		return RollbackResult{}, err
	}
	// THE FEED'S REMOVAL ALWAYS HAPPENS; an error means only that recording it
	// durably failed, which the feed has already logged and raised. The vendor
	// conversation is already rolled back, so the rollback succeeded.
	_ = v.deps.Feed.RollBackTurns(ws, req.Turns)
	log.Info(opRollBack, "the conversation was rolled back", dlog.Context{"files_restored": len(restored)})
	return RollbackResult{FilesRestored: len(restored)}, nil
}

func firstTurnOf(turns []ids.TurnID) string {
	if len(turns) == 0 {
		return ""
	}
	return string(turns[0])
}

// RollBackSession asks the shim to rewind the vendor conversation to just
// before the first of TURNS, answering the paths a restore changed back.
func (a *shimAdapter) RollBackSession(ctx context.Context, turns []ids.TurnID, restoreFiles bool) ([]string, error) {
	return rollBackSession(ctx, a.client, turns, restoreFiles)
}

// sessionRollBacker is the one shim call rollBackSession needs, satisfied by
// both the verbs' whole client and the queue's narrowed one.
type sessionRollBacker interface {
	RollBackSession(ctx context.Context, req *shimv1.RollBackSessionRequest) (*shimv1.RollBackSessionResponse, error)
}

// rollBackSession is THE ONE BUILDER of a RollBackSession request and the one
// reading of its answer, shared by the verbs' rollback and the queue's rewind
// of a failed try (sender.RollBackTurn), as killTurn is for KillTurn.
func rollBackSession(ctx context.Context, client sessionRollBacker, turns []ids.TurnID, restoreFiles bool) ([]string, error) {
	req := &shimv1.RollBackSessionRequest{ToBefore: &conversationv1.TurnId{Value: string(turns[0])}}
	for _, turn := range turns {
		req.DroppedTurns = append(req.DroppedTurns, &conversationv1.TurnId{Value: string(turn)})
	}
	if restoreFiles {
		req.Files = &shimv1.RollBackSessionRequest_RestoreFiles{RestoreFiles: &shimv1.RollBackSessionRestoreFiles{}}
	} else {
		req.Files = &shimv1.RollBackSessionRequest_KeepFiles{KeepFiles: &shimv1.RollBackSessionKeepFiles{}}
	}
	response, err := client.RollBackSession(ctx, req)
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		arm, vendor := rollBackSessionArm(failure)
		detail := failure.GetDetail()
		if vendor != "" {
			detail = vendor
		}
		return nil, &ShimRefusal{Verb: "RollBackSession", Arm: arm, Detail: detail}
	}
	return response.GetSuccess().GetFilesRestored().GetPaths(), nil
}
