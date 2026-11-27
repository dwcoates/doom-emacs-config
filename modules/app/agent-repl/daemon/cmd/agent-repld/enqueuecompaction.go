package main

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

// enqueuePrompts carries the merge orchestrator's enqueue compaction to the
// prompt handler, which is built after the orchestrator.
type enqueuePrompts struct {
	lateBound[prompthandler.Submitter]
	// awaitTurnEnd waits for the compaction's turn (workspace.Fleet.AwaitTurnEnd).
	awaitTurnEnd func(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error)
}

// compactForQueue submits the `/compact` a workspace's merge enters the queue
// behind and answers once its turn has ended (merge.Deps.CompactForQueue): an
// error when it did not complete. One asked for before the handler is bound
// is a wiring defect, answered as an error.
func (p *enqueuePrompts) compactForQueue(ctx context.Context, ws ids.WorkspaceID, instructions string) error {
	prompts, ok := p.load()
	if !ok {
		return fmt.Errorf("agent-repld: a merge's enqueue compaction was asked for before the prompt handler existed")
	}
	outcome, err := prompthandler.SubmitCompaction(ctx, prompts, ws, instructions,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_ENQUEUE_COMPACTION, prompthandler.MintKey, "merge-enqueue-compaction")
	if err != nil {
		return err
	}
	if outcome.Turn == "" {
		return fmt.Errorf("agent-repld: the enqueue compaction of %s answered no turn", ws)
	}
	closed, err := p.awaitTurnEnd(ctx, ws, outcome.Turn)
	if err != nil {
		return fmt.Errorf("agent-repld: wait for the enqueue compaction %s of %s: %w", outcome.Turn, ws, err)
	}
	if closed != wsm.CloseCompleted {
		return fmt.Errorf("agent-repld: the enqueue compaction %s of %s ended %s, not completed", outcome.Turn, ws, closed)
	}
	return nil
}

// setModel is the workspace verbs' queued model switch for the merge
// orchestrator, which is built before the verbs.
func (f *verbsForwarder) setModel(ctx context.Context, ws ids.WorkspaceID, model string) error {
	verbs, err := f.bound("a merge switched a workspace's model")
	if err != nil {
		return err
	}
	return verbs.SetModel(ctx, ws, model)
}
