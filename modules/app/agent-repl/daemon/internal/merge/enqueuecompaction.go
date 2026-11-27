package merge

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A WORKSPACE ENTERS THE MERGE QUEUE COMPACTED, ON SONNET (owner ruling,
// 2026-11-26). Once a finish's finish stage has run, and before its merge
// takes its place in line, the daemon compacts the workspace's conversation
// for remediation (conversation.v1.PromptOrigin.
// PROMPT_ORIGIN_MERGE_ENQUEUE_COMPACTION) and switches its session to the
// latest Sonnet. Every keep-alive while the merge waits, and every turn the
// queue hands the workspace (a failing test gate, a conflict), then reads a
// small Sonnet context. A merge that ends without landing hands the
// workspace back on the model it ran before, with no second compaction.
//
// THE ORDER IS STRUCTURAL. The compaction has ended before the merge is put
// in line (Deps.CompactForQueue answers only then), so nothing the queue sends can reach the uncompacted
// conversation; the switch and its restore are set-model acts on the
// workspace's prompt queue, which delivers them ahead of every prompt queued
// after them (workspace.verbs.SetModel), so the queue's first prompt runs on
// Sonnet and the handback runs on the model restored.
//
// A merge that lands directly (a deploy remediation's, FinishRemediation)
// never passes through here: the deploy mediator keeps its own model.

// BriefEnqueueCompaction is the instructions of the compaction a workspace's
// merge enters the queue behind: what the summary must keep for the
// remediation turns the queue may hand the workspace.
const BriefEnqueueCompaction = "merge-enqueue-compaction"

// EnqueueModel is the model a workspace's session runs on while its merge is
// in the queue. `sonnet` is the family alias the served catalog carries, so
// it names the latest Sonnet without pinning a version.
const EnqueueModel = "sonnet"

const opEnqueueCompaction = "daemon.merge.enqueue_compaction"

// compactForQueue compacts WS's conversation for remediation and switches its
// session to EnqueueModel, recording the model it ran on first so a merge that
// ends without landing restores it (restoreModel). Every failure is recorded
// here and answered, and the request is not put in line: a merge never
// enters the queue on the context it was meant to leave behind.
func (o *orchestrator) compactForQueue(ctx context.Context, ws ids.WorkspaceID) error {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws)}
	session, found, err := o.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(opEnqueueCompaction, "could not read the session whose merge enters the queue; it is not compacted", withField(fields, "error", err.Error()))
		return err
	}
	// THE FINISH WAS ASKED FROM A TURN, so a session stands; one missing is
	// a defect in what recorded the request.
	if !found {
		err := fmt.Errorf("merge: %s finished with no session recorded", ws)
		log.Error(opEnqueueCompaction, "the workspace whose merge enters the queue has no session; it is not compacted", withField(fields, "error", err.Error()))
		return err
	}
	fields["model"] = session.Model
	if err := o.deps.DB.HoldMergeRestoreModel(ctx, ws, session.Model); err != nil {
		log.Error(opEnqueueCompaction, "could not record the model the workspace returns to; it is not compacted", withField(fields, "error", err.Error()))
		return err
	}
	instructions, err := o.deps.Briefs(BriefEnqueueCompaction, nil)
	if err != nil {
		log.Error(opEnqueueCompaction, "could not compose the compaction's instructions; it is not compacted", withField(fields, "error", err.Error()))
		return err
	}
	if err := o.deps.CompactForQueue(ctx, ws, instructions); err != nil {
		log.Error(opEnqueueCompaction, "the compaction of the workspace whose merge enters the queue did not complete; the merge is not put in line", withField(fields, "error", err.Error()))
		return err
	}
	fields["switched_to"] = EnqueueModel
	if err := o.deps.SetModel(ctx, ws, EnqueueModel); err != nil {
		log.Error(opEnqueueCompaction, "could not switch the compacted workspace's model; the merge is not put in line", withField(fields, "error", err.Error()))
		return err
	}
	log.Info(opEnqueueCompaction, "compacted the workspace and switched its model; its merge goes in line", fields)
	return nil
}

// restoreModel switches WS's session back to the model it ran on before its
// merge entered the queue, when one is recorded: the merge ended without
// landing and the workspace is handed back. Nothing is compacted. A failure
// is recorded here; the handback goes on, since the workspace's own
// conversation is where the user sees it.
func (o *orchestrator) restoreModel(ctx context.Context, ws ids.WorkspaceID) {
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws)}
	model, held, err := o.deps.DB.TakeMergeRestoreModel(ctx, ws)
	if err != nil {
		log.Error(opEnqueueCompaction, "could not read the model the handed-back workspace returns to; its model is not restored", withField(fields, "error", err.Error()))
		return
	}
	if !held {
		return
	}
	fields["model"] = model
	if err := o.deps.SetModel(ctx, ws, model); err != nil {
		log.Error(opEnqueueCompaction, "could not switch the handed-back workspace back to its model", withField(fields, "error", err.Error()))
		return
	}
	log.Info(opEnqueueCompaction, "switched the handed-back workspace back to the model it ran before its merge", fields)
}
