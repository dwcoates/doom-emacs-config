package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opHomeQuestion is the operation a card's question sent to the home
// repository's main workspace is recorded under.
const opHomeQuestion = "daemon.workspace.home_question"

// BriefHomeQuestion is the prompt a feed card's `?` question is sent to the
// home repository's main workspace as (prompts/workspace-question-to-main.md).
const BriefHomeQuestion = "workspace-question-to-main"

// ErrNoHomeMainWorkspace is a home question with nowhere to go: the daemon's
// checkout is in no repository whose main worktree is a registered workspace.
var ErrNoHomeMainWorkspace = errors.New("workspace: the home repository registers no main workspace")

// AskHomeMain sends QUESTION, the card quoted with the person's words, from
// ASKER to the main workspace of the repository agent-repl's own checkout is
// in, as an agent-to-agent prompt from ASKER (PROMPT_ORIGIN_AGENT_SENT). It is
// DEFERRED, so it never interrupts the main workspace's running turn, and it
// opens that workspace first, which draws its tab. Answers the recipient.
//
// It is the one ownership point of the send's record: every way it falls
// short is logged here, then returned.
func (v *verbs) AskHomeMain(ctx context.Context, asker ids.WorkspaceID, question string) (wsm.Workspace, error) {
	record, log, err := v.owned(ctx, "SubmitPrompt", asker)
	if err != nil {
		return wsm.Workspace{}, err
	}
	log = log.With(dlog.Context{"checkout": v.deps.CheckoutRoot})
	main, err := v.deps.Git.MainWorktree(ctx, v.deps.CheckoutRoot)
	if err != nil {
		log.Error(opHomeQuestion, "git could not name the main worktree of agent-repl's own checkout", dlog.WithCause(nil, err))
		return wsm.Workspace{}, fmt.Errorf("ask the home main workspace: %w", err)
	}
	target, err := v.deps.DB.WorkspaceByDir(ctx, main)
	if errors.Is(err, wsm.ErrNotFound) {
		log.Error(opHomeQuestion, "the home repository has no registered main workspace to send the question to", dlog.Context{"main_worktree": main})
		return wsm.Workspace{}, fmt.Errorf("ask the home main workspace at %q: %w", main, ErrNoHomeMainWorkspace)
	}
	if err != nil {
		log.Error(opHomeQuestion, "could not read the home repository's main workspace", dlog.WithCause(dlog.Context{"main_worktree": main}, err))
		return wsm.Workspace{}, fmt.Errorf("ask the home main workspace at %q: %w", main, err)
	}
	log = log.With(dlog.Context{"recipient": string(target.ID), "recipient_name": target.Name})
	text, err := v.composeBrief(ctx, log, opHomeQuestion, BriefHomeQuestion, map[string]string{
		"asker":    record.Name,
		"question": question,
	})
	if err != nil {
		return wsm.Workspace{}, err
	}
	// OPEN IS IDEMPOTENT: a live main workspace is left as it is; a closed
	// one is reopened.
	if err := v.Open(ctx, target.ID, nil); err != nil {
		log.Error(opHomeQuestion, "could not open the home main workspace to send the question to", dlog.WithCause(nil, err))
		return wsm.Workspace{}, fmt.Errorf("open the home main workspace %s: %w", target.ID, err)
	}
	submission := deferredBrief(target.ID, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT)
	submission.SentBy = &asker
	if _, err := v.queueBrief(ctx, log, opHomeQuestion, "the home main workspace's", "the card's question", submission); err != nil {
		return wsm.Workspace{}, err
	}
	log.Info(opHomeQuestion, "sent the card's question to the home main workspace", dlog.Context{"turn": string(submission.Turn)})
	return target, nil
}
