// Package retarget changes where a workspace's merge lands AND tells the
// workspace's session, as one operation (agentrepl.v1.SetWorkspaceParent).
//
// A retarget nobody told the agent about is how work lands in the wrong place:
// the agent runs `/agent-repl-merge-queue one-shot` believing its merge lands where it did
// when it was created. So this package is the ONLY caller of the verb that
// records a retarget, and it never returns success unless the notice is queued
// in the workspace's session. A notice that cannot be queued undoes the
// retarget and the operation fails. Both surfaces — the rpc and the command
// file — call Retarget; neither has a second way in.
package retarget

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/wsm"
)

// op is the operation every record of this package carries.
const op = "daemon.retarget"

// BriefRetargeted is the brief the notice is composed from.
const BriefRetargeted = "workspace-retargeted"

// Origin is the notice's prompt origin: its own, as every daemon submit site's
// is.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_RETARGETED

// Verbs is the workspace verbs this package uses.
type Verbs interface {
	// SetParent records the retarget and republishes the roster.
	SetParent(ctx context.Context, ws ids.WorkspaceID, retarget wsm.Retarget) (wsm.ParentChange, error)
	// RestoreParent undoes a retarget unless a later one superseded it.
	RestoreParent(ctx context.Context, change wsm.ParentChange) error
}

// Prompts is the submission path every prompt takes: SubmitPrompt's own
// body, the one the command file's `prompt` entry calls.
type Prompts = prompthandler.Submitter

// Retargeter is the one operation both surfaces call.
type Retargeter interface {
	// Retarget applies RETARGET -- the workspace's parent and where its merge
	// lands -- and queues the notice in its session. A refusal is the verb's
	// typed refusal; any error means nothing changed.
	Retarget(ctx context.Context, ws ids.WorkspaceID, retarget wsm.Retarget) error
}

// Deps are the service's collaborators.
type Deps struct {
	Verbs   Verbs
	Prompts Prompts
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	Log    dlog.Surfaces
	// NewKey mints the notice's idempotency key; nil mints a random one.
	NewKey func() (string, error)
}

// Service is the retarget operation.
type Service struct {
	deps Deps
}

var _ Retargeter = (*Service)(nil)

// New builds the service.
func New(deps Deps) *Service {
	if deps.NewKey == nil {
		deps.NewKey = randomKey
	}
	return &Service{deps: deps}
}

// Retarget implements Retargeter.
//
// The notice is DEFERRED: it never interjects a running turn, it runs as the
// next turn once that one ends, and it wakes an idle agent at once. A session
// that is not up holds it until bring-up, the same as any submission.
func (s *Service) Retarget(ctx context.Context, ws ids.WorkspaceID, retarget wsm.Retarget) error {
	change, err := s.deps.Verbs.SetParent(ctx, ws, retarget)
	if err != nil {
		// The verb recorded its refusal or its failure where it happened.
		return err
	}
	log := s.deps.Log.Global().With(dlog.Context{
		"workspace": string(ws), "retarget": retarget.String(), "parent": ids.Optional(change.After.Parent), "target_dir": change.After.Dir,
	})
	turn, err := s.notify(ctx, change)
	if err != nil {
		// A SESSION PARKED AT ITS COLD GATE takes no prompt until the gate is
		// answered: that is the queue answering, refused by the gate's own
		// name (the rpc's cold_gate arm), not the notice breaking.
		var coldGate *promptqueue.ColdGateRefusal
		if errors.As(err, &coldGate) {
			log.Info(op, "the session is parked at its cold gate, so the notice cannot be queued; undoing the retarget", dlog.Context{"cause": err.Error()})
		} else {
			log.Error(op, "could not queue the retarget's notice; undoing the retarget", dlog.Context{"cause": err.Error()})
		}
		if undoErr := s.deps.Verbs.RestoreParent(ctx, change); undoErr != nil {
			return errors.Join(fmt.Errorf("retarget %s: notify: %w", ws, err), fmt.Errorf("retarget %s: undo: %w", ws, undoErr))
		}
		return fmt.Errorf("retarget %s: notify: %w", ws, err)
	}
	log.Info(op, "retargeted the workspace's merge and queued its notice", dlog.Context{"turn": string(turn)})
	return nil
}

// notify composes and queues the notice for one change.
func (s *Service) notify(ctx context.Context, change wsm.ParentChange) (ids.TurnID, error) {
	text, err := s.deps.Briefs(BriefRetargeted, map[string]string{
		"target":   Describe(change.After),
		"previous": Describe(change.Before),
	})
	if err != nil {
		return "", fmt.Errorf("compose: %w", err)
	}
	key, err := s.deps.NewKey()
	if err != nil {
		return "", fmt.Errorf("mint the idempotency key: %w", err)
	}
	outcome, err := s.deps.Prompts.Submit(ctx, change.Workspace, feed.SaidText(text), key, Origin, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		return "", fmt.Errorf("submit: %w", err)
	}
	return outcome.Turn, nil
}

// Describe names a retarget's side as the notice states it: where the merge
// lands -- a workspace's name and checkout, or the repository's main checkout
// and the default branch -- and the relationship: a child of a parent (of
// another repository when it is), or a top-level peer.
func Describe(target wsm.MergeTarget) string {
	lands := fmt.Sprintf("the repository's main checkout at `%s`, on its default branch `%s`", target.Dir, target.DefaultBranch)
	if target.LandsInto != nil {
		lands = fmt.Sprintf("the workspace `%s`, whose checkout is `%s`", target.LandsIntoName, target.Dir)
	}
	switch {
	case target.Parent == nil:
		return lands + ", as a top-level peer nobody coordinates"
	case target.CrossRepository:
		return fmt.Sprintf("%s, as a child of the workspace `%s` of another repository, which owns and coordinates it", lands, target.ParentName)
	}
	return fmt.Sprintf("%s, as a child of the workspace `%s`, which owns and coordinates it", lands, target.ParentName)
}

// randomKey mints a notice's idempotency key: every retarget is its own
// notice.
func randomKey() (string, error) {
	return prompthandler.MintKey("workspace-retargeted")
}
