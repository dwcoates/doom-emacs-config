package server

import (
	"context"
	"fmt"
	"slices"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

const opSetWorkspaceModel = "daemon.server.set_workspace_model"

// SetWorkspaceModel sets a workspace's model after compacting its
// conversation: the agent API's one way to set a model.
//
// THE ORDER IS STRUCTURAL, in four steps that each end the rpc on failure:
// the model is checked against the catalog ListCreationModels offers (so an
// unknown model compacts nothing), the compaction is submitted exactly as
// CompactWorkspace submits it, the compaction's turn ending is AWAITED (the
// session watcher's latch, bounded only by the caller's own context), and
// only a turn that COMPLETED lets the model be set. A compaction that did not
// complete leaves the model unchanged.
func (s *server) SetWorkspaceModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceModelRequest],
) (*connect.Response[agentreplv1.SetWorkspaceModelResponse], error) {
	const rpc = "SetWorkspaceModel"
	if err := validateSetWorkspaceModelRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspaceModelResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	model := req.Msg.GetModel().GetName()
	asker, by := askerOf(req.Msg.GetRequester())
	log := subject.Log.With(dlog.Context{"workspace": string(subject.Record.ID), "model": model,
		"requested_by": asker.String(), "requester": string(by)})

	if subject.Record.Closed {
		return answer(resp, s.refuse(log, opSetWorkspaceModel, resp, s.fill(refusal{Arm: "closed", Reason: "a closed workspace runs no session to compact", Info: true})))
	}
	if refused, err := s.checkCatalogModel(ctx, subject.Record, model); err != nil {
		return nil, fail(log, opSetWorkspaceModel, err)
	} else if refused != nil {
		return answer(resp, s.refuse(log, opSetWorkspaceModel, resp, s.fill(*refused)))
	}

	outcome, refused, err := s.submitCompaction(ctx, subject, nil)
	if err != nil {
		return nil, fail(log, opSetWorkspaceModel, err)
	}
	if refused != nil {
		return answer(resp, s.answerRefusal(log, opSetWorkspaceModel, resp, refused, nil))
	}
	if outcome.Turn == "" {
		return nil, fail(log, opSetWorkspaceModel, fmt.Errorf("the compaction of %s was submitted with no turn to await", subject.Record.ID))
	}
	log = log.With(dlog.Context{"turn": string(outcome.Turn)})

	closed, err := s.deps.AwaitTurnEnd(ctx, subject.Record.ID, outcome.Turn)
	if err != nil {
		return nil, fail(log, opSetWorkspaceModel, fmt.Errorf("await the compaction's end before setting the model: %w", err))
	}
	if closed != wsm.CloseCompleted {
		log.Error(opSetWorkspaceModel, "the compaction did not complete, so the model was left unchanged", dlog.Context{"how": closed.String()})
		return answer(resp, s.refuse(log, opSetWorkspaceModel, resp, s.fill(refusal{
			Arm:    "compaction_failed",
			Reason: fmt.Sprintf("the compaction ended %s", closed),
			Fields: map[string]any{"how": closed.String()},
		})))
	}
	if err := s.deps.Verbs.SetModel(ctx, subject.Record.ID, model); err != nil {
		log.Error(opSetWorkspaceModel, "the compaction completed and the model change was refused", dlog.WithCause(nil, err))
		return answer(resp, s.answerRefusal(log, opSetWorkspaceModel, resp, err, nil))
	}
	log.Info(opSetWorkspaceModel, "compacted the workspace and set its model", nil)
	resp.Result = &agentreplv1.SetWorkspaceModelResponse_Success{Success: &agentreplv1.SetWorkspaceModelSuccess{}}
	return connect.NewResponse(resp), nil
}

// checkCatalogModel checks MODEL against the catalog ListCreationModels
// offers for the workspace's repository, the catalog a create's model is
// checked against. It answers the refusal for a model the catalog does not
// offer or a catalog never served, and an error when the registry or the
// catalog cannot be read.
func (s *server) checkCatalogModel(ctx context.Context, record wsm.Workspace, model string) (*refusal, error) {
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		return nil, fmt.Errorf("list the repositories: %w", err)
	}
	repository, found := wsm.RepositoryWithID(repositories, record.Repo)
	if !found {
		return nil, fmt.Errorf("repository %q of workspace %q is not registered", record.Repo, record.ID)
	}
	catalog, err := s.deps.Verbs.CreationCatalog(ctx, repository.Dir)
	if err != nil {
		return nil, err
	}
	if !catalog.Known {
		return &refusal{Arm: "model_catalog_unknown", Info: true,
			Reason: fmt.Sprintf("the model %q cannot be checked: no session on login %q has served a model catalog", model, catalog.ConfigDir),
			Fields: map[string]any{"model": model, "config_dir": catalog.ConfigDir}}, nil
	}
	names := catalog.Names()
	if slices.Contains(names, model) {
		return nil, nil
	}
	return &refusal{Arm: workspace.ArmModelNotInCatalog, Info: true,
		Reason: fmt.Sprintf("the model %q is not in the catalog of login %q %v", model, catalog.ConfigDir, names),
		Fields: map[string]any{"model": model, "config_dir": catalog.ConfigDir, "options": names}}, nil
}
