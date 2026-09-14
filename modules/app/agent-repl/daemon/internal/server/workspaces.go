package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// The workspace lifecycle verbs. Every one of them resolves the echoed ref,
// refuses a workspace this daemon does not serve, and DELEGATES; the policy
// (what quiet means for a close, what a nuke destroys) lives in the verbs.

// saidText flattens a composed prompt into the text the creation spec takes.
//
// SEAM MISMATCH (recorded in the report): workspace.CreateSpec.InitialPrompt is
// a STRING while the contract carries conversation.v1.UserSaid, so an image
// block attached to a creation prompt has nowhere to go. The text blocks are
// joined verbatim and the loss is logged rather than hidden.
func (s *server) saidText(log dlog.Logger, field string, said *conversationv1.UserSaid) string {
	if said == nil {
		return ""
	}
	var parts []string
	dropped := 0
	for _, block := range said.GetContent().GetBlocks() {
		if text := block.GetText(); text != nil {
			parts = append(parts, text.GetText())
			continue
		}
		dropped++
	}
	if dropped > 0 {
		log.Warn("daemon.server.create_workspace",
			"a creation prompt carried non-text blocks the creation spec cannot hold",
			dlog.Context{"field": field, "dropped": dropped})
	}
	return strings.Join(parts, "\n")
}

// repositoryDir resolves a RepositoryRef against the registry, keyed on `id`
// and falling back to the dir when the ref names only one.
func (s *server) repositoryDir(ctx context.Context, ref *workspacev1.RepositoryRef) (string, error, bool) {
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		return "", err, false
	}
	for _, repository := range repositories {
		if id := ref.GetId(); id != "" && string(repository.ID) == id {
			return repository.Dir, nil, true
		}
		if dir := ref.GetDir(); dir != "" && repository.Dir == dir {
			return repository.Dir, nil, true
		}
	}
	return "", nil, false
}

// CreateWorkspace materializes a new workspace, standard or one-shot. The
// daemon names, branches and creates everything; registration happens only
// AFTER materialization.
func (s *server) CreateWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateWorkspaceRequest],
) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	const rpc = "CreateWorkspace"
	if err := validateCreateWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CreateWorkspaceResponse{}
	dir, err, known := s.repositoryDir(ctx, req.Msg.GetRepository())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if !known {
		return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
			Arm:      "unknown_repository",
			Reason:   fmt.Sprintf("no repository matches the ref %q", req.Msg.GetRepository().GetId()),
			NotFound: true,
		})))
	}

	spec := workspace.CreateSpec{RepoDir: dir}
	if standard := req.Msg.GetStandard(); standard != nil {
		spec.InitialPrompt = s.saidText(s.log, "standard.initial_prompt", standard.GetInitialPrompt())
		spec.BaseRef = standard.GetBaseRef()
		spec.Name = standard.GetName()
		spec.MergeActions = s.mergeActions(standard.GetMergeActions())
	}
	if oneShot := req.Msg.GetOneShot(); oneShot != nil {
		spec.OneShot = true
		spec.InitialPrompt = s.saidText(s.log, "one_shot.prompt", oneShot.GetPrompt())
	}
	if parent := req.Msg.GetParent(); parent != nil {
		id := ids.WorkspaceID(parent.GetWorkspace().GetId())
		spec.Parent = &id
		if parent.GetFork() != nil {
			spec.ForkFrom = &id
		}
	}
	if req.Msg.Model != nil {
		spec.Model = req.Msg.GetModel()
	}
	if req.Msg.GetAllowUngated() != nil {
		// The consent is recorded against the mode the creation asks for; the
		// verbs refuse an ungated mode with none.
		spec.ConsentedUngatedMode = spec.PermissionMode
	}
	if req.Msg.Priority != nil {
		priority := priorityOf(req.Msg.GetPriority())
		spec.Priority = &priority
	}

	created, err := s.deps.Verbs.Create(ctx, spec)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.create_workspace", "created a workspace",
		dlog.Context{"workspace": string(created.ID), "dir": created.Dir})
	resp.Result = &agentreplv1.CreateWorkspaceResponse_Success{
		Success: &agentreplv1.CreateWorkspaceSuccess{Workspace: refOf(created)},
	}
	return connect.NewResponse(resp), nil
}

// mergeActions renders the configured merge actions.
//
// SEAM MISMATCH (recorded in the report): wsm.MergeActions holds PROMPT NAMES
// while the contract carries composed UserSaid prompts, so the composed text is
// carried through as the "name" until the two agree.
func (s *server) mergeActions(actions *agentreplv1.CreateWorkspaceMergeActions) wsm.MergeActions {
	out := wsm.MergeActions{}
	if actions == nil {
		return out
	}
	if before := actions.GetBeforeWsMerge(); before != nil {
		out.Before = append(out.Before, s.saidText(s.log, "merge_actions.before_ws_merge", before))
	}
	if after := actions.GetPostprocessingPrompt(); after != nil {
		out.After = append(out.After, s.saidText(s.log, "merge_actions.postprocessing_prompt", after))
	}
	return out
}

// priorityOf renders a wire priority as the registry's.
func priorityOf(p *agentreplv1.WorkspacePriority) wsm.Priority {
	switch {
	case p.GetP05() != nil:
		return wsm.PriorityP05
	case p.GetP1() != nil:
		return wsm.PriorityP1
	case p.GetP2() != nil:
		return wsm.PriorityP2
	default:
		return wsm.PriorityP3
	}
}

// RegisterWorkspace records a workspace Emacs announced. It is idempotent by
// normalized dir and is NOT a per-workspace verb: there is no ref to key on
// yet, so no ownership refusal applies.
func (s *server) RegisterWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspaceRequest],
) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	const rpc = "RegisterWorkspace"
	if err := validateRegisterWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RegisterWorkspaceResponse{}
	record, err := s.deps.Verbs.Register(ctx, req.Msg.GetDir(), wsm.RegisterFacts{})
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.register_workspace", "registered a workspace",
		dlog.Context{"workspace": string(record.ID), "dir": record.Dir})
	resp.Result = &agentreplv1.RegisterWorkspaceResponse_Success{
		Success: &agentreplv1.RegisterWorkspaceSuccess{Workspace: refOf(record)},
	}
	return connect.NewResponse(resp), nil
}

// RegisterRepository records a repository resolved from any path inside it,
// and registers that repository's main worktree as an open workspace. It is
// NOT a per-workspace verb -- the REQUEST carries no ref to key on -- so no
// ownership refusal applies; the workspace in the answer is minted here,
// exactly as RegisterWorkspace mints the one it answers with.
func (s *server) RegisterRepository(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterRepositoryRequest],
) (*connect.Response[agentreplv1.RegisterRepositoryResponse], error) {
	const rpc = "RegisterRepository"
	if err := validateRegisterRepositoryRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RegisterRepositoryResponse{}
	registered, err := s.deps.Verbs.RegisterRepository(ctx, req.Msg.GetPath())
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.register_repository", "registered a repository and its main worktree",
		dlog.Context{
			"path": req.Msg.GetPath(), "repository": string(registered.Repository.ID),
			"dir": registered.Repository.Dir, "already_known": registered.RepositoryAlreadyKnown,
			"workspace":               string(registered.Workspace.ID),
			"workspace_already_known": registered.WorkspaceAlreadyKnown,
		})
	resp.Result = &agentreplv1.RegisterRepositoryResponse_Success{
		Success: &agentreplv1.RegisterRepositorySuccess{
			Repository: &workspacev1.RepositoryRef{
				Id: string(registered.Repository.ID), Dir: registered.Repository.Dir,
			},
			AlreadyKnown:          registered.RepositoryAlreadyKnown,
			Workspace:             refOf(registered.Workspace),
			WorkspaceAlreadyKnown: registered.WorkspaceAlreadyKnown,
		},
	}
	return connect.NewResponse(resp), nil
}

// OpenWorkspace spawns a registered-but-closed workspace's session.
func (s *server) OpenWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenWorkspaceRequest],
) (*connect.Response[agentreplv1.OpenWorkspaceResponse], error) {
	const rpc = "OpenWorkspace"
	resp := &agentreplv1.OpenWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Open(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.OpenWorkspaceResponse_Success{
		Success: &agentreplv1.OpenWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// CloseWorkspace tears down a workspace's editor state. It requires quiet, and
// the refusal manifests in the footer as well as in this answer.
func (s *server) CloseWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseWorkspaceRequest],
) (*connect.Response[agentreplv1.CloseWorkspaceResponse], error) {
	const rpc = "CloseWorkspace"
	resp := &agentreplv1.CloseWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Close(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.CloseWorkspaceResponse_Success{
		Success: &agentreplv1.CloseWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// KillWorkspace is the big red button: forced session death, never blocking.
func (s *server) KillWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.KillWorkspaceRequest],
) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
	const rpc = "KillWorkspace"
	resp := &agentreplv1.KillWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Kill(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.KillWorkspaceResponse_Success{
		Success: &agentreplv1.KillWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// NukeWorkspace destroys data: kill if live, then delete the worktree and the
// branch, then forget the record.
func (s *server) NukeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.NukeWorkspaceRequest],
) (*connect.Response[agentreplv1.NukeWorkspaceResponse], error) {
	const rpc = "NukeWorkspace"
	resp := &agentreplv1.NukeWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Nuke(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.NukeWorkspaceResponse_Success{
		Success: &agentreplv1.NukeWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// ForgetWorkspace removes a CLOSED workspace's registry record and touches no
// file. It is the undo for a registration, and the verb it delegates to owns
// every refusal: an open workspace, a workspace that is not quiet, and a
// workspace others were spawned from. The roster's new state arrives on its
// own stream, as a nuke's does.
//
// No PublishHostWorkspace follows it, deliberately, and this is the one
// lifecycle verb where that is so: the record the host view is composed FROM
// is gone by the time the verb returns, so a recompose could only fail to
// resolve the workspace it was asked to draw.
func (s *server) ForgetWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.ForgetWorkspaceRequest],
) (*connect.Response[agentreplv1.ForgetWorkspaceResponse], error) {
	const rpc = "ForgetWorkspace"
	resp := &agentreplv1.ForgetWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Forget(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.ForgetWorkspaceResponse_Success{
		Success: &agentreplv1.ForgetWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// MergeWorkspace enqueues the workspace's merge. Its life from there is the
// feed's merge bubble.
func (s *server) MergeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.MergeWorkspaceRequest],
) (*connect.Response[agentreplv1.MergeWorkspaceResponse], error) {
	const rpc = "MergeWorkspace"
	resp := &agentreplv1.MergeWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Merge.Enqueue(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.MergeWorkspaceResponse_Success{
		Success: &agentreplv1.MergeWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// RestartWorkspace bounces the workspace's shim, gracefully unless forced.
func (s *server) RestartWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RestartWorkspaceRequest],
) (*connect.Response[agentreplv1.RestartWorkspaceResponse], error) {
	const rpc = "RestartWorkspace"
	resp := &agentreplv1.RestartWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Restart(ctx, subject.Record.ID, req.Msg.GetForce()); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.RestartWorkspaceResponse_Success{
		Success: &agentreplv1.RestartWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// SelectWorkspace records the user's switch to this workspace and clears its
// attention marker. It is idempotent.
func (s *server) SelectWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectWorkspaceRequest],
) (*connect.Response[agentreplv1.SelectWorkspaceResponse], error) {
	const rpc = "SelectWorkspace"
	resp := &agentreplv1.SelectWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.Select(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SelectWorkspaceResponse_Success{
		Success: &agentreplv1.SelectWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// SetWorkspacePriority sets or clears the roster's ordering priority. An unset
// priority is the CLEAR spelling.
func (s *server) SetWorkspacePriority(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspacePriorityRequest],
) (*connect.Response[agentreplv1.SetWorkspacePriorityResponse], error) {
	const rpc = "SetWorkspacePriority"
	if err := validateSetWorkspacePriorityRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspacePriorityResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var priority *wsm.Priority
	if req.Msg.Priority != nil {
		value := priorityOf(req.Msg.GetPriority())
		priority = &value
	}
	if err := s.deps.Verbs.SetPriority(ctx, subject.Record.ID, priority); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetWorkspacePriorityResponse_Success{
		Success: &agentreplv1.SetWorkspacePrioritySuccess{},
	}
	return connect.NewResponse(resp), nil
}

// subjectFor validates a bare {workspace} request, resolves the ref and refuses
// an unowned workspace. `done` reports that the caller must answer at once —
// with the refusal already encoded onto resp, or with the Connect error.
func (s *server) subjectFor(
	ctx context.Context,
	rpc string,
	ref *workspacev1.WorkspaceRef,
	resp proto.Message,
) (resolved, *connect.Error, bool) {
	if err := validateWorkspaceRef("workspace", ref); err != nil {
		return resolved{}, err, true
	}
	subject, r, err := s.resolveRef(ctx, rpc, ref)
	if err != nil {
		return resolved{}, fail(s.log, rpc, err), true
	}
	if r != nil {
		return resolved{}, s.refuse(s.log, rpc, resp, *r), true
	}
	return subject, nil, false
}
