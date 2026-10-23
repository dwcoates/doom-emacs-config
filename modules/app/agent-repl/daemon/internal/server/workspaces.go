package server

import (
	"time"

	"agent-repld/internal/creategate"
	"agent-repld/internal/runtimemetrics"
	"context"
	"errors"
	"fmt"
	"strings"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/semver"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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

// repositoryFor resolves a RepositoryRef against the registry, keyed on `id`
// and falling back to the dir when the ref names only one. The bool reports
// that a registered repository matched.
func (s *server) repositoryFor(ctx context.Context, ref *workspacev1.RepositoryRef) (wsm.Repository, error, bool) {
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		return wsm.Repository{}, err, false
	}
	if id := ref.GetId(); id != "" {
		if repository, found := wsm.RepositoryWithID(repositories, ids.RepoID(id)); found {
			return repository, nil, true
		}
	}
	if dir := ref.GetDir(); dir != "" {
		if repository, found := wsm.RepositoryAt(repositories, dir); found {
			return repository, nil, true
		}
	}
	return wsm.Repository{}, nil, false
}

// resolveRepository resolves REF for RPC, or says the caller must answer at
// once: `done` is true with the unknown_repository refusal already on RESP
// (cerr nil), or with the read's failure as cerr. It is the one spelling of
// that refusal for every rpc that names a repository.
func (s *server) resolveRepository(
	ctx context.Context,
	rpc string,
	resp proto.Message,
	ref *workspacev1.RepositoryRef,
) (wsm.Repository, *connect.Error, bool) {
	repository, err, known := s.repositoryFor(ctx, ref)
	if err != nil {
		return wsm.Repository{}, fail(s.log, rpc, err), true
	}
	if !known {
		return wsm.Repository{}, s.refuse(s.log, rpc, resp, s.fill(refusal{
			Arm:      "unknown_repository",
			Reason:   fmt.Sprintf("no repository matches the ref %q", ref.GetId()),
			NotFound: true,
		})), true
	}
	return repository, nil, false
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
	repository, cerr, done := s.resolveRepository(ctx, rpc, resp, req.Msg.GetRepository())
	if done {
		return answer(resp, cerr)
	}
	if async := req.Msg.GetAsyncMerge(); async != nil {
		return s.createAsyncMerge(ctx, rpc, resp, repository, req.Msg)
	}

	spec := workspace.CreateSpec{
		RepoDir:   repository.Dir,
		Requester: requesterID(req.Msg.GetRequester()),
		ForkFrom:  forkSourceID(req.Msg.GetContextOrigin()),
	}
	if standard := req.Msg.GetStandard(); standard != nil {
		spec.InitialPrompt = s.saidText(s.log, "standard.initial_prompt", standard.GetInitialPrompt())
		spec.Name = standard.GetName()
		spec.DeployHooks = deployHooksOf(standard.GetDeployHooks())
		spec.Parent, spec.OwnerNotice = topologyOwner(standard.GetTopology())
		spec.FromOrigin = standard.GetTopology().GetPrimary().GetFromOrigin() != nil
	}
	if oneShot := req.Msg.GetOneShot(); oneShot != nil {
		spec.OneShot = true
		spec.InitialPrompt = s.saidText(s.log, "one_shot.prompt", oneShot.GetPrompt())
		spec.Parent, spec.OwnerNotice = topologyOwner(oneShot.GetTopology())
		spec.FromOrigin = oneShot.GetTopology().GetPrimary().GetFromOrigin() != nil
		if about := oneShot.GetContext(); about != nil {
			// The context must be a registered workspace this daemon serves; a
			// refusal names it on the create's own arm.
			subject, refused, done := s.resolveNaming(ctx, rpc, resp, about.GetWorkspace(),
				map[string]string{workspace.ArmUnknownWorkspace: "unknown_context_workspace"})
			if done {
				return answer(resp, refused)
			}
			id := subject.Record.ID
			spec.ContextWorkspace = &id
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

	// THE WORK RUNS DETACHED FROM THE REQUEST CONTEXT in both forms. bgCtx
	// keeps this request's values but drops its cancellation, so a client
	// deadline or disconnect can never cancel `git worktree add` mid-run — the
	// defect that silently failed a create. It ends only with the daemon.
	// A CREATE HOLDS A TICKET FOR ITS WHOLE RUN, released only once its
	// outcome is answered or published (creategate): a stand-down closes the
	// gate and waits the tickets out, so nothing it snapshots or exits over
	// is a create still running. One the gate will not admit is refused here,
	// before anything was named or built.
	admitted, release, err := s.deps.Creates.Admit(ctx)
	if err != nil {
		return answer(resp, s.refuseUnadmitted(rpc, resp, err))
	}
	bgCtx, cancel := s.backgroundContext(admitted)

	// OPTION B: a create that carries an op_id is ACKED NOW and worked in the
	// background. The rpc returns at once so a slow naming call or worktree add
	// never leaves the client without feedback, and every stage and the
	// terminal outcome ride WatchDaemon keyed on the op_id
	// (workspace_mutation_progress.proto). A create WITHOUT an op_id keeps the
	// legacy synchronous contract: the same detached work, but this rpc waits
	// for it and answers success/error itself.
	if opID := req.Msg.GetOpId(); opID != "" {
		spec.Progress = createProgressReporter{server: s, opID: opID}
		go func() {
			// The goroutine OWNS the cancel: cancelling in the handler would
			// tear bgCtx down the instant the ack returns, which is the very
			// cancellation this detaches from. It owns the ticket too, which is
			// released after the outcome is published.
			defer cancel()
			defer release()
			s.runBackgroundCreate(bgCtx, rpc, spec, opID)
		}()
		resp.Result = &agentreplv1.CreateWorkspaceResponse_Accepted{
			Accepted: &agentreplv1.CreateWorkspaceAccepted{OpId: opID},
		}
		s.log.Info("daemon.server.create_workspace", "accepted a create and detached it to the background",
			dlog.Context{"op_id": opID})
		return connect.NewResponse(resp), nil
	}

	defer cancel()
	defer release()
	created, err := s.deps.Verbs.Create(bgCtx, spec)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info("daemon.server.create_workspace", "created a workspace",
		dlog.Context{"workspace": string(created.ID), "dir": created.Dir})
	resp.Result = &agentreplv1.CreateWorkspaceResponse_Success{
		Success: &agentreplv1.CreateWorkspaceSuccess{Workspace: created.Ref()},
	}
	return connect.NewResponse(resp), nil
}

// refuseUnadmitted answers a create the gate did not admit: transferring_away
// naming the successor the create is to be sent to, or standing_down when none
// is listening. Both are the stand-down's ordinary answer, never a fault.
func (s *server) refuseUnadmitted(rpc string, resp proto.Message, err error) *connect.Error {
	var closed *creategate.ClosedError
	if !errors.As(err, &closed) {
		return fail(s.log, rpc, err)
	}
	if closed.Successor != "" {
		return s.refuse(s.log, rpc, resp, s.fill(refusal{
			Arm:    workspace.ArmTransferringAway,
			Reason: err.Error(),
			Fields: map[string]any{"address": closed.Successor},
			Info:   true,
		}))
	}
	return s.refuse(s.log, rpc, resp, s.fill(refusal{Arm: "standing_down", Reason: err.Error(), Info: true}))
}

// requesterID is the requesting workspace's id, nil when the user asked.
// Whether it is registered is the create's to judge (unknown_requester).
func requesterID(r *agentreplv1.WorkspaceRequester) *ids.WorkspaceID {
	asker := r.GetWorkspace()
	if asker == nil {
		return nil
	}
	id := ids.WorkspaceID(asker.GetWorkspace().GetId())
	return &id
}

// forkSourceID is the workspace whose conversation is forked, nil for a fresh
// one. Whether it is a workspace of the repository is the create's to judge
// (unknown_fork_source).
func forkSourceID(c *agentreplv1.WorkspaceContextOrigin) *ids.WorkspaceID {
	fork := c.GetFork()
	if fork == nil {
		return nil
	}
	id := ids.WorkspaceID(fork.GetWorkspace().GetId())
	return &id
}

// topologyOwner is a child's owner and its notice to the owner, nil and empty
// for a workspace cut from the primary branch. Whether the owner is a
// workspace of the repository, and whether the notice is required, is the
// create's to judge.
func topologyOwner(t *agentreplv1.WorkspaceTopology) (*ids.WorkspaceID, string) {
	child := t.GetChild()
	if child == nil {
		return nil, ""
	}
	id := ids.WorkspaceID(child.GetOwner().GetId())
	return &id, child.GetOwnerNotice().GetText()
}

// runBackgroundCreate performs an option-B create to completion and publishes
// its terminal outcome on the mutation-progress channel. It runs in its own
// goroutine under a context detached from the accepting request, so nothing the
// client does can cancel it; the daemon owns it to completion.
func (s *server) runBackgroundCreate(ctx context.Context, rpc string, spec workspace.CreateSpec, opID string) {
	created, err := s.deps.Verbs.Create(ctx, spec)
	if err != nil {
		s.publishCreateFailed(rpc, opID, err)
		return
	}
	s.log.Info("daemon.server.create_workspace", "created a workspace",
		dlog.Context{"workspace": string(created.ID), "dir": created.Dir, "op_id": opID})
	s.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId: opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Create{
			Create: &agentreplv1.WorkspaceCreateProgress{
				Step: &agentreplv1.WorkspaceCreateProgress_Succeeded{
					Succeeded: &agentreplv1.WorkspaceCreateSucceeded{
						Workspace: created.Ref(),
						Name:      created.Name,
					},
				},
			},
		},
	})
}

// publishCreateFailed maps a background create's error onto a terminal failure
// event, mirroring the synchronous form: a typed refusal it would have answered
// on `error`, or an internal error it would have failed the rpc with. The
// underlying cause is already in the daemon log (the verb records it at ERROR);
// an internal failure is re-surfaced here at ERROR too, because after the ack
// there is no rpc error left to carry it.
func (s *server) publishCreateFailed(rpc, opID string, err error) {
	failed := &agentreplv1.WorkspaceCreateFailed{}
	if resp := (&agentreplv1.CreateWorkspaceResponse{}); s.refuseOnto(s.log, rpc, resp, err) {
		failed.Cause = &agentreplv1.WorkspaceCreateFailed_Refusal{Refusal: resp.GetError()}
	}
	if failed.Cause == nil {
		s.log.Error("daemon.server.create_workspace", "a background create failed",
			dlog.Context{"op_id": opID, "cause": err.Error()})
		failed.Cause = &agentreplv1.WorkspaceCreateFailed_Internal{Internal: err.Error()}
	}
	s.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId: opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Create{
			Create: &agentreplv1.WorkspaceCreateProgress{
				Step: &agentreplv1.WorkspaceCreateProgress_Failed{Failed: failed},
			},
		},
	})
}

// createProgressReporter relays a create's stage transitions onto the
// mutation-progress channel, keyed on the op_id the create was accepted under.
// It satisfies workspace.CreateProgress, keeping the verb itself proto-free.
type createProgressReporter struct {
	server *server
	opID   string
}

func (r createProgressReporter) Stage(stage workspace.CreateStage) {
	wire := &agentreplv1.WorkspaceCreateStage{}
	switch stage {
	case workspace.CreateStageDerivingName:
		wire.Stage = &agentreplv1.WorkspaceCreateStage_DerivingName{DerivingName: &agentreplv1.WorkspaceCreateStageDerivingName{}}
	case workspace.CreateStageCreatingWorktree:
		wire.Stage = &agentreplv1.WorkspaceCreateStage_CreatingWorktree{CreatingWorktree: &agentreplv1.WorkspaceCreateStageCreatingWorktree{}}
	case workspace.CreateStageStartingSession:
		wire.Stage = &agentreplv1.WorkspaceCreateStage_StartingSession{StartingSession: &agentreplv1.WorkspaceCreateStageStartingSession{}}
	default:
		// An unmapped stage is a bug in this switch, not a client condition —
		// but a create's progress relay must never take the daemon down, so it
		// is surfaced loudly and the create proceeds unaffected.
		r.server.log.Error("daemon.server.create_workspace", "an unmapped create stage was reported; it was not relayed",
			dlog.Context{"op_id": r.opID, "stage": int(stage)})
		return
	}
	r.server.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId: r.opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Create{
			Create: &agentreplv1.WorkspaceCreateProgress{
				Step: &agentreplv1.WorkspaceCreateProgress_EnteredStage{EnteredStage: wire},
			},
		},
	})
}

// openProgressReporter relays an open's stage transitions onto the
// mutation-progress channel, keyed on the op_id the request supplied. It
// satisfies workspace.OpenProgress, keeping the verb itself proto-free.
type openProgressReporter struct {
	server *server
	opID   string
}

func (r openProgressReporter) Stage(stage workspace.OpenStage) {
	wire := &agentreplv1.WorkspaceOpenStage{}
	switch stage {
	case workspace.OpenStageCheckingWorktree:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_CheckingWorktree{CheckingWorktree: &agentreplv1.WorkspaceOpenStageCheckingWorktree{}}
	case workspace.OpenStageStartingSession:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_StartingSession{StartingSession: &agentreplv1.WorkspaceOpenStageStartingSession{}}
	case workspace.OpenStageReviving:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_Reviving{Reviving: &agentreplv1.WorkspaceOpenStageReviving{}}
	case workspace.OpenStageClearingClosed:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_ClearingClosed{ClearingClosed: &agentreplv1.WorkspaceOpenStageClearingClosed{}}
	case workspace.OpenStageCheckingBuild:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_CheckingBuild{CheckingBuild: &agentreplv1.WorkspaceOpenStageCheckingBuild{}}
	case workspace.OpenStageRestoringWorktree:
		wire.Stage = &agentreplv1.WorkspaceOpenStage_RestoringWorktree{RestoringWorktree: &agentreplv1.WorkspaceOpenStageRestoringWorktree{}}
	default:
		// An unmapped stage is a bug in this switch, not a client condition —
		// but an open's progress relay must never take the open down, so it is
		// surfaced loudly and the open proceeds unaffected.
		r.server.log.Error("daemon.server.open_workspace", "an unmapped open stage was reported; it was not relayed",
			dlog.Context{"op_id": r.opID, "stage": int(stage)})
		return
	}
	r.server.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId: r.opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Open{
			Open: &agentreplv1.WorkspaceOpenProgress{EnteredStage: wire},
		},
	})
}

// deployHooksOf renders a request's own deploy hooks, which the request's
// validation held to the config's rules (validateDeployHooks).
func deployHooksOf(hooks *agentreplv1.CreateWorkspaceDeployHooks) wsm.DeployHooks {
	return wsm.DeployHooks{PreDeploy: hooks.GetPreDeploy(), PostDeploy: hooks.GetPostDeploy()}
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
		Success: &agentreplv1.RegisterWorkspaceSuccess{Workspace: record.Ref()},
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
			Workspace:             registered.Workspace.Ref(),
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
	began := time.Now()
	resp := &agentreplv1.OpenWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	// A client that minted an op_id also gets the open's STAGES, pushed onto
	// WatchDaemon keyed on it while this rpc is still in flight. A client that
	// minted none gets exactly the behavior it always had: no reporter, no
	// pushes. The terminal outcome stays on this answer either way.
	var progress workspace.OpenProgress
	if opID := req.Msg.GetOpId(); opID != "" {
		progress = openProgressReporter{server: s, opID: opID}
	}
	// A REVERT IS AN OPEN TOO: the same open runs, and the verb then queues
	// the merge-revert brief into the reopened session.
	open := s.deps.Verbs.Open
	if req.Msg.GetRevertMerge() != nil {
		open = s.deps.Verbs.OpenToRevertMerge
	}
	if err := open(ctx, subject.Record.ID, progress); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// THE OPEN IS TIMED TO ITS SESSION BEING UP, which is when the verb
	// returns: the live workspace_open latency.
	s.deps.Latencies.ObserveLatency(subject.Record.ID, runtimemetrics.WorkspaceOpen, time.Since(began).Seconds())
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
	if err := validateWorkspaceRequester("requester", req.Msg.GetRequester()); err != nil {
		return nil, err
	}
	resp := &agentreplv1.CloseWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	success := &agentreplv1.CloseWorkspaceSuccess{Outcome: &agentreplv1.CloseWorkspaceSuccess_Closed{Closed: &agentreplv1.CloseWorkspaceSuccessClosed{}}}
	switch by, asker := askerOf(req.Msg.GetRequester()); {
	case by == merge.RequestedByUser:
		if err := s.deps.Verbs.Close(ctx, ws, wsm.CloseByUser); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	case asker == ws:
		later, err := s.deps.Verbs.CloseSelf(ctx, ws)
		if err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
		if later {
			success.Outcome = &agentreplv1.CloseWorkspaceSuccess_ClosesWhenTurnEnds{ClosesWhenTurnEnds: &agentreplv1.CloseWorkspaceSuccessClosesWhenTurnEnds{}}
		}
	default:
		if err := s.deps.Verbs.Close(ctx, ws, wsm.CloseByAgent); err != nil {
			return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
		}
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, ws)
	resp.Result = &agentreplv1.CloseWorkspaceResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// KillWorkspace is the big red button: forced session death, never blocking.
//
// A REQUEST CARRYING AN op_id IS ACKED AT ONCE (owner ruling, 2026-09-29):
// the fast half refuses what it must and marks the workspace closed, the rpc
// answers `accepted`, and the session's death runs detached, its end pushed
// on mutation_progress. Without one the rpc answers once the session is dead.
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
	ws := subject.Record.ID
	teardown, err := s.deps.Verbs.BeginKill(ctx, ws)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	s.PublishHostWorkspace(ctx, ws)
	if opID := req.Msg.GetOpId(); opID != "" {
		s.runDetachedTeardown(ctx, subject.Log, rpc, opID, teardown, func(err error) {
			s.PublishHostWorkspace(context.Background(), ws)
			s.publishKillEnd(subject.Log, opID, err)
		})
		resp.Result = &agentreplv1.KillWorkspaceResponse_Accepted{
			Accepted: &agentreplv1.KillWorkspaceAccepted{OpId: opID},
		}
		return connect.NewResponse(resp), nil
	}
	if err := teardown(ctx); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, ws)
	resp.Result = &agentreplv1.KillWorkspaceResponse_Success{
		Success: &agentreplv1.KillWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// NukeWorkspace destroys data: kill if live, then delete the worktree and the
// branch, then forget the record. An op_id acks at once, as KillWorkspace's
// does, and a git failure after the ack is pushed as the typed `git_failed`.
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
	teardown, err := s.deps.Verbs.BeginNuke(ctx, subject.Record.ID)
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	if opID := req.Msg.GetOpId(); opID != "" {
		s.runDetachedTeardown(ctx, subject.Log, rpc, opID, teardown, func(err error) {
			s.publishNukeEnd(subject.Log, rpc, opID, err)
		})
		resp.Result = &agentreplv1.NukeWorkspaceResponse_Accepted{
			Accepted: &agentreplv1.NukeWorkspaceAccepted{OpId: opID},
		}
		return connect.NewResponse(resp), nil
	}
	if err := teardown(ctx); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.NukeWorkspaceResponse_Success{
		Success: &agentreplv1.NukeWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// runDetachedTeardown runs an accepted verb's teardown to completion in its
// own goroutine, under a context detached from the accepting request (a client
// that hangs up must never cut a session kill or a `git worktree remove`
// short), and hands its outcome to END.
func (s *server) runDetachedTeardown(ctx context.Context, log dlog.Logger, rpc, opID string, teardown workspace.Teardown, end func(error)) {
	bgCtx, cancel := s.backgroundContext(ctx)
	log.Info(opServerTeardown, "accepted the verb and detached its teardown", dlog.Context{"rpc": rpc, "op_id": opID})
	go func() {
		// The goroutine OWNS the cancel, as runBackgroundCreate's does.
		defer cancel()
		end(teardown(bgCtx))
	}()
}

// publishKillEnd pushes an accepted kill's one terminal event. A failure is
// recorded at ERROR here: after the ack there is no rpc left to carry it.
func (s *server) publishKillEnd(log dlog.Logger, opID string, err error) {
	progress := &agentreplv1.WorkspaceKillProgress{}
	if err != nil {
		log.Error(opServerTeardown, "an accepted kill's teardown failed; the workspace stays closed", dlog.Context{
			"rpc": "KillWorkspace", "op_id": opID, "cause": err.Error(),
		})
		progress.Step = &agentreplv1.WorkspaceKillProgress_Failed{Failed: &agentreplv1.WorkspaceKillFailed{Internal: err.Error()}}
	} else {
		log.Info(opServerTeardown, "an accepted kill's teardown finished", dlog.Context{"rpc": "KillWorkspace", "op_id": opID})
		progress.Step = &agentreplv1.WorkspaceKillProgress_Succeeded{Succeeded: &agentreplv1.WorkspaceKillSucceeded{}}
	}
	s.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId:  opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Kill{Kill: progress},
	})
}

// publishNukeEnd pushes an accepted nuke's one terminal event: a typed refusal
// the synchronous form would have answered on `error`, or an internal failure.
func (s *server) publishNukeEnd(log dlog.Logger, rpc, opID string, err error) {
	progress := &agentreplv1.WorkspaceNukeProgress{}
	switch {
	case err == nil:
		log.Info(opServerTeardown, "an accepted nuke's teardown finished", dlog.Context{"rpc": rpc, "op_id": opID})
		progress.Step = &agentreplv1.WorkspaceNukeProgress_Succeeded{Succeeded: &agentreplv1.WorkspaceNukeSucceeded{}}
	default:
		failed := &agentreplv1.WorkspaceNukeFailed{}
		if resp := (&agentreplv1.NukeWorkspaceResponse{}); s.refuseOnto(log, rpc, resp, err) {
			failed.Cause = &agentreplv1.WorkspaceNukeFailed_Refusal{Refusal: resp.GetError()}
		}
		if failed.Cause == nil {
			log.Error(opServerTeardown, "an accepted nuke's teardown failed; the workspace stays closed", dlog.Context{
				"rpc": rpc, "op_id": opID, "cause": err.Error(),
			})
			failed.Cause = &agentreplv1.WorkspaceNukeFailed_Internal{Internal: err.Error()}
		}
		progress.Step = &agentreplv1.WorkspaceNukeProgress_Failed{Failed: failed}
	}
	s.MutationProgress(&agentreplv1.WorkspaceMutationProgress{
		OpId:  opID,
		Event: &agentreplv1.WorkspaceMutationProgress_Nuke{Nuke: progress},
	})
}

// opServerTeardown records the detached teardown of an accepted kill or nuke.
const opServerTeardown = "daemon.server.workspace_teardown"

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

// finishRenames spells the queue's refusals as FinishWorkspace's own arms.
var finishRenames = map[string]string{
	merge.ArmAlreadyMerging: "already_deploying",
}

// FinishWorkspace enqueues the deploy of a finished workspace's own branch,
// which runs in that workspace by its repository's deploy config. Its life
// from there is that workspace's bubble.
func (s *server) FinishWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.FinishWorkspaceRequest],
) (*connect.Response[agentreplv1.FinishWorkspaceResponse], error) {
	const rpc = "FinishWorkspace"
	if err := validateFinishWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FinishWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if _, err := s.deps.Merge.Enqueue(ctx, finishRequest(subject.Record.ID, req.Msg)); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, finishRenames))
	}
	// The verb moved the session's standing or the composer's gate; the host
	// view is recomposed from what the change left behind.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.FinishWorkspaceResponse_Success{
		Success: &agentreplv1.FinishWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// finishRequest reads the request into the orchestrator's request: the
// workspace's own branch, in place, asked by the requester and placed as the
// request says. It closes the workspace once its deploy succeeds.
func finishRequest(ws ids.WorkspaceID, req *agentreplv1.FinishWorkspaceRequest) merge.Request {
	out := merge.Request{Workspace: ws}
	out.By, out.Asker = askerOf(req.GetRequester())
	out.Source = wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}
	out.Source.Front = req.GetPlacement().GetFront() != nil
	out.Source.Version = versionIncrement(req.GetVersionIncrement())
	return out
}

// askerOf is who asked, as the merge queue reads it: the user, or a
// workspace's agent and that workspace.
func askerOf(r *agentreplv1.WorkspaceRequester) (merge.Requester, ids.WorkspaceID) {
	if id := requesterID(r); id != nil {
		return merge.RequestedByAgent, *id
	}
	return merge.RequestedByUser, ""
}

// versionIncrement reads the request's version increment, nil when it carries
// none. The request's validation refused a malformed one before this.
func versionIncrement(v *agentreplv1.MergeWorkspaceVersionIncrement) *wsm.VersionIncrement {
	if v == nil {
		return nil
	}
	out := &wsm.VersionIncrement{ChangelogLine: v.GetChangelogLine()}
	switch v.GetIncrement().(type) {
	case *agentreplv1.MergeWorkspaceVersionIncrement_Minor:
		out.Part = semver.Minor
	case *agentreplv1.MergeWorkspaceVersionIncrement_Patch:
		out.Part = semver.Patch
	}
	return out
}

// RestartWorkspace bounces the workspace's shim and reloads its webapp page,
// immediately: there is no graceful mode.
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
	if err := s.deps.Verbs.Restart(ctx, subject.Record.ID); err != nil {
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
	history := workspace.RecordSelection
	if req.Msg.GetKeepSelectionHistory() {
		history = workspace.KeepSelectionHistory
	}
	selected, cerr := s.selectSubject(ctx, rpc, subject, history, req.Msg.GetReveal(), resp)
	if selected == nil {
		return answer(resp, cerr)
	}
	resp.Result = &agentreplv1.SelectWorkspaceResponse_Success{Success: selected}
	return connect.NewResponse(resp), nil
}

// selectSubject is THE ONE SELECT a client's request makes of the daemon. It
// selects SUBJECT under HISTORY and reveals REVEAL's row when it is set, and
// answers the success to send. A nil success is no selection: a refusal set
// onto RESP's error arm with a nil error, or the error to answer.
func (s *server) selectSubject(
	ctx context.Context,
	rpc string,
	subject resolved,
	history workspace.SelectionHistory,
	reveal *frontendv1.FeedId,
	resp proto.Message,
) (*agentreplv1.SelectWorkspaceSuccess, *connect.Error) {
	// SELECTING A HIDDEN MERGE WORKSPACE REVEALS IT, before the selection, so
	// the roster draws the workspace it names as current.
	if subject.Record.MergeHidden() {
		if err := s.deps.Merge.RevealMergeWorkspace(ctx, subject.Record.ID, "the user selected it"); err != nil {
			return nil, fail(subject.Log, rpc, err)
		}
	}
	if err := s.deps.Verbs.Select(ctx, subject.Record.ID, history); err != nil {
		// A CALLER THAT LEFT IS NOT A FAILURE. A user switching workspaces
		// quickly cancels the select of the one they switched away from while
		// its revival runs; the revival goes on without it, and the answer
		// has nobody to reach.
		if ctx.Err() != nil && errors.Is(err, ctx.Err()) {
			subject.Log.Info(rpc, "the caller left before the select finished", dlog.Context{"cause": err.Error()})
			return nil, connect.NewError(connect.CodeCanceled, err)
		}
		return nil, s.answerRefusal(subject.Log, rpc, resp, err, nil)
	}
	// THE USER CHOSE A WORKSPACE (a switch in Emacs, a handed banner's click),
	// so no banner made before the choice may steer a later return to Emacs.
	s.deps.Banners.Forget("a workspace was selected")
	// A ROW THE SELECTION NAMES is revealed on the selected workspace's page
	// (an async merge's queue entry opening its bubble in the requester).
	if reveal.GetValue() != "" {
		s.revealRow(subject.Log, subject.Record.ID, reveal)
	}
	// THE REVISION IS READ AFTER THE STAMP, whose render published under the
	// resolver's lock (sidebar.Resolver.Revision): every roster at it or later
	// carries this selection, so the editor that moved on it is never moved
	// back by a roster from before it.
	return &agentreplv1.SelectWorkspaceSuccess{RosterRevision: s.deps.Sidebar.Revision()}, nil
}

// MarkWorkspaceViewed records that the user has SEEN this workspace, which
// reads the last turn's result and draws its roster row PARTIAL on its
// turn-end arm until the next turn — if the row is done or interrupted; on any
// other status the roster drops the report. It is idempotent, and it is the
// editor's verb: dwell is an editor fact.
func (s *server) MarkWorkspaceViewed(
	ctx context.Context,
	req *connect.Request[agentreplv1.MarkWorkspaceViewedRequest],
) (*connect.Response[agentreplv1.MarkWorkspaceViewedResponse], error) {
	const rpc = "MarkWorkspaceViewed"
	resp := &agentreplv1.MarkWorkspaceViewedResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Verbs.MarkViewed(ctx, subject.Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.MarkWorkspaceViewedResponse_Success{
		Success: &agentreplv1.MarkWorkspaceViewedSuccess{},
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

// SetWorkspacePrincipalLink sets or clears the workspace's principal link,
// which the topbar's summary opens. The new title arrives on the topbar stream.
func (s *server) SetWorkspacePrincipalLink(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspacePrincipalLinkRequest],
) (*connect.Response[agentreplv1.SetWorkspacePrincipalLinkResponse], error) {
	const rpc = "SetWorkspacePrincipalLink"
	if err := validateSetWorkspacePrincipalLinkRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspacePrincipalLinkResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var link *string
	if set := req.Msg.GetSet(); set != nil {
		url := set.GetUrl()
		link = &url
	}
	if err := s.deps.Verbs.SetPrincipalLink(ctx, subject.Record.ID, link); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetWorkspacePrincipalLinkResponse_Success{
		Success: &agentreplv1.SetWorkspacePrincipalLinkSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// SetWorkspaceHidden moves a workspace into the roster's hidden section, or
// back out of it. The new state arrives on the roster stream.
func (s *server) SetWorkspaceHidden(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceHiddenRequest],
) (*connect.Response[agentreplv1.SetWorkspaceHiddenResponse], error) {
	const rpc = "SetWorkspaceHidden"
	if err := validateSetWorkspaceHiddenRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspaceHiddenResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	hidden := req.Msg.GetHide() != nil
	if err := s.deps.Verbs.SetHidden(ctx, subject.Record.ID, hidden); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetWorkspaceHiddenResponse_Success{
		Success: &agentreplv1.SetWorkspaceHiddenSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// SetWorkspaceParent retargets a workspace's merge, to another workspace of
// its repository or to the main checkout, and queues the notice in its
// session in the same operation (internal/retarget). The new nesting arrives
// on the roster stream.
func (s *server) SetWorkspaceParent(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceParentRequest],
) (*connect.Response[agentreplv1.SetWorkspaceParentResponse], error) {
	const rpc = "SetWorkspaceParent"
	if err := validateSetWorkspaceParentRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SetWorkspaceParentResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	var parent *ids.WorkspaceID
	if child := req.Msg.GetChildOf(); child != nil {
		// THE PARENT IS RESOLVED AGAINST THE REGISTRY ALONE: it is not the
		// subject of the request, so which daemon serves it does not matter,
		// and its two resolution refusals are the parent's own arms.
		resolvedParent, r, err := s.resolveRegistered(ctx, rpc, child.GetParent())
		if err != nil {
			return answer(resp, failResolution(subject.Log, rpc, err))
		}
		if r != nil {
			arm, ok := parentArms[r.Arm]
			if !ok {
				return answer(resp, fail(subject.Log, rpc, fmt.Errorf("the parent's resolution refused with %q, which has no parent arm", r.Arm)))
			}
			r.Arm = arm
			return answer(resp, s.refuse(subject.Log, rpc, resp, *r))
		}
		id := resolvedParent.Record.ID
		parent = &id
	}
	if err := s.deps.Retarget.Retarget(ctx, subject.Record.ID, parent); err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SetWorkspaceParentResponse_Success{
		Success: &agentreplv1.SetWorkspaceParentSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// SwapWorkspaces exchanges two workspaces' places in the registry order. Both refs resolve as any workspace ref does, and a refusal of either
// names the ref it refused; the new order arrives on the roster stream.
func (s *server) SwapWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.SwapWorkspacesRequest],
) (*connect.Response[agentreplv1.SwapWorkspacesResponse], error) {
	const rpc = "SwapWorkspaces"
	if err := validateSwapWorkspacesRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SwapWorkspacesResponse{}
	subjects, refused, done := s.resolveAllNaming(ctx, rpc, resp, []*workspacev1.WorkspaceRef{req.Msg.GetWorkspace(), req.Msg.GetTarget()})
	if done {
		return answer(resp, refused)
	}
	if err := s.deps.Verbs.Swap(ctx, subjects[0].Record.ID, subjects[1].Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subjects[0].Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.SwapWorkspacesResponse_Success{
		Success: &agentreplv1.SwapWorkspacesSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// MoveWorkspace moves one workspace onto another's tab in the sidebar's
// arrangement. Both refs resolve as any workspace ref does, and a refusal of
// either names the ref it refused; the new order arrives on the roster
// stream.
func (s *server) MoveWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.MoveWorkspaceRequest],
) (*connect.Response[agentreplv1.MoveWorkspaceResponse], error) {
	const rpc = "MoveWorkspace"
	if err := validateMoveWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.MoveWorkspaceResponse{}
	subjects, refused, done := s.resolveAllNaming(ctx, rpc, resp, []*workspacev1.WorkspaceRef{req.Msg.GetWorkspace(), req.Msg.GetTarget()})
	if done {
		return answer(resp, refused)
	}
	if err := s.deps.Verbs.Move(ctx, subjects[0].Record.ID, subjects[1].Record.ID); err != nil {
		return answer(resp, s.answerRefusal(subjects[0].Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.MoveWorkspaceResponse_Success{
		Success: &agentreplv1.MoveWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// parentArms renames the parent ref's resolution refusals onto
// SetWorkspaceParentError's parent arms. A resolution answers exactly these
// two, so any other arm is a defect, failed at ERROR rather than renamed.
var parentArms = map[string]string{
	workspace.ArmUnknownWorkspace:     workspace.ArmUnknownParent,
	workspace.ArmWorkspaceRefMismatch: "parent_ref_mismatch",
}

// resolveNaming resolves one of a request's workspace refs, and on a refusal
// encodes it onto resp with the refused ref on the arm's `workspace` field,
// the arm renamed through rename when the request spells it differently.
// `done` reports that the caller must answer at once with the returned error.
func (s *server) resolveNaming(
	ctx context.Context,
	rpc string,
	resp proto.Message,
	ref *workspacev1.WorkspaceRef,
	rename map[string]string,
) (resolved, *connect.Error, bool) {
	subject, r, err := s.resolveRef(ctx, rpc, ref)
	return s.answerResolution(rpc, resp, ref, rename, subject, r, err)
}

// resolveRegisteredNaming is resolveNaming for a ref resolved against the
// registry alone (resolveRegistered): a workspace named as what is read or
// followed, whichever daemon serves it.
func (s *server) resolveRegisteredNaming(
	ctx context.Context,
	rpc string,
	resp proto.Message,
	ref *workspacev1.WorkspaceRef,
	rename map[string]string,
) (resolved, *connect.Error, bool) {
	subject, r, err := s.resolveRegistered(ctx, rpc, ref)
	return s.answerResolution(rpc, resp, ref, rename, subject, r, err)
}

// answerResolution is the one answer of a ref's resolution for resolveNaming
// and resolveRegisteredNaming: the subject, or the refusal encoded onto RESP
// with the refused ref on the arm's `workspace` field and the arm renamed
// through RENAME.
func (s *server) answerResolution(
	rpc string,
	resp proto.Message,
	ref *workspacev1.WorkspaceRef,
	rename map[string]string,
	subject resolved,
	r *refusal,
	err error,
) (resolved, *connect.Error, bool) {
	if err != nil {
		return resolved{}, failResolution(s.log, rpc, err), true
	}
	if r == nil {
		return subject, nil, false
	}
	named := *r
	if to, renamed := rename[named.Arm]; renamed {
		named.Arm = to
	}
	named.Fields = map[string]any{"workspace": ref}
	for field, value := range r.Fields {
		named.Fields[field] = value
	}
	return resolved{}, s.refuse(s.log, rpc, resp, named), true
}

// resolveAllNaming resolves REFS in order through resolveNaming, each
// refusal naming the ref it refused. The first refusal answers at once, so
// nothing after it is resolved; `done' says the caller answers with refused.
func (s *server) resolveAllNaming(
	ctx context.Context,
	rpc string,
	resp proto.Message,
	refs []*workspacev1.WorkspaceRef,
) ([]resolved, *connect.Error, bool) {
	subjects := make([]resolved, 0, len(refs))
	for _, ref := range refs {
		subject, refused, done := s.resolveNaming(ctx, rpc, resp, ref, nil)
		if done {
			return nil, refused, true
		}
		subjects = append(subjects, subject)
	}
	return subjects, nil, false
}

// FoldRepository expands or collapses one repository's roster section. The
// new fold arrives on the roster stream.
func (s *server) FoldRepository(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldRepositoryRequest],
) (*connect.Response[agentreplv1.FoldRepositoryResponse], error) {
	const rpc = "FoldRepository"
	if err := validateFoldRepositoryRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FoldRepositoryResponse{}
	repository, cerr, done := s.resolveRepository(ctx, rpc, resp, req.Msg.GetRepository())
	if done {
		return answer(resp, cerr)
	}
	folded := req.Msg.GetCollapse() != nil
	if err := s.deps.Verbs.FoldRepository(ctx, repository.ID, folded); err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.FoldRepositoryResponse_Success{Success: &agentreplv1.FoldRepositorySuccess{}}
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
		return resolved{}, failResolution(s.log, rpc, err), true
	}
	if r != nil {
		return resolved{}, s.refuse(s.log, rpc, resp, *r), true
	}
	return subject, nil, false
}
