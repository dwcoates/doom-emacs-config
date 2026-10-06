package server

import (
	"context"
	"errors"

	"connectrpc.com/connect"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/rollout"
)

// The handover rendezvous, called ON THE NEW DAEMON by each participant.
//
// THE WEB SIDE NEVER REDIALS (project lead): Emacs reloads the webview at the
// successor's address and the FRESH page calls AdoptWebWorkspace ONCE AT BOOT,
// before it opens any view stream. So `no_transfer_announced` is the ORDINARY
// answer on every non-handover page boot: it is recorded at INFO, never at WARN
// and never as a fault. The host side is unchanged — Emacs adopts on the
// announcement.

// AdoptHostWorkspace is Emacs's half of the rendezvous.
func (s *server) AdoptHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptHostWorkspaceRequest],
) (*connect.Response[agentreplv1.AdoptHostWorkspaceResponse], error) {
	const rpc = "AdoptHostWorkspace"
	resp := &agentreplv1.AdoptHostWorkspaceResponse{}
	subject, cerr, done := s.subjectForAdoption(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Rollout.AdoptHost(ctx, subject.Record.ID); err != nil {
		refused, ok := s.asRefusal(err)
		if !ok {
			return nil, fail(subject.Log, rpc, err)
		}
		return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
	}
	subject.Log.Info("daemon.server.adopt_host", "the host adopted the workspace", nil)
	resp.Result = &agentreplv1.AdoptHostWorkspaceResponse_Success{
		Success: &agentreplv1.AdoptHostWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// AdoptWebWorkspace is the reloaded page's half of the rendezvous. The first
// call from ANY connection satisfies the web participant.
func (s *server) AdoptWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptWebWorkspaceRequest],
) (*connect.Response[agentreplv1.AdoptWebWorkspaceResponse], error) {
	const rpc = "AdoptWebWorkspace"
	resp := &agentreplv1.AdoptWebWorkspaceResponse{}
	subject, cerr, done := s.subjectForAdoption(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	if err := s.deps.Rollout.AdoptWeb(ctx, subject.Record.ID); err != nil {
		refused, ok := s.asRefusal(err)
		if !ok {
			return nil, fail(subject.Log, rpc, err)
		}
		// The ORDINARY page boot: no handover was announced, so there is
		// nothing to adopt. It is an answer, recorded at INFO.
		refused.Info = errors.Is(err, rollout.ErrNoTransferAnnounced)
		return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
	}
	subject.Log.Info("daemon.server.adopt_web", "the webview adopted the workspace", nil)
	resp.Result = &agentreplv1.AdoptWebWorkspaceResponse_Success{
		Success: &agentreplv1.AdoptWebWorkspaceSuccess{},
	}
	return connect.NewResponse(resp), nil
}

// subjectForAdoption resolves an adoption call's ref WITHOUT the ownership
// refusal a served verb makes: `not_yet_adopted` is exactly the state an
// adoption call exists to leave, so refusing on it would make the rendezvous
// unreachable. The unknown-workspace and ref-mismatch refusals still stand.
//
// IT IS resolveRegistered, the registry resolution every verb shares. Its own
// copy here answered EVERY failed registry read as `unknown_workspace`: on the
// 2026-10-06 handover the successor's read for queen-model's adopting page met
// `sql: database is closed`, the page was told the daemon had never heard of
// its workspace and failed its boot, and the host's adopt then waited out its
// 10s for a web participant that never came. A broken read is a failure, and
// only a read that found nothing is `unknown_workspace`.
//
// AN ADOPTION'S REFUSAL IS RECORDED AT INFO. An adopt call names a workspace a
// transfer announced, so a refusal of it is never the background noise a
// DEBUG refusal is; the page's own ERROR was the only record of it.
func (s *server) subjectForAdoption(
	ctx context.Context,
	rpc string,
	ref *workspacev1.WorkspaceRef,
	resp proto.Message,
) (resolved, *connect.Error, bool) {
	if err := validateWorkspaceRef("workspace", ref); err != nil {
		return resolved{}, err, true
	}
	subject, r, err := s.resolveRegistered(ctx, rpc, ref)
	if err != nil {
		return resolved{}, failResolution(s.log, rpc, err), true
	}
	if r != nil {
		r.Info = true
		return resolved{}, s.refuse(s.log, rpc, resp, *r), true
	}
	return subject, nil, false
}
