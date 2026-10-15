package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
)

// ArrangeSidebar changes the repository grouping's arrangement, which the tab
// bar walks too: the repositories' order, a repository's subsections, and
// which subsection a workspace sits in and where. Every ref is resolved
// first, so an unknown one is refused before anything is written; the verb's
// own refusals map onto the response's arms.
func (s *server) ArrangeSidebar(
	ctx context.Context,
	req *connect.Request[agentreplv1.ArrangeSidebarRequest],
) (*connect.Response[agentreplv1.ArrangeSidebarResponse], error) {
	const rpc = "ArrangeSidebar"
	if err := validateArrangeSidebarRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ArrangeSidebarResponse{}
	var err error
	switch change := req.Msg.GetChange().(type) {
	case *agentreplv1.ArrangeSidebarRequest_CreateSubsection:
		repository, refused, done := s.resolveRepository(ctx, rpc, resp, change.CreateSubsection.GetRepository())
		if done {
			return answer(resp, refused)
		}
		err = s.deps.Verbs.CreateSubsection(ctx, repository.ID, change.CreateSubsection.GetName())
	case *agentreplv1.ArrangeSidebarRequest_RenameSubsection:
		rename := change.RenameSubsection
		err = s.deps.Verbs.RenameSubsection(ctx, ids.SubsectionID(rename.GetSubsection().GetId()), rename.GetName())
	case *agentreplv1.ArrangeSidebarRequest_DeleteSubsection:
		err = s.deps.Verbs.DeleteSubsection(ctx, ids.SubsectionID(change.DeleteSubsection.GetSubsection().GetId()))
	case *agentreplv1.ArrangeSidebarRequest_MoveSubsection:
		move := change.MoveSubsection
		var before *ids.SubsectionID
		if ref := move.GetBefore(); ref != nil {
			id := ids.SubsectionID(ref.GetId())
			before = &id
		}
		err = s.deps.Verbs.MoveSubsection(ctx, ids.SubsectionID(move.GetSubsection().GetId()), before)
	case *agentreplv1.ArrangeSidebarRequest_MoveRepository:
		var (
			answered bool
			cerr     *connect.Error
		)
		answered, cerr, err = s.moveRepository(ctx, rpc, resp, change.MoveRepository)
		if answered {
			return answer(resp, cerr)
		}
	case *agentreplv1.ArrangeSidebarRequest_PlaceWorkspace:
		var (
			answered bool
			cerr     *connect.Error
		)
		answered, cerr, err = s.placeWorkspace(ctx, rpc, resp, change.PlaceWorkspace)
		if answered {
			return answer(resp, cerr)
		}
	default:
		// validateArrangeSidebarRequest refuses an unset arm, and every arm the
		// contract names is a case above: reaching here is a validator that
		// fell out of step with the contract.
		panic(fmt.Sprintf("ArrangeSidebar: change arm %T passed validation that no case handles", change))
	}
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	// THE VERB REPUBLISHED BEFORE ANSWERING, so the roster the topic holds now
	// carries the change: its revision is the one a page waits for.
	var revision uint64
	if latest, ok := s.deps.Sidebar.Topic().Latest(); ok {
		revision = latest.GetRevision()
	}
	resp.Result = &agentreplv1.ArrangeSidebarResponse_Success{Success: &agentreplv1.ArrangeSidebarSuccess{RosterRevision: revision}}
	return connect.NewResponse(resp), nil
}

// moveRepository resolves the moved repository and its anchor, then moves it.
// `answered` says a resolution refused (or the two refs name one repository)
// and the caller answers at once with cerr; otherwise err is the verb's.
func (s *server) moveRepository(
	ctx context.Context,
	rpc string,
	resp *agentreplv1.ArrangeSidebarResponse,
	move *agentreplv1.ArrangeMoveRepository,
) (answered bool, cerr *connect.Error, err error) {
	repository, refused, done := s.resolveRepository(ctx, rpc, resp, move.GetRepository())
	if done {
		return true, refused, nil
	}
	var before *ids.RepoID
	if ref := move.GetBefore(); ref != nil {
		anchor, refused, done := s.resolveRepository(ctx, rpc, resp, ref)
		if done {
			return true, refused, nil
		}
		if anchor.ID == repository.ID {
			return true, invalid("move_repository.before", "a repository cannot move before itself"), nil
		}
		before = &anchor.ID
	}
	return false, nil, s.deps.Verbs.MoveRepository(ctx, repository.ID, before)
}

// placeWorkspace resolves the placed workspace and its anchor, then places it.
// `answered` says a resolution refused and the caller answers at once with
// cerr; otherwise err is the verb's.
func (s *server) placeWorkspace(
	ctx context.Context,
	rpc string,
	resp *agentreplv1.ArrangeSidebarResponse,
	place *agentreplv1.ArrangePlaceWorkspace,
) (answered bool, cerr *connect.Error, err error) {
	var subjects []resolved
	refs := []*workspacev1.WorkspaceRef{place.GetWorkspace()}
	if anchor := place.GetBefore(); anchor != nil {
		refs = append(refs, anchor)
	}
	for _, ref := range refs {
		subject, refused, done := s.resolveNaming(ctx, rpc, resp, ref, nil)
		if done {
			return true, refused, nil
		}
		subjects = append(subjects, subject)
	}
	var into *ids.SubsectionID
	if ref := place.GetSubsection(); ref != nil {
		id := ids.SubsectionID(ref.GetId())
		into = &id
	}
	var before *ids.WorkspaceID
	if len(subjects) == 2 {
		before = &subjects[1].Record.ID
	}
	return false, nil, s.deps.Verbs.PlaceWorkspace(ctx, subjects[0].Record.ID, into, before)
}
