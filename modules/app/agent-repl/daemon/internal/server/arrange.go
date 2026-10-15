package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
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
	var (
		err     error
		created *agentreplv1.SubsectionRef
	)
	switch change := req.Msg.GetChange().(type) {
	case *agentreplv1.ArrangeSidebarRequest_CreateSubsection:
		repository, refused, done := s.resolveRepository(ctx, rpc, resp, change.CreateSubsection.GetRepository())
		if done {
			return answer(resp, refused)
		}
		var sub wsm.Subsection
		if sub, err = s.deps.Verbs.CreateSubsection(ctx, repository.ID, change.CreateSubsection.GetName()); err == nil {
			created = &agentreplv1.SubsectionRef{Id: string(sub.ID)}
		}
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
		place := change.PlaceWorkspace
		var (
			answered bool
			cerr     *connect.Error
		)
		answered, cerr, err = s.placeWorkspaces(ctx, rpc, resp, []*workspacev1.WorkspaceRef{place.GetWorkspace()}, place.GetSubsection(), place.GetBefore())
		if answered {
			return answer(resp, cerr)
		}
	case *agentreplv1.ArrangeSidebarRequest_PlaceWorkspaces:
		place := change.PlaceWorkspaces
		var (
			answered bool
			cerr     *connect.Error
		)
		answered, cerr, err = s.placeWorkspaces(ctx, rpc, resp, place.GetWorkspaces(), place.GetSubsection(), place.GetBefore())
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
	resp.Result = &agentreplv1.ArrangeSidebarResponse_Success{Success: &agentreplv1.ArrangeSidebarSuccess{RosterRevision: revision, Created: created}}
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

// placeWorkspaces resolves the placed workspaces and their anchor, then
// places them as one block. `answered` says a resolution refused and the
// caller answers at once with cerr; otherwise err is the verb's.
func (s *server) placeWorkspaces(
	ctx context.Context,
	rpc string,
	resp *agentreplv1.ArrangeSidebarResponse,
	refs []*workspacev1.WorkspaceRef,
	into *agentreplv1.SubsectionRef,
	anchor *workspacev1.WorkspaceRef,
) (answered bool, cerr *connect.Error, err error) {
	resolve := func(ref *workspacev1.WorkspaceRef) (ids.WorkspaceID, *connect.Error, bool) {
		subject, refused, done := s.resolveNaming(ctx, rpc, resp, ref, nil)
		return subject.Record.ID, refused, done
	}
	placed := make([]ids.WorkspaceID, 0, len(refs))
	for _, ref := range refs {
		id, refused, done := resolve(ref)
		if done {
			return true, refused, nil
		}
		placed = append(placed, id)
	}
	var before *ids.WorkspaceID
	if anchor != nil {
		id, refused, done := resolve(anchor)
		if done {
			return true, refused, nil
		}
		before = &id
	}
	var destination *ids.SubsectionID
	if into != nil {
		id := ids.SubsectionID(into.GetId())
		destination = &id
	}
	return false, nil, s.deps.Verbs.PlaceWorkspaces(ctx, placed, destination, before)
}

// GetSidebarArrangement answers the repository grouping's arrangement, for
// an agent organizing it: repositories, their unsectioned families and their
// subsections' families, in the order the sidebar draws them.
func (s *server) GetSidebarArrangement(
	ctx context.Context,
	_ *connect.Request[agentreplv1.GetSidebarArrangementRequest],
) (*connect.Response[agentreplv1.GetSidebarArrangementResponse], error) {
	const rpc = "GetSidebarArrangement"
	arranged, err := s.deps.Verbs.SidebarArrangement(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	resp := &agentreplv1.GetSidebarArrangementResponse{}
	for _, repo := range arranged {
		out := &agentreplv1.ArrangedRepository{
			Repository: &workspacev1.RepositoryRef{Id: string(repo.Repository.ID), Dir: repo.Repository.Dir},
			Name:       repo.Repository.Name,
			Workspaces: arrangedWorkspaces(repo.Workspaces),
		}
		for _, sub := range repo.Subsections {
			out.Subsections = append(out.Subsections, &agentreplv1.ArrangedSubsection{
				Subsection: &agentreplv1.SubsectionRef{Id: string(sub.Subsection.ID)},
				Name:       sub.Subsection.Name,
				Workspaces: arrangedWorkspaces(sub.Workspaces),
			})
		}
		resp.Repositories = append(resp.Repositories, out)
	}
	return connect.NewResponse(resp), nil
}

// arrangedWorkspaces projects families onto the wire, children nested.
func arrangedWorkspaces(in []sidebar.ArrangedWorkspace) []*agentreplv1.ArrangedWorkspace {
	out := make([]*agentreplv1.ArrangedWorkspace, 0, len(in))
	for _, family := range in {
		ws := family.Workspace
		wire := &agentreplv1.ArrangedWorkspace{
			Workspace: &workspacev1.WorkspaceRef{Id: string(ws.ID), Dir: ws.Dir},
			Name:      ws.Name,
			Branch:    ws.Branch,
			Hidden:    ws.Hidden,
			Children:  arrangedWorkspaces(family.Children),
		}
		if ws.Parent != nil {
			wire.Parent = &workspacev1.WorkspaceRef{Id: string(*ws.Parent)}
		}
		out = append(out, wire)
	}
	return out
}
