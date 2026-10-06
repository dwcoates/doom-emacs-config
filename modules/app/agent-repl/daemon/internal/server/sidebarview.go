package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// UpdateSidebarView changes one piece of the sidebar's view state — a fold or
// the grouping shown — which the roster push carries to every
// page. A change to what the view already holds is success.
func (s *server) UpdateSidebarView(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateSidebarViewRequest],
) (*connect.Response[agentreplv1.UpdateSidebarViewResponse], error) {
	const rpc = "UpdateSidebarView"
	if err := validateUpdateSidebarViewRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateSidebarViewResponse{}
	var err error
	switch change := req.Msg.GetChange().(type) {
	case *agentreplv1.UpdateSidebarViewRequest_FoldSection:
		var (
			answered bool
			cerr     *connect.Error
		)
		answered, cerr, err = s.foldSection(ctx, rpc, resp, change.FoldSection)
		if answered {
			return answer(resp, cerr)
		}
	case *agentreplv1.UpdateSidebarViewRequest_ShowGrouping:
		grouping := wsm.GroupingRepository
		if change.ShowGrouping.GetTask() != nil {
			grouping = wsm.GroupingTask
		}
		err = s.deps.Verbs.ShowGrouping(ctx, grouping)
	default:
		// validateUpdateSidebarViewRequest refuses an unset arm, and every arm
		// the contract names is a case above: reaching here is a validator
		// that fell out of step with the contract.
		panic(fmt.Sprintf("UpdateSidebarView: change arm %T passed validation that no case handles", change))
	}
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.UpdateSidebarViewResponse_Success{Success: &agentreplv1.UpdateSidebarViewSuccess{}}
	return connect.NewResponse(resp), nil
}

// foldSection folds one section. A repository is resolved first, so an
// unknown one is refused before anything is written: `answered` then says the
// caller answers at once, with the refusal on resp or the Connect error. The
// verb's own outcome comes back as err for the caller to map.
func (s *server) foldSection(
	ctx context.Context,
	rpc string,
	resp *agentreplv1.UpdateSidebarViewResponse,
	fold *agentreplv1.SidebarViewFoldSection,
) (answered bool, cerr *connect.Error, err error) {
	folded := fold.GetCollapse() != nil
	switch section := fold.GetSection().(type) {
	case *agentreplv1.SidebarViewFoldSection_Repository:
		repository, refused, done := s.resolveRepository(ctx, rpc, resp, section.Repository)
		if done {
			return true, refused, nil
		}
		return false, nil, s.deps.Verbs.FoldRepository(ctx, repository.ID, folded)
	case *agentreplv1.SidebarViewFoldSection_Task:
		return false, nil, s.deps.Verbs.FoldTaskSection(ctx, ids.TaskID(section.Task.GetId()), folded)
	case *agentreplv1.SidebarViewFoldSection_RecentlyMerged:
		return false, nil, s.deps.Verbs.FoldMergedSection(ctx, folded)
	default:
		panic(fmt.Sprintf("UpdateSidebarView: section arm %T passed validation that no case handles", section))
	}
}
