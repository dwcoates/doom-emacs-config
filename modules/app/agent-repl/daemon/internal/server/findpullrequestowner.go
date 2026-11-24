package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/prwatch"
)

// FindPullRequestOwner answers which workspace owns the pull request a
// clicked link names (endpoint_find_pull_request_owner.proto), so a page's
// shift-click on the link switches to it. The daemon alone decides whether
// the url names a pull request (prwatch.ParseURL) and who owns it
// (wsm.PullRequestOwnerOf); the request names no workspace, since the answer
// is the same for every page.
func (s *server) FindPullRequestOwner(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindPullRequestOwnerRequest],
) (*connect.Response[agentreplv1.FindPullRequestOwnerResponse], error) {
	const rpc = "FindPullRequestOwner"
	url := req.Msg.GetUrl()
	if strings.TrimSpace(url) == "" {
		return nil, invalid("url", "a url is not blank")
	}
	answer := func(success *agentreplv1.FindPullRequestOwnerSuccess) *connect.Response[agentreplv1.FindPullRequestOwnerResponse] {
		return connect.NewResponse(&agentreplv1.FindPullRequestOwnerResponse{
			Result: &agentreplv1.FindPullRequestOwnerResponse_Success{Success: success}})
	}
	pr, ok := prwatch.ParseURL(url)
	if !ok {
		s.log.Debug(rpc, "the url names no pull request", dlog.Context{"url": url})
		return answer(&agentreplv1.FindPullRequestOwnerSuccess{Answer: &agentreplv1.FindPullRequestOwnerSuccess_NotPullRequest{
			NotPullRequest: &agentreplv1.FindPullRequestOwnerNotPullRequest{}}}), nil
	}
	fields := dlog.Context{"url": url, "pull_request": pr.String()}
	owner, found, err := s.deps.DB.PullRequestOwnerOf(ctx, pr)
	if err != nil {
		return nil, fail(s.log.With(fields), rpc, fmt.Errorf("read the pull request's owner: %w", err))
	}
	if !found {
		s.log.Info(rpc, "no workspace owns the pull request", fields)
		return answer(&agentreplv1.FindPullRequestOwnerSuccess{Answer: &agentreplv1.FindPullRequestOwnerSuccess_Unowned{
			Unowned: &agentreplv1.FindPullRequestOwnerUnowned{}}}), nil
	}
	record, err := s.deps.DB.Workspace(ctx, owner.Workspace)
	if err != nil {
		return nil, fail(s.log.With(fields), rpc, fmt.Errorf("read the owning workspace %s: %w", owner.Workspace, err))
	}
	owned := &agentreplv1.FindPullRequestOwnerOwned{Workspace: record.Ref()}
	if owner.Closed {
		owned.Standing = &agentreplv1.FindPullRequestOwnerOwned_Closed{Closed: &agentreplv1.FindPullRequestOwnerClosed{}}
	} else {
		owned.Standing = &agentreplv1.FindPullRequestOwnerOwned_Open{Open: &agentreplv1.FindPullRequestOwnerOpen{}}
	}
	fields["owner"], fields["owner_name"], fields["closed"] = string(owner.Workspace), record.Name, owner.Closed
	s.log.Info(rpc, "found the workspace that owns the pull request", fields)
	return answer(&agentreplv1.FindPullRequestOwnerSuccess{Answer: &agentreplv1.FindPullRequestOwnerSuccess_Owned{Owned: owned}}), nil
}
