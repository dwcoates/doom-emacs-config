//go:build integration

package integration

import (
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/integration/harness"
)

func TestAnAgentsMergeOntoAgentReplsMasterTagsItsLandingAndDrawsTheVersion(t *testing.T) {
	t.Parallel()
	// Arrange: master carries the version 0.4.7, and the workspace has work.
	s := newSourcedRepo(t)
	s.repo.Tag("0.4.7")
	harness.CommitWork(t, s.f.ws.GetDir())
	root := s.f.watchRootFeed()

	// Act: the agent asks for a minor increment.
	resp, err := s.d.Client().MergeWorkspace(s.d.Ctx(), connect.NewRequest(harness.MergeRequest(s.f.ws, harness.OneShot(), func(req *agentreplv1.MergeWorkspaceRequest) {
		req.Requester = harness.AskedBy(s.f.ws)
		req.VersionIncrement = &agentreplv1.MergeWorkspaceVersionIncrement{ChangelogLine: "Merges record a version",
			Increment: &agentreplv1.MergeWorkspaceVersionIncrement_Minor{Minor: &agentreplv1.MergeWorkspaceVersionIncrementMinor{}}}
	})))
	requireRPCAnswered(t, "MergeWorkspace", resp, err)
	merge := s.awaitConcluded(t, root)
	s.d.AwaitLandingDeployed()

	// Assert: the landing is tagged 0.5.0 with the changelog line, and the
	// head draws it with the minor number apart.
	if merge.GetSuccess() == nil {
		t.Fatalf("the merge ended %v, want landed", merge.GetResult())
	}
	tag, tagged := s.repo.Tags()["0.5.0"]
	if !tagged || tag.Commit != merge.GetSuccess().GetCommit() || tag.Message != "Merges record a version" {
		t.Fatalf("tags = %+v, want 0.5.0 on the landed commit %s with the changelog line", s.repo.Tags(), merge.GetSuccess().GetCommit())
	}
	v := merge.GetHead().GetVersion()
	if v.GetLeading().GetText() != "0." || v.GetIncremented().GetText() != "5" || v.GetTrailing().GetText() != ".0" {
		t.Fatalf("head version = %+v, want 0. 5 .0", v)
	}
}
