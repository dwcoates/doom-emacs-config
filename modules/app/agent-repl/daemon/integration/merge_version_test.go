//go:build integration

package integration

import (
	"testing"

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
	commandfileWrite(t, s.d, "workspace_commands_version.json",
		`[{"type":"merge","project_dir":"`+s.f.ws.GetDir()+`","one_shot":true,"version_part":"minor","changelog_line":"Merges record a version"}]`)
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
