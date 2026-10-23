//go:build integration

package integration

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/integration/harness"
)

// skillsChangedUpdate is the shim's statement that its vendor's skill set went
// stale on disk (conversation.v1.SessionSkillsChanged).
func skillsChangedUpdate(paths ...string) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_SkillsChanged{
		SkillsChanged: &conversationv1.SessionSkillsChanged{Paths: paths},
	}}
}

// TestAStaleSkillSetReplacesAnIdleSessionsShim pins the regression: the vendor
// loads its skills once, at its start, so a skill installed mid-session reaches
// the catalog only through a new vendor process. An idle session whose shim
// states its skill set stale is replaced through the bounce a stale build
// takes, and its conversation resumes on the new shim, whose StartSession reads
// the catalog afresh.
func TestAStaleSkillSetReplacesAnIdleSessionsShim(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the
	// graceful stand-down the fake shim ends by exiting, and the shim link it
	// severs, exactly as a restart's.
	f.d.ExpectWorkspaceRestartWarnings()
	f.shim.ExpectStartSession()

	// Act
	f.shim.PushSessionUpdate(skillsChangedUpdate("/config/skills/agent-repl-workspaces"))

	// Assert: the session resumes on the replacement shim.
	resume := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1)).ExpectStartSession()
	if resume.GetResume() == nil {
		t.Fatalf("StartSession on the replacement shim = %v, want a resume source", resume)
	}
}
