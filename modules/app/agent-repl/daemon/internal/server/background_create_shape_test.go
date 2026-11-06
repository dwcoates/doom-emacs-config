package server

import (
	"context"
	"slices"
	"testing"

	"agent-repld/internal/sourcescan"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

func TestEveryBackgroundCreateIsStartedByStartBackgroundCreate(t *testing.T) {
	// Act: the package's own production sources.
	sites := sourcescan.Sites(t, "s.runBackgroundCreate(")

	// Assert: the one goroutine that runs a create detached lives in
	// startBackgroundCreate, so no op-carrying create can run it apart.
	if !slices.Equal(sites, []string{"workspaces.go"}) {
		t.Fatalf("runBackgroundCreate called from %v, want only startBackgroundCreate in workspaces.go", sites)
	}
}

func TestStartBackgroundCreateReportsOnItsOpAndSurvivesTheCallersContext(t *testing.T) {
	// Arrange: a create held open until the caller's context is gone.
	h := newHarness(t)
	s := h.Server.(*server)
	h.Verbs.createRec = wsm.Workspace{ID: "ws-created", Dir: "/tmp/ws", Name: "minted"}
	h.Verbs.createEntered = make(chan struct{})
	h.Verbs.createRelease = make(chan struct{})
	stream := proveDaemonSubscription(t, h)
	admitted, release, err := s.deps.Creates.Admit(context.Background())
	if err != nil {
		t.Fatalf("Admit: %v", err)
	}
	callerCtx, callerCancel := context.WithCancel(admitted)
	bgCtx, cancel := s.backgroundContext(callerCtx)

	// Act.
	ticket := s.startBackgroundCreate(bgCtx, cancel, release, workspace.CreateSpec{RepoDir: "/tmp/repo"}, "op-helper")
	<-h.Verbs.createEntered
	callerCancel()
	close(h.Verbs.createRelease)

	// Assert.
	if ticket == nil {
		t.Fatalf("startBackgroundCreate answered no ticket")
	}
	if !stream.Receive() {
		t.Fatalf("receive the outcome: %v", stream.Err())
	}
	prog := stream.Msg().GetMutationProgress()
	if prog.GetOpId() != "op-helper" || prog.GetCreate().GetSucceeded() == nil {
		t.Fatalf("progress = %v, want succeeded on op-helper", prog)
	}
	if spec := h.Verbs.createSpecs[0]; spec.Progress == nil || spec.Progress.OpID() != "op-helper" {
		t.Fatalf("the create ran with no reporter on op-helper: %+v", spec.Progress)
	}
}
