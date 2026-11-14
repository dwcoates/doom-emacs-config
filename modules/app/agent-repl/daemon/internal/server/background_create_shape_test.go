package server

import (
	"context"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
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

	spec := workspace.NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir = "/tmp/repo"

	// Act.
	ticket := s.startBackgroundCreate(bgCtx, cancel, release, spec, "op-helper")
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

// TestABackgroundCreateThatPanicsIsAnsweredFailedOnItsOp pins the option-B
// create's panic boundary: the op is answered Failed naming the panic, the
// panic is recorded with the op, the create and the stack, and the panic goes
// on with its original value.
func TestABackgroundCreateThatPanicsIsAnsweredFailedOnItsOp(t *testing.T) {
	const value = "injected create invariant"
	cases := []struct {
		name   string
		assert func(t *testing.T, raised any, failed string, log *dlog.TestLogger)
	}{
		{name: "the panic goes on with its original value", assert: func(t *testing.T, raised any, _ string, _ *dlog.TestLogger) {
			if raised != value {
				t.Fatalf("raised %v, want %q raised again", raised, value)
			}
		}},
		{name: "the op is answered failed naming the panic", assert: func(t *testing.T, _ any, failed string, _ *dlog.TestLogger) {
			if !strings.Contains(failed, value) {
				t.Fatalf("failed = %q, want the op's internal failure to name the panic", failed)
			}
		}},
		{name: "the panic is recorded with the op, the create and the stack", assert: func(t *testing.T, _ any, _ string, log *dlog.TestLogger) {
			record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.server.create_workspace",
				Message: "a background create panicked: its op is answered failed, and the daemon panics again"})
			stack, _ := record.Context["stack"].(string)
			if !ok || record.Context["op_id"] != "op-panics" || record.Context["create_id"] != "create-own" ||
				record.Context["panic"] != value || !strings.Contains(stack, "RunCreate") {
				t.Fatalf("records = %+v, want the panic recorded with its op, create and stack", log.Records())
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			s := h.Server.(*server)
			h.Verbs.createPanic = value
			stream := proveDaemonSubscription(t, h)
			spec := workspace.NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
			spec.RepoDir = "/tmp/repo"
			reporter := s.CreateProgress("op-panics")
			spec.Progress = reporter
			ticket := s.deps.Verbs.EnterCreate(spec)

			// Act.
			raised := func() (r any) {
				defer func() { r = recover() }()
				s.answerBackgroundCreateForTest(ticket, reporter)
				return nil
			}()
			if !stream.Receive() {
				t.Fatalf("receive the outcome: %v", stream.Err())
			}
			prog := stream.Msg().GetMutationProgress()
			if prog.GetOpId() != "op-panics" || prog.GetCreate().GetFailed() == nil {
				t.Fatalf("progress = %v, want failed on op-panics", prog)
			}

			// Assert.
			tc.assert(t, raised, prog.GetCreate().GetFailed().GetInternal(), log)
		})
	}
}

// answerBackgroundCreateForTest runs runBackgroundCreate in the caller's
// goroutine, so a test recovers what it panics with.
func (s *server) answerBackgroundCreateForTest(ticket *workspace.CreateTicket, reporter workspace.CreateProgress) {
	s.runBackgroundCreate(context.Background(), ticket, reporter)
}
