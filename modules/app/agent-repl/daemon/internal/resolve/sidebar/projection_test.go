package sidebar_test

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/resolve/footer"
)

// footerOf answers the footer the fanned resolver projects, so a test can
// hand it a fact the roster is never told.
func footerOf(t *testing.T, r sidebarResolver) footer.Resolver {
	t.Helper()
	f, ok := r.Resolver.(*fanned)
	if !ok {
		t.Fatalf("the resolver is %T, want the fanned pair", r.Resolver)
	}
	return f.footer
}

// footerFault is a standing fault in the cell the health partition gives its
// kind, as the fault surfaces open it.
func footerFault(t *testing.T, id, kind string, daemonScope bool) footer.Fault {
	t.Helper()
	cell, ok := health.FaultFooterCell(kind, daemonScope)
	if !ok {
		t.Fatalf("health.FaultFooterCell(%q) has no cell", kind)
	}
	return footer.Fault{ID: id, Kind: kind, Status: string(cell.Status), SubStatus: cell.SubStatus, At: epoch}
}

// TestAFaultOnlyTheFooterHoldsMovesTheRow is the owner's report of
// 2026-10-08: a standing `watch_open_refused` fault drew the strip blue while
// the rail and the tab read thinking, because the fault reached the footer
// alone. The row projects the footer's status, so it moves with it.
func TestAFaultOnlyTheFooterHoldsMovesTheRow(t *testing.T) {
	// Arrange: a turn in flight.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(theWS)

	// Act: the fault reaches the footer, and only the footer.
	footerOf(t, r).OpenFault(theWS, footerFault(t, "fault-1", health.KindWatchOpenRefused, false))

	// Assert
	if got := statusName(onlyRow(t, r)); got != "severed" {
		t.Fatalf("status = %q, want severed: the footer's agent_repl_fault · severed", got)
	}
}

func TestTheRowLeavesAFaultTheFooterClosed(t *testing.T) {
	// Arrange: a turn in flight under a standing fault.
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(theWS)
	footerOf(t, r).OpenFault(theWS, footerFault(t, "fault-1", health.KindWatchOpenRefused, false))

	// Act
	footerOf(t, r).CloseFault(theWS, "fault-1")

	// Assert
	if got := statusName(onlyRow(t, r)); got != "thinking" {
		t.Fatalf("status = %q, want thinking once the fault closed", got)
	}
}

func TestARefusedCloseDrawsTheRowClosing(t *testing.T) {
	// Arrange
	r := live(t, arrange(t))

	// Act
	footerOf(t, r).SetClosing(theWS, &footer.CloseBlocked{Reason: "held_prompts", Detail: "a prompt is held"})

	// Assert
	if got := statusName(onlyRow(t, r)); got != "closing" {
		t.Fatalf("status = %q, want closing", got)
	}
}

func TestAnImpairedDaemonDrawsTheRowDaemonImpaired(t *testing.T) {
	// Arrange
	r := live(t, arrange(t))

	// Act: a daemon-scoped fault stands on every strip.
	footerOf(t, r).OpenFault("", footerFault(t, "fault-1", health.KindWsmReadOnly, true))

	// Assert
	if got := statusName(onlyRow(t, r)); got != "daemon_impaired" {
		t.Fatalf("status = %q, want daemon_impaired", got)
	}
}

func TestAnOpenQuestionDrawsTheRowQuestion(t *testing.T) {
	// Arrange
	r := live(t, arrange(t))
	r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
	r.AckTurn(theWS)

	// Act
	footerOf(t, r).OnQuestion(theWS, agent("main"), &conversationv1.AgentQuestion{
		Id: &conversationv1.AgentQuestionId{Value: "q1"},
		Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
			Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
				Question: &conversationv1.AgentQuestionText{Text: "which one?"},
			}}},
		}},
	})

	// Assert
	if got := statusName(onlyRow(t, r)); got != "question" {
		t.Fatalf("status = %q, want question", got)
	}
}

// TestAGateDrawsTheRowForTheGate pins the owner's ruling of 2026-10-08: a
// workspace whose running turn waits on a permission or a question gate draws
// the gate's own arm, green, never thinking; a cold gate stays `waiting`.
func TestAGateDrawsTheRowForTheGate(t *testing.T) {
	tests := []struct {
		name string
		open func(f footer.Resolver)
		want string
	}{
		{name: "a permission gate", open: func(f footer.Resolver) {
			f.OnPermission(theWS, agent("main"), &conversationv1.AgentPermission{
				Id: &conversationv1.AgentPermissionId{Value: "p1"},
				Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
					Prompt: &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run make"},
				}},
			})
		}, want: "permission"},
		{name: "a cold gate", open: func(f footer.Resolver) {
			f.SetColdGate(theWS, footer.ColdGate{Standing: true, Cost: footer.ColdGateCost{Lead: "cold"}})
		}, want: "waiting"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a turn in flight.
			r := live(t, arrange(t))
			r.SetTurn(theWS, &footer.TurnStarted{At: epoch, Act: footer.ActPrompt})
			r.AckTurn(theWS)

			// Act
			tt.open(footerOf(t, r))

			// Assert
			if got := statusName(onlyRow(t, r)); got != tt.want {
				t.Fatalf("status = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAProjectedRowRecordsNoClaimViolation(t *testing.T) {
	// Arrange
	r := live(t, arrange(t))

	// Act
	footerOf(t, r).SetClosing(theWS, &footer.CloseBlocked{Reason: "held_prompts", Detail: "a prompt is held"})
	onlyRow(t, r)

	// Assert
	for _, op := range []string{"daemon.sidebar.status_claim", "daemon.sidebar.project_arm", "daemon.sidebar.footer_status_unset"} {
		if hasError(r.surfaces.Records(), op) {
			t.Fatalf("the roster recorded %s", op)
		}
	}
}
