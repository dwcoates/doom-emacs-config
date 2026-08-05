package sessioncontroller

import (
	"context"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
)

// newLeaseGateManager builds the smallest Manager guardMergeLease needs: the
// SSM it asks and the logger it reports through. Nothing else is reachable,
// because the gate runs BEFORE the session is brought up.
func newLeaseGateManager(applier *fakeApplier, logf func(string, ...any)) *Manager {
	return &Manager{cfg: Config{SSM: applier}, logf: logf}
}

func TestGuardMergeLease(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		held    bool
		who     submitter
		wantErr string
	}{
		{
			name: "a user prompt on an unleased workspace passes",
			held: false,
			who:  submitterUser,
		},
		{
			name: "a merge prompt on a leased workspace passes",
			held: true,
			who:  submitterMergeLeaseHolder,
		},
		{
			name:    "a user prompt on a leased workspace is refused",
			held:    true,
			who:     submitterUser,
			wantErr: "is being merged",
		},
		{
			name:    "a merge prompt with no lease held is refused",
			held:    false,
			who:     submitterMergeLeaseHolder,
			wantErr: "NO merge lease held",
		},
		{
			// The revival gate admits this submitter past HIBERNATION; it must
			// not also carry it past the merge lease, or a compact-first
			// revival would inject a full-context `/compact` into a session in
			// the middle of conflict resolution.
			name:    "a revival compaction on a leased workspace is refused",
			held:    true,
			who:     submitterRevival,
			wantErr: "revival of workspace",
		},
		{
			name: "a revival compaction on an unleased workspace passes",
			held: false,
			who:  submitterRevival,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var lines []string
			m := newLeaseGateManager(
				&fakeApplier{mergeLeases: map[string]bool{"ws": tt.held}},
				func(format string, args ...any) { lines = append(lines, format) },
			)

			// Act.
			err := m.guardMergeLease("ws", tt.who, "req-1", "frontend")

			// Assert.
			if tt.wantErr == "" {
				if err != nil {
					t.Fatalf("guardMergeLease = %v, want nil", err)
				}
				if len(lines) != 0 {
					t.Fatalf("a permitted submit logged %d line(s), want none", len(lines))
				}
				return
			}
			if err == nil {
				t.Fatalf("guardMergeLease = nil, want an error mentioning %q", tt.wantErr)
			}
			if !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("error = %q, want it to mention %q", err, tt.wantErr)
			}
			if len(lines) == 0 {
				t.Fatal("the refusal was not recorded through the canonical logger")
			}
		})
	}
}

func TestGuardMergeLeaseRefusalExplainsItself(t *testing.T) {
	// Arrange. The message is surfaced verbatim to whoever typed the prompt, so
	// it must say what happened, why, and what became of the text.
	m := newLeaseGateManager(
		&fakeApplier{mergeLeases: map[string]bool{"ws": true}},
		func(string, ...any) {},
	)

	// Act.
	err := m.guardMergeLease("ws", submitterUser, "req-1", "frontend")

	// Assert.
	if err == nil {
		t.Fatal("guardMergeLease = nil, want a refusal")
	}
	for _, want := range []string{"lease", "merged", "nothing was submitted and nothing was queued", "resubmit"} {
		if !strings.Contains(strings.ToLower(err.Error()), strings.ToLower(want)) {
			t.Fatalf("refusal %q does not explain %q", err, want)
		}
	}
}

func TestSubmitMergePromptRequiresARequestID(t *testing.T) {
	// Arrange.
	m := newLeaseGateManager(&fakeApplier{}, func(string, ...any) {})

	// Act.
	err := m.SubmitMergePrompt(context.Background(), "ws", "", "resolve it", "", corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)

	// Assert.
	if err == nil {
		t.Fatal("SubmitMergePrompt with no request id = nil, want an error")
	}
	if !strings.Contains(err.Error(), "request id") {
		t.Fatalf("error = %q, want it to name the missing request id", err)
	}
}

func TestSubmitMergePromptRefusesWithoutTheLease(t *testing.T) {
	// Arrange. No lease is held, so nothing may be submitted on a merge's
	// behalf — and the refusal must land before any session is brought up,
	// which a Manager with no locator proves by not panicking.
	m := newLeaseGateManager(&fakeApplier{}, func(string, ...any) {})

	// Act.
	err := m.SubmitMergePrompt(context.Background(), "ws", "req-1", "resolve it", "", corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)

	// Assert.
	if err == nil {
		t.Fatal("SubmitMergePrompt with no lease = nil, want an error")
	}
	if !strings.Contains(err.Error(), "NO merge lease held") {
		t.Fatalf("error = %q, want it to name the absent lease", err)
	}
}

func TestSubmitPromptRefusedWhileTheLeaseIsHeld(t *testing.T) {
	// Arrange.
	m := newLeaseGateManager(
		&fakeApplier{mergeLeases: map[string]bool{"ws": true}},
		func(string, ...any) {},
	)

	// Act.
	err := m.SubmitPrompt(context.Background(), "ws", "req-1", "hello", "", testPromptOrigin)

	// Assert.
	if err == nil {
		t.Fatal("SubmitPrompt into a leased workspace = nil, want a refusal")
	}
	if !strings.Contains(err.Error(), "is being merged") {
		t.Fatalf("error = %q, want the merge refusal", err)
	}
}

func TestSubmitWorkspaceInitialPromptRefusedWhileTheLeaseIsHeld(t *testing.T) {
	// Arrange. Machinery is not exempt: only the lease HOLDER may submit.
	m := newLeaseGateManager(
		&fakeApplier{mergeLeases: map[string]bool{"ws": true}},
		func(string, ...any) {},
	)

	// Act.
	err := m.SubmitWorkspaceInitialPrompt(context.Background(), "ws", "job-1", "hello", "")

	// Assert.
	if err == nil {
		t.Fatal("SubmitWorkspaceInitialPrompt into a leased workspace = nil, want a refusal")
	}
	if !strings.Contains(err.Error(), "is being merged") {
		t.Fatalf("error = %q, want the merge refusal", err)
	}
}

func TestInterruptForMergeWithNoLiveSessionIsSatisfied(t *testing.T) {
	// Arrange. The lease's precondition is that nothing of the user's runs
	// behind the shim, and a workspace with no session controller meets it.
	var lines []string
	m := &Manager{logf: func(format string, args ...any) { lines = append(lines, format) }}

	// Act.
	displaced, err := m.InterruptForMerge(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("InterruptForMerge with no live session = %v, want nil", err)
	}
	if displaced != nil {
		t.Fatalf("InterruptForMerge with no live session displaced %+v, want nothing", displaced)
	}
	if len(lines) != 1 {
		t.Fatalf("logged %d line(s), want the absent-session note recorded exactly once", len(lines))
	}
}

func TestRunningTurnReportsNothingWhileIdle(t *testing.T) {
	// Arrange. An idle session has no turn to displace, whatever text the last
	// one left behind.
	m := &Manager{}
	d := &sessionController{runningText: "do the thing"}

	// Act / Assert.
	if got := m.runningTurn(d); got != nil {
		t.Fatalf("runningTurn on an idle session = %+v, want nil", got)
	}
}

func TestRunningTurnReportsNothingForAPreDaemonTurn(t *testing.T) {
	// Arrange. A turn that predates this daemon leaves runningText empty, and
	// an empty prompt is not something that can be put back.
	m := &Manager{}
	d := &sessionController{turn: turnRecord{phase: turnPhaseNamed, turnID: "t_42"}, runningText: ""}

	// Act / Assert.
	if got := m.runningTurn(d); got != nil {
		t.Fatalf("runningTurn over a pre-daemon turn = %+v, want nil", got)
	}
}

func TestRunningTurnCarriesPromptAndPermissionMode(t *testing.T) {
	// Arrange. Both halves describe the SAME turn, so a resume needs both.
	m := &Manager{}
	d := &sessionController{
		turn:                  turnRecord{phase: turnPhaseNamed, turnID: "t_42"},
		runningText:           "do the thing",
		runningPermissionMode: "acceptEdits",
	}

	// Act.
	got := m.runningTurn(d)

	// Assert.
	if got == nil {
		t.Fatal("runningTurn over a live turn = nil, want the displaced turn")
	}
	if got.Prompt != "do the thing" || got.PermissionMode != "acceptEdits" {
		t.Fatalf("runningTurn = %+v, want the running prompt and its mode", got)
	}
}

func TestResumeDisplacedTurnRefusesAnEmptyPrompt(t *testing.T) {
	// Arrange. Nothing to put back is a caller bug, not a quiet no-op: a
	// swallowed one would look exactly like a resume that worked.
	m := &Manager{}

	// Act.
	err := m.ResumeDisplacedTurn(context.Background(), "ws", ssm.DisplacedTurn{})

	// Assert.
	if err == nil {
		t.Fatal("ResumeDisplacedTurn with no prompt = nil, want a refusal")
	}
}
