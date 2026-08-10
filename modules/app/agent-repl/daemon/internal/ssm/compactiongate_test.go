package ssm

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ---------------------------------------------------------------------------
// THE COMPACTION GATE: a compaction is never the second one of an unchanged
// conversation.
//
// The two timestamps and their comparison are the whole policy, so the tests
// split along the two writers (what closes the gate, what re-opens it) and the
// one reader (what the comparison answers at each boundary).
// ---------------------------------------------------------------------------

// ---- The comparison ------------------------------------------------------

func TestCompactionGateRedundantVerdicts(t *testing.T) {
	tests := []struct {
		name string
		gate CompactionGate
		want bool
	}{
		{
			name: "a workspace with no history at all is not redundant",
			gate: CompactionGate{},
			want: false,
		},
		{
			name: "a compaction with nothing said since is redundant",
			gate: CompactionGate{CompactedAtMs: 200, PromptAtMs: 100},
			want: true,
		},
		{
			name: "a prompt after the compaction re-opens the gate",
			gate: CompactionGate{CompactedAtMs: 200, PromptAtMs: 201},
			want: false,
		},
		{
			name: "a prompt at the same instant as the compaction is not new material",
			gate: CompactionGate{CompactedAtMs: 200, PromptAtMs: 200},
			want: true,
		},
		{
			name: "prompts with no compaction on record are never redundant",
			gate: CompactionGate{PromptAtMs: 100},
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.gate.Redundant(); got != tc.want {
				t.Fatalf("Redundant() = %v, want %v (gate %+v)", got, tc.want, tc.gate)
			}
		})
	}
}

// ---- What closes the gate ------------------------------------------------

func TestCompactionGateClosesOnACompletedCompaction(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.NoteCompactionCompleted("ws1"); err != nil {
		t.Fatalf("NoteCompactionCompleted: %v", err)
	}

	// Assert.
	gate := mustGate(t, m, "ws1")
	if !gate.Redundant() {
		t.Fatalf("gate = %+v, want redundant after a completed compaction", gate)
	}
	if !cl.contains("compaction gate CLOSED ws=ws1") {
		t.Fatalf("missing gate-closed log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestCompactionGateNeverMovesBackwardsOnAReplayedCompaction(t *testing.T) {
	// Arrange — a compaction, then the user speaking, which re-opens the gate.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteCompactionCompleted("ws1"); err != nil {
		t.Fatalf("NoteCompactionCompleted: %v", err)
	}
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	replayed := mustGate(t, m, "ws1")

	// Act — the SAME compaction arrives again off a replayed stream. It is
	// older than the prompt and must not close the gate over it.
	if err := noteCompactionGateEdge(m.db, "ws1", "compacted_at", replayed.CompactedAtMs); err != nil {
		t.Fatalf("replaying the compaction edge: %v", err)
	}

	// Assert.
	gate := mustGate(t, m, "ws1")
	if gate.Redundant() {
		t.Fatalf("gate = %+v, want the prompt to still stand over a replayed older compaction", gate)
	}
}

func TestCompactionGateRefusesAnEmptyWorkspace(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	err := m.NoteCompactionCompleted("")

	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("err = %v, want an empty-workspace rejection", err)
	}
}

func TestCompactionGateReadRefusesAnEmptyWorkspace(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	_, err := m.CompactionGateOf("")

	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("err = %v, want an empty-workspace rejection", err)
	}
}

func TestCompactionGateReadReportsAFailedQuery(t *testing.T) {
	// Arrange — the gate's own table is gone, which is the shape every read
	// failure takes: the caller must see an error rather than a permissive zero.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if _, err := m.db.Exec(`DROP TABLE compaction_gate`); err != nil {
		t.Fatalf("dropping the gate table: %v", err)
	}

	_, err := m.CompactionGateOf("ws1")

	if err == nil || !strings.Contains(err.Error(), "read compaction gate") {
		t.Fatalf("err = %v, want the read failure surfaced", err)
	}
}

func TestCompactionGateReportsAFailedCloseWrite(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if _, err := m.db.Exec(`DROP TABLE compaction_gate`); err != nil {
		t.Fatalf("dropping the gate table: %v", err)
	}

	err := m.NoteCompactionCompleted("ws1")

	if err == nil || !strings.Contains(err.Error(), "record compaction gate compacted_at") {
		t.Fatalf("err = %v, want the write failure surfaced", err)
	}
}

// ---- What re-opens the gate ----------------------------------------------

func TestCompactionGateReopensOnTheUsersPrompt(t *testing.T) {
	// Arrange — compacted, so the gate stands closed.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteCompactionCompleted("ws1"); err != nil {
		t.Fatalf("NoteCompactionCompleted: %v", err)
	}

	// Act.
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	// Assert — there is new material, so a compaction is no longer a duplicate.
	if gate := mustGate(t, m, "ws1"); gate.Redundant() {
		t.Fatalf("gate = %+v, want the user's prompt to re-open it", gate)
	}
}

func TestCompactionGateStaysClosedThroughIdleMachinery(t *testing.T) {
	// Arrange — compacted, so the gate stands closed.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteCompactionCompleted("ws1"); err != nil {
		t.Fatalf("NoteCompactionCompleted: %v", err)
	}

	// Act — a keep-alive ping. It is the daemon refreshing a prompt cache, not
	// the user saying something, and it is exactly what an idle session does
	// while it is idle enough to be compacted.
	if err := m.MarkPromptAccepted("ws1", "s1", "ping-1", PromptAdmissionIdleMachinery, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	// Assert.
	if gate := mustGate(t, m, "ws1"); !gate.Redundant() {
		t.Fatalf("gate = %+v, want a keep-alive ping to leave it closed", gate)
	}
}

func TestCompactionGateReopensOnAPromptAcceptedIdempotently(t *testing.T) {
	// Arrange — a turn is already claimed, so the accept takes the IDEMPOTENT
	// branch. The user still spoke.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteCompactionCompleted("ws1"); err != nil {
		t.Fatalf("NoteCompactionCompleted: %v", err)
	}
	if err := applyTest(m, evTurnStarted("s1", 1)); err != nil {
		t.Fatalf("turn started: %v", err)
	}

	// Act.
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	// Assert.
	if gate := mustGate(t, m, "ws1"); gate.Redundant() {
		t.Fatalf("gate = %+v, want the idempotent accept to re-open it", gate)
	}
}

func TestCompactionGatePromptWriteFailureLeavesThePromptAccepted(t *testing.T) {
	// Arrange — the gate's table is gone, so its bookkeeping write cannot land.
	// Losing the user's prompt over that would be a far larger harm than one
	// declined compaction.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if _, err := m.db.Exec(`DROP TABLE compaction_gate`); err != nil {
		t.Fatalf("dropping the gate table: %v", err)
	}

	// Act.
	err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {})

	// Assert — the prompt stands, and the failure is reported rather than
	// swallowed.
	if err != nil {
		t.Fatalf("MarkPromptAccepted = %v, want the prompt accepted despite the gate write", err)
	}
	if got := mustCurrent(t, m, "ws1").GetState(); got != frontendv1.RenderState_RENDER_STATE_SUBMITTING {
		t.Fatalf("state = %s, want SUBMITTING", renderName(got))
	}
	if !cl.contains("recording the compaction gate's prompt edge FAILED ws=ws1") {
		t.Fatalf("missing gate-write failure log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

// ---- The clear, which closes the same gate -------------------------------

func TestCompactionGateRedundantVerdictsForAClear(t *testing.T) {
	tests := []struct {
		name string
		gate CompactionGate
		want bool
	}{
		{
			name: "a clear with nothing said since is redundant",
			gate: CompactionGate{ClearedAtMs: 200, PromptAtMs: 100},
			want: true,
		},
		{
			name: "a prompt after the clear re-opens the gate",
			gate: CompactionGate{ClearedAtMs: 200, PromptAtMs: 201},
			want: false,
		},
		{
			name: "a clear after an older compaction keeps the gate closed",
			gate: CompactionGate{CompactedAtMs: 100, ClearedAtMs: 300, PromptAtMs: 200},
			want: true,
		},
		{
			name: "a prompt after the clear outranks the older compaction too",
			gate: CompactionGate{CompactedAtMs: 100, ClearedAtMs: 200, PromptAtMs: 300},
			want: false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			if got := tc.gate.Redundant(); got != tc.want {
				t.Fatalf("Redundant() = %v, want %v (gate %+v)", got, tc.want, tc.gate)
			}
		})
	}
}

func TestCompactionGateClosesOnACompletedClear(t *testing.T) {
	// Arrange.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.NoteConversationCleared("ws1"); err != nil {
		t.Fatalf("NoteConversationCleared: %v", err)
	}

	// Assert.
	gate := mustGate(t, m, "ws1")
	if !gate.Redundant() {
		t.Fatalf("gate = %+v, want redundant after a completed clear", gate)
	}
	if !cl.contains("compaction gate CLOSED ws=ws1 cleared_at=") {
		t.Fatalf("missing gate-closed log:\n%s", strings.Join(cl.lines, "\n"))
	}
}

func TestCompactionGateClearIsRecordedApartFromTheCompaction(t *testing.T) {
	// Arrange — nothing has ever compacted this workspace.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	// Act.
	if err := m.NoteConversationCleared("ws1"); err != nil {
		t.Fatalf("NoteConversationCleared: %v", err)
	}

	// Assert — a decline taken from this gate can say the conversation was
	// cleared rather than misreporting a compaction that never happened.
	gate := mustGate(t, m, "ws1")
	if gate.CompactedAtMs != 0 {
		t.Fatalf("gate = %+v, want a clear to leave compacted_at untouched", gate)
	}
	if gate.ClearedAtMs == 0 {
		t.Fatalf("gate = %+v, want the clear instant recorded", gate)
	}
}

func TestCompactionGateReopensOnThePromptAfterAClear(t *testing.T) {
	// Arrange — cleared, so the gate stands closed.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteConversationCleared("ws1"); err != nil {
		t.Fatalf("NoteConversationCleared: %v", err)
	}

	// Act.
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}

	// Assert.
	if gate := mustGate(t, m, "ws1"); gate.Redundant() {
		t.Fatalf("gate = %+v, want the prompt after the clear to re-open it", gate)
	}
}

func TestCompactionGateNeverMovesBackwardsOnAReplayedClear(t *testing.T) {
	// Arrange — a clear, then the user speaking.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if err := m.NoteConversationCleared("ws1"); err != nil {
		t.Fatalf("NoteConversationCleared: %v", err)
	}
	if err := m.MarkPromptAccepted("ws1", "s1", "req-1", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err != nil {
		t.Fatalf("MarkPromptAccepted: %v", err)
	}
	replayed := mustGate(t, m, "ws1")

	// Act — the same ContextCleared arrives again off a replayed stream.
	if err := noteCompactionGateEdge(m.db, "ws1", "cleared_at", replayed.ClearedAtMs); err != nil {
		t.Fatalf("replaying the clear edge: %v", err)
	}

	// Assert.
	if gate := mustGate(t, m, "ws1"); gate.Redundant() {
		t.Fatalf("gate = %+v, want the prompt to still stand over a replayed older clear", gate)
	}
}

func TestCompactionGateClearRefusesAnEmptyWorkspace(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})

	err := m.NoteConversationCleared("")

	if err == nil || !strings.Contains(err.Error(), "empty workspace") {
		t.Fatalf("err = %v, want an empty-workspace rejection", err)
	}
}

func TestCompactionGateClearReportsAFailedCloseWrite(t *testing.T) {
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	if _, err := m.db.Exec(`DROP TABLE compaction_gate`); err != nil {
		t.Fatalf("dropping the gate table: %v", err)
	}

	err := m.NoteConversationCleared("ws1")

	if err == nil || !strings.Contains(err.Error(), "record compaction gate cleared_at") {
		t.Fatalf("err = %v, want the write failure surfaced", err)
	}
}

// mustGate reads a workspace's compaction gate or fails the test.
func mustGate(t *testing.T, m *Manager, workspace string) CompactionGate {
	t.Helper()
	gate, err := m.CompactionGateOf(workspace)
	if err != nil {
		t.Fatalf("CompactionGateOf(%s): %v", workspace, err)
	}
	return gate
}
