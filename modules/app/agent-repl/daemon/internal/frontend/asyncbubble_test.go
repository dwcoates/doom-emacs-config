package frontend

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// openKind is the shortest valid spec for each kind, so a test that is about
// something else does not restate the whole spec.
func openKind(t *testing.T, kind DetachKind) *frontendv1.AsyncBubble {
	t.Helper()
	spec := BubbleSpec{TaskID: "t1", Kind: kind, OriginToolUseID: "tu1", StartedAtMs: 5}
	if kind == DetachUnrecognized {
		spec.ToolName = "Frobnicate"
	}
	b, err := OpenAsyncBubble(spec)
	if err != nil {
		t.Fatalf("OpenAsyncBubble(%s): %v", kind, err)
	}
	return b
}

func responseEmission(id string) *frontendv1.AgentEmission {
	return &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_Response{
		Response: &frontendv1.AgentResponse{Body: &datav1.ApiAssistantMessage{Id: id}},
	}}
}

func int32p(v int32) *int32 { return &v }

// --- opening ---------------------------------------------------------------

func TestOpenAsyncBubbleDerivesItsIdFromTheTaskId(t *testing.T) {
	first := openKind(t, DetachAgent)
	second := openKind(t, DetachAgent)
	if first.GetId() != second.GetId() {
		t.Fatalf("the same detachment must resolve to the same bubble across a replay, got %q then %q", first.GetId(), second.GetId())
	}
}

func TestOpenAsyncBubbleNeverMintsABlankId(t *testing.T) {
	if id := openKind(t, DetachAgent).GetId(); id == "" {
		t.Fatal("a bubble id is the routing handle and is never empty")
	}
}

func TestOpenAsyncBubbleRefusesADetachmentWithNoTaskId(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{Kind: DetachAgent, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a detachment with no task id has nothing to mint an id from and must be refused")
	}
}

func TestOpenAsyncBubbleRefusesADetachmentItCannotAttributeToACall(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Kind: DetachAgent})
	if err == nil {
		t.Fatal("an unattributable detachment is a daemon fault, never a bubble with a blank origin")
	}
}

func TestOpenAsyncBubbleRefusesAnUnresolvedKind(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Kind: DetachUnresolved, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a kindless bubble carries no body a renderer can draw and must be refused")
	}
}

func TestOpenAsyncBubbleRefusesAnUnclassifiedSpawnThatNamesNoTool(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Kind: DetachUnrecognized, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("the unclassified arm exists to NAME the tool it could not classify; an anonymous one must be refused")
	}
}

func TestOpenAsyncBubbleCarriesTheToolNameOnTheUnclassifiedArm(t *testing.T) {
	if got := openKind(t, DetachUnrecognized).GetUnclassified().GetToolName(); got != "Frobnicate" {
		t.Fatalf("want tool_name=%q, got %q", "Frobnicate", got)
	}
}

func TestOpenAsyncBubbleCarriesTheCommandOnTheShellArm(t *testing.T) {
	b, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Kind: DetachShell, OriginToolUseID: "tu1", Command: "sleep 9"})
	if err != nil {
		t.Fatal(err)
	}
	if got := b.GetShell().GetCommand(); got != "sleep 9" {
		t.Fatalf("want command=%q, got %q", "sleep 9", got)
	}
}

func TestOpenAsyncBubbleOpensLive(t *testing.T) {
	if openKind(t, DetachAgent).GetLiveness().GetLive() == nil {
		t.Fatal("a bubble opens live: the work has just been launched")
	}
}

func TestOpenAsyncBubbleStatesTheTailCapOnAnItemCountedFold(t *testing.T) {
	if got := openKind(t, DetachAgent).GetAgent().GetFold().GetTailCap(); got != StreamItemCap {
		t.Fatalf("the cap is a daemon fact stated on the fold: want %d, got %d", StreamItemCap, got)
	}
}

func TestOpenAsyncBubbleCarriesTheParentPointerForANestedDispatch(t *testing.T) {
	b, err := OpenAsyncBubble(BubbleSpec{TaskID: "t2", Kind: DetachAgent, OriginToolUseID: "tu2", ParentBubbleID: "bubble:t1"})
	if err != nil {
		t.Fatal(err)
	}
	if got := b.GetParentBubbleId(); got != "bubble:t1" {
		t.Fatalf("want parent_bubble_id=%q, got %q", "bubble:t1", got)
	}
}

// --- classification --------------------------------------------------------

func TestDetachKindFromTaskKindResolvesAnAgent(t *testing.T) {
	if got := DetachKindFromTaskKind(corev1.TaskKind_TASK_KIND_AGENT); got != DetachAgent {
		t.Fatalf("want agent, got %s", got)
	}
}

func TestDetachKindFromTaskKindResolvesAShell(t *testing.T) {
	if got := DetachKindFromTaskKind(corev1.TaskKind_TASK_KIND_SHELL); got != DetachShell {
		t.Fatalf("want shell, got %s", got)
	}
}

func TestDetachKindFromTaskKindResolvesAWorkflow(t *testing.T) {
	if got := DetachKindFromTaskKind(corev1.TaskKind_TASK_KIND_WORKFLOW); got != DetachWorkflow {
		t.Fatalf("want workflow, got %s", got)
	}
}

func TestDetachKindFromTaskKindNeverReadsAnUnsetEnumAsAnUnknownTool(t *testing.T) {
	if got := DetachKindFromTaskKind(corev1.TaskKind_TASK_KIND_UNSPECIFIED); got != DetachUnresolved {
		t.Fatalf("an unset kind is a shim omission, not the unclassified verdict; got %s", got)
	}
}

// --- agent fold ------------------------------------------------------------

func TestAppendAsyncEmissionsProducesTheAgentArm(t *testing.T) {
	up, err := AppendAsyncEmissions(openKind(t, DetachAgent), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetAgent() == nil {
		t.Fatalf("an agent bubble's update must carry the agent arm, got %T", up.GetUpdate())
	}
}

func TestAppendAsyncEmissionsAddressesTheUpdateToItsBubble(t *testing.T) {
	b := openKind(t, DetachAgent)
	up, err := AppendAsyncEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetBubbleId() != b.GetId() {
		t.Fatalf("want bubble_id=%q, got %q", b.GetId(), up.GetBubbleId())
	}
}

func TestAppendAsyncEmissionsFoldsIntoTheBubbleItPushesFrom(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := AppendAsyncEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7); err != nil {
		t.Fatal(err)
	}
	if got := len(b.GetAgent().GetEmissions()); got != 1 {
		t.Fatalf("the snapshot fold and the delta come from one call: want 1 folded emission, got %d", got)
	}
}

func TestAppendAsyncEmissionsRejectsAnAgentUpdateAddressedToAShellBubble(t *testing.T) {
	_, err := AppendAsyncEmissions(openKind(t, DetachShell), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err == nil {
		t.Fatal("an update whose arm does not match the bubble's kind is a daemon bug and must be rejected, not coerced")
	}
}

func TestAppendAsyncEmissionsNamesBothKindsInItsRefusal(t *testing.T) {
	_, err := AppendAsyncEmissions(openKind(t, DetachShell), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err == nil || !strings.Contains(err.Error(), "shell") || !strings.Contains(err.Error(), "agent") {
		t.Fatalf("the refusal must name the disagreement, got %v", err)
	}
}

func TestAppendAsyncEmissionsProducesNoUpdateForAnEmptyBatch(t *testing.T) {
	up, err := AppendAsyncEmissions(openKind(t, DetachAgent), nil, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("an empty batch is not a push")
	}
}

func TestAppendAsyncEmissionsRecordsLastActivity(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := AppendAsyncEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 77); err != nil {
		t.Fatal(err)
	}
	if got := b.GetLiveness().GetLive().GetLastActivityMs(); got != 77 {
		t.Fatalf("want last_activity_ms=77, got %d", got)
	}
}

func TestAppendAsyncEmissionsKeepsTheTailAtTheCap(t *testing.T) {
	b := openKind(t, DetachAgent)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}
	if _, err := AppendAsyncEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}
	if got := len(b.GetAgent().GetEmissions()); got != StreamItemCap {
		t.Fatalf("want the fold capped at %d, got %d", StreamItemCap, got)
	}
}

func TestAppendAsyncEmissionsReportsWhatTheCapDropped(t *testing.T) {
	b := openKind(t, DetachAgent)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}
	if _, err := AppendAsyncEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetAgent().GetFold().GetDroppedBefore(); got != 5 {
		t.Fatalf("a capped fold that says nothing is indistinguishable from a complete one: want dropped_before=5, got %d", got)
	}
}

func TestAppendAsyncEmissionsDoesNotAliasTheBubblesFoldOntoTheUpdate(t *testing.T) {
	b := openKind(t, DetachAgent)
	up, err := AppendAsyncEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetAgent().GetFold() == b.GetAgent().GetFold() {
		t.Fatal("a queued frame's fold must not be rewritten by later folding")
	}
}

// --- journal fold ----------------------------------------------------------

func TestAppendAsyncJournalRowsProducesTheJournalArm(t *testing.T) {
	up, err := AppendAsyncJournalRows(openKind(t, DetachWorkflow),
		[]*frontendv1.AsyncWorkflowJournalRow{{Label: "step"}}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetJournal() == nil {
		t.Fatalf("a workflow bubble's update must carry the journal arm, got %T", up.GetUpdate())
	}
}

func TestAppendAsyncJournalRowsRejectsAJournalUpdateAddressedToAShellBubble(t *testing.T) {
	_, err := AppendAsyncJournalRows(openKind(t, DetachShell),
		[]*frontendv1.AsyncWorkflowJournalRow{{Label: "step"}}, 7)
	if err == nil {
		t.Fatal("a journal update addressed to a shell bubble is a daemon bug and must be rejected")
	}
}

func TestAppendAsyncJournalRowsKeepsTheTailAtTheCap(t *testing.T) {
	b := openKind(t, DetachWorkflow)
	var rows []*frontendv1.AsyncWorkflowJournalRow
	for i := 0; i < StreamItemCap+3; i++ {
		rows = append(rows, &frontendv1.AsyncWorkflowJournalRow{Label: "step"})
	}
	if _, err := AppendAsyncJournalRows(b, rows, 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetJournal().GetFold().GetDroppedBefore(); got != 3 {
		t.Fatalf("want dropped_before=3, got %d", got)
	}
}

// --- byte spools -----------------------------------------------------------

func TestAppendAsyncOutputProducesTheShellArmForAShellBubble(t *testing.T) {
	up, err := AppendAsyncOutput(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell() == nil {
		t.Fatalf("a shell bubble's append must carry the shell arm, got %T", up.GetUpdate())
	}
}

func TestAppendAsyncOutputProducesTheUnclassifiedArmForAnUnclassifiedBubble(t *testing.T) {
	up, err := AppendAsyncOutput(openKind(t, DetachUnrecognized), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetUnclassified() == nil {
		t.Fatalf("an unclassified bubble's append must carry the unclassified arm, got %T", up.GetUpdate())
	}
}

func TestAppendAsyncOutputStartsTheFirstAppendAtOffsetZero(t *testing.T) {
	up, err := AppendAsyncOutput(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 0 {
		t.Fatalf("want from_offset=0, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendAsyncOutputTakesFromOffsetFromTheSpoolsOwnCursor(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendAsyncOutput(b, "de", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 3 {
		t.Fatalf("the second append must start where the spool's cursor stood: want 3, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendAsyncOutputAdvancesTheSpoolCursorByTheAppendedBytes(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetShell().GetOutput().GetThroughOffset(); got != 3 {
		t.Fatalf("want through_offset=3, got %d", got)
	}
}

func TestAppendAsyncOutputFoldsTheBytesIntoTheSpool(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendAsyncOutput(b, "de", 8); err != nil {
		t.Fatal(err)
	}
	if got := b.GetShell().GetOutput().GetText(); got != "abcde" {
		t.Fatalf("want spool text %q, got %q", "abcde", got)
	}
}

func TestAppendAsyncOutputRejectsAByteAppendAddressedToAnAgentBubble(t *testing.T) {
	if _, err := AppendAsyncOutput(openKind(t, DetachAgent), "abc", 7); err == nil {
		t.Fatal("an agent bubble has no byte spool; an append addressed to it must be rejected")
	}
}

func TestAppendAsyncOutputProducesNoUpdateForAnEmptyChunk(t *testing.T) {
	up, err := AppendAsyncOutput(openKind(t, DetachShell), "", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("a quiet read is not a push")
	}
}

func TestAppendAsyncOutputThroughStartsAFreshBubbleAtOffsetZero(t *testing.T) {
	up, err := AppendAsyncOutputThrough(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 0 {
		t.Fatalf("a newly opened bubble carries an empty body, so the first append starts at 0, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendAsyncOutputThroughAppendsOnlyWhatIsPastTheCursor(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutputThrough(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendAsyncOutputThrough(b, "abcde", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetText() != "de" {
		t.Fatalf("a restated snapshot must append only its new bytes, got %q", up.GetShell().GetText())
	}
}

func TestAppendAsyncOutputThroughResumesFromASnapshotsThroughOffset(t *testing.T) {
	// A bubble redelivered in a snapshot arrives with its spool cursor already
	// advanced; the next append must continue from there, not from zero.
	b := openKind(t, DetachShell)
	b.GetShell().GetOutput().Text = "abc"
	b.GetShell().GetOutput().ThroughOffset = 3
	up, err := AppendAsyncOutputThrough(b, "abcde", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 3 {
		t.Fatalf("want from_offset=3, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendAsyncOutputThroughProducesNoUpdateForAnUnchangedSnapshot(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutputThrough(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendAsyncOutputThrough(b, "abc", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("a retrieval that restates what the spool already holds is not a push")
	}
}

func TestAppendAsyncOutputThroughRefusesASourceThatRewound(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendAsyncOutputThrough(b, "abcdef", 7); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendAsyncOutputThrough(b, "ab", 8); err == nil {
		t.Fatal("a snapshot shorter than the cursor is a gap, and re-appending from zero would duplicate what the client holds")
	}
}

// --- settlement ------------------------------------------------------------

func TestSettleAsyncBubbleResolvesDoneFromAZeroExitCode(t *testing.T) {
	b := openKind(t, DetachShell)
	up, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(0), AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}
	if up.GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatal("exit code 0 is a real zero and always means clean exit")
	}
}

func TestSettleAsyncBubbleResolvesErrorFromANonzeroExitCode(t *testing.T) {
	b := openKind(t, DetachShell)
	up, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(2), AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}
	if up.GetLiveness().GetLiveness().GetSettled().GetError() == nil {
		t.Fatal("a nonzero exit code resolves the error outcome, whatever the shim's status word said")
	}
}

func TestSettleAsyncBubbleKeepsTheExitCodeBesideTheOutcome(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_KILLED, ExitCode: int32p(137), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if got := b.GetLiveness().GetSettled().GetShellExit().GetCode(); got != 137 {
		t.Fatalf("a killed process still carries its exit status: want 137, got %d", got)
	}
}

func TestSettleAsyncBubbleReadsAKillAsKilledDespiteItsNonzeroExit(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_KILLED, ExitCode: int32p(137), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatal("work stopped from outside did not fail, and must not be reported to the user as an error")
	}
}

func TestSettleAsyncBubbleLeavesShellExitAbsentForWorkThatIsNotAProcess(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetLiveness().GetSettled().GetShellExit() != nil {
		t.Fatal("an agent concluded, it did not exit; a fabricated exit status would be unreadable")
	}
}

func TestSettleAsyncBubbleReadsALostTaskAsKilledRatherThanDone(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_LOST, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatal("a lost task stopped, but nothing says it succeeded")
	}
}

func TestSettleAsyncBubbleRefusesAnUnspecifiedStatusWithNoExitCode(t *testing.T) {
	_, err := SettleAsyncBubble(openKind(t, DetachAgent), AsyncVerdict{AtMs: 9})
	if err == nil {
		t.Fatal("a settled bubble with no outcome is unrepresentable and must be refused, never stood in for")
	}
}

func TestSettleAsyncBubbleCarriesTheFailureMessageWithoutManufacturingOne(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_ERROR, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if got := b.GetLiveness().GetSettled().GetError().GetMessage(); got != "" {
		t.Fatalf("a source that reported failure without a reason gets no manufactured one, got %q", got)
	}
}

func TestSettleAsyncBubbleStopsRecordingActivityOnceSettled(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(0), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendAsyncOutput(b, "late", 99); err != nil {
		t.Fatal(err)
	}
	if b.GetLiveness().GetLive() != nil {
		t.Fatal("a late append must not resurrect a settled bubble's live arm")
	}
}

// --- classification verdict on the tool card -------------------------------

func toolCallItem(toolUseID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_ToolCall{
			ToolCall: &frontendv1.AgentToolCall{Call: &datav1.ToolUseBlock{Id: toolUseID}},
		}},
	}}
}

func toolOutcomeItem(toolUseID string) *frontendv1.ConversationItem {
	return &frontendv1.ConversationItem{Item: &frontendv1.ConversationItem_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_ToolOutcome{
			ToolOutcome: &frontendv1.AgentToolOutcome{ToolUseId: toolUseID},
		}},
	}}
}

func TestStampSpawnedBubbleIDsStampsTheCall(t *testing.T) {
	item := toolCallItem("tu1")
	StampSpawnedBubbleIDs([]*frontendv1.ConversationItem{item},
		func(string) string { return "bubble:t1" })
	if got := item.GetAgent().GetToolCall().GetSpawnedBubbleId(); got != "bubble:t1" {
		t.Fatalf("want the call stamped with the bubble id, got %q", got)
	}
}

func TestStampSpawnedBubbleIDsStampsTheOutcomeWithTheSameString(t *testing.T) {
	call, outcome := toolCallItem("tu1"), toolOutcomeItem("tu1")
	StampSpawnedBubbleIDs([]*frontendv1.ConversationItem{call, outcome},
		func(string) string { return "bubble:t1" })
	if call.GetAgent().GetToolCall().GetSpawnedBubbleId() != outcome.GetAgent().GetToolOutcome().GetSpawnedBubbleId() {
		t.Fatal("the daemon resolves the id once and stamps the same string on both")
	}
}

func TestStampSpawnedBubbleIDsLeavesACallThatDetachedNothingEmpty(t *testing.T) {
	item := toolCallItem("tu1")
	StampSpawnedBubbleIDs([]*frontendv1.ConversationItem{item}, func(string) string { return "" })
	if got := item.GetAgent().GetToolCall().GetSpawnedBubbleId(); got != "" {
		t.Fatalf("empty means 'this call detached nothing' and is the only reading of empty, got %q", got)
	}
}

// --- frame plumbing --------------------------------------------------------

func TestAsyncBubbleDeltaFrameWrapsTheDeltaInItsArm(t *testing.T) {
	d := &frontendv1.AsyncBubbleDelta{Workspace: "/ws"}
	if got := AsyncBubbleDeltaFrame(d).GetAsyncBubbleDelta(); got != d {
		t.Fatalf("want the delta on frame arm 20, got %v", got)
	}
}

func TestAnAsyncBubbleDeltaRoutesToItsOwnWorkspace(t *testing.T) {
	frame := AsyncBubbleDeltaFrame(&frontendv1.AsyncBubbleDelta{Workspace: "/ws"})
	if _, ok := scopeFrame(frame, Scope{Workspace: "/ws"}); !ok {
		t.Fatal("a fenced push routes by workspace, exactly as ConversationDelta does")
	}
}

func TestAnAsyncBubbleDeltaIsWithheldFromAnotherWorkspacesClient(t *testing.T) {
	frame := AsyncBubbleDeltaFrame(&frontendv1.AsyncBubbleDelta{Workspace: "/ws"})
	if _, ok := scopeFrame(frame, Scope{Workspace: "/other"}); ok {
		t.Fatal("without a case of its own the delta would fall to the connection-global default and leak across workspaces")
	}
}

func TestAScopedSnapshotKeepsItsAsyncBubbles(t *testing.T) {
	snap := &frontendv1.StateSnapshot{AsyncBubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1"}}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetAsyncBubbles()); got != 1 {
		t.Fatalf("a scoped client that lost its bubbles would reconnect with detached work missing, got %d", got)
	}
}
