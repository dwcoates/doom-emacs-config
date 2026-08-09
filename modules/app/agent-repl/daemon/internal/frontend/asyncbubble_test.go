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
	spec := BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: kind, OriginToolUseID: "tu1", StartedAtMs: 5}
	if kind == DetachUnrecognized {
		spec.ToolName = "Frobnicate"
	}
	if kind == DetachSkill {
		spec.SkillName, spec.Args = "demo", "run it"
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
	_, err := OpenAsyncBubble(BubbleSpec{Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a detachment with no task id has nothing to mint an id from and must be refused")
	}
}

func TestOpenAsyncBubbleRefusesADetachmentItCannotAttributeToACall(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachAgent})
	if err == nil {
		t.Fatal("an unattributable detachment is a daemon fault, never a bubble with a blank origin")
	}
}

func TestOpenAsyncBubbleAdmitsWorkNoToolCallSpawned(t *testing.T) {
	// Arrange / Act
	b, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, NoSpawningCall: true})

	// Assert
	if err != nil {
		t.Fatalf("OpenAsyncBubble error = %v, want a bubble: the contract admits an empty origin_tool_use_id for work no tool call spawned", err)
	}
	if got := b.GetOriginToolUseId(); got != "" {
		t.Fatalf("origin_tool_use_id = %q, want empty", got)
	}
}

func TestOpenAsyncBubbleRefusesASpecThatIsBothSpawnedAndUnspawned(t *testing.T) {
	// Arrange / Act
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, OriginToolUseID: "tu1", NoSpawningCall: true})

	// Assert
	if err == nil {
		t.Fatal("a bubble cannot be both announcement-born and call-spawned; the contradiction must be refused rather than silently resolved one way")
	}
}

func TestOpenAsyncBubbleRefusesAnUnresolvedKind(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachUnresolved, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a kindless bubble carries no body a renderer can draw and must be refused")
	}
}

func TestOpenAsyncBubbleRefusesAnUnclassifiedSpawnThatNamesNoTool(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachUnrecognized, OriginToolUseID: "tu1"})
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
	b, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, OriginToolUseID: "tu1", Command: "sleep 9"})
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
	b, err := OpenAsyncBubble(BubbleSpec{TaskID: "t2", Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu2", ParentBubbleID: "bubble:t1"})
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

// --- merge fold ------------------------------------------------------------

func TestOpenAsyncBubbleGivesAMergeRunTheMergeArm(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachMerge)

	// Assert
	if b.GetMerge() == nil {
		t.Fatalf("a merge run opened on arm %T, want the merge arm", b.GetKind())
	}
}

func TestAsyncBubbleKindReadsTheMergeArmBack(t *testing.T) {
	// Arrange, Act, Assert
	if got := AsyncBubbleKind(openKind(t, DetachMerge)); got != DetachMerge {
		t.Fatalf("AsyncBubbleKind = %s, want merge: a bubble's kind must survive a round trip through its arm", got)
	}
}

// AMENDED: these tests pinned whole-bubble RE-DELIVERY, which was the interim
// shape a merge window advanced by while AsyncBubbleUpdate carried no arm it
// could use. The contract's update oneof says of itself "Never a re-send of the
// whole bubble", and its `merge = 15` arm now exists precisely so a merge run's
// progress is an APPEND. So what these assert is the arm, not the copy.

func TestAppendWindowEmissionsDeliversAMergeFoldOnTheMergeArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert: AMENDED from "returns the whole bubble for re-delivery" — the
	// update oneof forbids re-sending the whole bubble, and the merge arm is
	// what the contract added to replace that route.
	if up.GetMerge() == nil {
		t.Fatalf("a merge fold arrived on arm %T, want the merge arm the contract added for it", up.GetUpdate())
	}
}

func TestAppendWindowEmissionsCarriesOnlyTheNewEmissions(t *testing.T) {
	// Arrange: a bubble that has already folded once.
	b := openKind(t, DetachMerge)
	if _, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7); err != nil {
		t.Fatal(err)
	}

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m2")}, 8)
	if err != nil {
		t.Fatal(err)
	}

	// Assert: AMENDED — the whole point of the arm is that a window running for
	// an hour does not re-transmit its transcript on every new line.
	if got := len(up.GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the update carried %d emissions, want only the one appended: an update is a delta, never the fold to date", got)
	}
}

func TestAppendWindowEmissionsAddressesTheUpdateToItsBubble(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := up.GetBubbleId(); got != b.GetId() {
		t.Fatalf("the update is addressed to %q, want the bubble %q it folded into: the id is the only routing handle it carries", got, b.GetId())
	}
}

func TestAppendWindowEmissionsFoldsIntoTheMergeArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	if _, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the merge bubble folded %d emissions, want 1", got)
	}
}

func TestAppendWindowEmissionsKeepsTheTailAtTheCap(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}

	// Act
	if _, err := AppendWindowEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetMerge().GetEmissions()); got != StreamItemCap {
		t.Fatalf("want the merge fold capped at %d, got %d", StreamItemCap, got)
	}
}

func TestAppendWindowEmissionsReportsWhatTheCapDropped(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}

	// Act
	if _, err := AppendWindowEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := b.GetMerge().GetFold().GetDroppedBefore(); got != 5 {
		t.Fatalf("a capped merge fold that says nothing is indistinguishable from a complete one: want dropped_before=5, got %d", got)
	}
}

func TestAppendWindowEmissionsDoesNotAliasTheBubblesFoldOntoTheUpdate(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetMerge().GetFold() == b.GetMerge().GetFold() {
		t.Fatal("a queued frame's fold must not be rewritten by later folding")
	}
}

func TestAppendWindowEmissionsProducesNothingForAnEmptyBatch(t *testing.T) {
	// Arrange, Act
	got, err := AppendWindowEmissions(openKind(t, DetachMerge), nil, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got != nil {
		t.Fatal("an empty batch changed nothing and must produce no wire traffic")
	}
}

func TestAppendWindowEmissionsRefusesANonWindowBubble(t *testing.T) {
	// Arrange, Act
	_, err := AppendWindowEmissions(openKind(t, DetachAgent), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)

	// Assert
	if err == nil {
		t.Fatal("a merge fold addressed to an agent bubble is a daemon bug and must be refused rather than coerced")
	}
}

// --- skill fold ------------------------------------------------------------

func TestOpenAsyncBubbleGivesASkillInvocationTheSkillArm(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if b.GetSkill() == nil {
		t.Fatalf("a skill invocation opened on arm %T, want the skill arm", b.GetKind())
	}
}

func TestOpenAsyncBubbleCarriesTheSkillNameVerbatim(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if got := b.GetSkill().GetSkillName(); got != "demo" {
		t.Fatalf("skill_name = %q, want the name as invoked, verbatim", got)
	}
}

func TestOpenAsyncBubbleCarriesTheSkillArgsVerbatim(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if got := b.GetSkill().GetArgs(); got != "run it" {
		t.Fatalf("args = %q, want the invocation's arguments, verbatim", got)
	}
}

func TestOpenAsyncBubbleOpensASkillBodyEmpty(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert: the contract says the body is empty until resolution delivers it.
	if got := b.GetSkill().GetBody(); got != "" {
		t.Fatalf("a freshly opened skill bubble carried body %q, want it empty until resolution delivers one", got)
	}
}

func TestOpenAsyncBubbleRefusesANamelessSkillInvocation(t *testing.T) {
	// Arrange, Act
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachSkill, OriginToolUseID: "tu1"})

	// Assert
	if err == nil {
		t.Fatal("a skill bubble that names no skill has nothing a reader could act on and must be refused rather than opened blank")
	}
}

func TestAsyncBubbleKindReadsTheSkillArmBack(t *testing.T) {
	// Arrange, Act, Assert
	if got := AsyncBubbleKind(openKind(t, DetachSkill)); got != DetachSkill {
		t.Fatalf("AsyncBubbleKind = %s, want skill: a bubble's kind must survive a round trip through its arm", got)
	}
}

func TestAppendWindowEmissionsDeliversASkillFoldOnTheSkillArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("s1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetSkill().GetEmissions() == nil {
		t.Fatalf("a skill fold arrived on arm %T, want the skill arm's emissions", up.GetUpdate())
	}
}

func TestAppendWindowEmissionsFoldsIntoTheSkillArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)

	// Act
	if _, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("s1")}, 7); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetSkill().GetEmissions()); got != 1 {
		t.Fatalf("the skill bubble folded %d emissions, want 1", got)
	}
}

func TestAppendWindowEmissionsKeepsTheSkillTailAtTheCap(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("s"))
	}

	// Act
	if _, err := AppendWindowEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := len(b.GetSkill().GetEmissions()); got != StreamItemCap {
		t.Fatalf("want the skill fold capped at %d, got %d", StreamItemCap, got)
	}
}

func TestResolveSkillBodyPutsTheContentsOnTheBubble(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)

	// Act
	if _, err := ResolveSkillBody(b, "# Demo skill"); err != nil {
		t.Fatal(err)
	}

	// Assert: the snapshot the bubble IS must carry what the update said.
	if got := b.GetSkill().GetBody(); got != "# Demo skill" {
		t.Fatalf("the bubble's body = %q, want the resolved contents verbatim", got)
	}
}

func TestResolveSkillBodyDeliversTheContentsOnTheBodyArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)

	// Act
	up, err := ResolveSkillBody(b, "# Demo skill")
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := up.GetSkill().GetBody().GetContents(); got != "# Demo skill" {
		t.Fatalf("the body update carried %q, want the resolved contents verbatim", got)
	}
}

func TestResolveSkillBodyReplacesRatherThanAccumulates(t *testing.T) {
	// Arrange: a replayed resolution delivers the same file a second time.
	b := openKind(t, DetachSkill)
	if _, err := ResolveSkillBody(b, "# Demo skill"); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := ResolveSkillBody(b, "# Demo skill"); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := b.GetSkill().GetBody(); got != "# Demo skill" {
		t.Fatalf("the body = %q, want the file once: the arm replaces the body whole", got)
	}
}

func TestResolveSkillBodyRefusesANonSkillBubble(t *testing.T) {
	// Arrange, Act
	_, err := ResolveSkillBody(openKind(t, DetachMerge), "# Demo skill")

	// Assert
	if err == nil {
		t.Fatal("only a skill bubble has a body to resolve, so a body addressed to a merge bubble is a daemon bug and must be refused rather than coerced")
	}
}

func TestAppendAsyncEmissionsRefusesAnAgentUpdateAddressedToASkillBubble(t *testing.T) {
	// Arrange, Act
	_, err := AppendAsyncEmissions(openKind(t, DetachSkill), []*frontendv1.AgentEmission{responseEmission("s1")}, 7)

	// Assert
	if err == nil {
		t.Fatal("a skill bubble advances by its own arm, so an agent update aimed at one is the kind mismatch a receiver rejects and must never be produced")
	}
}

func TestAppendAsyncEmissionsRefusesAnAgentUpdateAddressedToAMergeBubble(t *testing.T) {
	// Arrange, Act
	_, err := AppendAsyncEmissions(openKind(t, DetachMerge), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)

	// Assert
	if err == nil {
		// AMENDED REASON: the merge arm now exists, so the old sentence ("there
		// is no merge arm") is no longer why this is refused. The refusal stands
		// on the arm rule the update oneof states — "The arm MUST match the
		// bubble's kind" — and `agent` and `merge` remain distinct kinds on the
		// wire even though they carry the same message.
		t.Fatal("an agent update aimed at a merge bubble is the kind mismatch a receiver rejects and must never be produced: the merge kind has an arm of its own")
	}
}

func TestSettleAsyncBubbleSettlesAMergeRunThroughTheOneLivenessArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := SettleAsyncBubble(b, AsyncVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatalf("a merge run settles through the kind-independent liveness arm; got %v", up.GetUpdate())
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

func TestAScopedSnapshotKeepsItsOwnWorkspacesBubbles(t *testing.T) {
	snap := &frontendv1.StateSnapshot{AsyncBubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/ws"}}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetAsyncBubbles()); got != 1 {
		t.Fatalf("a scoped client that lost its bubbles would reconnect with detached work missing, got %d", got)
	}
}

func TestAScopedSnapshotDropsAnotherWorkspacesBubbles(t *testing.T) {
	snap := &frontendv1.StateSnapshot{AsyncBubbles: []*frontendv1.AsyncBubble{{Id: "bubble:t1", Workspace: "/other"}}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetAsyncBubbles()); got != 0 {
		t.Fatalf("a bubble belonging to another workspace must not reach this client, got %d", got)
	}
}

func TestAScopedSnapshotKeepsOnlyTheClientsWorkspaceAmongSeveral(t *testing.T) {
	snap := &frontendv1.StateSnapshot{AsyncBubbles: []*frontendv1.AsyncBubble{
		{Id: "bubble:a", Workspace: "/ws"},
		{Id: "bubble:b", Workspace: "/other"},
		{Id: "bubble:c", Workspace: "/ws"},
	}}
	got := filterSnapshot(snap, Scope{Workspace: "/ws"}).GetAsyncBubbles()
	if len(got) != 2 || got[0].GetId() != "bubble:a" || got[1].GetId() != "bubble:c" {
		t.Fatalf("want only /ws's bubbles in order, got %v", got)
	}
}

func TestABubbleCarriesTheWorkspaceItWasOpenedFor(t *testing.T) {
	if got := openKind(t, DetachAgent).GetWorkspace(); got != "/ws" {
		t.Fatalf("want workspace=%q, got %q", "/ws", got)
	}
}

func TestOpenAsyncBubbleRefusesABubbleThatNamesNoWorkspace(t *testing.T) {
	_, err := OpenAsyncBubble(BubbleSpec{TaskID: "t1", Kind: DetachAgent, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("the workspace is the only routing key a snapshot has, and a bubble without one reaches every scoped client")
	}
}
