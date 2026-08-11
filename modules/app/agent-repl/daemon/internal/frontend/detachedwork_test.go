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
func openKind(t *testing.T, kind DetachKind) *frontendv1.Message {
	t.Helper()
	spec := DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: kind, OriginToolUseID: "tu1", StartedAtMs: 5}
	if kind == DetachUnrecognized {
		spec.ToolName = "Frobnicate"
	}
	if kind == DetachSkill {
		spec.SkillName, spec.Args = "demo", "run it"
	}
	b, err := OpenDetachedWork(spec)
	if err != nil {
		t.Fatalf("OpenDetachedWork(%s): %v", kind, err)
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

func TestOpenDetachedWorkDerivesItsIdFromTheTaskId(t *testing.T) {
	first := openKind(t, DetachAgent)
	second := openKind(t, DetachAgent)
	if first.GetUuid() != second.GetUuid() {
		t.Fatalf("the same detachment must resolve to the same work across a replay, got %q then %q", first.GetUuid(), second.GetUuid())
	}
}

func TestOpenDetachedWorkNeverMintsABlankId(t *testing.T) {
	if id := openKind(t, DetachAgent).GetUuid(); id == "" {
		t.Fatal("a work id is the routing handle and is never empty")
	}
}

func TestOpenDetachedWorkRefusesADetachmentWithNoTaskId(t *testing.T) {
	_, err := OpenDetachedWork(DetachedWorkSpec{Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a detachment with no task id has nothing to mint an id from and must be refused")
	}
}

func TestOpenDetachedWorkRefusesADetachmentItCannotAttributeToACall(t *testing.T) {
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachAgent})
	if err == nil {
		t.Fatal("an unattributable detachment is a daemon fault, never a work with a blank origin")
	}
}

func TestOpenDetachedWorkAdmitsWorkNoToolCallSpawned(t *testing.T) {
	// Arrange / Act
	b, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, NoSpawningCall: true})

	// Assert
	if err != nil {
		t.Fatalf("OpenDetachedWork error = %v, want a work: the contract admits an empty origin_tool_use_id for work no tool call spawned", err)
	}
	if got := b.GetDetachedWork().GetOriginToolUseId(); got != "" {
		t.Fatalf("origin_tool_use_id = %q, want empty", got)
	}
}

func TestOpenDetachedWorkRefusesASpecThatIsBothSpawnedAndUnspawned(t *testing.T) {
	// Arrange / Act
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, OriginToolUseID: "tu1", NoSpawningCall: true})

	// Assert
	if err == nil {
		t.Fatal("a work cannot be both announcement-born and call-spawned; the contradiction must be refused rather than silently resolved one way")
	}
}

func TestOpenDetachedWorkRefusesAnUnresolvedKind(t *testing.T) {
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachUnresolved, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("a kindless work carries no body a renderer can draw and must be refused")
	}
}

func TestOpenDetachedWorkRefusesAnUnclassifiedSpawnThatNamesNoTool(t *testing.T) {
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachUnrecognized, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("the unclassified arm exists to NAME the tool it could not classify; an anonymous one must be refused")
	}
}

func TestOpenDetachedWorkCarriesTheToolNameOnTheUnclassifiedArm(t *testing.T) {
	if got := openKind(t, DetachUnrecognized).GetDetachedWork().GetUnclassified().GetToolName(); got != "Frobnicate" {
		t.Fatalf("want tool_name=%q, got %q", "Frobnicate", got)
	}
}

func TestOpenDetachedWorkCarriesTheCommandOnTheShellArm(t *testing.T) {
	b, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachShell, OriginToolUseID: "tu1", Command: "sleep 9"})
	if err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetShell().GetCommand(); got != "sleep 9" {
		t.Fatalf("want command=%q, got %q", "sleep 9", got)
	}
}

func TestOpenDetachedWorkOpensLive(t *testing.T) {
	if openKind(t, DetachAgent).GetDetachedWork().GetLiveness().GetLive() == nil {
		t.Fatal("a work opens live: the work has just been launched")
	}
}

func TestOpenDetachedWorkStatesTheTailCapOnAnItemCountedFold(t *testing.T) {
	if got := openKind(t, DetachAgent).GetDetachedWork().GetAgent().GetFold().GetTailCap(); got != StreamItemCap {
		t.Fatalf("the cap is a daemon fact stated on the fold: want %d, got %d", StreamItemCap, got)
	}
}

func TestOpenDetachedWorkCarriesTheParentPointerForANestedDispatch(t *testing.T) {
	b, err := OpenDetachedWork(DetachedWorkSpec{
		TaskID: "t2", Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu2",
		ParentMessageID: "detached-work:t1", ParentTopLevelMessageID: "detached-work:t1",
	})
	if err != nil {
		t.Fatal(err)
	}
	// AMENDED: containment moved off DetachedWork.parent_bubble_id — a second
	// tree walking only detached work — onto the ONE containment relation,
	// MessageLineage.parent_message_id.
	if got := b.GetLineage().GetParentMessageId(); got != "detached-work:t1" {
		t.Fatalf("want parent_message_id=%q, got %q", "detached-work:t1", got)
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

func TestAppendDetachedEmissionsProducesTheAgentArm(t *testing.T) {
	up, err := AppendDetachedEmissions(openKind(t, DetachAgent), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetAgent() == nil {
		t.Fatalf("an agent work's update must carry the agent arm, got %T", up.GetUpdate())
	}
}

func TestAppendDetachedEmissionsAddressesTheUpdateToItsDetachedWork(t *testing.T) {
	b := openKind(t, DetachAgent)
	up, err := AppendDetachedEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetMessageId() != b.GetUuid() {
		t.Fatalf("want message_id=%q, got %q", b.GetUuid(), up.GetMessageId())
	}
}

func TestAppendDetachedEmissionsFoldsIntoTheDetachedWorkItPushesFrom(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := AppendDetachedEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7); err != nil {
		t.Fatal(err)
	}
	if got := len(b.GetDetachedWork().GetAgent().GetEmissions()); got != 1 {
		t.Fatalf("the snapshot fold and the delta come from one call: want 1 folded emission, got %d", got)
	}
}

func TestAppendDetachedEmissionsRejectsAnAgentUpdateAddressedToAShellDetachedWork(t *testing.T) {
	_, err := AppendDetachedEmissions(openKind(t, DetachShell), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err == nil {
		t.Fatal("an update whose arm does not match the work's kind is a daemon bug and must be rejected, not coerced")
	}
}

func TestAppendDetachedEmissionsNamesBothKindsInItsRefusal(t *testing.T) {
	_, err := AppendDetachedEmissions(openKind(t, DetachShell), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err == nil || !strings.Contains(err.Error(), "shell") || !strings.Contains(err.Error(), "agent") {
		t.Fatalf("the refusal must name the disagreement, got %v", err)
	}
}

func TestAppendDetachedEmissionsProducesNoUpdateForAnEmptyBatch(t *testing.T) {
	up, err := AppendDetachedEmissions(openKind(t, DetachAgent), nil, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("an empty batch is not a push")
	}
}

func TestAppendDetachedEmissionsRecordsLastActivity(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := AppendDetachedEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 77); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetLiveness().GetLive().GetLastActivityMs(); got != 77 {
		t.Fatalf("want last_activity_ms=77, got %d", got)
	}
}

func TestAppendDetachedEmissionsKeepsTheTailAtTheCap(t *testing.T) {
	b := openKind(t, DetachAgent)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}
	if _, err := AppendDetachedEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}
	if got := len(b.GetDetachedWork().GetAgent().GetEmissions()); got != StreamItemCap {
		t.Fatalf("want the fold capped at %d, got %d", StreamItemCap, got)
	}
}

func TestAppendDetachedEmissionsReportsWhatTheCapDropped(t *testing.T) {
	b := openKind(t, DetachAgent)
	var ems []*frontendv1.AgentEmission
	for i := 0; i < StreamItemCap+5; i++ {
		ems = append(ems, responseEmission("m"))
	}
	if _, err := AppendDetachedEmissions(b, ems, 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetAgent().GetFold().GetDroppedBefore(); got != 5 {
		t.Fatalf("a capped fold that says nothing is indistinguishable from a complete one: want dropped_before=5, got %d", got)
	}
}

func TestAppendDetachedEmissionsDoesNotAliasTheDetachedWorkFoldOntoTheUpdate(t *testing.T) {
	b := openKind(t, DetachAgent)
	up, err := AppendDetachedEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetAgent().GetFold() == b.GetDetachedWork().GetAgent().GetFold() {
		t.Fatal("a queued frame's fold must not be rewritten by later folding")
	}
}

// --- merge fold ------------------------------------------------------------

func TestOpenDetachedWorkGivesAMergeRunTheMergeArm(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachMerge)

	// Assert
	if b.GetDetachedWork().GetMerge() == nil {
		t.Fatalf("a merge run opened on arm %T, want the merge arm", b.GetDetachedWork().GetKind())
	}
}

func TestDetachedWorkKindReadsTheMergeArmBack(t *testing.T) {
	// Arrange, Act, Assert
	if got := DetachedWorkKind(openKind(t, DetachMerge)); got != DetachMerge {
		t.Fatalf("DetachedWorkKind = %s, want merge: a work's kind must survive a round trip through its arm", got)
	}
}

// AMENDED: these tests pinned whole-work RE-DELIVERY, which was the interim
// shape a merge window advanced by while DetachedWorkUpdate carried no arm it
// could use. The contract's update oneof says of itself "Never a re-send of the
// whole work", and its `merge = 15` arm now exists precisely so a merge run's
// progress is an APPEND. So what these assert is the arm, not the copy.

func TestAppendWindowEmissionsDeliversAMergeFoldOnTheMergeArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert: AMENDED from "returns the whole work for re-delivery" — the
	// update oneof forbids re-sending the whole work, and the merge arm is
	// what the contract added to replace that route.
	if up.GetMerge() == nil {
		t.Fatalf("a merge fold arrived on arm %T, want the merge arm the contract added for it", up.GetUpdate())
	}
}

func TestAppendWindowEmissionsCarriesOnlyTheNewEmissions(t *testing.T) {
	// Arrange: a work that has already folded once.
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

func TestAppendWindowEmissionsAddressesTheUpdateToItsDetachedWork(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if got := up.GetMessageId(); got != b.GetUuid() {
		t.Fatalf("the update is addressed to %q, want the work %q it folded into: the id is the only routing handle it carries", got, b.GetUuid())
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
	if got := len(b.GetDetachedWork().GetMerge().GetEmissions()); got != 1 {
		t.Fatalf("the merge work folded %d emissions, want 1", got)
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
	if got := len(b.GetDetachedWork().GetMerge().GetEmissions()); got != StreamItemCap {
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
	if got := b.GetDetachedWork().GetMerge().GetFold().GetDroppedBefore(); got != 5 {
		t.Fatalf("a capped merge fold that says nothing is indistinguishable from a complete one: want dropped_before=5, got %d", got)
	}
}

func TestAppendWindowEmissionsDoesNotAliasTheDetachedWorkFoldOntoTheUpdate(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := AppendWindowEmissions(b, []*frontendv1.AgentEmission{responseEmission("m1")}, 7)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetMerge().GetFold() == b.GetDetachedWork().GetMerge().GetFold() {
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

func TestAppendWindowEmissionsRefusesANonWindowDetachedWork(t *testing.T) {
	// Arrange, Act
	_, err := AppendWindowEmissions(openKind(t, DetachAgent), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)

	// Assert
	if err == nil {
		t.Fatal("a merge fold addressed to an agent work is a daemon bug and must be refused rather than coerced")
	}
}

// --- skill fold ------------------------------------------------------------

func TestOpenDetachedWorkGivesASkillInvocationTheSkillArm(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if b.GetDetachedWork().GetSkill() == nil {
		t.Fatalf("a skill invocation opened on arm %T, want the skill arm", b.GetDetachedWork().GetKind())
	}
}

func TestOpenDetachedWorkCarriesTheSkillNameVerbatim(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if got := b.GetDetachedWork().GetSkill().GetSkillName(); got != "demo" {
		t.Fatalf("skill_name = %q, want the name as invoked, verbatim", got)
	}
}

func TestOpenDetachedWorkCarriesTheSkillArgsVerbatim(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert
	if got := b.GetDetachedWork().GetSkill().GetArgs(); got != "run it" {
		t.Fatalf("args = %q, want the invocation's arguments, verbatim", got)
	}
}

func TestOpenDetachedWorkOpensASkillBodyEmpty(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachSkill)

	// Assert: the contract says the body is empty until resolution delivers it.
	if got := b.GetDetachedWork().GetSkill().GetBody(); got != "" {
		t.Fatalf("a freshly opened skill work carried body %q, want it empty until resolution delivers one", got)
	}
}

func TestOpenDetachedWorkRefusesANamelessSkillInvocation(t *testing.T) {
	// Arrange, Act
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Workspace: "/ws", Kind: DetachSkill, OriginToolUseID: "tu1"})

	// Assert
	if err == nil {
		t.Fatal("a skill work that names no skill has nothing a reader could act on and must be refused rather than opened blank")
	}
}

func TestDetachedWorkKindReadsTheSkillArmBack(t *testing.T) {
	// Arrange, Act, Assert
	if got := DetachedWorkKind(openKind(t, DetachSkill)); got != DetachSkill {
		t.Fatalf("DetachedWorkKind = %s, want skill: a work's kind must survive a round trip through its arm", got)
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
	if got := len(b.GetDetachedWork().GetSkill().GetEmissions()); got != 1 {
		t.Fatalf("the skill work folded %d emissions, want 1", got)
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
	if got := len(b.GetDetachedWork().GetSkill().GetEmissions()); got != StreamItemCap {
		t.Fatalf("want the skill fold capped at %d, got %d", StreamItemCap, got)
	}
}

func TestResolveSkillBodyPutsTheContentsOnTheDetachedWork(t *testing.T) {
	// Arrange
	b := openKind(t, DetachSkill)

	// Act
	if _, err := ResolveSkillBody(b, "# Demo skill"); err != nil {
		t.Fatal(err)
	}

	// Assert: the snapshot the work IS must carry what the update said.
	if got := b.GetDetachedWork().GetSkill().GetBody(); got != "# Demo skill" {
		t.Fatalf("the work's body = %q, want the resolved contents verbatim", got)
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
	if got := b.GetDetachedWork().GetSkill().GetBody(); got != "# Demo skill" {
		t.Fatalf("the body = %q, want the file once: the arm replaces the body whole", got)
	}
}

func TestResolveSkillBodyRefusesANonSkillDetachedWork(t *testing.T) {
	// Arrange, Act
	_, err := ResolveSkillBody(openKind(t, DetachMerge), "# Demo skill")

	// Assert
	if err == nil {
		t.Fatal("only a skill work has a body to resolve, so a body addressed to a merge work is a daemon bug and must be refused rather than coerced")
	}
}

func TestAppendDetachedEmissionsRefusesAnAgentUpdateAddressedToASkillDetachedWork(t *testing.T) {
	// Arrange, Act
	_, err := AppendDetachedEmissions(openKind(t, DetachSkill), []*frontendv1.AgentEmission{responseEmission("s1")}, 7)

	// Assert
	if err == nil {
		t.Fatal("a skill work advances by its own arm, so an agent update aimed at one is the kind mismatch a receiver rejects and must never be produced")
	}
}

func TestAppendDetachedEmissionsRefusesAnAgentUpdateAddressedToAMergeDetachedWork(t *testing.T) {
	// Arrange, Act
	_, err := AppendDetachedEmissions(openKind(t, DetachMerge), []*frontendv1.AgentEmission{responseEmission("m1")}, 7)

	// Assert
	if err == nil {
		// AMENDED REASON: the merge arm now exists, so the old sentence ("there
		// is no merge arm") is no longer why this is refused. The refusal stands
		// on the arm rule the update oneof states — "The arm MUST match the
		// work's kind" — and `agent` and `merge` remain distinct kinds on the
		// wire even though they carry the same message.
		t.Fatal("an agent update aimed at a merge work is the kind mismatch a receiver rejects and must never be produced: the merge kind has an arm of its own")
	}
}

func TestSettleDetachedWorkSettlesAMergeRunThroughTheOneLivenessArm(t *testing.T) {
	// Arrange
	b := openKind(t, DetachMerge)

	// Act
	up, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	if up.GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatalf("a merge run settles through the kind-independent liveness arm; got %v", up.GetUpdate())
	}
}

// --- journal fold ----------------------------------------------------------

func TestAppendDetachedJournalRowsProducesTheJournalArm(t *testing.T) {
	up, err := AppendDetachedJournalRows(openKind(t, DetachWorkflow),
		[]*frontendv1.DetachedWorkJournalRow{{Label: "step"}}, 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetJournal() == nil {
		t.Fatalf("a workflow work's update must carry the journal arm, got %T", up.GetUpdate())
	}
}

func TestAppendDetachedJournalRowsRejectsAJournalUpdateAddressedToAShellDetachedWork(t *testing.T) {
	_, err := AppendDetachedJournalRows(openKind(t, DetachShell),
		[]*frontendv1.DetachedWorkJournalRow{{Label: "step"}}, 7)
	if err == nil {
		t.Fatal("a journal update addressed to a shell work is a daemon bug and must be rejected")
	}
}

func TestAppendDetachedJournalRowsKeepsTheTailAtTheCap(t *testing.T) {
	b := openKind(t, DetachWorkflow)
	var rows []*frontendv1.DetachedWorkJournalRow
	for i := 0; i < StreamItemCap+3; i++ {
		rows = append(rows, &frontendv1.DetachedWorkJournalRow{Label: "step"})
	}
	if _, err := AppendDetachedJournalRows(b, rows, 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetJournal().GetFold().GetDroppedBefore(); got != 3 {
		t.Fatalf("want dropped_before=3, got %d", got)
	}
}

// --- byte spools -----------------------------------------------------------

func TestAppendDetachedOutputProducesTheShellArmForAShellDetachedWork(t *testing.T) {
	up, err := AppendDetachedOutput(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell() == nil {
		t.Fatalf("a shell work's append must carry the shell arm, got %T", up.GetUpdate())
	}
}

func TestAppendDetachedOutputProducesTheUnclassifiedArmForAnUnclassifiedDetachedWork(t *testing.T) {
	up, err := AppendDetachedOutput(openKind(t, DetachUnrecognized), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetUnclassified() == nil {
		t.Fatalf("an unclassified work's append must carry the unclassified arm, got %T", up.GetUpdate())
	}
}

func TestAppendDetachedOutputStartsTheFirstAppendAtOffsetZero(t *testing.T) {
	up, err := AppendDetachedOutput(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 0 {
		t.Fatalf("want from_offset=0, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendDetachedOutputTakesFromOffsetFromTheSpoolsOwnCursor(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendDetachedOutput(b, "de", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 3 {
		t.Fatalf("the second append must start where the spool's cursor stood: want 3, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendDetachedOutputAdvancesTheSpoolCursorByTheAppendedBytes(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetShell().GetOutput().GetThroughOffset(); got != 3 {
		t.Fatalf("want through_offset=3, got %d", got)
	}
}

func TestAppendDetachedOutputFoldsTheBytesIntoTheSpool(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutput(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendDetachedOutput(b, "de", 8); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetShell().GetOutput().GetText(); got != "abcde" {
		t.Fatalf("want spool text %q, got %q", "abcde", got)
	}
}

func TestAppendDetachedOutputRejectsAByteAppendAddressedToAnAgentDetachedWork(t *testing.T) {
	if _, err := AppendDetachedOutput(openKind(t, DetachAgent), "abc", 7); err == nil {
		t.Fatal("an agent work has no byte spool; an append addressed to it must be rejected")
	}
}

func TestAppendDetachedOutputProducesNoUpdateForAnEmptyChunk(t *testing.T) {
	up, err := AppendDetachedOutput(openKind(t, DetachShell), "", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("a quiet read is not a push")
	}
}

func TestAppendDetachedOutputThroughStartsAFreshDetachedWorkAtOffsetZero(t *testing.T) {
	up, err := AppendDetachedOutputThrough(openKind(t, DetachShell), "abc", 7)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 0 {
		t.Fatalf("a newly opened work carries an empty body, so the first append starts at 0, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendDetachedOutputThroughAppendsOnlyWhatIsPastTheCursor(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutputThrough(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendDetachedOutputThrough(b, "abcde", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetText() != "de" {
		t.Fatalf("a restated snapshot must append only its new bytes, got %q", up.GetShell().GetText())
	}
}

func TestAppendDetachedOutputThroughResumesFromASnapshotsThroughOffset(t *testing.T) {
	// A work redelivered in a snapshot arrives with its spool cursor already
	// advanced; the next append must continue from there, not from zero.
	b := openKind(t, DetachShell)
	b.GetDetachedWork().GetShell().GetOutput().Text = "abc"
	b.GetDetachedWork().GetShell().GetOutput().ThroughOffset = 3
	up, err := AppendDetachedOutputThrough(b, "abcde", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up.GetShell().GetFromOffset() != 3 {
		t.Fatalf("want from_offset=3, got %d", up.GetShell().GetFromOffset())
	}
}

func TestAppendDetachedOutputThroughProducesNoUpdateForAnUnchangedSnapshot(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutputThrough(b, "abc", 7); err != nil {
		t.Fatal(err)
	}
	up, err := AppendDetachedOutputThrough(b, "abc", 8)
	if err != nil {
		t.Fatal(err)
	}
	if up != nil {
		t.Fatal("a retrieval that restates what the spool already holds is not a push")
	}
}

func TestAppendDetachedOutputThroughRefusesASourceThatRewound(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := AppendDetachedOutputThrough(b, "abcdef", 7); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendDetachedOutputThrough(b, "ab", 8); err == nil {
		t.Fatal("a snapshot shorter than the cursor is a gap, and re-appending from zero would duplicate what the client holds")
	}
}

// --- settlement ------------------------------------------------------------

func TestSettleDetachedWorkResolvesDoneFromAZeroExitCode(t *testing.T) {
	b := openKind(t, DetachShell)
	up, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(0), AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}
	if up.GetLiveness().GetLiveness().GetSettled().GetDone() == nil {
		t.Fatal("exit code 0 is a real zero and always means clean exit")
	}
}

func TestSettleDetachedWorkResolvesErrorFromANonzeroExitCode(t *testing.T) {
	b := openKind(t, DetachShell)
	up, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(2), AtMs: 9})
	if err != nil {
		t.Fatal(err)
	}
	if up.GetLiveness().GetLiveness().GetSettled().GetError() == nil {
		t.Fatal("a nonzero exit code resolves the error outcome, whatever the shim's status word said")
	}
}

func TestSettleDetachedWorkKeepsTheExitCodeBesideTheOutcome(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_KILLED, ExitCode: int32p(137), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetLiveness().GetSettled().GetShellExit().GetCode(); got != 137 {
		t.Fatalf("a killed process still carries its exit status: want 137, got %d", got)
	}
}

func TestSettleDetachedWorkReadsAKillAsKilledDespiteItsNonzeroExit(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_KILLED, ExitCode: int32p(137), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetDetachedWork().GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatal("work stopped from outside did not fail, and must not be reported to the user as an error")
	}
}

func TestSettleDetachedWorkLeavesShellExitAbsentForWorkThatIsNotAProcess(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetDetachedWork().GetLiveness().GetSettled().GetShellExit() != nil {
		t.Fatal("an agent concluded, it did not exit; a fabricated exit status would be unreadable")
	}
}

func TestSettleDetachedWorkReadsALostTaskAsKilledRatherThanDone(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_LOST, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if b.GetDetachedWork().GetLiveness().GetSettled().GetKilled() == nil {
		t.Fatal("a lost task stopped, but nothing says it succeeded")
	}
}

func TestSettleDetachedWorkRefusesAnUnspecifiedStatusWithNoExitCode(t *testing.T) {
	_, err := SettleDetachedWork(openKind(t, DetachAgent), DetachedVerdict{AtMs: 9})
	if err == nil {
		t.Fatal("a settled work with no outcome is unrepresentable and must be refused, never stood in for")
	}
}

func TestSettleDetachedWorkCarriesTheFailureMessageWithoutManufacturingOne(t *testing.T) {
	b := openKind(t, DetachAgent)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_ERROR, AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if got := b.GetDetachedWork().GetLiveness().GetSettled().GetError().GetMessage(); got != "" {
		t.Fatalf("a source that reported failure without a reason gets no manufactured one, got %q", got)
	}
}

func TestSettleDetachedWorkStopsRecordingActivityOnceSettled(t *testing.T) {
	b := openKind(t, DetachShell)
	if _, err := SettleDetachedWork(b, DetachedVerdict{Status: corev1.TerminalStatus_TERMINAL_STATUS_DONE, ExitCode: int32p(0), AtMs: 9}); err != nil {
		t.Fatal(err)
	}
	if _, err := AppendDetachedOutput(b, "late", 99); err != nil {
		t.Fatal(err)
	}
	if b.GetDetachedWork().GetLiveness().GetLive() != nil {
		t.Fatal("a late append must not resurrect a settled work's live arm")
	}
}

// --- classification verdict on the tool card -------------------------------

func toolCallItem(toolUseID string) *frontendv1.Message {
	return &frontendv1.Message{Payload: &frontendv1.Message_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_ToolCall{
			ToolCall: &frontendv1.AgentToolCall{Call: &datav1.ToolUseBlock{Id: toolUseID}},
		}},
	}}
}

func toolOutcomeItem(toolUseID string) *frontendv1.Message {
	return &frontendv1.Message{Payload: &frontendv1.Message_Agent{
		Agent: &frontendv1.AgentEmission{Emission: &frontendv1.AgentEmission_ToolOutcome{
			ToolOutcome: &frontendv1.AgentToolOutcome{ToolUseId: toolUseID},
		}},
	}}
}

func TestStampSpawnedMessageIDsStampsTheCall(t *testing.T) {
	item := toolCallItem("tu1")
	StampSpawnedMessageIDs([]*frontendv1.Message{item},
		func(string) string { return "work:t1" })
	if got := item.GetAgent().GetToolCall().GetSpawnedMessageId(); got != "work:t1" {
		t.Fatalf("want the call stamped with the work id, got %q", got)
	}
}

func TestStampSpawnedMessageIDsStampsTheOutcomeWithTheSameString(t *testing.T) {
	call, outcome := toolCallItem("tu1"), toolOutcomeItem("tu1")
	StampSpawnedMessageIDs([]*frontendv1.Message{call, outcome},
		func(string) string { return "work:t1" })
	if call.GetAgent().GetToolCall().GetSpawnedMessageId() != outcome.GetAgent().GetToolOutcome().GetSpawnedMessageId() {
		t.Fatal("the daemon resolves the id once and stamps the same string on both")
	}
}

func TestStampSpawnedMessageIDsLeavesACallThatDetachedNothingEmpty(t *testing.T) {
	item := toolCallItem("tu1")
	StampSpawnedMessageIDs([]*frontendv1.Message{item}, func(string) string { return "" })
	if got := item.GetAgent().GetToolCall().GetSpawnedMessageId(); got != "" {
		t.Fatalf("empty means 'this call detached nothing' and is the only reading of empty, got %q", got)
	}
}

// --- frame plumbing --------------------------------------------------------

func TestDetachedWorkDeltaFrameWrapsTheDeltaInItsArm(t *testing.T) {
	d := &frontendv1.DetachedWorkDelta{Workspace: "/ws"}
	if got := DetachedWorkDeltaFrame(d).GetDetachedWorkDelta(); got != d {
		t.Fatalf("want the delta on frame arm 20, got %v", got)
	}
}

func TestADetachedWorkDeltaRoutesToItsOwnWorkspace(t *testing.T) {
	frame := DetachedWorkDeltaFrame(&frontendv1.DetachedWorkDelta{Workspace: "/ws"})
	if _, ok := scopeFrame(frame, Scope{Workspace: "/ws"}); !ok {
		t.Fatal("a fenced push routes by workspace, exactly as ConversationDelta does")
	}
}

func TestADetachedWorkDeltaIsWithheldFromAnotherWorkspacesClient(t *testing.T) {
	frame := DetachedWorkDeltaFrame(&frontendv1.DetachedWorkDelta{Workspace: "/ws"})
	if _, ok := scopeFrame(frame, Scope{Workspace: "/other"}); ok {
		t.Fatal("without a case of its own the delta would fall to the connection-global default and leak across workspaces")
	}
}

// detachedWorkMessage is a snapshot-shaped detached-work MESSAGE: the uuid is
// the work's identity now that DetachedWork carries no id of its own, and the
// workspace stays on the payload where filterDetachedWork reads it.
func detachedWorkMessage(uuid, workspace string) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid:    uuid,
		Lineage: FeedRowLineage(uuid),
		Payload: &frontendv1.Message_DetachedWork{DetachedWork: &frontendv1.DetachedWork{Workspace: workspace}},
	}
}

func TestAScopedSnapshotKeepsItsOwnWorkspacesDetachedWork(t *testing.T) {
	snap := &frontendv1.StateSnapshot{DetachedWork: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/ws")}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetDetachedWork()); got != 1 {
		t.Fatalf("a scoped client that lost its work would reconnect with detached work missing, got %d", got)
	}
}

func TestAScopedSnapshotDropsAnotherWorkspacesDetachedWork(t *testing.T) {
	snap := &frontendv1.StateSnapshot{DetachedWork: []*frontendv1.Message{detachedWorkMessage("detached-work:t1", "/other")}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetDetachedWork()); got != 0 {
		t.Fatalf("a work belonging to another workspace must not reach this client, got %d", got)
	}
}

func TestAScopedSnapshotKeepsOnlyTheClientsWorkspaceAmongSeveral(t *testing.T) {
	snap := &frontendv1.StateSnapshot{DetachedWork: []*frontendv1.Message{
		detachedWorkMessage("detached-work:a", "/ws"),
		detachedWorkMessage("detached-work:b", "/other"),
		detachedWorkMessage("detached-work:c", "/ws"),
	}}
	got := filterSnapshot(snap, Scope{Workspace: "/ws"}).GetDetachedWork()
	if len(got) != 2 || got[0].GetUuid() != "detached-work:a" || got[1].GetUuid() != "detached-work:c" {
		t.Fatalf("want only /ws's work in order, got %v", got)
	}
}

// A message that is NOT detached work has no workspace to scope by, so it is
// dropped rather than delivered to every client. This is the refusal
// filterDetachedWork's comment names as a daemon bug, and it must stay covered.
func TestAScopedSnapshotDropsAMessageThatIsNotDetachedWork(t *testing.T) {
	snap := &frontendv1.StateSnapshot{DetachedWork: []*frontendv1.Message{
		{Uuid: "m1", Lineage: FeedRowLineage("m1"), Payload: &frontendv1.Message_Agent{Agent: responseEmission("m1")}},
	}}
	if got := len(filterSnapshot(snap, Scope{Workspace: "/ws"}).GetDetachedWork()); got != 0 {
		t.Fatalf("a snapshot entry with no detached-work payload carries no routing key and must be dropped, got %d", got)
	}
}

func TestADetachedWorkCarriesTheWorkspaceItWasOpenedFor(t *testing.T) {
	if got := openKind(t, DetachAgent).GetDetachedWork().GetWorkspace(); got != "/ws" {
		t.Fatalf("want workspace=%q, got %q", "/ws", got)
	}
}

func TestOpenDetachedWorkRefusesADetachedWorkThatNamesNoWorkspace(t *testing.T) {
	_, err := OpenDetachedWork(DetachedWorkSpec{TaskID: "t1", Kind: DetachAgent, OriginToolUseID: "tu1"})
	if err == nil {
		t.Fatal("the workspace is the only routing key a snapshot has, and a work without one reaches every scoped client")
	}
}

// --- lineage ---------------------------------------------------------------
//
// NEW with MessageLineage. Lineage is set at exactly two constructors, and
// these pin what each writes: FeedRowLineage for a message sitting directly in
// the feed, and OpenDetachedWork for a message that IS detached work. The audit
// in lineage.go checks the same invariant on the way out; this is where it is
// MADE.

func TestFeedRowLineageNamesTheMessageAsItsOwnRoot(t *testing.T) {
	// Arrange, Act
	got := FeedRowLineage("m1")

	// Assert: the contract says top_level_message_id is self-referential on a
	// feed row, so a page query selecting by it needs no null case and no walk.
	if got.GetTopLevelMessageId() != "m1" {
		t.Fatalf("top_level_message_id = %q, want the message's own uuid %q", got.GetTopLevelMessageId(), "m1")
	}
}

func TestFeedRowLineageNamesNoParent(t *testing.T) {
	// Arrange, Act
	got := FeedRowLineage("m1")

	// Assert: absence IS the fact — an empty parent means the message sits
	// directly in the feed, never that a parent went unresolved.
	if got.GetParentMessageId() != "" {
		t.Fatalf("parent_message_id = %q, want empty: a feed row is contained by the feed itself", got.GetParentMessageId())
	}
}

func TestOpenDetachedWorkMakesUncontainedWorkItsOwnRoot(t *testing.T) {
	// Arrange, Act
	b := openKind(t, DetachAgent)

	// Assert: RULING 5 — detached work with no parent IS a feed row.
	if got := b.GetLineage().GetTopLevelMessageId(); got != b.GetUuid() {
		t.Fatalf("top_level_message_id = %q, want the work's own uuid %q", got, b.GetUuid())
	}
}

func TestOpenDetachedWorkCopiesTheParentsRootDown(t *testing.T) {
	// Arrange, Act: work contained by work that is itself contained.
	b, err := OpenDetachedWork(DetachedWorkSpec{
		TaskID: "t3", Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu3",
		ParentMessageID: "detached-work:t2", ParentTopLevelMessageID: "detached-work:t1",
	})
	if err != nil {
		t.Fatal(err)
	}

	// Assert: the root is COPIED, never walked — that walk is the unbounded
	// traversal the denormalized field exists to remove.
	if got := b.GetLineage().GetTopLevelMessageId(); got != "detached-work:t1" {
		t.Fatalf("top_level_message_id = %q, want the parent's root %q rather than the parent itself", got, "detached-work:t1")
	}
}

func TestOpenDetachedWorkRefusesARootWithNoParent(t *testing.T) {
	// Arrange, Act
	_, err := OpenDetachedWork(DetachedWorkSpec{
		TaskID: "t2", Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu2",
		ParentTopLevelMessageID: "detached-work:t1",
	})

	// Assert
	if err == nil {
		t.Fatal("a message with no parent IS its own top-level row, so naming a different root asserts containment nobody stated and must be refused")
	}
}

func TestOpenDetachedWorkRefusesAParentWithNoRoot(t *testing.T) {
	// Arrange, Act
	_, err := OpenDetachedWork(DetachedWorkSpec{
		TaskID: "t2", Workspace: "/ws", Kind: DetachAgent, OriginToolUseID: "tu2",
		ParentMessageID: "detached-work:t1",
	})

	// Assert
	if err == nil {
		t.Fatal("a parent with no root would leave top_level_message_id to be derived by a walk, which is the unbounded traversal lineage exists to remove, and must be refused")
	}
}
