// skills_e2e_test.go — SPEC.md section C, "Skills" (#64-65), against the
// `skill-invocation` capture golden (agent-shim/claude/shim/testdata/captures/
// MANIFEST.md: "hook, thinking, skillUse, read, response -> success.completed",
// GROUNDED). Contract points cited per-test below.
//
// The `!skill [skill-name] [args]` parameterization (this file's whole
// reason to exist) is itself GROUNDED per the manifest's own entry:
//
//	"`!skill [skill-name] [args]` parameterization
//	(src/fake/scenarios/skills.ts). GROUNDED: skill-invocation (this
//	manifest, above) remains the golden for the SHAPE (tool_use ->
//	{success, commandName, allowedTools} ack -> isMeta document) --
//	unchanged. Only the fixed "fake-skill" name/args/document body are
//	now derived from the prompt's own argument (first token = skill
//	name, rest = args; the document body is templated on the name), so
//	a caller can name e.g. `create-or-update-workspace` with args
//	`merge`. Retires mergewindow_e2e_test.go's mergeSkillCallLine
//	fabrication."
//
// This file drives that real parameterization end to end instead of
// fabricating a skill body: every distinctive document string an assertion
// below checks for is produced by the REAL shim's `skillDocument` template
// (agent-shim/claude/shim/src/fake/scenarios/skills.ts), reproduced here only
// as the expected-value literal a Go assertion needs, never written to any
// transcript or store by this test itself (the grep gate in main_test.go
// would refuse that shape outright).
//
// The document's exact carriage as markdown, byte for byte, is grounded in
// the wire contract itself, not inferred from reading the converter:
// proto/src/conversation/v1/agent_activity.proto's AgentSkillDocument comment
// states plainly "A skill IS a markdown document, so it is carried and
// rendered as one rather than parsed into fields" -- i.e. verbatim, not a
// derived or reformatted string.
package e2e

import (
	"fmt"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// expectedSkillDocument reproduces, byte for byte, the DOCUMENT body the real
// shim's fake vendor writes for a given skill name
// (agent-shim/claude/shim/src/fake/scenarios/skills.ts's own skillDocument
// function, reproduced here as a Go literal template so this test asserts
// against the real fake-SDK's own distinctive body rather than a fabricated
// one).
func expectedSkillDocument(skill string) string {
	return fmt.Sprintf(
		"Base directory for this skill: /w/s/.claude/skills/%s\n\n# %s\n\nThe offline skill's instructions, as the vendor injects them.",
		skill, skill,
	)
}

// awaitSkillLoadedRow drives the daemon's real feed until the given turn's
// row settles into FeedSkill's loaded arm (SPEC.md §C #64-65; daemon.md
// "SKILL CARD (windows are dead)": "a Skill invocation's bubble is populated
// from exactly the TWO shim messages -- the invocation and the skill document
// content; NO temporal window folds subsequent responses under it"). Checks
// the turn's already-open page first (driveDocumentedPrompt already awaited
// the turn's terminal, so the loaded row is ordinarily already present), then
// falls back to the live watch stream, bounded by DefaultTimeout, purely as a
// defensive fallback -- never a sleep.
func awaitSkillLoadedRow(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, turn *conversationv1.TurnId) *frontendv1.FeedRow {
	t.Helper()
	opened, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("OpenFeed: %v", err)
	}
	success := opened.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed = %v, want success", opened.Msg)
	}
	isSkillLoaded := func(row *frontendv1.FeedRow) bool {
		return row.GetTurn().GetValue() == turn.GetValue() && row.GetActivity().GetSkill().GetLoaded() != nil
	}
	for _, row := range success.GetPage().GetSuccess().GetRows() {
		if isSkillLoaded(row) {
			return row
		}
	}
	stream := w.WatchFeedOn(w.Client(), success.GetWatch())
	defer stream.Close()
	return harness.AwaitView(t, w.Ctx(), stream, "the loaded skill card for turn "+turn.GetValue(), isSkillLoaded)
}

// skillScenarioCase is one skill-invocation prompt and the skill/args it must
// parameterize, per SPEC.md §D row 56 (skill-invocation) mapped onto
// SPEC.md §C #64-65 -- #64 drives the golden's own bare form (defaulting to
// "fake-skill"), #65 drives the landed name/args parameterization for real.
type skillScenarioCase struct {
	name      string // Go subtest name, matching SPEC.md's own test name.
	prompt    string // the exact "!skill ..." prompt submitted.
	wantSkill string // the skill name the fake's skillInvocationOf derives.
}

func TestSkillInvocation(t *testing.T) {
	// #64 SkillInvocation -- `skill-invocation` -- tool_use -> ack -> isMeta
	// document triple, driven bare ("!skill" with no name/args), which the
	// fake's skillInvocationOf resolves to its documented default,
	// "fake-skill" (skills.ts: `if (args === "") return { skill: "fake-skill",
	// skillArgs: "--target one" }`).
	tc := skillScenarioCase{
		name:      "SkillInvocation",
		prompt:    "!skill",
		wantSkill: "fake-skill",
	}
	t.Run(tc.name, func(t *testing.T) { runSkillScenarioCase(t, tc) })
}

func TestSkillNamedAndArgsParameterized(t *testing.T) {
	// #65 SkillNamedAndArgsParameterized -- `!skill [skill-name] [args]`
	// (landed addition; E2E-EVENT-INVENTORY.md remediation item 5) -- an
	// ARBITRARY skill name/args/document body, replacing the fixed
	// "fake-skill". SPEC.md's own example is driven verbatim:
	// "create-or-update-workspace merge".
	tc := skillScenarioCase{
		name:      "SkillNamedAndArgsParameterized",
		prompt:    "!skill create-or-update-workspace merge",
		wantSkill: "create-or-update-workspace",
	}
	t.Run(tc.name, func(t *testing.T) { runSkillScenarioCase(t, tc) })
}

// runSkillScenarioCase is the shared AAA body for both skill tests: they
// differ only in which prompt is submitted and which skill name the fake
// derives from it, so the arrange/act/assert shape is identical and factored
// once rather than duplicated per SPEC.md test.
func runSkillScenarioCase(t *testing.T, tc skillScenarioCase) {
	t.Helper()

	// Arrange: one real daemon+store+sidecar+shim world, one fake-git
	// repository registered as the workspace. Only the systems this suite
	// owns (claude-repld, the shim, shim-store, shim-sidecar) run for real;
	// git is an external dependency and stays mocked via the daemon
	// harness's own scripted fakegit world, exactly as daemon/integration's
	// own tests register a workspace (harness.NewRepo + harness.Register).
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: drive the real `!skill ...` prompt through the daemon's real
	// SubmitPrompt, to the real shim's `--fake` vendor, through to a
	// durable store write (driveDocumentedPrompt, since this scenario's
	// documented invocation form carries the parameterized name/args after
	// "!skill", not a bare "!"+scenario-name).
	turn := driveDocumentedPrompt(t, w, ws, w.DefaultConfigDir, tc.prompt)

	// Assert: the feed settles into a loaded skill card whose document is
	// this skill's own distinctive body (never a fabricated one), and whose
	// invocation line at least names the invoked skill.
	row := awaitSkillLoadedRow(t, w, ws, turn)
	loaded := row.GetActivity().GetSkill().GetLoaded()
	if loaded == nil {
		t.Fatalf("skill row's activity = %v, want a loaded skill card", row.GetActivity())
	}

	if got, want := loaded.GetDocument().GetMarkdown(), expectedSkillDocument(tc.wantSkill); got != want {
		t.Errorf("loaded skill document =\n%q\nwant (this skill's own distinctive body)\n%q", got, want)
	}

	// The invocation line is daemon-composed (feed.proto: "the line a user
	// would have typed"); this suite does not pin its exact composed
	// wording (that is a daemon presentation detail this spec pass never
	// read production source to confirm), only that it names the invoked
	// skill, which is the fact the parameterization exists to prove.
	if inv := row.GetActivity().GetSkill().GetInvocation().GetText(); !strings.Contains(inv, tc.wantSkill) {
		t.Errorf("skill invocation line = %q, want it to name the invoked skill %q", inv, tc.wantSkill)
	}

	// The fake always declares allowances for the invoked skill (skills.ts's
	// SKILL scenario sets `allowedTools` unconditionally), so the loaded
	// card's allowances line must be present (feed.proto: "UNSET when the
	// skill declared none; absence draws no line, never an empty one" --
	// the converse of that rule is what this checks: the skill here DID
	// declare allowances, so the field must be set).
	if loaded.GetAllowances() == nil {
		t.Errorf("loaded skill card's allowances = nil, want the fake's declared allowedTools to have produced one")
	}
}
