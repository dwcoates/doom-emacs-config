//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/integration/harness"
)

// OWNER 20 of PLAYTEST-PLAN.md's partition: K61-K63 -- multi-workspace
// concurrency, in ONE Emacs with SEVERAL workspaces.
//
// Every other owner's playbooks act on one workspace at a time, so the thing
// they can never say anything about is the case a user is actually in: two
// workspaces doing different things at the same time, and one tab bar that
// has to be right about BOTH of them. That is this section's whole subject,
// and it is why every capture here is of the tab bar with two tabs on it
// rather than of one tab in isolation.
//
// THE FAKE'S TURN GATE IS ONE ENV PAIR PER EMACS. `turnGatePathEnv` and
// `turnGateTextEnv` are set on the Emacs process and inherited by every
// shim it spawns (hibernation_e2e_test.go), so ALL of this Emacs's
// workspaces gate on the SAME text. That is not a limitation here, it is the
// mechanism: submitting the same gated prompt in two workspaces parks both,
// and opening the one gate file releases both -- which is exactly the
// simultaneity K.61 is about.

// pt20GatedPrompt is the prompt K.61 and K.63 park on.
//
// The fake holds a turn whose FULL submitted text matches the gate text
// until the gate file exists (`agent-shim/claude/shim/src/fake/index.ts`).
// It carries no `!` prefix, so it falls through to the fake's default prose
// scenario and concludes ordinarily once the gate opens.
const pt20GatedPrompt = "hold this turn open for owner twenty"

// pt20AskPrompt is K.62's ask: the fake's held permission request. It is
// ALSO the gate text in that playbook, so the ask can be made to fire at a
// chosen instant -- after the user has moved to another workspace.
const pt20AskPrompt = "!perm-hold"

// pt20IdleArm is the arm a registered-but-never-submitted workspace carries,
// named rather than written at each site so a step asserts the arm it means.
const pt20IdleArm = ":none"

// pt20MergingArm is the arm K.63 photographs, read from the module's own
// vocabulary rather than guessed: `agent-repl-status-color-table` carries
// `:merging` and `agent-repl-status-tab-bar-color-overrides` is where the
// TAB BAR paints it purple, which is the whole reason the merging tab is
// distinguishable from an untouched one in the picture.
const pt20MergingArm = ":merging"

// pt20PermissionArm is the arm a workspace holding an open permission ask
// carries. It is in `emGHIRunningArms`, because an ask is a turn still in
// flight waiting on the user.
const pt20PermissionArm = ":permission"

// pt20OpenPermissionCard is the selector for an ask the page is SHOWING.
//
// GROUNDED IN THE WEBAPP, not guessed: `webapp/src/feed/feed-view.ts` puts
// the row's kind on `[data-row-kind]`, and
// `webapp/src/feed/asks/permission.ts` gives an OPEN consent card the class
// `permission pending` and each of its answer buttons a `[data-permission]`
// attribute -- the same selector shape `webapp/test/webapp-layer/
// cards.layer.test.ts` asserts against. So this matches a card that is drawn
// AND still answerable, which is what "the ask is shown" means.
const pt20OpenPermissionCard = `[data-feed-row][data-row-kind="permission"] .permission.pending [data-permission]`

// pt20MergeGateScript is the merge test gate K.63 holds the merge open with.
//
// WHY A GATE HERE AT ALL. `:merging` is the arm the orchestrator carries
// while the target repository's test gate RUNS, and a scripted gate that
// exits immediately would make that arm a transient a capture races. So the
// gate waits for a file this playbook creates, which puts the merge's
// duration under the playbook's control the same way the fake's turn gate
// puts the turn's under it.
//
// IT IS BOUNDED. A gate that waited forever would hang the run instead of
// failing it, so the loop has a cap and a gate that never opens exits
// NON-ZERO with a stated reason -- which surfaces as a failed merge the
// playbook's own assertion then reports, rather than as a timeout with no
// cause. The cap is 600 iterations of one second, two orders of magnitude
// above the sub-second the playbook itself needs, because it is a runaway
// stop and not a synchronization bound.
//
// `bash` runs it (`merge.TestCommandFor` spawns the script through bash), so
// it needs no execute bit, and it prints the per-suite pass line
// `merge/testgate.go`'s `suitePassed` parses before exiting 0.
const pt20MergeGateScript = `#!/bin/bash
gate="$1"
if [ -z "$gate" ]; then gate="$AGENT_REPL_PLAYTEST_MERGE_GATE"; fi
i=0
while [ ! -f "$gate" ]; do
  i=$((i + 1))
  if [ "$i" -gt 600 ]; then
    echo "playtest merge gate $gate never opened after $i seconds" >&2
    exit 111
  fi
  sleep 1
done
echo "e2e: passed in 1s"
exit 0
`

// pt20WriteMergeGate installs that script and answers its path and its gate
// file's path.
func pt20WriteMergeGate(t *testing.T, dir string) (script, gate string) {
	t.Helper()
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("prepare the merge gate directory %s: %v", dir, err)
	}
	script = filepath.Join(dir, "gated-test-all.sh")
	gate = filepath.Join(dir, "merge-gate")
	body := strings.Replace(pt20MergeGateScript,
		`gate="$AGENT_REPL_PLAYTEST_MERGE_GATE"`, `gate="`+gate+`"`, 1)
	if err := os.WriteFile(script, []byte(body), 0o755); err != nil {
		t.Fatalf("write the merge gate script %s: %v", script, err)
	}
	return script, gate
}

// pt20OpenGate creates a gate file, which is what RELEASES whatever is
// parked on it.
func pt20OpenGate(t *testing.T, path string) {
	t.Helper()
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("open the gate at %s: %v", path, err)
	}
}

// pt20ArmClaim is one workspace's arm as a capture's subject.
type pt20ArmClaim struct {
	// WS is the workspace, and Want is the arm the step is about.
	WS   string
	Want string
}

// pt20CaptureArms takes ONE tab-bar picture whose subject is what several
// workspaces are painted with AT THE SAME INSTANT, and refuses if any one of
// them has moved off the arm the step is about.
//
// WHY IT IS NOT `captureArm` CALLED TWICE. `captureArm` re-reads one arm and
// refuses if THAT one moved -- which is exactly right when the picture is
// about one tab. This section's pictures are about the tab bar holding two
// DIFFERENT states at once, and two sequential calls would each re-read
// their own arm at a different instant and take a picture each: a reviewer
// would then be shown two frames of a concurrency claim neither of them
// makes. So every arm is re-read first, the whole set is held to the step's
// claim, and ONE picture is taken with a manifest sentence naming all of
// them.
//
// The colors come from the module's own tables through `armPaint`, so the
// sentence a reviewer checks is the product's decision rather than this
// file's guess at it -- including the TAB BAR's own overrides, which is what
// makes the merging tab's purple the sentence's word and not "no disc".
func (s *playtestScenario) pt20CaptureArms(t *testing.T, name, act string, claims []pt20ArmClaim, extra string) {
	t.Helper()
	if len(claims) == 0 {
		t.Fatalf("pt20CaptureArms(%s) was given no arm to be about", name)
	}
	var asserted, sentences []string
	for _, claim := range claims {
		arm, color := s.armPaint(t, claim.WS)
		if arm != claim.Want {
			t.Fatalf("the arm for %s is %s at capture time, want %s: the step's subject moved before "+
				"its picture was taken", claim.WS, arm, claim.Want)
		}
		face := s.tabFaceFor(t, claim.WS)
		asserted = append(asserted, fmt.Sprintf(
			"`agent-repl-roster-status-for-ws` still reads %s for %q at the instant of the capture, "+
				"which the module's tables paint %s, and `agent-repl-workspace-tabline-formatted` "+
				"wrote the face %s", arm, claim.WS, color, face))
		sentences = append(sentences, armSentence(claim.WS, arm, color))
	}
	s.Book.capture(name, act, strings.Join(asserted, "; "), strings.Join(sentences, " ")+" "+extra)
}

// pt20Select switches to a workspace by its PROJECT ROOT and asserts the
// selection landed -- the module's own current-workspace accessor, which is
// what `playtest_03_switch_and_close_test.go` asserts a switch with.
func (s *playtestScenario) pt20Select(t *testing.T, dir, name string) {
	t.Helper()
	s.E.Eval(`(agent-repl-switch-to-project ` + elispString(dir) + `)`)
	s.E.AwaitEval(fmt.Sprintf("%q to become the selected workspace", name),
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == name })
}

// pt20PointAt re-points the scenario's acting workspace, which is what
// `playtestScenario` documents its Name and Input fields for: "a playbook
// with several workspaces re-points them". Every playbook in this file does.
func (s *playtestScenario) pt20PointAt(t *testing.T, name string) {
	t.Helper()
	s.Name = name
	s.Input = awaitInputBuffer(t, s.E, name)
}

// ---------------------------------------------------------------------------
// K.61 -- two workspaces thinking at once
// ---------------------------------------------------------------------------

// TestPlaytestTwoWorkspacesThinkingAtOnce is plan K.61.
//
// BOTH TURNS ARE HELD, and that is the only way this picture exists. A
// playbook that submitted in one workspace, switched, submitted in the other
// and then photographed would be racing the fake's answer in the first: by
// the time the second turn is in flight the first has concluded, and the
// picture would show one red tab and one green -- which is a true picture of
// a product that never runs two turns at once, and a false one of this one.
// So both are parked on the fake's single gate, both arms are asserted red,
// the picture is taken, and only then is the gate opened.
func TestPlaytestTwoWorkspacesThinkingAtOnce(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "k61-turn-gate")
	s := newPlaytestScenario(t, "20-concurrency-two-thinking",
		"Plan K.61. Two workspaces holding a turn each AT THE SAME TIME, the tab bar carrying both "+
			"as running, and each settling on its own when the one gate opens.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, pt20GatedPrompt))
	p, e := s.Book, s.E

	// Arrange: the first workspace, with its panel open so the composer RET
	// path is the real one.
	first := s.repoAt(t, "repo-first")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	s.awaitArm(t, firstName, "the first workspace's arm before anything is submitted", pt20IdleArm)
	p.note("the first repository registered through `SPC TAB C-n` and its panel opened",
		fmt.Sprintf("the webapp drew its footer against this daemon, and %q's arm is %s", firstName, pt20IdleArm))

	// Act: park the first turn.
	s.submit(t, pt20GatedPrompt)
	// `:thinking` BY NAME. The bring-up walks `:none` -> `:init` ->
	// `:submitting` -> `:thinking`, and a wait satisfied by an earlier one
	// would photograph a session still coming up -- a real state, and not
	// the one K.61 is about.
	s.awaitArm(t, firstName, "the first workspace's turn to reach thinking", ":thinking")
	p.note("the gated prompt submitted in the first workspace with composer RET",
		fmt.Sprintf("%q's arm is `:thinking`, and the fake's gate at %s is still shut, so the turn "+
			"cannot conclude", firstName, gatePath))

	// Act: a second workspace, which registering SELECTS, and its own turn.
	second := s.repoAt(t, "repo-second")
	secondName := s.register(t, second.Dir)
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	s.openPanel(t)
	s.pt20PointAt(t, secondName)
	s.submit(t, pt20GatedPrompt)
	s.awaitArm(t, secondName, "the second workspace's turn to reach thinking", ":thinking")
	p.note("a second repository registered, its panel opened, and the SAME gated prompt submitted in it",
		fmt.Sprintf("%q's arm is `:thinking` too -- the fake's gate is one env pair per Emacs, so both "+
			"workspaces park on the same text", secondName))

	// Assert and capture: BOTH arms, re-read at one instant, in one picture.
	if names := s.tabNames(); len(names) != 2 {
		t.Fatalf("the tab bar draws %v, want both workspaces", names)
	}
	s.pt20CaptureArms(t, "both-thinking",
		"two workspaces each holding a turn parked on the fake's one gate",
		[]pt20ArmClaim{{WS: firstName, Want: ":thinking"}, {WS: secondName, Want: ":thinking"}},
		"THE POINT OF THIS PICTURE IS THAT BOTH ARE RED AT ONCE. The tab bar carries TWO workspace "+
			"tabs and neither is idle: two turns are genuinely in flight in one editor. The second "+
			"tab is the selected one -- registering selects -- so it may carry the selection "+
			"highlight as well as its running color.")

	// Act: the one gate releases both.
	pt20OpenGate(t, gatePath)
	firstDone := s.awaitArm(t, firstName, "the first workspace's turn to settle", emGHISettledArms...)
	secondDone := s.awaitArm(t, secondName, "the second workspace's turn to settle", emGHISettledArms...)
	s.pt20CaptureArms(t, "both-settled",
		"the fake's gate opened, so both parked turns concluded",
		[]pt20ArmClaim{{WS: firstName, Want: firstDone}, {WS: secondName, Want: secondDone}},
		"Both turns are over. EACH WORKSPACE SETTLED ON ITS OWN ARM: they were released by one gate "+
			"but concluded independently, and nothing about one tab's finish moved the other's.")
}

// ---------------------------------------------------------------------------
// K.62 -- attention in a background workspace while the foreground is idle
// ---------------------------------------------------------------------------

// TestPlaytestAttentionInBackgroundWhileForegroundIdle is plan K.62.
//
// This is the case a single-workspace playbook cannot produce: the user is
// looking at a workspace that has NOTHING running, and a DIFFERENT workspace
// wants them. What must be true is that the background tab says so while the
// foreground tab stays plainly idle, and that going there shows the ask.
//
// Two arrangements are forced, and both are the Emacs layer's own:
//   - `agent-repl--emacs-focused-p` is overridden, because it is an
//     environment probe and a container has no desktop to answer it
//     truthfully. Without it the notification policy takes the unfocused
//     branch and the tab marker is never the reaction under test.
//   - the ask must ARRIVE while its workspace is unselected, which is the
//     case `host.el` routes to `agent-repl-status-blink-tab`. Since
//     `agent-repl-send` submits to the CURRENT workspace, the turn is
//     started while it IS selected and parked on the fake's gate, the other
//     workspace is selected, and only then is the gate opened.
func TestPlaytestAttentionInBackgroundWhileForegroundIdle(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "k62-ask-gate")
	s := newPlaytestScenario(t, "20-concurrency-background-attention",
		"Plan K.62. A permission ask fires against a BACKGROUND workspace while the one the user is "+
			"looking at is idle, and switching to it shows the ask.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, pt20AskPrompt))
	p, e := s.Book, s.E

	e.Eval(`(progn
             (defun agent-repl-playtest--focused (&rest _) t)
             (advice-add 'agent-repl--emacs-focused-p :override #'agent-repl-playtest--focused)
             t)`)
	p.note("`agent-repl--emacs-focused-p` overridden to answer true",
		"the notification policy takes its FOCUSED branch, which is the one that marks a tab; a "+
			"container has no desktop to answer the real probe truthfully")

	// Arrange: the workspace the ask will be raised against.
	asking := s.repoAt(t, "repo-asking")
	askingName := s.register(t, asking.Dir)
	s.openPanel(t)

	// Act: start the ask's turn while this workspace is still selected, and
	// park it on the gate.
	s.submit(t, pt20AskPrompt)
	s.awaitArm(t, askingName, "the ask's turn to be in flight before the switch", emGHIRunningArms...)
	p.note("`!perm-hold` submitted in the first workspace and parked on the fake's turn gate",
		fmt.Sprintf("%q's arm is one of the module's own running arms, so the turn is genuinely in "+
			"flight and the ask has not yet gone out", askingName))

	// Arrange: the workspace the user will be looking at. NOTHING is ever
	// submitted in it, which is what "the foreground is idle" means.
	idle := s.repoAt(t, "repo-idle")
	idleName := s.register(t, idle.Dir)
	e.AwaitEval("the idle workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == idleName })
	s.openPanel(t)
	s.pt20PointAt(t, idleName)
	s.awaitArm(t, idleName, "the foreground workspace's arm to be idle", pt20IdleArm)
	p.note("a second repository registered -- which selects it -- and its panel opened; nothing is ever submitted in it",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q and its arm is %s, so the foreground is "+
			"genuinely idle and %q is genuinely a BACKGROUND workspace", idleName, pt20IdleArm, askingName))

	// Act: the gate opens, so the ask fires against the workspace the user is
	// NOT looking at.
	pt20OpenGate(t, gatePath)
	e.AwaitTrue("the background workspace's attention marker to be drawn",
		`(and (agent-repl-status-attention-visible-p `+elispString(askingName)+`) t)`)
	s.awaitArm(t, askingName, "the background workspace's arm to reach permission", pt20PermissionArm)
	// THE FOREGROUND IS RE-READ HERE, not assumed. What K.62 claims is that
	// the background's attention did not disturb the tab the user is on, and
	// that claim is only made by reading the foreground arm AFTER the ask
	// arrived.
	s.awaitArm(t, idleName, "the foreground workspace's arm to still be idle after the ask arrived", pt20IdleArm)
	s.pt20CaptureArms(t, "attention-in-background",
		"the gate opened, so the ask fired against the workspace the user is NOT looking at",
		[]pt20ArmClaim{{WS: idleName, Want: pt20IdleArm}, {WS: askingName, Want: pt20PermissionArm}},
		fmt.Sprintf("`agent-repl-status-attention-visible-p` is true for %q, so BESIDES its status "+
			"color that tab must carry an ATTENTION MARKER. %q is the SELECTED tab and must look "+
			"plainly untouched. The marker blinks on the module's own schedule and may be caught "+
			"mid-blink; what must be visible is that the two tabs are painted differently and the "+
			"UNSELECTED one is the one calling for the user.", askingName, idleName))

	// Act: the user goes there. The ask must be on the page.
	s.pt20Select(t, asking.Dir, askingName)
	s.pt20PointAt(t, askingName)
	s.awaitInPage(t, "the open permission card to be shown in the page the user switched to",
		`document.querySelector(`+jsString(pt20OpenPermissionCard)+`) !== null`)
	s.pt20CaptureArms(t, "ask-shown-after-switch",
		"`agent-repl-switch-to-project` to the asking workspace's own project root",
		[]pt20ArmClaim{{WS: askingName, Want: pt20PermissionArm}, {WS: idleName, Want: pt20IdleArm}},
		fmt.Sprintf("The selection has MOVED to %q -- that tab is now the highlighted one -- and the "+
			"webapp below the tab bar carries an OPEN PERMISSION CARD with answer buttons on it: "+
			"`%s` matched. The ask raised while the workspace was in the background is the thing the "+
			"user is now looking at.", askingName, pt20OpenPermissionCard))

	// Teardown hygiene, not an assertion: EMACS ANSWERS NOTHING -- there is
	// no permission-answering command in `lisp/` -- so a parked ask must not
	// outlive the playbook, or the world's own shutdown waits on an answer
	// nobody will ever give. `playtest_04_tab_arms_test.go` kills it the same
	// way.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(askingName) + `) t)`)
}

// ---------------------------------------------------------------------------
// K.63 -- a merge in flight beside a turn in flight
// ---------------------------------------------------------------------------

// pt20MergeSettledArms are the arms a merge run ends on.
//
// Read from `agent-repl-status-color-table`'s merge family rather than
// invented: `:merged` is the clean landing, and the other two are the ways
// it stops. All three are TERMINAL, which is what this step waits for -- and
// listing all three means a merge that ended badly is reported as the arm it
// ended on instead of timing out with no cause.
var pt20MergeSettledArms = []string{":merged", ":merge-conflict", ":merge-failed"}

// pt20CreateChild runs the ORDINARY create command with only its READERS
// stubbed, the way `emacs_handover_e2e_test.go`'s scenario 40 does: the
// command's own call sites run, and nothing reaches past the command.
func pt20CreateChild(t *testing.T, e *Emacs, label, name, prompt string) {
	t.Helper()
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(label) + `))
                       ((symbol-function 'read-string)
                        (lambda (prompt &rest _)
                          (cond ((string-prefix-p "Initial prompt" prompt) ` + elispString(prompt) + `)
                                ((string-prefix-p "Name" prompt) ` + elispString(name) + `)
                                (t "")))))
                 (agent-repl-create-workspace)
                 t)`)
}

// pt20ChildOpeningPrompt is the initial prompt the merging workspace is
// created with.
//
// IT MUST NOT BE THE GATE TEXT. The gate matches the FULL submitted text, so
// a child created with the gated prompt would park its opening turn too --
// and a workspace merges from IDLE, so the merge could never start. This is
// the fake's plain streamed-prose scenario, which concludes on its own.
const pt20ChildOpeningPrompt = "!prose-streamed"

// pt20MergeTriggerPath is the file the merging workspace lands a commit on.
//
// A merge needs something to merge, and it is committed through the SCRIPTED
// FAKE GIT the world installed ahead of Emacs on PATH (`harness.Repo`). NO
// REAL GIT PROCESS RUNS ANYWHERE, which is the module's own standing rule.
// The path is deliberately NOT under the daemon's own subsystem: a
// daemon-subsystem range on a self-repo classifies as a self-merge rollout
// and hands the daemon over mid-playbook, which would tear down the very
// concurrency this playbook is about.
const pt20MergeTriggerPath = "docs/playtest-owner-20.md"

// TestPlaytestMergeBesideRunningTurn is plan K.63.
//
// The claim is that the two pipelines are INDEPENDENT: the system's own work
// on one workspace and the agent's work on another are in flight at the same
// instant, the tab bar says which is which, and each settles without waiting
// on the other.
//
// BOTH SIDES ARE GATED, on two different files. The turn is parked on the
// fake's turn gate; the merge is parked on a scripted test gate that waits
// for a file of its own. Without both, `:merging` and `:thinking` would each
// be a transient and the mid-flight picture would be a race between them.
func TestPlaytestMergeBesideRunningTurn(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	turnGate := filepath.Join(box.Scratch(), "k63-turn-gate")
	gateScript, mergeGate := pt20WriteMergeGate(t, filepath.Join(box.Scratch(), "k63-merge-gate"))

	// Arrange: the repository the merge lands in is stated as the daemon's
	// own checkout, because that is the one shape whose merge geometry the
	// Emacs-layer create/merge pair drives end to end
	// (`emacs_handover_e2e_test.go`). Its test gate is the gated script, so
	// the merge parks where this playbook chooses.
	selfRepo := harness.NewRepoAt(t, filepath.Join(box.Scratch(), "self-repo"))
	s := newPlaytestScenario(t, "20-concurrency-merge-beside-turn",
		"Plan K.63. One workspace's MERGE and another workspace's TURN in flight at the same instant, "+
			"and each settling on its own.",
		WithEmacsEnv("AGENT_REPL_SELF_REPO_DIR", selfRepo.Dir),
		WithEmacsEnv("AGENT_REPL_TEST_ALL_SCRIPT", gateScript),
		WithEmacsEnv(turnGatePathEnv, turnGate),
		WithEmacsEnv(turnGateTextEnv, pt20GatedPrompt))
	p, e := s.Book, s.E
	s.E.ArtifactPaths = append(s.E.ArtifactPaths, filepath.Join(selfRepo.Dir, ".claude"))

	// Arrange: register the repository so the roster carries the section the
	// create command picks from.
	repoName := s.register(t, selfRepo.Dir)
	label := emHO40AwaitSectionLabel(t, e, selfRepo.Dir)
	p.note("the merge target repository registered through `SPC TAB C-n`",
		fmt.Sprintf("the roster carries its repository section as %q, and Emacs's registry carries "+
			"%q", label, repoName))

	// Arrange: the workspace that will be MERGED, and its opening turn
	// concluded -- a workspace merges from idle, never mid-turn.
	before := e.EvalStrings(emGHIWorkspaceNamesForm)
	pt20CreateChild(t, e, label, "k63-merging", pt20ChildOpeningPrompt)
	after := decodeStrings(e.AwaitEval("the merging workspace to appear in Emacs's registry",
		emGHIWorkspaceNamesForm,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == len(before)+1 }))
	merging := emHO40AddedName(t, before, after)
	s.awaitArm(t, merging, "the merging workspace's opening turn to conclude", emGHISettledArms...)
	mergingDir := e.EvalString(`(or (plist-get (agent-repl-host-ref ` + elispString(merging) + `) :dir) "")`)
	if mergingDir == "" {
		t.Fatalf("the workspace %q holds no worktree directory, want the daemon-minted one", merging)
	}
	p.note("a child workspace created on that repository through `agent-repl-create-workspace`, and its opening turn let conclude",
		fmt.Sprintf("%q is in Emacs's registry on a daemon-minted worktree, and its arm is settled -- "+
			"a workspace merges from IDLE", merging))

	// Arrange and act: the OTHER workspace, holding a turn parked on the
	// fake's gate.
	other := s.repoAt(t, "repo-turning")
	otherName := s.register(t, other.Dir)
	s.openPanel(t)
	s.pt20PointAt(t, otherName)
	s.submit(t, pt20GatedPrompt)
	s.awaitArm(t, otherName, "the other workspace's turn to reach thinking", ":thinking")
	p.note("a second repository registered, its panel opened, and the gated prompt submitted in it",
		fmt.Sprintf("%q's arm is `:thinking`, parked on the fake's turn gate", otherName))

	// Act: land a commit in the merging workspace's own worktree through the
	// SCRIPTED FAKE GIT, then enqueue the merge through the ordinary verb.
	selfRepo.CommitIn(mergingDir, pt20MergeTriggerPath, "owner 20, playbook K.63\n")
	// The binding is a LOOKUP and the command is then invoked with its
	// argument, exactly the way `register` asserts `TAB C-n`: `SPC TAB M`
	// PROMPTS for a workspace when given none, so pressing it would open a
	// completing-read instead of merging.
	if want, got := "agent-repl-merge-workspace", e.LeaderBinding("TAB M"); got != want {
		t.Fatalf("SPC TAB M resolves to %q, want %q", got, want)
	}
	e.Eval(`(agent-repl-merge-workspace ` + elispString(merging) + `)`)

	// Assert: the merge reaches `:merging` -- the arm the orchestrator
	// carries while the target's test gate RUNS, which is the arm parked on
	// the merge gate -- while the other workspace is still thinking.
	s.awaitArm(t, merging, "the merging workspace's arm to reach merging", pt20MergingArm)
	s.awaitArm(t, otherName, "the other workspace's turn to still be thinking", ":thinking")
	if names := s.tabNames(); len(names) < 3 {
		t.Fatalf("the tab bar draws %v, want the repository, the merging workspace and the turning one", names)
	}
	s.pt20CaptureArms(t, "merging-beside-thinking",
		"the merge enqueued through `agent-repl-merge-workspace` and parked on the scripted test gate, "+
			"with the other workspace's turn still parked on the fake's turn gate",
		[]pt20ArmClaim{{WS: merging, Want: pt20MergingArm}, {WS: otherName, Want: ":thinking"}},
		"THE POINT OF THIS PICTURE IS THE TWO KINDS OF WORK SIDE BY SIDE. One tab reports the "+
			"SYSTEM's work -- a merge running, which the tab bar's own override paints PURPLE rather "+
			"than the shared table's none -- and another reports the AGENT's, painted red. They must "+
			"be visibly different colors on the same bar.")

	// Act: each side is released by its OWN gate, so neither settles because
	// the other did.
	pt20OpenGate(t, mergeGate)
	mergeArm := s.awaitArm(t, merging, "the merge to reach a terminal arm", pt20MergeSettledArms...)
	if mergeArm != ":merged" {
		t.Fatalf("the merge for %q ended on %s, want :merged: the scripted gate passes and no conflict "+
			"was scripted, so any other terminal arm is a real failure", merging, mergeArm)
	}
	pt20OpenGate(t, turnGate)
	turnArm := s.awaitArm(t, otherName, "the other workspace's turn to settle", emGHISettledArms...)
	s.pt20CaptureArms(t, "both-settled",
		"the merge gate opened and then the turn gate, so each side was released by its own",
		[]pt20ArmClaim{{WS: merging, Want: mergeArm}, {WS: otherName, Want: turnArm}},
		"Both pipelines are done. EACH SETTLED ON ITS OWN ARM and from its own gate: the merge "+
			"landed and the turn concluded, and neither finish moved the other's tab.")
}
