//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"testing"
)

// OWNER 4 of PLAYTEST-PLAN.md's partition: B11-B13 -- the tab's arm through
// one turn, attention on a permission ask, and attention on a question.
//
// B.11 and B.12's first half are here. B.13 is unwritten, and B.12's second
// half is blocked (see PLAYTEST-SPEC.md, "What is blocked").

// playtestUnwiredArm is the arm a workspace carries before anything has been
// wired to it, and playtestVendorBlockedArm is the arm a vendor failure
// leaves it on.
//
// Neither is in `emGHISettledArms`, and that is correct rather than an
// oversight: that list is the FINISH EDGE's settled set, while these two are
// states a turn never produced. They are named here so a playbook asserts the
// arm it means instead of "any of the settled ones", which would pass on the
// wrong picture.
const (
	playtestUnwiredArm       = ":none"
	playtestVendorBlockedArm = ":vendor-blocked"
)

// playtestGatedPrompt is the prompt a gated playbook submits.
//
// The fake SDK holds a turn whose FULL submitted text matches the gate text
// until the gate file exists (`agent-shim/claude/shim/src/fake/index.ts`), so
// a playbook that must photograph a RUNNING tab is synchronized on the work
// rather than racing a turn that would otherwise finish first. It carries no
// `!` prefix, so it falls through to the fake's default prose scenario and
// concludes ordinarily once the gate opens.
const playtestGatedPrompt = "hold this turn open for the playtest"

// TestPlaytestTabArmIdleThinkingDone is plan B.11: the tab's arm through one
// ordinary turn, photographed at each of the three states the owner named.
//
// The turn is GATED rather than raced. Waiting for a running arm and then
// photographing would be a race against the fake answering, and a picture
// taken on the wrong side of it is worse than no picture: it looks like a
// product that never paints a running tab.

// TestPlaytestTabArmIdleThinkingDone is plan B.11: the tab's arm through one
// ordinary turn, photographed at each of the three states the owner named.
//
// The turn is GATED rather than raced. Waiting for a running arm and then
// photographing would be a race against the fake answering, and a picture
// taken on the wrong side of it is worse than no picture: it looks like a
// product that never paints a running tab.
func TestPlaytestTabArmIdleThinkingDone(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "b11-turn-gate")
	s := newPlaytestScenario(t, "04-arm-idle-thinking-done",
		"Plan B.11. One ordinary turn, and the workspace's tab through idle, thinking and done.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, playtestGatedPrompt))

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	s.awaitArm(t, name, "the tab's arm before anything is submitted", playtestUnwiredArm)
	s.captureArm(t, "arm-idle", name, "one repository registered, its panel open, nothing submitted",
		playtestUnwiredArm,
		"The webapp's footer says the session is idle and the feed is empty: nothing has run.")

	s.submit(t, playtestGatedPrompt)
	// `:thinking` BY NAME, not "any running arm". The bring-up walks
	// `:none` -> `:init` -> `:submitting` -> `:thinking`, and a wait
	// satisfied by the first of those photographs a prompt still sitting in
	// the hold tray while the session comes up -- which is a real state, and
	// not the one this step is about.
	s.awaitArm(t, name, "the tab's arm to reach thinking once the turn is in flight", ":thinking")
	s.captureArm(t, "arm-thinking", name,
		"the prompt submitted with composer RET, and held in flight by the fake's turn gate",
		":thinking",
		"The webapp's footer says a turn is RUNNING, and the feed carries the user's own prompt "+
			"bubble. The turn cannot conclude: the fake's gate is still shut.")

	// The gate opens only now, so the turn concludes on this playbook's own
	// schedule rather than whenever the fake got there.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}
	done := s.awaitArm(t, name, "the tab's arm to settle when the turn concludes", emGHISettledArms...)
	s.captureArm(t, "arm-done", name, "the gate opened, and the fake's prose answer concluded the turn",
		done,
		"The turn is over: the webapp's footer is idle again and the feed carries the answer.")
}

// TestPlaytestTabArmFailed is plan B.14: a turn that fails, and the tab that
// says so.
//
// `!fail-execution` ends the turn on the vendor's own execution error, which
// is a PURPLE fault by the module's color rule — the vendor's work — and not
// the blue of a broken local environment. That distinction is the picture's
// whole subject.

// TestPlaytestTabArmAttentionOnPermission is plan B.12's first half: a
// permission ask raised against a workspace the user is NOT looking at, and
// the attention marker the tab then paints.
//
// EMACS ANSWERS NOTHING. There is no permission-answering command in
// `lisp/`; the notification policy is Emacs's whole reaction to an ask, and
// the card in the webapp is the answering surface. B.12's second half —
// the marker CLEARING when the ask is answered from that card — is not here,
// because answering means clicking a feed row and the root feed's live tail
// is broken (see PLAYTEST-SPEC.md, "What is blocked").
//
// Two arrangements are forced, and both are the Emacs layer's own:
//   - `agent-repl--emacs-focused-p` is overridden, because it is an
//     environment probe and a container has no desktop to answer it
//     truthfully.
//   - the workspace under the ask is NOT the selected one, which is the case
//     `host.el` routes to `agent-repl-status-blink-tab`.
func TestPlaytestTabArmAttentionOnPermission(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), "b12-ask-gate")
	const askPrompt = "!perm-hold"
	s := newPlaytestScenario(t, "04-arm-attention-on-permission",
		"Plan B.12, first half. A permission ask raised against an unselected workspace, and the "+
			"attention marker its tab paints. Answering the ask — B.12's second half — awaits the "+
			"root feed's live tail.",
		WithEmacsEnv(turnGatePathEnv, gatePath),
		WithEmacsEnv(turnGateTextEnv, askPrompt))
	p, e := s.Book, s.E

	e.Eval(`(progn
             (defun agent-repl-playtest--focused (&rest _) t)
             (advice-add 'agent-repl--emacs-focused-p :override #'agent-repl-playtest--focused)
             t)`)

	first := s.repoAt(t, "repo-asking")
	askingName := s.register(t, first.Dir)
	s.openPanel(t)

	// THE ORDER PROBLEM, and the gate that solves it. `agent-repl-send`
	// submits to the CURRENT workspace, so the turn can only be started while
	// this one is selected -- and the ask must ARRIVE while it is not. So the
	// turn is started here, parked on the fake's gate, the second workspace is
	// selected, and only then is the gate opened.
	s.submit(t, askPrompt)
	s.awaitArm(t, askingName, "the gated turn to be in flight before the switch", emGHIRunningArms...)
	p.note("`!perm-hold` submitted and parked on the fake's turn gate",
		"the arm is one of the module's own running arms, so the turn is genuinely in flight")

	second := s.repoAt(t, "repo-other")
	otherName := s.register(t, second.Dir)
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == otherName })
	p.note("a second repository registered, which selects it",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q, so %q is genuinely unselected", otherName, askingName))

	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}
	e.AwaitTrue("the unselected workspace's attention marker to be drawn",
		`(and (agent-repl-status-attention-visible-p `+elispString(askingName)+`) t)`)
	p.capture("attention-marker", "the gate opened, so the ask fired against the workspace the user is not looking at",
		fmt.Sprintf("`agent-repl-status-attention-visible-p` is true for %q", askingName),
		fmt.Sprintf("The tab bar carries BOTH workspaces. %q is the selected one, and %q — which is NOT "+
			"selected — carries an ATTENTION MARKER beside its name. The marker blinks on the "+
			"module's own schedule, so it may be caught mid-blink; what must be visible is that the "+
			"two tabs are painted differently and the unselected one is the one calling for the user.",
			otherName, askingName))

	// Teardown hygiene, not an assertion: a parked ask must not outlive the
	// playbook, or the world's own shutdown waits on an answer nobody will
	// ever give.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(askingName) + `) t)`)
}
