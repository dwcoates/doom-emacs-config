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
// All three are here. B.12 and B.13 answer their asks from the webapp's own
// card in the real webview, which became possible once the root feed's live
// tail was served (PLAYTEST-SPEC.md, "What this has already found", 2).
//
// WHAT "CLEARS" MEANS, read off the contract rather than the plan's
// shorthand. `RosterRow.attention` (frontend/v1/sidebar.proto) is set when a
// notification fires and CLEARED WHEN SelectWorkspace NAMES THE WORKSPACE --
// never by the answer itself. A user who answers an ask has, by then,
// selected the workspace to look at its card, so the marker leaves on the
// selection and the ask closes on the answer; both playbooks photograph both
// edges apart, so a reviewer is told which of the two each picture shows.

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
	// THE PAGE IS AWAITED, NOT ASSUMED TO HAVE FOLLOWED. The arm await is
	// satisfied by the ROSTER's push, and the footer, the feed and the hold
	// tray are three other views on the same turn -- so a capture taken the
	// instant the arm arrives has photographed the page's PRE-TURN state
	// twice already: an `idle` footer, an empty feed, and the prompt still
	// badged in the hold tray, under a manifest sentence promising the
	// opposite. Each of the three is awaited on its own so a picture can only
	// be taken once the page says what the sentence claims, and so a view
	// that genuinely lags names ITSELF in the failure rather than leaving a
	// reviewer to guess which one did.
	s.awaitInPage(t, "the footer's status word to leave idle, which is the footer following the turn's start",
		`document.querySelector(".footer-status") &&
         document.querySelector(".footer-status").getAttribute("data-arm") !== "idle"`)
	s.awaitInPage(t, "the user's own prompt bubble to arrive on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`)
	// THE TRAY HOLDS THE PROMPT ONLY UNTIL THE SESSION IS UP. A held card
	// carries `data-held-turn` (`webapp/src/tray/held-prompt.ts`), so its
	// ABSENCE from the drawn tray is the release, and the tray having drawn
	// at all is what distinguishes a released prompt from a tray that never
	// rendered.
	s.awaitInPage(t, "the hold tray to have released the prompt, so nothing is still held",
		`document.querySelector('[data-component="hold-tray"]').textContent.trim() !== "" &&
         document.querySelector('[data-component="hold-tray"] [data-held-turn]') === null`)
	s.captureArm(t, "arm-thinking", name,
		"the prompt submitted with composer RET, and held in flight by the fake's turn gate",
		":thinking",
		"The tab bracket is the subject of this picture, and the WEBVIEW BENEATH IT SHOWS THE SAME "+
			"TURN: the footer's left cell reads `thinking` beside `submitting`, the user's own "+
			"prompt bubble (`"+playtestGatedPrompt+"`, signed `You`) sits on the standing tail with "+
			"NO answer beneath it yet, the hold tray reads `held (0) / nothing held` because the "+
			"prompt was released the moment the session came up, and the footer carries a live "+
			"elapsed-seconds count with a `stop` button beside it. Those four are the three DOM "+
			"assertions above plus the tab's own arm, so the picture and the assertions are one "+
			"turn seen four ways rather than a page lagging its roster.")

	// The gate opens only now, so the turn concludes on this playbook's own
	// schedule rather than whenever the fake got there.
	if err := os.WriteFile(gatePath, nil, 0o644); err != nil {
		t.Fatalf("open the fake's turn gate at %s: %v", gatePath, err)
	}
	done := s.awaitArm(t, name, "the tab's arm to settle when the turn concludes", emGHISettledArms...)
	// The same two views, awaited on the settle edge for the same reason.
	s.awaitInPage(t, "the assistant's response bubble to settle on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitInPage(t, "the footer's status word to return to idle once the turn is over",
		`document.querySelector(".footer-status") &&
         document.querySelector(".footer-status").getAttribute("data-arm") === "idle"`)
	s.captureArm(t, "arm-done", name, "the gate opened, and the fake's prose answer concluded the turn",
		done,
		"The turn is over: the webapp's footer status word reads `idle` again, and the feed carries "+
			"BOTH bubbles -- the user's prompt above, and beneath it the assistant's settled prose "+
			"answer.")
}

// ---------------------------------------------------------------------------
// B.12 and B.13 share one arrangement: an ask raised against a workspace the
// user is NOT looking at.
// ---------------------------------------------------------------------------

// playtestAsk is one ask-raising world: the workspace the ask is raised
// against, the workspace the user is looking at instead, and the gate that
// orders the two.
type playtestAsk struct {
	s          *playtestScenario
	gatePath   string
	askingDir  string
	askingName string
	otherName  string
}

// rowAttentionForm reads the attention flag off WS's roster row -- the
// DAEMON's own marker, as pushed, rather than the blink the tab bar times
// from it. It is the assertion behind both "attention set" and "attention
// cleared": `agent-repl-status-attention-visible-p` is what the bar DRAWS,
// and this is what the daemon SAID.
func rowAttentionForm(ws string) string {
	return `(and (agent-repl-roster-row-attention-p (agent-repl-roster-row-for-ws ` + elispString(ws) + `)) t)`
}

// newPlaytestAsk arranges an ask against an unselected workspace and waits
// for the attention marker it raises.
//
// EMACS ANSWERS NOTHING OF ITS OWN. There is no ask-answering command in
// `lisp/`; the notification policy is Emacs's whole reaction to an ask, and
// the card in the webapp is the answering surface.
//
// Two arrangements are forced, and both are the Emacs layer's own:
//   - `agent-repl--emacs-focused-p` is overridden, because it is an
//     environment probe and a container has no desktop to answer it
//     truthfully.
//   - the workspace under the ask is NOT the selected one, which is the case
//     `host.el` routes to `agent-repl-status-blink-tab`.
//
// THE ORDER PROBLEM, and the gate that solves it. `agent-repl-send` submits
// to the CURRENT workspace, so the turn can only be started while this one
// is selected -- and the ask must ARRIVE while it is not. So the turn is
// started here, parked on the fake's gate, the second workspace is selected,
// and only then is the gate opened.
func newPlaytestAsk(t *testing.T, book, purpose, gateName, askPrompt string) *playtestAsk {
	t.Helper()
	box := requireSandbox(t)
	gatePath := filepath.Join(box.Scratch(), gateName)
	s := newPlaytestScenario(t, book, purpose,
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
	s.awaitArm(t, askingName, "the tab's arm before anything is submitted", playtestUnwiredArm)
	p.note("the asking repository registered, its panel open, nothing submitted",
		"the roster's arm for it is "+playtestUnwiredArm+" and the webapp drew its footer against this daemon")

	s.submit(t, askPrompt)
	s.awaitArm(t, askingName, "the gated turn to be in flight before the switch", emGHIRunningArms...)
	p.note("`"+askPrompt+"` submitted with composer RET and parked on the fake's turn gate",
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
	return &playtestAsk{s: s, gatePath: gatePath, askingDir: first.Dir, askingName: askingName, otherName: otherName}
}

// awaitAttentionRaised waits for the daemon's marker on the asking row AND
// for the tab bar to have drawn it, in that order: the push is the fact and
// the drawing is the tab bar following it.
func (a *playtestAsk) awaitAttentionRaised(t *testing.T) {
	t.Helper()
	a.s.E.AwaitTrue("the daemon's roster push to carry the attention marker on the asking workspace",
		rowAttentionForm(a.askingName))
	a.s.E.AwaitTrue("the unselected workspace's attention marker to be drawn",
		`(and (agent-repl-status-attention-visible-p `+elispString(a.askingName)+`) t)`)
}

// captureAttention photographs the asking tab with its marker up, on the
// arm the step is about.
func (a *playtestAsk) captureAttention(t *testing.T, name, act, arm, subject string) {
	t.Helper()
	a.s.captureArm(t, name, a.askingName, act, arm,
		fmt.Sprintf("The tab bar carries BOTH workspaces. %q is the selected one, and %q -- which is NOT "+
			"selected -- carries an ATTENTION MARKER (the `%s` glyph) beside its name. The marker blinks on the "+
			"module's own schedule, so it may be caught mid-blink; what must be visible is that the "+
			"two tabs are painted differently and the unselected one is the one calling for the user. %s",
			a.otherName, a.askingName, "●", subject))
}

// selectAsking switches back to the asking workspace, which is the ONE
// edge the contract clears the marker on: `RosterRow.attention` is cleared
// when SelectWorkspace names the workspace, re-pushing the roster. The
// panel is then re-shown so the ask's card is the thing on screen, and the
// probe is re-pointed at that workspace's webview.
func (a *playtestAsk) selectAsking(t *testing.T) {
	t.Helper()
	s, e, p := a.s, a.s.E, a.s.Book
	e.Eval(`(agent-repl-switch-to-project ` + elispString(a.askingDir) + `)`)
	e.AwaitEval("the asking workspace to become the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == a.askingName })
	e.AwaitTrue("the daemon's roster push to have dropped the attention marker now the workspace is selected",
		`(not `+rowAttentionForm(a.askingName)+`)`)
	e.AwaitTrue("the tab bar to have undrawn the marker",
		`(not (agent-repl-status-attention-visible-p `+elispString(a.askingName)+`))`)
	p.note("`agent-repl-switch-to-project` back to the asking workspace, which is SelectWorkspace",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q, the roster row no longer carries `attention`, "+
			"and `agent-repl-status-attention-visible-p` is nil: the marker left on the selection, "+
			"which is the edge the contract clears it on", a.askingName))
	s.Name = a.askingName
	s.openPanel(t)
}

// TestPlaytestTabArmAttentionOnPermission is plan B.12: a permission ask
// raised against an unselected workspace, the attention marker its tab
// paints, and the marker gone once the user comes to answer it.
//
// `!perm-allow-once` is the ask that HOLDS: the fake's gate keeps the ask
// open until the card answers it, and the turn concludes on that answer --
// so the last picture is of a settled tab and not of a turn parked forever
// (`!perm-hold` never concludes on its own and can only be interrupted,
// which would make "done" unphotographable).
func TestPlaytestTabArmAttentionOnPermission(t *testing.T) {
	t.Parallel()
	const askPrompt = "!perm-allow-once"
	a := newPlaytestAsk(t, "04-arm-attention-on-permission",
		"Plan B.12. A permission ask raised against an unselected workspace, the attention marker "+
			"its tab paints, the marker leaving when the workspace is selected to answer, and the "+
			"ask answered from the webapp's own card.",
		"b12-ask-gate", askPrompt)
	s, p := a.s, a.s.Book

	// `:permission` BY NAME: the daemon's own arm for "a gated call is
	// waiting on the user", and the one the tab bar must paint here.
	s.awaitArm(t, a.askingName, "the asking workspace's arm to reach permission once the ask is open", ":permission")
	a.awaitAttentionRaised(t)
	a.captureAttention(t, "attention-on-permission",
		"the gate opened, so the permission ask fired against the workspace the user is not looking at",
		":permission",
		"Its arm is permission, so the bracket is painted GREEN beneath the marker.")

	a.selectAsking(t)
	s.awaitInPage(t, "the permission card to be open in the asking workspace's webview",
		`document.querySelector('[data-feed-row] [data-permission="allowOnce"]')`)
	// THE SENTENCE'S OTHER PROMISES, ASSERTED. The manifest below tells a
	// reviewer the card offers a standing verb and a denial beside `allow
	// once`, carries the optional `why not` field, and has a RUNNING shell
	// bubble under it. Each is a `data-*` hook the webapp layer's own suite
	// already asserts, so promising them in prose without reading them here
	// would leave the picture the only thing that could catch their loss --
	// and a missing control is exactly what a reviewer's eye slides past.
	s.awaitInPage(t, "the card's other two verbs and its denial-reason field to be drawn beside `allow once`",
		`document.querySelector('[data-feed-row] [data-permission="allowStanding"]') &&
         document.querySelector('[data-feed-row] [data-permission="deny"]') &&
         document.querySelector('[data-feed-row] [data-permission-reason]')`)
	s.awaitInPage(t, "the gated shell call to be drawn beneath the card, its head still live",
		`document.querySelector('[data-feed-row] .shell-bubble[data-state="live"]')`)
	s.captureArm(t, "selected-marker-cleared", a.askingName,
		"the asking workspace selected, its panel showing the open permission card",
		":permission",
		fmt.Sprintf("%q is now the SELECTED tab and carries NO attention marker; its arm is still "+
			"permission, so the selected tab is painted GREEN. The webapp shows the OPEN permission "+
			"card headed `Claude wants to run Bash`, reading `waiting`, offering `allow once`, "+
			"`always allow` and `deny` with an optional `why not` field beside them; the gated "+
			"`$ git status` call is drawn beneath the card as a shell bubble marked `running...`, "+
			"which is what the ask is holding up.", a.askingName))

	s.clickInPage(t, "the permission card's `allow once` button", `[data-feed-row] [data-permission="allowOnce"]`)
	s.awaitInPage(t, "the card to settle as allowed once",
		`document.querySelector('[data-permission-verdict="allowedOnce"]')`)
	done := s.awaitArm(t, a.askingName, "the turn to conclude once the allowed call ran", emGHISettledArms...)
	// The other half of this step's sentence: the allowed call actually RAN
	// and the turn closed on prose. Asserted for the same reason as the open
	// card's controls above -- a verdict chip over a shell head that never
	// left `live` would be a permission answered into nothing, and that is
	// not a difference a picture makes obvious.
	s.awaitInPage(t, "the allowed shell call's head to settle once it ran",
		`document.querySelector('[data-feed-row] .shell-bubble[data-state="settled"]')`)
	s.awaitInPage(t, "the closing prose answer to settle on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.captureArm(t, "answered-done", a.askingName,
		"`allow once` clicked on the card, the allowed call run, and the turn concluded",
		done,
		"The card has closed onto its verdict -- the buttons are gone and an `allowed once` chip "+
			"stands where they were -- the shell bubble beneath it has settled to `ran ... done` "+
			"with `clean` as its output, and the closing answer `Ran the command.` follows. The "+
			"tab carries no marker.")
	p.note("teardown", "nothing is parked: the ask was answered and the turn concluded, so the world shuts down on its own")
}

// TestPlaytestTabArmAttentionOnQuestion is plan B.13: `!ask-single` raised
// against an unselected workspace, the attention marker its tab paints, and
// the marker gone once the user comes to answer it.
//
// A question has NO arm of its own: the daemon's status walk knows
// permissions (a gated call waiting on the user) but a question is asked
// through the tool plane and the turn is simply still running, so the arm
// under an open question is `:thinking`. The tab is therefore RED with the
// marker on it -- which is the picture, and what tells B.13 apart from B.12.
func TestPlaytestTabArmAttentionOnQuestion(t *testing.T) {
	t.Parallel()
	const askPrompt = "!ask-single"
	a := newPlaytestAsk(t, "04-arm-attention-on-question",
		"Plan B.13. A single-select question raised against an unselected workspace, the attention "+
			"marker its tab paints, the marker leaving when the workspace is selected to answer, and "+
			"the question answered from the webapp's own card.",
		"b13-ask-gate", askPrompt)
	s, p := a.s, a.s.Book

	a.awaitAttentionRaised(t)
	s.awaitArm(t, a.askingName, "the asking workspace's arm while the question is open", ":thinking")
	a.captureAttention(t, "attention-on-question",
		"the gate opened, so the question fired against the workspace the user is not looking at",
		":thinking",
		"Its arm is thinking -- a question has no arm of its own -- so the bracket is painted RED beneath the marker.")

	a.selectAsking(t)
	const option = `[data-feed-row] [data-question-option="New worktree off master"]`
	s.awaitInPage(t, "the question card to be open in the asking workspace's webview",
		`document.querySelector('`+option+`') && document.querySelector('[data-feed-row] [data-question-submit]')`)
	// ALL FOUR CHIPS, AND THE FREE-TEXT ESCAPE. The manifest below promises a
	// reviewer a four-option card with a `something else` field under it, so
	// both are read here rather than left to the eye: a card drawn with three
	// of the fake's four options, or one that dropped the escape the proto
	// says every ask offers, is a defect a picture invites a reviewer to
	// count their way past.
	s.awaitInPage(t, "all four of the fake's option chips to be drawn, and the free-text escape beneath them",
		`document.querySelectorAll('[data-feed-row] [data-question-option]').length === 4 &&
         document.querySelector('[data-feed-row] [data-question-other]') !== null`)
	s.captureArm(t, "selected-marker-cleared", a.askingName,
		"the asking workspace selected, its panel showing the open question card",
		":thinking",
		fmt.Sprintf("%q is now the SELECTED tab and carries NO attention marker; its arm is still "+
			"thinking, so the selected tab is painted RED. The webapp shows the OPEN question card "+
			"headed `Setup`, asking `How do you want the new branch set up?`, with the fake's four "+
			"option chips each carrying its own description, a `something else` free-text field "+
			"BENEATH them, and an `answer` button. The free-text field is there even though "+
			"`!ask-single` offered only the four options, and that is the contract rather than a "+
			"stray control: `AgentQuestionSelection.free_text` (conversation/v1/question.proto) "+
			"says an ask ALWAYS offers a free-text escape whether or not the agent asked for one.",
			a.askingName))

	s.clickInPage(t, "the question's first option chip", option)
	s.awaitInPage(t, "the option to read as checked", `document.querySelector('`+option+`').checked`)
	s.clickInPage(t, "the question card's submit button", `[data-feed-row] [data-question-submit]`)
	s.awaitInPage(t, "the question card to settle as answered",
		`document.querySelector('[data-feed-row] .question[data-state="answered"]')`)
	done := s.awaitArm(t, a.askingName, "the turn to conclude once the question was answered", emGHISettledArms...)
	// The answered card has PUT ITS CONTROLS AWAY, and the turn closed on
	// prose. Both are the sentence's promise; a card left showing its chips
	// beside an `answered` state would be an answer the user could still
	// change, which is not what this picture says.
	s.awaitInPage(t, "the answered card to have put its chips and its free-text escape away",
		`document.querySelectorAll('[data-feed-row] [data-question-option]').length === 0 &&
         document.querySelector('[data-feed-row] [data-question-other]') === null &&
         document.querySelector('[data-feed-row] [data-question-submit]') === null`)
	s.awaitInPage(t, "the closing prose answer to settle on the standing tail",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.captureArm(t, "answered-done", a.askingName,
		"the first option chosen, the card submitted, and the turn concluded",
		done,
		"The card has closed onto its answer -- the four chips, the free-text field and the "+
			"`answer` button are gone, and the `Setup` header now carries `New worktree off "+
			"master` on one line -- and the closing answer `The setup question was answered.` "+
			"follows it. The tab carries no marker.")
	p.note("teardown", "nothing is parked: the question was answered and the turn concluded, so the world shuts down on its own")
}
