//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"strings"
	"testing"
)

// OWNER 12 of PLAYTEST-PLAN.md's partition: E37-E41 -- permissions,
// questions, the parked ask across a workspace switch, and the topbar's
// permission-mode picker.
//
// E37, E38 and E40 are TABLE-DRIVEN, one loop per family, with a capture of
// the card open and a capture of it answered or settled per row. Every row
// of a family runs in ONE world, one after another on the same workspace:
// the feed accumulates, so a row's card is addressed by its ORDINAL among
// the family's rows rather than by "the open one", which would be ambiguous
// the moment a second card existed.
//
// EMACS ANSWERS NOTHING. There is no permission- or question-answering
// command in `lisp/` (EMACS-LAYER-SPEC.md, "Two brief items the contract
// does not have"); the card in the webapp is the answering surface, so every
// answer here is a CLICK inside the real webview, the way a user answers.

// playtestAskBound bounds one wait on an ask's SETTLEMENT being drawn: the
// click's AnswerPermission/AnswerQuestion round trip, the shim resuming the
// gated call, the daemon re-pushing the row, and the page drawing it.
//
// MEASURED, over the runs that produced the captures in `.playtest-out`
// (the roster arm's own settle after the click on a permission card): 0.9s,
// 1.1s, 1.3s, 1.4s, 1.6s. 5s is roughly three times the worst of those; the
// page-draw bound (2s) is for a redraw of something already pushed and is
// not what this wait is on.
const playtestAskBound = DefaultTimeout

// permissionRowSel addresses the family's i-th permission card.
func permissionRowSel(i int) string {
	return fmt.Sprintf(`document.querySelectorAll('[data-feed-row][data-row-kind="permission"]')[%d]`, i)
}

// questionRowSel addresses the family's i-th question card.
func questionRowSel(i int) string {
	return fmt.Sprintf(`document.querySelectorAll('[data-feed-row][data-row-kind="question"]')[%d]`, i)
}

// readInPage answers a JavaScript expression's STRING inside the webview.
//
// It is the substrate's probe used for a value rather than a verdict: the
// script is issued, and the answer is polled through the same two-eval
// shape `awaitInPageFor` uses, until the callback has stored one. The
// expression is wrapped so a throw is answered as text rather than lost,
// and so an empty string is still an answer.
func (s *playtestScenario) readInPage(t *testing.T, what, expression string) string {
	t.Helper()
	script := `(function () { try { return "ok:" + String(` + expression + `); }
	                          catch (e) { return "threw:" + e; } })()`
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	raw := s.E.AwaitEvalFor(playtestPageBound, "reading "+what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(script)+`)`,
		func(raw json.RawMessage) bool {
			return strings.HasPrefix(decodeString(raw), "ok:") || strings.HasPrefix(decodeString(raw), "threw:")
		})
	answer := decodeString(raw)
	if strings.HasPrefix(answer, "threw:") {
		t.Fatalf("reading %s threw inside the page: %s", what, strings.TrimPrefix(answer, "threw:"))
	}
	return strings.TrimPrefix(answer, "ok:")
}

// clickWithin clicks one element addressed by a JavaScript ELEMENT
// EXPRESSION rather than a selector, the way `clickInPage` does for a
// selector: the element is waited for first, and the click's own answer is
// waited on, so an expression that names nothing fails as "nothing to click".
// It exists because a family's rows are addressed by ordinal
// (`document.querySelectorAll(...)[i]`), which no selector can spell.
func (s *playtestScenario) clickWithin(t *testing.T, what, elementExpr string) {
	t.Helper()
	s.awaitInPage(t, what+" to be there to click", elementExpr+` != null`)
	s.clickOnce(t, what, elementExpr)
}

// ---------------------------------------------------------------------------
// E37 and E38: permissions
// ---------------------------------------------------------------------------

// permissionRow is one row of the permission families.
type permissionRow struct {
	prompt string
	// answer is the `data-permission` button clicked on the open card; empty
	// for a scenario whose denial never reaches an open ask.
	answer string
	// standing says whether the open card must offer the standing button.
	standing bool
	// verdict is the `data-permission-verdict` arm the settled card carries.
	verdict string
	// toolRan says whether the gated call then ran (returned) or was denied.
	toolRan bool
	// mode is the permission mode the topbar must read after the answer, when
	// the answer changes it.
	mode string
	// open and settled are the manifest sentences.
	open, settled string
}

// runPermissionFamily drives one table of permission rows in one world.
func runPermissionFamily(t *testing.T, s *playtestScenario, rows []permissionRow) {
	t.Helper()
	p := s.Book
	for i, row := range rows {
		sel := permissionRowSel(i)
		s.submit(t, row.prompt)

		if row.answer != "" {
			// THE CARD IS OPEN, and its buttons are the ones the vendor's offer
			// permits: `[data-permission="allowStanding"]` exists iff the ask
			// carried `standing_offered` (feed.proto field 5).
			s.awaitInPageFor(t, playtestAskBound, "the permission card to be drawn open",
				sel+` && `+sel+`.querySelector('.permission[data-state="open"]') !== null`)
			s.awaitInPage(t, "the open card's buttons to match the offer",
				`(`+sel+`.querySelector('[data-permission="allowOnce"]') !== null) && `+
					`(`+sel+`.querySelector('[data-permission="deny"]') !== null) && `+
					`((`+sel+`.querySelector('[data-permission="allowStanding"]') !== null) === `+fmt.Sprint(row.standing)+`)`)
			s.awaitArm(t, s.Name, "the tab's arm to say the turn is waiting on the ask", emGHIRunningArms...)
			buttons := s.readInPage(t, "the open card's button labels",
				`Array.prototype.map.call(`+sel+`.querySelectorAll('[data-permission]'), function (b) { return b.textContent; }).join(" | ")`)
			headline := s.readInPage(t, "the open card's headline", sel+`.querySelector('.perm-headline').textContent`)
			p.capture(strings.TrimPrefix(row.prompt, "!")+"-open",
				fmt.Sprintf("`%s` submitted with composer RET", row.prompt),
				fmt.Sprintf("the card is drawn `[data-state=\"open\"]`, its buttons are [%s] (standing offered: %v), the headline reads %q, and the roster arm is a running one",
					buttons, row.standing, headline),
				row.open)

			s.clickWithin(t, "the "+row.answer+" button", sel+`.querySelector('[data-permission="`+row.answer+`"]')`)
		}

		// THE SETTLED CARD, by its arm's NAME: the verdict element names the
		// answered arm, so a denial by policy can never be mistaken for the
		// user's own deny.
		s.awaitInPageFor(t, playtestAskBound, "the permission card to settle as "+row.verdict,
			sel+` && `+sel+`.querySelector('[data-permission-verdict="`+row.verdict+`"]') !== null`)
		// THE GATED CALL'S OWN SETTLEMENT: the tool card's `data-state` is the
		// outcome arm (`returned` or `denied`), mirrored onto the row, and a
		// returned call's `data-verdict` is the verdict arm on the card itself.
		toolState := `denied`
		toolExpr := `document.querySelectorAll('[data-feed-row][data-unit="simpleToolCall"][data-state="denied"]').length`
		if row.toolRan {
			toolState = `returned.succeeded`
			toolExpr = `document.querySelectorAll('[data-feed-row][data-unit="simpleToolCall"][data-state="returned"] .tool-card[data-verdict="succeeded"]').length`
		}
		s.awaitInPageFor(t, playtestAskBound, "the gated tool call to settle "+toolState, toolExpr+` >= `+fmt.Sprint(i+1))
		if row.mode != "" {
			s.awaitInPageFor(t, playtestAskBound, "the topbar's mode picker to read "+row.mode,
				`document.querySelector('.topbar-mode-button[data-mode="`+row.mode+`"]') !== null`)
		}
		s.awaitArm(t, s.Name, "the turn to conclude", emGHISettledArms...)
		verdict := s.readInPage(t, "the verdict wording", sel+`.querySelector('[data-permission-verdict]').textContent`)
		act := "the " + row.answer + " button clicked on the card"
		if row.answer == "" {
			act = "nothing clicked: the denial never reached an open ask"
		}
		p.capture(strings.TrimPrefix(row.prompt, "!")+"-settled", act,
			fmt.Sprintf("the card carries `[data-permission-verdict=\"%s\"]` reading %q, the gated call settled %s, and the roster arm settled",
				row.verdict, verdict, toolState),
			row.settled)
	}
}

// TestPlaytestPermissionAllowFamily is plan E.37: the three allows, the card
// each opens, the answer, and standing offered versus not.
func TestPlaytestPermissionAllowFamily(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "12-perm-allow",
		"Plan E.37. `!perm-allow-once`, `!perm-allow-standing`, `!perm-allow-standing-mode` and "+
			"`!perm-no-standing`: the card, the answer clicked on it, and the standing button "+
			"present or absent as the vendor's offer decides.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	runPermissionFamily(t, s, []permissionRow{
		{
			prompt: "!perm-allow-once", answer: "allowOnce", standing: true, verdict: "allowedOnce", toolRan: true,
			open: "A PENDING permission card at the bottom of the feed: a headline naming the gated Bash call, " +
				"its argument line, a ticking `waiting` clock, and THREE buttons in this order: `allow once`, " +
				"`always allow`, `deny`, with a `why not (optional)` text field beside them.",
			settled: "The same card RESOLVED: the buttons are gone and an OK-colored badge reads `allowed once`. " +
				"Beneath it the gated tool call has run to success and the turn concluded with prose.",
		},
		{
			prompt: "!perm-allow-standing", answer: "allowStanding", standing: true, verdict: "allowedStanding", toolRan: true,
			open: "A SECOND pending permission card below the first (resolved) one, again with all THREE buttons: " +
				"`allow once`, `always allow`, `deny`.",
			settled: "The second card RESOLVED with an OK-colored badge reading `allowed with standing`. " +
				"The gated call ran and the turn concluded.",
		},
		{
			prompt: "!perm-allow-standing-mode", answer: "allowStanding", standing: true, verdict: "allowedStanding", toolRan: true,
			mode: "accept_edits",
			open: "A THIRD pending card with all THREE buttons. The topbar's permission-mode button still reads " +
				"`default`.",
			settled: "The third card RESOLVED reading `allowed with standing`, AND the topbar's permission-mode " +
				"button now reads `accept edits`: the vendor's offered standing changed the session's mode.",
		},
		{
			prompt: "!perm-no-standing", answer: "allowOnce", standing: false, verdict: "allowedOnce", toolRan: true,
			open: "A FOURTH pending card with only TWO buttons, `allow once` and `deny`: NO `always allow` button " +
				"is drawn, because the vendor offered no standing form for this call.",
			settled: "The fourth card RESOLVED reading `allowed once`; the gated call ran and the turn concluded.",
		},
	})
}

// TestPlaytestPermissionDenyFamily is plan E.38: every refusal's wording.
func TestPlaytestPermissionDenyFamily(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "12-perm-deny",
		"Plan E.38. `!perm-deny-user`, `!perm-deny-policy` and `!perm-undecidable`: each refusal's own "+
			"wording, and which of them ever had an open ask.")
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	runPermissionFamily(t, s, []permissionRow{
		{
			prompt: "!perm-deny-user", answer: "deny", standing: true, verdict: "deniedByUser", toolRan: false,
			open: "A PENDING permission card with THREE buttons: `allow once`, `always allow`, `deny`.",
			settled: "The card RESOLVED with an ERROR-colored badge reading `denied by user`. The gated tool " +
				"call beneath it is drawn DENIED (not failed), and the turn still concluded: the agent " +
				"routed around the denial.",
		},
		{
			prompt: "!perm-deny-policy", verdict: "deniedByPolicy", toolRan: false,
			settled: "A SECOND permission card that was NEVER open: no buttons, an ERROR-colored badge whose " +
				"wording is the daemon's own policy sentence (it names the deny rule, and must NOT read " +
				"`denied by user`). The gated call beneath it is drawn denied; the turn concluded.",
		},
		{
			prompt: "!perm-undecidable", verdict: "deniedUndecidable", toolRan: false,
			settled: "A THIRD permission card, also never open, whose ERROR-colored badge reads `denied for " +
				"want of a decider` followed by the vendor's own detail that the classifier could not reach " +
				"a verdict. It must read as NOBODY's decision: neither `denied by user` nor a rule.",
		},
	})
}

// ---------------------------------------------------------------------------
// E39: the parked ask across a workspace switch
// ---------------------------------------------------------------------------

// TestPlaytestPermissionHoldSurvivesASwitch is plan E.39: an open ask stays
// open across a switch to another workspace and back.
//
// `!perm-hold` parks however its ask resolves, so the ask is open for as long
// as this playbook wants it; the switch is `agent-repl-switch-to-project`,
// the same verb owner 3 drives, and the panel is re-opened on each side so
// the webview photographed is the SELECTED workspace's own.
func TestPlaytestPermissionHoldSurvivesASwitch(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "12-perm-hold",
		"Plan E.39. `!perm-hold`: the ask stays open across a switch to another workspace and back.")
	p, e := s.Book, s.E

	first := s.repoAt(t, "repo-asking")
	askingName := s.register(t, first.Dir)
	s.openPanel(t)
	second := s.repoAt(t, "repo-other")
	otherName := s.register(t, second.Dir)
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == otherName })
	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the asking workspace to be the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == askingName })
	s.Name = askingName
	s.openPanel(t)
	p.note("two repositories registered and the first re-selected with its panel open",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q and its webview is live", askingName))

	const openCard = `document.querySelector('[data-feed-row][data-row-kind="permission"] .permission[data-state="open"]')`
	s.submit(t, "!perm-hold")
	s.awaitInPageFor(t, playtestAskBound, "the parked ask's card to be drawn open", openCard+` !== null`)
	s.awaitArm(t, askingName, "the tab's arm to say the turn is in flight", emGHIRunningArms...)
	p.capture("hold-open", "`!perm-hold` submitted; its ask is open and the turn parks on it",
		"the feed carries a `.permission[data-state=\"open\"]` card and the roster arm is a running one",
		fmt.Sprintf("The feed of %q carries a PENDING permission card for a `sleep 600` Bash call with its three "+
			"buttons and a ticking `waiting` clock. The tab bar shows both workspaces with %q selected.", askingName, askingName))

	e.Eval(`(agent-repl-switch-to-project ` + elispString(second.Dir) + `)`)
	e.AwaitEval("the other workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == otherName })
	s.Name = otherName
	s.openPanel(t)
	s.awaitInPage(t, "the other workspace's feed to carry no permission card",
		`document.querySelectorAll('[data-feed-row][data-row-kind="permission"]').length === 0`)
	p.capture("hold-switched-away", "`agent-repl-switch-to-project` to the other workspace, and its panel opened",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q and its page carries NO permission row", otherName),
		fmt.Sprintf("The webview is %q's own: an EMPTY feed with no card in it. The tab bar shows %q selected, "+
			"and %q — which still holds the open ask — is the other tab.", otherName, otherName, askingName))

	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the asking workspace to be the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == askingName })
	s.Name = askingName
	s.openPanel(t)
	s.awaitInPage(t, "the parked ask to still be drawn open", openCard+` !== null`)
	if arm, _ := s.armPaint(t, askingName); !containsString(emGHIRunningArms, arm) {
		t.Fatalf("the asking workspace's arm is %s after the switch back, want a running one: the park did not survive", arm)
	}
	p.capture("hold-switched-back", "switched back to the asking workspace, and its panel opened",
		"the card is still `.permission[data-state=\"open\"]` and the roster arm is still a running one",
		fmt.Sprintf("The SAME pending permission card as the first picture, still with its three buttons and its "+
			"clock now further along; nothing answered it. %q is selected again.", askingName))

	// Teardown hygiene, not an assertion: a parked ask must not outlive the
	// playbook, or the world's own shutdown waits on an answer nobody will
	// ever give.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(askingName) + `) t)`)
}

// ---------------------------------------------------------------------------
// E40: questions
// ---------------------------------------------------------------------------

// questionRow is one row of the question family.
type questionRow struct {
	prompt string
	// mode is the `data-question-mode` the first question block must carry.
	mode string
	// options is how many `[data-question-option]` inputs the first block draws.
	options int
	// blocks is how many questions the batch carries.
	blocks int
	// answer performs the user's answer inside the card; nil leaves it
	// unanswered.
	answer func(t *testing.T, s *playtestScenario, sel string)
	// settled is how the card settles: "answered" draws one `.q-verdict` per
	// question (none carries a `data-arm`), "expired" draws one
	// `.q-verdict[data-arm="expired"]`.
	settledArm string
	// open and settled are the manifest sentences.
	open, settled string
}

// TestPlaytestQuestionFamily is plan E.40: each question card's shape and
// its answered state.
func TestPlaytestQuestionFamily(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "12-questions",
		"Plan E.40. `!ask-single`, `!ask-multi`, `!ask-free`, `!ask-unanswered`: each question card's "+
			"shape, and its answered or expired state.")
	p := s.Book
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	pick := func(t *testing.T, s *playtestScenario, sel, block, label string) {
		t.Helper()
		s.clickWithin(t, "the option "+label, sel+`.querySelector('[data-question="`+block+`"] [data-question-option="`+label+`"]')`)
	}
	submit := func(t *testing.T, s *playtestScenario, sel string) {
		t.Helper()
		s.clickWithin(t, "the question card's submit button", sel+`.querySelector('[data-question-submit]')`)
	}

	rows := []questionRow{
		{
			prompt: "!ask-single", mode: "singleSelect", options: 4, blocks: 1,
			answer: func(t *testing.T, s *playtestScenario, sel string) {
				pick(t, s, sel, "0", "New worktree off master")
				submit(t, s, sel)
			},
			settledArm: "answered",
			open: "A question card with the chip `Setup`, the question `How do you want the new branch set up?`, " +
				"FOUR radio options (`New worktree off master`, `Switch this checkout`, `Reuse the existing " +
				"branch`, `Do not branch`) each with a description under it, a `something else` text field, " +
				"and a submit button.",
			settled: "The same card RESOLVED: the options are gone and one verdict line carries the chip `Setup` " +
				"and the chosen label `New worktree off master`. The turn concluded beneath it.",
		},
		{
			prompt: "!ask-multi", mode: "multiSelect", options: 3, blocks: 2,
			answer: func(t *testing.T, s *playtestScenario, sel string) {
				pick(t, s, sel, "0", "Unit")
				pick(t, s, sel, "0", "Integration")
				pick(t, s, sel, "1", "Now")
				submit(t, s, sel)
			},
			settledArm: "answered",
			open: "A SECOND question card carrying TWO question blocks: `Suites` with `Which suites should run?` " +
				"and THREE checkboxes (`Unit`, `Integration`, `Elisp`), then `When` with `Run them now?` and " +
				"TWO radios (`Now`, `After the merge`). One submit button for the batch.",
			settled: "The second card RESOLVED with TWO verdict lines in order: `Suites` with `Unit` and " +
				"`Integration`, then `When` with `Now`.",
		},
		{
			prompt: "!ask-free", mode: "singleSelect", options: 2, blocks: 1,
			answer: func(t *testing.T, s *playtestScenario, sel string) {
				// Typed, not picked: the free-text field is read at submit time,
				// so setting its value is what typing into it leaves behind.
				s.awaitInPage(t, "the free-text field to take the typed answer",
					`(function () { var f = `+sel+`.querySelector('[data-question-other]');
					                if (!f) { return false; }
					                f.value = "Whichever one is cheapest right now."; return true; })()`)
				submit(t, s, sel)
			},
			settledArm: "answered",
			open: "A THIRD question card, chip `Model`, `Which model should the sweep use?`, TWO radios " +
				"(`Fake Opus`, `Fake Haiku`) and the `something else` field.",
			settled: "The third card RESOLVED with a verdict line carrying the chip `Model` and the TYPED text " +
				"`Whichever one is cheapest right now.` rather than either listed label.",
		},
		{
			prompt: "!ask-unanswered", mode: "singleSelect", options: 2, blocks: 1,
			settledArm: "expired",
			open: "A FOURTH question card, `Should I keep going?`, with its options and submit button: " +
				"nothing has been clicked.",
			settled: "The fourth card RESOLVED WITHOUT AN ANSWER: a muted badge says the question went " +
				"unanswered. Beneath it the turn CONCLUDED with the agent's own prose (`Nobody answered " +
				"the question.`) rather than reading interrupted -- the denial the teardown handed the " +
				"gate is what the agent concluded on, which is the terminal the vendor's own recording " +
				"of this scenario carries.",
		},
	}

	for i, row := range rows {
		sel := questionRowSel(i)
		s.submit(t, row.prompt)
		s.awaitInPageFor(t, playtestAskBound, "the question card to be drawn open",
			sel+` && `+sel+`.querySelector('[data-state="open"]') !== null`)
		s.awaitInPage(t, "the card's shape to be the scenario's",
			`(`+sel+`.querySelectorAll('[data-question]').length === `+fmt.Sprint(row.blocks)+`) && `+
				`(`+sel+`.querySelector('[data-question="0"]').getAttribute('data-question-mode') === "`+row.mode+`") && `+
				`(`+sel+`.querySelector('[data-question="0"]').querySelectorAll('[data-question-option]').length === `+fmt.Sprint(row.options)+`) && `+
				`(`+sel+`.querySelector('[data-question-submit]') !== null)`)
		s.awaitArm(t, s.Name, "the tab's arm to say the turn is waiting on the question", emGHIRunningArms...)
		text := s.readInPage(t, "the question text", sel+`.querySelector('.q-text').textContent`)
		p.capture(strings.TrimPrefix(row.prompt, "!")+"-open",
			fmt.Sprintf("`%s` submitted with composer RET", row.prompt),
			fmt.Sprintf("the card is open with %d block(s), the first `%s` with %d options, asking %q; the roster arm is a running one",
				row.blocks, row.mode, row.options, text),
			row.open)

		act := "the answer picked on the card and its submit button clicked"
		var arm string
		if row.answer != nil {
			row.answer(t, s, sel)
			// TWO STEPS, NOT A CONJUNCTION, and each guarded against the row
			// being momentarily absent: a single predicate that could fail
			// three ways reports none of them, which is how a settled card
			// that read right in the page still timed out here once.
			s.awaitInPageFor(t, playtestAskBound, "the question card to draw its verdict line(s)",
				sel+` && `+sel+`.querySelectorAll('.q-verdict').length === `+fmt.Sprint(row.blocks))
			s.awaitInPageFor(t, playtestAskBound, "the question card to read answered",
				sel+` && `+sel+`.querySelector('[data-state="answered"]') !== null`)
			arm = s.awaitArm(t, s.Name, "the turn to conclude", emGHISettledArms...)
		} else {
			// NOBODY ANSWERS, so the ask can only be released by a teardown,
			// and Emacs's one interrupting act is a FORCED restart
			// (EMACS-LAYER-SPEC.md area G, "There is no interrupt command").
			//
			// THE TURN THEN CONCLUDES RATHER THAN READING INTERRUPTED, and
			// that is this scenario's own shape rather than a defect. The
			// teardown resolves the pending gate as DENIED (shim.md,
			// "PERMISSION-CALLBACK LIVENESS"); the fake's `ask-unanswered`
			// takes that denial as the batch ending unanswered and CONCLUDES
			// with "Nobody answered the question."; and the vendor's own
			// captured recording for this scenario ends `success.completed`
			// too (shim `testdata/captures/MANIFEST.md`, row
			// `question-unanswered`). So what is asserted is that the arm
			// SETTLES, and the arm it settles on is read and written into the
			// manifest rather than guessed at -- which is the open question
			// recorded above `TestQuestionUnanswered` in
			// `questions_e2e_test.go` resolving, measured here as `:done`.
			act = "nothing answered; the turn torn down through `agent-repl-restart-workspace` with FORCE"
			s.E.Eval(`(agent-repl-restart-workspace t ` + elispString(s.Name) + `)`)
			arm = s.awaitArm(t, s.Name, "the roster arm to settle after the teardown", emGHISettledArms...)
			s.awaitInPageFor(t, playtestAskBound, "the question card to settle expired",
				sel+`.querySelector('.q-verdict[data-arm="expired"]') !== null`)
		}
		verdicts := s.readInPage(t, "the verdict lines",
			`Array.prototype.map.call(`+sel+`.querySelectorAll('.q-verdict'), function (v) { return v.textContent; }).join(" || ")`)
		p.capture(strings.TrimPrefix(row.prompt, "!")+"-settled", act,
			fmt.Sprintf("the card settled %s with %d `.q-verdict` line(s) reading %q, and the roster arm settled %s", row.settledArm, row.blocks, verdicts, arm),
			row.settled)
	}
}

// ---------------------------------------------------------------------------
// E41: the topbar's permission-mode picker
// ---------------------------------------------------------------------------

// TestPlaytestPermissionModePicker is plan E.41: the picker changed from the
// topbar, and the mode it then shows.
//
// The topbar draws only once the session has something to say about the
// account and the model, which is after the first turn on a cold workspace
// (see `awaitPageMounted`), so one plain turn runs first.
func TestPlaytestPermissionModePicker(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "12-mode-picker",
		"Plan E.41. The topbar's permission-mode picker: opened, a mode picked, and the picker then "+
			"reading the mode the daemon restated.")
	p := s.Book
	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	s.submit(t, "one plain prose turn so the topbar draws")
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	const button = `document.querySelector('.topbar-mode-button')`
	s.awaitInPage(t, "the topbar's mode button to read the default mode", button+` && `+button+`.getAttribute("data-mode") === "default"`)
	label := s.readInPage(t, "the mode button's label", button+`.textContent`)
	p.capture("mode-default", "one prose turn concluded, so the topbar is drawn",
		fmt.Sprintf("`.topbar-mode-button[data-mode=\"default\"]` is drawn, labeled %q", label),
		"The topbar carries a permission-mode button reading `default` beside the model selector. No "+
			"options list is open.")

	// THE BUTTON IS WHAT A READER CLICKS. The wrap (`.topbar-mode`) carries the
	// toggle and the reveal anchor, and the button is inside it, so a click on
	// the button reaches the toggle by bubbling -- which is the path a real
	// pointer takes and the one the webapp layer's own model-selector scenario
	// drives.
	s.clickInPage(t, "the mode button", ".topbar-mode-button")
	// The reveal layer, the panel and the options are each their own step, so a
	// failure names WHICH of them did not happen instead of reporting a
	// conjunction that could have failed three ways.
	t.Logf("after the click: %s reveal layer(s), %s open reveal(s), %s reveal anchor(s), %s mode option(s)",
		s.readInPage(t, "the reveal layer count", `document.querySelectorAll('.topbar-reveal-layer').length`),
		s.readInPage(t, "the open reveal count", `document.querySelectorAll('[data-reveal]').length`),
		s.readInPage(t, "the reveal anchor count", `document.querySelectorAll('[data-reveal-anchor]').length`),
		s.readInPage(t, "the mode option count", `document.querySelectorAll('[data-mode-option]').length`))
	s.awaitInPage(t, "the mode reveal to open", `document.querySelector('[data-reveal="mode"]') !== null`)
	s.awaitInPage(t, "the mode reveal to carry the six served modes",
		`document.querySelectorAll('[data-reveal="mode"] [data-mode-option]').length === 6`)
	options := s.readInPage(t, "the served modes",
		`Array.prototype.map.call(document.querySelectorAll('[data-reveal="mode"] [data-mode-option]'), function (o) { return o.getAttribute("data-mode-option"); }).join(",")`)
	p.capture("mode-picker-open", "the mode button clicked",
		fmt.Sprintf("the `[data-reveal=\"mode\"]` panel is open carrying the six served modes: %s", options),
		"Under the topbar's mode button an options list is open with SIX rows in this order: `default`, "+
			"`accept edits`, `plan`, `bypass`, `dont ask`, `auto`.")

	s.clickInPage(t, "the plan option", `[data-reveal="mode"] [data-mode-option="plan"]`)
	// THE NEW MODE ARRIVES ON THE PUSHED TOPBAR, not in the response: the
	// picker reading `plan` is the daemon's own restatement drawn.
	s.awaitInPageFor(t, playtestAskBound, "the mode button to read the picked mode", button+`.getAttribute("data-mode") === "plan"`)
	s.awaitInPage(t, "the reveal to be closed after the pick", `document.querySelector('[data-reveal="mode"]') === null`)
	if got := s.readInPage(t, "the mode button's label", button+`.textContent`); got != "plan" {
		t.Fatalf("the mode button reads %q after picking plan, want %q", got, "plan")
	}
	p.capture("mode-picked-plan", "the `plan` option clicked",
		"`.topbar-mode-button[data-mode=\"plan\"]` is drawn labeled `plan`, and the reveal is closed",
		"The topbar's permission-mode button now reads `plan`, and the options list is closed. Nothing "+
			"else on the page changed.")
}
