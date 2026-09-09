//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"testing"
)

// OWNER 7 of PLAYTEST-PLAN.md's partition: C19-C21 -- the composer and
// delivery. Type and submit, focus and discard; a prompt the DAEMON holds
// behind a live turn and the tray that shows it; a prompt EMACS defers and
// drains on the finish edge.
//
// Two different holds are photographed here, and they are not the same
// thing. C20's held prompt is the daemon's: the composer submitted it while
// a turn was running, the daemon took it and parked it, and the webapp's
// hold tray draws what the daemon is holding. C21's deferred prompt is
// Emacs's own: `agent-repl-queue-deferred-prompt` keeps it in
// `agent-repl--prompt-queue` and nothing reaches the daemon until the
// roster's finish edge releases it. EMACS-LAYER-SPEC.md's readback table
// names the queue, never the tray, as where Emacs's held prompts are read
// from -- and the daemon's are read from the tray because Emacs never held
// them at all.

// playtestSelectedBufferForm reads the name of the buffer in the SELECTED
// window, which is what "focused" means to a user.
const playtestSelectedBufferForm = `(buffer-name (window-buffer (selected-window)))`

// userPromptRowWith answers whether the root feed carries a user-prompt row
// whose text includes TEXT.
func userPromptRowWith(text string) string {
	return `Array.prototype.some.call(
                document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]'),
                function (row) { return row.textContent.indexOf(` + jsString(text) + `) !== -1; })`
}

// heldCardWith answers whether the hold tray carries a held-prompt card
// whose text includes TEXT.
func heldCardWith(text string) string {
	return `Array.prototype.some.call(
                document.querySelectorAll('[data-component="hold-tray"] [data-held-turn]'),
                function (card) { return card.textContent.indexOf(` + jsString(text) + `) !== -1; })`
}

// settledResponseWith answers whether the root feed carries a SETTLED
// response bubble whose prose includes TEXT.
//
// A TURN DRAWS SEVERAL RESPONSE ROWS, NOT ONE, so a count of
// `[data-unit="response"]` rows counts the vendor's block structure rather
// than the turns that settled: the fake SDK's default prose turn
// (`fake/scenarios/prose.ts`) emits TWO text blocks -- "Here is what I
// found." and the turn's conclusion -- and the daemon folds each block into
// its own activity row, so one finished turn leaves TWO settled response
// bubbles behind. Naming the TEXT is what makes the check a claim about one
// turn: `feed-view.ts` mirrors the bubble's own `data-state` onto the row
// chrome, so `success` on the row carrying that text is the drawn bubble
// saying it settled.
func settledResponseWith(text string) string {
	return `Array.prototype.some.call(
                document.querySelectorAll('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]'),
                function (row) { return row.textContent.indexOf(` + jsString(text) + `) !== -1; })`
}

// settledEchoFor is the settled response bubble THIS prompt's turn concluded
// with. The fake's prose scenario ends every turn with `echo: <the prompt
// verbatim>` and the daemon's success frame restates it whole, so the
// conclusion identifies the turn the way `userPromptRowWith` identifies the
// prompt -- a bubble left over from any other turn could not satisfy it.
func settledEchoFor(prompt string) string {
	return settledResponseWith("echo: " + prompt)
}

// awaitComposerCleared waits for the daemon's acceptance to clear the
// composer, which is the acceptance's own visible act in Emacs.
func (s *playtestScenario) awaitComposerCleared(t *testing.T) {
	t.Helper()
	s.E.AwaitEval("the daemon's acceptance to clear the composer",
		`(with-current-buffer `+elispString(s.Input)+` (buffer-string))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "" })
}

// ---------------------------------------------------------------------------
// C19. type, submit, prompt bubble; SPC o v focuses; discard clears
// ---------------------------------------------------------------------------

// TestPlaytestComposerSubmitFocusDiscard is plan C.19.
func TestPlaytestComposerSubmitFocusDiscard(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "07-composer",
		"Plan C.19. A prompt typed into the composer and submitted with RET draws as the user's "+
			"own bubble in the webview; `SPC o v` moves focus into the composer from elsewhere; "+
			"`agent-repl-discard-input` empties a draft without submitting it.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	// TYPE AND SUBMIT. The row is looked for BY ITS TEXT, so a bubble left
	// over from some other prompt could not satisfy this.
	const prompt = "show me this prompt as my own bubble"
	s.submit(t, prompt)
	s.awaitInPage(t, "the user's prompt bubble carrying the typed text to arrive on the standing tail",
		userPromptRowWith(prompt))
	s.awaitComposerCleared(t)
	s.awaitInPage(t, "the assistant's response bubble for this prompt to settle beneath it",
		settledEchoFor(prompt))
	s.awaitArm(t, s.Name, "the turn to settle", emGHISettledArms...)
	p.capture("prompt-bubble", fmt.Sprintf("%q typed into the composer and submitted with composer RET", prompt),
		"a `[data-row-kind=\"userPrompt\"]` row carrying that exact text is in the root feed, the composer "+
			"emptied on the daemon's acceptance, a settled `[data-unit=\"response\"]` row carries this turn's "+
			"conclusion, and the roster arm settled",
		fmt.Sprintf("The feed carries the user's own prompt bubble reading %q, and beneath it the assistant's "+
			"response bubbles with prose in them -- the turn's opening line and its conclusion. The composer "+
			"window beside the webview is EMPTY: the typed text moved from the composer into the bubble.", prompt))

	// SPC o v FROM SOMEWHERE ELSE. Focus is moved OUT of the composer first
	// -- into the webview's window -- because `agent-repl-focus-input` from
	// inside the composer is the jump-back branch, which would prove nothing
	// about focusing it.
	if want, got := "agent-repl-focus-input", e.LeaderBinding("o v"); got != want {
		t.Fatalf("SPC o v resolves to %q, want %q", got, want)
	}
	webview := e.EvalString(`(agent-repl--frontend-webview-buffer-name ` + elispString(s.Name) + `)`)
	e.Eval(`(let ((win (get-buffer-window ` + elispString(webview) + `)))
              (unless win (error "the webview %s has no window" ` + elispString(webview) + `))
              (select-window win)
              t)`)
	if got := e.EvalString(playtestSelectedBufferForm); got != webview {
		t.Fatalf("the selected window shows %q before SPC o v, want the webview %q", got, webview)
	}
	e.Eval(`(progn (call-interactively #'agent-repl-focus-input) t)`)
	if got := e.EvalString(playtestSelectedBufferForm); got != s.Input {
		t.Fatalf("after SPC o v the selected window shows %q, want the composer %q", got, s.Input)
	}
	p.note("focus moved into the webview's window, then `SPC o v` (`agent-repl-focus-input`) invoked",
		fmt.Sprintf("the selected window's buffer is the composer %q", s.Input))

	// DISCARD. The binding is the composer's own `C-c C-c`, pressed in the
	// composer, and the negative half -- nothing was submitted -- is read
	// off the feed: the row count of user prompts is still one.
	const draft = "this draft is going nowhere"
	typeIntoComposer(e, s.Input, draft)
	if got := composerText(e, s.Input); got != draft {
		t.Fatalf("the composer holds %q after typing, want %q", got, draft)
	}
	if want, got := "agent-repl-discard-input", e.BindingForIn(s.Input, "C-c C-c"); got != want {
		t.Fatalf("composer C-c C-c resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "C-c C-c")
	if got := composerText(e, s.Input); got != "" {
		t.Fatalf("the composer still holds %q after a discard, want it empty", got)
	}
	// `agent-repl-discard-input` is synchronous, so a submission it made
	// would already have gone out; the feed's user-prompt count is the
	// daemon's word that it did not.
	s.awaitInPage(t, "the feed to still carry exactly one user prompt after the discard",
		`document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]').length === 1`)
	p.note(fmt.Sprintf("%q typed into the composer, then `C-c C-c` (`agent-repl-discard-input`) pressed in it", draft),
		"the composer is empty and the feed still carries exactly one user prompt, so the draft was discarded and not submitted")
}

// ---------------------------------------------------------------------------
// C20. a held prompt while a turn is live: the tray; discard from the tray
// ---------------------------------------------------------------------------

// TestPlaytestHeldPromptInTray is plan C.20.
//
// The live turn is lifecycle.ts's `!hold`, which parks until interrupted,
// so the second submission is PROVABLY held behind it rather than merely
// fast. The hold is the daemon's (daemon.md: "a prompt submitted while a
// turn runs is HELD daemon-side"), so it is read from the tray the webapp
// draws from `WatchDaemonHolds` and NOT from `agent-repl--prompt-queue`,
// which never sees it.
func TestPlaytestHeldPromptInTray(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "07-held-prompt",
		"Plan C.20. A prompt submitted while a `!hold` turn is live is held by the daemon and drawn "+
			"in the hold tray; dropping it from the tray removes it.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)

	s.submit(t, "!hold")
	s.awaitArm(t, s.Name, "the `!hold` turn to be in flight", emGHIRunningArms...)
	s.awaitInPage(t, "the `!hold` prompt bubble to arrive", userPromptRowWith("!hold"))
	p.note("`!hold` submitted with composer RET",
		"the roster arm is one of the module's running arms and the `!hold` prompt bubble is in the feed, so a turn is genuinely live")

	// THE SECOND PROMPT. Nothing in Emacs gates it -- `agent-repl-send`
	// sends regardless of the arm -- so it reaches the daemon and the daemon
	// holds it.
	const held = "a second prompt held behind the running turn"
	s.submit(t, held)
	s.awaitComposerCleared(t)
	s.awaitInPage(t, "the hold tray to draw a held-prompt card carrying the second prompt's text", heldCardWith(held))
	if got := e.EvalInt(emGHIHeldPromptsForm(s.Name)); got != 0 {
		t.Fatalf("Emacs's own prompt queue holds %d prompts, want 0: the hold is the daemon's, not Emacs's", got)
	}
	s.awaitInPage(t, "the held prompt to NOT be in the feed as a user prompt",
		`!(`+userPromptRowWith(held)+`)`)
	p.capture("held-in-tray", fmt.Sprintf("%q submitted with composer RET while the `!hold` turn is live", held),
		"a `[data-held-turn]` card in `[data-component=\"hold-tray\"]` carries that text, the composer emptied on "+
			"acceptance, no user-prompt row carries it, and `agent-repl--prompt-queue` holds nothing",
		fmt.Sprintf("The feed carries the `!hold` prompt bubble, and beneath the conversation the HOLD TRAY draws "+
			"one parked card reading %q -- subdued, with a dashed border, visibly NOT a conversation bubble -- "+
			"with Release and Drop controls. There is no second user bubble in the feed.", held))

	// DROP FROM THE TRAY. The card leaves on the daemon's next push; the
	// tray's own empty marker is what says the hold is gone.
	s.clickInPage(t, "the held card's Drop button", `[data-component="hold-tray"] [data-held-turn] [data-held-action="drop"]`)
	s.awaitInPage(t, "the hold tray to be empty after the drop",
		`document.querySelector('[data-component="hold-tray"] .hold-tray-empty[data-empty]') !== null`)
	s.awaitInPage(t, "the dropped prompt to still not be in the feed", `!(`+userPromptRowWith(held)+`)`)
	p.capture("tray-emptied", "the held card's Drop control clicked",
		"the tray draws its `.hold-tray-empty[data-empty]` marker and no user-prompt row carries the dropped text",
		"The hold tray beneath the conversation reads \"nothing held\" and carries NO card. The feed still "+
			"carries the `!hold` prompt bubble and no bubble for the dropped prompt.")

	// Teardown hygiene, not an assertion: the parked `!hold` turn must not
	// outlive the playbook.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(s.Name) + `) t)`)
}

// ---------------------------------------------------------------------------
// C21. a deferred prompt drains on the finish edge
// ---------------------------------------------------------------------------

// TestPlaytestDeferredPromptDrains is plan C.21.
//
// The enqueue must happen WHILE the turn is running, and a Go-side poll
// cannot hit that window without racing the turn's end. So, exactly as the
// Emacs layer's own scenario 24 does, the enqueue is armed INSIDE Emacs on
// `agent-repl-roster-update-functions`: it fires on the first push showing
// a running arm, types into the composer and invokes the ordinary
// `agent-repl-queue-deferred-prompt` command (`SPC j RET`).
func TestPlaytestDeferredPromptDrains(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "07-deferred-drain",
		"Plan C.21. A prompt deferred with `SPC j RET` while a turn runs is held in Emacs's own "+
			"queue and drained as its own turn on the roster's finish edge.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	installArmObserver(t, e)

	if want, got := "agent-repl-queue-deferred-prompt", e.LeaderBinding("j RET"); got != want {
		t.Fatalf("SPC j RET resolves to %q, want %q", got, want)
	}

	const first = "hello from the deferred-prompt playbook"
	const deferred = "and then tell me about the roster"
	e.Eval(`(progn
             (defvar agent-repl-playtest--queued-depth nil)
             (setq agent-repl-playtest--queued-depth nil)
             (defun agent-repl-playtest--queue-when-running (_roster)
               (when (and (null agent-repl-playtest--queued-depth)
                          (memq (agent-repl-e2e--arm-of ` + elispString(s.Name) + `)
                                agent-repl-roster-running-statuses))
                 (with-current-buffer (agent-repl--input-buffer ` + elispString(s.Name) + `)
                   (erase-buffer)
                   (insert ` + elispString(deferred) + `)
                   (call-interactively #'agent-repl-queue-deferred-prompt))
                 (setq agent-repl-playtest--queued-depth
                       (length (agent-repl-prompt-queue-pending ` + elispString(s.Name) + ` :deferred)))))
             (add-hook 'agent-repl-roster-update-functions
                       #'agent-repl-playtest--queue-when-running)
             t)`)

	s.submit(t, first)
	e.AwaitTrue("the deferred prompt to be queued during the running turn",
		`agent-repl-playtest--queued-depth`)
	if depth := e.EvalInt(`(or agent-repl-playtest--queued-depth 0)`); depth != 1 {
		t.Fatalf("the queue held %d deferred prompts mid-turn, want 1", depth)
	}
	p.note(fmt.Sprintf("%q submitted with composer RET; on the first running push, %q typed and `SPC j RET` (`agent-repl-queue-deferred-prompt`) invoked", first, deferred),
		"`agent-repl-prompt-queue-pending` held exactly one :deferred entry, read inside Emacs the instant the command returned")

	// THE FINISH EDGE DRAINS IT: Emacs's queue empties, and the held text
	// then goes out as its own turn and draws as its own bubble.
	e.AwaitEval("the deferred prompt queue to drain on the finish edge",
		`(length (agent-repl-prompt-queue-pending `+elispString(s.Name)+` :deferred))`,
		func(raw json.RawMessage) bool { return string(raw) == "0" })
	s.awaitInPage(t, "the drained prompt to arrive as its own user bubble", userPromptRowWith(deferred))
	s.awaitInPage(t, "the first turn's response bubble to have settled", settledEchoFor(first))
	s.awaitInPage(t, "the drained turn's own response bubble to settle", settledEchoFor(deferred))
	s.awaitArm(t, s.Name, "the drained turn to settle", emGHISettledArms...)
	s.awaitInPage(t, "the feed to carry exactly two user prompts",
		`document.querySelectorAll('[data-feed-row][data-row-kind="userPrompt"]').length === 2`)
	p.capture("feed-after-drain", "the first turn finished, which is the edge the queue drains on",
		"`agent-repl--prompt-queue` is empty, a `[data-row-kind=\"userPrompt\"]` row carries the deferred text, "+
			"the feed holds exactly two user prompts, and EACH turn's conclusion is carried by a settled "+
			"`[data-unit=\"response\"]` row, and the arm settled",
		fmt.Sprintf("The feed carries both exchanges in order: the user's prompt %q with the assistant's "+
			"response beneath it, then the user's deferred prompt %q with its own response beneath that. "+
			"The hold tray beneath them reads \"nothing held\", and the composer is empty.", first, deferred))
}
