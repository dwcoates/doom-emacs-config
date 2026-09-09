//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strconv"
	"testing"

	"claude-repld/integration/harness"
)

// THE PLAYBOOKS.
//
// PLAYTEST-SPEC.md is the design and playtest_capture_test.go is the
// mechanism. Each test here is one SCRIPTED SEQUENCE OF USER ACTS against
// the real application -- real Doom, a daemon Emacs spawned through its own
// launcher, the real shim, store and sidecar, and the fake SDK as the only
// vendor -- with a picture taken after every act and a manifest sentence
// saying what that picture must show.
//
// IT IS NOT A REPLACEMENT FOR THE E2E ASSERTIONS AND DOES NOT TRY TO BE.
// Every wait here is on the same fact the Emacs layer's own scenarios wait
// on, and the mechanical assertions are only that a capture exists, is the
// declared geometry, and is not blank. WHAT the picture shows is read by a
// human, which is the whole reason the pictures exist: the tab bar's paint
// is an SVG and the webapp is inside a WebKit view, and neither is
// something a Go assertion can look at.
//
// THE TAG IS THE POINT. These run only under `-tags playtest`, so the
// ordinary `go test ./e2e` never starts one: a playbook takes an Emacs slot
// for its whole length and writes files a human then has to read, which is
// not work a merge gate should be doing.

// playtestPageBound bounds one wait on something being DRAWN inside the
// webview.
//
// It is `emacsTurnBound`, deliberately and not by accident of reuse: every
// one of these waits is "a fake-SDK turn reached the daemon, the daemon
// pushed a frame, and the webapp drew it", which is that bound's own subject
// plus one render. The fake vendor answers without network or model
// latency, so nothing here is waiting on a model.
const playtestPageBound = emacsTurnBound

// playtestProbeSetup installs the page probe.
//
// `xwidget-webkit-execute-script` is ASYNCHRONOUS -- it hands the script to
// WebKit and answers a callback later -- so a probe cannot be one eval. It
// is two: each call issues the script again and answers what the PREVIOUS
// issue's callback stored, and the Go side polls it through the layer's own
// `AwaitEval`. That converges on the truth within one poll and never sleeps.
//
// The stored answer is reset before each new question, so a wait can never
// be satisfied by the answer to the previous one.
const playtestProbeSetup = `(progn
             (defvar agent-repl-playtest--js nil)
             (defun agent-repl-playtest--probe (ws script)
               (let* ((buf (get-buffer (agent-repl--frontend-webview-buffer-name ws)))
                      (xw (and buf (agent-repl--frontend-webview-live-widget buf))))
                 (unless xw (error "no live webview for %s" ws))
                 (xwidget-webkit-execute-script
                  xw script
                  (lambda (value) (setq agent-repl-playtest--js (format "%s" value))))
                 agent-repl-playtest--js))
             t)`

// pageYes wraps a JavaScript expression so the probe's answer is one of two
// words. A predicate that throws answers "no" rather than wedging the wait
// on a callback WebKit will never make.
func pageYes(expression string) string {
	return `(function () { try { return (` + expression + `) ? "yes" : "no"; }
                           catch (e) { return "no"; } })()`
}

// ---------------------------------------------------------------------------
// THE SHARED ARRANGEMENT
// ---------------------------------------------------------------------------

// playtestScenario is one playbook's world: a booted Emacs on a full-screen
// frame, a daemon it launched itself, and the playbook that photographs it.
type playtestScenario struct {
	Book  *playbook
	World *EmacsWorld
	E     *Emacs
	Box   sandbox

	// Name and Input are the first registered workspace and its composer,
	// once `register` has run.
	Name  string
	Input string
}

// newPlaytestScenario brings Emacs up, sizes the frame, and spawns the
// daemon through the module's own launcher -- and stops there, because the
// first thing several playbooks want a picture of is an editor with nothing
// registered in it yet.
func newPlaytestScenario(t *testing.T, name, purpose string, options ...EmacsWorldOption) *playtestScenario {
	t.Helper()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, options...)
	e := w.Emacs

	book := newPlaybook(t, e, name, purpose)
	// THE FRAME IS SIZED BEFORE ANYTHING IS DRAWN IN IT. Emacs takes a
	// default frame far smaller than the screen, and the panel's webview is
	// laid out in real pixels -- so a frame resized after the webview
	// existed would photograph a webapp that had been laid out for a
	// different window.
	book.prepareFrame()
	e.EnsureDaemon()

	return &playtestScenario{Book: book, World: w, E: e, Box: box}
}

// register registers one scripted fake-git worktree through the ordinary
// command and records the composer it materializes.
func (s *playtestScenario) register(t *testing.T, dir string) string {
	t.Helper()
	name := addProjectWorkspace(t, s.E, dir)
	if s.Name == "" {
		s.Name = name
	}
	return name
}

// openPanel opens the panel and waits until the WEBAPP HAS DRAWN, not
// merely until the buffers exist.
//
// The webapp exposes no readiness flag of its own -- there is no
// `data-ready`, no global -- so the signal is the one the webapp layer's own
// suite uses: the footer's status word is empty until the daemon's
// `WatchFooter` push has arrived and been rendered, so a non-empty one means
// the page is live against this daemon.
func (s *playtestScenario) openPanel(t *testing.T) {
	t.Helper()
	s.E.Eval(`(agent-repl-frontend-open-panel)`)
	s.Input = awaitInputBuffer(t, s.E, s.Name)
	s.E.AwaitEvalFor(playtestPageBound, "the panel's webview to be live",
		`(let* ((buf (get-buffer (agent-repl--frontend-webview-buffer-name `+elispString(s.Name)+`)))
                (xw (and buf (agent-repl--frontend-webview-live-widget buf))))
           (and xw (xwidget-webkit-uri xw)))`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	s.E.Eval(playtestProbeSetup)
	s.awaitInPage(t, "the webapp to draw its footer, which means the daemon's push arrived",
		`document.querySelector(".footer-status") &&
         document.querySelector(".footer-status").textContent.trim() !== ""`)
}

// submit types a prompt into the composer and PRESSES RET, which is how a
// user submits.
func (s *playtestScenario) submit(t *testing.T, text string) {
	t.Helper()
	typeIntoComposer(s.E, s.Input, text)
	if want, got := "agent-repl-send", s.E.BindingForIn(s.Input, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	s.E.KeysIn(s.Input, "RET")
}

// awaitInPage waits until a JavaScript predicate holds inside the webview.
func (s *playtestScenario) awaitInPage(t *testing.T, what, expression string) {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(playtestPageBound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(pageYes(expression))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// clickInPage clicks one element inside the webview, the way a user does,
// and refuses loudly if the selector names nothing.
func (s *playtestScenario) clickInPage(t *testing.T, what, selector string) {
	t.Helper()
	s.awaitInPage(t, what+" to be there to click", `document.querySelector(`+jsString(selector)+`)`)
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(playtestPageBound, "the click on "+what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+
			elispString(pageYes(`(function () { var el = document.querySelector(`+jsString(selector)+`);
                                                if (!el) { return false; }
                                                el.click();
                                                return true; })()`))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// jsString renders a Go string as a JavaScript string literal. The selectors
// here carry double quotes, so single quotes are the delimiter and the two
// characters that could still end the literal are escaped.
func jsString(s string) string {
	out := make([]rune, 0, len(s)+2)
	out = append(out, '\'')
	for _, r := range s {
		if r == '\'' || r == '\\' {
			out = append(out, '\\')
		}
		out = append(out, r)
	}
	return string(append(out, '\''))
}

// awaitSettled waits for this workspace's roster arm to reach a settled arm,
// which is the finish edge every one of Emacs's reactions rides.
func (s *playtestScenario) awaitSettled(t *testing.T, what string) {
	t.Helper()
	emGHIAwaitStatus(t, s.E, s.Name, what, emGHISettledArms...)
}

// ---------------------------------------------------------------------------
// A. COLD START, THE FIRST TURN, AND THE WINDOW LAYOUT
// ---------------------------------------------------------------------------

// TestPlaytestColdStartAndFirstTurn walks the shortest path a new user
// takes: an editor, a workspace, a panel, a prompt, an answer -- and then
// the one window act that changes the whole frame.
func TestPlaytestColdStartAndFirstTurn(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "cold-start",
		"The shortest path a user takes: boot, register a repository, open the panel, "+
			"submit one plain-prose prompt, read the answer, and toggle fullscreen.")
	p, e := s.Book, s.E

	p.capture("doom-booted", "Emacs booted the image's real Doom and spawned the daemon through its own launcher",
		"An Emacs frame filling the whole screen. Doom is up, so the frame is themed rather than default-grey. "+
			"NOTHING agent-repl is registered yet, so the tab bar carries no workspace tab.")

	repository := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))
	s.register(t, repository.Dir)

	p.capture("workspace-registered", "`agent-repl-add-project-workspace` on a scripted fake-git worktree",
		"The tab bar now carries EXACTLY ONE workspace tab, named after the repository, "+
			"with its roster arm painted beside the name as a small colored disc. "+
			"The arm is a settled one, so the disc is green -- not the red band of a running turn.")

	s.openPanel(t)
	p.capture("panel-open", "`agent-repl-frontend-open-panel`",
		"The frame is split: the WEBAPP is drawn inside the panel window -- workspace sidebar down "+
			"one side, topbar across the top, an empty feed, and the progress footer along the bottom "+
			"with a status word in it. A separate small Emacs window holds the composer. "+
			"THE WEBAPP MUST NOT BE A BLANK WHITE RECTANGLE.")

	const prompt = "draw one plain prose answer for the playtest"
	typeIntoComposer(e, s.Input, prompt)
	p.capture("prompt-typed", "the prompt typed into the composer buffer, not yet sent",
		"The composer window carries the typed text verbatim. The feed is still empty: nothing was sent.")

	if want, got := "agent-repl-send", e.BindingForIn(s.Input, "RET"); got != want {
		t.Fatalf("composer RET resolves to %q, want %q", got, want)
	}
	e.KeysIn(s.Input, "RET")
	s.awaitInPage(t, "the user's own prompt bubble to be drawn",
		`document.querySelector('[data-feed-row][data-row-kind="userPrompt"]')`)
	s.awaitInPage(t, "the assistant's response bubble to settle",
		`document.querySelector('[data-feed-row][data-row-kind="activity"][data-unit="response"][data-state="success"]')`)
	s.awaitSettled(t, "the turn to settle after the response")

	p.capture("turn-answered", "RET pressed in the composer, and the fake SDK's plain-prose answer drawn",
		"The feed carries TWO bubbles in order: the user's own prompt bubble with the text typed at "+
			"step 04, and beneath it the assistant's response bubble with prose in it. "+
			"The tab bar's arm is settled again and the footer is no longer running a turn.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitTrue("the fullscreen configuration to be recorded",
		`(and agent-repl--window-fullscreen-config t)`)
	p.capture("fullscreen", "`agent-repl-fullscreen-and-focus` (`SPC w f`)",
		"ONE window fills the whole frame. The webapp is drawn edge to edge and the composer window is gone.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitEval("the fullscreen configuration to be released",
		`(and agent-repl--window-fullscreen-config t)`,
		func(raw json.RawMessage) bool { return isJSONNull(raw) })
	p.capture("fullscreen-restored", "the same command again, restoring the layout",
		"The split of step 05 is back, with the same two bubbles still in the feed: "+
			"the toggle restored the layout rather than rebuilding it.")
}

// ---------------------------------------------------------------------------
// B. A PERMISSION ASK, DRAWN, ANSWERED AND SETTLED
// ---------------------------------------------------------------------------

// TestPlaytestPermissionAsk photographs the ask card through its whole life.
//
// EMACS ANSWERS NOTHING, by contract: there is no permission-answering
// command in `lisp/`, and the card in the webapp is the answering surface.
// So the answer is a click on the card, which is exactly what a user does.
func TestPlaytestPermissionAsk(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "permission",
		"A permission ask drawn as a card, answered from the card the way a user answers it, "+
			"and the settled verdict pushed back onto the same row.")
	p, e := s.Book, s.E

	repository := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))
	s.register(t, repository.Dir)
	s.openPanel(t)

	// `!perm-hold` raises an ask and leaves it outstanding, so the card is
	// guaranteed to be standing when the picture is taken rather than raced.
	s.submit(t, "!perm-hold")
	s.awaitInPage(t, "the permission card to be waiting",
		`document.querySelector('[data-feed-row][data-row-kind="permission"] .perm-waiting')`)
	p.capture("ask-waiting", "the fake SDK's `!perm-hold` scenario raised an ask that stays outstanding",
		"The feed carries a PERMISSION CARD: the tool it wants to run, a reason line, and its "+
			"action buttons (Allow once / Allow / Deny), with the card visibly WAITING for an answer. "+
			"The workspace's tab-bar arm is a running one, and the footer says a turn is in flight.")

	s.clickInPage(t, "the card's Allow-once button",
		`[data-feed-row][data-row-kind="permission"] [data-permission="allowOnce"]`)
	s.awaitInPage(t, "the answered verdict to be pushed back onto the same row",
		`document.querySelector('[data-feed-row][data-row-kind="permission"] .perm-verdict')`)
	p.capture("ask-answered", "the Allow-once button clicked in the card, as a user clicks it",
		"THE SAME ROW now shows a settled verdict instead of buttons: the action buttons are gone "+
			"and the card reads as answered. No second permission card appeared.")

	s.awaitSettled(t, "the turn to settle once the ask was answered")
	p.capture("turn-settled", "the turn ran on and finished",
		"The turn has finished: the tab-bar arm is settled (green) rather than running, and the "+
			"footer is idle. The answered permission card is still in the feed as history.")
}

// ---------------------------------------------------------------------------
// C. A SUBAGENT'S NESTED SUB-FEED, AND A DETACHED SHELL
// ---------------------------------------------------------------------------

// TestPlaytestSubagentAndDetachedShell photographs the two feed families
// that are containers rather than bubbles: a subagent, whose own activity
// lives in a nested feed the user opens, and a background shell, which is
// live and then settled.
func TestPlaytestSubagentAndDetachedShell(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "subagent-and-shell",
		"A subagent bubble with its nested sub-feed opened, and a detached shell row live and "+
			"then settled.")
	p, e := s.Book, s.E

	repository := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))
	s.register(t, repository.Dir)
	s.openPanel(t)

	const subagentRow = `[data-feed-row][data-row-kind="activity"][data-unit="subagent"]`
	s.submit(t, "!subagent")
	s.awaitInPage(t, "the subagent bubble to be drawn", `document.querySelector('`+subagentRow+`')`)
	p.capture("subagent-bubble", "the fake SDK's `!subagent` scenario ran a synchronous subagent",
		"The feed carries a SUBAGENT BUBBLE whose head names the commission the agent issued, "+
			"with a caret to open it. Its own activity is NOT on the top-level feed: the bubble is "+
			"closed, so nothing of the subagent's work is visible yet.")

	s.clickInPage(t, "the subagent bubble's caret", subagentRow+` [data-expand]`)
	s.awaitInPage(t, "the nested sub-feed to be open",
		`document.querySelector('`+subagentRow+`[data-expanded="true"]')`)
	p.capture("subagent-subfeed-open", "the caret clicked, opening the subagent's nested sub-feed",
		"The SAME bubble is now open and a NESTED FEED is drawn inside it, indented under the head, "+
			"carrying the subagent's own commission prompt and its activity. The top-level feed above "+
			"and below it is unchanged.")

	s.awaitSettled(t, "the subagent turn to settle")

	// A shell that never settles, so the live shape is photographable rather
	// than raced against its own completion.
	s.submit(t, "!bash-detach-live")
	s.awaitInPage(t, "a live detached shell row to be drawn",
		`document.querySelector('[data-feed-row][data-row-kind="detachedShell"][data-state="live"]')`)
	p.capture("shell-live", "the fake SDK's `!bash-detach-live` scenario backgrounded a shell that keeps running",
		"The feed carries a DETACHED SHELL row that is visibly LIVE: a running dot, a clock counting "+
			"the run, and a control to stop it. It carries no exit code, because it has not exited.")

	s.submit(t, "!bash-detach")
	s.awaitInPage(t, "a settled detached shell row to be drawn",
		`document.querySelector('[data-feed-row][data-row-kind="detachedShell"][data-state="completed"]')`)
	p.capture("shell-settled", "the `!bash-detach` scenario backgrounded a shell that completes",
		"A SECOND detached shell row, this one SETTLED: it names its exit code and reads as completed "+
			"rather than running. The live row from step 03 is still live above it, so the two shapes "+
			"are side by side in one picture.")
}

// ---------------------------------------------------------------------------
// D. A TURN THAT DIES, AND AN ALLOWANCE THAT IS SPENT
// ---------------------------------------------------------------------------

// TestPlaytestQueryDeathAndAllowance photographs the two ways a session goes
// wrong without anything being broken locally: the vendor's own query dying
// under a turn, and a rate-limit window the account has spent.
func TestPlaytestQueryDeathAndAllowance(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "query-death-and-allowance",
		"A turn whose vendor query dies, and the footer's allowance line after a rate-limit event.")
	p, e := s.Book, s.E

	repository := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))
	s.register(t, repository.Dir)
	s.openPanel(t)

	// THE FAILURE OVERLAY IS NOT THIS. `[data-component="failure-overlay"]`
	// is for client-local failures -- the daemon unreachable, a stale
	// bundle, a frame that would not decode. A vendor query dying under a
	// turn is a TURN-ENDED row carrying the cause, which is what this waits
	// on and what the picture must show.
	s.submit(t, "!query-eof")
	s.awaitInPage(t, "the turn-ended row naming the query's death",
		`document.querySelector('[data-feed-row][data-row-kind="turnEnded"] [data-turn-error="queryDied"]')`)
	p.capture("query-died", "the fake SDK's `!query-eof` scenario ended the vendor query with an unexpected EOF",
		"The feed's last row says the TURN ENDED IN ERROR because the vendor's query DIED -- the "+
			"cause named as an unexpected end of the stream, not as a generic failure. It is drawn "+
			"in the vendor's purple, not the local-environment blue: nothing on this machine broke.")

	s.awaitSettled(t, "the workspace to settle after the query death")

	// The allowance line is drawn while the footer's activity is the
	// rate-limited arm, so the wait is on the line itself rather than on the
	// turn, and the picture is taken the moment it is up.
	s.submit(t, "!rate-limit-five-hour")
	s.awaitInPage(t, "the footer's session allowance line",
		`document.querySelector('.footer-allowance[data-allowance="session"]')`)
	p.capture("allowance", "the fake SDK's `!rate-limit-five-hour` scenario reported a spent five-hour window",
		"The PROGRESS FOOTER carries an ALLOWANCE LINE for the five-hour session window: how much of "+
			"it is spent and when it resets. It is in the footer's own strip, NOT in the expanded "+
			"footer, which carries the agent and task roster and nothing else.")

	s.awaitSettled(t, "the workspace to settle after the rate-limit turn")
}

// ---------------------------------------------------------------------------
// E. THE TAB BAR WITH TWO WORKSPACES, AND A SCHEDULED DRAIN
// ---------------------------------------------------------------------------

// TestPlaytestTabBarAndDrain photographs the surface a Connect client cannot
// see at all -- Emacs's own tab bar -- and the standing banner a scheduled
// shutdown puts on both the mode line and the webapp.
func TestPlaytestTabBarAndDrain(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "tab-bar-and-drain",
		"Two workspaces on the tab bar with the right arm painted on each, and the standing drain "+
			"banner a scheduled shutdown raises.")
	p, e := s.Book, s.E

	first := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo-first"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(first.Dir, ".claude"))
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	p.capture("one-workspace", "one repository registered and its panel open",
		"The tab bar carries EXACTLY ONE workspace tab, and it is the selected one: its name is "+
			"highlighted and its arm is settled.")

	second := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), "repo-second"))
	e.ArtifactPaths = append(e.ArtifactPaths, filepath.Join(second.Dir, ".claude"))
	secondName := s.register(t, second.Dir)
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	names := e.EvalStrings(emacsWSTablineNamesForm)
	if len(names) != 2 {
		t.Fatalf("the tab bar draws %v, want both workspaces", names)
	}
	p.capture("two-workspaces", "a second repository registered through the same command",
		fmt.Sprintf("The tab bar carries TWO workspace tabs, %q then %q, in the roster's own order. "+
			"The SECOND is the selected one -- registering selects it -- and only that one is "+
			"highlighted. Both arms are settled discs.", firstName, secondName))

	// A turn that parks in flight, so the running arm is photographable
	// rather than raced against its own completion.
	s.Name, s.Input = secondName, awaitInputBuffer(t, e, secondName)
	s.submit(t, holdScenario)
	emGHIAwaitStatus(t, e, secondName, "the held turn to be in flight", emGHIRunningArms...)
	p.capture("running-arm", "a turn submitted on the selected workspace, parked in flight by the fake SDK",
		fmt.Sprintf("The tab for %q now paints a RUNNING arm -- the red band -- while %q keeps its "+
			"settled green disc. Two workspaces, two different arms, in one tab bar.",
			secondName, firstName))

	// The held turn is released before the drain: a drain announced over a
	// turn nobody will ever end would make the world's own teardown wait on
	// an interrupt that is not coming.
	e.Eval(`(ignore-errors (agent-repl-kill-workspace ` + elispString(secondName) + `) t)`)

	// Five minutes out, so the daemon is still serving for the rest of the
	// test and its teardown. `agent-repl-daemon-shutdown-schedule` prompts
	// for its reason, so the reader is bound for the duration of the one
	// call -- the standard ERT way, which keeps the command running its own
	// argument collection.
	const drainMinutes = 5
	const drainReason = "maintenance"
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(drainReason) + `)))
              (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(drainMinutes) + `)
              t)`)
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs", `(and agent-repl-link-drain t)`)
	p.capture("drain-scheduled", "`agent-repl-daemon-shutdown-schedule` five minutes out, reason \"maintenance\"",
		"A STANDING DRAIN BANNER is up in two places at once: Emacs's mode line reads "+
			"`drain HH:MM · maintenance`, and the webapp draws its own drain banner across the top "+
			"of the panel. The tab bar is otherwise unchanged.")
}
