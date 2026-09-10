//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// THE PLAYBOOK SUBSTRATE, shared by every owner's playbook files.
//
// The playbooks themselves live one file per PLAYTEST-PLAN.md owner,
// `playtest_NN_<subject>_test.go`, and nothing in them is shared with
// another owner except what is here.
//
// PLAYTEST-PLAN.md is the owner's plan, PLAYTEST-SPEC.md is how this layer
// implements it, and playtest_capture_test.go is the capture mechanism. Each
// test here is one SCRIPTED SEQUENCE OF USER ACTS against the real
// application -- real Doom, a daemon Emacs spawned through its own launcher,
// the real shim, store and sidecar, and the fake SDK as the only vendor.
//
// FUNCTIONAL FIRST, PICTURES SECOND, which is the plan's own rule:
//
//   - EVERY step carries a programmatic assertion, and every one of them is
//     an assertion the Emacs layer's own scenarios already make -- the roster
//     arm awaits, the panel-window await, the composer binding check, the
//     registry readback. Nothing new is invented to assert with, and a step
//     that cannot even start fails RIGHT THERE with the elisp error text,
//     the `*Messages*` tail and the world's logs attached, which is the
//     harness's ordinary failure path.
//   - A CAPTURE IS OPTIONAL, taken only where the step's subject is VISUAL
//     -- the tab bar's painted state and the webapp's rendering -- and only
//     AFTER that step's assertion passed. A reviewer is never handed a
//     picture of a world that was already broken.
//
// So a playbook is a sequence of the same waits the e2e tests already use,
// with a `capture` call on the visual ones and a `note` on the rest.
//
// EVERY PLAYBOOK IS ITS OWN WORLD and shares nothing: its own Emacs on its
// own Xvfb, its own daemon/shim/store/sidecar, its own scratch root, its own
// sockets, its own artifact directory. The only ordering constraint is the
// layer's own Emacs slot gate, which `NewEmacsWorld` takes.
//
// THE TAG IS THE POINT. These run only under `-tags playtest`, so the
// ordinary `go test ./e2e` never starts one.

// playtestPageBound bounds one wait on something being DRAWN inside the
// webview.
//
// MEASURED, over the runs that produced the captures in `.playtest-out`: the
// webapp's own first draw (the footer's status word appearing, which is the
// daemon's first push rendered) took 242ms, 262ms, 423ms, 445ms and 487ms.
// 2s is roughly four times the worst of those, and the extra margin over a
// 3x rule is deliberate: this covers a WebKit view starting its own web and
// network processes on a container's first scenario, which is the case a
// bound exists to tolerate.
const playtestPageBound = 2 * time.Second

// playtestProbeSetup installs the page probe.
//
// `xwidget-webkit-execute-script` is ASYNCHRONOUS -- it hands the script to
// WebKit and answers a callback later -- so a probe cannot be one eval. It
// is two: each call issues the script again and answers what the PREVIOUS
// issue's callback stored, and the Go side polls it through the layer's own
// `AwaitEval`. That converges within one poll and never sleeps.
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
// words, and so a "no" CARRIES ITS OWN DIAGNOSIS.
//
// A wait that fails prints its last value, so making that value say what the
// page actually held is what turns "never satisfied" into a finding. It is
// how the boot defect this playtest found was diagnosed: the page's url,
// its row count and its failure cards, read off the last unsatisfied probe.
func pageYes(expression string) string {
	return `(function () {
                   try { if (` + expression + `) { return "yes"; } }
                   catch (e) { return "no: the predicate threw " + e; }
                   var overlay = document.querySelector('[data-component="failure-overlay"]');
                   var arms = overlay ? Array.prototype.map.call(
                     overlay.querySelectorAll(".failure-card"),
                     function (c) { return c.getAttribute("data-arm"); }).join(",") : "<no overlay>";
                   return "no: url=" + location.href +
                          " rows=" + document.querySelectorAll("[data-feed-row]").length +
                          " failureArms=[" + arms + "]" +
                          " text=" + (document.body ? document.body.innerText.slice(0, 200) : "<no body>");
                 })()`
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

	// Name and Input are the workspace the playbook is acting on and its
	// composer buffer. A playbook with several workspaces re-points them.
	Name  string
	Input string
}

// newPlaytestScenario brings Emacs up, sizes the frame, and spawns the
// daemon through the module's own launcher -- and stops there, because the
// first thing several playbooks want a picture of is an editor with nothing
// registered in it yet.
func newPlaytestScenario(t *testing.T, name, purpose string, options ...EmacsWorldOption) *playtestScenario {
	t.Helper()
	s := newColdPlaytestScenario(t, name, purpose, options...)
	s.ensureDaemon(t)
	return s
}

// newColdPlaytestScenario is newPlaytestScenario WITHOUT the daemon: Emacs
// is up and the frame is sized, and no launcher has run yet.
//
// It exists for the playbooks whose subject IS the launch -- section A's
// cold start and build failure -- which must arrange the launcher (a build
// script that fails, an address file that is absent) before it runs, and
// must observe the editor from the instant before it does. Every other
// playbook wants the daemon up and takes newPlaytestScenario.
func newColdPlaytestScenario(t *testing.T, name, purpose string, options ...EmacsWorldOption) *playtestScenario {
	t.Helper()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, options...)
	e := w.Emacs

	book := newPlaybook(t, e, name, purpose)
	// THE FRAME IS SIZED BEFORE ANYTHING IS DRAWN IN IT. Emacs takes a
	// default frame far smaller than the screen, and the panel's webview is
	// laid out in real pixels -- so a frame resized after the webview
	// existed would photograph a webapp laid out for a different window.
	book.prepareFrame()

	return &playtestScenario{Book: book, World: w, E: e, Box: box}
}

// ensureDaemon runs the module's own cold-start ensure and records it.
//
// THE COLD-START LAUNCH IS AN ASSERTION, not arrangement: `EnsureDaemon`
// waits for `agent-repl-link--primary`, so a launcher that composes a wrong
// argv fails here rather than somewhere downstream.
func (s *playtestScenario) ensureDaemon(t *testing.T) {
	t.Helper()
	s.E.EnsureDaemon()
	s.Book.note("Emacs spawned the daemon through its own launcher (`agent-repl-frontend-daemon-ensure`)",
		"Emacs holds a live link: `agent-repl-link--primary` is non-nil")
}

// repoAt mints one scripted fake-git worktree under this world's own
// scratch. NO REAL GIT RUNS ANYWHERE: `harness.NewRepoAt` scripts the fake
// git the world installed ahead of Emacs on PATH.
func (s *playtestScenario) repoAt(t *testing.T, name string) *harness.Repo {
	t.Helper()
	repository := harness.NewRepoAt(t, filepath.Join(s.Box.Scratch(), name))
	s.E.ArtifactPaths = append(s.E.ArtifactPaths, filepath.Join(repository.Dir, ".claude"))
	return repository
}

// register registers one worktree through the ordinary command, asserting
// the binding the user reaches it by first.
func (s *playtestScenario) register(t *testing.T, dir string) string {
	t.Helper()
	// `SPC TAB C-n` prompts for the directory, so the binding is a LOOKUP and
	// the command is then invoked with its argument -- the standard way this
	// layer drives a prompting verb.
	if want, got := "agent-repl-add-project-workspace", s.E.LeaderBinding("TAB C-n"); got != want {
		t.Fatalf("SPC TAB C-n resolves to %q, want %q", got, want)
	}
	name := addProjectWorkspace(t, s.E, dir)
	if s.Name == "" {
		s.Name = name
	}
	return name
}

// openPanel opens the panel and waits until the WEBAPP HAS DRAWN, not merely
// until the buffers exist.
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
	s.awaitPageMounted(t)
	// EVERY CAPTURE FROM HERE ON WAITS FOR THE PAGE'S OWN FRAMES. See
	// playtestPaintFrames: an xwidget webview reaches the glass only when
	// Emacs copies its offscreen surface, so a capture taken before WebKit
	// produced a frame photographs the previous page state under an
	// assertion that legitimately passed. The playbook's hook is installed
	// here because here is where a page first exists to paint.
	//
	// AND IT ANSWERS NOTHING WHEN THERE IS NO PAGE. The hook is installed
	// once and fires on every LATER capture, including captures of a frame
	// this workspace's panel has since been taken off -- `SPC o C` hides both
	// panels by its own docstring, and A.8's deprio capture is a picture of
	// exactly that. There are no frames to wait for when no webview is live,
	// and waiting for them fails a step whose subject is their absence.
	s.Book.awaitPaint = func() time.Duration {
		if !s.webviewIsLive(t) {
			return 0
		}
		return s.awaitPagePainted(t)
	}
	// AND EVERY CAPTURE FROM HERE ON HOLDS THE PAGE'S RESTING ANIMATIONS
	// STILL WHILE IT FIRES. See playtestMotionPaused: the webapp's prompt
	// bubbles, state dots and footer breath animate forever by design, and a
	// capture cannot wait out a screen that never stops changing. Installed
	// here, beside the paint gate, because here is where a page first exists
	// to hold.
	//
	// AND IT HOLDS NOTHING WHEN THERE IS NO PAGE, for the reason the paint
	// gate answers nothing: a hidden panel has no resting animations to still.
	s.Book.holdMotion = func() func() {
		if !s.webviewIsLive(t) {
			return func() {}
		}
		return s.holdPageMotion(t)
	}
}

// webviewIsLive answers whether THIS SCENARIO'S CURRENT WORKSPACE has a live
// webview widget right now.
//
// IT IS A READ, NOT A WAIT, and that is the whole point: it is asked at the
// instant of a capture to decide whether there is a page to talk to at all,
// so a wait here would turn "there is deliberately no panel" into a two
// second stall and then a failure. The form is the one `openPanel` waits on,
// so the two cannot disagree about what "live" means.
func (s *playtestScenario) webviewIsLive(t *testing.T) bool {
	t.Helper()
	return s.E.EvalBool(`(let* ((buf (get-buffer (agent-repl--frontend-webview-buffer-name ` +
		elispString(s.Name) + `)))
                              (xw (and buf (agent-repl--frontend-webview-live-widget buf))))
                         (and xw (window-live-p (get-buffer-window buf)) t))`)
}

// holdPageMotion freezes the page's resting animations and answers the
// release.
//
// BOTH HALVES ARE WAITED ON. `xwidget-webkit-execute-script` is
// asynchronous, so a script that was merely ISSUED has not necessarily run:
// a capture that photographed before the flag landed would settle against a
// still-animating page, and a playbook that moved on before the release
// landed would leave every later capture in it frozen. So each half asserts
// the attribute it just wrote, through the same probe every other page act
// here goes through.
func (s *playtestScenario) holdPageMotion(t *testing.T) func() {
	t.Helper()
	s.awaitInPage(t, "the page's resting animations to be held still for the capture",
		playtestHoldMotionScript())
	return func() {
		s.awaitInPage(t, "the page's resting animations to be released after the capture",
			playtestReleaseMotionScript())
	}
}

// playtestPaintBound bounds one wait on the page delivering the frames the
// capture needs.
//
// MEASURED, over the seven captures of owner 7's three playbooks and the
// substrate's own, in each of three consecutive runs -- twenty-one gates in
// all. Every one answered between 44 and 144ms, mean 76ms, and none took
// two polls more than the one before it. It is bounded by playtestPageBound
// -- 14x the worst of those -- rather than by a tighter number of its own,
// because the case it must tolerate is the same one that bound exists for:
// a WebKit view that has just started its web and network processes on a
// container's first scenario. A page whose compositor produces NO frame in
// that long is a page whose picture would be a lie, so running this bound
// out is the right failure rather than a wait to widen.
const playtestPaintBound = playtestPageBound

// playtestPaintToken numbers the paint requests within one page, so a
// capture's wait can never be satisfied by the frames an earlier capture
// asked for.
var playtestPaintToken atomic.Uint64

// awaitPagePainted blocks until the webview has delivered
// playtestPaintFrames animation frames raised after this call, and answers
// how long that took.
//
// A WORKSPACE WITH NO LIVE WEBVIEW IS A REAL ANSWER, not a swallowed error.
// A playbook may photograph the frame after its panel is gone, and there is
// then no page whose paint could be waited for -- so the form below reports
// that case as its own word and the wait accepts it, rather than the probe
// raising "no live webview" and failing a capture that has nothing to do
// with a page. Every other failure still fails.
func (s *playtestScenario) awaitPagePainted(t *testing.T) time.Duration {
	t.Helper()
	started := time.Now()
	token := fmt.Sprintf("capture-%d", playtestPaintToken.Add(1))
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(playtestPaintBound, "the webview to deliver its own frames for what the DOM now holds",
		`(let* ((buf (get-buffer (agent-repl--frontend-webview-buffer-name `+elispString(s.Name)+`)))
                (xw (and buf (agent-repl--frontend-webview-live-widget buf))))
           (if (not xw)
               "no-webview"
             (agent-repl-playtest--probe `+elispString(s.Name)+` `+
			elispString(playtestPaintGateScript(token))+`)))`,
		func(raw json.RawMessage) bool {
			answer := decodeString(raw)
			return answer == "yes" || answer == "no-webview"
		})
	return time.Since(started)
}

// awaitPageMounted is the boot assertion every playbook makes before it
// looks at anything, and it exists because a URI is not a page.
//
// `TestEmacsProofOfLife` asserts that the panel's WKWebView was created and
// NAVIGATED -- it reads the widget's own `xwidget-webkit-uri` -- and that is
// all it asserts. A page that loads its bundle, throws out of `boot` and
// renders NOTHING satisfies that completely, which is exactly what happened:
// the webapp could not boot at all for as long as `mountFailureOverlay` ran
// before `setLogger`, and every xwidget scenario on that tree was looking at
// an empty document while reporting on the product.
//
// So this asserts the page MOUNTED, in three claims that fail apart:
//
//   - the feed controller drew its host. `mountFeed` runs after the login
//     overlay, the sidebar and the topbar, so `[data-feed="root"]` existing
//     means the boot got past every mount before it -- one cheap check
//     standing for the whole sequence.
//   - the footer carries a status word, which is empty until the daemon's
//     own push has arrived AND been rendered. That is the page being live
//     against THIS daemon rather than merely having run its own code.
//   - the failure overlay is EMPTY. A page that filed `bootFailed` has
//     already told us why it is blank, and a wait that timed out instead
//     would bury that answer.
//
// The topbar is deliberately NOT in here even though it is a mount too:
// MEASURED, it draws only once the session has something to say about the
// account and the model, which is after the first turn on a cold workspace,
// so requiring it here would fail every playbook that photographs an idle
// editor.
func (s *playtestScenario) awaitPageMounted(t *testing.T) {
	t.Helper()
	s.awaitInPage(t, "the webapp's feed host to be mounted, which means the boot got past every mount before it",
		`document.querySelector('[data-feed="root"]') !== null`)
	s.awaitInPage(t, "the webapp to draw its footer status, which means the daemon's push arrived and was rendered",
		`document.querySelector(".footer-status") &&
         document.querySelector(".footer-status").textContent.trim() !== ""`)
	s.awaitInPage(t, "the failure overlay to be carrying nothing",
		`document.querySelector('[data-component="failure-overlay"]').hasAttribute("data-empty")`)
}

// submit types a prompt into the composer and PRESSES RET, which is how a
// user submits. The binding is asserted rather than assumed: a RET that
// resolved to anything else would submit nothing and fail a wait five
// seconds later with no cause attached.
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
	s.awaitInPageFor(t, playtestPageBound, what, expression)
}

// awaitInPageFor is awaitInPage with an explicit bound, for a page act that
// is not a redraw. Per SPEC.md section B and the module AGENTS.md, a per-site
// bound is a NAMED constant with a stated reason, never a duration written
// at the call site -- so this takes one rather than a number.
func (s *playtestScenario) awaitInPageFor(t *testing.T, bound time.Duration, what, expression string) {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(bound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(pageYes(expression))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// awaitTailClearsFooter waits until the feed's tail is actually on screen,
// clear of the docked progress footer.
//
// PHOTOGRAPHED FIRST, ASSERTED SECOND. Plan B.15's `arm-revived` picture
// caught the last bubble sliced off by the footer strip in two runs of three,
// and a picture is the only thing that catches it: nothing about the arm, the
// feed's rows or the daemon's view is wrong when it happens. The occlusion is
// a SCROLL POSITION, not a layout — `#footer` is a flex sibling laid out below
// `#feed-scroll`, so the box's height already excludes it, and a footer that
// appears or grows AFTER the render that parked the tail shrinks the box under
// a scrollTop nobody moved. The tail then sits that many pixels below the fold
// and the fold is the strip's top edge.
//
// So the claim is exactly two numbers: nothing of the feed is left below the
// fold, and the fold is above the footer. Both come off the live boxes, so
// this says what the camera saw rather than what the markup intended.
//
// The 2px slack is subpixel layout, not tolerance for being wrong: browsers
// round fractional heights and a box parked at its bottom can read a hair
// short of it.
func (s *playtestScenario) awaitTailClearsFooter(t *testing.T) {
	t.Helper()
	s.awaitInPage(t, "the feed's last row to sit clear of the progress footer",
		`(function () {
                   var box = document.getElementById('feed-scroll');
                   var strip = document.querySelector('#footer .pfooter');
                   if (!box || !strip) { return false; }
                   var below = box.scrollHeight - box.scrollTop - box.clientHeight;
                   var overlap = box.getBoundingClientRect().bottom - strip.getBoundingClientRect().top;
                   return below <= 2 && overlap <= 0; })()`)
}

// clickInPage clicks one element inside the webview, the way a user does.
//
// The element is WAITED FOR first and the click's own answer is waited on,
// so a selector that names nothing fails as "nothing to click" rather than
// as whatever assertion came next.
func (s *playtestScenario) clickInPage(t *testing.T, what, selector string) {
	t.Helper()
	s.awaitInPage(t, what+" to be there to click", `document.querySelector(`+jsString(selector)+`)`)
	s.clickOnce(t, what, `document.querySelector(`+jsString(selector)+`)`)
}

// clickOnce issues ELEMENTEXPR's click through the probe and waits for its
// answer, clicking EXACTLY ONCE however many polls the answer takes.
func (s *playtestScenario) clickOnce(t *testing.T, what, elementExpr string) {
	t.Helper()
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(playtestPageBound, "the click on "+what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+
			elispString(pageYes(pageClickOnce(elementExpr)))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// playtestClickSeq mints one token per CLICK SITE, so two clicks on the same
// element are two distinct acts rather than one remembered one.
var playtestClickSeq atomic.Uint64

// pageClickOnce wraps ELEMENTEXPR's click so re-issuing the script cannot
// click twice.
//
// WHY, AND IT IS A MEASURED DEFECT RATHER THAN A PRECAUTION. The probe is two
// evals (`playtestProbeSetup`): every poll RE-ISSUES the script and answers
// what the previous issue's callback stored. A read-only predicate does not
// care how often it runs; a click does. Measured in the permission-mode
// picker's own playbook, from the page's own client log: the mode reveal
// OPENED at 02:14:42.173 and CLOSED at 02:14:42.195 with no topbar push in
// between -- the second poll had clicked the toggle again. Whether a toggle
// ended open or closed therefore depended on how many polls the answer took,
// which is a flake in every playbook that clicks a toggle, a checkbox or a
// submit button.
//
// The token is written into the page only ONCE THE CLICK ACTUALLY HAPPENED,
// so an issue that found nothing to click still clicks when the element
// arrives.
func pageClickOnce(elementExpr string) string {
	token := fmt.Sprintf("playtest-click-%d", playtestClickSeq.Add(1))
	return `(function () {
                   var done = window.__playtestClicked || (window.__playtestClicked = {});
                   if (done[` + jsString(token) + `]) { return true; }
                   var el = ` + elementExpr + `;
                   if (!el) { return false; }
                   done[` + jsString(token) + `] = true;
                   el.click();
                   return true;
                 })()`
}

// tabNames reads the names the tab bar DRAWS, in roster order.
//
// NOT `tab-bar-tabs`: `status.el` paints the bar from `tab-bar-format`, so
// Emacs's built-in tabs are window configurations named after whatever
// buffer they hold. `agent-repl--ws-tabline-names` is the enumeration the
// renderer itself walks, which is what "the tab bar's names" means here.
func (s *playtestScenario) tabNames() []string {
	s.E.t.Helper()
	return s.E.EvalStrings(emacsWSTablineNamesForm)
}

// awaitArm waits for a workspace's roster arm to be one of ARMS and answers
// the one it settled on. It is the Emacs layer's own helper, unchanged: the
// roster's arm vocabulary is the ONE source for the tab's color, so this
// reads the paint DECISION rather than the paint.
func (s *playtestScenario) awaitArm(t *testing.T, ws, what string, arms ...string) string {
	t.Helper()
	return emGHIAwaitStatus(t, s.E, ws, what, arms...)
}

// armPaint answers a workspace's arm and the COLOR NAME the module decided
// for it, read at the instant it is asked.
//
// IT IS RE-READ AT CAPTURE TIME, AND THAT IS THE WHOLE REASON IT EXISTS. An
// arm is a transient: `awaitArm` is satisfied by the first push carrying the
// arm it wanted, and the row may have moved on by the time the picture is
// taken. A manifest sentence written from the AWAITED arm would then tell a
// reviewer to expect a color the product had already correctly stopped
// painting, and the reviewer would file the harness's own race as a defect.
//
// The color comes from `agent-repl-status-tab-bar-color-table` -- the
// module's own derived table, `agent-repl-status-color-table` with
// `agent-repl-status-tab-bar-color-overrides` layered over it -- so the
// sentence a reviewer checks is the module's own decision rather than this
// file's guess at it.
//
// THE TAB-BAR TABLE, NOT THE SHARED ONE, and the difference is a real one
// every caller of `captureArm` depends on. What is photographed is the TAB
// BAR, and the tab bar declares its own overrides: the three IN-FLIGHT merge
// arms and `:vendor-blocked` are painted differently there. Reading the
// shared table wrote "carries NO arm color at all" into the manifest for a
// merging tab the product paints PURPLE -- a sentence that sends a reviewer
// to file the harness's own mistake as a defect in the paint. The tab-bar
// table is DERIVED from the shared one, so it can never disagree about which
// arms EXIST, only about the four colors it explicitly overrides.
func (s *playtestScenario) armPaint(t *testing.T, ws string) (arm, color string) {
	t.Helper()
	pair := s.E.EvalStrings(`(let* ((arm (agent-repl-roster-status-for-ws ` + elispString(ws) + `))
                                     (color (cdr (assq arm agent-repl-status-tab-bar-color-table))))
                                (list (format "%s" arm) (format "%s" (or color "<no color-table entry>"))))`)
	if len(pair) != 2 {
		t.Fatalf("reading the arm and color for %s answered %v, want an arm and a color", ws, pair)
	}
	return pair[0], pair[1]
}

// tabFaceFor answers the FACE the module actually put on this workspace's
// name in the string `tab-bar-format` renders.
//
// WHY THIS EXISTS BESIDE `armPaint`. `armPaint` reads the DECISION -- the
// arm, and the color the module's own table gives it. This reads what the
// module then WROTE. The two answer different questions, and when a picture
// disagrees with the manifest they are what says which half is wrong: a face
// that matches the arm means the paint was produced correctly and the
// display did not show it, and a face that does not means the string was
// wrong before any redisplay was involved.
//
// It reads `agent-repl-workspace-tabline-formatted`, which is the function
// installed in `tab-bar-format` and therefore the one that drives the
// VISIBLE bar -- not the Doom-API path `agent-repl--tabline-advice` serves.
func (s *playtestScenario) tabFaceFor(t *testing.T, ws string) string {
	t.Helper()
	// EVERY distinct face in the drawn line, not the one at the name's first
	// character: the module puts the state color on the tab's own bracket and
	// background while the NAME may carry Doom's selected-tab face, so reading
	// one position answers the wrong question.
	return s.E.EvalString(`(let* ((line (agent-repl-workspace-tabline-formatted))
                                  (seen nil)
                                  (i 0)
                                  (n (length line)))
                             (while (< i n)
                               (let ((f (get-text-property i 'face line)))
                                 (when (and f (not (member (format "%S" f) seen)))
                                   (push (format "%S" f) seen)))
                               (setq i (1+ i)))
                             (if seen
                                 (mapconcat #'identity (nreverse seen) " | ")
                               "<the drawn tabline carries no face at all>"))`)
}

// tabPaint is WHERE a tab's arm color actually landed, read off the entry the
// module rendered rather than derived from the color table.
type tabPaint struct {
	// Selected says whether this is the selected tab.
	Selected bool
	// SelectedBg is `agent-repl--color-selected-bg` when Selected, else "".
	SelectedBg string
	// BracketBg and NameBg are the backgrounds the rendered entry's faces
	// STATE THEMSELVES on its `[N]` bracket run and on its workspace-name
	// run, and are empty when a face states none of its own. Equal and
	// non-empty means the arm color reached the whole entry.
	BracketBg, NameBg string
	// NameFace is the face the entry put on the name region, printed.
	NameFace string
}

// WholeEntry says the arm color reached the name region too.
func (p tabPaint) WholeEntry() bool { return p.BracketBg != "" && p.BracketBg == p.NameBg }

// tabEntryPaint reads the two backgrounds out of the entry
// `agent-repl--render-tab-entry` produces for WS.
//
// WHY THE COLOR TABLE IS NOT ENOUGH, and it was a manifest saying the wrong
// thing about a correct picture. How far an arm's color reaches is not the
// table's decision: `agent-repl--render-tab-entry` takes the BRACKET-ONLY
// spec whenever `agent-repl--ws-display-state` suppresses the full-tab color
// -- a workspace whose panels are dismissed, or a `:ready` one the user has
// already looked at -- and then `agent-repl--tab-spec-bracket-only` leaves
// `:bg` unspecified so only the badge carries the arm. Measured, in owner
// 20's K.63: the `:merging` workspace's badge was `#a21caf` while its name
// region was `#14141a`, under a sentence promising purple across the whole
// entry.
//
// So both runs are read from the rendered string at their own positions, the
// same way `tabFaceFor` reads the faces the module wrote: the bracket at the
// `[` the entry opens its badge with, the name at the workspace's own name.
//
// AND A COLOR IS NAMED ONLY WHERE THE FACE STATES ONE ITSELF. An inherited
// background cannot be resolved back to what was drawn: Doom's
// `+workspace-tab-face` is `:inherit default`, and asking for its inherited
// background on the graphical frame answered `white` for a name region the
// decoded pixels put at `#14141a` -- 1100 of them. So the read follows no
// inheritance, a face with no background of its own answers empty, and the
// sentence then makes the claim a reviewer CAN check: that the name region is
// not the arm color, and which of Doom's own faces it was drawn in.
func (s *playtestScenario) tabEntryPaint(t *testing.T, ws string) tabPaint {
	t.Helper()
	got := s.E.EvalStrings(`(let* ((ws ` + elispString(ws) + `)
                                   (cur (agent-repl--ws-current-name))
                                   (line (agent-repl--render-tab-entry ws cur 1))
                                   (frame (or (car (seq-filter #'display-graphic-p (frame-list)))
                                              (selected-frame)))
                                   (bg-at
                                    (lambda (pos)
                                      (let ((f (and pos (get-text-property pos 'face line))))
                                        (cond
                                         ((null f) "")
                                         ((and (listp f) (plist-member f :background))
                                          (format "%s" (plist-get f :background)))
                                         ((symbolp f)
                                          (let ((bg (face-attribute f :background frame)))
                                            (if (stringp bg) bg "")))
                                         (t "")))))
                                   (bracket (string-match (regexp-quote "[") line))
                                   (name (string-match (regexp-quote ws) line)))
                              (list (funcall bg-at bracket)
                                    (funcall bg-at name)
                                    (format "%S" (and name (get-text-property name 'face line)))))`)
	if len(got) != 3 {
		t.Fatalf("reading the rendered tab entry for %s answered %v, want a bracket background, a name "+
			"background and the name's face", ws, got)
	}
	return tabPaint{
		Selected:   s.E.EvalBool(`(equal ` + elispString(ws) + ` (agent-repl--ws-current-name))`),
		SelectedBg: s.tabSelectedBackground(t, ws),
		BracketBg:  got[0],
		NameBg:     got[1],
		NameFace:   got[2],
	}
}

// armSentence is the manifest sentence for a tab painted from one arm.
//
// WHAT THE PRODUCT ACTUALLY PAINTS, WHICH IS NOT A DISC. `agent-repl--render-tab`
// draws each tab as a bracketed index -- `agent-repl-tab-bracket-format`, "[N]" --
// followed by the workspace name, and it is the BRACKETED INDEX that always
// carries the arm's color, as the bracket run's BACKGROUND. A sentence
// promising a "status disc beside the name" sends a reviewer hunting a glyph
// the module never draws, and every owner's manifest inherits this sentence.
//
// HOW FAR THE COLOR REACHES IS READ, NEVER ASSUMED, and it is the half a
// reviewer was being lied to about. `agent-repl--render-tab-entry` paints an
// UNSELECTED tab's whole entry with its palette row's `:bg`, which IS the arm
// color; hands the SELECTED tab's name region Doom's selected-tab face, so
// the arm stops at the badge; and takes the BRACKET-ONLY spec whenever
// `agent-repl--ws-display-state` suppresses the full-tab color -- a workspace
// whose panels are dismissed, or a `:ready` one already viewed -- where the
// arm stops at the badge on an unselected tab too. Measured, in owner 20's
// section: K.61's unselected `:thinking` tab ran `#cc3333` unbroken from
// bracket through name, and K.63's unselected `:merging` tab carried `#a21caf`
// on its badge with `#14141a` under its name. One sentence cannot be true of
// both, so PAINT carries what `tabEntryPaint` read off the rendered entry and
// the sentence says which of the three the reviewer is looking at.
//
// "none" is a real answer and not a missing one: the color table maps
// `:none` and the whole merge family to it, and the index badge then carries
// NO arm color at all. Saying "a none-colored badge" would send a reviewer
// looking for something that is not there.
//
// AND "NO ARM COLOR" IS NOT "THE SAME COLOR AS THE BAR". The ground the
// badge is drawn on is the TAB'S OWN, and a selected tab's is
// `agent-repl--color-selected-bg` -- a grey visibly darker than the bar
// around it, from `agent-repl--tab-default`'s `:selected` plist. A sentence
// promising "the tab's ordinary background" therefore told a reviewer
// looking at a selected tab that a darker badge was wrong, when it is the
// selection the product is drawing. SELECTEDBG carries that color when the
// tab is selected and is empty when it is not, so the sentence names the
// ground the reviewer will actually see; it is READ OFF THE MODULE at
// capture time rather than written here, because a color spelled twice
// drifts.
func armSentence(ws, arm, color string, paint tabPaint) string {
	if color == "none" {
		ground := "the tab bar's own ordinary background"
		if paint.Selected && paint.SelectedBg != "" {
			ground = fmt.Sprintf("the SELECTED tab's own background (`%s`), which is visibly darker "+
				"than the bar around it -- that darker ground is the SELECTION, not an arm color",
				paint.SelectedBg)
		}
		return fmt.Sprintf("The tab for %q carries NO arm color at all on its %s index badge: its arm "+
			"is %s, which the module's own color table maps to no color, so the badge is drawn on "+
			"%s and only the name beside it is faced.", ws, tabBadgeShape, arm, ground)
	}
	if paint.WholeEntry() {
		return fmt.Sprintf("The tab for %q is painted %s ACROSS THE WHOLE ENTRY -- its %s index badge "+
			"and the workspace name beside it alike, both on `%s`: its arm is %s, and %s is the color "+
			"the module's own table gives that arm.",
			ws, strings.ToUpper(color), tabBadgeShape, paint.BracketBg, arm, color)
	}
	ground := fmt.Sprintf("`%s`", paint.NameBg)
	if paint.NameBg == "" {
		ground = fmt.Sprintf("whatever the theme gives Doom's own `%s`, which states no background of "+
			"its own", paint.NameFace)
	}
	name := fmt.Sprintf("the name beside it is NOT that color -- it is drawn on %s", ground)
	if paint.Selected {
		name = fmt.Sprintf("the name beside it is drawn on the SELECTION's own ground (%s) instead, "+
			"which is `agent-repl--tab-face` dimming the state to the badge on whichever tab is "+
			"selected", ground)
	}
	return fmt.Sprintf("The tab for %q carries %s on its %s index badge ALONE (`%s`): its arm is %s, "+
		"and %s is the color the module's own table gives that arm, while %s.",
		ws, strings.ToUpper(color), tabBadgeShape, paint.BracketBg, arm, color, name)
}

// tabBadgeShape is the shape of the run the arm color lands on, spelled the
// way `agent-repl-tab-bracket-format` draws it.
const tabBadgeShape = "`[N]`"

// captureArm takes one tab-bar picture whose subject is the arm a workspace
// is painted with, re-reading the arm at the moment of the capture and
// REFUSING if it has moved off the one the step is about.
func (s *playtestScenario) captureArm(t *testing.T, name, ws, act string, want string, extra string) {
	t.Helper()
	arm, color := s.armPaint(t, ws)
	if arm != want {
		t.Fatalf("the arm for %s is %s at capture time, want %s: the step's subject moved before its picture was taken",
			ws, arm, want)
	}
	face := s.tabFaceFor(t, ws)
	s.Book.capture(name, act,
		fmt.Sprintf("`agent-repl-roster-status-for-ws` still reads %s at the instant of the capture, which the module's color table paints %s, and `agent-repl-workspace-tabline-formatted` wrote the face %s onto that tab", arm, color, face),
		armSentence(ws, arm, color, s.tabEntryPaint(t, ws))+" "+extra)
}

// tabSelectedBackground answers the background a SELECTED tab is drawn on
// when WS is the selected one, and the empty string when it is not.
//
// Both halves come from the module: selection is `agent-repl--ws-current-name`,
// which is the very comparison `agent-repl--render-tab-entry` makes, and the
// color is `agent-repl--color-selected-bg`, which is what
// `agent-repl--tab-default`'s `:selected` plist puts under the badge. Reading
// them rather than restating them is what keeps the manifest sentence from
// promising a color the module has since changed.
func (s *playtestScenario) tabSelectedBackground(t *testing.T, ws string) string {
	t.Helper()
	return s.E.EvalString(`(if (equal ` + elispString(ws) + ` (agent-repl--ws-current-name))
                               agent-repl--color-selected-bg
                             "")`)
}
