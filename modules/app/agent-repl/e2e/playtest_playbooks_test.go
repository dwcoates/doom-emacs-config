//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// THE PLAYBOOKS.
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
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box, options...)
	e := w.Emacs

	book := newPlaybook(t, e, name, purpose)
	// THE FRAME IS SIZED BEFORE ANYTHING IS DRAWN IN IT. Emacs takes a
	// default frame far smaller than the screen, and the panel's webview is
	// laid out in real pixels -- so a frame resized after the webview
	// existed would photograph a webapp laid out for a different window.
	book.prepareFrame()

	s := &playtestScenario{Book: book, World: w, E: e, Box: box}
	// THE COLD-START LAUNCH IS AN ASSERTION, not arrangement: `EnsureDaemon`
	// waits for `agent-repl-link--primary`, so a launcher that composes a
	// wrong argv fails here rather than somewhere downstream.
	e.EnsureDaemon()
	book.note("Emacs spawned the daemon through its own launcher (`agent-repl-frontend-daemon-ensure`)",
		"Emacs holds a live link: `agent-repl-link--primary` is non-nil")
	return s
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
	s.awaitInPage(t, "the webapp to draw its footer, which means the daemon's push arrived",
		`document.querySelector(".footer-status") &&
         document.querySelector(".footer-status").textContent.trim() !== ""`)
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
	s.E.Eval(`(setq agent-repl-playtest--js nil)`)
	s.E.AwaitEvalFor(playtestPageBound, what,
		`(agent-repl-playtest--probe `+elispString(s.Name)+` `+elispString(pageYes(expression))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
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
// The color comes from `agent-repl-status-color-table` and
// `agent-repl--color-by-name` -- the module's own two tables, which
// `AGENTS.md` names as the one source for tab coloring -- so the sentence a
// reviewer checks is the module's own decision rather than this file's guess
// at it.
func (s *playtestScenario) armPaint(t *testing.T, ws string) (arm, color string) {
	t.Helper()
	pair := s.E.EvalStrings(`(let* ((arm (agent-repl-roster-status-for-ws ` + elispString(ws) + `))
                                     (color (cdr (assq arm agent-repl-status-color-table))))
                                (list (format "%s" arm) (format "%s" (or color "<no color-table entry>"))))`)
	if len(pair) != 2 {
		t.Fatalf("reading the arm and color for %s answered %v, want an arm and a color", ws, pair)
	}
	return pair[0], pair[1]
}

// armSentence is the manifest sentence for a tab painted from one arm.
//
// "none" is a real answer and not a missing one: the color table maps
// `:none` and the whole merge family to it, and the tab then carries NO disc
// at all. Saying "a none-colored disc" would send a reviewer looking for
// something that is not there.
func armSentence(ws, arm, color string) string {
	if color == "none" {
		return fmt.Sprintf("The tab for %q carries NO status disc at all: its arm is %s, which the "+
			"module's own color table maps to no color.", ws, arm)
	}
	return fmt.Sprintf("The tab for %q carries a %s status disc beside its name: its arm is %s, and "+
		"%s is the color the module's own table gives that arm.", ws, strings.ToUpper(color), arm, color)
}

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
	s.Book.capture(name, act,
		fmt.Sprintf("`agent-repl-roster-status-for-ws` still reads %s at the instant of the capture, which the module's color table paints %s", arm, color),
		armSentence(ws, arm, color)+" "+extra)
}

// ---------------------------------------------------------------------------
// SECTION A — BOOT AND ROSTER
// ---------------------------------------------------------------------------

// TestPlaytestColdStartAndFirstTab is plan A.1 and A.4: an editor with
// nothing in it, a daemon Emacs spawned itself, and the first workspace tab.
//
// The two captures are the tab bar, which is the plan's own visual subject
// for section A and a surface no Connect-dialing test can see at all.
func TestPlaytestColdStartAndFirstTab(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "a-cold-start",
		"Plan A.1 and A.4. A cold Emacs spawns its own daemon, then registers a repository "+
			"from a directory and gains its first workspace tab.")
	p, e := s.Book, s.E

	if !decodeBool(e.Eval(`(and agent-repl--frontend-daemon-process
                                (process-live-p agent-repl--frontend-daemon-process))`)) {
		t.Fatal("the launcher reports no live daemon process after EnsureDaemon")
	}
	if _, err := os.Stat(filepath.Join(e.StateDir, "daemon.addr")); err != nil {
		t.Fatalf("the daemon published no address under the state root Emacs handed it: %v", err)
	}
	p.capture("cold-editor", "nothing registered yet",
		"`agent-repl--frontend-daemon-process` is live and the daemon published `daemon.addr` under the state root",
		"An Emacs frame filling the whole screen, booted through the image's real Doom. "+
			"The tab bar carries NO workspace tab: nothing is registered yet.")

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	if got := s.tabNames(); len(got) != 1 || got[0] != name {
		t.Fatalf("the tab bar draws %v, want exactly [%s]", got, name)
	}
	// A WORKSPACE NOTHING HAS BEEN WIRED TO IS `:none`, AND THAT IS THE
	// POINT OF THIS PICTURE. Registering mints an identity; it does not bring
	// a session up, which the first submit does. Per the module's color rule
	// `:none` is TEAL -- "nothing is wired, and nothing is wrong" -- and it
	// is emphatically not the blue of a broken link.
	s.awaitArm(t, name, "the new workspace's tab arm to be published", playtestUnwiredArm)
	s.captureArm(t, "first-tab", name,
		"`agent-repl-add-project-workspace` (`SPC TAB C-n`) on a scripted fake-git worktree",
		playtestUnwiredArm,
		fmt.Sprintf("The tab bar must carry EXACTLY ONE workspace tab and it must be named %q: "+
			"`agent-repl--ws-tabline-names` says the module knows about exactly that one.", name))
}

// TestPlaytestSwitchBetweenWorkspaces is plan A.7: a second workspace, the
// selection moving between the two, and the tab bar following it.
func TestPlaytestSwitchBetweenWorkspaces(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "a-switch",
		"Plan A.7. Two workspaces on the tab bar, and the selection moving between them.")
	p, e := s.Book, s.E

	first := s.repoAt(t, "repo-first")
	firstName := s.register(t, first.Dir)
	s.openPanel(t)
	p.note("the first repository registered and its panel opened",
		"the composer buffer exists and the webapp drew its footer against this daemon")

	second := s.repoAt(t, "repo-second")
	secondName := s.register(t, second.Dir)
	// Registering SELECTS, which is one of Emacs's only two inputs to the
	// roster, so the assertion is on the module's own current-workspace
	// accessor rather than on anything drawn.
	e.AwaitEval("the second workspace to become the selected one",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == secondName })
	names := s.tabNames()
	if len(names) != 2 {
		t.Fatalf("the tab bar draws %v, want both workspaces", names)
	}
	p.capture("two-tabs", "a second repository registered through the same verb",
		fmt.Sprintf("`agent-repl--ws-tabline-names` is %v and `agent-repl--ws-current-name` is %q", names, secondName),
		fmt.Sprintf("The tab bar must carry TWO workspace tabs, %q and %q, in that order, and the "+
			"SECOND must be the highlighted one — registering selects it.", names[0], names[1]))

	// `agent-repl-switch-to-project` takes a PROJECT ROOT PATH, not a
	// workspace name -- its own docstring says so -- and taking the target as
	// an argument is why the picker is neither the subject nor stubbed.
	e.Eval(`(agent-repl-switch-to-project ` + elispString(first.Dir) + `)`)
	e.AwaitEval("the first workspace to become the selected one again",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool { return decodeString(raw) == firstName })
	p.capture("switched-back", "`agent-repl-switch-to-project` back to the first workspace",
		fmt.Sprintf("`agent-repl--ws-current-name` is %q", firstName),
		fmt.Sprintf("The SAME two tabs in the SAME order, with the highlight moved back to %q. "+
			"The selection moved; the roster did not.", firstName))
}

// TestPlaytestCloseAndKillLeaveTheEditorAnswering is plan A.9's close and
// kill, and it is deliberately FUNCTIONAL-ONLY: what it proves is that the
// tab goes away, the daemon still holds the session after a close, and Emacs
// is still answering afterwards. None of that is a picture.
//
// The heartbeat is the real assertion behind the last of those, and it is
// armed for the whole life of the process: this is the sentinel/kill-buffer
// recursion, which manifests only as an editor that stops answering.
func TestPlaytestCloseAndKillLeaveTheEditorAnswering(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "a-close-and-kill",
		"Plan A.9. Close is a view act and kill never blocks, and neither wedges the editor. "+
			"FUNCTIONAL ONLY: nothing here has a visual subject, so nothing is captured.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	// `SPC j d` takes the CURRENT workspace, so it is PRESSED: real keymap
	// lookup, real command.
	e.Leader("j d")
	e.AwaitEval("the closed workspace's tab to be gone",
		emacsWSTablineNamesForm,
		func(raw json.RawMessage) bool { return !containsString(decodeStrings(raw), name) })
	p.note("`SPC j d` pressed to close the workspace",
		"the name is gone from `agent-repl--ws-tabline-names`")

	// CLOSE IS A VIEW ACT BY CONTRACT, so the only way to say the daemon still
	// holds the workspace is to ask the daemon -- at the address Emacs's own
	// launcher published.
	awaitDaemonRoster(t, e.DaemonAddr(), emacsVerbBound,
		"the daemon to still hold the closed workspace",
		func(r *frontendv1.WorkspaceRoster) bool { return len(r.GetRepository().GetSections()) > 0 })
	p.note("the daemon asked for its own roster at the address the launcher published",
		"the daemon still carries the workspace: closing is a VIEW act and destroys nothing")

	e.AwaitEvalFor(emacsWedgeProbeBound, "emacs to still answer its command loop after the close and kill",
		`(and (emacs-pid) t)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	p.note("Emacs probed for liveness after the close",
		"the command loop still answers, and the heartbeat has not missed for the whole run")
}

// ---------------------------------------------------------------------------
// SECTION B — TAB-BAR ARMS, ONE PLAYBOOK PER TRANSITION
// ---------------------------------------------------------------------------

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
	s := newPlaytestScenario(t, "b-arm-idle-thinking-done",
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
func TestPlaytestTabArmFailed(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "b-arm-failed",
		"Plan B.14. A turn that fails at the vendor, and the arm the tab paints for it.")
	p := s.Book

	repository := s.repoAt(t, "repo")
	name := s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	s.submit(t, "!fail-execution")
	// `:vendor-blocked` IS THE ANSWER, and it is asserted by name rather than
	// as "any settled arm". The module's color rule puts it in PURPLE -- the
	// vendor's own work went wrong -- and the whole value of this picture is
	// that the tab does NOT paint the blue of a broken local environment for
	// a failure that is not the local environment's.
	s.awaitArm(t, name, "the tab's arm to reach the vendor-blocked arm", playtestVendorBlockedArm)
	s.captureArm(t, "arm-after-failure", name,
		"the fake SDK's `!fail-execution` scenario ended the turn on an execution error",
		playtestVendorBlockedArm,
		"PURPLE is the whole subject: the vendor's own work went wrong, so the tab must NOT paint "+
			"the BLUE that means something on this machine broke, and must not still paint the RED "+
			"of a turn that is running.")
}

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
	s := newPlaytestScenario(t, "b-arm-attention-on-permission",
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

// ---------------------------------------------------------------------------
// SECTION I — PANELS AND LAYOUT
// ---------------------------------------------------------------------------

// TestPlaytestPanelAndFullscreen is plan I.53 and I.54: the panel opened into
// the main area, made fullscreen, and restored.
func TestPlaytestPanelAndFullscreen(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "i-panel-and-fullscreen",
		"Plan I.53 and I.54. The panel opened into the main area, then fullscreen and back.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	windows := e.EvalInt(`(length (window-list))`)
	p.capture("panel-open", "`agent-repl-frontend-open-panel`",
		fmt.Sprintf("the panel's webview is live, the composer buffer exists, and the frame holds %d windows", windows),
		"The frame is SPLIT: the WEBAPP is drawn inside the panel window — workspace sidebar down "+
			"one side, an empty feed, and the progress footer along the bottom with a status word in "+
			"it — and a separate Emacs window holds the composer. THE WEBAPP MUST NOT BE A BLANK "+
			"WHITE RECTANGLE.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitTrue("the fullscreen configuration to be recorded",
		`(and agent-repl--window-fullscreen-config t)`)
	p.capture("fullscreen", "`agent-repl-fullscreen-and-focus` (`SPC w f`)",
		"`agent-repl--window-fullscreen-config` is non-nil, so the layout was saved to be restored",
		"ONE window fills the whole frame. The webapp is drawn edge to edge and the composer window "+
			"is gone.")

	e.Eval(`(agent-repl-fullscreen-and-focus)`)
	e.AwaitEval("the fullscreen configuration to be released",
		`(and agent-repl--window-fullscreen-config t)`,
		func(raw json.RawMessage) bool { return isJSONNull(raw) })
	if got := e.EvalInt(`(length (window-list))`); got != windows {
		t.Fatalf("the frame holds %d windows after restoring, want the %d it started with", got, windows)
	}
	p.capture("fullscreen-restored", "the same command again, restoring the layout",
		fmt.Sprintf("`agent-repl--window-fullscreen-config` is nil and the frame holds its original %d windows", windows),
		"The split of the first capture is back, unchanged: the toggle RESTORED the layout rather "+
			"than rebuilding some other one.")
}

// ---------------------------------------------------------------------------
// SECTION J — DAEMON LIFECYCLE
// ---------------------------------------------------------------------------

// TestPlaytestScheduledDrainBanner is plan J.57: a scheduled shutdown, and
// the standing banner it raises in two places at once.
func TestPlaytestScheduledDrainBanner(t *testing.T) {
	t.Parallel()
	s := newPlaytestScenario(t, "j-drain",
		"Plan J.57. A scheduled shutdown, and the standing drain banner it puts on Emacs's mode "+
			"line and across the webapp.")
	p, e := s.Book, s.E

	repository := s.repoAt(t, "repo")
	s.register(t, repository.Dir)
	s.openPanel(t)
	p.note("one repository registered with its panel open",
		"the panel's webview is live and the webapp drew its footer")

	// Five minutes out, so the daemon is still serving for the rest of the
	// playbook and its teardown. The command prompts for its reason, so the
	// reader is bound for the duration of the one call -- the standard ERT
	// way, which keeps the command running its own argument collection.
	const drainMinutes = 5
	const drainReason = "maintenance"
	e.Eval(`(cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _) ` + elispString(drainReason) + `)))
              (agent-repl-daemon-shutdown-schedule ` + strconv.Itoa(drainMinutes) + `)
              t)`)
	e.AwaitTrue("the daemon's drain_scheduled push to reach Emacs", `(and agent-repl-link-drain t)`)
	if arm := e.EvalString(`(format "%s" (plist-get (plist-get agent-repl-link-drain :reason) :arm))`); arm != ":"+drainReason {
		t.Fatalf("the standing drain's reason arm is %q, want %q", arm, ":"+drainReason)
	}
	// The segment's own composition is the subject here, which is this
	// layer's sanctioned exception to "never scrape human text where a
	// variable exists".
	segment := e.EvalString(`(or agent-repl-link-drain-segment "")`)
	if !strings.HasPrefix(segment, "drain ") || !strings.HasSuffix(segment, "· "+drainReason) {
		t.Fatalf("the drain segment is %q, want \"drain HH:MM · %s\"", segment, drainReason)
	}
	s.awaitInPage(t, "the webapp's own drain banner to be drawn",
		`document.querySelector('[data-component="drain-banner"]') &&
         document.querySelector('[data-component="drain-banner"]').textContent.trim() !== ""`)
	p.capture("drain-scheduled", "`agent-repl-daemon-shutdown-schedule` five minutes out, reason \"maintenance\"",
		fmt.Sprintf("`agent-repl-link-drain` carries the reason arm `:%s`, `agent-repl-link-drain-segment` renders %q, and the webapp's own drain banner is non-empty", drainReason, segment),
		"The WEBAPP draws a STANDING DRAIN BANNER across the top of the panel, naming the reason "+
			"(\"maintenance\") and how long is left. Emacs's own `drain HH:MM · maintenance` segment is "+
			"asserted as a string rather than read off this picture: it lives in `global-mode-string`, "+
			"which Doom's mode line renders on the right of whatever line has room, so which line "+
			"carries it is not this playbook's claim.")
}
