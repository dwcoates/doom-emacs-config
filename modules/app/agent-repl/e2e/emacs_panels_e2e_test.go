package e2e

import (
	"encoding/json"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"testing"

	"agent-repld/integration/harness"
)

// AREA C of EMACS-LAYER-SPEC.md: panel and window lifecycle, six scenarios,
// all Emacs-only. No Connect-dialing test can reach any of them: their whole
// subject is `window-list`, the tab order, and the workspace plist a close
// variant writes.
//
// Per the spec's binding table, area C is where the panel open/close
// bindings are PRESSED (`Leader`) rather than looked up: "these are exactly
// the ones a `map!' regression would break silently, because the command
// keeps working." The bindings are `keybindings.el`'s own leader forms —
// `SPC o c' (agent-repl-simple), `SPC o C' (agent-repl), `SPC o v'
// (agent-repl-focus-input) and `SPC w f' (agent-repl-fullscreen-and-focus).

// panelSettleBound bounds a panel open's own settling — window creation plus
// the frontend dispatch that precedes it. It is the module's boot budget
// (`emacsBootBound`), which is the launcher's own budget for the daemon the
// dispatch talks to; a panel cannot legitimately outlast the link it rides.
const panelSettleBound = emacsBootBound

// emacsPanelWorld is one world with Emacs up, the daemon spawned by Emacs's
// own launcher, and `count` workspaces registered against scripted fake-git
// worktrees. It returns the workspace names in registration order.
//
// THE FRAME IT HANDS OVER IS PANEL-FREE AND SETTLED: each registration's own
// landing is waited out and then put away (`awaitRegistrationLanding`,
// `putTheLandingAway`), so a scenario's first act is not racing a panel show
// the registration asked for on the user's behalf.
//
// No real git: `harness.NewRepoAt` is the scripted fake, per SPEC.md.
func emacsPanelWorld(t *testing.T, count int) (*EmacsWorld, []string) {
	t.Helper()

	box := requireSandbox(t)
	w := NewEmacsWorld(t, box)
	e := w.Emacs
	e.EnsureDaemon()

	var names []string
	for i := 0; i < count; i++ {
		dir := filepath.Join(box.Scratch(), "repo-"+strconv.Itoa(i))
		repository := harness.NewRepoAt(t, dir)
		e.Eval(`(agent-repl-add-project-workspace ` + elispString(repository.Dir) + `)`)
		want := i + 1
		raw := e.AwaitEval("the workspace registry to hold the new workspace",
			workspaceRegistryNamesForm,
			func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == want })
		names = registryNames(raw)
		awaitRegistrationLanding(e, repository.Dir)
	}
	awaitTabsOpen(e, names)
	// The registry is a hash, so its iteration order says nothing; the
	// current workspace is what the panel verbs act on, and it is the one
	// most recently added.
	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	if current == "" || current == "nil" {
		t.Fatal("no current workspace after registering one")
	}
	putTheLandingAway(e, current)
	return w, names
}

// The landing each registration performs is waited out through
// `awaitRegistrationLanding` (landing_test.go), the ONE settle every caller
// that mints a workspace goes through.

// putTheLandingAway leaves every scenario the same starting frame: the
// workspaces exist and their panels are NOT on it.
//
// The panels go away through the module's own plain close rather than a
// window delete, because that close is what RESTORES and clears the layout
// the landing's own show saved (`agent-repl--restore-fullscreen-config`). A
// hand-rolled teardown leaves `:fullscreen-config` standing, and a later open
// keeps the standing one — so the close under test restores the frame as it
// stood at the LANDING rather than as the scenario arranged it, which is
// exactly how scenario 18 lost its work window.
func putTheLandingAway(e *Emacs, current string) {
	e.t.Helper()
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)
	e.Eval(`(call-interactively #'agent-repl-simple)`)
	e.AwaitEvalFor(panelSettleBound, "the landing's panels to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})
	if e.EvalBool(`(and (agent-repl--ws-get ` + elispString(current) + ` :fullscreen-config) t)`) {
		e.t.Fatalf("the close left workspace %q a saved layout; a later open would restore the landing's frame, not the scenario's", current)
	}
}

func registryNames(raw json.RawMessage) []string { return decodeStrings(raw) }

// windowBuffers is the frame's window list, read as DATA: buffer names per
// window, never `format-mode-line` and never drawn text.
func windowBuffers(e *Emacs) []string {
	e.t.Helper()
	return e.EvalStrings(`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`)
}

// panelBufferCount counts the windows showing one of the module's two panel
// buffer families, named by their own format defcustoms rather than by a
// literal: `agent-repl-frontend-buffer-name-format` and
// `agent-repl-panel-buffer-name-format`.
func panelBufferCount(buffers []string, frontendPrefix, panelPrefix string) int {
	n := 0
	for _, b := range buffers {
		if strings.HasPrefix(b, frontendPrefix) || strings.HasPrefix(b, panelPrefix) {
			n++
		}
	}
	return n
}

// bufferNamePrefixes reads the literal prefix of each panel buffer-name
// format, so the assertions follow the defcustoms instead of restating them.
func bufferNamePrefixes(e *Emacs) (frontend, panel string) {
	e.t.Helper()
	frontend = e.EvalString(`(car (split-string agent-repl-frontend-buffer-name-format "%"))`)
	panel = e.EvalString(`(car (split-string agent-repl-panel-buffer-name-format "%"))`)
	return frontend, panel
}

// openPanel drives the ordinary command, as a command.
func openPanel(e *Emacs) {
	e.t.Helper()
	e.Eval(`(call-interactively #'agent-repl-frontend-open-panel)`)
}

// awaitPanelWindows waits until the frame shows at least one panel window.
func awaitPanelWindows(e *Emacs, frontendPrefix, panelPrefix string) {
	e.t.Helper()
	e.AwaitEvalFor(panelSettleBound, "the agent-repl panel windows to appear",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) > 0
		})
}

// testEmacsDeleteOtherWindowsOverPanelsKeepsTheWorkLayout is scenario 18's
// other half: the ORDINARY window key, pressed over the panels.
//
// `delete-other-windows` is not the module's close. Pressed from the
// composer it takes the webview window, and the composer -- delete-protected
// so a stray sweep cannot strand the pair -- survives it alone. The frame
// then holds half a mount: `agent-repl--panels-visible-p` answers nil, so no
// close path runs and the workspace's `:fullscreen-config`, recorded against
// the landing this scenario arranges, is left standing behind whatever the
// user does next.
//
// The panels come back on their own from there -- the window-change
// reconciler repairs exactly this half-a-pair state
// (`agent-repl-window--ensure-layout`) by remounting the workspace's own show
// -- and it is that remount which must record the frame it is ACTUALLY
// covering. Kept, the landing-era configuration made the next close restore
// the LANDING over the user's own window, which then had no window at all.
func testEmacsDeleteOtherWindowsOverPanelsKeepsTheWorkLayout(t *testing.T) {
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)
	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)

	const landing, work = "*e2e-dow-landing*", "*e2e-dow-work*"

	// The landing the panels are opened over: this is what the open saves.
	e.Eval(`(progn (delete-other-windows)
                   (switch-to-buffer (get-buffer-create ` + elispString(landing) + `))
                   t)`)

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	// ACT, as one user gesture: the ordinary sweep from the composer, and
	// then the buffer the user opens on the frame it leaves behind. One
	// `Eval` because the reconciler's repair runs from a timer -- split in
	// two, the repair could land between them and there would be no
	// half-torn frame for the user to open a buffer onto.
	e.Eval(`(progn (select-window (get-buffer-window (agent-repl--ws-get ` + elispString(current) + ` :input-buffer)))
                   (delete-other-windows)
                   (switch-to-buffer (get-buffer-create ` + elispString(work) + `))
                   t)`)

	// The panels come back through the module's own repair.
	e.AwaitEvalFor(panelSettleBound, "the panels to come back over the user's window",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) >= 2
		})

	// And the plain close puts them away again.
	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})

	// ASSERT: the user's own buffer still has a window, and the landing the
	// panels were first opened over did not come back over it.
	after := windowBuffers(e)
	if !e.EvalBool(`(and (get-buffer-window (get-buffer ` + elispString(work) + `)) t)`) {
		t.Fatalf("the user's work buffer %q has no window after the close; the frame holds %q", work, after)
	}
	if e.EvalBool(`(and (get-buffer-window (get-buffer ` + elispString(landing) + `)) t)`) {
		t.Fatalf("the close restored the landing %q over the user's own window; the frame holds %q", landing, after)
	}
}

// testEmacsOpenProgressLadderReachesRendered is scenario 19: the blessed
// host-native progress ladder raised by `SPC o c` walks its stages and ends
// with the real panel on the frame, never on a terminal phase.
//
// THE LADDER IS RAISED ONLY BY THE TOGGLE (`agent-repl--panels-show-or-open`
// calls `agent-repl--open-progress-start`); `agent-repl-frontend-open-panel`
// mounts the page without one, so a test that opened through it read the
// registry as "gone" before any ladder ever existed. The ladder is therefore
// recorded off its one publication, `agent-repl-open-progress-change-functions`,
// from before the keypress: the trail must open on `:requested`, rest only on
// ladder stages, and end "gone" -- `agent-repl--open-progress-finish`, which
// runs once the real panel is displayed -- with no terminal phase anywhere.
func testEmacsOpenProgressLadderReachesRendered(t *testing.T) {
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)
	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	e.Eval(`(progn
	          (defvar e2e-open-progress-trail nil)
	          (setq e2e-open-progress-trail nil)
	          (add-hook 'agent-repl-open-progress-change-functions
	                    (lambda ()
	                      (push (let ((entry (gethash ` + elispString(current) + ` agent-repl--open-progress)))
	                              (if entry (format "%s" (plist-get entry :phase)) "gone"))
	                            e2e-open-progress-trail)))
	          t)`)
	terminal := e.EvalStrings(`(mapcar (lambda (p) (format "%s" p)) agent-repl--open-progress-terminal-phases)`)
	stages := e.EvalStrings(`(mapcar (lambda (entry) (format "%s" (car entry))) agent-repl--open-progress-stages)`)

	// Act.
	e.Leader("o c")
	awaitPanelWindows(e, frontendPrefix, panelPrefix)
	raw := e.AwaitEvalFor(panelSettleBound, "the open-progress ladder to end",
		`(reverse e2e-open-progress-trail)`,
		func(raw json.RawMessage) bool {
			trail := decodeStrings(raw)
			return len(trail) > 0 && (trail[len(trail)-1] == "gone" || slices.Contains(terminal, trail[len(trail)-1]))
		})

	// Assert.
	trail := decodeStrings(raw)
	if trail[0] != ":requested" {
		t.Fatalf("the ladder's trail %q does not open on :requested; SPC o c raised no ladder", trail)
	}
	for _, phase := range trail[:len(trail)-1] {
		if !slices.Contains(stages, phase) {
			t.Fatalf("the ladder's trail %q passes %q, which is not a stage of %q", trail, phase, stages)
		}
	}
	if last := trail[len(trail)-1]; last != "gone" {
		t.Fatalf("the ladder's trail %q ends on the terminal phase %q, want the placeholder finished with the panel up", trail, last)
	}
}
