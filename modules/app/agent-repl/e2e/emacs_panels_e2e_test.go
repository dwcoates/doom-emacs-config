package e2e

import (
	"encoding/json"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"claude-repld/integration/harness"
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
			`(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) names)`,
			func(raw json.RawMessage) bool { return len(decodeStrings(raw)) == want })
		names = registryNames(raw)
		awaitRegistrationLanding(e, repository.Dir)
	}
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

// awaitRegistrationLanding waits out the landing that REGISTERING a
// workspace performs.
//
// A REGISTERED WORKSPACE COMES UP ON ITS OWN PANEL, by design: the daemon
// mints the ref, `agent-repl-verbs-select-minted` switches to the minted
// worktree, and the switch arms `:pending-show-panels` so the perspective
// activation drain shows the workspace's view
// (`agent-repl--arm-landing-panels`, `agent-repl--drain-pending-show-panels`).
// That landing waits on the daemon's answer, so it is ASYNCHRONOUS: a
// scenario that starts measuring windows as soon as the registry holds the
// name is racing a panel show it never asked for. Under the soak the show
// arrived mid-scenario — the frame already held both panels before the open
// under test, and a work layout arranged before an open was collapsed by the
// arriving show.
//
// Settled means BOTH: nothing is left armed, and the workspace's own webview
// has a live window. The window is what makes the wait safe — the flag alone
// reads as "settled" in the moment before the arm.
//
// The bound is `emacsVerbBound`, not `panelSettleBound`: this wait spans a
// WHOLE VERB — the register call out of Emacs, the daemon's minted ref, and
// the landing that ref triggers — rather than a panel's own settling on a
// frame that already holds the workspace.
func awaitRegistrationLanding(e *Emacs, dir string) {
	e.t.Helper()
	// The form answers the STATE rather than a bare yes/no, so a wait that
	// is never satisfied names which half of "settled" is missing.
	e.AwaitEvalFor(emacsVerbBound, "the registration's landing to put the workspace's panel on the frame",
		`(let* ((ws (agent-repl--ws-name-for-dir `+elispString(dir)+`))
                        (buf (and ws (agent-repl--ws-get ws :frontend-buffer))))
                   (format "ws=%s pending=%s webview=%s window=%s"
                           ws
                           (and ws (agent-repl--ws-get ws :pending-show-panels))
                           (and (buffer-live-p buf) (buffer-name buf))
                           (and (buffer-live-p buf) (window-live-p (get-buffer-window buf)))))`,
		func(raw json.RawMessage) bool {
			var state string
			if err := json.Unmarshal(raw, &state); err != nil {
				return false
			}
			return !strings.HasPrefix(state, "ws=nil") &&
				strings.Contains(state, "pending=nil") &&
				strings.HasSuffix(state, "window=t")
		})
}

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

// TestEmacsPanelOpensIntoTheMainArea is scenario 14.
func TestEmacsPanelOpensIntoTheMainArea(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	before := len(windowBuffers(e))

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	after := windowBuffers(e)
	if len(after) <= before {
		t.Fatalf("the window list did not grow across an open: %d -> %d (%q)", before, len(after), after)
	}
	var sawFrontend, sawPanel bool
	for _, b := range after {
		if strings.HasPrefix(b, frontendPrefix) {
			sawFrontend = true
		}
		if strings.HasPrefix(b, panelPrefix) {
			sawPanel = true
		}
	}
	if !sawFrontend {
		t.Fatalf("no window shows a %q buffer after the open; windows were %q", frontendPrefix, after)
	}
	if !sawPanel {
		t.Fatalf("no window shows a %q buffer after the open; windows were %q", panelPrefix, after)
	}
}

// TestEmacsPlainCloseHidesPanelsAndLeavesTheTabAlone is scenario 15: the
// close variant that only hides, pressed as `SPC o c`.
func TestEmacsPlainCloseHidesPanelsAndLeavesTheTabAlone(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 2)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)
	orderBefore := e.EvalStrings(`agent-repl-roster--tab-order`)

	if want, got := "agent-repl-simple", e.LeaderBinding("o c"); got != want {
		t.Fatalf("SPC o c resolves to %q, want %q: the module's `map!' leader form did not take", got, want)
	}
	e.Leader("o c")

	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})

	orderAfter := e.EvalStrings(`agent-repl-roster--tab-order`)
	if strings.Join(orderBefore, "\x00") != strings.Join(orderAfter, "\x00") {
		t.Fatalf("the plain close moved the tab order: %q -> %q; only the deprio close may touch it", orderBefore, orderAfter)
	}
}

// TestEmacsPlainCloseTogglesThePanelsBack is the other half of scenario 15:
// `SPC o c` is a TOGGLE, so a second press on a workspace whose panels are
// merely hidden must put them back.
//
// It is a separate scenario from the close because it reaches a DIFFERENT
// branch of `agent-repl--toggle`: with no panel window on the frame the
// toggle asks the frontend registry whether the workspace is running
// (`:running-p-fn`) and shows rather than opens. That slot pointed at a void
// symbol, and nothing caught it — the close half passes without ever asking
// the question, and no unit test of a function that does not exist can fail.
func TestEmacsPlainCloseTogglesThePanelsBack(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})
	// The hidden webview is what makes the workspace READ as running, which
	// is the state the second press is about.
	if !e.EvalBool(`(and (buffer-live-p (agent-repl--ws-get ` + elispString(current) + ` :frontend-buffer)) t)`) {
		t.Fatal("the plain close killed the webview buffer; it only hides the panels")
	}
	if !e.EvalBool(`(and (funcall (agent-repl-frontend-running-p-fn (agent-repl--ws-frontend ` + elispString(current) + `)) ` + elispString(current) + `) t)`) {
		t.Fatalf("the frontend registry says workspace %q is not running while its webview is alive; the toggle would mount a second page", current)
	}

	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to come back",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) >= 2
		})
	// SHOWN, NOT REMOUNTED: the same buffer is back, so the page the user was
	// looking at was not thrown away and rebuilt.
	if !e.EvalBool(`(and (buffer-live-p (agent-repl--ws-get ` + elispString(current) + ` :frontend-buffer))
                          (get-buffer-window (agent-repl--ws-get ` + elispString(current) + ` :frontend-buffer))
                          t)`) {
		t.Fatalf("workspace %q's own webview buffer is not the one on the frame after the toggle reopened", current)
	}
}

// TestEmacsWebviewIsAtHomeOnItsOwnDaemon is the rescue's own predicate,
// against the real running daemon: `agent-repl--frontend-webview-at-home-p`
// decides whether `SPC o L` navigates or leaves the page alone, so a
// predicate that cannot be CALLED makes the rescue unreachable.
//
// It reached a `agent-repl--frontend-base-url` that no longer exists, and
// every unit test of the rescue mocked around it.
func TestEmacsWebviewIsAtHomeOnItsOwnDaemon(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	// Home is the origin of the connection that OWNS the workspace, and it is
	// the origin the webview's own URL is built from.
	origin := e.EvalString(`(or (agent-repl--frontend-home-origin ` + elispString(current) + `) "")`)
	if origin == "" {
		t.Fatalf("workspace %q has no home origin while its daemon link is up", current)
	}
	url := e.EvalString(`(agent-repl-frontend-webview-url ` + elispString(current) + `)`)
	if !strings.HasPrefix(url, origin) {
		t.Fatalf("the webview URL %q is not served from the workspace's own home origin %q", url, origin)
	}
	if !e.EvalBool(`(and (agent-repl--frontend-webview-at-home-p ` + elispString(current) + ` ` + elispString(url) + `) t)`) {
		t.Fatalf("the workspace's own webview URL %q does not read as home", url)
	}
	// And a page that left the daemon does not: this is the state the rescue
	// exists for, and reading it wrong either strands the user or throws away
	// a rendered feed.
	if e.EvalBool(`(and (agent-repl--frontend-webview-at-home-p ` + elispString(current) + ` "about:blank") t)`) {
		t.Fatal("`about:blank` reads as home; the rescue would refuse to bring a stranded webview back")
	}
}

// TestEmacsDeprioCloseShufflesTheTab is scenario 16. The two close variants
// differing is the whole reason both commands exist, so the claim is exactly
// the difference: `SPC o C` records `:saved-tab-index` and pushes the
// workspace's tab to the back.
func TestEmacsDeprioCloseShufflesTheTab(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 2)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	if want, got := "agent-repl", e.LeaderBinding("o C"); got != want {
		t.Fatalf("SPC o C resolves to %q, want %q: the module's `map!' leader form did not take", got, want)
	}
	e.Leader("o C")

	e.AwaitEvalFor(panelSettleBound, "the deprio close to record a saved tab index",
		`(agent-repl--ws-get `+elispString(current)+` :saved-tab-index)`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })

	order := e.EvalStrings(`agent-repl-roster--tab-order`)
	if len(order) < 2 {
		t.Fatalf("the tab order holds %d entries, need two for a push-to-back to be observable: %q", len(order), order)
	}
	if order[len(order)-1] != current {
		t.Fatalf("the deprio'd workspace %q is not last in the tab order %q", current, order)
	}
}

// TestEmacsFocusInputSelectsTheComposer is scenario 17.
func TestEmacsFocusInputSelectsTheComposer(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	want := e.AwaitEvalFor(panelSettleBound, "the workspace's input buffer to exist",
		`(let ((buf (agent-repl--input-buffer `+elispString(current)+`))) (and buf (buffer-name buf)))`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	var inputBuffer string
	if err := json.Unmarshal(want, &inputBuffer); err != nil {
		t.Fatalf("decode the input buffer name: %v", err)
	}

	// Select the WEBVIEW window, which is the one window on the frame that is
	// not the composer. `(car (window-list))` is the SELECTED window by
	// definition, so the old arrangement moved nothing: once the open's own
	// mount had selected the composer — which it does, and under load it wins
	// the race — the press landed in the jump-back branch rather than the
	// show-or-focus branch this scenario is about.
	e.AwaitEvalFor(panelSettleBound, "the workspace's webview window to be on the frame",
		`(let ((buf (agent-repl--ws-get `+elispString(current)+` :frontend-buffer)))
                   (and (buffer-live-p buf) (window-live-p (get-buffer-window buf)) t))`,
		func(raw json.RawMessage) bool { return !isJSONNull(raw) })
	e.Eval(`(progn (select-window (get-buffer-window (agent-repl--ws-get ` + elispString(current) + ` :frontend-buffer))) t)`)

	if want, got := "agent-repl-focus-input", e.LeaderBinding("o v"); got != want {
		t.Fatalf("SPC o v resolves to %q, want %q: the module's `map!' leader form did not take", got, want)
	}
	e.Leader("o v")

	e.AwaitEvalFor(panelSettleBound, "the composer window to be selected",
		`(buffer-name (window-buffer (selected-window)))`,
		func(raw json.RawMessage) bool {
			var name string
			return json.Unmarshal(raw, &name) == nil && name == inputBuffer
		})
}

// TestEmacsFullscreenTogglesAndRestores is scenario 18.
//
// The command has two branches and only the non-agent one maximizes, so the
// press happens from an ordinary work window: from inside a panel buffer
// `agent-repl-fullscreen-and-focus` moves point to the composer instead,
// which is a different claim and not this scenario's.
func TestEmacsFullscreenTogglesAndRestores(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	// THE WORK LAYOUT IS ARRANGED BEFORE THE PANELS, because a buffer opened
	// while the panels are visible closes them: `close-panels-on-open.el`
	// advises `switch-to-buffer' itself, so a split made after the open
	// collapses back to the pre-panel layout and there is no "work window
	// beside the panels" state to press the key in. Arranged first, this
	// two-window layout is what the panels' own open SAVES and what the plain
	// close restores -- so the maximize below acts on a real work layout.
	e.Eval(`(progn (delete-other-windows)
                   (switch-to-buffer (get-buffer-create "*e2e-work-a*"))
                   (select-window (split-window))
                   (switch-to-buffer (get-buffer-create "*e2e-work-b*"))
                   t)`)

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	// Back to the work layout: the plain close restores what the open saved.
	e.Leader("o c")
	e.AwaitEvalFor(panelSettleBound, "the panel windows to go away",
		`(mapcar (lambda (w) (buffer-name (window-buffer w))) (window-list))`,
		func(raw json.RawMessage) bool {
			return panelBufferCount(decodeStrings(raw), frontendPrefix, panelPrefix) == 0
		})
	e.Eval(`(progn (select-window (get-buffer-window (get-buffer "*e2e-work-b*"))) t)`)
	beforeBuffers := windowBuffers(e)
	before := len(beforeBuffers)
	if before < 2 {
		t.Fatalf("the restored work layout holds %d windows (%q); a restore is unobservable from one", before, beforeBuffers)
	}

	if want, got := "agent-repl-fullscreen-and-focus", e.LeaderBinding("w f"); got != want {
		t.Fatalf("SPC w f resolves to %q, want %q: the module's `map!' leader form did not take", got, want)
	}

	e.Leader("w f")
	if !e.EvalBool(`(and agent-repl--window-fullscreen-config t)`) {
		t.Fatal("the first SPC w f saved no window configuration, so there is nothing to restore to")
	}
	if maximized := windowBuffers(e); len(maximized) != 1 || maximized[0] != "*e2e-work-b*" {
		t.Fatalf("the frame holds %q after SPC w f, want exactly the maximized work window", maximized)
	}

	e.Leader("w f")
	if e.EvalBool(`(and agent-repl--window-fullscreen-config t)`) {
		t.Fatal("the second SPC w f left the saved window configuration standing")
	}
	if after := len(windowBuffers(e)); after != before {
		t.Fatalf("the window count did not return to its starting value: %d -> %d", before, after)
	}
	// RESTORED, not merely re-split: the same buffers are back in the same
	// order, which a rebuilt layout would not guarantee.
	if after := windowBuffers(e); strings.Join(after, "\x00") != strings.Join(beforeBuffers, "\x00") {
		t.Fatalf("the restored layout shows %q, want the layout it replaced, %q", after, beforeBuffers)
	}
}

// TestEmacsDeleteOtherWindowsOverPanelsKeepsTheWorkLayout is scenario 18's
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
func TestEmacsDeleteOtherWindowsOverPanelsKeepsTheWorkLayout(t *testing.T) {
	t.Parallel()
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

// TestEmacsOpenProgressLadderReachesRendered is scenario 19: the blessed
// host-native progress ladder walks its stages and ends on a NON-terminal
// phase.
//
// A finished open drops its registry entry outright
// (`agent-repl--open-progress-finish` remhashes it), so "gone" and "on a
// non-terminal stage past `:requested`" are the two shapes of success; a
// phase in `agent-repl--open-progress-terminal-phases` is the failure.
func TestEmacsOpenProgressLadderReachesRendered(t *testing.T) {
	t.Parallel()
	w, _ := emacsPanelWorld(t, 1)
	e := w.Emacs
	frontendPrefix, panelPrefix := bufferNamePrefixes(e)

	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	phaseForm := `(let ((entry (gethash ` + elispString(current) + ` agent-repl--open-progress)))
                     (if entry (format "%s" (plist-get entry :phase)) "gone"))`

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	raw := e.AwaitEvalFor(panelSettleBound, "the open-progress ladder to leave :requested",
		phaseForm,
		func(raw json.RawMessage) bool {
			var phase string
			if err := json.Unmarshal(raw, &phase); err != nil {
				return false
			}
			return phase != ":requested"
		})
	var phase string
	if err := json.Unmarshal(raw, &phase); err != nil {
		t.Fatalf("decode the ladder phase: %v", err)
	}

	terminal := e.EvalStrings(`(mapcar (lambda (p) (format "%s" p)) agent-repl--open-progress-terminal-phases)`)
	for _, bad := range terminal {
		if phase == bad {
			t.Fatalf("the open-progress ladder ended on the terminal phase %q", phase)
		}
	}
	if phase == "gone" {
		return
	}
	stages := e.EvalStrings(`(mapcar (lambda (entry) (format "%s" (car entry))) agent-repl--open-progress-stages)`)
	onLadder := false
	for _, stage := range stages {
		if phase == stage {
			onLadder = true
		}
	}
	if !onLadder {
		t.Fatalf("the ladder rests on %q, which is neither a stage of %q nor a terminal phase", phase, stages)
	}
}
