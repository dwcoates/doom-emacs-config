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
	}
	// The registry is a hash, so its iteration order says nothing; the
	// current workspace is what the panel verbs act on, and it is the one
	// most recently added.
	current := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`)
	if current == "" || current == "nil" {
		t.Fatal("no current workspace after registering one")
	}
	return w, names
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

	// Select a window that is NOT the composer, so the command has somewhere
	// to move point FROM.
	e.Eval(`(progn (select-window (car (window-list))) t)`)

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

	openPanel(e)
	awaitPanelWindows(e, frontendPrefix, panelPrefix)

	// An ordinary, non-agent work window, selected.
	e.Eval(`(progn (select-window (split-window))
                   (switch-to-buffer (get-buffer-create "*e2e-work*"))
                   t)`)
	before := len(windowBuffers(e))

	if want, got := "agent-repl-fullscreen-and-focus", e.LeaderBinding("w f"); got != want {
		t.Fatalf("SPC w f resolves to %q, want %q: the module's `map!' leader form did not take", got, want)
	}

	e.Leader("w f")
	if !e.EvalBool(`(and agent-repl--window-fullscreen-config t)`) {
		t.Fatal("the first SPC w f saved no window configuration, so there is nothing to restore to")
	}

	e.Leader("w f")
	if e.EvalBool(`(and agent-repl--window-fullscreen-config t)`) {
		t.Fatal("the second SPC w f left the saved window configuration standing")
	}
	if after := len(windowBuffers(e)); after != before {
		t.Fatalf("the window count did not return to its starting value: %d -> %d", before, after)
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
