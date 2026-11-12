package e2e

import (
	"encoding/json"
	"path/filepath"
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
