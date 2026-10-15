package e2e

// doomtier_test.go — the Doom tier's tests (doomworld_test.go): what only a
// booted Doom shows, against the fake daemon. Each test boots its own Emacs.

import (
	"encoding/json"
	"strings"
	"testing"
)

func TestDoomPopupOpensAFileRightSideHalfWidth(t *testing.T) {
	t.Parallel()
	// Arrange
	box := requireSandbox(t)
	w := NewDoomWorld(t, box)
	f := &emacsFindFileFixture{Emacs: w.Emacs}
	e := w.Emacs
	repo := f.repoUnderHome(t, "popup")
	path := f.fileIn(t, repo.Dir, "lines.txt", "one\ntwo\nthree\nfour\nfive\n")
	const wantLine = 4

	// Act
	e.Eval(`(agent-repl-popup-open ` + elispString(path) + ` 4)`)

	// Assert
	if side := f.windowSideOf(path); side != "right" {
		t.Fatalf("the popup window's window-side = %q, want %q", side, "right")
	}
	frameWidth := e.EvalInt(`(frame-width)`)
	popupWidth := e.EvalInt(`(window-total-width (get-buffer-window (get-file-buffer ` + elispString(path) + `) t))`)
	floor := int(float64(frameWidth) * popupWidthFloorFraction)
	ceiling := int(float64(frameWidth) * popupWidthCeilingFraction)
	if popupWidth < floor || popupWidth > ceiling {
		t.Fatalf("the popup window is %d columns of a %d-column frame, want about half (%d..%d)", popupWidth, frameWidth, floor, ceiling)
	}
	if got := e.EvalInt(`(with-current-buffer (get-file-buffer ` + elispString(path) + `) (line-number-at-pos (point)))`); got != wantLine {
		t.Fatalf("point is on line %d, want %d: the popup's LINE is 1-indexed, as HostOpenInEditor.line is", got, wantLine)
	}
}

// TestDoomRegisteringAWorkspaceSelectsIt is Emacs's half of "a workspace
// registered while you stand in it is the daemon's current row" (d337583fcb):
// once the daemon's ref attaches, Emacs sends SelectWorkspace for it. The
// daemon's half, the roster stamping that row current, is
// daemon/integration's TestSelectWorkspaceStampsCurrentOnTheRoster.
func TestDoomRegisteringAWorkspaceSelectsIt(t *testing.T) {
	t.Parallel()
	// Arrange
	box := requireSandbox(t)
	w := NewDoomWorld(t, box)

	// Act
	ws := w.register(t, box, "repo")

	// Assert
	call := w.Fake.AwaitCall("SelectWorkspace", 1, emacsVerbBound)
	var body struct {
		Workspace struct {
			ID string `json:"id"`
		} `json:"workspace"`
	}
	if err := json.Unmarshal(call.Body, &body); err != nil || body.Workspace.ID != ws.ID {
		t.Fatalf("SelectWorkspace = %s (%v), want it to name the registered workspace's ref %q", call.Body, err, ws.ID)
	}
}

// TestDoomWorkspaceNavigationFollowsTheDrawnTabBar: the chords walk the bar
// the roster draws, wrapping, and `M-<n>' is the n-th DRAWN tab -- the
// module's minor-mode map winning Doom's own numerals in a booted Doom.
func TestDoomWorkspaceNavigationFollowsTheDrawnTabBar(t *testing.T) {
	t.Parallel()
	// Arrange: three workspaces, so a direction is observable.
	box := requireSandbox(t)
	w := NewDoomWorld(t, box)
	e := w.Emacs
	f := &emacsWorkspaceFixture{Emacs: e}
	a, b, c := w.register(t, box, "repo-a"), w.register(t, box, "repo-b"), w.register(t, box, "repo-c")
	// The drawn order is the ROSTER's, deliberately not registration order.
	w.pushRoster(t, c, b, c, a)
	order := awaitTabsOpen(e, emacsVerbBound, []string{a.Name, b.Name, c.Name})
	if strings.Join(order, ",") != strings.Join([]string{b.Name, c.Name, a.Name}, ",") {
		t.Fatalf("agent-repl-roster--tab-order = %v, want the roster's own order %v", order, []string{b.Name, c.Name, a.Name})
	}
	f.awaitCurrent(c.Name, "Emacs to follow the reordered roster's current")
	for keys, want := range map[string]string{
		"s-}": "agent-repl-switch-right",
		"s-{": "agent-repl-switch-left",
		"M-1": "agent-repl-switch-to-workspace-1",
		"M-2": "agent-repl-switch-to-workspace-2",
	} {
		if got := e.BindingFor(keys); got != want {
			t.Fatalf("%s resolves to %q in the running Doom, want %q", keys, got, want)
		}
	}

	// Act + Assert: `s-}' walks the bar left to right, wrapping, and `s-{'
	// right to left.
	f.standOn(order[0])
	f.pressAndLand("s-}", order[1], order)
	f.pressAndLand("s-}", order[2], order)
	f.pressAndLand("s-}", order[0], order)
	f.standOn(order[2])
	f.pressAndLand("s-{", order[1], order)
	f.pressAndLand("s-{", order[0], order)
	f.pressAndLand("s-{", order[2], order)

	// `M-2' is the second DRAWN tab, and `M-1' the first -- never Doom's
	// `main' perspective.
	f.standOn(order[0])
	f.pressAndLand("M-2", order[1], order)
	f.pressAndLand("M-1", order[0], order)
	if current, main := e.EvalString(`(format "%s" (agent-repl--ws-current-name))`), e.EvalString(`(format "%s" (agent-repl--ws-main-name))`); current == main {
		t.Fatalf("`M-1' landed on %q, Doom's own startup perspective, not the drawn bar %v", current, order)
	}
}

// TestDoomANewProjectDoesNotRecycleTheWorkspaceBeingLeft: registering a
// second project while standing in a created workspace gives the new project
// its own perspective, and the one being left keeps its perspective and its
// tab -- Doom's `+workspaces-switch-to-project-h' recycles it unless the
// module's policy is installed, which only a booted Doom can show.
func TestDoomANewProjectDoesNotRecycleTheWorkspaceBeingLeft(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace, then a created one the create selects.
	box := requireSandbox(t)
	w := NewDoomWorld(t, box)
	e := w.Emacs
	f := &emacsWorkspaceFixture{Emacs: e}
	if got := e.EvalString(`(format "%s" +workspaces-on-switch-project-behavior)`); got != "t" {
		t.Fatalf("+workspaces-on-switch-project-behavior is %q in the running Doom, want t: at any other value "+
			"`+workspaces-switch-to-project-h` may RECYCLE the workspace being left", got)
	}
	w.register(t, box, "repo")
	created := w.create(t, box, "repo-created")

	// Act: a second project registered, which switches projects out of the
	// created workspace.
	second := w.register(t, box, "repo-second")

	// Assert: the created workspace still has its perspective and its tab.
	if tabs := f.tabNames(); !containsString(tabs, created.Name) || !containsString(tabs, second.Name) {
		t.Fatalf("agent-repl--ws-tabline-names = %v, want both the created %q and the second %q", tabs, created.Name, second.Name)
	}
	if persps := e.EvalStrings(`(if (boundp 'persp-names-cache) persp-names-cache nil)`); !containsString(persps, created.Name) {
		t.Fatalf("persp-names-cache = %v, want the created workspace %q still in it: Doom recycled the workspace being left", persps, created.Name)
	}
}
