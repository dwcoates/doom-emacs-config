// The Emacs client layer, AREA F — find-file routing and the shared popup
// (EMACS-LAYER-SPEC.md scenarios 31-34). EMACS-ONLY: none of it exists on the
// wire, so a Connect-dialing test cannot reach any of it.
//
// The subject is `lisp/find-file-workspace.el', which is `:around' ADVICE on
// the display primitives (`switch-to-buffer',
// `pop-to-buffer-same-window') — not a replacement for `find-file'. That is
// why every scenario here calls the ORDINARY `find-file': calling the
// module's own routing helpers would step over the exact seam the advice
// lives on, and prove nothing about the entry a user actually uses.
//
// # No real git, and no injected shortcut past the routing
//
// Root detection is reached through the module's own DOCUMENTED injection
// point, `agent-repl-find-file-workspace-root-function' — "THE INJECTION
// POINT for root detection: tests bind this to a fixture so no test ever runs
// real git". The scripted fake git the world installs answers the daemon's
// git surface, not `rev-parse --show-toplevel', and the project ruling
// prohibits real git everywhere, so the fixture is the only correct answer
// here. NOTHING ELSE is stubbed: the advice, the four-case routing decision,
// the pending table, the refusal table, the reconcile hook and the window
// placement all run for real, against a real daemon Emacs itself launched.
//
// # Everything lives under HOME
//
// `agent-repl--ffw-under-home-p' refuses to route a root outside the user's
// home, and this layer's HOME is the Emacs scratch root, so every repository
// in this file is created UNDER `Emacs.Root'. A repository placed elsewhere
// in the scratch would make each of these tests pass vacuously, by never
// routing at all.
package e2e

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/integration/harness"
)

// popupWidthFloorFraction and popupWidthCeilingFraction bracket "about half
// the frame". `agent-repl-popup-width-fraction' is 0.5 and the popup rounds
// it against `frame-width', but a side window's final width is the window
// manager's answer and not the request, so the assertion is the CONTRACT
// ("half the frame") with room for the rounding, not the arithmetic.
const (
	popupWidthFloorFraction   = 0.3
	popupWidthCeilingFraction = 0.7
)

// ---------------------------------------------------------------------------
// THE AREA FIXTURE
// ---------------------------------------------------------------------------

// emacsFindFileFixture is an Emacs world with the daemon launched by Emacs,
// and helpers for making files under HOME.
type emacsFindFileFixture struct {
	World *EmacsWorld
	Emacs *Emacs
}

// repoUnderHome makes a scripted fake-git repository under HOME, which is
// where the routing's own home check requires it to be.
func (f *emacsFindFileFixture) repoUnderHome(t *testing.T, name string) *harness.Repo {
	t.Helper()
	return harness.NewRepoAt(t, filepath.Join(f.Emacs.Root, name))
}

// fileIn writes an ordinary text file into DIR. Not a store row and not a
// vendor JSONL line: a plain file, which is the thing a user visits.
func (f *emacsFindFileFixture) fileIn(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("e2e: mkdir for %s: %v", path, err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("e2e: write %s: %v", path, err)
	}
	return path
}

// windowSideOf answers the `window-side' parameter of the window showing
// PATH, as a string. Read as DATA off the window, never off the modeline.
func (f *emacsFindFileFixture) windowSideOf(path string) string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalString(`(let* ((buf (get-file-buffer ` + elispString(path) + `))
                                      (win (and buf (get-buffer-window buf t))))
                                 (format "%s" (and win (window-parameter win 'window-side))))`)
}
