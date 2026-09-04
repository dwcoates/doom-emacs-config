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
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"claude-repld/integration/harness"
)

// emacsFindFileBound bounds one routed visit: the advice, the verb it may
// send, the daemon's answer, the roster push, and the placement Emacs makes
// out of it. Same shape of work as a workspace verb, so the same bound.
//
// PROVISIONAL for the reason EMACS-LAYER-SPEC.md's bounds table gives: the
// image's Emacs 28.2 cannot boot interactive Doom, so no healthy run exists
// to measure a tighter multiple against.
const emacsFindFileBound = DefaultTimeout

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

func newEmacsFindFileFixture(t *testing.T, box sandbox) *emacsFindFileFixture {
	t.Helper()
	w := NewEmacsWorld(t, box)
	w.Emacs.EnsureDaemon()
	return &emacsFindFileFixture{World: w, Emacs: w.Emacs}
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

// registerWorkspace onboards DIR through the ordinary command and answers the
// workspace name Emacs ended up knowing it by.
func (f *emacsFindFileFixture) registerWorkspace(t *testing.T, dir string) string {
	t.Helper()
	e := f.Emacs
	before := len(f.workspaceNames())
	e.Eval(`(agent-repl-add-project-workspace ` + elispString(dir) + `)`)
	raw := e.AwaitEvalFor(emacsFindFileBound, "the workspace to appear in Emacs's registry",
		`(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) names)`,
		func(raw json.RawMessage) bool { return len(decodeStrings(raw)) > before })
	names := decodeStrings(raw)
	return names[len(names)-1]
}

func (f *emacsFindFileFixture) workspaceNames() []string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalStrings(
		`(let (names) (maphash (lambda (k v) (when (plist-get v :project-dir) (push k names))) agent-repl--workspaces) names)`)
}

// findFileWithRoot visits PATH the ordinary way, with root detection pointed
// at ROOT for the duration of the visit. The `let' is the module's own
// injection point; `find-file' is the real command, so the real advice on the
// real display primitive is what runs.
func (f *emacsFindFileFixture) findFileWithRoot(path, root string) {
	f.Emacs.t.Helper()
	f.Emacs.Eval(`(let ((agent-repl-find-file-workspace-root-function
                        (lambda (_dir) ` + elispString(root) + `)))
                    (find-file ` + elispString(path) + `)
                    t)`)
}

// windowSideOf answers the `window-side' parameter of the window showing
// PATH, as a string. Read as DATA off the window, never off the modeline.
func (f *emacsFindFileFixture) windowSideOf(path string) string {
	f.Emacs.t.Helper()
	return f.Emacs.EvalString(`(let* ((buf (get-file-buffer ` + elispString(path) + `))
                                      (win (and buf (get-buffer-window buf t))))
                                 (format "%s" (and win (window-parameter win 'window-side))))`)
}

// hashKeys reads a hash table's keys as a list of strings.
func hashKeys(e *Emacs, table string) []string {
	e.t.Helper()
	return e.EvalStrings(`(let (ks) (maphash (lambda (k _v) (push (format "%s" k) ks)) ` + table + `) ks)`)
}

// ---------------------------------------------------------------------------
// #31 — VisitingAFileRoutesIntoItsOwningWorkspace
// ---------------------------------------------------------------------------

// TestEmacsVisitingAFileRoutesIntoItsOwningWorkspace is scenario 31: a file
// under a REGISTERED worktree lands in the workspace that owns it.
//
// Two claims, and the second is the one a naive implementation gets wrong:
// the owning workspace is SELECTED, and the file's window is NOT a side
// window. Routing places a file in the tab's largest non-agent-repl window;
// dropping it into a popup would be the module's own "a popup is not where a
// file belongs" rule broken.
func TestEmacsVisitingAFileRoutesIntoItsOwningWorkspace(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsFindFileFixture(t, box)
	e := f.Emacs

	repo := f.repoUnderHome(t, "owned")
	ws := f.registerWorkspace(t, repo.Dir)
	path := f.fileIn(t, repo.Dir, "owned.txt", "one\ntwo\nthree\n")

	// Leave the owning workspace before visiting, so "switched to it" is a
	// change this test caused rather than where it already was.
	e.Eval(`(tab-bar-new-tab)`)

	f.findFileWithRoot(path, repo.Dir)

	e.AwaitEvalFor(emacsFindFileBound, "the owning workspace to be selected",
		`(format "%s" (agent-repl--ws-current-name))`,
		func(raw json.RawMessage) bool {
			var got string
			return !isJSONNull(raw) && json.Unmarshal(raw, &got) == nil && got == ws
		})

	if side := f.windowSideOf(path); side != "nil" {
		t.Fatalf("the routed file's window has window-side %q, want none: a file is not placed in a side window", side)
	}
}

// ---------------------------------------------------------------------------
// #32 — AnUnroutableFileRecordsARefusal
// ---------------------------------------------------------------------------

// TestEmacsAnUnroutableFileRecordsARefusal is scenario 32: a root no
// workspace owns takes the CREATE branch, and when that verb refuses, the
// refusal is RECORDED and the file still opens.
//
// "The file is never lost" is the claim, and it is the one that matters: a
// routing that cannot acquire a workspace must fall through to ordinary
// display rather than swallow the visit. The create verb is pointed at a
// refusing function through the module's own documented injection point,
// because a REFUSAL is the precondition and there is no other way to make the
// daemon refuse an ordinary directory on demand.
func TestEmacsAnUnroutableFileRecordsARefusal(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsFindFileFixture(t, box)
	e := f.Emacs

	orphan := f.repoUnderHome(t, "unowned")
	path := f.fileIn(t, orphan.Dir, "orphan.txt", "no workspace owns me\n")

	e.Eval(`(let ((agent-repl-find-file-workspace-root-function
                    (lambda (_dir) ` + elispString(orphan.Dir) + `))
                  (agent-repl-find-file-workspace-create-function
                    (lambda (_root) (error "the workspace could not be created"))))
                (find-file ` + elispString(path) + `)
                t)`)

	refused := hashKeys(e, "agent-repl--ffw-refused")
	if len(refused) != 1 {
		t.Fatalf("agent-repl--ffw-refused = %v, want exactly the one refused root", refused)
	}

	// The file STILL OPENED, in an ordinary window.
	if !e.EvalBool(`(and (get-file-buffer ` + elispString(path) + `)
                          (get-buffer-window (get-file-buffer ` + elispString(path) + `) t)
                          t)`) {
		t.Fatalf("the file %s is not displayed after the refusal: a refused routing falls through, it never loses the file", path)
	}
	if side := f.windowSideOf(path); side != "nil" {
		t.Fatalf("the fallen-through file's window has window-side %q, want none", side)
	}
}

// ---------------------------------------------------------------------------
// #33 — PendingPlacementResolvesAfterRosterReconcile
// ---------------------------------------------------------------------------

// TestEmacsPendingPlacementResolvesAfterRosterReconcile is scenario 33, and
// the PENDING path is what makes this area more than a switch:
//
// When the verb answers but the workspace's tab is not there YET, the
// placement is HELD rather than dropped, and the file is deliberately shown
// nowhere in the meantime — "the user asked for it in its workspace, and a
// transient placement elsewhere is the thing this routing exists to stop".
// The tab's later ARRIVAL, on `agent-repl-roster-update-functions', is what
// fires it.
//
// The hold is produced honestly: the create verb is pointed at a function
// that does not open anything, so the acquire path finds no tab when it
// returns. The DRAIN is then produced by the real thing — an ordinary
// `agent-repl-add-project-workspace' on the same directory, whose roster push
// reconciles the tab into existence.
func TestEmacsPendingPlacementResolvesAfterRosterReconcile(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsFindFileFixture(t, box)
	e := f.Emacs

	repo := f.repoUnderHome(t, "later")
	path := f.fileIn(t, repo.Dir, "later.txt", "held until my tab arrives\n")

	e.Eval(`(let ((agent-repl-find-file-workspace-root-function
                    (lambda (_dir) ` + elispString(repo.Dir) + `))
                  (agent-repl-find-file-workspace-create-function
                    (lambda (_root) nil)))
                (find-file ` + elispString(path) + `)
                t)`)

	if pending := hashKeys(e, "agent-repl--ffw-pending"); len(pending) != 1 {
		t.Fatalf("agent-repl--ffw-pending = %v, want exactly the one held placement", pending)
	}
	if refused := hashKeys(e, "agent-repl--ffw-refused"); len(refused) != 0 {
		t.Fatalf("agent-repl--ffw-refused = %v, want none: the verb answered, it did not refuse", refused)
	}

	// The tab ARRIVES, through the ordinary register command and the roster
	// push it provokes. `agent-repl--ffw-pending-fire' rides the reconcile
	// hook, so the drain is the daemon's push reaching Emacs, not a poke.
	f.registerWorkspace(t, repo.Dir)

	e.AwaitEvalFor(emacsFindFileBound, "the held placement to drain on the roster reconcile",
		`(hash-table-count agent-repl--ffw-pending)`,
		func(raw json.RawMessage) bool {
			var n int
			return !isJSONNull(raw) && json.Unmarshal(raw, &n) == nil && n == 0
		})

	if !e.EvalBool(`(and (get-buffer-window (get-file-buffer ` + elispString(path) + `) t) t)`) {
		t.Fatalf("the held file %s was never placed after its tab arrived", path)
	}
}

// ---------------------------------------------------------------------------
// #34 — PopupOpensRightSideHalfWidth
// ---------------------------------------------------------------------------
//
// `agent-repl-popup-open' is THE ONE shared subroutine every open-a-file
// affordance must call, and divergence between call sites is a defect by
// ruling. Its two documented shapes are asserted as two tests, one edge case
// each: a file with a line, and a directory.

// TestEmacsPopupOpensAFileRightSideHalfWidth is scenario 34's file case: the
// window's side is `right', its width is about half the frame, and point is
// on the requested (1-indexed) line.
func TestEmacsPopupOpensAFileRightSideHalfWidth(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsFindFileFixture(t, box)
	e := f.Emacs

	repo := f.repoUnderHome(t, "popup")
	path := f.fileIn(t, repo.Dir, "lines.txt", "one\ntwo\nthree\nfour\nfive\n")
	const wantLine = 4

	e.Eval(`(agent-repl-popup-open ` + elispString(path) + ` 4)`)

	if side := f.windowSideOf(path); side != "right" {
		t.Fatalf("the popup window's window-side = %q, want %q", side, "right")
	}

	frameWidth := e.EvalInt(`(frame-width)`)
	popupWidth := e.EvalInt(`(window-total-width
                               (get-buffer-window (get-file-buffer ` + elispString(path) + `) t))`)
	floor := int(float64(frameWidth) * popupWidthFloorFraction)
	ceiling := int(float64(frameWidth) * popupWidthCeilingFraction)
	if popupWidth < floor || popupWidth > ceiling {
		t.Fatalf("the popup window is %d columns of a %d-column frame, want about half (%d..%d)",
			popupWidth, frameWidth, floor, ceiling)
	}

	if got := e.EvalInt(`(with-current-buffer (get-file-buffer ` + elispString(path) + `)
                            (line-number-at-pos (point)))`); got != wantLine {
		t.Fatalf("point is on line %d, want %d: the popup's LINE is 1-indexed, as HostOpenInEditor.line is", got, wantLine)
	}
}

// TestEmacsPopupOpensADirectoryInDired is scenario 34's directory case: the
// same one subroutine answers a directory with a DIRED buffer, in the same
// right-side popup. A per-caller variant for directories is exactly the
// divergence popup.el exists to prevent.
func TestEmacsPopupOpensADirectoryInDired(t *testing.T) {
	t.Parallel()
	box := requireSandbox(t)
	f := newEmacsFindFileFixture(t, box)
	e := f.Emacs

	repo := f.repoUnderHome(t, "popup-dir")
	f.fileIn(t, repo.Dir, "inside.txt", "a file so the directory is not empty\n")

	e.Eval(`(agent-repl-popup-open ` + elispString(repo.Dir) + `)`)

	// The dired buffer is found by what it IS — a `dired-mode' buffer whose
	// `default-directory' is the popped directory — rather than by a name
	// dired composed, which is a rendering detail.
	const diredWindowSide = `(let ((dir (file-name-as-directory (expand-file-name %s))))
                               (catch 'found
                                 (dolist (buf (buffer-list) "no-dired-buffer")
                                   (with-current-buffer buf
                                     (when (and (eq major-mode 'dired-mode)
                                                (equal (expand-file-name default-directory) dir))
                                       (throw 'found
                                              (let ((win (get-buffer-window buf t)))
                                                (format "%%s" (and win (window-parameter win 'window-side))))))))))`

	got := e.EvalString(fmt.Sprintf(diredWindowSide, elispString(repo.Dir)))
	switch got {
	case "right":
		// The one shared subroutine answered a directory in dired, in the
		// same right-side popup a file gets.
	case "no-dired-buffer":
		t.Fatalf("agent-repl-popup-open on a directory produced no dired buffer for %s", repo.Dir)
	default:
		t.Fatalf("the dired popup's window-side = %q, want %q", got, "right")
	}
}
