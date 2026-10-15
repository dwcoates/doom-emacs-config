package e2e

// doomtier_test.go — the Doom tier's tests (doomworld_test.go): what only a
// booted Doom shows, against the fake daemon. Each test boots its own Emacs.

import "testing"

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
