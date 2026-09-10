//go:build playtest

package e2e

import "testing"

// THE SUBSTRATE'S PROOF THAT THE WHOLE FRAME IS ON THE GLASS.
//
// WHAT WENT WRONG. Every playbook sized its frame with `(set-frame-size
// frame 1280 1024 t)`, and a pixelwise `set-frame-size` sets the frame's TEXT
// AREA -- which excludes the tab bar, the fringes and the scroll bar. So the
// frame that got photographed was NATIVELY 1296x1060, at position (0,0), on a
// 1280x1024 display.
//
// MEASURED, in this image, before the fix:
//
//	                          no panel            panel open
//	frame native size         1296x1060           1296x1060
//	frame position            (0 . 0)             (0 . 0)
//	display                   1280x1024           1280x1024
//	windows (top, height)     scratch: 36, 1006   frontend: 36, 772
//	                                              input:   808, 234
//
// The sole window with no panel therefore ended at y=1042: its mode line drew
// at 1024..1042 and the echo area at 1042..1060, both ENTIRELY off a screen
// that stops at 1023. Playtest owner 3 filed it from the pictures -- an
// editor with no mode line and no echo area anywhere in the capture -- and it
// is the likely shape of the "blank frame" evidence too. With a panel up the
// same 36px were missing; the interior mode line between the two windows
// (drawn at 790..808) is the only reason those captures looked normal.
//
// The cure is `fitFrameToDisplay`: it asks for the text size that makes the
// NATIVE size the display's, computed from the frame's own chrome. This file
// is what holds it, in both states, against the real Emacs on the real Xvfb
// -- the arithmetic is only right if the chrome it reads is the chrome the X
// server drew.

// frameStates are the two shapes a playbook photographs an editor in. They
// are the rows of every table below, because the defect was invisible in one
// of them and glaring in the other.
var frameStates = []struct {
	name    string
	arm     string
	arrange func(t *testing.T, s *playtestScenario)
}{
	{
		name: "with no panel up",
		arm:  "no-panel",
		arrange: func(t *testing.T, s *playtestScenario) {
			t.Helper()
		},
	},
	{
		name: "with a panel up",
		arm:  "panel",
		arrange: func(t *testing.T, s *playtestScenario) {
			t.Helper()
			repository := s.repoAt(t, "repo")
			s.register(t, repository.Dir)
			s.openPanel(t)
		},
	},
}

// TestPlaytestTheFrameFitsBelowTheDisplayEdge is the frame's own claim: all
// of it is on the screen the capture reads.
//
// It is asserted as `position + native height <= display height` rather than
// as an equality with the declared geometry, because that is the property a
// capture actually depends on -- a frame one pixel past the bottom edge loses
// the echo area, and no other check in this suite can see that. The geometry
// equality is `fitFrameToDisplay`'s own, made where the size is set.
func TestPlaytestTheFrameFitsBelowTheDisplayEdge(t *testing.T) {
	for _, state := range frameStates {
		t.Run(state.name, func(t *testing.T) {
			// Arrange.
			t.Parallel()
			s := newPlaytestScenario(t, "00-frame-fits-"+state.arm,
				"The substrate's own proof that the whole Emacs frame is on the screen every capture "+
					"is read off, so no picture in any playbook is missing its mode line or its echo area.")
			state.arrange(t, s)

			// Act.
			top := s.E.EvalInt(`(cdr (frame-position (selected-frame)))`)
			height := s.E.EvalInt(`(frame-native-height (selected-frame))`)
			display := s.E.EvalInt(`(display-pixel-height)`)

			// Assert.
			if top+height > display {
				t.Errorf("the frame is %dpx tall at y=%d, so it runs to y=%d on a display %dpx high: "+
					"the bottom %dpx of it -- the mode line and the echo area -- are off the glass and "+
					"absent from every capture taken of it",
					height, top, top+height, display, top+height-display)
			}
		})
	}
}

// TestPlaytestTheModeLineIsOnTheGlass is the OTHER half, and it is not the
// same claim.
//
// A frame that fits can still lay its windows out past its own bottom edge --
// and it is the mode line, not the frame, that a reviewer reads a playbook's
// pictures for. So this asks the window tree directly: the lowest row any
// window occupies is its mode line's last row, and that row must be a row the
// display carries.
func TestPlaytestTheModeLineIsOnTheGlass(t *testing.T) {
	for _, state := range frameStates {
		t.Run(state.name, func(t *testing.T) {
			// Arrange.
			t.Parallel()
			s := newPlaytestScenario(t, "00-mode-line-on-glass-"+state.arm,
				"The substrate's own proof that the mode line of the bottom window is drawn inside the "+
					"display, so a reviewer reading a capture sees the editor's own status line in it.")
			state.arrange(t, s)

			// Act: the bottom edge of the lowest window, which is the row
			// after its mode line's last, in the frame's own pixels.
			bottom := s.E.EvalInt(`(apply #'max (mapcar (lambda (w)
                             (+ (window-pixel-top w) (window-pixel-height w)))
                           (window-list nil 'never)))`)
			top := s.E.EvalInt(`(cdr (frame-position (selected-frame)))`)
			display := s.E.EvalInt(`(display-pixel-height)`)

			// Assert.
			if top+bottom > display {
				t.Errorf("the lowest window ends at frame row %d, so its mode line draws at screen row "+
					"%d on a display %dpx high: it is %dpx past the bottom edge, so no capture of this "+
					"frame carries a mode line at all",
					bottom, top+bottom-1, display, top+bottom-display)
			}
		})
	}
}
