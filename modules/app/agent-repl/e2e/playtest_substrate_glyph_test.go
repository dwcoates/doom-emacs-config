//go:build playtest

package e2e

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"image"
	"image/color"
	"os"
	"strings"
	"testing"
)

// THE SUBSTRATE'S GLYPH PRECONDITION, MEASURED IN PIXELS.
//
// playtest_substrate_font_test.go asks fontconfig which face it would pick.
// This file asks the RENDERER what it actually drew, because the two can
// disagree and the defect that produced this file was invisible to the
// first question.
//
// WHAT HAPPENED. The image's color-emoji rule bound Noto Color Emoji into
// every generic family with `binding="same"`, which is STRONG for a family
// an application names, while the distro's own DejaVu entries in those same
// lists are bound weakly. fontconfig ranks a strong family match above a
// weak one, so the emoji font won every codepoint BOTH fonts carry. Noto
// Color Emoji's charset is emoji plus U+0020, U+0023, U+002A and
// U+0030-U+0039 -- the pieces of a keycap sequence -- so letters fell
// through to DejaVu and looked right while every DIGIT and every SPACE was
// drawn from a color bitmap: uncolorable, because a bitmap glyph ignores the
// CSS `color`, and laid out on the emoji advance, which is about twice a
// text space. Owner 10 photographed the digits ("3 . 2 k", byte-identical
// gray between runs whose CSS color differed) and owner 5 photographed the
// spacing (every line of webapp prose double-spaced between words).
//
// Neither is visible to `fc-match emoji`, which kept answering correctly the
// whole time. So the assertions here are on the framebuffer: a glyph is
// drawn in the color the page asked for, or it is not.
//
// These run inside the sandbox, in the real `xwidget-webkit` webview, on the
// real Xvfb -- the same renderer and the same X server every playtest
// picture is taken through.

// glyphMarkerColor is the CSS color the page paints its glyph in.
//
// IT IS ARBITRARY ON PURPOSE, and the arrangement PROVES it is unused: every
// measurement below is preceded by a capture of the same page with the glyph
// hidden, which must carry none of this color anywhere on the screen. So a
// count of these pixels is a count of the glyph, and a theme that happened
// to paint something near it fails loudly in the arrangement rather than
// quietly satisfying an assertion.
var glyphMarkerColor = color.RGBA{R: 7, G: 199, B: 251, A: 255}

// glyphMarkerTolerance is how far a pixel may sit from the marker on each
// channel and still count. A glyph's edges are antialiased against the
// page's white, so its outline shades away from the marker; the interior of
// a 120px glyph is the marker exactly.
const glyphMarkerTolerance = 8

// glyphFontSizePx is the size the page draws its glyph at.
//
// Large, because the measurements are pixel counts and pixel extents: at a
// prose size a whole glyph is a few hundred pixels and one antialiased row
// is a large fraction of it, while at 120px the interior dominates and the
// advance differences the spacing check reads are tens of pixels apart.
const glyphFontSizePx = 120

// glyphInkFloor is the fewest marker pixels a drawn 120px glyph may carry.
//
// MEASURED on the image this landed with: "3" inks 1795 marker pixels and
// "k" 1699. The floor is an order of magnitude under the smaller of those,
// because what it separates is not one glyph from another -- it is a glyph
// DRAWN IN THE PAGE'S COLOR from one drawn as a color bitmap that ignores
// it, and the latter inks none at all. Every count is logged, so a run that
// drifts toward the floor says so before it crosses it.
const glyphInkFloor = 200

// glyphEmojiColorMargin is how many more distinct colors the screen must
// carry with a color emoji on it than with nothing on it.
//
// A color bitmap glyph is a photograph: MEASURED, 🎯 alone took the screen
// from 166 distinct colors to 1684, a gain of 1518. A tofu box would add one
// and a monochrome outline a handful, so a margin two orders of magnitude
// under the measured gain separates "the color font was used" from either.
const glyphEmojiColorMargin = 32

// TestPlaytestSandboxDrawsTextGlyphsFromATextFace is the pixel half of the
// image's font precondition.
//
// ONE EMACS, SEVERAL EDGE CASES, and that is deliberate rather than a
// shortcut: each subtest below asserts exactly one thing, and every one of
// them is about the SAME page in the SAME webview. Giving each its own
// top-level test would boot four real Emacsen on four Xvfbs, through the
// layer's own serialized Emacs slot, to make four assertions about one
// renderer's font choice.
func TestPlaytestSandboxDrawsTextGlyphsFromATextFace(t *testing.T) {
	page := newGlyphPage(t)

	t.Run("a digit follows the page's CSS color", func(t *testing.T) {
		// Arrange: the same page with nothing drawn in it, which fixes that
		// the marker color belongs to the glyph and to nothing else.
		page.requireMarkerAbsentWhenHidden(t)

		// Act.
		inked := page.inkOf(t, "3")
		t.Logf("the digit \"3\" inked %d pixels of the page's own color", inked)

		// Assert.
		if inked < glyphInkFloor {
			t.Errorf("the digit \"3\" inked %d pixels of the page's own color, want at least %d: the "+
				"digit is being drawn from the color emoji font (whose charset carries U+0030-U+0039 "+
				"for keycap sequences), and a color bitmap ignores the CSS `color` -- so every number "+
				"in every playtest capture is uncolorable. See e2e/sandbox/fontconfig/99-agent-repl-emoji.conf",
				inked, glyphInkFloor)
		}
	})

	t.Run("a letter follows the page's CSS color", func(t *testing.T) {
		// Arrange: the letter is the CONTROL for the digit above. It was
		// always right, because no Latin letter is in the emoji font's
		// charset -- so a run where this one also fails is a different
		// fault, and the two failing apart is what says so.
		page.requireMarkerAbsentWhenHidden(t)

		// Act.
		inked := page.inkOf(t, "k")
		t.Logf("the letter \"k\" inked %d pixels of the page's own color", inked)

		// Assert.
		if inked < glyphInkFloor {
			t.Errorf("the letter \"k\" inked %d pixels of the page's own color, want at least %d: the "+
				"webview is not drawing text in the color the page asked for at all, which is a wider "+
				"fault than the digit's", inked, glyphInkFloor)
		}
	})

	t.Run("a space between letters takes a text-face advance", func(t *testing.T) {
		// Arrange: three strings whose inked extents differ by exactly one
		// advance each. The extent of a run is (sum of advances) minus the
		// side bearings of its first and last glyph, and all three runs
		// begin and end with "a" -- so the bearings cancel and each
		// difference IS an advance, with no constant to calibrate.
		page.requireMarkerAbsentWhenHidden(t)

		// Act.
		one := page.extentOf(t, "a")
		two := page.extentOf(t, "aa")
		spaced := page.extentOf(t, "a a")
		letterAdvance := two - one
		spaceAdvance := spaced - two
		t.Logf("at font-size %dpx: \"a\" spans %dpx, \"aa\" %dpx, \"a a\" %dpx -- an \"a\" advances %dpx and a space %dpx",
			glyphFontSizePx, one, two, spaced, letterAdvance, spaceAdvance)

		// Assert: in any text face a space is NARROWER than a lowercase "a"
		// (DejaVu Sans: 0.318em against 0.613em, and MEASURED here at 38px
		// against 74px at a font-size of 120). The emoji font's advance
		// is about 1.275em, so a space drawn from it is twice the letter --
		// which is the doubled inter-word spacing owner 5 photographed
		// across every line of webapp prose.
		if spaceAdvance >= letterAdvance {
			t.Errorf("a space advanced %dpx and an \"a\" advanced %dpx at font-size %dpx; a space must be "+
				"the narrower of the two. The space is being drawn from the color emoji font (U+0020 is "+
				"in its charset), whose advance is about 1.275em, so every line of prose in every "+
				"playtest capture is double-spaced between words. See "+
				"e2e/sandbox/fontconfig/99-agent-repl-emoji.conf",
				spaceAdvance, letterAdvance, glyphFontSizePx)
		}
	})

	t.Run("an emoji still renders in color", func(t *testing.T) {
		// Arrange: the blank page's own color count, so the emoji's
		// contribution is measured against this screen rather than a number
		// written down here. This is also the check the whole fix must not
		// break -- the rule exists so emoji are not tofu.
		blank := page.distinctColorsWhenHidden(t)

		// Act.
		withEmoji := page.distinctColorsOf(t, "\U0001F3AF")
		t.Logf("the screen carried %d distinct colors with the emoji drawn and %d with nothing, a gain of %d",
			withEmoji, blank, withEmoji-blank)

		// Assert.
		if withEmoji < blank+glyphEmojiColorMargin {
			t.Errorf("the screen carried %d distinct colors with 🎯 drawn on it and %d with nothing, "+
				"a gain of %d, want at least %d: a color bitmap glyph contributes hundreds, so this "+
				"emoji drew as tofu or as a monochrome outline and every playtest picture's tree "+
				"glyphs are unreviewable", withEmoji, blank, withEmoji-blank, glyphEmojiColorMargin)
		}
		// AND IT IS NOT THE PAGE'S COLOR, which is the other half of "in
		// color": a color bitmap ignores the CSS `color` it is under.
		if inked := page.markerPixels(t); inked >= glyphInkFloor {
			t.Errorf("🎯 inked %d pixels of the page's own CSS color; it is being drawn as a text glyph "+
				"in the page's color rather than as the color bitmap the font carries", inked)
		}
	})
}

// ---------------------------------------------------------------------------
// THE PAGE
// ---------------------------------------------------------------------------

// glyphPage is one webview showing one glyph at a time, and the framebuffer
// reads that measure it.
//
// It carries no daemon and no webapp: what is under test is the IMAGE's font
// resolution inside WebKit, so the page is a data: URL of this file's own
// making. Everything the product would add is noise here.
type glyphPage struct {
	e *Emacs
}

// newGlyphPage brings up an Emacs, sizes its frame to the whole screen, and
// puts a webview in it.
func newGlyphPage(t *testing.T) *glyphPage {
	t.Helper()
	box := requireSandbox(t)
	w := NewEmacsWorld(t, box)
	e := w.Emacs

	// The frame is sized before anything is drawn in it, for the reason
	// `playbook.prepareFrame` gives: the webview is laid out in real pixels,
	// so a frame resized afterwards would measure a page laid out for a
	// different window. It goes through `fitFrameToDisplay` -- the same
	// helper the playbooks use, and not a second copy of the arithmetic --
	// because a frame sized past the screen edge is read here as a page that
	// drew the wrong pixels. The blink is off because `settleFramebuffer`
	// waits for the screen to hold still, and a blinking cursor guarantees
	// it never does.
	e.Eval(`(progn (blink-cursor-mode -1) t)`)
	fitFrameToDisplay(t, e)

	p := &glyphPage{e: e}
	p.show(t, "", false)
	return p
}

// glyphProbeSetup installs the page probe, in the two-eval shape
// `playtestProbeSetup` explains: `xwidget-webkit-execute-script` is
// asynchronous, so each call issues the script again and answers what the
// PREVIOUS issue's callback stored.
const glyphProbeSetup = `(progn
             (defvar agent-repl-glyphtest--js nil)
             (defun agent-repl-glyphtest--probe (script)
               (let ((xw (xwidget-webkit-current-session)))
                 (unless xw (error "no live webkit session for the glyph page"))
                 (xwidget-webkit-execute-script
                  xw script
                  (lambda (value) (setq agent-repl-glyphtest--js (format "%s" value))))
                 agent-repl-glyphtest--js))
             t)`

// glyphPageHTML builds the page: a white ground, and one run of text drawn
// at `glyphFontSizePx` in the marker color.
//
// THE FONT STACK IS THE WEBAPP'S OWN -- `ui-sans-serif, system-ui,
// sans-serif`, from webapp/src/styles.css -- because that is the stack whose
// resolution is under test. A stack written here could resolve differently
// from the one the pictures are taken of.
func glyphPageHTML(text string, hidden bool) string {
	visibility := "visible"
	if hidden {
		visibility = "hidden"
	}
	return `<!doctype html><html><head><meta charset="utf-8"><title>glyph</title><style>
html, body { margin: 0; padding: 0; background: #ffffff; }
#glyph {
  position: absolute; left: 40px; top: 40px;
  font-family: ui-sans-serif, system-ui, sans-serif;
  font-size: ` + fmt.Sprint(glyphFontSizePx) + `px;
  line-height: 1.4;
  white-space: pre;
  color: rgb(` + fmt.Sprintf("%d,%d,%d", glyphMarkerColor.R, glyphMarkerColor.G, glyphMarkerColor.B) + `);
  visibility: ` + visibility + `;
}
</style></head><body><span id="glyph">` + htmlEscape(text) + `</span></body></html>`
}

// htmlEscape is the four-character escape a text node needs. The strings
// this file draws are single glyphs, but they are still not pasted into
// markup unescaped.
func htmlEscape(s string) string {
	r := strings.NewReplacer("&", "&amp;", "<", "&lt;", ">", "&gt;", `"`, "&quot;")
	return r.Replace(s)
}

// show navigates the webview to a page drawing TEXT, and returns once the
// page has mounted.
func (p *glyphPage) show(t *testing.T, text string, hidden bool) {
	t.Helper()
	// base64, not percent-encoding: the strings drawn here carry spaces and
	// astral-plane emoji, and a data: URL that had to escape both by hand is
	// one more thing between the test and what it measures.
	url := "data:text/html;charset=utf-8;base64," +
		base64.StdEncoding.EncodeToString([]byte(glyphPageHTML(text, hidden)))

	p.e.Eval(`(progn (delete-other-windows) (xwidget-webkit-browse-url ` + elispString(url) + `) t)`)
	p.e.AwaitEvalFor(playtestPageBound, "the glyph page's webview to be live",
		`(let ((xw (xwidget-webkit-current-session)))
           (and xw (xwidget-webkit-uri xw)))`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	p.e.Eval(glyphProbeSetup)

	// The page is MOUNTED when its own span is in the document with the text
	// this navigation asked for -- a URI is not a page, and a webview that
	// still holds the PREVIOUS glyph would answer every question about it.
	p.e.Eval(`(setq agent-repl-glyphtest--js nil)`)
	p.e.AwaitEvalFor(playtestPageBound, "the glyph page to hold the text this step navigated to",
		`(agent-repl-glyphtest--probe `+elispString(pageYes(
			`document.readyState === "complete" && document.getElementById("glyph") !== null && `+
				`document.getElementById("glyph").textContent === `+jsString(text)))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// awaitPainted blocks until the webview has delivered its own frames for the
// DOM now in it. See playtestPaintFrames: without this a read photographs
// whatever the offscreen surface happened to hold.
func (p *glyphPage) awaitPainted(t *testing.T, token string) {
	t.Helper()
	p.e.Eval(`(setq agent-repl-glyphtest--js nil)`)
	p.e.AwaitEvalFor(playtestPaintBound, "the glyph page to deliver its own frames",
		`(agent-repl-glyphtest--probe `+elispString(playtestPaintGateScript(token))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// capture redraws the frame, waits for the page's frames, and decodes the X
// server's screen memory once it has held still. It is `playbook.capture`'s
// mechanism without the artifact writing, which this suite has nowhere to
// put: these tests run under the plain `-run TestPlaytestSandbox` gate, with
// no playtest artifacts root set.
func (p *glyphPage) capture(t *testing.T, token string) *image.RGBA {
	t.Helper()
	p.redraw(t)
	p.awaitPainted(t, token)
	p.redraw(t)

	body, _, _ := settleFramebuffer(func() []byte {
		p.e.Eval(`(progn (redraw-frame) (redisplay t) t)`)
		return p.readFramebuffer(t)
	}, realSettleClock{})

	img, err := decodeXWD(body)
	if err != nil {
		t.Fatalf("decode the framebuffer at %s: %v", p.e.Display.FramebufferPath, err)
	}
	return img
}

// redraw garbages the frame twice, for the double-buffer reason
// `playbook.redrawFrame` states.
func (p *glyphPage) redraw(t *testing.T) {
	t.Helper()
	p.e.Eval(`(progn
             (force-mode-line-update t)
             (redraw-frame)
             (redisplay t)
             (redraw-frame)
             (redisplay t)
             t)`)
}

// readFramebuffer copies the X server's live screen memory.
func (p *glyphPage) readFramebuffer(t *testing.T) []byte {
	t.Helper()
	body, err := os.ReadFile(p.e.Display.FramebufferPath)
	if err != nil {
		t.Fatalf("read the Xvfb framebuffer at %s: %v", p.e.Display.FramebufferPath, err)
	}
	return body
}

// ---------------------------------------------------------------------------
// THE MEASUREMENTS
// ---------------------------------------------------------------------------

// inkOf draws TEXT and answers how many pixels of the page's own color
// reached the glass.
func (p *glyphPage) inkOf(t *testing.T, text string) int {
	t.Helper()
	p.show(t, text, false)
	return p.markerPixels(t)
}

// markerPixels counts the page's own color on the CURRENT screen.
func (p *glyphPage) markerPixels(t *testing.T) int {
	t.Helper()
	img := p.capture(t, glyphToken())
	return countColorWithin(img, glyphMarkerColor, glyphMarkerTolerance)
}

// extentOf draws TEXT and answers the width, in pixels, of the band its
// marker-colored pixels span.
func (p *glyphPage) extentOf(t *testing.T, text string) int {
	t.Helper()
	p.show(t, text, false)
	img := p.capture(t, glyphToken())

	minX, maxX, found := markerColumns(img, glyphMarkerColor, glyphMarkerTolerance)
	if !found {
		t.Fatalf("%q drew no pixels of the page's own color at all, so its extent cannot be measured; "+
			"the digit and letter checks in this test name what that means", text)
	}
	return maxX - minX + 1
}

// distinctColorsOf draws TEXT and answers how many distinct colors the
// screen then carries.
func (p *glyphPage) distinctColorsOf(t *testing.T, text string) int {
	t.Helper()
	p.show(t, text, false)
	return distinctColors(p.capture(t, glyphToken()))
}

// distinctColorsWhenHidden answers the same for the page with nothing drawn
// in it -- the baseline every emoji claim is made against.
func (p *glyphPage) distinctColorsWhenHidden(t *testing.T) int {
	t.Helper()
	p.show(t, "\U0001F3AF", true)
	return distinctColors(p.capture(t, glyphToken()))
}

// requireMarkerAbsentWhenHidden is the arrangement every ink measurement
// rests on: with the glyph hidden, NOTHING on this screen is the marker
// color, so a later count of it is a count of the glyph.
func (p *glyphPage) requireMarkerAbsentWhenHidden(t *testing.T) {
	t.Helper()
	p.show(t, "0123456789 abc", true)
	if stray := p.markerPixels(t); stray != 0 {
		t.Fatalf("%d pixels of the marker color rgb(%d,%d,%d) are on the screen with the glyph HIDDEN; "+
			"the editor's own chrome is painting near it, so counting that color would not count the "+
			"glyph. Move glyphMarkerColor.", stray, glyphMarkerColor.R, glyphMarkerColor.G, glyphMarkerColor.B)
	}
}

// markerColumns answers the leftmost and rightmost columns carrying a pixel
// within TOLERANCE of WANT.
func markerColumns(img *image.RGBA, want color.RGBA, tolerance uint8) (minX, maxX int, found bool) {
	near := func(a, b uint8) bool {
		if a > b {
			a, b = b, a
		}
		return b-a <= tolerance
	}
	bounds := img.Bounds()
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			c := img.RGBAAt(x, y)
			if !near(c.R, want.R) || !near(c.G, want.G) || !near(c.B, want.B) {
				continue
			}
			if !found || x < minX {
				minX = x
			}
			if !found || x > maxX {
				maxX = x
			}
			found = true
		}
	}
	return minX, maxX, found
}

// glyphToken numbers a capture's paint request within this page, so a wait
// can never be satisfied by the frames an earlier capture asked for.
func glyphToken() string {
	return fmt.Sprintf("glyph-%d", playtestPaintToken.Add(1))
}
