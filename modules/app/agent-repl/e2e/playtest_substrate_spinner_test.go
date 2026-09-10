//go:build playtest

package e2e

import (
	"encoding/json"
	"fmt"
	"image"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// THE SUBSTRATE'S RUNNING-MARKER PRECONDITION, MEASURED IN PIXELS.
//
// WHAT HAPPENED. Owner 13 filed every `Bash … running…` badge in both of
// their playbooks as drawing an EMPTY BOX where the orange running marker
// belongs, and read it as the font fix next door taking the marker's glyph
// away. IT IS NOT A GLYPH. The marker inside that pill is `.tool-spinner`, a
// CSS-drawn ring (`border-radius: 50%` over a `0.7em` box, its top border in
// `--thinking`), and no font resolves it -- which is also why
// playtest_substrate_font_test.go's fc-match questions kept answering
// correctly while the pictures stayed wrong.
//
// The arc is laid out from the start at `opacity: 0`, on purpose, so the
// badge does not jump; `tool-run-appear` raises it, after a 1s delay that is
// the whole mechanic (an arc appears only for a call the user actually waits
// on). The capture's photographer's flag then freezes every animation where
// it stands -- and an animation frozen DURING its delay never leaves it. So
// the flag held the arc at the one opacity the eye never sees it rest at,
// and every running card in every playbook is photographed inside that
// second: a playbook asserts `data-state="running"` and captures on the next
// step, and a rewrite restarts the delay.
//
// MEASURED, owner 13's two green runs: all six `running…` badges across
// `13-shell` and `13-detached-shell` drew a reserved but empty box.
//
// The fix is a photographer's rule beside the one that pauses
// (`:root[data-motion="paused"] .tool-spinner { opacity: 1 }` in
// webapp/src/styles.css), and this file is the assertion that it holds
// against the SHIPPED SHEET, in the real `xwidget-webkit` webview, on the
// real Xvfb -- the same renderer every playtest picture is taken through.

// spinnerRootFontSizePx is the root font size the stage sets.
//
// Large, because `.badge`'s `0.7rem` and `.tool-spinner`'s `0.7em` are both
// relative: at the product's own root size the arc is seven pixels across
// and a pixel count of it is noise. At this size it is a couple of hundred.
const spinnerRootFontSizePx = 200

// spinnerBorderWidthPx is the stroke width the stage gives the arc.
//
// THE ONE THING THE STAGE OVERRIDES, and it is geometry, never the mechanic
// under test: the shipped rule's `2px` stroke stays 2px however large the
// box grows, so a ring of it is almost entirely antialiased edge and an
// exact-color count of it would measure the renderer's blending rather than
// the arc's opacity. Widening the stroke gives the measurement an interior.
// `opacity`, which is what this file asserts on, is untouched.
const spinnerBorderWidthPx = 10

// spinnerArcInkFloor is the fewest marker pixels a drawn arc may carry.
//
// MEASURED on the image this landed with: 620, and it is a band rather than
// the whole ring -- `thinking-spin` is paused at rotation 0 by the flag, so
// the top border is axis-aligned and only its axis-aligned run comes back
// exactly the marker color. The floor is six times under that, because what
// it separates is not one arc from another: it is an arc DRAWN AT ALL from
// one frozen at `opacity: 0`, and the latter inks NONE. Every count is
// logged, so a run drifting toward the floor says so before it crosses it.
const spinnerArcInkFloor = 100

// TestPlaytestSandboxDrawsARunningToolsArcUnderThePhotographersFlag is the
// pixel half of the running marker's precondition.
//
// ONE EMACS, SEVERAL EDGE CASES, for the reason
// TestPlaytestSandboxDrawsTextGlyphsFromATextFace states: every subtest here
// is about the SAME page in the SAME webview, and splitting them would boot
// three real Emacsen on three Xvfbs through the layer's serialized Emacs
// slot to ask one renderer three questions.
func TestPlaytestSandboxDrawsARunningToolsArcUnderThePhotographersFlag(t *testing.T) {
	page := newSpinnerPage(t)

	t.Run("the arc inks the page's color when the flag is raised inside its fade-in delay", func(t *testing.T) {
		// Arrange: the same stage with the arc removed from the badge, which
		// fixes that the marker color belongs to the arc and to nothing
		// else -- the pill's own border, the badge text and the editor's
		// chrome are all on this screen either way.
		page.requireMarkerAbsentWithoutTheArc(t)

		// Act: the flag is raised by the page's own head script, so it is on
		// BEFORE the arc's animation starts. That is the worst case and it
		// is the playbooks' case; making it structural rather than racing a
		// 1s delay is the point.
		page.show(t, spinnerStageArc, spinnerFlagAtParse)
		inked := page.markerPixels(t)
		t.Logf("the running arc inked %d pixels of the page's own color with the flag raised at parse time", inked)

		// Assert.
		if inked < spinnerArcInkFloor {
			t.Errorf("the running arc inked %d pixels of rgb(%d,%d,%d), want at least %d: the "+
				"photographer's flag is freezing `tool-run-appear` inside its 1s delay, so every "+
				"`running…` badge in every playbook photographs a reserved but EMPTY box. See "+
				"`:root[data-motion=\"paused\"] .tool-spinner` in webapp/src/styles.css",
				inked, glyphMarkerColor.R, glyphMarkerColor.G, glyphMarkerColor.B, spinnerArcInkFloor)
		}
	})

	t.Run("the arc's own fade-in still carries it, with no flag raised at all", func(t *testing.T) {
		// Arrange: the same badge on a page that raises nothing, left to run
		// its own animation.
		//
		// NOT A PIXEL COUNT, and the reason is `thinking-spin`: with the flag
		// raised at parse time the ring is frozen at rotation 0 and its top
		// border is axis-aligned, so a band of it is EXACTLY the marker
		// color. Left to turn, the ring is paused at an arbitrary angle and
		// WebKit rasterizes the rotated layer -- MEASURED, 620 exact-color
		// pixels at rotation 0 and 0 at an arbitrary one. That is the
		// renderer's resampling, not the arc's opacity, and opacity is what
		// this file asserts on. So this half asks the renderer for the
		// computed value instead.
		page.show(t, spinnerStageArc, spinnerFlagNone)

		// Act: the arc's own `tool-run-appear` is left to reach full opacity,
		// which is a wait ON THE ANIMATION -- a delay that changed would move
		// this wait rather than break it.
		page.awaitFadedIn(t)

		// Assert: the animation ran, so the photographer's rule did not
		// REPLACE the fade-in. A rule that merely forced the arc visible
		// would satisfy the check above while the product's own mechanic had
		// quietly stopped working.
		//
		// (The wait is the assertion: it fails loudly with the last computed
		// opacity if the arc never gets there.)
	})

	t.Run("the arc still waits out its delay before fading in", func(t *testing.T) {
		// Arrange: the page from the subtest above, already faded in.

		// Act: the delay the renderer computed for the arc, per animation, in
		// the shorthand's own order -- the rotation first, the fade-in second.
		got := page.computedStyleOf(t, "animationDelay")
		t.Logf("the running arc's computed animation-delay is %q", got)

		// Assert: the 1s that makes the arc a signal rather than a
		// decoration is still there. It is the delay the photographer's flag
		// used to freeze the arc inside, so a "fix" that deleted it would
		// make this file's first check pass for the wrong reason.
		if !strings.Contains(got, "1s") {
			t.Errorf("the running arc's computed animation-delay is %q, want the fade-in's 1s: "+
				"an arc that appears the instant a call starts is a decoration, not the signal "+
				"that the user is waiting on this one", got)
		}
	})
}

// ---------------------------------------------------------------------------
// THE PAGE
// ---------------------------------------------------------------------------

// spinnerStage says what the stage draws inside the `running…` pill.
type spinnerStage int

const (
	// spinnerStageArc draws the badge the product draws: the arc, then the
	// word.
	spinnerStageArc spinnerStage = iota
	// spinnerStageNoArc draws the same badge with the arc element left out,
	// which is the arrangement every ink count rests on.
	spinnerStageNoArc
)

// spinnerFlag says when the photographer's flag goes up.
type spinnerFlag int

const (
	// spinnerFlagAtParse raises it from the page's own head, before the
	// arc's animation has started -- the playbooks' case.
	spinnerFlagAtParse spinnerFlag = iota
	// spinnerFlagNone leaves the page unflagged, so the arc animates as a
	// user's does.
	spinnerFlagNone
)

// spinnerPage is one webview showing one `running…` badge at a time, and the
// framebuffer reads that measure it.
//
// It carries no daemon and no shim: what is under test is one rule of the
// SHIPPED STYLESHEET, which is inlined into the page verbatim, so everything
// the product would add around it is noise here.
type spinnerPage struct {
	e     *Emacs
	sheet string
	dir   string
	seq   int
}

// newSpinnerPage brings up an Emacs, sizes its frame to the whole screen,
// and puts a webview in it.
func newSpinnerPage(t *testing.T) *spinnerPage {
	t.Helper()
	box := requireSandbox(t)

	sheet, err := os.ReadFile(filepath.Join(repo.repoDir, "webapp", "src", "styles.css"))
	if err != nil {
		t.Fatalf("read the shipped webapp stylesheet: %v", err)
	}

	w := NewEmacsWorld(t, box)
	e := w.Emacs

	// The frame is sized before anything is drawn in it, and the blink is
	// off, for the reasons `newGlyphPage` gives.
	e.Eval(fmt.Sprintf(`(progn
             (blink-cursor-mode -1)
             (set-frame-size (selected-frame) %d %d t)
             (redisplay t)
             t)`, playtestFrameWidth, playtestFrameHeight))

	p := &spinnerPage{e: e, sheet: string(sheet), dir: t.TempDir()}
	p.show(t, spinnerStageNoArc, spinnerFlagAtParse)
	return p
}

// spinnerProbeSetup installs the page probe, in the two-eval shape
// `playtestProbeSetup` explains: `xwidget-webkit-execute-script` is
// asynchronous, so each call issues the script again and answers what the
// PREVIOUS issue's callback stored.
const spinnerProbeSetup = `(progn
             (defvar agent-repl-spinnertest--js nil)
             (defun agent-repl-spinnertest--probe (script)
               (let ((xw (xwidget-webkit-current-session)))
                 (unless xw (error "no live webkit session for the spinner page"))
                 (xwidget-webkit-execute-script
                  xw script
                  (lambda (value) (setq agent-repl-spinnertest--js (format "%s" value))))
                 agent-repl-spinnertest--js))
             t)`

// spinnerPageHTML builds the page: the shipped stylesheet verbatim, a white
// ground, and the product's own `running…` badge markup blown up so a pixel
// count of the arc has signal.
//
// THE MARKUP IS THE PRODUCT'S -- `<span class="badge run"><span
// class="tool-spinner">` plus the word, as `drawFeedToolCallRunning` builds
// it in webapp/src/feed/cards/tool-call.ts. Markup written here could carry
// classes the shipped rules never see.
func (p *spinnerPage) spinnerPageHTML(stage spinnerStage, flag spinnerFlag) string {
	arc := `<span class="tool-spinner" id="arc" aria-hidden="true"></span>`
	if stage == spinnerStageNoArc {
		arc = ""
	}
	head := ""
	if flag == spinnerFlagAtParse {
		head = `<script>document.documentElement.setAttribute(` +
			jsString(playtestMotionAttribute) + `, ` + jsString(playtestMotionPaused) + `);</script>`
	}
	return `<!doctype html><html><head><meta charset="utf-8"><title>spinner</title>` + head +
		`<style>` + p.sheet + `</style><style>
html { font-size: ` + fmt.Sprint(spinnerRootFontSizePx) + `px; }
body { margin: 0; padding: 0; background: #ffffff; }
#stage { position: absolute; left: 40px; top: 40px; }
#stage .tool-spinner {
  /* The arc's own color, and ONLY the arc's: --thinking is redeclared on the
     ring itself rather than on the stage, because .badge.run paints the word
     "running…" in that same variable -- a stage-level declaration made the
     WORD the marker color, and the arrangement below counted 14693 of those
     pixels with the arc removed. Declared here, the shipped border-top-color
     resolves to the marker while the word keeps the product's orange. The
     ring's other three sides are this color at 25% over white, nowhere near
     it, so only the top border is counted. */
  --thinking: rgb(` + fmt.Sprintf("%d,%d,%d", glyphMarkerColor.R, glyphMarkerColor.G, glyphMarkerColor.B) + `);
  border-width: ` + fmt.Sprint(spinnerBorderWidthPx) + `px;
}
</style></head><body><span id="stage"><span class="badge run">` + arc + `running…</span></span></body></html>`
}

// show writes the page to this test's own directory, navigates the webview
// to it, and returns once the page has mounted and its flag is up.
//
// A FILE, NOT A data: URL, unlike the glyph page beside it: the shipped
// stylesheet is a quarter of a megabyte, and a base64 data: URL of it would
// be passed to Emacs as one enormous elisp string literal. Each navigation
// gets its own file, so the mount check below cannot be satisfied by the
// page the PREVIOUS navigation left in the webview.
func (p *spinnerPage) show(t *testing.T, stage spinnerStage, flag spinnerFlag) {
	t.Helper()
	p.seq++
	path := filepath.Join(p.dir, fmt.Sprintf("spinner-%d.html", p.seq))
	if err := os.WriteFile(path, []byte(p.spinnerPageHTML(stage, flag)), 0o644); err != nil {
		t.Fatalf("write the spinner page to %s: %v", path, err)
	}

	p.e.Eval(`(progn (delete-other-windows) (xwidget-webkit-browse-url ` +
		elispString("file://"+path) + `) t)`)
	p.e.AwaitEvalFor(playtestPageBound, "the spinner page's webview to be live",
		`(let ((xw (xwidget-webkit-current-session)))
           (and xw (xwidget-webkit-uri xw)))`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	p.e.Eval(spinnerProbeSetup)

	// The page is MOUNTED when the badge this navigation asked for is in the
	// document -- a URI is not a page, and a webview still holding the
	// previous stage would answer every question about it.
	wantArc := "true"
	if stage == spinnerStageNoArc {
		wantArc = "false"
	}
	p.e.Eval(`(setq agent-repl-spinnertest--js nil)`)
	p.e.AwaitEvalFor(playtestPageBound, "the spinner page to hold the badge this step navigated to",
		`(agent-repl-spinnertest--probe `+elispString(pageYes(
			`document.readyState === "complete" && `+
				`document.querySelector("#stage .badge.run") !== null && `+
				`(document.getElementById("arc") !== null) === `+wantArc))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })

	if flag == spinnerFlagAtParse {
		p.requireMotionHeld(t)
	}
}

// awaitFadedIn blocks until the arc's own `tool-run-appear` has carried it
// to full opacity. It is a WAIT ON THE ANIMATION, not a sleep: the page is
// asked what the renderer computed, so a delay that changed would move this
// wait rather than break it.
func (p *spinnerPage) awaitFadedIn(t *testing.T) {
	t.Helper()
	p.e.Eval(`(setq agent-repl-spinnertest--js nil)`)
	p.e.AwaitEvalFor(spinnerFadeInBound, "the running arc's own fade-in to reach full opacity",
		`(agent-repl-spinnertest--probe `+elispString(pageYes(
			`getComputedStyle(document.getElementById("arc")).opacity === "1"`))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// spinnerFadeInBound is how long the arc's fade-in is given to run.
//
// The animation is a 1s delay plus a 0.2s fade, so 1.2s is the whole of it;
// the bound is a small multiple of that, which leaves room for the webview's
// own frame scheduling and nothing else.
const spinnerFadeInBound = 4 * playtestPageBound

// computedStyleOf answers one computed style property of the arc, as the
// renderer resolved it.
func (p *spinnerPage) computedStyleOf(t *testing.T, property string) string {
	t.Helper()
	p.e.Eval(`(setq agent-repl-spinnertest--js nil)`)
	raw := p.e.AwaitEvalFor(playtestPageBound, "the running arc's computed "+property,
		`(agent-repl-spinnertest--probe `+elispString(
			`getComputedStyle(document.getElementById("arc")).`+property)+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) != "" })
	return decodeString(raw)
}

// requireMotionHeld is the precondition every capture below rests on: with
// the flag down, `thinking-spin` turns forever and the framebuffer never
// holds still, so a settle would run its whole budget out and photograph a
// torn frame.
func (p *spinnerPage) requireMotionHeld(t *testing.T) {
	t.Helper()
	p.e.Eval(`(setq agent-repl-spinnertest--js nil)`)
	p.e.AwaitEvalFor(playtestPageBound, "the photographer's flag to be up on the spinner page",
		`(agent-repl-spinnertest--probe `+elispString(pageYes(
			`document.documentElement.getAttribute(`+jsString(playtestMotionAttribute)+`) === `+
				jsString(playtestMotionPaused)))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// ---------------------------------------------------------------------------
// THE MEASUREMENTS
// ---------------------------------------------------------------------------

// markerPixels counts the page's own color on the CURRENT screen.
func (p *spinnerPage) markerPixels(t *testing.T) int {
	t.Helper()
	img := p.capture(t, spinnerToken())
	return countColorWithin(img, glyphMarkerColor, glyphMarkerTolerance)
}

// requireMarkerAbsentWithoutTheArc is the arrangement every ink count rests
// on: with the arc left out of the badge, NOTHING on this screen is the
// marker color, so a later count of it is a count of the arc.
func (p *spinnerPage) requireMarkerAbsentWithoutTheArc(t *testing.T) {
	t.Helper()
	p.show(t, spinnerStageNoArc, spinnerFlagAtParse)
	if stray := p.markerPixels(t); stray != 0 {
		t.Fatalf("%d pixels of the marker color rgb(%d,%d,%d) are on the screen with the arc left OUT "+
			"of the badge; something else is painting it, so counting that color would not count the "+
			"arc. Move glyphMarkerColor.", stray, glyphMarkerColor.R, glyphMarkerColor.G, glyphMarkerColor.B)
	}
}

// capture redraws the frame, waits for the page's frames, and decodes the X
// server's screen memory once it has held still -- `playbook.capture`'s
// mechanism without the artifact writing this suite has nowhere to put.
func (p *spinnerPage) capture(t *testing.T, token string) *image.RGBA {
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

// awaitPainted blocks until the webview has delivered its own frames for the
// DOM now in it. See playtestPaintFrames.
func (p *spinnerPage) awaitPainted(t *testing.T, token string) {
	t.Helper()
	p.e.Eval(`(setq agent-repl-spinnertest--js nil)`)
	p.e.AwaitEvalFor(playtestPaintBound, "the spinner page to deliver its own frames",
		`(agent-repl-spinnertest--probe `+elispString(playtestPaintGateScript(token))+`)`,
		func(raw json.RawMessage) bool { return decodeString(raw) == "yes" })
}

// redraw garbages the frame twice, for the double-buffer reason
// `playbook.redrawFrame` states.
func (p *spinnerPage) redraw(t *testing.T) {
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
func (p *spinnerPage) readFramebuffer(t *testing.T) []byte {
	t.Helper()
	body, err := os.ReadFile(p.e.Display.FramebufferPath)
	if err != nil {
		t.Fatalf("read the Xvfb framebuffer at %s: %v", p.e.Display.FramebufferPath, err)
	}
	return body
}

// spinnerToken numbers a capture's paint request within this page, so a wait
// can never be satisfied by the frames an earlier capture asked for.
func spinnerToken() string {
	return fmt.Sprintf("spinner-%d", playtestPaintToken.Add(1))
}
