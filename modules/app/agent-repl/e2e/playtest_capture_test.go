package e2e

import (
	"encoding/binary"
	"fmt"
	"image"
	"image/color"
	"image/png"
	"math/bits"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// THE SCREENSHOT MECHANISM.
//
// PLAYTEST-SPEC.md is the design; this file is the part of it that runs on
// every ordinary `go test` — the decoder, the blank-frame floor and their
// unit tests. The playbooks that USE it are behind the `playtest` build tag
// in playtest_playbooks_test.go, because each one boots a real Emacs and
// writes PNGs a human then looks at.
//
// WHY THE X FRAMEBUFFER AND NOT `x-export-frames`. Emacs 27+ can export its
// own frame as a PNG from inside the process, which needs no tool in the
// image at all, and it was measured first for exactly that reason. It is not
// usable here: `x-export-frames` re-renders the frame through EMACS'S OWN
// REDISPLAY, and the panel is an `xwidget-webkit` webview — a real GTK child
// widget that WebKit paints, not something Emacs draws. Measured in this
// image, on a page with a solid red body: the exported PNG carries the
// header line, the mode line and the frame's chrome, and an EMPTY WHITE
// RECTANGLE where the webview is. The webapp is the whole reason a
// screenshot is wanted, so a capture that cannot see it is not a capture.
//
// The same page captured out of the X framebuffer carries 405,401 red and
// 20,820 green pixels — the webview, painted. So the capture is the X
// server's own screen memory, which `-fbdir` puts on disk (see
// `startXvfb`), and which nothing else in this image can read: it carries
// no xwd, no ImageMagick, no scrot and no xdotool, all four verified absent
// from inside the container rather than inferred from the Dockerfile.
//
// The format is therefore X's own XWD, decoded here rather than converted by
// a tool. That is not a workaround for the missing tool — it is what the
// mechanical assertions need anyway: the geometry and the blank-frame floor
// are read off the decoded pixels, so a converter would only have added a
// dependency between two things this file already does.

// playtestFrameWidth and playtestFrameHeight are the geometry every capture
// must have. They are DERIVED from `xvfbScreen` rather than restated, so the
// screen the display is started at and the geometry the manifest declares
// cannot drift apart.
var playtestFrameWidth, playtestFrameHeight = mustParseXvfbGeometry(xvfbScreen)

// mustParseXvfbGeometry reads "<w>x<h>x<depth>" into its width and height.
// It panics rather than returning an error: `xvfbScreen` is a constant of
// this package, so a shape it cannot parse is a defect in the source and not
// a runtime condition.
func mustParseXvfbGeometry(screen string) (int, int) {
	parts := strings.Split(screen, "x")
	if len(parts) != 3 {
		panic(fmt.Sprintf("xvfbScreen %q is not <width>x<height>x<depth>", screen))
	}
	w, err := strconv.Atoi(parts[0])
	if err != nil {
		panic(fmt.Sprintf("xvfbScreen %q has a non-numeric width: %v", screen, err))
	}
	h, err := strconv.Atoi(parts[1])
	if err != nil {
		panic(fmt.Sprintf("xvfbScreen %q has a non-numeric height: %v", screen, err))
	}
	return w, h
}

// playtestBlankFloor is how many DISTINCT colors a capture must carry before
// it counts as a picture of something.
//
// MEASURED, inside this image, at the same 1280x1024 geometry every capture
// uses:
//
//	an Xvfb with nothing on it        1 distinct color
//	Emacs with a blank webview     1485 distinct colors
//	Emacs with the page painted    1647 distinct colors
//
// So the two regimes are three orders apart and the floor is not a delicate
// number. 64 sits 23x below the weakest DRAWN frame ever observed and 64x
// above the blank one, which is what makes it a floor rather than a
// threshold anyone has to tune.
//
// WHAT IT DOES AND DOES NOT CLAIM. It catches the failure that would
// otherwise waste a whole review — a frame that never appeared, an Xvfb that
// died, a capture taken from the wrong screen — and it deliberately claims
// nothing about WHAT was drawn. That is the reviewer's job, and pixel-golden
// comparison is not attempted anywhere here: it is brittle against font
// hinting, a scrollbar, a clock, and every other honest difference between
// two runs of the same product.
const playtestBlankFloor = 64

// playtestSettleBound is how long one capture waits for the screen to stop
// changing before it takes the picture.
//
// A screenshot of a frame mid-redraw is half of one state and half of
// another, and the file it lands in is the evidence a human then reads. So a
// capture reads the framebuffer until it has been UNCHANGED FOR A WHOLE
// WINDOW (`playtestSettleWindow`), which is a real quiescence and not an
// interval anybody guessed.
//
// It is a PATIENCE BUDGET, NOT AN ASSERTION, and that distinction is the
// reason it may be missed without failing anything: a surface that is
// genuinely animating — a spinner, the attention marker's own 2s blink
// schedule, a retry arc — never settles, and refusing to photograph it would
// refuse exactly the states this playtest exists to show. A capture that
// runs out its budget takes the last read it made and SAYS SO in the
// manifest, so a reviewer looking at a torn frame knows why it is torn.
//
// The cursor's own blink is switched off in the playbook's setup rather than
// waited out, because a blinking cursor alone would make every capture in
// every playbook run this budget to its end.
const playtestSettleBound = 2 * time.Second

// playtestSettleInterval is how often a settling capture re-reads the
// framebuffer. It is the package's own poll interval: the read is a 5 MiB
// copy out of tmpfs, so a shorter one would buy nothing but copies.
const playtestSettleInterval = pollInterval

// playtestPaintFrames is how many ANIMATION FRAMES the page must deliver,
// after the capture forced its redraw, before the picture is taken.
//
// WHAT THIS CURES, and it is the same one defect wearing two faces.
//
// An `xwidget-webkit` webview on X is OFFSCREEN-RENDERED: WebKit paints into
// a GTK offscreen surface, and those pixels reach the glass ONLY when
// Emacs's own redisplay copies that surface into the frame while drawing the
// xwidget's glyph. So a capture's redraw copies WHATEVER THE SURFACE HELD AT
// THAT INSTANT. If WebKit has taken the DOM change but not yet produced a
// frame for it, the copy is of the previous page state -- and the picture is
// a stale webview under a DOM assertion that legitimately passed. Worse, the
// frame WebKit then produces raises a damage signal, and the redisplay that
// signal schedules is INCREMENTAL: it draws the xwidget glyph into the back
// buffer and swaps, so the glass gets a current webview inside a buffer that
// carries none of the chrome the capture's full redraw had put in the OTHER
// buffer. Both were observed, in three runs of one owner's section: a picture
// of an empty feed whose DOM held two settled bubbles, and a picture of a
// current webview with no tab bar and no mode lines at all.
//
// Neither is a torn frame, which is why `settleFrame` never caught them: the
// stale screen is perfectly still, so two consecutive reads agree and the
// capture reports settled=true on a lie.
//
// So the capture WAITS FOR THE PAGE'S OWN FRAMES before its final redraw.
// TWO is the smallest count that proves anything: the first
// `requestAnimationFrame` callback runs BEFORE that frame is painted, and the
// second runs only once the first frame has been produced -- so two
// callbacks mean a frame carrying the current DOM exists to be copied. One
// would only mean the page was asked.
const playtestPaintFrames = 2

// playtestPaintStateGlobal is where the page keeps one paint request's
// progress between the probe's issue and the probe's readback.
//
// It is per-request rather than a single slot: `xwidget-webkit-execute-script`
// is asynchronous and the probe issues its script again on every poll, so a
// script that started a fresh frame chain each time would never finish one.
// Keying by the capture's own token makes the second and later issues pure
// READS of the chain the first issue started.
const playtestPaintStateGlobal = "__agentReplPlaytestPaint"

// playtestPaintGateScript builds the page-side script for one paint request.
//
// It answers "yes" once the page has delivered playtestPaintFrames frames
// for THIS token, and otherwise a "no:" carrying the count it is at -- the
// same shape `pageYes` uses, and for the same reason: a wait that fails
// prints its last value, so the value has to say what the page was doing.
func playtestPaintGateScript(token string) string {
	frames := strconv.Itoa(playtestPaintFrames)
	key := jsString(token)
	store := "window." + playtestPaintStateGlobal
	return `(function () {
                   ` + store + ` = ` + store + ` || {};
                   var state = ` + store + `[` + key + `];
                   if (!state) {
                     state = ` + store + `[` + key + `] = { frames: 0 };
                     var tick = function () {
                       state.frames += 1;
                       if (state.frames < ` + frames + `) { requestAnimationFrame(tick); }
                     };
                     requestAnimationFrame(tick);
                   }
                   if (state.frames >= ` + frames + `) { return "yes"; }
                   return "no: the page has delivered " + state.frames + " of ` + frames + `" +
                          " animation frames since the capture forced its redraw";
                 })()`
}

// playtestMotionAttribute and playtestMotionPaused are the webapp's own
// photographer's flag: `data-motion="paused"` on the page's root element
// freezes every running CSS animation WHERE IT STANDS, and removing the
// attribute lets them all continue from there.
//
// WHY A CAPTURE NEEDS IT. Several of the webapp's animations run forever and
// by design — the prompt bubble's `bubble-wave`, the sidebar's state-dot
// pulse, the footer's breath, a running tool's arc. `settleFrame` waits for
// the framebuffer to hold still for a whole `playtestSettleWindow` before it
// fires, and against an animation that never stops that wait cannot be
// satisfied: it runs `playtestSettleBound` out and photographs a moving
// screen anyway, with the manifest's torn-frame note on a picture that is
// not, in fact, showing anything wrong.
//
// MEASURED, before this existed, by the settle diagnostic
// (`playtestSettleDiagEnv`) over one run of the D29-D32 playbook: 10 of 10
// captures ran the full 2s budget, 645 rounds saw the framebuffer change,
// and EVERY persistent changed box was right-edged at the prompt column
// (x=1149-1150) with its left edge at a `.bubble.user` bubble's own left
// edge. One box per prompt bubble in the scrollback, and nothing anywhere
// else: not the sidebar's workspace age, not the footer's elapsed clock. The
// page's one-second tickers change a handful of glyphs once a second and
// leave ~950ms of stillness for a 50ms window to close in, so they were
// never the cause and are not what this holds.
//
// IT IS HELD PER CAPTURE, NOT FOR THE RUN. A page paused at boot would never
// run the animations whose FINAL state a picture is supposed to show — a
// running tool's arc fades in on a 1s delay, and a page that never ran that
// delay would photograph an invisible spinner in every playbook. So the flag
// goes on as a capture begins and comes off as it ends, and between captures
// the page animates exactly as a user's does.
const (
	playtestMotionAttribute = "data-motion"
	playtestMotionPaused    = "paused"
)

// playtestHoldMotionScript sets the flag and answers whether it is set, in
// the `pageYes` shape: a wait that fails prints its last value.
func playtestHoldMotionScript() string {
	return `(function () {
                   document.documentElement.setAttribute(` + jsString(playtestMotionAttribute) + `,
                                                         ` + jsString(playtestMotionPaused) + `);
                   return document.documentElement.getAttribute(` + jsString(playtestMotionAttribute) + `) === ` +
		jsString(playtestMotionPaused) + `;
                 })()`
}

// playtestReleaseMotionScript removes the flag and answers whether it is
// gone. The release is asserted rather than fired and forgotten: a page left
// paused would freeze every animation for every LATER capture in the same
// playbook, and those pictures would be wrong in a way no assertion here
// looks at.
func playtestReleaseMotionScript() string {
	return `(function () {
                   document.documentElement.removeAttribute(` + jsString(playtestMotionAttribute) + `);
                   return !document.documentElement.hasAttribute(` + jsString(playtestMotionAttribute) + `);
                 })()`
}

// jsString renders a Go string as a JavaScript string literal.
//
// It lives HERE, in the untagged half of the mechanism, rather than beside
// the playbooks that mostly call it: the paint gate below is compiled by the
// ordinary `go test` so its unit tests need no sandbox, and one escaping
// shared by the selectors and the gate is the only way the two cannot drift.
// The selectors here carry double quotes, so single quotes are the delimiter
// and the two characters that could still end the literal are escaped.
func jsString(s string) string {
	out := make([]rune, 0, len(s)+2)
	out = append(out, '\'')
	for _, r := range s {
		if r == '\'' || r == '\\' {
			out = append(out, '\\')
		}
		out = append(out, r)
	}
	return string(append(out, '\''))
}

// countColorWithin counts how many pixels of IMG are within TOLERANCE of
// WANT on every channel.
//
// It is what turns "the glass carries this change" into a number. A capture
// asserting a solid region it just painted into the page cannot ask for an
// EXACT color -- the region's edges are antialiased and a scrollbar or a
// selection may sit over part of it -- so the measure is a count of pixels
// near the color, held to a floor, rather than an equality anywhere.
func countColorWithin(img *image.RGBA, want color.RGBA, tolerance uint8) int {
	near := func(a, b uint8) bool {
		if a > b {
			a, b = b, a
		}
		return b-a <= tolerance
	}
	n := 0
	bounds := img.Bounds()
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			c := img.RGBAAt(x, y)
			if near(c.R, want.R) && near(c.G, want.G) && near(c.B, want.B) {
				n++
			}
		}
	}
	return n
}

// playtestSettleWindow is how long the framebuffer must stay UNCHANGED
// before a capture calls it settled.
//
// WHY A WINDOW AND NOT TWO AGREEING READS. Two consecutive reads
// `playtestSettleInterval` apart is not quiescence, it is a coin toss: a GTK
// webview is composited on the frame clock (16.7ms at 60Hz) and Emacs's own
// X drawing after `redraw-frame` lands when the X SERVER processes it, so a
// paint that is already committed — in the page's DOM, or queued in X — is
// simply not on the glass yet, and two reads a few milliseconds apart agree
// about the frame before it. MEASURED, in two real playtest runs:
//
//	04-arm-link-severed     settled=true took  2ms; the picture carried the
//	                        webview alone and every piece of Emacs's own
//	                        chrome — tab bar, mode lines, composer — blank
//	                        white. The next capture, 22ms later, was whole.
//	04-arm-detached-settled settled=true took  5ms; the tab bar was correctly
//	                        green while the webview inside it still showed the
//	                        PREVIOUS state, though the step's own
//	                        `awaitInPage` on `[data-state="completed"]` had
//	                        already passed. The other run of the same capture,
//	                        taken a few ms later by chance, was whole.
//
// Both tore at 2-5ms and both were correct on the glass ~22ms later, so the
// window has to outlast a display frame rather than merely exceed a poll.
// 50ms is THREE frame periods: a frame that has been identical across a
// window that long has had a whole composite, plus two more, to land in it.
//
// WHAT THE OTHER TWO GATES DO NOT COVER, and why this one is not redundant
// beside them. The paint gate above cures the SECOND of those two tears at
// its source, in the page: a webview whose offscreen surface still held the
// previous DOM. The full redisplay `settleFrame` drives between its reads
// cures a screen Emacs has simply not repainted yet. Neither can see the
// FIRST: Emacs's own chrome and the webview had both been drawn and were
// still ARRIVING at the X server, so every read that ran was of a frame in
// flight, and each of those reads is as still as the last. Only a window on
// the glass itself catches that one.
//
// THE COST, STATED: at least +50ms on every capture that settles, and a
// whole playtest takes about 250 of them — about 12.5s across the entire
// run, against pictures a human is going to read and disbelieve if they are
// torn. "At least", because the window closes on the grid of poll instants
// `settleFrame` actually reads at and each of those reads drives a full
// redisplay first, so a capture pays the redisplays the window spans as
// well.
//
// `playtestSettleBound` still caps the whole wait, and the not-settled path
// is unchanged: an animating surface runs the budget out and says so in the
// manifest.
const playtestSettleWindow = 50 * time.Millisecond

// ---------------------------------------------------------------------------
// XWD
// ---------------------------------------------------------------------------

// XWD header field offsets, in 32-bit big-endian words, from X's own
// XWDFileHeader. They are named rather than inlined because the file has
// twenty-five of them and an off-by-one reads a plausible number out of the
// wrong field — which is exactly what happened while this was being written:
// `bits_per_pixel` read as 5120 and the decode "succeeded" against garbage.
const (
	xwdHeaderSize     = 0
	xwdFileVersion    = 1
	xwdPixmapFormat   = 2
	xwdPixmapDepth    = 3
	xwdPixmapWidth    = 4
	xwdPixmapHeight   = 5
	xwdByteOrder      = 7
	xwdBitsPerPixel   = 11
	xwdBytesPerLine   = 12
	xwdVisualClass    = 13
	xwdRedMask        = 14
	xwdGreenMask      = 15
	xwdBlueMask       = 16
	xwdNColors        = 19
	xwdMinHeaderWords = 25
)

// The XWD constants this decoder accepts. Anything else is REFUSED rather
// than coped with: the only writer here is the Xvfb this layer starts
// itself, at a screen depth this layer states itself, so a file in another
// shape is a fact about the harness that has changed and must be seen.
const (
	xwdVersion7      = 7
	xwdFormatZPixmap = 2
	xwdTrueColor     = 4
	xwdDirectColor   = 5
	xwdColorEntry    = 12 // sizeof(XWDColor)
	xwdLSBFirst      = 0
)

// decodeXWD decodes one X Window Dump into an image.
func decodeXWD(body []byte) (*image.RGBA, error) {
	if len(body) < xwdMinHeaderWords*4 {
		return nil, fmt.Errorf("an XWD is at least %d bytes of header, got %d", xwdMinHeaderWords*4, len(body))
	}
	word := func(i int) uint32 { return binary.BigEndian.Uint32(body[i*4:]) }

	if v := word(xwdFileVersion); v != xwdVersion7 {
		return nil, fmt.Errorf("XWD file version %d, want %d", v, xwdVersion7)
	}
	if f := word(xwdPixmapFormat); f != xwdFormatZPixmap {
		return nil, fmt.Errorf("XWD pixmap format %d, want ZPixmap (%d)", f, xwdFormatZPixmap)
	}
	if c := word(xwdVisualClass); c != xwdTrueColor && c != xwdDirectColor {
		return nil, fmt.Errorf("XWD visual class %d, want TrueColor (%d) or DirectColor (%d)",
			c, xwdTrueColor, xwdDirectColor)
	}
	if b := word(xwdBitsPerPixel); b != 32 {
		return nil, fmt.Errorf("XWD carries %d bits per pixel, want 32", b)
	}

	width, height := int(word(xwdPixmapWidth)), int(word(xwdPixmapHeight))
	if width <= 0 || height <= 0 {
		return nil, fmt.Errorf("XWD geometry is %dx%d", width, height)
	}
	stride := int(word(xwdBytesPerLine))
	if stride < width*4 {
		return nil, fmt.Errorf("XWD bytes per line is %d, too small for %d pixels of 4 bytes", stride, width)
	}

	// The pixels start after the header, the window name inside it, and the
	// colormap: even a TrueColor dump carries one, and Xvfb writes 256
	// entries.
	offset := int(word(xwdHeaderSize)) + int(word(xwdNColors))*xwdColorEntry
	if want := offset + stride*height; len(body) < want {
		return nil, fmt.Errorf("XWD is %d bytes, want at least %d for %dx%d pixels at stride %d",
			len(body), want, width, height, stride)
	}

	red, err := newChannel("red", word(xwdRedMask))
	if err != nil {
		return nil, err
	}
	green, err := newChannel("green", word(xwdGreenMask))
	if err != nil {
		return nil, err
	}
	blue, err := newChannel("blue", word(xwdBlueMask))
	if err != nil {
		return nil, err
	}

	lsbFirst := word(xwdByteOrder) == xwdLSBFirst
	img := image.NewRGBA(image.Rect(0, 0, width, height))
	for y := 0; y < height; y++ {
		row := body[offset+y*stride:]
		for x := 0; x < width; x++ {
			var pixel uint32
			if lsbFirst {
				pixel = binary.LittleEndian.Uint32(row[x*4:])
			} else {
				pixel = binary.BigEndian.Uint32(row[x*4:])
			}
			img.SetRGBA(x, y, color.RGBA{
				R: red.of(pixel),
				G: green.of(pixel),
				B: blue.of(pixel),
				A: 0xff,
			})
		}
	}
	return img, nil
}

// channel turns one X pixel mask into an 8-bit component.
//
// The masks are read rather than assumed even though this image's Xvfb
// always reports 0xff0000/0x00ff00/0x0000ff: a mask is what the FILE says
// the pixel means, and reading it is how a capture from a differently
// configured server decodes correctly instead of decoding into swapped
// colors that still look like a picture.
type channel struct {
	mask  uint32
	shift int
	// scale carries a mask narrower than 8 bits up to full range, so a
	// 16-bit visual does not decode as a dark image.
	scale float64
}

func newChannel(name string, mask uint32) (channel, error) {
	if mask == 0 {
		return channel{}, fmt.Errorf("XWD %s mask is zero, so no pixel could carry that channel", name)
	}
	width := bits.OnesCount32(mask)
	if mask>>bits.TrailingZeros32(mask) != uint32(1)<<width-1 {
		return channel{}, fmt.Errorf("XWD %s mask %#x is not one contiguous run of bits", name, mask)
	}
	return channel{
		mask:  mask,
		shift: bits.TrailingZeros32(mask),
		scale: 255 / float64(uint32(1)<<width-1),
	}, nil
}

func (c channel) of(pixel uint32) uint8 {
	return uint8(float64((pixel&c.mask)>>c.shift)*c.scale + 0.5)
}

// distinctColors counts how many different colors an image carries, which is
// the blank-frame measure playtestBlankFloor is stated in.
func distinctColors(img *image.RGBA) int {
	seen := make(map[uint32]struct{})
	bounds := img.Bounds()
	for y := bounds.Min.Y; y < bounds.Max.Y; y++ {
		for x := bounds.Min.X; x < bounds.Max.X; x++ {
			c := img.RGBAAt(x, y)
			seen[uint32(c.R)<<16|uint32(c.G)<<8|uint32(c.B)] = struct{}{}
		}
	}
	return len(seen)
}

// ---------------------------------------------------------------------------
// WHERE A FRAME IS STILL CHANGING
// ---------------------------------------------------------------------------

// playtestSettleDiagEnv, when set to any non-empty value, makes every settle
// round that saw the framebuffer change say WHERE it changed.
//
// It exists because "the screen was still changing" is not a diagnosis. A
// capture that runs its whole patience budget out has learned only that two
// reads differed, and the three candidate causes — an animating surface in
// the page, a blinking piece of Emacs's own chrome, and the capture's own
// forced redraw flipping the double buffer — are told apart by WHICH PIXELS
// moved, not by how long the wait was. So a diagnostic run reports the
// bounding box, and the box names the culprit: a small box on the sidebar's
// age column is a clock, a box that is the whole frame is the buffer parity.
const playtestSettleDiagEnv = "AGENT_REPL_PLAYTEST_SETTLE_DIAG"

// xwdChangedBounds answers the bounding box of the pixels that differ
// between two XWD dumps, and how many differ.
//
// It reads the raw dumps rather than two decoded images: the comparison
// `settleFramebuffer` makes is over the bytes, so a diagnostic that decoded
// first could report "nothing changed" for a difference that restarted the
// window, and a full decode of two 5 MiB dumps per settle round would cost
// more than the wait it is measuring.
//
// An empty rectangle means the two agree over their pixels.
func xwdChangedBounds(prev, cur []byte) (image.Rectangle, int, error) {
	geom := func(name string, body []byte) (w, h, stride, offset int, err error) {
		if len(body) < xwdMinHeaderWords*4 {
			return 0, 0, 0, 0, fmt.Errorf("the %s dump is %d bytes, under the %d of an XWD header",
				name, len(body), xwdMinHeaderWords*4)
		}
		word := func(i int) int { return int(binary.BigEndian.Uint32(body[i*4:])) }
		w, h = word(xwdPixmapWidth), word(xwdPixmapHeight)
		stride = word(xwdBytesPerLine)
		offset = word(xwdHeaderSize) + word(xwdNColors)*xwdColorEntry
		if w <= 0 || h <= 0 {
			return 0, 0, 0, 0, fmt.Errorf("the %s dump's geometry is %dx%d", name, w, h)
		}
		if stride < w*4 {
			return 0, 0, 0, 0, fmt.Errorf("the %s dump's stride is %d, too small for %d pixels of 4 bytes",
				name, stride, w)
		}
		if want := offset + stride*h; len(body) < want {
			return 0, 0, 0, 0, fmt.Errorf("the %s dump is %d bytes, want at least %d for %dx%d at stride %d",
				name, len(body), want, w, h, stride)
		}
		return w, h, stride, offset, nil
	}

	pw, ph, pstride, poffset, err := geom("earlier", prev)
	if err != nil {
		return image.Rectangle{}, 0, err
	}
	cw, ch, cstride, coffset, err := geom("later", cur)
	if err != nil {
		return image.Rectangle{}, 0, err
	}
	if pw != cw || ph != ch {
		return image.Rectangle{}, 0, fmt.Errorf("the two dumps are %dx%d and %dx%d, so no per-pixel box exists",
			pw, ph, cw, ch)
	}

	minX, minY, maxX, maxY, changed := cw, ch, -1, -1, 0
	for y := 0; y < ch; y++ {
		prow := prev[poffset+y*pstride:][:cw*4]
		crow := cur[coffset+y*cstride:][:cw*4]
		for x := 0; x < cw; x++ {
			if string(prow[x*4:x*4+4]) == string(crow[x*4:x*4+4]) {
				continue
			}
			changed++
			if x < minX {
				minX = x
			}
			if x > maxX {
				maxX = x
			}
			if y < minY {
				minY = y
			}
			if y > maxY {
				maxY = y
			}
		}
	}
	if changed == 0 {
		return image.Rectangle{}, 0, nil
	}
	return image.Rect(minX, minY, maxX+1, maxY+1), changed, nil
}

// ---------------------------------------------------------------------------
// UNIT TESTS FOR THE MECHANISM ITSELF
// ---------------------------------------------------------------------------
//
// These run in the ORDINARY `go test ./e2e`, without the tag and without a
// sandbox: the decoder is the one piece of the playtest whose correctness a
// human looking at a picture cannot check, because a wrong decode produces a
// picture that still looks like a picture.

// buildXWD writes a minimal XWD in the shape Xvfb produces, for the tests
// below. Fields not named here are zero, which is what X writes for the ones
// this decoder does not read.
func buildXWD(t *testing.T, width, height int, opts func(h []uint32), pixels []uint32) []byte {
	t.Helper()
	const ncolors = 2
	header := make([]uint32, xwdMinHeaderWords)
	header[xwdHeaderSize] = uint32(xwdMinHeaderWords * 4)
	header[xwdFileVersion] = xwdVersion7
	header[xwdPixmapFormat] = xwdFormatZPixmap
	header[xwdPixmapDepth] = 24
	header[xwdPixmapWidth] = uint32(width)
	header[xwdPixmapHeight] = uint32(height)
	header[xwdByteOrder] = xwdLSBFirst
	header[xwdBitsPerPixel] = 32
	header[xwdBytesPerLine] = uint32(width * 4)
	header[xwdVisualClass] = xwdTrueColor
	header[xwdRedMask] = 0xff0000
	header[xwdGreenMask] = 0x00ff00
	header[xwdBlueMask] = 0x0000ff
	header[xwdNColors] = ncolors
	if opts != nil {
		opts(header)
	}

	out := make([]byte, 0, len(header)*4+ncolors*xwdColorEntry+len(pixels)*4)
	for _, w := range header {
		out = binary.BigEndian.AppendUint32(out, w)
	}
	// The colormap the decoder must SKIP rather than read. It is deliberately
	// non-zero: a decoder that forgot it would land on these bytes and draw
	// them as pixels.
	for i := 0; i < ncolors*xwdColorEntry; i++ {
		out = append(out, 0xa5)
	}
	for _, p := range pixels {
		out = binary.LittleEndian.AppendUint32(out, p)
	}
	return out
}

func TestPlaytestDecodesAnXvfbFramebuffer(t *testing.T) {
	t.Parallel()
	body := buildXWD(t, 2, 2, nil, []uint32{
		0x00ff0000, 0x0000ff00,
		0x000000ff, 0x00ffffff,
	})

	img, err := decodeXWD(body)
	if err != nil {
		t.Fatalf("decode a well-formed XWD: %v", err)
	}
	want := []color.RGBA{
		{R: 0xff, A: 0xff}, {G: 0xff, A: 0xff},
		{B: 0xff, A: 0xff}, {R: 0xff, G: 0xff, B: 0xff, A: 0xff},
	}
	for i, expect := range want {
		x, y := i%2, i/2
		if got := img.RGBAAt(x, y); got != expect {
			t.Errorf("pixel (%d,%d) decoded to %v, want %v", x, y, got, expect)
		}
	}
}

func TestPlaytestSkipsTheColormapBetweenHeaderAndPixels(t *testing.T) {
	t.Parallel()
	// One pixel, pure blue. The colormap bytes ahead of it are 0xa5a5a5a5,
	// so a decoder that read from the wrong offset would answer that instead.
	img, err := decodeXWD(buildXWD(t, 1, 1, nil, []uint32{0x000000ff}))
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if got, want := img.RGBAAt(0, 0), (color.RGBA{B: 0xff, A: 0xff}); got != want {
		t.Errorf("the single pixel decoded to %v, want %v: the colormap was not skipped", got, want)
	}
}

func TestPlaytestHonorsTheChannelMasks(t *testing.T) {
	t.Parallel()
	// A server whose red and blue masks are swapped. The decoder must read
	// the masks rather than assume X's usual order, or the capture silently
	// comes out with its colors exchanged.
	swap := func(h []uint32) {
		h[xwdRedMask] = 0x0000ff
		h[xwdBlueMask] = 0xff0000
	}
	img, err := decodeXWD(buildXWD(t, 1, 1, swap, []uint32{0x00ff0000}))
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if got, want := img.RGBAAt(0, 0), (color.RGBA{B: 0xff, A: 0xff}); got != want {
		t.Errorf("the pixel decoded to %v, want %v: the masks were not honored", got, want)
	}
}

func TestPlaytestReadsTheServersByteOrder(t *testing.T) {
	t.Parallel()
	// MSBFirst, with the same pixel written big-endian.
	msb := func(h []uint32) { h[xwdByteOrder] = 1 }
	body := buildXWD(t, 1, 1, msb, nil)
	body = binary.BigEndian.AppendUint32(body, 0x0000ff00)
	img, err := decodeXWD(body)
	if err != nil {
		t.Fatalf("decode: %v", err)
	}
	if got, want := img.RGBAAt(0, 0), (color.RGBA{G: 0xff, A: 0xff}); got != want {
		t.Errorf("the pixel decoded to %v, want %v: MSBFirst was read as LSBFirst", got, want)
	}
}

func TestPlaytestRefusesAnUnexpectedXWDShape(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		opts func(h []uint32)
		want string
	}{
		{"a version this decoder has never seen", func(h []uint32) { h[xwdFileVersion] = 6 }, "file version 6"},
		{"an XYPixmap rather than a ZPixmap", func(h []uint32) { h[xwdPixmapFormat] = 1 }, "pixmap format 1"},
		{"a palette visual", func(h []uint32) { h[xwdVisualClass] = 3 }, "visual class 3"},
		{"16 bits per pixel", func(h []uint32) { h[xwdBitsPerPixel] = 16 }, "16 bits per pixel"},
		{"a channel no pixel can carry", func(h []uint32) { h[xwdGreenMask] = 0 }, "green mask is zero"},
		{"a mask that is not one run of bits", func(h []uint32) { h[xwdRedMask] = 0xf0f000 }, "not one contiguous run"},
		{"a stride too small for the width", func(h []uint32) { h[xwdBytesPerLine] = 3 }, "bytes per line is 3"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			_, err := decodeXWD(buildXWD(t, 1, 1, tc.opts, []uint32{0}))
			if err == nil {
				t.Fatalf("decoding %s succeeded; it must be refused", tc.name)
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Errorf("the refusal is %q, want it to name %q", err, tc.want)
			}
		})
	}
}

func TestPlaytestRefusesATruncatedFramebuffer(t *testing.T) {
	t.Parallel()
	// A capture read while the file was still being written is short, and
	// decoding it would silently produce a picture of uninitialized memory.
	full := buildXWD(t, 4, 4, nil, make([]uint32, 16))
	if _, err := decodeXWD(full[:len(full)-8]); err == nil {
		t.Fatal("a truncated XWD decoded; it must be refused")
	}
}

func TestPlaytestABlankFrameIsUnderTheFloorAndADrawnOneIsOver(t *testing.T) {
	t.Parallel()
	// The two regimes the floor separates, at their measured extremes: a
	// screen with nothing on it carries exactly one color, and every drawn
	// frame observed in this image carried over 1400.
	blank := image.NewRGBA(image.Rect(0, 0, 32, 32))
	for i := range blank.Pix {
		if i%4 == 3 {
			blank.Pix[i] = 0xff
		}
	}
	if got := distinctColors(blank); got != 1 {
		t.Errorf("a uniform frame carries %d distinct colors, want 1", got)
	}
	if got := distinctColors(blank); got >= playtestBlankFloor {
		t.Errorf("a uniform frame counts %d colors, which is not under the floor of %d",
			got, playtestBlankFloor)
	}

	drawn := image.NewRGBA(image.Rect(0, 0, 32, 32))
	for y := 0; y < 32; y++ {
		for x := 0; x < 32; x++ {
			drawn.SetRGBA(x, y, color.RGBA{R: uint8(x * 8), G: uint8(y * 8), A: 0xff})
		}
	}
	if got := distinctColors(drawn); got < playtestBlankFloor {
		t.Errorf("a drawn frame counts %d colors, under the floor of %d", got, playtestBlankFloor)
	}
}

// The paint gate is the piece of the capture a human looking at a picture
// cannot check either, and for the same reason as the decoder: a gate that
// silently waits for the WRONG thing produces a picture that still looks
// like a picture. So its script is built by a pure function and pinned here.

func TestPlaytestPaintGateScriptIsAboutItsOwnToken(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		token string
		want  string
	}{
		{"an ordinary capture token", "capture-1", `['capture-1']`},
		{"a second capture, whose slot must not be the first's", "capture-2", `['capture-2']`},
		{"a token carrying the quote that delimits it", "cap'ture", `['cap\'ture']`},
		{"a token carrying the escape character", `cap\ture`, `['cap\\ture']`},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			script := playtestPaintGateScript(tc.token)
			if !strings.Contains(script, playtestPaintStateGlobal+tc.want) {
				t.Errorf("the gate for %q reads %s%s nowhere in:\n%s",
					tc.token, playtestPaintStateGlobal, tc.want, script)
			}
		})
	}
}

func TestPlaytestPaintGateWaitsForTheMeasuredFrameCount(t *testing.T) {
	t.Parallel()
	// The count is the whole claim: one frame proves only that the page was
	// ASKED, because the first requestAnimationFrame callback runs before
	// that frame is painted. A gate that stopped naming the constant would
	// wait for a number nobody measured.
	script := playtestPaintGateScript("capture-1")
	want := "state.frames >= " + strconv.Itoa(playtestPaintFrames)
	if !strings.Contains(script, want) {
		t.Errorf("the gate does not answer on %q, so it is not waiting for the measured %d frames:\n%s",
			want, playtestPaintFrames, script)
	}
}

func TestPlaytestPaintGateDiagnosesItsOwnNo(t *testing.T) {
	t.Parallel()
	// A wait that fails prints its last value, and the value has to say what
	// the page was doing -- the same rule pageYes follows.
	script := playtestPaintGateScript("capture-1")
	if !strings.Contains(script, `return "yes"`) {
		t.Error("the gate never answers \"yes\", so no wait on it could be satisfied")
	}
	if !strings.Contains(script, `"no: the page has delivered " + state.frames`) {
		t.Errorf("the gate's \"no\" does not carry the frame count it is at:\n%s", script)
	}
}

func TestPlaytestCountsThePixelsNearAColor(t *testing.T) {
	t.Parallel()
	// A 4x4 image: the top half is the wanted color exactly, the third row is
	// two channels off it, and the bottom row is nothing like it.
	want := color.RGBA{R: 0xff, G: 0x00, B: 0xff, A: 0xff}
	img := image.NewRGBA(image.Rect(0, 0, 4, 4))
	for x := 0; x < 4; x++ {
		img.SetRGBA(x, 0, want)
		img.SetRGBA(x, 1, want)
		img.SetRGBA(x, 2, color.RGBA{R: 0xfd, G: 0x02, B: 0xff, A: 0xff})
		img.SetRGBA(x, 3, color.RGBA{R: 0x11, G: 0x22, B: 0x33, A: 0xff})
	}

	tests := []struct {
		name      string
		tolerance uint8
		want      int
	}{
		{"exactly the color, and nothing else counted", 0, 8},
		{"a tolerance too small to reach the near row", 1, 8},
		{"a tolerance that reaches the near row", 2, 12},
		{"a tolerance that still cannot reach an unrelated color", 8, 12},
		{"a tolerance wide enough to swallow the whole image", 0xff, 16},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			if got := countColorWithin(img, want, tc.tolerance); got != tc.want {
				t.Errorf("counting within %d of %v answered %d, want %d", tc.tolerance, want, got, tc.want)
			}
		})
	}
}

func TestPlaytestCountsNoPixelsOfAColorThatIsNotThere(t *testing.T) {
	t.Parallel()
	// The zero the defect produced: the proof region was in the DOM and not
	// on the glass, so the capture carried none of its color at all.
	img := image.NewRGBA(image.Rect(0, 0, 8, 8))
	for y := 0; y < 8; y++ {
		for x := 0; x < 8; x++ {
			img.SetRGBA(x, y, color.RGBA{R: 0x20, G: 0x20, B: 0x20, A: 0xff})
		}
	}
	if got := countColorWithin(img, color.RGBA{R: 0xff, B: 0xff, A: 0xff}, 8); got != 0 {
		t.Errorf("a frame with none of the color counted %d pixels of it, want 0", got)
	}
}

func TestPlaytestGeometryIsTheDisplaysOwn(t *testing.T) {
	t.Parallel()
	// The manifest declares a geometry and the assertions hold every capture
	// to it, so it must be the geometry the display was actually started at
	// rather than a second spelling of it.
	if got, want := fmt.Sprintf("%dx%d", playtestFrameWidth, playtestFrameHeight),
		strings.Join(strings.Split(xvfbScreen, "x")[:2], "x"); got != want {
		t.Errorf("the playtest geometry is %s, want the display's own %s", got, want)
	}
}

// fakeSettleClock is time as `settleFramebuffer` sees it, under the test's
// control. `Sleep` only advances the reading; nothing here waits on anything,
// so the settle logic is exercised at full speed and without a display.
type fakeSettleClock struct {
	now time.Time
}

func (c *fakeSettleClock) Now() time.Time { return c.now }

func (c *fakeSettleClock) Sleep(d time.Duration) { c.now = c.now.Add(d) }

// settledAt is when a window opened at `opened` closes, on the grid of poll
// instants `settleFramebuffer` actually reads at. It is derived rather than
// written out so the expectations below follow the constants instead of
// restating a number that would go stale beside them.
func settledAt(opened time.Duration) time.Duration {
	for at := opened; ; at += playtestSettleInterval {
		if at-opened >= playtestSettleWindow {
			return at
		}
	}
}

// boundExceededAt is the first poll instant past `playtestSettleBound`, which
// is when a screen that never holds still gives up.
func boundExceededAt() time.Duration {
	for at := time.Duration(0); ; at += playtestSettleInterval {
		if at > playtestSettleBound {
			return at
		}
	}
}

func TestPlaytestSettlesOnlyAfterAWholeQuietWindow(t *testing.T) {
	t.Parallel()
	// The defect this covers: two reads a few ms apart agreed on a frame that
	// had not yet received a paint already committed in the DOM, and the
	// capture was declared settled at 2ms and 5ms onto a torn picture. A
	// window longer than a display frame is what makes the agreement mean
	// something.
	//
	// `frame` is the body of the nth read, counted from the first.
	tests := []struct {
		name        string
		frame       func(n int) string
		wantSettled bool
		wantBody    string
		wantTook    time.Duration
	}{
		{
			name:        "a screen already still settles when the window closes and not one poll before",
			frame:       func(int) string { return "still" },
			wantSettled: true,
			wantBody:    "still",
			wantTook:    settledAt(0),
		},
		{
			name: "a paint landing inside the window restarts it",
			frame: func(n int) string {
				if n < 2 {
					return "before the paint"
				}
				return "after the paint"
			},
			wantSettled: true,
			wantBody:    "after the paint",
			// The third read (n=2) is two intervals in, and its window runs
			// from there rather than from the start.
			wantTook: settledAt(2 * playtestSettleInterval),
		},
		{
			name:        "a screen that never holds still runs the budget out",
			frame:       func(n int) string { return fmt.Sprintf("frame %d", n) },
			wantSettled: false,
			wantTook:    boundExceededAt(),
			// The answer is the LAST read, which is the frame the manifest's
			// torn-frame note is about.
			wantBody: fmt.Sprintf("frame %d", int(boundExceededAt()/playtestSettleInterval)),
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			// Arrange.
			clock := &fakeSettleClock{now: time.Unix(0, 0)}
			reads := 0
			read := func() []byte {
				body := tc.frame(reads)
				reads++
				return []byte(body)
			}

			// Act.
			body, settled, took := settleFramebuffer(read, clock)

			// Assert.
			if settled != tc.wantSettled {
				t.Errorf("settled=%v, want %v", settled, tc.wantSettled)
			}
			if string(body) != tc.wantBody {
				t.Errorf("the answered frame is %q, want %q", body, tc.wantBody)
			}
			if took != tc.wantTook {
				t.Errorf("it took %s, want %s", took, tc.wantTook)
			}
		})
	}
}

func TestPlaytestSettleWindowOutlastsADisplayFrame(t *testing.T) {
	t.Parallel()
	// The measurement the constant is stated in: a display frame is 16.7ms at
	// 60Hz, and both torn captures were settled at 2-5ms — well inside one. A
	// window that does not outlast a frame period could not have caught
	// either of them, whatever the poll interval is.
	const displayFrame = 16700 * time.Microsecond
	if playtestSettleWindow <= displayFrame {
		t.Errorf("the settle window is %s, which does not outlast one %s display frame",
			playtestSettleWindow, displayFrame)
	}
	if playtestSettleWindow >= playtestSettleBound {
		t.Errorf("the settle window is %s and the whole patience budget is %s: a window "+
			"no capture can complete would make every picture torn",
			playtestSettleWindow, playtestSettleBound)
	}
}

// ---------------------------------------------------------------------------
// THE PLAYBOOK
// ---------------------------------------------------------------------------

// playtestArtifactsSubdir is where a run's PNGs and manifests land under the
// suite's own artifacts root.
const playtestArtifactsSubdir = "playtest"

// playtestManifestFile is the file a reviewer reads beside the pictures.
const playtestManifestFile = "MANIFEST.md"

// playbook is one scripted sequence of user actions with a picture after
// every step.
type playbook struct {
	t *testing.T
	e *Emacs

	// Name is the playbook's own directory under the artifacts root.
	Name string
	// Dir is that directory, made once.
	Dir string

	// awaitPaint blocks until the webview has delivered
	// playtestPaintFrames animation frames raised AFTER it was called, and
	// answers how long that took. See playtestPaintFrames: without it a
	// capture photographs whatever the webview's offscreen surface happened
	// to hold, which is not necessarily the DOM the step just asserted.
	//
	// IT IS NIL UNTIL A PANEL IS OPEN, and that is a real state rather than
	// an unset option: several playbooks photograph an editor with no
	// workspace registered in it at all, and there is no page in those
	// frames to have painted anything. `playtestScenario.openPanel` installs
	// it the moment one exists.
	awaitPaint func() time.Duration

	// holdMotion freezes the page's resting animations for the duration of
	// one capture and answers the release. See playtestMotionPaused: the
	// webapp's prompt-bubble wave, state dots and footer breath run
	// FOREVER by design, so a capture of a page carrying any of them can
	// only run its whole patience budget out and photograph a torn frame.
	//
	// IT IS NIL UNTIL A PANEL IS OPEN, for the same reason `awaitPaint` is:
	// a frame with no page in it has no animation to hold.
	holdMotion func() func()

	// lastSettled and lastSettle are what the most recent `capture` learned
	// about the screen it photographed. They exist for ONE caller: the
	// substrate's own proof that an idle frame settles (see
	// playtest_substrate_settle_test.go). The manifest note is for a human
	// and the log line is for a reader, and neither is a claim anything
	// fails on — so the fact a test can assert is kept here rather than
	// scraped back out of either.
	lastSettled bool
	lastSettle  time.Duration

	step     int
	manifest *os.File
}

// newPlaybook claims the playbook's artifact directory and opens its
// manifest.
//
// It FAILS rather than skipping when no artifacts root is set. A playtest
// whose pictures go nowhere has done nothing at all, and reporting that as a
// pass is the one outcome a visual suite must never have; `bin/playtest.sh`
// is what sets the variable, and the failure names it.
func newPlaybook(t *testing.T, e *Emacs, name, purpose string) *playbook {
	t.Helper()

	root := os.Getenv(ArtifactsEnv)
	if root == "" {
		t.Fatalf("the playtest writes pictures and there is nowhere to write them: %s is unset. "+
			"Run it through modules/app/agent-repl/bin/playtest.sh, which sets it.", ArtifactsEnv)
	}
	dir := filepath.Join(root, playtestArtifactsSubdir, name)
	// THE PLAYBOOK OWNS ITS DIRECTORY AND SWEEPS ITS OWN, which is why the
	// runner does not sweep the tree. A picture left by an earlier run is
	// indistinguishable from one this run took, and that is the one way a
	// visual review reaches a confident wrong answer -- but a runner that
	// swept everything would also delete the twelve playbooks a `-run` of one
	// playbook does not re-take.
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("sweep the previous run's %s: %v", dir, err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("prepare the playbook directory %s: %v", dir, err)
	}

	manifest, err := os.Create(filepath.Join(dir, playtestManifestFile))
	if err != nil {
		t.Fatalf("create the playbook manifest in %s: %v", dir, err)
	}
	p := &playbook{t: t, e: e, Name: name, Dir: dir, manifest: manifest}
	t.Cleanup(func() {
		if err := manifest.Close(); err != nil {
			t.Errorf("close the playbook manifest: %v", err)
		}
		t.Logf("playtest %s: %d steps in %s", name, p.step, dir)
	})

	p.write("# Playbook: %s\n\n%s\n\n", name, purpose)
	p.write("- Geometry: **%dx%d**, fixed — every capture in every playbook is this size, so the pictures compare run to run.\n",
		playtestFrameWidth, playtestFrameHeight)
	p.write("- Vendor: **the fake SDK only**. `AGENT_REPL_FORBID_VENDOR_CALLS=1` is on the Emacs process and inherited by the daemon, the shim, the store and the sidecar, so no real Claude call can occur.\n")
	p.write("- Git: the scripted fake git. No real git process runs anywhere.\n")
	p.write("- Every picture is the X server's own screen memory, so what is drawn INSIDE the webview is in it.\n")
	p.write("- FUNCTIONAL FIRST: every step carries a programmatic assertion, and a step that cannot start fails the run right there. A picture is taken ONLY where the step's subject is visual, and only AFTER its assertion passed, so no picture here is a picture of a broken world.\n\n")
	p.write("| # | the act | asserted | image | what the image must show |\n|---|---|---|---|---|\n")
	return p
}

func (p *playbook) write(format string, args ...any) {
	p.t.Helper()
	if _, err := fmt.Fprintf(p.manifest, format, args...); err != nil {
		p.t.Fatalf("write the playbook manifest: %v", err)
	}
}

// prepareFrame puts the Emacs frame into the shape every capture is taken
// in: the whole screen, and no cursor blink.
//
// THE BLINK IS OFF BECAUSE OF THE SETTLE, not for looks. `settleFrame` waits
// for the framebuffer to hold still for a whole `playtestSettleWindow`, and a
// blinking cursor guarantees it never does — so every capture in every
// playbook would run its whole patience budget and then photograph an
// animating screen.
func (p *playbook) prepareFrame() {
	p.t.Helper()
	p.e.Eval(fmt.Sprintf(`(progn
             (blink-cursor-mode -1)
             (set-frame-size (selected-frame) %d %d t)
             (redisplay t)
             t)`, playtestFrameWidth, playtestFrameHeight))
}

// note records a step that has NO picture.
//
// Most steps are like this. Per PLAYTEST-PLAN.md's "Functional first,
// pictures second", a capture is taken only where the step's subject is
// VISUAL -- the webapp's rendering and the tab bar's painted state -- and
// photographing anything else only gives a reviewer more pictures to read
// and no more to decide. The step is still recorded, because a manifest a
// reviewer follows must carry the whole sequence and not only the parts
// that produced an image.
//
// `asserted` names what the step PROVED programmatically. It is not
// re-checked here: the assertion has already run, and a step whose
// assertion failed never reaches this line.
func (p *playbook) note(act, asserted string) {
	p.t.Helper()
	p.step++
	p.write("| %02d | %s | %s | — | — |\n", p.step, mdCell(act), mdCell(asserted))
}

// mdCell makes one string safe to place in a markdown table cell.
//
// A manifest row is written with `|` as the column separator, so a `|` in
// any cell -- and the substrate produces them, `tabFaceFor` joins the faces
// it found with " | " -- silently splits that row into extra columns. The
// manifest then still LOOKS like a table and is wrong, which is worse than
// a manifest that fails to render: a reviewer reads the shifted columns as
// the harness's answer.
//
// Every cell this file writes goes through here, not only the ones known to
// carry a pipe today, because the cells are built from product strings and
// which of them can carry one is not this file's to know.
func mdCell(s string) string {
	return strings.ReplaceAll(s, "|", "\\|")
}

// capture takes one picture, holds it to the mechanical assertions, and
// records what a reviewer should see in it.
//
// IT IS CALLED ONLY AFTER THE STEP'S OWN FUNCTIONAL ASSERTION HAS PASSED,
// which is the plan's rule and not a convention: a reviewer must never be
// handed a picture of a world that was already broken, because then every
// difference in it is unexplained.
//
// `act` is what was just done, `asserted` is what the step proved
// programmatically, and `expected` is the sentence the reviewer checks the
// picture against. The last of those is never asserted, because what the
// picture SHOWS is the reviewer's judgment and the whole reason this suite
// exists.
//
// It answers the decoded capture, so a caller that has a MECHANICAL claim
// about the pixels -- the substrate's own proof that the glass carries the
// DOM change, in playtest_00_feed_tail_test.go -- can make it on the very
// image that was written. Every other caller ignores it: what a picture
// SHOWS is the reviewer's judgment.
func (p *playbook) capture(step, act, asserted, expected string) *image.RGBA {
	p.t.Helper()
	p.step++
	name := fmt.Sprintf("%02d-%s.png", p.step, step)

	// THE PAGE'S RESTING ANIMATIONS ARE HELD FIRST, and released the moment
	// this capture has its bytes. See playtestMotionPaused. It is FIRST
	// because everything below it — the redraws, the paint gate and the
	// settle — is a wait for the screen to stop changing, and against an
	// animation that never stops that wait has no answer.
	if p.holdMotion != nil {
		defer p.holdMotion()()
	}

	// THE WHOLE FRAME IS REDRAWN, AND THAT IS A MEASUREMENT.
	//
	// The tab bar is repainted by Emacs's C redisplay, which keeps the last
	// items vector it built and compares the next one with `equal` -- a
	// comparison that ignores text properties. The product answers that
	// structurally: `agent-repl-workspace-tabline-formatted` leads every
	// render with `agent-repl--tabline-render-key`, an invisible generation
	// that advances exactly when the rendered rows change in any way, faces
	// included, and a roster push schedules the redisplay itself. So a
	// capture never busts anything: `force-mode-line-update` makes redisplay
	// rebuild the items, and `redisplay` runs it now. A tab bar wrong after
	// that is wrong in the product.
	p.redrawFrame()

	// AND THE PAGE'S OWN FRAMES, BETWEEN THE TWO REDRAWS. The redraw above
	// is the kick -- it is what an idle Emacs on an Xvfb nobody types into
	// needs before it draws what it has already decided. The redraw below is
	// what COPIES the webview's offscreen surface onto the glass, and it has
	// to happen after the page has produced a frame for the DOM this step
	// asserted, or the copy is of the previous page state and the damage
	// signal for the real frame lands mid-capture. See playtestPaintFrames.
	paint := time.Duration(0)
	if p.awaitPaint != nil {
		paint = p.awaitPaint()
	}
	p.redrawFrame()

	body, settled, took, rounds := p.settleFrame()
	p.lastSettled, p.lastSettle = settled, took
	img, err := decodeXWD(body)
	if err != nil {
		p.t.Fatalf("capture %s: decode the framebuffer at %s: %v", name, p.e.Display.FramebufferPath, err)
	}

	if got := img.Bounds(); got.Dx() != playtestFrameWidth || got.Dy() != playtestFrameHeight {
		p.t.Errorf("capture %s is %dx%d, want the declared %dx%d",
			name, got.Dx(), got.Dy(), playtestFrameWidth, playtestFrameHeight)
	}
	colors := distinctColors(img)
	if colors < playtestBlankFloor {
		p.t.Errorf("capture %s carries %d distinct colors, under the blank floor of %d: "+
			"the screen was empty, so nothing in this picture is worth reviewing",
			name, colors, playtestBlankFloor)
	}

	path := filepath.Join(p.Dir, name)
	file, err := os.Create(path)
	if err != nil {
		p.t.Fatalf("create %s: %v", path, err)
	}
	if err := png.Encode(file, img); err != nil {
		closeErr := file.Close()
		p.t.Fatalf("encode %s: %v (closing it said: %v)", path, err, closeErr)
	}
	if err := file.Close(); err != nil {
		p.t.Fatalf("close %s: %v", path, err)
	}

	note := ""
	if !settled {
		// NOT AN ERROR, AND NOT SILENT EITHER. See playtestSettleBound: a
		// surface that is genuinely animating never settles, and a reviewer
		// reading a torn picture is owed the reason.
		note = fmt.Sprintf(" _(the screen was still changing after %s, so this frame may be torn)_", playtestSettleBound)
	}
	p.write("| %02d | %s | %s | `%s` | %s%s |\n", p.step, mdCell(act), mdCell(asserted), mdCell(name), mdCell(expected), note)
	p.t.Logf("playtest phase capture-%02d-%s settled=%v paint %s settle %s over %d redisplay rounds (%d distinct colors)",
		p.step, step, settled, paint.Round(time.Millisecond), took.Round(time.Millisecond), rounds, colors)
	return img
}

// redrawFrame makes the next redisplay draw EVERY glyph of the frame, twice,
// and runs it now.
//
// WHY `redraw-frame`, AND WHY TWICE. Emacs draws this frame double buffered
// and swaps with XdbeCopied, which promises the back buffer a copy of the
// front after each swap. This X server does not keep that promise: measured,
// a tab bar drawn in one redisplay was on the glass, gone after the next
// redisplay that changed anything at all, and back after the one after that
// -- the two buffers alternate, and Emacs, trusting the copy, redraws only
// the glyphs it believes changed. An incremental redisplay therefore lands
// the new tab bar in ONE buffer, and which buffer is on the glass at the
// read is parity. Garbaging the frame makes the next redisplay draw EVERY
// glyph; doing it twice puts the complete current frame in both buffers, so
// the read is right whichever one is in front.
func (p *playbook) redrawFrame() {
	p.t.Helper()
	p.e.Eval(`(progn
             (force-mode-line-update t)
             (redraw-frame)
             (redisplay t)
             (redraw-frame)
             (redisplay t)
             t)`)
}

// settleFrame drives full redisplays until the framebuffer has held still
// across a whole `playtestSettleWindow`, and answers the last read either
// way, with the number of redisplay rounds it took.
//
// WHY A REDISPLAY SITS BETWEEN THE READS. Reads with nothing driven between
// them agree trivially on a screen Emacs has simply not repainted yet, and
// that is a picture of the PREVIOUS state passed off as settled. MEASURED,
// on the tab bar after a roster push opened a new tab: with the two garbaged
// redisplays above already done, the bar still showed the tab set from
// before the push in three of four registrations, and one more
// `(redraw-frame) (redisplay t)` showed the new one every time. So the
// quiescence this waits for is "further full redisplays changed nothing",
// which is a property of the screen rather than of the poll interval, and
// the round count is logged so a capture that needed more than the window's
// own rounds is on the record.
//
// AND THE REDISPLAY BETWEEN THE READS IS `redrawFrame`, THE DOUBLE ONE. It
// used to be a single `(redraw-frame) (redisplay t)`, which is exactly the
// incremental-parity hazard `redrawFrame` documents: one garbaged redisplay
// fills ONE of the two buffers, and this X server does not honour XdbeCopied,
// so which buffer the next read sees is a coin toss. MEASURED, by the
// settle diagnostic over one run of the D29-D32 playbook: 4 of 645 changed
// rounds reported the WHOLE 1280x1024 frame changing at once, in consecutive
// pairs — the two buffers alternating under the reads, on a screen where
// nothing had been redrawn. Nothing else in a run moves a third of a million
// pixels between two reads 20ms apart. Doing what the capture's own kick does
// — garbage and redisplay twice — puts the complete frame in both buffers
// every round, so the window closes on the glass rather than on the parity.
//
// AND WHY THE READS ARE AN INTERVAL APART, EACH BEHIND ITS OWN EVAL. On pgtk
// `redisplay` paints Emacs's own surface; the pixels reach the X server only
// when GTK's main loop runs, which happens while Emacs waits for input --
// after an eval has answered, never inside it. A read taken the instant an
// eval returns can therefore precede the flush of the very redisplay it
// asked for. MEASURED: with one redisplay between the reads and no interval,
// the tab bar after a roster push opened a new tab still read as the
// previous tab set in one run of three, the "settled" read and the one
// before it agreeing because neither had been flushed yet. So every read
// compared here follows an eval of its own, with the poll interval between
// them, and the first read is the one that OPENS the window rather than one
// half of a pair.
//
// The window logic itself is `settleFramebuffer`, which takes its reads and
// its clock, so the one thing here a picture cannot show -- WHEN a screen is
// declared settled -- is exercised without a display. The rounds are counted
// in the read this passes it, because driving the redisplay is this
// playbook's business and not the window's.
func (p *playbook) settleFrame() (body []byte, settled bool, took time.Duration, rounds int) {
	p.t.Helper()
	diag := os.Getenv(playtestSettleDiagEnv) != ""
	var last []byte
	redisplayAndRead := func() []byte {
		rounds++
		p.redrawFrame()
		body := p.readFramebuffer()
		if diag && last != nil {
			box, changed, err := xwdChangedBounds(last, body)
			switch {
			case err != nil:
				p.t.Logf("playtest settle diag round %d: %v", rounds, err)
			case changed > 0:
				p.t.Logf("playtest settle diag round %d: %d pixels changed in %v (%dx%d)",
					rounds, changed, box, box.Dx(), box.Dy())
			}
		}
		last = body
		return body
	}
	body, settled, took = settleFramebuffer(redisplayAndRead, realSettleClock{})
	return body, settled, took, rounds
}

// settleClock is the passage of time `settleFramebuffer` measures and waits
// on. Production passes the real one; the unit tests drive a fake, so no test
// in this file ever sleeps.
type settleClock interface {
	Now() time.Time
	Sleep(time.Duration)
}

type realSettleClock struct{}

func (realSettleClock) Now() time.Time        { return time.Now() }
func (realSettleClock) Sleep(d time.Duration) { time.Sleep(d) }

// settleFramebuffer re-reads at `playtestSettleInterval` and answers settled
// once every read across a full `playtestSettleWindow` has agreed with the
// read that opened that window. ANY change restarts the window, so the frame
// it answers with is always the one that then held still.
//
// `playtestSettleBound` caps the whole wait: a surface that never holds still
// answers its LAST read with settled=false, which is the manifest's torn-frame
// note and not a failure.
//
// THE SLEEP BETWEEN READS IS NOT SYNCHRONIZATION, and there is nothing here
// to synchronize on: the X server's screen memory is a file that changes
// with no notification of any kind, so the only way to learn it stopped
// changing is to look again. The interval is the package's own poll
// interval, the window is the measured quiescence bound stated at
// `playtestSettleWindow`, and neither stands in for a channel that could
// have carried the fact.
func settleFramebuffer(read func() []byte, clock settleClock) (body []byte, settled bool, took time.Duration) {
	started := clock.Now()
	deadline := started.Add(playtestSettleBound)
	held := read()
	opened := clock.Now()
	for {
		now := clock.Now()
		if now.Sub(opened) >= playtestSettleWindow {
			return held, true, now.Sub(started)
		}
		if now.After(deadline) {
			return held, false, now.Sub(started)
		}
		clock.Sleep(playtestSettleInterval)
		current := read()
		if string(current) != string(held) {
			held = current
			opened = clock.Now()
		}
	}
}

// readFramebuffer copies the X server's live screen memory.
func (p *playbook) readFramebuffer() []byte {
	p.t.Helper()
	body, err := os.ReadFile(p.e.Display.FramebufferPath)
	if err != nil {
		p.t.Fatalf("read the Xvfb framebuffer at %s: %v", p.e.Display.FramebufferPath, err)
	}
	return body
}

// TestPlaytestTheChangedBoxIsTheSmallestOneHoldingEveryMovedPixel is the
// diagnostic's whole claim: a reviewer reads the culprit off the box, so the
// box has to be the pixels that moved and nothing else.
func TestPlaytestTheChangedBoxIsTheSmallestOneHoldingEveryMovedPixel(t *testing.T) {
	t.Parallel()
	// A 4x4 field with two pixels moved, at (1,1) and (2,3): the tight box
	// is x in [1,3), y in [1,4), and a box that merely CONTAINED them would
	// pass a looser assertion while naming the wrong surface on a real
	// frame.
	base := make([]uint32, 16)
	moved := make([]uint32, 16)
	copy(moved, base)
	moved[1*4+1] = 0x00ff0000
	moved[3*4+2] = 0x0000ff00

	box, changed, err := xwdChangedBounds(buildXWD(t, 4, 4, nil, base), buildXWD(t, 4, 4, nil, moved))
	if err != nil {
		t.Fatalf("bound the change between two well-formed dumps: %v", err)
	}
	if changed != 2 {
		t.Errorf("the diagnostic counted %d moved pixels, want the 2 that moved", changed)
	}
	if want := image.Rect(1, 1, 3, 4); box != want {
		t.Errorf("the changed box is %v, want the tight %v", box, want)
	}
}

// TestPlaytestTwoIdenticalFramesChangedNothing keeps the settled case honest:
// a diagnostic that reported a box for a still screen would accuse a surface
// that never moved.
func TestPlaytestTwoIdenticalFramesChangedNothing(t *testing.T) {
	t.Parallel()
	pixels := []uint32{0x00ff0000, 0x0000ff00, 0x000000ff, 0x00ffffff}
	body := buildXWD(t, 2, 2, nil, pixels)

	box, changed, err := xwdChangedBounds(body, buildXWD(t, 2, 2, nil, pixels))
	if err != nil {
		t.Fatalf("bound the change between two identical dumps: %v", err)
	}
	if changed != 0 {
		t.Errorf("the diagnostic counted %d moved pixels between two identical dumps, want 0", changed)
	}
	if !box.Empty() {
		t.Errorf("the changed box for two identical dumps is %v, want an empty one", box)
	}
}

// TestPlaytestPaddingBeyondTheRowIsNotAChange holds the diagnostic to the
// PIXELS rather than to the bytes. A row's stride may exceed its pixels, and
// whatever X leaves in that padding is not on the glass -- so a diagnostic
// that compared whole rows would report a box for a frame nobody could see
// move.
func TestPlaytestPaddingBeyondTheRowIsNotAChange(t *testing.T) {
	t.Parallel()
	// Two 1x2 dumps at a stride of three pixels, agreeing on their one real
	// pixel per row and differing in every padding word.
	pad := func(fill uint32) []byte {
		return buildXWD(t, 1, 2, func(h []uint32) { h[xwdBytesPerLine] = 3 * 4 },
			[]uint32{0x00112233, fill, fill, 0x00445566, fill, fill})
	}

	box, changed, err := xwdChangedBounds(pad(0x00000000), pad(0x00ffffff))
	if err != nil {
		t.Fatalf("bound the change between two padded dumps: %v", err)
	}
	if changed != 0 {
		t.Errorf("the diagnostic counted %d moved pixels, want 0: only the row padding differed", changed)
	}
	if !box.Empty() {
		t.Errorf("the changed box is %v, want an empty one: only the row padding differed", box)
	}
}

// TestPlaytestTwoGeometriesHaveNoBoxBetweenThem refuses rather than answers.
// A run whose screen changed size has a fact about the harness in it, and a
// per-pixel box invented across two geometries would bury it.
func TestPlaytestTwoGeometriesHaveNoBoxBetweenThem(t *testing.T) {
	t.Parallel()
	_, _, err := xwdChangedBounds(buildXWD(t, 2, 2, nil, make([]uint32, 4)),
		buildXWD(t, 3, 2, nil, make([]uint32, 6)))

	if err == nil {
		t.Fatal("two dumps of different geometry were bounded against each other; want a refusal")
	}
	if !strings.Contains(err.Error(), "2x2") || !strings.Contains(err.Error(), "3x2") {
		t.Errorf("the refusal says %q, and never names both geometries", err)
	}
}

// TestPlaytestATruncatedDumpIsRefusedByName keeps the diagnostic's own
// failure legible: a short read of the framebuffer must say which of the two
// dumps was short, not panic inside a slice.
func TestPlaytestATruncatedDumpIsRefusedByName(t *testing.T) {
	t.Parallel()
	whole := buildXWD(t, 2, 2, nil, make([]uint32, 4))

	_, _, err := xwdChangedBounds(whole, whole[:len(whole)-4])

	if err == nil {
		t.Fatal("a truncated dump was bounded; want a refusal")
	}
	if !strings.Contains(err.Error(), "later") {
		t.Errorf("the refusal says %q, and never says WHICH of the two dumps was short", err)
	}
}

// TestPlaytestHoldingMotionSetsTheFlagTheStylesheetReads keeps the Go side
// and the stylesheet on ONE spelling of the attribute. The rule that freezes
// the animations is `:root[data-motion="paused"]`, and a script that wrote
// any other name would set an attribute nothing reads -- the capture would
// then settle or not by luck, with no failure anywhere to say why.
func TestPlaytestHoldingMotionSetsTheFlagTheStylesheetReads(t *testing.T) {
	t.Parallel()
	got := playtestHoldMotionScript()

	if !strings.Contains(got, jsString(playtestMotionAttribute)) {
		t.Errorf("the hold script is %q, and never names the %s attribute the stylesheet reads",
			got, playtestMotionAttribute)
	}
	if !strings.Contains(got, jsString(playtestMotionPaused)) {
		t.Errorf("the hold script is %q, and never writes the %q value the stylesheet matches",
			got, playtestMotionPaused)
	}
}

// TestPlaytestReleasingMotionRemovesTheFlagRatherThanBlankingIt is the other
// half, and the distinction matters: the stylesheet matches the attribute's
// VALUE, so a release that wrote an empty string would leave the attribute
// on the element for every later reader to find and would not obviously be
// wrong. It is removed.
func TestPlaytestReleasingMotionRemovesTheFlagRatherThanBlankingIt(t *testing.T) {
	t.Parallel()
	got := playtestReleaseMotionScript()

	if !strings.Contains(got, "removeAttribute") {
		t.Errorf("the release script is %q, and never removes the attribute", got)
	}
	if strings.Contains(got, "setAttribute") {
		t.Errorf("the release script is %q; it writes the attribute rather than removing it", got)
	}
}
