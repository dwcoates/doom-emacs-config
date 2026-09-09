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
// capture reads the framebuffer until TWO CONSECUTIVE READS AGREE BYTE FOR
// BYTE, which is a real quiescence and not an interval anybody guessed.
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
// for two consecutive reads of the framebuffer to agree, and a blinking
// cursor guarantees they never do — so every capture in every playbook would
// run its whole patience budget and then photograph an animating screen.
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
	p.write("| %02d | %s | %s | — | — |\n", p.step, act, asserted)
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
func (p *playbook) capture(step, act, asserted, expected string) {
	p.t.Helper()
	p.step++
	name := fmt.Sprintf("%02d-%s.png", p.step, step)

	// THE WHOLE FRAME IS REDRAWN, TWICE, AND THAT IS A MEASUREMENT.
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
	//
	// WHY `redraw-frame`, AND WHY TWICE. Emacs draws this frame double
	// buffered and swaps with XdbeCopied, which promises the back buffer a
	// copy of the front after each swap. This X server does not keep that
	// promise: measured, a tab bar drawn in one redisplay was on the glass,
	// gone after the next redisplay that changed anything at all, and back
	// after the one after that -- the two buffers alternate, and Emacs,
	// trusting the copy, redraws only the glyphs it believes changed. An
	// incremental redisplay therefore lands the new tab bar in ONE buffer,
	// and which buffer is on the glass at the read is parity. Garbaging the
	// frame makes the next redisplay draw EVERY glyph; doing it twice puts
	// the complete current frame in both buffers, so the read is right
	// whichever one is in front.
	p.e.Eval(`(progn
             (force-mode-line-update t)
             (redraw-frame)
             (redisplay t)
             (redraw-frame)
             (redisplay t)
             t)`)

	body, settled, took, rounds := p.settleFrame()
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
	p.write("| %02d | %s | %s | `%s` | %s%s |\n", p.step, act, asserted, name, expected, note)
	p.t.Logf("playtest phase capture-%02d-%s settled=%v took %s over %d redisplay rounds (%d distinct colors)",
		p.step, step, settled, took.Round(time.Millisecond), rounds, colors)
}

// settleFrame drives full redisplays until two CONSECUTIVE ones leave the
// framebuffer identical, and answers the last read either way, with the
// number of redisplay rounds it took.
//
// WHY A REDISPLAY SITS BETWEEN THE TWO READS. Two reads a few milliseconds
// apart with nothing driven between them agree trivially on a screen Emacs
// has simply not repainted yet, and that is a picture of the PREVIOUS
// state passed off as settled. MEASURED, on the tab bar after a roster push
// opened a new tab: with the two garbaged redisplays above already done,
// the bar still showed the tab set from before the push in three of four
// registrations, and one more `(redraw-frame) (redisplay t)` showed the new
// one every time. So the quiescence this waits for is "a further full
// redisplay changed nothing", which is a property of the screen rather than
// of the poll interval, and the round count is logged so a capture that
// needed more than one is on the record.
//
// AND WHY THE READS ARE AN INTERVAL APART, EACH BEHIND ITS OWN EVAL. On pgtk
// `redisplay` paints Emacs's own surface; the pixels reach the X server only
// when GTK's main loop runs, which happens while Emacs waits for input --
// after an eval has answered, never inside it. A read taken the instant an
// eval returns can therefore precede the flush of the very redisplay it
// asked for. MEASURED: with one redisplay between the reads and no interval,
// the tab bar after a roster push opened a new tab still read as the
// previous tab set in one run of three, the "settled" read and the one
// before it agreeing because neither had been flushed yet. So BOTH reads
// compared here follow an eval of their own, with the poll interval between
// them, and the first read taken before any eval is never one of the pair.
func (p *playbook) settleFrame() (body []byte, settled bool, took time.Duration, rounds int) {
	p.t.Helper()
	started := time.Now()
	deadline := started.Add(playtestSettleBound)
	redisplayAndRead := func() []byte {
		rounds++
		p.e.Eval(`(progn (redraw-frame) (redisplay t) t)`)
		return p.readFramebuffer()
	}
	previous := redisplayAndRead()
	for {
		time.Sleep(playtestSettleInterval)
		current := redisplayAndRead()
		if string(current) == string(previous) {
			return current, true, time.Since(started), rounds
		}
		previous = current
		if time.Now().After(deadline) {
			return current, false, time.Since(started), rounds
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
