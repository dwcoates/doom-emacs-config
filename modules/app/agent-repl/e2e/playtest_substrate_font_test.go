//go:build playtest

package e2e

import (
	"os/exec"
	"strings"
	"testing"
)

// THE SUBSTRATE'S FONT PRECONDITION. A playtest exists to be LOOKED AT: the
// metaprompt's tree glyphs (🎯 and friends) reach the webapp through the
// shim, and the Emacs frame paints emoji in its tab bar. An image with no
// color emoji font draws every one of them as a tofu box, and a picture of
// tofu cannot be reviewed against its manifest sentence -- so a whole run's
// output is thrown away with nothing wrong in the product.
//
// That failure is silent everywhere else: the captures exist, they are the
// declared geometry, and they clear the blank-frame floor. It is caught here
// instead, before any playbook spends an Emacs slot, by asking fontconfig
// the same question the renderers ask it.
//
// These run INSIDE the container (the whole playtest suite does), so
// `fc-match` here is the image's own fontconfig. See
// `e2e/sandbox/fontconfig/99-agent-repl-emoji.conf`.

// emojiFontFamily is the family the image installs and every check below
// expects to land on.
const emojiFontFamily = "Noto Color Emoji"

// TestPlaytestSandboxResolvesTheEmojiFamilyToAColorEmojiFont is the direct
// question: does this image have an emoji font at all?
func TestPlaytestSandboxResolvesTheEmojiFamilyToAColorEmojiFont(t *testing.T) {
	// Arrange.
	requireSandbox(t)

	// Act.
	got := fcMatch(t, "--format=%{family}", "emoji")

	// Assert.
	if !strings.Contains(got, emojiFontFamily) {
		t.Errorf("`fc-match emoji` answered %q, want a match naming %q; this image has no color "+
			"emoji font, so every playtest picture draws tofu where an emoji belongs "+
			"(rebuild with `e2e/sandbox/bin/e2e-sandbox.sh build`)", got, emojiFontFamily)
	}
}

// TestPlaytestSandboxListsAnInstalledColorEmojiFont separates the two ways
// the previous check can fail: the font is missing from the image entirely,
// versus installed but unreachable through the alias.
func TestPlaytestSandboxListsAnInstalledColorEmojiFont(t *testing.T) {
	// Arrange.
	requireSandbox(t)

	// Act.
	out, err := exec.Command("fc-list").CombinedOutput()
	if err != nil {
		t.Fatalf("fc-list failed in the sandbox: %v\n%s", err, out)
	}

	// Assert.
	if !strings.Contains(string(out), emojiFontFamily) {
		t.Errorf("`fc-list` names no %q; the font package is not installed in this image at all, "+
			"which is a different fault from an alias that does not resolve", emojiFontFamily)
	}
}

// TestPlaytestSandboxGenericFamiliesFallBackToTheColorEmojiFont covers the
// other half. Neither Emacs's pgtk backend nor WebKit asks for "emoji" by
// name: each asks for a generic family and walks fontconfig's fallback list
// for a face that carries the codepoint. A font installed but absent from
// that list still draws tofu.
func TestPlaytestSandboxGenericFamiliesFallBackToTheColorEmojiFont(t *testing.T) {
	// Arrange: the generic families every surface in the picture resolves
	// through -- the webapp's CSS stacks end in one of them, and the Emacs
	// frame's default face is the third.
	tests := []struct {
		name    string
		generic string
	}{
		{"the webapp's prose falls back", "sans-serif"},
		{"a serif stack falls back", "serif"},
		{"the Emacs frame's own family falls back", "monospace"},
	}

	requireSandbox(t)

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act: `-s` prints the whole sorted fallback list, which is the
			// list a renderer walks.
			got := fcMatch(t, "-s", `--format=%{family}\n`, tc.generic)

			// Assert.
			if !strings.Contains(got, emojiFontFamily) {
				t.Errorf("%q cannot fall back to %q -- its fallback list is:\n%s\nan emoji in a %q "+
					"run of text therefore draws as tofu", tc.generic, emojiFontFamily, got, tc.generic)
			}
		})
	}
}

// TestPlaytestSandboxGenericFamiliesResolveToATextFace is the other side of
// the fallback, and it is the one the shipped rule got wrong.
//
// A fallback is only a fallback if it LOSES to the text faces ahead of it.
// The rule bound Noto Color Emoji into each generic family with
// `binding="same"` -- strong, for a family an application names -- while the
// distro's DejaVu entries in those lists are weak, and a strong family match
// outranks a weak one. So `fc-match sans-serif` answered "Noto Color Emoji"
// and every renderer in the image took a color emoji font as its text font.
func TestPlaytestSandboxGenericFamiliesResolveToATextFace(t *testing.T) {
	// Arrange: the same three generics the fallback check walks -- the
	// webapp's CSS stacks end in one of them and the Emacs frame's default
	// face is the third.
	tests := []struct {
		name    string
		generic string
	}{
		{"the webapp's prose", "sans-serif"},
		{"a serif stack", "serif"},
		{"the Emacs frame's own family", "monospace"},
	}

	requireSandbox(t)

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := strings.TrimSpace(fcMatch(t, "--format=%{family}", tc.generic))

			// Assert.
			if strings.Contains(got, emojiFontFamily) {
				t.Errorf("`fc-match %s` answered %q: the color emoji font is being PREFERRED for the "+
					"generic family rather than kept as a last resort, so every glyph it happens to "+
					"carry -- the digits and the space -- is drawn from a color bitmap. See "+
					"e2e/sandbox/fontconfig/99-agent-repl-emoji.conf", tc.generic, got)
			}
		})
	}
}

// TestPlaytestSandboxATextCodepointResolvesToATextFace is the defect at its
// narrowest: the codepoints Noto Color Emoji carries that are NOT emoji.
//
// Its charset is the emoji plus U+0020, U+0023, U+002A and U+0030-U+0039 --
// the pieces a keycap sequence is composed from. A renderer looking for a
// face that has one of those asks fontconfig with the codepoint in the
// pattern's charset, which is what these queries are, and the answer must be
// the text face every letter beside it came from.
func TestPlaytestSandboxATextCodepointResolvesToATextFace(t *testing.T) {
	// Arrange: one query per codepoint the emoji font overlaps text on, plus
	// a letter it does not carry as the control.
	tests := []struct {
		name    string
		pattern string
	}{
		{"a digit", "sans-serif:charset=0030"},
		{"a space", "sans-serif:charset=0020"},
		{"a hash", "sans-serif:charset=0023"},
		{"an asterisk", "sans-serif:charset=002a"},
		{"a letter, which the emoji font never carried", "sans-serif:charset=006b"},
		{"a digit in the frame's monospace", "monospace:charset=0030"},
		{"a space in the frame's monospace", "monospace:charset=0020"},
	}

	requireSandbox(t)

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := strings.TrimSpace(fcMatch(t, "--format=%{family}", tc.pattern))

			// Assert.
			if strings.Contains(got, emojiFontFamily) {
				t.Errorf("`fc-match %s` answered %q, want the text face: this codepoint is in the emoji "+
					"font's charset for keycap sequences only, and drawing it from there makes it "+
					"uncolorable and laid out on an emoji advance", tc.pattern, got)
			}
		})
	}
}

// webappStatusGlyphs is every non-ASCII glyph the webapp draws as a STATUS
// MARKER -- the roster and footer arms, the fold triangles, the tone marks,
// the metaprompt tree's connectors -- with the source that draws each.
//
// WHY THE LIST EXISTS. Owner 13 filed the `running…` badge's marker as a
// glyph this image had stopped covering. It is not a glyph at all (see
// playtest_substrate_spinner_test.go), but the question it raised is a real
// one and nothing was asking it: the image ships exactly two font families,
// and every one of these markers has to come out of the text one. A marker
// no face carries draws as tofu, and a marker the COLOR face wins draws as
// an uncolorable bitmap on an emoji advance -- the same fault the digits and
// the space had. Both are invisible to `fc-match emoji`.
//
// The emoji this same webapp draws (✅ ✏️ 🔧 👀 📊, and the metaprompt tree's
// own) are deliberately NOT here: they are the color font's job, and the
// tests above are the ones that hold it.
var webappStatusGlyphs = []struct {
	name      string
	codepoint string
}{
	{"the fold triangle, closed (webapp/src/sidebar/row.ts)", "25b8"},
	{"the fold triangle, open (webapp/src/feed/bubble.ts)", "25be"},
	{"the fold triangle, lifted (webapp/src/fold.ts)", "25b4"},
	{"the row menu's ellipsis (webapp/src/sidebar/roster.ts)", "22ef"},
	{"the trailing ellipsis (webapp/src/feed/cards/tool-call.ts)", "2026"},
	{"the queue tone (webapp/src/sidebar/tones.ts)", "2261"},
	{"the recycle tone (webapp/src/sidebar/tones.ts)", "27f3"},
	{"the conflict tone (webapp/src/sidebar/tones.ts)", "21c4"},
	{"the failed tone (webapp/src/sidebar/tones.ts)", "2715"},
	{"the merge glyph (webapp/src/feed/merge/merge.ts)", "21c4"},
	{"the failed mark (webapp/src/footer/expanded.ts)", "2717"},
	{"the passed mark (webapp/src/footer/strip.ts)", "2713"},
	{"the agents glyph (webapp/src/footer/expanded.ts)", "2699"},
	{"the pending task glyph (webapp/src/panels/panels.ts)", "2610"},
	{"the RUNNING task glyph (webapp/src/panels/panels.ts)", "25d0"},
	{"the completed task glyph (webapp/src/panels/panels.ts)", "2611"},
	{"the monitors glyph (webapp/src/footer/expanded.ts)", "25c9"},
	{"the crons glyph (webapp/src/footer/expanded.ts)", "25f7"},
	{"the alarm glyph (webapp/src/footer/strip.ts)", "26a0"},
	{"the live tab mark (webapp/src/feed/merge/tab-strip.ts)", "25cf"},
	{"the parked tab mark (webapp/src/feed/merge/tab-strip.ts)", "2016"},
	{"the stop glyph (webapp/src/footer/stop.ts)", "25a0"},
	{"the failure overlay's mark (webapp/src/failure/overlay.ts)", "2759"},
	{"the metaprompt tree's rail (webapp/src/metaprompt-tree.ts)", "2502"},
	{"the metaprompt tree's tee (webapp/src/metaprompt-tree.ts)", "251c"},
	{"the metaprompt tree's elbow (webapp/src/metaprompt-tree.ts)", "2514"},
	{"the metaprompt tree's dash (webapp/src/metaprompt-tree.ts)", "2500"},
}

// TestPlaytestSandboxCoversEveryStatusGlyphTheWebappDraws is the "genuinely
// uncovered" branch: a codepoint NO installed face carries.
//
// `fc-match` cannot answer it -- it always names its best candidate, covered
// or not, so it says "DejaVu Sans" for a codepoint DejaVu has never had.
// `fc-list :charset=` is the question that can come back empty.
func TestPlaytestSandboxCoversEveryStatusGlyphTheWebappDraws(t *testing.T) {
	// Arrange.
	requireSandbox(t)

	for _, tc := range webappStatusGlyphs {
		t.Run(tc.name, func(t *testing.T) {
			// Act: every family in the image that carries this codepoint.
			out, err := exec.Command("fc-list", ":charset="+tc.codepoint, "family").CombinedOutput()
			if err != nil {
				t.Fatalf("fc-list :charset=%s failed in the sandbox: %v\n%s", tc.codepoint, err, out)
			}
			got := strings.TrimSpace(string(out))

			// Assert: SOME face carries it, and a text one -- the color
			// emoji font alone would draw an uncolorable bitmap where a
			// status marker that follows the CSS color belongs.
			if got == "" {
				t.Errorf("no font in this image carries U+%s, so %s draws as a TOFU BOX in every "+
					"picture; install a face that covers it (see e2e/sandbox/Dockerfile)",
					strings.ToUpper(tc.codepoint), tc.name)
				return
			}
			if !strings.Contains(got, "DejaVu") {
				t.Errorf("only %q carries U+%s -- %s is therefore drawn from a color bitmap, which "+
					"ignores the CSS color the marker is painted in", got, strings.ToUpper(tc.codepoint), tc.name)
			}
		})
	}
}

// TestPlaytestSandboxResolvesEveryStatusGlyphToATextFace is the other half:
// a codepoint BOTH fonts carry must still come from the text one.
//
// Four of these markers are in Noto Color Emoji's charset as well (the gear,
// the checked ballot box, the warning sign, the pencil), which is the exact
// shape of the digits-and-space defect: a strong family binding made the
// color font win a codepoint the text font also had, and the glyph stopped
// following the CSS color.
func TestPlaytestSandboxResolvesEveryStatusGlyphToATextFace(t *testing.T) {
	// Arrange.
	requireSandbox(t)

	for _, tc := range webappStatusGlyphs {
		t.Run(tc.name, func(t *testing.T) {
			// Act: the webapp's own stacks end in `sans-serif`.
			got := strings.TrimSpace(fcMatch(t, "--format=%{family}", "sans-serif:charset="+tc.codepoint))

			// Assert.
			if strings.Contains(got, emojiFontFamily) {
				t.Errorf("`fc-match sans-serif:charset=%s` answered %q, want the text face: %s would "+
					"be drawn from a color bitmap, uncolorable and laid out on an emoji advance",
					tc.codepoint, got, tc.name)
			}
		})
	}
}

// fcMatch runs fc-match and fails the test loudly if it cannot.
func fcMatch(t *testing.T, args ...string) string {
	t.Helper()
	out, err := exec.Command("fc-match", args...).CombinedOutput()
	if err != nil {
		t.Fatalf("fc-match %v failed in the sandbox: %v\n%s", args, err, out)
	}
	return string(out)
}
