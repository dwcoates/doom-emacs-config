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

// fcMatch runs fc-match and fails the test loudly if it cannot.
func fcMatch(t *testing.T, args ...string) string {
	t.Helper()
	out, err := exec.Command("fc-match", args...).CombinedOutput()
	if err != nil {
		t.Fatalf("fc-match %v failed in the sandbox: %v\n%s", args, err, out)
	}
	return string(out)
}
