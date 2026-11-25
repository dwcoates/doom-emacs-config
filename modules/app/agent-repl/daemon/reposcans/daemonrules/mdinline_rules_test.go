package daemonrules

import (
	"path/filepath"
	"testing"

	"agent-repld/reposcans/internal/mdinlinetest"
	"agent-repld/reposcans/internal/sourcescan"
)

// TestEveryProseRewriteWalksThroughTheSharedWalk holds each rewrite of drawn
// prose to the one shared walk (mdinline.RewriteProse).
func TestEveryProseRewriteWalksThroughTheSharedWalk(t *testing.T) {
	cases := []struct{ dir, file, fn string }{
		{"internal/pullrefs", "pullrefs.go", "Linkify"},
		{"internal/slackrefs", "slackrefs.go", "Linkify"},
		{"internal/codespans", "codespans.go", "Markup"},
	}
	for _, tc := range cases {
		t.Run(tc.dir, func(t *testing.T) {
			// Act, Assert
			mdinlinetest.AssertRewriteShape(t, filepath.Join(sourcescan.DaemonRoot(t), tc.dir, tc.file), tc.fn)
		})
	}
}

// TestEveryLinkRewriteWritesThroughWriteLink holds every link a rewrite adds
// to mdinline.WriteLink.
func TestEveryLinkRewriteWritesThroughWriteLink(t *testing.T) {
	cases := []struct{ dir, file string }{
		{"internal/pullrefs", "pullrefs.go"},
		{"internal/slackrefs", "slackrefs.go"},
	}
	for _, tc := range cases {
		t.Run(tc.dir, func(t *testing.T) {
			// Act, Assert
			mdinlinetest.AssertNoHandWrittenLinks(t, filepath.Join(sourcescan.DaemonRoot(t), tc.dir, tc.file))
		})
	}
}
