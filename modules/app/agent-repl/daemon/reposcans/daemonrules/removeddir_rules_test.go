package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// TestARemovedDirectoryIsRoutedThroughWorkspaceUnlessGoneAlone holds each
// resolver that resolves a workspace's log sink to the shared helper for a
// removed directory, with no hand-rolled branch on it beside the helper.
func TestARemovedDirectoryIsRoutedThroughWorkspaceUnlessGoneAlone(t *testing.T) {
	cases := []struct{ dir, file string }{
		// THE FEED'S SINK IS RESOLVED AS THE WORKSPACE IS PRIMED (prime.go).
		{internalResolveFeedDir, "prime.go"},
		{internalResolveFooterDir, "resolver.go"},
	}
	for _, tc := range cases {
		t.Run(tc.dir, func(t *testing.T) {
			// Arrange
			b, err := os.ReadFile(filepath.Join(sourcescan.DaemonRoot(t), tc.dir, tc.file))
			if err != nil {
				t.Fatal(err)
			}
			src := string(b)

			// Act, Assert: the shared helper is taken, and no hand-rolled branch on
			// the removed directory stands beside it.
			if !strings.Contains(src, "dlog.WorkspaceUnlessGone(") || strings.Contains(src, "dlog.ErrWorkspaceDirGone") {
				t.Fatalf("%s does not route a removed directory through dlog.WorkspaceUnlessGone alone", tc.file)
			}
		})
	}
}
