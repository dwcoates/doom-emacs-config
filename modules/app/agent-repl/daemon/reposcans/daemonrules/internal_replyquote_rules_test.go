package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalReplyquoteDir is the package these rules hold, under the daemon module's root.
const internalReplyquoteDir = "internal/replyquote"

// TestEveryFenceInThePackageIsComposedByFenced pins that the package's quote
// composes its fence through Fenced: a second hand-built fence-text-fence
// would drift from it (another separator, a fence one short).
func TestEveryFenceInThePackageIsComposedByFenced(t *testing.T) {
	sourcescan.InPackage(t, internalReplyquoteDir)
	// Arrange / Act
	calls := sourcescan.Count(t, internalReplyquoteDir, "Fence(")

	// Assert: Fence's own definition and Fenced's one call of it.
	if calls != 2 {
		t.Fatalf("Fence( occurs %d times in the package, want 2: its definition and Fenced's one call", calls)
	}
}
