package daemonrules

import (
	"reflect"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalPrompthandlerDir is the package these rules hold, under the daemon module's root.
const internalPrompthandlerDir = "internal/prompthandler"

// TestNoNoteSiteHandRollsItsBlocks pins every daemon note to withNote: a site
// that splices a note block by hand fails here rather than drifting from it.
func TestNoNoteSiteHandRollsItsBlocks(t *testing.T) {
	sourcescan.InPackage(t, internalPrompthandlerDir)
	// Act
	sites := sourcescan.Sites(t, internalPrompthandlerDir, "&conversationv1.UserContentBlock{")

	// Assert
	if !reflect.DeepEqual(sites, []string{"handler.go"}) || sourcescan.Count(t, internalPrompthandlerDir, "&conversationv1.UserContentBlock{") != 1 {
		t.Fatalf("note blocks built in %v; build them only in withNote", sites)
	}
}
