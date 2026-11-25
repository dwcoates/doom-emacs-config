package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalShimclientDir is the package these rules hold, under the daemon module's root.
const internalShimclientDir = "internal/shimclient"

// connected() is the one place a link is announced, so no site can publish
// LinkConnected without advancing the count a consumer relies on.
func TestOnlyConnectedAnnouncesALink(t *testing.T) {
	sourcescan.InPackage(t, internalShimclientDir)
	// Act
	announcements := sourcescan.Count(t, internalShimclientDir, "publish(LinkConnected)")

	// Assert
	if announcements != 1 {
		t.Fatalf("production source publishes LinkConnected at %d sites; only connected() may", announcements)
	}
}
