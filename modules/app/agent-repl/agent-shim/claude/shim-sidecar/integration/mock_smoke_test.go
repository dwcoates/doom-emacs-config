package integration

import (
	"testing"
	"time"
)

func TestMockGeneratesATranscriptForASimpleScenario(t *testing.T) {
	start := time.Now()
	tree := generateMock(t, "!md", waitTerminal)
	t.Logf("generate took %s", time.Since(start))
	if len(tree.Transcripts) == 0 {
		t.Fatalf("the mocked vendor wrote no transcript under %s (log: %s)", tree.ConfigRoot, tree.LogPath)
	}
	t.Logf("session=%s transcripts=%v subagents=%v spools=%v", tree.VendorSessionID, tree.Transcripts, tree.Subagents, tree.Spools)
}
