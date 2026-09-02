package feed

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// TestUnproducedImageResolverRefusesAndNamesTheMissingProducer covers the
// deliberate absence: with no asset origin serving image sources, the resolver
// refuses with a sentence that says WHAT is missing rather than answering an
// empty src.
func TestUnproducedImageResolverRefusesAndNamesTheMissingProducer(t *testing.T) {
	// Arrange.
	resolve := UnproducedImageResolver(dlog.NewTestLogger())

	// Act.
	src, alt, err := resolve(&conversationv1.ImageBlock{
		Location: &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: "/tmp/shot.png"}},
	})

	// Assert.
	if err == nil {
		t.Fatal("the unproduced image resolver answered a source, want a refusal")
	}
	if !strings.Contains(err.Error(), "no producer") {
		t.Fatalf("refusal = %q, want it to name the missing producer", err)
	}
	if src != "" || alt != "" {
		t.Fatalf("src, alt = %q, %q, want both empty on a refusal", src, alt)
	}
}

// TestUnproducedImageResolverRecordsTheGap covers the log: the gap must be
// legible in the daemon's own narrative, not only in the drawn refusal.
func TestUnproducedImageResolverRecordsTheGap(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()
	resolve := UnproducedImageResolver(log)

	// Act.
	_, _, _ = resolve(&conversationv1.ImageBlock{
		Location: &conversationv1.ImageBlock_Url{Url: &conversationv1.ImageBlockUrl{Url: "https://example.invalid/a.png"}},
	})

	// Assert.
	var found bool
	for _, r := range log.Records() {
		if r.Operation == "daemon.feed.image_unproduced" && r.Level == "error" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %v, want an error under daemon.feed.image_unproduced", log.Records())
	}
}
