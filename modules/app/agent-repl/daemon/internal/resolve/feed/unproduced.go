package feed

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// THE IMAGE ORIGIN HAS NO PRODUCER.
//
// Turning an ImageBlock into a `src` a webview can load needs an asset origin
// that serves the referenced bytes on the daemon's own host. Nothing in the
// landed daemon serves one: the asset origin serves the webapp's dist
// directory and nothing else, and no component maps an image reference onto a
// path beneath it.
//
// So the resolver is wired with an EXPLICIT REFUSAL rather than left nil or
// given an improvised mapping. A nil resolver refuses too, but silently as a
// missing dependency; this one names the producer that is missing, at the row
// that needed it, so the gap is legible in the daemon's own log and in the
// drawn refusal rather than being discovered as a blank image.

// UnproducedImageResolver is the image resolver the composition root wires
// while no asset origin produces image sources. Every call refuses LOUDLY and
// names what is missing.
func UnproducedImageResolver(log dlog.Logger) ImageResolver {
	return func(block *conversationv1.ImageBlock) (string, string, error) {
		log.Error("daemon.feed.image_unproduced",
			"a prompt carried an image and no asset origin produces a source for it",
			dlog.Context{"block": blockDescription(block)})
		return "", "", fmt.Errorf(
			"feed: no producer resolves an image reference into a servable source; " +
				"the daemon serves no image asset origin")
	}
}

// blockDescription names an image reference for a record without leaking its
// bytes into the log.
func blockDescription(block *conversationv1.ImageBlock) string {
	if block == nil {
		return "an unset image block"
	}
	return fmt.Sprintf("%T", block.GetLocation())
}
