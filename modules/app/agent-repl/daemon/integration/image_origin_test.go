//go:build integration

package integration

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// THE ATTACHED IMAGE IS DRAWN, AND ITS BYTES ARE SERVED BY THE DAEMON.
//
// The composer attaches an image as an `ImageBlock{path}` beside the words
// (`lisp/clipboard-image.el`), and the webapp draws it as an `<img>` whose
// `src` the DAEMON resolved (`webapp/src/feed/rows/blocks.ts`). Nothing in
// between could do it: the webview has no filesystem and the editor is not an
// origin. These tests are that whole hop end to end against a real daemon --
// the drawn block carries a src, and a GET of that src off the daemon's own
// listener answers the file's bytes.

// attachedPNG is a one-pixel PNG, so what the origin serves is a real image
// rather than a string that happens to be bytes.
var attachedPNG = []byte{
	0x89, 'P', 'N', 'G', 0x0d, 0x0a, 0x1a, 0x0a,
	0x00, 0x00, 0x00, 0x0d, 'I', 'H', 'D', 'R',
	0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,
	0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,
	0x89,
}

// saidWithImage builds the UserSaid the composer submits for a prompt with an
// attachment: the words first, then the image, in that order.
func saidWithImage(text, path, mediaType string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}}},
			{Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
				Location:  &conversationv1.ImageBlock_Path{Path: &conversationv1.ImageBlockPath{Path: path}},
				MediaType: mediaType,
			}}},
		},
	}}
}

// writeAttachment writes the sample PNG somewhere the daemon can read it.
func writeAttachment(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "clip.png")
	if err := os.WriteFile(path, attachedPNG, 0o644); err != nil {
		t.Fatalf("write the attachment: %v", err)
	}
	return path
}

// submitWithAttachment submits one prompt carrying PATH and answers the drawn
// row's image block.
func submitWithAttachment(t *testing.T, f *fixture, path string) *frontendv1.FeedImageBlock {
	t.Helper()
	feed := f.watchRootFeed()
	f.submitRaw(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           saidWithImage("what is in this picture?", path, "image/png"),
		IdempotencyKey: "k-image",
		Origin:         origin,
	})
	row := awaitRow(t, f, feed, "the mirrored user_prompt row", func(r *frontendv1.FeedRow) bool {
		return r.GetUserPrompt() != nil
	})
	blocks := row.GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 2 {
		t.Fatalf("the drawn prompt carries %d blocks, want the words and the image: %v", len(blocks), blocks)
	}
	image := blocks[1].GetImage()
	if image == nil {
		t.Fatalf("the drawn prompt's second block is %v, want an image block", blocks[1].GetBlock())
	}
	return image
}

// TestAnAttachedImageIsDrawnAsAnImageBlock covers the drawn arm: an attached
// image reaches the page as an image block with a source, never as the
// `unsupported block: image` placeholder a missing resolver produced.
func TestAnAttachedImageIsDrawnAsAnImageBlock(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	path := writeAttachment(t)

	// Act.
	image := submitWithAttachment(t, f, path)

	// Assert.
	if image.GetSrc() == "" {
		t.Fatalf("the drawn image block carries no src: %v", image)
	}
	if image.GetAlt() != "clip.png" {
		t.Errorf("the drawn image block's alt is %q, want the file's own name", image.GetAlt())
	}
}

// TestTheDrawnImageSourceServesTheAttachedBytes covers the other half of the
// hop: the src is loadable off the daemon's own listener and answers the file.
func TestTheDrawnImageSourceServesTheAttachedBytes(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})
	path := writeAttachment(t)
	image := submitWithAttachment(t, f, path)

	// Act.
	resp, err := http.Get("http://" + f.d.Addr + image.GetSrc())
	if err != nil {
		t.Fatalf("GET %s: %v", image.GetSrc(), err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read the served image: %v", err)
	}

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("the image origin answered %d for %s, want 200", resp.StatusCode, image.GetSrc())
	}
	if got := resp.Header.Get("Content-Type"); got != "image/png" {
		t.Errorf("the served content type is %q, want the record's %q", got, "image/png")
	}
	if string(body) != string(attachedPNG) {
		t.Errorf("the origin served %d bytes, want the attachment's %d", len(body), len(attachedPNG))
	}
}

// TestAnUnregisteredImageIDIsRefused is the arbitrary-file-read refusal
// against a REAL daemon: a path no conversation carried has no id, so nothing
// a page can ask for reaches it.
func TestAnUnregisteredImageIDIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newOpened(t, harness.Opts{})

	// Act.
	resp, err := http.Get("http://" + f.d.Addr +
		"/feed-images/0000000000000000000000000000000000000000000000000000000000000000")
	if err != nil {
		t.Fatalf("GET an unregistered image id: %v", err)
	}
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("the image origin answered %d for an unregistered id, want 404", resp.StatusCode)
	}
}
