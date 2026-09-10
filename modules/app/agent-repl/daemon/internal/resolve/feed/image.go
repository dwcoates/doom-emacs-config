package feed

import (
	"fmt"
	"path/filepath"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// HOW AN IMAGE REFERENCE BECOMES A `src`.
//
// `ImageBlock` has two location arms and they resolve differently, which is
// the whole reason the proto separated them:
//
//   - `path` names a file on the PRODUCER's host. The daemon runs on that
//     host, so it registers the path with its own image origin and answers
//     the origin's URL. Nothing else in the system could: the webview has no
//     filesystem, and the editor is not an origin.
//   - `url` is already fetchable and is answered VERBATIM. Re-serving it
//     through the daemon would put the daemon in the middle of a vendor fetch
//     it has no reason to be in.
//
// An unset arm is a REFUSAL, not an empty src: an `<img src="">` reloads the
// page's own document in every browser, which is the one outcome worse than
// drawing nothing.

// ImageRegistrar makes a host path servable and answers the source a client
// loads it from. `imageorigin.Origin.Register` is the implementation; the
// resolver takes the function so this package keeps no dependency on the
// server's side of the daemon.
type ImageRegistrar func(path, mediaType string) (string, error)

// PathImageResolver resolves an image reference against the daemon's own
// image origin.
func PathImageResolver(register ImageRegistrar, log dlog.Logger) (ImageResolver, error) {
	if register == nil {
		return nil, fmt.Errorf("feed: an image registrar is required")
	}
	if log == nil {
		return nil, fmt.Errorf("feed: a logger is required")
	}
	return func(block *conversationv1.ImageBlock) (string, string, error) {
		switch location := block.GetLocation().(type) {
		case *conversationv1.ImageBlock_Path:
			path := location.Path.GetPath()
			src, err := register(path, block.GetMediaType())
			if err != nil {
				log.Error("daemon.feed.image_unregistrable",
					"a prompt's image path could not be made servable",
					dlog.Context{"path": path, "media_type": block.GetMediaType(), "cause": err.Error()})
				return "", "", fmt.Errorf("feed: register the image at %q: %w", path, err)
			}
			// The ALT text is the file's own name: it is what the user
			// recognizes the attachment by, and it is the same name the
			// composer's marker line drew when they attached it.
			alt := filepath.Base(path)
			log.Debug("daemon.feed.image_resolved",
				"a prompt's image path resolved to a source on the daemon's image origin",
				dlog.Context{"path": path, "src": src})
			return src, alt, nil
		case *conversationv1.ImageBlock_Url:
			url := location.Url.GetUrl()
			if url == "" {
				log.Error("daemon.feed.image_unresolvable",
					"a prompt's image carries a url arm with no url", dlog.Context{})
				return "", "", fmt.Errorf("feed: an image's url arm carries no url")
			}
			log.Debug("daemon.feed.image_resolved",
				"a prompt's image url is answered verbatim", dlog.Context{"url": url})
			return url, "", nil
		default:
			log.Error("daemon.feed.image_unresolvable",
				"a prompt's image states no location, so it has no source",
				dlog.Context{"location": blockDescription(block)})
			return "", "", fmt.Errorf("feed: an image block states no location")
		}
	}, nil
}

// blockDescription names an image reference for a record without leaking its
// bytes into the log.
func blockDescription(block *conversationv1.ImageBlock) string {
	if block == nil {
		return "an unset image block"
	}
	return fmt.Sprintf("%T", block.GetLocation())
}
