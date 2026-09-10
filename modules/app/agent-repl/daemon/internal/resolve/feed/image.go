package feed

import (
	"fmt"
	"net/url"
	"path"
	"path/filepath"
	"strings"

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
			return resolveImageURL(log, location.Url.GetUrl())
		default:
			log.Error("daemon.feed.image_unresolvable",
				"a prompt's image states no location, so it has no source",
				dlog.Context{"location": blockDescription(block)})
			return "", "", fmt.Errorf("feed: an image block states no location")
		}
	}, nil
}

// safeImageSchemes are the url schemes a resolved `src` may carry. The src is
// assigned to an `<img>`'s `src` on the far end, so the set is stated HERE —
// where the daemon owns the resolution — rather than left for the client to
// sniff. `data:` is in the set because it is what the shim carries a vendor's
// inlined bytes as; anything else is refused rather than passed through.
var safeImageSchemes = map[string]bool{"http": true, "https": true, "data": true}

// resolveImageURL answers a url reference: the url VERBATIM as the src, and
// the file it names as the alt. The url is not rewritten — it is the vendor's
// own, and a daemon that re-composed it would be inventing a source — but it
// is checked, because an unparseable url and a scheme an `<img>` must not be
// handed are both a broken picture on every client rather than a src.
func resolveImageURL(log dlog.Logger, raw string) (string, string, error) {
	if raw == "" {
		log.Error("daemon.feed.image_unresolvable",
			"a prompt's image carries a url arm with no url", dlog.Context{})
		return "", "", fmt.Errorf("feed: an image's url arm carries no url")
	}
	parsed, err := url.Parse(raw)
	if err != nil {
		log.Error("daemon.feed.image_url_unparseable",
			"a prompt's image url cannot be parsed, so no src is drawn from it",
			dlog.Context{"cause": err.Error()})
		return "", "", fmt.Errorf("feed: an image's url is unparseable: %w", err)
	}
	scheme := strings.ToLower(parsed.Scheme)
	if !safeImageSchemes[scheme] {
		log.Error("daemon.feed.image_url_scheme",
			"a prompt's image url names a scheme an image element must not be handed",
			dlog.Context{"scheme": scheme})
		return "", "", fmt.Errorf("feed: an image's url names the unservable scheme %q", scheme)
	}
	log.Debug("daemon.feed.image_resolved",
		"a prompt's image url is answered verbatim", dlog.Context{"url": raw})
	return raw, imageURLAlt(parsed, scheme), nil
}

// imageURLAlt names the picture for a reader who cannot see it. A fetchable
// url names a FILE, exactly as the path arm's alt does; a `data:` url names
// nothing, and an empty alt is what FeedImageBlock says an unnamed image
// carries.
func imageURLAlt(parsed *url.URL, scheme string) string {
	if scheme == "data" {
		return ""
	}
	base := path.Base(parsed.Path)
	if base == "." || base == "/" {
		return ""
	}
	return base
}

// blockDescription names an image reference for a record without leaking its
// bytes into the log.
func blockDescription(block *conversationv1.ImageBlock) string {
	if block == nil {
		return "an unset image block"
	}
	return fmt.Sprintf("%T", block.GetLocation())
}
