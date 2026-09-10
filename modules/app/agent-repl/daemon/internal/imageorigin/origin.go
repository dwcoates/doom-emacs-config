// Package imageorigin serves the images a drawn feed refers to.
//
// A `conversationv1.ImageBlock` with a `path` arm names A FILE ON THE
// PRODUCER'S HOST, and the daemon runs on that host — so the daemon is the
// one component that can turn such a reference into something a webview can
// load. Nothing else could: the webview speaks HTTP and has no filesystem,
// and the editor that composed the attachment is not an origin.
//
// THE ORIGIN SERVES ONLY WHAT A FEED ALREADY DREW, and that is what keeps it
// from being an arbitrary-file-read endpoint on the developer's machine. A
// path becomes servable ONLY by being registered, and a path is registered
// ONLY by the feed resolver drawing a record that referenced it. So a request
// naming a path nobody's conversation carried has no id to ask for: the
// illegal read is unrepresentable in the request rather than rejected by a
// check that a later refactor could drop. The id is the digest of the path,
// so registering the same path twice is the same id and the registry cannot
// grow one entry per redraw.
package imageorigin

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"claude-repld/internal/dlog"
)

// Route is the origin's mount point on the daemon's own mux. It ends in a
// slash because the id is the rest of the path.
const Route = "/feed-images/"

// op is the origin's operation name.
const op = "daemon.imageorigin"

// entry is one registered image: the host path and the media type the record
// stated. The media type is the RECORD'S, never sniffed from the extension —
// the producer knew what it captured and said so.
type entry struct {
	path      string
	mediaType string
}

// Origin is the registry and the handler together: registering is what makes
// a path servable, so the two cannot be wired apart.
type Origin struct {
	log dlog.Logger

	mu      sync.RWMutex
	entries map[string]entry
}

// New builds an empty origin.
func New(log dlog.Logger) (*Origin, error) {
	if log == nil {
		return nil, fmt.Errorf("imageorigin: a logger is required")
	}
	return &Origin{log: log, entries: make(map[string]entry)}, nil
}

// Register makes PATH servable under MEDIATYPE and answers the source a
// client loads it from — a root-relative URL, because the webapp is served
// from this same origin and a host in the src would pin the page to whatever
// address the daemon happened to bind at.
//
// It refuses a relative path: the arm's own documentation says the path is
// absolute, and a relative one would be resolved against the DAEMON's working
// directory, which is not the producer's and is nobody's intent.
func (o *Origin) Register(path, mediaType string) (string, error) {
	switch {
	case path == "":
		return "", fmt.Errorf("imageorigin: an image reference carries no path")
	case !filepath.IsAbs(path):
		return "", fmt.Errorf("imageorigin: the image path %q is not absolute", path)
	case mediaType == "":
		return "", fmt.Errorf("imageorigin: the image at %s states no media type", path)
	}
	sum := sha256.Sum256([]byte(path))
	id := hex.EncodeToString(sum[:])
	o.mu.Lock()
	_, already := o.entries[id]
	o.entries[id] = entry{path: path, mediaType: mediaType}
	o.mu.Unlock()
	o.log.Debug(op, "registered an image reference as servable",
		dlog.Context{"id": id, "path": path, "media_type": mediaType, "already_registered": already})
	return Route + id, nil
}

// Handler serves a registered image by id.
func (o *Origin) Handler() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet && r.Method != http.MethodHead {
			o.log.Debug(op, "refused a non-read method on the image origin",
				dlog.Context{"method": r.Method, "path": r.URL.Path})
			http.Error(w, "the image origin serves GET and HEAD only", http.StatusMethodNotAllowed)
			return
		}
		id := strings.TrimPrefix(r.URL.Path, Route)
		o.mu.RLock()
		found, ok := o.entries[id]
		o.mu.RUnlock()
		if !ok {
			// NOT AN ERROR RECORD: an id nobody registered is the ordinary
			// answer for a stale page asking again after a restart, and the
			// origin holds its registry only in memory.
			o.log.Debug(op, "no image is registered under this id", dlog.Context{"id": id})
			http.NotFound(w, r)
			return
		}
		file, err := os.Open(found.path)
		if err != nil {
			o.log.Error(op, "a registered image could not be opened",
				dlog.Context{"id": id, "path": found.path, "cause": err.Error()})
			http.Error(w, "the registered image is not readable", http.StatusNotFound)
			return
		}
		defer file.Close()
		info, err := file.Stat()
		if err != nil {
			o.log.Error(op, "a registered image could not be stat'd",
				dlog.Context{"id": id, "path": found.path, "cause": err.Error()})
			http.Error(w, "the registered image is not readable", http.StatusNotFound)
			return
		}
		// The media type is the RECORD's, so it is set before ServeContent
		// gets a chance to sniff one out of the first bytes.
		w.Header().Set("Content-Type", found.mediaType)
		o.log.Debug(op, "served a registered image",
			dlog.Context{"id": id, "path": found.path, "media_type": found.mediaType, "bytes": info.Size()})
		http.ServeContent(w, r, "", info.ModTime(), file)
	})
}
