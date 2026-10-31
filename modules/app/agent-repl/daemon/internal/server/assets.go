package server

import (
	"bytes"
	"fmt"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"agent-repld/internal/dlog"
)

// The static asset origin. The webapp's dist directory is served BENEATH the
// Connect routes on the SAME origin, which is what makes the webview URL
// (`http://<daemon.addr>/?workspace=<id>&dir=<dir>`) and the rpc endpoint one
// host. See ARCHITECTURE.md "rollout" ("Asset origin").
//
// The ENTRY POINT is re-stat'd on every request and answered with
// Cache-Control: no-store, so a rebuilt webapp is picked up by a reload rather
// than by a cache eviction. NOTHING ELSE gets that header: the hashed bundles
// beneath it are immutable by name and must stay cacheable.
//
// THE FEED TEXT ZOOM IS STAMPED INTO THE ENTRY POINT. The document's <html>
// element is served carrying the zoom in force as an inline
// `--feed-text-scale`, the same property feed/feed-text-scale.ts writes, so a
// page's FIRST paint already draws feed text at the user's zoom. The feed
// watch's FeedTextScale push still carries every later change, but it arrives
// only after the first page of rows is painted, which drew a visible snap from
// the default size to the saved one on every page load.

// opAssets is the asset origin's operation name.
const opAssets = "daemon.server.assets"

// entryPoint is the webapp's entry document.
const entryPoint = "index.html"

// assets builds the asset origin handler for Deps.WebappDist.
func (s *server) assets() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.Method != http.MethodGet && r.Method != http.MethodHead {
			s.log.Debug(opAssets, "refused a non-read method on the asset origin",
				dlog.Context{"method": r.Method, "path": r.URL.Path})
			http.Error(w, "the asset origin serves GET and HEAD only", http.StatusMethodNotAllowed)
			return
		}
		clean := path(r.URL.Path)
		if clean == "" || clean == entryPoint {
			s.serveEntryPoint(w, r)
			return
		}
		target := filepath.Join(s.deps.WebappDist, filepath.FromSlash(clean))
		if !within(s.deps.WebappDist, target) {
			s.log.Warn(opAssets, "refused an asset path that escapes the dist directory",
				dlog.Context{"path": r.URL.Path})
			http.NotFound(w, r)
			return
		}
		info, err := os.Stat(target)
		if err != nil || info.IsDir() {
			s.log.Debug(opAssets, "no such asset", dlog.Context{"path": clean})
			http.NotFound(w, r)
			return
		}
		// THE ENTRY BUNDLE IS PAGE LIFECYCLE, recorded at INFO: a page whose
		// load finished but that never attached is told apart from one whose
		// script was never fetched only by this record (2026-10-31, an e2e
		// whose host page never connected and left no record of why).
		if isEntryBundle(clean) {
			s.log.Info(opAssets, "served the webapp's entry bundle", dlog.Context{"path": clean})
		} else {
			s.log.Debug(opAssets, "served an asset", dlog.Context{"path": clean})
		}
		http.ServeFile(w, r, target)
	})
}

// serveEntryPoint answers the entry document, RE-STAT'D on every request so a
// rewritten index.html is served without a restart, and marked no-store so no
// cache holds the old one.
func (s *server) serveEntryPoint(w http.ResponseWriter, r *http.Request) {
	target := filepath.Join(s.deps.WebappDist, entryPoint)
	info, err := os.Stat(target)
	if err != nil {
		s.log.Error(opAssets, "the webapp entry point could not be stat'd",
			dlog.Context{"path": target, "cause": err.Error()})
		http.Error(w, "the webapp entry point is not available", http.StatusNotFound)
		return
	}
	body, err := os.ReadFile(target)
	if err != nil {
		s.log.Error(opAssets, "the webapp entry point could not be read",
			dlog.Context{"path": target, "cause": err.Error()})
		http.Error(w, "the webapp entry point is not readable", http.StatusInternalServerError)
		return
	}
	scale := s.currentFeedTextScale()
	body, err = stampFeedTextScale(body, scale)
	if err != nil {
		s.log.Error(opAssets, "the webapp entry point could not carry the feed text scale",
			dlog.Context{"path": target, "scale": scale, "cause": err.Error()})
		http.Error(w, "the webapp entry point has no single <html> element", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Cache-Control", "no-store")
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	s.log.Info(opAssets, "served the webapp entry point",
		dlog.Context{"path": target, "bytes": len(body), "modified": info.ModTime().String(), "feed_text_scale": scale})
	if r.Method == http.MethodHead {
		w.WriteHeader(http.StatusOK)
		return
	}
	if _, err := w.Write(body); err != nil {
		s.log.Debug(opAssets, "the client went away mid-entry-point",
			dlog.Context{"cause": err.Error()})
	}
}

// isEntryBundle reports whether PATH is the webapp's entry script, the one
// index.html names (buildid's webappEntry).
func isEntryBundle(path string) bool {
	return strings.HasPrefix(path, "assets/index-") && strings.HasSuffix(path, ".js")
}

// htmlOpenTag is the entry document's root element's opening, the one place
// stampFeedTextScale writes.
const htmlOpenTag = "<html"

// stampFeedTextScale writes `scale` onto the entry document's <html> element as
// an inline `--feed-text-scale` (the property styles.css multiplies every feed
// font size by). An inline style on the root element outranks the stylesheet's
// `:root` default, so the stamped value is in force from the first paint. A
// document without exactly one <html> opening tag is a broken build, refused
// rather than served unscaled.
func stampFeedTextScale(body []byte, scale float64) ([]byte, error) {
	if n := bytes.Count(body, []byte(htmlOpenTag)); n != 1 {
		return nil, fmt.Errorf("the entry document carries %d %q opening tags, want exactly 1", n, htmlOpenTag)
	}
	at := bytes.Index(body, []byte(htmlOpenTag)) + len(htmlOpenTag)
	attr := ` style="--feed-text-scale: ` + strconv.FormatFloat(scale, 'g', -1, 64) + `"`
	out := make([]byte, 0, len(body)+len(attr))
	out = append(out, body[:at]...)
	out = append(out, attr...)
	return append(out, body[at:]...), nil
}

// path normalizes a request path to a slash-relative asset path.
func path(raw string) string {
	return strings.TrimPrefix(strings.TrimPrefix(raw, "/"), "./")
}

// within reports whether target stays inside root, which is what keeps a
// "../" path off the rest of the filesystem.
func within(root, target string) bool {
	rel, err := filepath.Rel(root, target)
	if err != nil {
		return false
	}
	return rel != ".." && !strings.HasPrefix(rel, ".."+string(filepath.Separator))
}
