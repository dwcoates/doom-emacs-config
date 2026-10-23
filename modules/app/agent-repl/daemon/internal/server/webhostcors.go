package server

import (
	"net/http"
	"regexp"
)

// THE WEB VIEW HOST OUTLIVES THE DAEMON THAT SERVED IT. Emacs keeps one web
// view at the host page (webapp/src/host), loaded from whichever daemon was
// serving when it last loaded, and a handover no longer reloads it: the
// host's sidebar dials the successor from the origin it was loaded on, which
// names the earlier daemon's port. That is a cross-origin call, so the
// Connect routes answer the host's origin on ANY port, and no other origin:
// every workspace page dials the daemon that served it, on its own origin.

// webhostOrigin is the web view host's origin on any daemon's port
// (lisp/webhost.el `agent-repl-webhost-url`).
var webhostOrigin = regexp.MustCompile(`^http://host\.localhost:[0-9]+$`)

// connectExposedHeaders are the response headers a Connect client reads.
const connectExposedHeaders = "Grpc-Status, Grpc-Message, Grpc-Status-Details-Bin"

// withWebhostCORS lets the web view host call H across origins: a preflight
// from the host is answered here, and every answer to the host names it
// allowed. A request from any other origin passes through untouched.
func withWebhostCORS(h http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		origin := r.Header.Get("Origin")
		if !webhostOrigin.MatchString(origin) {
			h.ServeHTTP(w, r)
			return
		}
		header := w.Header()
		header.Set("Access-Control-Allow-Origin", origin)
		header.Add("Vary", "Origin")
		if r.Method == http.MethodOptions && r.Header.Get("Access-Control-Request-Method") != "" {
			header.Set("Access-Control-Allow-Methods", "POST, GET")
			header.Set("Access-Control-Allow-Headers", r.Header.Get("Access-Control-Request-Headers"))
			header.Set("Access-Control-Max-Age", "7200")
			w.WriteHeader(http.StatusNoContent)
			return
		}
		header.Set("Access-Control-Expose-Headers", connectExposedHeaders)
		h.ServeHTTP(w, r)
	})
}
