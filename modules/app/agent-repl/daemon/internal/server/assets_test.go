package server

import (
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestEntryPointIsNoStore pins that the ENTRY POINT is answered no-store, which
// is what makes a rebuilt webapp arrive on a reload.
func TestEntryPointIsNoStore(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + "/")
	if err != nil {
		t.Fatalf("get the entry point: %v", err)
	}
	defer resp.Body.Close()

	// Assert.
	if got := resp.Header.Get("Cache-Control"); got != "no-store" {
		t.Fatalf("Cache-Control = %q, want %q", got, "no-store")
	}
}

// TestAssetIsNotNoStore pins that NOTHING BUT the entry point gets no-store:
// the hashed bundles are immutable by name and must stay cacheable.
func TestAssetIsNotNoStore(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if err := os.WriteFile(filepath.Join(h.WebappDist, "app.js"), []byte("//"), 0o644); err != nil {
		t.Fatalf("write the asset: %v", err)
	}

	// Act.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + "/app.js")
	if err != nil {
		t.Fatalf("get the asset: %v", err)
	}
	defer resp.Body.Close()

	// Assert.
	if got := resp.Header.Get("Cache-Control"); got == "no-store" {
		t.Fatal("a hashed asset was answered no-store; only the entry point may be")
	}
}

// TestEntryPointIsReStattedPerRequest pins that a REWRITTEN index.html is
// served without restarting the daemon.
func TestEntryPointIsReStattedPerRequest(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	first, err := h.HTTP.Client().Get(h.HTTP.URL + "/")
	if err != nil {
		t.Fatalf("get the entry point: %v", err)
	}
	first.Body.Close()
	if err := os.WriteFile(filepath.Join(h.WebappDist, entryPoint), []byte("<html>second</html>"), 0o644); err != nil {
		t.Fatalf("rewrite the entry point: %v", err)
	}

	// Act.
	second, err := h.HTTP.Client().Get(h.HTTP.URL + "/")
	if err != nil {
		t.Fatalf("re-get the entry point: %v", err)
	}
	defer second.Body.Close()
	body := make([]byte, 64)
	n, _ := second.Body.Read(body)

	// Assert.
	if got := string(body[:n]); got != `<html style="--feed-text-scale: 1">second</html>` {
		t.Fatalf("entry point body = %q, want the rewritten document", got)
	}
}

// TestUnknownAssetIsNotFound pins that a missing asset is a 404 rather than a
// silent fallback to the entry point, which would hide a broken build.
func TestUnknownAssetIsNotFound(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + "/missing.js")
	if err != nil {
		t.Fatalf("get the asset: %v", err)
	}
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode != http.StatusNotFound {
		t.Fatalf("status = %d, want %d", resp.StatusCode, http.StatusNotFound)
	}
}

// TestAssetPathEscapeIsRefused pins that a traversal path never reaches outside
// the dist directory.
func TestAssetPathEscapeIsRefused(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	req, err := http.NewRequest(http.MethodGet, h.HTTP.URL+"/x", nil)
	if err != nil {
		t.Fatalf("build the request: %v", err)
	}
	req.URL.Opaque = "//" + req.URL.Host + "/../../etc/passwd"
	resp, err := h.HTTP.Client().Do(req)
	if err != nil {
		t.Fatalf("get the asset: %v", err)
	}
	defer resp.Body.Close()

	// Assert.
	if resp.StatusCode == http.StatusOK {
		t.Fatal("a traversal path was served; it must be refused")
	}
}

// TestChunkIsServedUnderEveryPageHost pins that a code-split chunk the entry
// document names by an origin-absolute `/assets/<name>` url resolves whatever
// Host the page was loaded under: the webview loads the page from its
// workspace's own `ws-<id>.localhost:<port>`, and the asset origin serves
// dist/ by path alone.
func TestChunkIsServedUnderEveryPageHost(t *testing.T) {
	const chunk = "assets/proto-Ab12_-.js"
	const body = "export const x = 1;"
	cases := []struct {
		name string
		host func(port string) string
	}{
		{name: "a per-workspace host", host: func(port string) string { return "ws-abc123.localhost:" + port }},
		{name: "the loopback address", host: func(port string) string { return "127.0.0.1:" + port }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			if err := os.MkdirAll(filepath.Join(h.WebappDist, "assets"), 0o755); err != nil {
				t.Fatalf("make the assets directory: %v", err)
			}
			if err := os.WriteFile(filepath.Join(h.WebappDist, filepath.FromSlash(chunk)), []byte(body), 0o644); err != nil {
				t.Fatalf("write the chunk: %v", err)
			}
			req, err := http.NewRequest(http.MethodGet, h.HTTP.URL+"/"+chunk, nil)
			if err != nil {
				t.Fatalf("build the request: %v", err)
			}
			req.Host = tc.host(req.URL.Port())

			// Act.
			resp, err := h.HTTP.Client().Do(req)
			if err != nil {
				t.Fatalf("get the chunk: %v", err)
			}
			defer resp.Body.Close()
			got, err := io.ReadAll(resp.Body)
			if err != nil {
				t.Fatalf("read the chunk: %v", err)
			}

			// Assert.
			if resp.StatusCode != http.StatusOK || string(got) != body {
				t.Fatalf("status = %d body = %q, want %d %q", resp.StatusCode, got, http.StatusOK, body)
			}
		})
	}
}

// TestStampFeedTextScale pins the stamp itself: the zoom lands as an inline
// `--feed-text-scale` on the one <html> opening tag, and a document without
// exactly one is refused rather than served unscaled.
func TestStampFeedTextScale(t *testing.T) {
	tests := []struct {
		name    string
		body    string
		scale   float64
		want    string
		wantErr string
	}{
		{
			name:  "a bare html tag carries the scale",
			body:  "<!doctype html><html><body></body></html>",
			scale: 1.12,
			want:  `<!doctype html><html style="--feed-text-scale: 1.12"><body></body></html>`,
		},
		{
			name:  "an html tag with attributes keeps them after the scale",
			body:  `<html lang="en"><head></head></html>`,
			scale: 0.5,
			want:  `<html style="--feed-text-scale: 0.5" lang="en"><head></head></html>`,
		},
		{
			name:  "the default scale is written whole",
			body:  "<html></html>",
			scale: 1,
			want:  `<html style="--feed-text-scale: 1"></html>`,
		},
		{
			name:    "a document with no html tag is refused",
			body:    "<!doctype html><title>t</title>",
			scale:   1,
			wantErr: "carries 0",
		},
		{
			name:    "a document with two html tags is refused",
			body:    "<html></html><html></html>",
			scale:   1,
			wantErr: "carries 2",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := stampFeedTextScale([]byte(tc.body), tc.scale)

			// Assert.
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want one containing %q", err, tc.wantErr)
				}
				if got != nil {
					t.Fatalf("a refused stamp answered a body %q", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("stamp: %v", err)
			}
			if string(got) != tc.want {
				t.Fatalf("stamped body = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestEntryPointCarriesTheSeededFeedTextScale pins the reason for the stamp:
// a page loaded after the persisted zoom was seeded paints at that zoom from
// its first frame, before any feed watch has pushed it.
func TestEntryPointCarriesTheSeededFeedTextScale(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Server.SeedFeedTextScale(1.34)

	// Act.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + "/")
	if err != nil {
		t.Fatalf("get the entry point: %v", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read the entry point: %v", err)
	}

	// Assert.
	if got, want := string(body), `<html style="--feed-text-scale: 1.34">first</html>`; got != want {
		t.Fatalf("entry point body = %q, want %q", got, want)
	}
}

// TestEntryPointWithoutAnHTMLTagIsRefused pins the broken-build path: an entry
// document the zoom cannot be stamped into is answered 500 and logged once
// with the path, the scale and the cause, never served unscaled.
func TestEntryPointWithoutAnHTMLTagIsRefused(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	if err := os.WriteFile(filepath.Join(h.WebappDist, entryPoint), []byte("<title>no root</title>"), 0o644); err != nil {
		t.Fatalf("rewrite the entry point: %v", err)
	}

	// Act.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + "/")
	if err != nil {
		t.Fatalf("get the entry point: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)

	// Assert.
	if resp.StatusCode != http.StatusInternalServerError {
		t.Fatalf("status = %d, want %d", resp.StatusCode, http.StatusInternalServerError)
	}
	if strings.Contains(string(body), "no root") {
		t.Fatalf("the unstampable document was served: %q", body)
	}
	errs := log.at("ERROR")
	if len(errs) != 1 {
		t.Fatalf("ERROR records = %+v, want exactly one", errs)
	}
	rec := errs[0]
	if rec.Operation != opAssets {
		t.Fatalf("operation = %q, want %q", rec.Operation, opAssets)
	}
	if rec.Context["path"] != filepath.Join(h.WebappDist, entryPoint) || rec.Context["scale"] != 1.0 {
		t.Fatalf("context = %+v, want the entry path and scale 1", rec.Context)
	}
	if cause, _ := rec.Context["cause"].(string); !strings.Contains(cause, "carries 0") {
		t.Fatalf("cause = %v, want the tag count", rec.Context["cause"])
	}
}
