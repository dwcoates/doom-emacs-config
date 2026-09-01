package server

import (
	"net/http"
	"os"
	"path/filepath"
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
	if got := string(body[:n]); got != "<html>second</html>" {
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
