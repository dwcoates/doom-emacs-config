package server

import (
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"slices"
	"strings"
	"testing"
)

func postOpenExternal(t *testing.T, h *harness, body string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/open-external", "application/json",
		strings.NewReader(body))
	if err != nil {
		t.Fatalf("POST /open-external: %v", err)
	}
	t.Cleanup(func() { resp.Body.Close() })
	return resp
}

func TestOpenExternalHandsTheURLToTheBrowser(t *testing.T) {
	// Arrange.
	var got string
	h := newHarnessWith(t, Config{OpenExternalURL: func(url string) error {
		got = url
		return nil
	}})

	// Act.
	resp := postOpenExternal(t, h, `{"url":"https://example.com/x"}`)

	// Assert.
	if resp.StatusCode != http.StatusOK {
		t.Fatalf("status = %d, want 200 (body %s)", resp.StatusCode, responseBody(t, resp))
	}
	if got != "https://example.com/x" {
		t.Errorf("opener got %q, want %q", got, "https://example.com/x")
	}
}

func TestNewDefaultsTheExternalOpener(t *testing.T) {
	// Arrange/Act.
	h := newHarness(t)

	// Assert.
	if h.srv.openExternalURL == nil {
		t.Fatal("New left openExternalURL nil — POST /open-external would panic in production")
	}
}

func TestOpenExternalRefusesANonHTTPURL(t *testing.T) {
	// Arrange.
	opened := false
	h := newHarnessWith(t, Config{OpenExternalURL: func(string) error {
		opened = true
		return nil
	}})

	// Act.
	resp := postOpenExternal(t, h, `{"url":"file:///etc/passwd"}`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
	if opened {
		t.Error("a refused url still reached the opener")
	}
}

func TestOpenExternalRefusesAMissingURL(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, Config{OpenExternalURL: func(string) error { return nil }})

	// Act.
	resp := postOpenExternal(t, h, `{}`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestOpenExternalRefusesAMalformedBody(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, Config{OpenExternalURL: func(string) error { return nil }})

	// Act.
	resp := postOpenExternal(t, h, `not json`)

	// Assert.
	if resp.StatusCode != http.StatusBadRequest {
		t.Fatalf("status = %d, want 400", resp.StatusCode)
	}
}

func TestOpenExternalSurfacesAnOpenerFailure(t *testing.T) {
	// Arrange.
	h := newHarnessWith(t, Config{OpenExternalURL: func(string) error {
		return errors.New("chrome exploded")
	}})

	// Act.
	resp := postOpenExternal(t, h, `{"url":"https://example.com/x"}`)

	// Assert.
	if resp.StatusCode != http.StatusInternalServerError {
		t.Fatalf("status = %d, want 500", resp.StatusCode)
	}
	if body := responseBody(t, resp); !strings.Contains(body, "chrome exploded") {
		t.Errorf("body = %q, want it to carry the opener's cause", body)
	}
}

// loggingOpenExternalServer builds the smallest Server that can answer POST
// /open-external, capturing every canonical log line. newHarnessWith overrides
// Config.Logf with its own silent sink, so the log assertions below construct
// the Server directly rather than through the harness.
func loggingOpenExternalServer(t *testing.T, open func(string) error) (*Server, *[]string) {
	t.Helper()
	var lines []string
	srv := &Server{
		logf: func(format string, args ...any) {
			lines = append(lines, fmt.Sprintf(format, args...))
		},
		openExternalURL: open,
	}
	return srv, &lines
}

func recordOpenExternal(srv *Server, body string) *httptest.ResponseRecorder {
	rec := httptest.NewRecorder()
	req := httptest.NewRequest(http.MethodPost, "/open-external", strings.NewReader(body))
	srv.handleOpenExternal(rec, req)
	return rec
}

func TestOpenExternalLogsTheProfileOnFailure(t *testing.T) {
	// Arrange.
	srv, lines := loggingOpenExternalServer(t, func(string) error {
		return errors.New("boom")
	})

	// Act.
	recordOpenExternal(srv, `{"url":"https://example.com/x"}`)

	// Assert.
	want := `open-external: opening url="https://example.com/x" in profile "Profile 6" failed: boom`
	if !slices.Contains(*lines, want) {
		t.Errorf("logged %q, want it to contain %q", *lines, want)
	}
}

func TestOpenExternalLogsARefusal(t *testing.T) {
	// Arrange.
	srv, lines := loggingOpenExternalServer(t, func(string) error { return nil })

	// Act.
	recordOpenExternal(srv, `{"url":"file:///etc/passwd"}`)

	// Assert.
	found := false
	for _, l := range *lines {
		if strings.HasPrefix(l, `open-external: refused url="file:///etc/passwd"`) {
			found = true
		}
	}
	if !found {
		t.Errorf("logged %q, want an open-external refusal record", *lines)
	}
}

func TestOpenExternalLogsTheSuccess(t *testing.T) {
	// Arrange.
	srv, lines := loggingOpenExternalServer(t, func(string) error { return nil })

	// Act.
	recordOpenExternal(srv, `{"url":"https://example.com/x"}`)

	// Assert.
	want := `open-external: opened url="https://example.com/x" in profile "Profile 6"`
	if !slices.Contains(*lines, want) {
		t.Errorf("logged %q, want it to contain %q", *lines, want)
	}
}
