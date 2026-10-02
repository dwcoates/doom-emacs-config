package newsdigest

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/envc"
)

// serve answers every request with handler, for the test's lifetime.
func serve(t *testing.T, handler http.HandlerFunc) *httptest.Server {
	t.Helper()
	srv := httptest.NewServer(handler)
	t.Cleanup(srv.Close)
	return srv
}

func TestFetchAnswersTheBody(t *testing.T) {
	// Arrange
	var agent string
	srv := serve(t, func(w http.ResponseWriter, r *http.Request) {
		agent = r.Header.Get("User-Agent")
		_, _ = w.Write([]byte("feed"))
	})

	// Act
	body, err := HTTPFetcher{}.Fetch(context.Background(), srv.URL)

	// Assert
	if err != nil || string(body) != "feed" {
		t.Fatalf("Fetch = (%q, %v), want the body", body, err)
	}
	if agent != userAgent {
		t.Fatalf("user agent = %q, want %q", agent, userAgent)
	}
}

func TestFetchRefusesAStatusOtherThanOK(t *testing.T) {
	// Arrange
	srv := serve(t, func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusNotFound) })

	// Act
	_, err := HTTPFetcher{}.Fetch(context.Background(), srv.URL)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "404") {
		t.Fatalf("Fetch = %v, want a refusal naming 404", err)
	}
}

func TestFetchRefusesABodyOverTheCap(t *testing.T) {
	// Arrange
	srv := serve(t, func(w http.ResponseWriter, _ *http.Request) { _, _ = w.Write([]byte("12345")) })

	// Act
	_, err := HTTPFetcher{MaxBody: 4}.Fetch(context.Background(), srv.URL)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "exceeds") {
		t.Fatalf("Fetch = %v, want a refusal of the oversized body", err)
	}
}

func TestFetchEndsAtItsTimeout(t *testing.T) {
	// Arrange: the server answers only once the request is abandoned.
	srv := serve(t, func(_ http.ResponseWriter, r *http.Request) { <-r.Context().Done() })

	// Act
	_, err := HTTPFetcher{Timeout: 50 * time.Millisecond}.Fetch(context.Background(), srv.URL)

	// Assert
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Fetch = %v, want the deadline", err)
	}
}

func TestFetchRefusesAnUnbuildableRequest(t *testing.T) {
	// Act
	_, err := HTTPFetcher{}.Fetch(context.Background(), "://nothing")

	// Assert
	if err == nil {
		t.Fatal("Fetch = nil, want a refusal")
	}
}

func TestAGuardedFetchIsRefusedUnderTheVendorGuard(t *testing.T) {
	// Arrange
	inner := newFakeFetcher()
	inner.bodies["https://x.test"] = "body"
	guard := forbiddingGuard{}

	// Act
	_, err := GuardedFetcher{Guard: guard, Inner: inner}.Fetch(context.Background(), "https://x.test")

	// Assert
	var forbidden *envc.ForbiddenError
	if !errors.As(err, &forbidden) || forbidden.Site != GuardSite {
		t.Fatalf("Fetch = %v, want the guard's refusal naming %s", err, GuardSite)
	}
}

func TestAGuardedFetchPassesWhenTheGuardAllows(t *testing.T) {
	// Arrange
	inner := newFakeFetcher()
	inner.bodies["https://x.test"] = "body"

	// Act
	body, err := GuardedFetcher{Guard: envc.VendorGuard{}, Inner: inner}.Fetch(context.Background(), "https://x.test")

	// Assert
	if err != nil || string(body) != "body" {
		t.Fatalf("Fetch = (%q, %v), want the inner fetch's body", body, err)
	}
}

// forbiddingGuard is the vendor guard under AGENT_REPL_FORBID_VENDOR_CALLS.
type forbiddingGuard struct{}

func (forbiddingGuard) Check(site string) error { return &envc.ForbiddenError{Site: site} }
