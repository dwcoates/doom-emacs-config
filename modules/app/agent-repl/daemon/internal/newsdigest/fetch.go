package newsdigest

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"time"
)

// The fetch's bounds.
const (
	// DefaultFetchTimeout bounds ONE source's request, connect to last byte.
	// Every source is a small static document or feed served by a CDN, so a
	// source that has not answered in this long is down for this run.
	DefaultFetchTimeout = 20 * time.Second
	// DefaultMaxBody caps one source's body. The npm document, which carries
	// every version's manifest, is the largest source by far.
	DefaultMaxBody = 32 << 20
	// userAgent names the daemon to the sources it reads.
	userAgent = "agent-repl-news-digest/1"
)

// Fetcher reads one URL's body.
type Fetcher interface {
	Fetch(ctx context.Context, url string) ([]byte, error)
}

// HTTPFetcher is the production Fetcher: one GET per source, each under its
// own timeout.
type HTTPFetcher struct {
	// Client is the HTTP client; nil is a fresh one with no client-wide
	// timeout (each request carries its own).
	Client *http.Client
	// Timeout bounds one request; zero is DefaultFetchTimeout.
	Timeout time.Duration
	// MaxBody caps one body in bytes; zero is DefaultMaxBody.
	MaxBody int64
}

// Fetch GETs url and answers its body. A status other than 200, a body over
// the cap, or a request that outlives its timeout is an error naming it.
func (f HTTPFetcher) Fetch(ctx context.Context, url string) ([]byte, error) {
	timeout := f.Timeout
	if timeout == 0 {
		timeout = DefaultFetchTimeout
	}
	client := f.Client
	if client == nil {
		client = &http.Client{}
	}
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return nil, fmt.Errorf("building the request: %w", err)
	}
	req.Header.Set("User-Agent", userAgent)
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("the request failed: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("the source answered HTTP %d", resp.StatusCode)
	}
	limit := f.MaxBody
	if limit == 0 {
		limit = DefaultMaxBody
	}
	body, err := io.ReadAll(io.LimitReader(resp.Body, limit+1))
	if err != nil {
		return nil, fmt.Errorf("reading the body: %w", err)
	}
	if int64(len(body)) > limit {
		return nil, fmt.Errorf("the body exceeds %d bytes", limit)
	}
	return body, nil
}

// GuardSite is the vendor-guard site a guarded fetch asks under.
const GuardSite = "news_digest_fetch"

// Guard is the vendor guard's answer for one site (envc.VendorGuard).
type Guard interface {
	Check(site string) error
}

// GuardedFetcher refuses every fetch the vendor guard forbids. It wraps the
// fetcher of the REAL sources, so a daemon under AGENT_REPL_FORBID_VENDOR_CALLS
// never reaches Anthropic's servers; a test that names its own sources
// (EnvSources) fetches them unguarded, exactly as an explicitly named vendor
// binary is spawned unguarded.
type GuardedFetcher struct {
	Guard Guard
	Inner Fetcher
}

// Fetch refuses when the guard forbids the site, and fetches otherwise.
func (f GuardedFetcher) Fetch(ctx context.Context, url string) ([]byte, error) {
	if err := f.Guard.Check(GuardSite); err != nil {
		return nil, err
	}
	return f.Inner.Fetch(ctx, url)
}
