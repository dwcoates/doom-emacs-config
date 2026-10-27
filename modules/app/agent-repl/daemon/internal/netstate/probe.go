package netstate

import (
	"context"
	"fmt"
	"net/http"
	"net/url"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/envc"
)

// ProbeSite is the vendor-guard site the reachability check asks under: it
// touches the vendor's API host, so a run that forbids vendor calls forbids it.
const ProbeSite = "network_reachability"

// ProbeTimeout bounds one check: the shim's own (PROBE_TIMEOUT_MS), under the
// beat so a check settles before the next one would start.
const ProbeTimeout = 4 * time.Second

// DefaultAPIBaseURL is the public API, when nothing configures another.
const DefaultAPIBaseURL = "https://api.anthropic.com"

// HTTPProber is the daemon's reachability check: one HEAD of the API base URL
// the vendor is configured for, through the environment's proxy. Any HTTP
// answer is "reachable"; only getting none is "unreachable".
type HTTPProber struct {
	guard  envc.VendorGuard
	target *url.URL
	// unresolvable is why the configured base URL is no URL, answered as
	// "unreachable" by every check rather than replaced by the default.
	unresolvable string
	client *http.Client
}

// NewHTTPProber resolves the API base URL from GETENV (ANTHROPIC_BASE_URL,
// else DefaultAPIBaseURL). A base URL that does not parse is recorded once at
// ERROR.
func NewHTTPProber(guard envc.VendorGuard, getenv func(string) string, log dlog.Logger) *HTTPProber {
	base := getenv("ANTHROPIC_BASE_URL")
	if base == "" {
		base = DefaultAPIBaseURL
	}
	p := &HTTPProber{guard: guard, client: &http.Client{
		Timeout:   ProbeTimeout,
		Transport: &http.Transport{Proxy: http.ProxyFromEnvironment},
	}}
	target, err := url.Parse(base)
	if err != nil || target.Scheme == "" || target.Host == "" {
		p.unresolvable = fmt.Sprintf("ANTHROPIC_BASE_URL %q is not a URL", base)
		log.Error(op, "the API base URL does not parse; every reachability check answers unreachable", dlog.Context{"base_url": base})
		return p
	}
	p.target = target
	return p
}

// Probe asks once.
func (p *HTTPProber) Probe(ctx context.Context) (ProbeAnswer, error) {
	if err := p.guard.Check(ProbeSite); err != nil {
		return ProbeAnswer{}, err
	}
	if p.target == nil {
		return ProbeAnswer{Detail: p.unresolvable}, nil
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodHead, p.target.String(), nil)
	if err != nil {
		return ProbeAnswer{}, fmt.Errorf("netstate: build the reachability request: %w", err)
	}
	resp, err := p.client.Do(req)
	if err != nil {
		return ProbeAnswer{Detail: fmt.Sprintf("%s: %v", p.target.Host, err)}, nil
	}
	resp.Body.Close()
	return ProbeAnswer{Reachable: true, Detail: fmt.Sprintf("%s answered HEAD with %d", p.target.Host, resp.StatusCode)}, nil
}
