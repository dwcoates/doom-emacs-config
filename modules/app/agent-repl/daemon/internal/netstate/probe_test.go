package netstate

import (
	"context"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/envc"
)

func TestHTTPProber(t *testing.T) {
	answering := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusNotFound) }))
	t.Cleanup(answering.Close)
	gone := httptest.NewServer(http.HandlerFunc(func(http.ResponseWriter, *http.Request) {}))
	gone.Close()
	tests := []struct {
		name          string
		forbid        string
		base          string
		wantErr       bool
		wantReachable bool
		wantDetail    string
		wantError     bool
	}{
		{name: "any HTTP answer is reachable", base: answering.URL, wantReachable: true, wantDetail: "answered HEAD with 404"},
		{name: "no answer is unreachable", base: gone.URL, wantDetail: "connect"},
		{name: "a base URL that does not parse is unreachable and recorded", base: "::not a url", wantDetail: "is not a URL", wantError: true},
		{name: "a run that forbids vendor calls cannot probe", forbid: "1", base: answering.URL, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			log := dlog.NewTestLogger()
			p := NewHTTPProber(envc.NewVendorGuard(envc.Load()), func(string) string { return tc.base }, log)
			// Act
			got, err := p.Probe(context.Background())
			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("Probe error = %v, want error %v", err, tc.wantErr)
			}
			if got.Reachable != tc.wantReachable || !strings.Contains(got.Detail, tc.wantDetail) {
				t.Fatalf("Probe = %+v, want reachable %v with %q", got, tc.wantReachable, tc.wantDetail)
			}
			if _, recorded := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: op}); recorded != tc.wantError {
				t.Fatalf("error record = %v, want %v: %v", recorded, tc.wantError, log.Records())
			}
		})
	}
}

func TestHTTPProberDefaultsToThePublicAPI(t *testing.T) {
	// Act
	p := NewHTTPProber(envc.VendorGuard{}, func(string) string { return "" }, dlog.NewTestLogger())
	// Assert
	if p.target == nil || p.target.String() != DefaultAPIBaseURL {
		t.Fatalf("target = %v, want %s", p.target, DefaultAPIBaseURL)
	}
}
