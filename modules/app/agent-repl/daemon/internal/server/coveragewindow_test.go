package server

import (
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func TestCoverageWindowHandler(t *testing.T) {
	boom := errors.New("not built with -cover")
	cases := []struct {
		name       string
		method     string
		query      string
		apiErr     error
		wantStatus int
		wantCalls  []string
		wantError  string
	}{
		{name: "meta writes the meta-data into the dir", method: http.MethodPost, query: "op=meta&dir=/m", wantStatus: http.StatusNoContent, wantCalls: []string{"meta /m"}},
		{name: "counters writes the counters into the dir", method: http.MethodPost, query: "op=counters&dir=/c", wantStatus: http.StatusNoContent, wantCalls: []string{"counters /c"}},
		{name: "clear zeroes the counters", method: http.MethodPost, query: "op=clear", wantStatus: http.StatusNoContent, wantCalls: []string{"clear"}},
		{name: "a GET is refused", method: http.MethodGet, query: "op=clear", wantStatus: http.StatusMethodNotAllowed, wantError: "not a POST"},
		{name: "an unknown op is refused", method: http.MethodPost, query: "op=peek", wantStatus: http.StatusBadRequest, wantError: "unknown op"},
		{name: "counters with no dir is refused", method: http.MethodPost, query: "op=counters", wantStatus: http.StatusBadRequest, wantError: "no dir"},
		{name: "a runtime that cannot write answers its error", method: http.MethodPost, query: "op=counters&dir=/c", apiErr: boom, wantStatus: http.StatusInternalServerError, wantCalls: []string{"counters /c"}, wantError: "operation failed"},
		{name: "a runtime that cannot clear answers its error", method: http.MethodPost, query: "op=clear", apiErr: boom, wantStatus: http.StatusInternalServerError, wantCalls: []string{"clear"}, wantError: "operation failed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var calls []string
			api := coverageAPI{
				WriteMetaDir:     func(dir string) error { calls = append(calls, "meta "+dir); return tc.apiErr },
				WriteCountersDir: func(dir string) error { calls = append(calls, "counters "+dir); return tc.apiErr },
				ClearCounters:    func() error { calls = append(calls, "clear"); return tc.apiErr },
			}
			log := dlog.NewTestLogger()
			rec := httptest.NewRecorder()
			// Act
			newCoverageWindowHandler(api, log).ServeHTTP(rec, httptest.NewRequest(tc.method, CoverageWindowRoute+"?"+tc.query, nil))
			// Assert
			if rec.Code != tc.wantStatus {
				t.Fatalf("status = %d (%s), want %d", rec.Code, strings.TrimSpace(rec.Body.String()), tc.wantStatus)
			}
			if strings.Join(calls, ",") != strings.Join(tc.wantCalls, ",") {
				t.Fatalf("runtime calls = %v, want %v", calls, tc.wantCalls)
			}
			errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error", Operation: opCoverageWindow})
			if tc.wantError == "" {
				if len(errs) != 0 {
					t.Fatalf("error records = %v, want none", errs)
				}
				return
			}
			if len(errs) != 1 || !strings.Contains(errs[0].Message, tc.wantError) {
				t.Fatalf("error records = %v, want one containing %q", errs, tc.wantError)
			}
			if tc.apiErr != nil && errs[0].Context["cause"] != boom.Error() {
				t.Fatalf("error record context = %v, want the runtime's cause", errs[0].Context)
			}
		})
	}
}
