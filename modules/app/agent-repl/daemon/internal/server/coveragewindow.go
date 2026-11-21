package server

import (
	"fmt"
	"net/http"
	"runtime/coverage"

	"agent-repld/internal/dlog"
)

// THE COVERAGE WINDOW ROUTE is the test-only request a test harness sends a
// daemon it started to cut its coverage counters into per-test windows (the
// test runner's shadow-mode impact recording, testrun/internal/impact). It is
// served only to a daemon built with `-cover -covermode=atomic` and started
// with AGENT_REPL_TEST_COVERAGE_WINDOWS=1 (Deps.TestCoverageWindows); every
// other daemon has no such route.
//
// The operations, each `POST CoverageWindowRoute?op=<op>&dir=<dir>`:
//
//   - meta writes the binary's coverage meta-data into dir (once per process
//     is enough: every counters directory reads against it);
//   - counters writes the counters accumulated since the last clear (since
//     the process started, before the first) into dir;
//   - clear zeroes the counters, which opens the next window.
//
// The harness owns the sequencing: it holds one test at a time on the process
// (-test.parallel=1), so a window holds exactly one top-level test's work.
const CoverageWindowRoute = "/_test/coverage-window"

// opCoverageWindow is the route's log record.
const opCoverageWindow = "daemon.server.coverage_window"

// coverageAPI is the runtime's coverage functions (runtime/coverage), a seam
// so the route is exercised without a cover-built binary.
type coverageAPI struct {
	WriteMetaDir     func(dir string) error
	WriteCountersDir func(dir string) error
	ClearCounters    func() error
}

// runtimeCoverage is the real runtime's.
var runtimeCoverage = coverageAPI{
	WriteMetaDir:     coverage.WriteMetaDir,
	WriteCountersDir: coverage.WriteCountersDir,
	ClearCounters:    coverage.ClearCounters,
}

// coverageWindowHandler serves CoverageWindowRoute.
type coverageWindowHandler struct {
	api coverageAPI
	log dlog.Logger
}

func newCoverageWindowHandler(api coverageAPI, log dlog.Logger) http.Handler {
	return coverageWindowHandler{api: api, log: log}
}

func (h coverageWindowHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	fail := func(status int, message string, err error) {
		cause := ""
		if err != nil {
			cause = err.Error()
		}
		h.log.Error(opCoverageWindow, message, dlog.Context{
			"method": r.Method, "op": r.URL.Query().Get("op"), "dir": r.URL.Query().Get("dir"), "cause": cause,
		})
		http.Error(w, fmt.Sprintf("%s: %s", message, cause), status)
	}
	if r.Method != http.MethodPost {
		fail(http.StatusMethodNotAllowed, "refused a coverage window request that is not a POST", nil)
		return
	}
	op, dir := r.URL.Query().Get("op"), r.URL.Query().Get("dir")
	var err error
	switch op {
	case "meta", "counters":
		if dir == "" {
			fail(http.StatusBadRequest, "refused a coverage window request with no dir", nil)
			return
		}
		if op == "meta" {
			err = h.api.WriteMetaDir(dir)
		} else {
			err = h.api.WriteCountersDir(dir)
		}
	case "clear":
		err = h.api.ClearCounters()
	default:
		fail(http.StatusBadRequest, "refused a coverage window request with an unknown op", nil)
		return
	}
	if err != nil {
		fail(http.StatusInternalServerError, "the coverage window operation failed", err)
		return
	}
	h.log.Debug(opCoverageWindow, "served a coverage window operation", dlog.Context{"op": op, "dir": dir})
	w.WriteHeader(http.StatusNoContent)
}
