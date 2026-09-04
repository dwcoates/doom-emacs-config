//go:build perf

// perf_webapp_test.go — the WEBAPP-layer half of PERF-SPEC.md's first eight.
//
// ONE WORLD, ONE VITEST CHILD, FIVE ASSERTIONS. The child
// (`webapp/test/webapp-layer/perf.layer.test.ts`) measures every interval
// page-side on the real clock it captured before the harness faked timers
// (§A4), computes its percentiles with the identical nearest-rank rule (§D1),
// and ships them here as ONE `ClientLog` record. This file is where those
// numbers meet their budget, their baseline and the phase summary.
//
// WHY THE NUMBERS TRAVEL AND THE ASSERTIONS DO NOT: §A4.1 is decisive. The
// page's clock reads ~1970 under `vi.useFakeTimers`, so a webapp-layer
// timestamp can never be subtracted from a daemon one, and a Go-side
// measurement of a page-side hop is not available at any price. What CAN cross
// the boundary is a finished duration, and one aggregated record is the only
// shape that survives `clientlog-throttle.ts` (§A5).
//
// THE AREA HAS ITS OWN WORLD AND ITS OWN SLOT, exactly as every other webapp
// layer area does, and it runs in the SERIAL perf phase (§D4) rather than
// beside the functional suite.
package e2e

import (
	"context"
	"math"
	"testing"
	"time"

	"claude-repld/integration/harness"
)

// WebappLayerPerfTimeout bounds the perf area's vitest child.
//
// ITS OWN CONSTANT, because what it bounds is not an area: the perf child
// drives 20 real turns for rows 1b/3a, 20 more parked-and-interrupted turns
// for row 5, 20 asking turns for row 8a and 20 selection round trips for row
// 11c — 80 real chain traversals against one daemon, where the heaviest
// functional area drives 22. WebappLayerTimeout is sized at ~3x the slowest
// FUNCTIONAL child and would bound the wrong thing here.
//
// It bounds a HANG, not a synchronization wait: nothing in the child sleeps,
// each sample waits on the frame it times, and the child's own per-sample
// budget (SAMPLE_BUDGET_MS in perf.layer.test.ts) fails a stuck sample long
// before this fires.
//
// MEASURED, then set at ~3x the observed max — see PERF-SPEC.md §D, "the
// measured perf phase".
const WebappLayerPerfTimeout = 240 * time.Second

// wlPerfSamplesOperation is the operation the perf child logs its finished
// percentiles under, through the daemon's own ClientLog rpc.
//
// Matched VERBATIM against PERF_SAMPLES_OPERATION in
// `webapp/test/webapp-layer/perf.ts`; the two constants are documented on each
// other and move together.
const wlPerfSamplesOperation = "webapp-layer.perf.samples"

// The second workspace row 11c alternates the selection between. Read by
// `perf.layer.test.ts`; the names are documented on each other.
const (
	wlPerfWorkspaceIDBEnv  = "AGENT_REPL_E2E_WORKSPACE_ID_B"
	wlPerfWorkspaceDirBEnv = "AGENT_REPL_E2E_WORKSPACE_DIR_B"
)

// TestPerfWebappLayer drives the perf area and asserts its shipped numbers.
func TestPerfWebappLayer(t *testing.T) {
	perfRequire(t)
	wlHoldAreaSlot(t)
	npm := wlRequireNPM(t)
	webappDir := wlRequireWebappDeps(t)

	// Arrange: the world, the page's workspace, and the SECOND workspace whose
	// sidebar row row 11c moves the selection onto and off.
	w := NewWorld(t, WorldOpts{})
	repoA := harness.NewRepo(t)
	repoB := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repoA.Dir)
	wsB := harness.Register(t, w.Daemon, repoB.Dir)
	perfCalibrate(t, w)

	// THE HOST PARTICIPANT, WHICH EMACS WOULD BE — the same hold wlDriveArea
	// takes, and for the same reason: without it the footer resolves
	// `disconnected`, the real page closes its composer gate, and every
	// submission after the first is swallowed by the app itself. Held on THIS
	// area's own bound, never the harness's.
	holdCtx, releaseHold := context.WithTimeout(context.Background(), WebappLayerPerfTimeout)
	defer releaseHold()
	host := w.Daemon.WatchHostFor(holdCtx, ws)
	host.Drain()
	defer host.Close()

	env := append(wlChildEnv(t, w, ws),
		wlPerfWorkspaceIDBEnv+"="+wsB.GetId(),
		wlPerfWorkspaceDirBEnv+"="+wsB.GetDir(),
	)

	// Act: the child measures, then ships one record before it stops its page.
	child, err := wlStartVitest(t, npm, webappDir, "perf.layer.test.ts", env)
	if err != nil {
		t.Fatalf("e2e/webapp-layer: starting the perf child: %v", err)
	}
	defer child.Kill()
	if waitErr := child.WaitFor(WebappLayerPerfTimeout); waitErr != nil {
		t.Fatalf("e2e/webapp-layer: perf.layer.test.ts failed: %v", waitErr)
	}

	// Assert: the shipped percentiles, read off the workspace's own webapp
	// sink — the sink ClientLog persists a webview's records to.
	shipped := w.Daemon.AwaitLogRecord(harness.ClientLogPath(ws),
		"the perf child's shipped percentiles",
		func(r harness.LogRecord) bool { return r.Operation == wlPerfSamplesOperation })

	for _, tc := range []struct {
		name      string
		p50Budget time.Duration
		p95Budget time.Duration
	}{
		{"perf-prompt-bubble", perfBudgetPromptBubbleP50, perfBudgetPromptBubbleP95},
		{"perf-response-bubble", perfBudgetResponseBubbleP50, perfBudgetResponseBubbleP95},
		{"perf-interrupt-footer", perfBudgetInterruptFooterP50, perfBudgetInterruptFooterP95},
		{"perf-question-card", perfBudgetQuestionCardP50, perfBudgetQuestionCardP95},
		{"perf-sidebar-selected", perfBudgetSidebarSelectedP50, perfBudgetSidebarSelectedP95},
	} {
		// ONE SUBTEST EACH, so a DECLINED calibration's skip scopes to one
		// assertion instead of abandoning the four after it.
		t.Run(tc.name, func(t *testing.T) {
			wlShipped(t, shipped, tc.name).Assert(t, tc.p50Budget, tc.p95Budget)
		})
	}

	w.RequireNoUnexpectedExit(t)
}

// wlShipped reads one assertion's percentiles out of the shipped record.
//
// A MISSING KEY IS A FAILURE, never a zero: a zero would sail under every
// budget and report a green assertion that measured nothing, which is the one
// outcome this whole phase exists to make impossible.
func wlShipped(t *testing.T, r harness.LogRecord, name string) PerfShipped {
	t.Helper()
	return PerfShipped{
		Name: name,
		N:    int(wlShippedNumber(t, r, name+".n")),
		Min:  wlShippedDuration(t, r, name+".min_ms"),
		P50:  wlShippedDuration(t, r, name+".p50_ms"),
		P95:  wlShippedDuration(t, r, name+".p95_ms"),
		Max:  wlShippedDuration(t, r, name+".max_ms"),
	}
}

func wlShippedNumber(t *testing.T, r harness.LogRecord, key string) float64 {
	t.Helper()
	raw, ok := r.Context[key]
	if !ok {
		t.Fatalf("the perf child's record carries no %q; context keys: %v", key, wlContextKeys(r))
	}
	// google.protobuf.Struct numbers decode as float64 through encoding/json.
	value, ok := raw.(float64)
	if !ok {
		t.Fatalf("the perf child's %q = %v (%T), want a number", key, raw, raw)
	}
	return value
}

func wlShippedDuration(t *testing.T, r harness.LogRecord, key string) time.Duration {
	t.Helper()
	ms := wlShippedNumber(t, r, key)
	if math.IsNaN(ms) || ms < 0 {
		t.Fatalf("the perf child's %q = %v, want a non-negative duration in milliseconds", key, ms)
	}
	return time.Duration(ms * float64(time.Millisecond))
}

func wlContextKeys(r harness.LogRecord) []string {
	keys := make([]string, 0, len(r.Context))
	for k := range r.Context {
		keys = append(keys, k)
	}
	return keys
}
