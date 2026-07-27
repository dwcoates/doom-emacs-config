package handler

// The compaction boundary HOLD-BACK, driven end to end over a real file and a
// real cursor: a boundary that is the last line the scan can see is deferred
// rather than emitted, and the cursor is parked before it so a restart re-reads
// it. clearcompact_test.go covers the decision at the batch level; these tests
// exist for the parts only a file and a committed cursor can express — what the
// NEXT scan does with the held line, and what survives losing the process.

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// --- rig --------------------------------------------------------------------

// hbRig is one live transcript, its tailer, and the loud log both write to.
// It is the ccint rig plus the two things these tests assert on: the committed
// cursor each scan produced, and the log lines it produced getting there.
type hbRig struct {
	t    *testing.T
	tr   *tail.Tailer
	ctx  *tail.Context
	path string
	uuid string
	logs []string
	// last is the most recent poll result, whose Next IS the cursor the rig
	// committed (scan commits every poll, as the sidecar does on a store ack).
	last tail.PollResult
}

func hbNewRig(t *testing.T, vendorUUID string) *hbRig {
	t.Helper()
	root := filepath.Join(t.TempDir(), "config")
	dir := filepath.Join(root, "projects", "-Users-someone-workspace")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating the project dir: %v", err)
	}
	path := filepath.Join(dir, vendorUUID+".jsonl")
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatalf("creating the transcript: %v", err)
	}
	r := &hbRig{t: t, path: path, uuid: vendorUUID}
	tgt, ok := discover.New([]string{root}, filepath.Join(root, "spool"), r.log).Classify(path)
	if !ok {
		t.Fatalf("discover did not classify %s as a tailable target", path)
	}
	r.ctx = &tail.Context{SessionID: tgt.SessionID, Path: tgt.Path, Kind: tgt.Kind}
	r.tr = tail.New(path, tgt.Codec(), NewSessionTranscriptHandler(r.log), r.ctx, r.log)
	return r
}

// log is the rig's loud-log sink, shared by the tailer and the handler.
func (r *hbRig) log(format string, args ...any) {
	r.logs = append(r.logs, fmt.Sprintf(format, args...))
}

// write appends JSONL lines in file order and returns the file's size after the
// append — which is the byte offset the NEXT line will start at.
func (r *hbRig) write(lines ...string) int64 {
	r.t.Helper()
	f, err := os.OpenFile(r.path, os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		r.t.Fatalf("opening %s for append: %v", r.path, err)
	}
	defer f.Close()
	for _, ln := range lines {
		if _, err := f.WriteString(ln + "\n"); err != nil {
			r.t.Fatalf("appending to %s: %v", r.path, err)
		}
	}
	fi, err := os.Stat(r.path)
	if err != nil {
		r.t.Fatalf("stat %s: %v", r.path, err)
	}
	return fi.Size()
}

// scan runs one poll and commits it, which is the unit the sidecar's poll loop
// works in.
func (r *hbRig) scan() []*corev1.Event {
	r.t.Helper()
	res, err := r.tr.Poll()
	if err != nil {
		r.t.Fatalf("polling %s: %v", r.path, err)
	}
	r.tr.Commit(res)
	r.last = res
	return res.Events
}

// restart replaces the tailer, its handler and its context with fresh ones
// seeded from the committed cursor — the state a sidecar comes back with after
// dying mid-hold, when NOTHING in memory survives.
func (r *hbRig) restart() {
	r.t.Helper()
	r.ctx = &tail.Context{SessionID: r.ctx.SessionID, Path: r.ctx.Path, Kind: r.ctx.Kind}
	tr := tail.New(r.path, tail.JSONLCodec{}, NewSessionTranscriptHandler(r.log), r.ctx, r.log)
	tr.Restore(r.last.Next)
	r.tr = tr
}

// vendorTwins lists the vendor records in a batch — one per transcript LINE
// converted, which is how a redelivered line proves it was converted at all.
func vendorTwins(evs []*corev1.Event) []*corev1.Event {
	var out []*corev1.Event
	for _, e := range evs {
		if e.GetVendor() != nil {
			out = append(out, e)
		}
	}
	return out
}

func (r *hbRig) logged(substr string) bool {
	return strings.Contains(strings.Join(r.logs, "\n"), substr)
}

// missingSummaryLog is the loud line a bare emit must always carry.
const missingSummaryLog = "not followed by a compaction summary"

// --- a held boundary settled by a NON-summary line ---------------------------

func TestHeldBoundarySettledByANonSummaryLineEmitsBare(t *testing.T) {
	tests := []struct {
		name string
		next func(r *hbRig) string
	}{
		{
			name: "an ordinary prompt",
			next: func(r *hbRig) string { return ccintUserLine("u-next", r.uuid, "carry on") },
		},
		{
			name: "an assistant reply",
			next: func(r *hbRig) string { return ccintAssistantLine("a-next", r.uuid, "carrying on") },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the boundary is the last line the first scan sees, and
			// what lands after it is NOT its summary.
			r := hbNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
			r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "auto", 400000, 9000, 120000))
			first := r.scan()
			r.write(tc.next(r))
			// Act
			second := r.scan()
			// Assert: the next line in FILE ORDER settles the hold definitively —
			// the boundary emits, bare and loudly, on the scan that reads it.
			if n := len(ccintCompactions(first)); n != 0 {
				t.Fatalf("ContextCompacted count on the holding scan = %d, want 0", n)
			}
			got := ccintCompactions(second)
			if len(got) != 1 {
				t.Fatalf("ContextCompacted count on the settling scan = %d, want 1", len(got))
			}
			if s := got[0].GetContextCompacted().GetSummary(); s != "" {
				t.Fatalf("summary = %q, want empty (the following line is not a summary)", s)
			}
			if !r.logged(missingSummaryLog) {
				t.Fatalf("missing the loud summary-less log; got %v", r.logs)
			}
			// …and the line that settled it is converted like any other.
			if n := len(vendorTwins(second)); n != 2 {
				t.Fatalf("vendor twins on the settling scan = %d, want 2 (the redelivered boundary and the line after it)", n)
			}
		})
	}
}

// --- a held boundary settled by ANOTHER boundary -----------------------------

func TestHeldBoundarySettledByAnotherBoundaryHoldsTheNewOne(t *testing.T) {
	// Arrange: two compactions back to back, with a scan landing after each.
	// The second boundary settles the first (it is not a summary) and is itself
	// unsettled, so the hold moves onto it.
	r := hbNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	beforeSecond := r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "auto", 400000, 9000, 120000))
	r.scan()
	r.write(ccintBoundaryLine("b-2", r.uuid, "2026-07-21T18:06:00.000Z", "auto", 1, 2, 3))
	// Act
	got := ccintCompactions(r.scan())
	// Assert: exactly the first one emits, and the cursor parks before the
	// second so its own summary still has a chance to arrive.
	if len(got) != 1 || got[0].GetDedupKey() != "compact:b-1" {
		t.Fatalf("compactions = %d keyed %v, want 1 keyed compact:b-1", len(got), ccintCutKeys(got))
	}
	if off := r.last.Next.GetOffset(); off != beforeSecond {
		t.Fatalf("committed offset = %d, want %d (the byte the newly held boundary starts at)", off, beforeSecond)
	}
}

// --- a held boundary the file never says anything more about -----------------

func TestHeldBoundarySurvivedByOneSilentScanEmitsBare(t *testing.T) {
	// Arrange: the session stops AT the boundary — the summary never comes.
	r := hbNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 10, 5, 7))
	first := r.scan()
	// Act: a scan that brings nothing new. One is the whole silence bound.
	second := r.scan()
	// Assert
	if n := len(ccintCompactions(first)); n != 0 {
		t.Fatalf("ContextCompacted count on the holding scan = %d, want 0", n)
	}
	got := ccintCompactions(second)
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count after one silent scan = %d, want 1 (the compaction still happened)", len(got))
	}
	if s := got[0].GetContextCompacted().GetSummary(); s != "" {
		t.Fatalf("summary = %q, want empty", s)
	}
	if !r.logged(missingSummaryLog) {
		t.Fatalf("missing the loud summary-less log; got %v", r.logs)
	}
}

// --- the cursor never advances past a held boundary --------------------------

func TestHeldBoundaryIsRereadAndCoalescedAfterARestart(t *testing.T) {
	// Arrange: a settled line, then the boundary, which is where the scan lands.
	r := hbNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	beforeBoundary := r.write(ccintUserLine("u-1", r.uuid, "go on"))
	r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "manual", 435029, 8639, 194511))
	if n := len(ccintCompactions(r.scan())); n != 0 {
		t.Fatalf("ContextCompacted count on the holding scan = %d, want 0", n)
	}
	// Assert (durability): the committed cursor stops BEFORE the held line, so
	// the boundary is still on the unread side of it.
	if off := r.last.Next.GetOffset(); off != beforeBoundary {
		t.Fatalf("committed offset = %d, want %d (the byte the held boundary starts at)", off, beforeBoundary)
	}
	// Act: the process dies mid-hold and comes back on that cursor, by which
	// time the summary has been written.
	r.write(ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "what the discarded history said"))
	r.restart()
	got := ccintCompactions(r.scan())
	// Assert: nothing was lost with the process — the boundary is re-read from
	// the file and coalesced with the summary that is now next to it.
	if len(got) != 1 {
		t.Fatalf("ContextCompacted count after the restart = %d, want 1", len(got))
	}
	if s := got[0].GetContextCompacted().GetSummary(); s != "what the discarded history said" {
		t.Fatalf("summary = %q, want the summary line's text", s)
	}
}

// --- a held boundary settled by its own summary ------------------------------

func TestHeldBoundarySettledByItsSummaryEmitsOnceAndReleasesTheCursor(t *testing.T) {
	// Arrange: the scan boundary falls between the pair.
	r := hbNewRig(t, "f77ed5b2-5a0f-4f66-9c6c-9f4f50574caa")
	r.write(ccintBoundaryLine("b-1", r.uuid, "2026-07-21T18:05:50.654Z", "auto", 400000, 9000, 120000))
	first := r.scan()
	size := r.write(ccintSummaryLine("s-1", "b-1", r.uuid, "2026-07-21T18:05:50.653Z", "the summary that arrived late"))
	// Act
	second := r.scan()
	// Assert: exactly one event for the pair, never one per scan.
	all := append(append([]*corev1.Event{}, first...), second...)
	if n := len(ccintCompactions(all)); n != 1 {
		t.Fatalf("ContextCompacted count across both scans = %d, want 1", n)
	}
	// …and with the hold released, the cursor moves past BOTH lines.
	if off := r.last.Next.GetOffset(); off != size {
		t.Fatalf("committed offset = %d, want %d (past the boundary and its summary)", off, size)
	}
	if r.logged(missingSummaryLog) {
		t.Fatalf("a coalesced compaction loud-logged a missing summary; got %v", r.logs)
	}
}

// --- a handler with no redelivery promise never holds ------------------------

func TestBatchEndBoundaryWithoutRedeliveryEmitsImmediately(t *testing.T) {
	// Arrange: a caller handing the handler one standalone batch. There is no
	// next delivery, so a hold here would drop the compaction for good.
	h := NewSessionTranscriptHandler(quietLog)
	frames := framesFor(t, boundaryLineJSON("b1", "2026-07-07T22:01:31.660Z", "manual", 10, 5, 7))
	ctx := &Context{SessionID: "s1"}
	// Act
	ev := findCompacted(h.Handle(frames, ctx))
	// Assert
	if ev == nil {
		t.Fatal("a boundary was held by a handler nobody will redeliver to; the compaction still happened")
	}
	if ctx.HeldDeliveries != 0 {
		t.Fatalf("HeldDeliveries = %d, want 0", ctx.HeldDeliveries)
	}
}
