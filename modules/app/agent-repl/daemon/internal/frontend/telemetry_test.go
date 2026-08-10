package frontend

import (
	"strings"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// clientLogCmdFor is a workspace-addressed client log, which is what a webview
// actually sends: the record names the workspace it came from.
func clientLogCmdFor(requestID, workspace string) *frontendv1.FrontendCommand {
	cmd := clientLogCmd(requestID)
	cmd.Workspace = workspace
	return cmd
}

// recordingWriter collects the telemetry writer's warn lines under its own
// lock, since they are emitted from both the submitting and writing goroutines.
type recordingWriter struct {
	mu    sync.Mutex
	warns []string
}

func (r *recordingWriter) warnf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.warns = append(r.warns, sprintfLane(format, args...))
}

func (r *recordingWriter) lines() []string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return append([]string(nil), r.warns...)
}

func TestTelemetryWriterPreservesReceiptOrderPerWorkspace(t *testing.T) {
	// Arrange: the daemon's webapp.log is read as a sequence, so the single
	// writer goroutine must emit a workspace's records in receipt order.
	rec := &recordingWriter{}
	written := make(chan string, 3)
	w := newTelemetryWriter(rec.warnf, func(item telemetryRecord) {
		written <- item.cmd.GetRequestId()
	})

	// Act.
	for _, rid := range []string{"log1", "log2", "log3"} {
		w.submit(telemetryRecord{cmd: clientLogCmdFor(rid, "/ws/a")})
	}
	w.close()

	// Assert.
	var got []string
	for i := 0; i < 3; i++ {
		select {
		case rid := <-written:
			got = append(got, rid)
		case <-time.After(laneTestDeadline):
			t.Fatalf("only %d of 3 records were written: %v", len(got), got)
		}
	}
	if strings.Join(got, ",") != "log1,log2,log3" {
		t.Fatalf("write order = %v, want receipt order", got)
	}
}

func TestTelemetryWriterDropsTheOldestRecordOnOverflow(t *testing.T) {
	// Arrange: a writer parked inside one write, so the queue fills and the
	// next submit must evict the OLDEST queued record rather than the newest.
	rec := &recordingWriter{}
	release := make(chan struct{})
	inWrite := make(chan struct{})
	written := make(chan string, 8)
	w := newTelemetryWriter(rec.warnf, func(item telemetryRecord) {
		if item.cmd.GetRequestId() == "blocker" {
			close(inWrite)
			<-release
		}
		written <- item.cmd.GetRequestId()
	})
	w.capacity = 2
	w.submit(telemetryRecord{cmd: clientLogCmdFor("blocker", "/ws/a")})
	<-inWrite

	// Act: three more with room for two — the oldest of them is evicted.
	w.submit(telemetryRecord{cmd: clientLogCmdFor("oldest", "/ws/a")})
	w.submit(telemetryRecord{cmd: clientLogCmdFor("middle", "/ws/a")})
	w.submit(telemetryRecord{cmd: clientLogCmdFor("newest", "/ws/a")})
	close(release)
	w.close()

	// Assert.
	var got []string
	for i := 0; i < 3; i++ {
		select {
		case rid := <-written:
			got = append(got, rid)
		case <-time.After(laneTestDeadline):
			t.Fatalf("only %d of 3 records were written: %v", len(got), got)
		}
	}
	if strings.Join(got, ",") != "blocker,middle,newest" {
		t.Fatalf("written = %v, want the oldest queued record dropped", got)
	}
}

func TestTelemetryWriterReportsDropsWithWorkspaceAndCount(t *testing.T) {
	// Arrange: a drop is allowed, a SILENT drop is not — the report names the
	// workspace whose evidence was lost and how much of it.
	rec := &recordingWriter{}
	release := make(chan struct{})
	inWrite := make(chan struct{})
	w := newTelemetryWriter(rec.warnf, func(item telemetryRecord) {
		if item.cmd.GetRequestId() == "blocker" {
			close(inWrite)
			<-release
		}
	})
	w.capacity = 1
	w.submit(telemetryRecord{cmd: clientLogCmdFor("blocker", "/ws/flood")})
	<-inWrite

	// Act: two evictions, reported under the rate limit as one accumulated line.
	w.submit(telemetryRecord{cmd: clientLogCmdFor("a", "/ws/flood")})
	w.submit(telemetryRecord{cmd: clientLogCmdFor("b", "/ws/flood")})
	w.submit(telemetryRecord{cmd: clientLogCmdFor("c", "/ws/flood")})
	close(release)
	w.close()

	// Assert.
	var report string
	for _, line := range rec.lines() {
		if strings.Contains(line, "DROPPED") {
			report = line
		}
	}
	if report == "" {
		t.Fatalf("no drop report was written: %v", rec.lines())
	}
	if !strings.Contains(report, `ws=/ws/flood`) {
		t.Fatalf("drop report %q does not name the workspace", report)
	}
	if !strings.Contains(report, "dropped_total=2") {
		t.Fatalf("drop report %q does not count both drops", report)
	}
}

func TestTelemetryWriterReportsASlowWriteWithItsStageAndElapsed(t *testing.T) {
	// Arrange: the write still passes through daemon-global logging stages, so
	// a slow one must name itself rather than be inferred from the next
	// incident's timeline.
	rec := &recordingWriter{}
	done := make(chan struct{})
	w := newTelemetryWriter(rec.warnf, func(telemetryRecord) { close(done) })
	// Every write is over the bound, which is how the record's own content is
	// asserted without timing anything.
	w.warnAfter = 0

	// Act.
	w.submit(telemetryRecord{cmd: clientLogCmdFor("log", "/ws/a")})
	<-done
	w.close()

	// Assert.
	var slow string
	for _, line := range rec.lines() {
		if strings.Contains(line, "SLOW") {
			slow = line
		}
	}
	if slow == "" {
		t.Fatalf("no slow-write record was written: %v", rec.lines())
	}
	if !strings.Contains(slow, "stage=dlog_workspace_target") || !strings.Contains(slow, "elapsed_ms=") {
		t.Fatalf("slow-write record %q lacks its stage or elapsed", slow)
	}
}

func TestTelemetryWriterReportsARecordSubmittedAfterClose(t *testing.T) {
	// Arrange: the connection's writer is gone, so this record cannot be
	// written. It is still counted and still reported.
	rec := &recordingWriter{}
	w := newTelemetryWriter(rec.warnf, func(telemetryRecord) {})
	w.close()

	// Act.
	w.submit(telemetryRecord{cmd: clientLogCmdFor("late", "/ws/late")})

	// Assert.
	var report string
	for _, line := range rec.lines() {
		if strings.Contains(line, "DROPPED") {
			report = line
		}
	}
	if !strings.Contains(report, "ws=/ws/late") {
		t.Fatalf("post-close drop report = %q, want it to name the workspace", report)
	}
}
