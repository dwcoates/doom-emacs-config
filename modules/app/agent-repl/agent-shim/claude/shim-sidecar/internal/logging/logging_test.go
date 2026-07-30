package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"testing"
	"time"
)

func TestLogFormatsContextAndRoutesToBothSinks(t *testing.T) {
	var stderr, file bytes.Buffer
	l := New(&stderr, &file)
	var diagnostic Diagnostic
	l.With(Context{}).SetDiagnosticSink(func(d Diagnostic) { diagnostic = d })
	l.now = func() time.Time { return time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC) }
	l.pid = func() int { return 42 }
	l.With(Context{Component: "tail", Path: "/tmp/a", Session: "s1"}).With(Context{Operation: "poll"}).Log("read %d bytes", 42)

	var got record
	if file.Len() != 0 {
		t.Fatalf("session record leaked into global file: %q", file.String())
	}
	if err := json.Unmarshal(stderr.Bytes(), &got); err != nil {
		t.Fatalf("persistent record is not JSON: %v\n%s", err, file.String())
	}
	if got.Timestamp != "2026-07-28T12:34:56.789Z" || got.Runtime != "sidecar" || got.PID != 42 {
		t.Fatalf("runtime identity = %#v", got)
	}
	if got.Level != "info" || got.Verbosity != "normal" || got.Operation != "poll" || got.Message != "read 42 bytes" {
		t.Fatalf("record fields = %#v", got)
	}
	if got.ClaudeSessionID != "s1" || got.Context["component"] != "tail" || got.Context["path"] != "/tmp/a" {
		t.Fatalf("record attribution = %#v", got)
	}
	if diagnostic.Session != "s1" || diagnostic.Operation != "poll" || diagnostic.Context["path"] != "/tmp/a" {
		t.Fatalf("diagnostic sink attribution = %#v", diagnostic)
	}
}

func TestSessionDiagnosticRequiresSinkAndNeverWritesGlobalFile(t *testing.T) {
	var stderr, file bytes.Buffer
	l := New(&stderr, &file)
	got := capturePanic(t, func() {
		l.With(Context{Session: "s1", Operation: "poll"}).Log("read")
	})
	if !strings.Contains(fmt.Sprint(got), "sink is not installed") {
		t.Fatalf("panic = %v", got)
	}
	if file.Len() != 0 {
		t.Fatalf("session record leaked into global file: %q", file.String())
	}
}

func TestSinkEmergencyWritesJSONOnlyToStderrWithoutEnqueueing(t *testing.T) {
	var stderr, file bytes.Buffer
	var queued int
	l := New(&stderr, &file)
	l.With(Context{}).SetDiagnosticSink(func(Diagnostic) { queued++ })
	l.With(Context{Session: "s1", Operation: "diagnostic-flush", Level: "error", SinkEmergency: true}).Log("store unavailable")
	if queued != 0 {
		t.Fatalf("sink emergency enqueued %d diagnostics", queued)
	}
	if file.Len() != 0 {
		t.Fatalf("sink emergency wrote global file: %q", file.String())
	}
	var got record
	if err := json.Unmarshal(stderr.Bytes(), &got); err != nil {
		t.Fatalf("stderr emergency is not JSON: %v: %q", err, stderr.String())
	}
	if got.Level != "error" || got.ClaudeSessionID != "s1" || got.Message != "store unavailable" {
		t.Fatalf("stderr emergency record = %#v", got)
	}
}

func TestLogVerboseRequiresVerboseModeForEverySink(t *testing.T) {
	var stderr, file bytes.Buffer
	l := New(&stderr, &file)
	var diagnostics []Diagnostic
	l.With(Context{}).SetDiagnosticSink(func(d Diagnostic) {
		diagnostics = append(diagnostics, d)
	})
	var constructed bool
	l.now = func() time.Time {
		constructed = true
		return time.Now()
	}
	l.pid = func() int {
		constructed = true
		return 1
	}
	l.verbose = func() bool { return false }
	l.With(Context{Component: "discover", Operation: "scan"}).LogVerbose("scan complete")
	l.With(Context{Component: "tail", Session: "s1", Operation: "poll"}).LogVerbose("poll complete")
	if file.Len() != 0 || stderr.Len() != 0 || len(diagnostics) != 0 || constructed {
		t.Fatalf("disabled verbose record was constructed or reached a sink: constructed=%t file=%q stderr=%q diagnostics=%#v", constructed, file.String(), stderr.String(), diagnostics)
	}
	l.verbose = func() bool { return true }
	l.With(Context{Component: "discover", Operation: "scan"}).LogVerbose("scan enabled")
	l.With(Context{Component: "tail", Session: "s1", Operation: "poll"}).LogVerbose("poll enabled")
	if !strings.Contains(file.String(), "scan enabled") {
		t.Fatalf("enabled verbose record missing from file: %q", file.String())
	}
	if !strings.Contains(stderr.String(), "scan enabled") {
		t.Fatalf("enabled verbose record missing from stderr: %q", stderr.String())
	}
	if len(diagnostics) != 1 || diagnostics[0].Message != "poll enabled" {
		t.Fatalf("enabled verbose session diagnostics = %#v", diagnostics)
	}
}

func TestLogVerboseValidatesContextWhileDisabled(t *testing.T) {
	var stderr, file bytes.Buffer
	l := New(&stderr, &file)
	l.verbose = func() bool { return false }
	got := capturePanic(t, func() {
		l.With(Context{Component: "tail"}).LogVerbose("record")
	})
	if !strings.Contains(fmt.Sprint(got), "operation is required") {
		t.Fatalf("panic = %v, want missing operation", got)
	}
	got = capturePanic(t, func() {
		l.With(Context{Operation: "poll", Level: "fatal"}).LogVerbose("record")
	})
	if !strings.Contains(fmt.Sprint(got), "invalid level") {
		t.Fatalf("panic = %v, want invalid level", got)
	}
	if file.Len() != 0 || stderr.Len() != 0 {
		t.Fatalf("invalid disabled records mutated sinks: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestLogPersistentSinkFailureUsesEmergencyStderr(t *testing.T) {
	var stderr bytes.Buffer
	err := errors.New("disk full")
	l := New(&stderr, failingWriter{err: err})
	got := capturePanic(t, func() {
		l.With(Context{Component: "store", Operation: "write"}).Log("write failed")
	})
	var emergency record
	if decodeErr := json.Unmarshal(stderr.Bytes(), &emergency); decodeErr != nil {
		t.Fatalf("emergency stderr is not JSON: %v: %q", decodeErr, stderr.String())
	}
	if emergency.Operation != "sidecar.logging.sink-failure" || emergency.Level != "error" {
		t.Fatalf("emergency stderr record = %#v", emergency)
	}
	if !strings.Contains(fmt.Sprint(got), "disk full") {
		t.Fatalf("panic = %v, want persistent sink failure", got)
	}
}

func TestLogCompletesPartialGlobalWritesAndPoisonsAfterFailure(t *testing.T) {
	var stderr bytes.Buffer
	file := &partialWriter{limit: 3}
	l := New(&stderr, file)
	l.With(Context{Operation: "boot"}).Log("ready")
	if !strings.Contains(file.String(), "ready") {
		t.Fatalf("partial writer did not receive complete record: %q", file.String())
	}
	failing := New(&stderr, failingWriter{err: errors.New("disk full")})
	capturePanic(t, func() { failing.With(Context{Operation: "boot"}).Log("first") })
	got := capturePanic(t, func() { failing.With(Context{Operation: "boot"}).Log("second") })
	if !strings.Contains(fmt.Sprint(got), "previously failed") {
		t.Fatalf("poison panic = %v", got)
	}
}

func TestLogStderrSinkFailurePanicsAfterPersistence(t *testing.T) {
	var file bytes.Buffer
	err := errors.New("terminal closed")
	got := capturePanic(t, func() {
		New(failingWriter{err: err}, &file).With(Context{Component: "store", Operation: "write"}).Log("write failed")
	})
	if !strings.Contains(file.String(), "write failed") {
		t.Fatalf("persistent record missing before stderr failure: %q", file.String())
	}
	if !strings.Contains(fmt.Sprint(got), "terminal closed") {
		t.Fatalf("panic = %v, want stderr sink failure", got)
	}
}

func TestLogReportsBothSinkFailures(t *testing.T) {
	fileErr := errors.New("disk full")
	stderrErr := errors.New("terminal closed")
	got := capturePanic(t, func() {
		New(failingWriter{err: stderrErr}, failingWriter{err: fileErr}).With(Context{Component: "store", Operation: "write"}).Log("write failed")
	})
	message := fmt.Sprint(got)
	if !strings.Contains(message, "disk full") || !strings.Contains(message, "terminal closed") {
		t.Fatalf("panic = %v, want both sink failures", got)
	}
}

func TestLogRejectsMissingOperationAndInvalidLevel(t *testing.T) {
	var stderr, file bytes.Buffer
	l := New(&stderr, &file)
	got := capturePanic(t, func() {
		l.With(Context{Component: "tail"}).Log("record")
	})
	if !strings.Contains(fmt.Sprint(got), "operation is required") {
		t.Fatalf("panic = %v, want missing operation", got)
	}
	got = capturePanic(t, func() {
		l.With(Context{Operation: "poll", Level: "fatal"}).Log("record")
	})
	if !strings.Contains(fmt.Sprint(got), "invalid level") {
		t.Fatalf("panic = %v, want invalid level", got)
	}
	if file.Len() != 0 || stderr.Len() != 0 {
		t.Fatalf("invalid records mutated sinks: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestNilLoggerUseFailsLoudly(t *testing.T) {
	assertPanics := func(name string, call func()) {
		t.Helper()
		defer func() {
			if recover() == nil {
				t.Errorf("%s did not panic", name)
			}
		}()
		call()
	}
	var logger *Logger
	assertPanics("Logger.With", func() { logger.With(Context{}) })
	var bound *Bound
	assertPanics("Bound.With", func() { bound.With(Context{}) })
	assertPanics("Bound.Log", func() { bound.Log("record") })
	assertPanics("Bound.LogVerbose", func() { bound.LogVerbose("record") })
}

func capturePanic(t *testing.T, call func()) any {
	t.Helper()
	var got any
	func() {
		defer func() { got = recover() }()
		call()
	}()
	if got == nil {
		t.Fatal("call did not panic")
	}
	return got
}

type failingWriter struct{ err error }

func (w failingWriter) Write([]byte) (int, error) {
	if w.err == nil {
		return 0, io.ErrClosedPipe
	}
	return 0, w.err
}

type partialWriter struct {
	bytes.Buffer
	limit int
}

func (w *partialWriter) Write(p []byte) (int, error) {
	if len(p) > w.limit {
		p = p[:w.limit]
	}
	return w.Buffer.Write(p)
}
