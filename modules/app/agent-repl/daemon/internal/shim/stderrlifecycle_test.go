package shim

import (
	"errors"
	"io"
	"strings"
	"sync"
	"testing"
)

// blockedStderrReader keeps the stream live until the test releases it, which
// is what a descendant that inherited the child's stderr and outlived it looks
// like from the daemon's side.
type blockedStderrReader struct {
	started   chan struct{}
	release   chan struct{}
	closed    chan struct{}
	startOnce sync.Once
	closeOnce sync.Once
}

func newBlockedStderrReader() *blockedStderrReader {
	return &blockedStderrReader{started: make(chan struct{}), release: make(chan struct{}), closed: make(chan struct{})}
}

func (r *blockedStderrReader) Read([]byte) (int, error) {
	r.startOnce.Do(func() { close(r.started) })
	<-r.release
	return 0, io.EOF
}

// Close releases a blocked read the way closing a real descriptor would, and is
// idempotent so a test may release the stream itself first.
func (r *blockedStderrReader) Close() error {
	r.closeOnce.Do(func() {
		close(r.closed)
		r.releaseOnce()
	})
	return nil
}

func (r *blockedStderrReader) releaseOnce() {
	select {
	case <-r.release:
	default:
		close(r.release)
	}
}

// newTestStderrSink is the parsing sink every focused sink test drains into.
func newTestStderrSink(logger Logger) *stderrSink {
	return &stderrSink{logger: logger, tail: &stderrTail{}, parse: true}
}

// drainStderr runs one reader through a parsing sink to completion, including
// the trailing line the stream never terminated.
func drainStderr(t *testing.T, logger Logger, reader io.Reader) *stderrSink {
	t.Helper()
	sink := newTestStderrSink(logger)
	pump := newStderrPump(io.NopCloser(reader), sink, logger, 731, 739)
	pump.run()
	sink.flush()
	return sink
}

func assertLogContains(t *testing.T, logger *recordingLogger, fragments ...string) {
	t.Helper()
	for _, record := range logger.logged() {
		matched := true
		for _, fragment := range fragments {
			if !strings.Contains(record.line, fragment) {
				matched = false
				break
			}
		}
		if matched {
			return
		}
	}
	t.Fatalf("no log record contains all %q: %#v", fragments, logger.logged())
}

func assertNoLogContains(t *testing.T, logger *recordingLogger, fragment string) {
	t.Helper()
	for _, record := range logger.logged() {
		if strings.Contains(record.line, fragment) {
			t.Fatalf("unexpected log containing %q: %#v", fragment, record)
		}
	}
}

func TestStderrPumpReportsACleanEndOfStreamAsComplete(t *testing.T) {
	// Arrange
	logger := &recordingLogger{}
	pump := newStderrPump(io.NopCloser(strings.NewReader("")), newTestStderrSink(logger), logger, 731, 739)

	// Act
	pump.run()

	// Assert
	if outcome := pump.finish(); outcome != stderrOutcomeComplete {
		t.Fatalf("outcome = %q, want %q", outcome, stderrOutcomeComplete)
	}
	assertNoLogContains(t, logger, "stderr read failed")
}

func TestStderrPumpFinishReturnsOnlyAfterTheStreamHasEnded(t *testing.T) {
	// Arrange: finish is the point at which the tail is guaranteed to hold
	// everything the reaped child wrote, so it must not report an outcome while
	// a reader is still live.
	logger := &recordingLogger{}
	reader := newBlockedStderrReader()
	pump := newStderrPump(reader, newTestStderrSink(logger), logger, 731, 739)
	go pump.run()
	<-reader.started

	// Act / Assert: nothing is published while the reader is live.
	select {
	case <-pump.done:
		t.Fatal("the pump published an outcome before its stream ended")
	default:
	}
	reader.releaseOnce()
	if outcome := pump.finish(); outcome != stderrOutcomeComplete {
		t.Fatalf("outcome = %q, want %q", outcome, stderrOutcomeComplete)
	}
}

func TestStderrPumpReportsACloseFailureAtTheReap(t *testing.T) {
	// Arrange: a stream the daemon cannot release is a descriptor it has lost,
	// which must not pass quietly.
	logger := &recordingLogger{}
	reader := newBlockedStderrReader()
	reader.releaseOnce()
	pump := newStderrPump(closeFailingReader{reader}, newTestStderrSink(logger), logger, 731, 739)
	pump.run()

	// Act
	pump.finish()

	// Assert
	assertLogContains(t, logger, "closing the retained stderr stream failed", "pid=731", "pgid=739", "close refused")
}

func TestStderrPumpSurrendersATrailingLineTheChildNeverTerminated(t *testing.T) {
	// Arrange: a child that dies mid-line still wrote the only diagnosis there
	// is going to be.
	logger := &recordingLogger{}

	// Act
	sink := drainStderr(t, logger, strings.NewReader("Error: Cannot find module"))

	// Assert
	assertLogContains(t, logger, "shim stderr malformed", "Cannot find module")
	if got := sink.tail.String(); got != "Error: Cannot find module" {
		t.Fatalf("tail = %q, want the unterminated line retained verbatim", got)
	}
}

func TestStderrSinkSurrendersALineThatOutgrowsTheLineBound(t *testing.T) {
	// Arrange: an unterminated line must not become an unbounded allocation.
	logger := &recordingLogger{}

	// Act
	drainStderr(t, logger, strings.NewReader(strings.Repeat("x", maxEventLine+1)))

	// Assert
	assertLogContains(t, logger, "stderr line exceeded", "retention continues")
}

func TestStderrSinkKeepsRetainingAfterAnOversizedLine(t *testing.T) {
	// Arrange: the old scanner ABANDONED the stream on an over-long token, so
	// everything the child said afterwards was lost.
	logger := &recordingLogger{}

	// Act
	sink := drainStderr(t, logger, strings.NewReader(strings.Repeat("x", maxEventLine+1)+"\nlater words\n"))

	// Assert
	assertLogContains(t, logger, "shim stderr malformed", "later words")
	if !strings.Contains(sink.tail.String(), "later words") {
		t.Fatal("the tail stopped at the oversized line instead of continuing")
	}
}

func TestStderrSinkReportsADirectedWriteFailureWithoutLosingTheTail(t *testing.T) {
	// Arrange: the caller owns the stream, and its writer refuses the bytes.
	logger := &recordingLogger{}
	sink := &stderrSink{logger: logger, tail: &stderrTail{}, directed: failingWriter{}}

	// Act
	if _, err := sink.Write([]byte("boom\n")); err != nil {
		t.Fatalf("Write = %v, want the sink to absorb a directed failure", err)
	}

	// Assert
	assertLogContains(t, logger, "directed stderr write failed", "write refused")
	if got := sink.tail.String(); got != "boom\n" {
		t.Fatalf("tail = %q, want the bytes retained despite the directed failure", got)
	}
}

func TestStderrSinkDoesNotParseAStreamTheCallerOwns(t *testing.T) {
	// Arrange: a caller that owns stderr owns its rendering too.
	logger := &recordingLogger{}
	sink := &stderrSink{logger: logger, tail: &stderrTail{}, directed: io.Discard}

	// Act
	if _, err := sink.Write([]byte("not a canonical shim record\n")); err != nil {
		t.Fatal(err)
	}

	// Assert
	assertNoLogContains(t, logger, "shim stderr malformed")
}

func TestProcSignalFailureLogsCanonicalContextWithoutPartialAttribution(t *testing.T) {
	logger := &recordingLogger{}
	p := &Proc{logger: logger, shutdown: newShutdownAttribution(), pid: 731, pgid: 739}
	stop := Stop{Initiator: "session_controller_delete", Reason: "session deletion"}

	err := p.signal(stop, "terminate", func() error { return errors.New("signal refused") })

	if err == nil || !strings.Contains(err.Error(), "signal refused") {
		t.Fatalf("signal error = %v, want the send failure", err)
	}
	initiator, reason := p.shutdown.snapshot()
	if initiator != "child_exit" || reason != "no deliberate stop requested" {
		t.Fatalf("failed signal committed shutdown attribution: initiator=%q reason=%q", initiator, reason)
	}
	assertLogContains(t, logger, "shutdown signal failed", "verb=terminate", "pid=731", "pgid=739", `initiator="session_controller_delete"`, `reason="session deletion"`, "signal refused")
}

func TestProcSignalRejectsAndLogsInvalidStopWithoutSending(t *testing.T) {
	logger := &recordingLogger{}
	p := &Proc{logger: logger, shutdown: newShutdownAttribution(), pid: 731, pgid: 739}
	sent := false

	err := p.signal(Stop{Reason: "session deletion"}, "terminate", func() error {
		sent = true
		return nil
	})

	if err == nil || !strings.Contains(err.Error(), "Initiator") {
		t.Fatalf("signal error = %v, want missing Initiator refusal", err)
	}
	if sent {
		t.Fatal("invalid stop attribution sent a signal")
	}
	assertLogContains(t, logger, "shutdown attribution invalid", "verb=terminate", "pid=731", "pgid=739", `initiator=""`, `reason="session deletion"`, "Initiator")
}

func TestProcSignalSuccessLogsAndCommitsStopAttribution(t *testing.T) {
	logger := &recordingLogger{}
	p := &Proc{logger: logger, shutdown: newShutdownAttribution(), pid: 731, pgid: 739}
	stop := Stop{Initiator: "session_controller_hibernate", Reason: "idle timeout"}

	if err := p.signal(stop, "terminate", func() error { return nil }); err != nil {
		t.Fatalf("signal: %v", err)
	}

	initiator, reason := p.shutdown.snapshot()
	if initiator != stop.Initiator || reason != stop.Reason {
		t.Fatalf("successful signal did not commit shutdown attribution: initiator=%q reason=%q", initiator, reason)
	}
	assertLogContains(t, logger, "shutdown requested", "verb=terminate", "pid=731", "pgid=739", `initiator="session_controller_hibernate"`, `reason="idle timeout"`)
}

// closeFailingReader is a stream the daemon cannot release.
type closeFailingReader struct{ *blockedStderrReader }

func (r closeFailingReader) Close() error {
	_ = r.blockedStderrReader.Close()
	return errors.New("close refused")
}

// failingWriter is a caller-owned stderr writer that refuses the bytes.
type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("write refused") }
