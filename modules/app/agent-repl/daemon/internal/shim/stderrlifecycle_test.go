package shim

import (
	"errors"
	"io"
	"os"
	"strings"
	"testing"
)

// scriptedStderrReader makes scanner terminal conditions deterministic without
// starting a shim process.  Each chunk is returned once; terminal is returned
// after the final chunk.
type scriptedStderrReader struct {
	chunks   [][]byte
	terminal error
}

func (r *scriptedStderrReader) Read(p []byte) (int, error) {
	if len(r.chunks) == 0 {
		return 0, r.terminal
	}
	chunk := r.chunks[0]
	r.chunks = r.chunks[1:]
	return copy(p, chunk), nil
}

func (r *scriptedStderrReader) Close() error { return nil }

// blockedStderrReader keeps the scanner live until the test releases the
// terminal condition, proving that the pump completion signal is a join point.
type blockedStderrReader struct {
	started chan struct{}
	release chan struct{}
}

func (r *blockedStderrReader) Read([]byte) (int, error) {
	close(r.started)
	<-r.release
	return 0, io.EOF
}

func (r *blockedStderrReader) Close() error { return nil }

func newLifecycleTestPump(reader io.ReadCloser, logger Logger) *stderrPump {
	return newStderrPump(reader, logger, &stderrTail{}, 731, 739, true)
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

func TestStderrPumpLogsCleanEOFWithCanonicalLifecycleContext(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)

	pump.run()

	assertLogContains(t, logger, "stderr scanner started", "pid=731", "pgid=739", `shutdown_initiator="child_exit"`, `close_owner="unclaimed"`)
	assertLogContains(t, logger, "stderr scanner completed", "outcome=clean_eof", "expected_close=false", `shutdown_reason="no deliberate stop requested"`)
	assertNoLogContains(t, logger, "stderr scan error")
}

func TestStderrPumpAnnouncesItselfOnTheLifecycleChannel(t *testing.T) {
	// Arrange: the daemon's logger stamps Log level=error, so the channel the
	// pump announces itself on decides whether a healthy spawn writes an error
	// record. The scan-error path keeps Log, which the tests above cover.
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)

	// Act.
	pump.run()

	// Assert.
	for _, record := range logger.logged() {
		if strings.Contains(record.line, "stderr scanner started") {
			if record.level != "lifecycle" {
				t.Fatalf("scanner-start record = %#v, want the lifecycle channel", record)
			}
			return
		}
	}
	t.Fatalf("no scanner-start record at all: %#v", logger.logged())
}

func TestStderrPumpClassifiesOnlyLifecycleOwnedClosedReaderAsExpected(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: os.ErrClosed}, logger)
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "stderr scanner completed", "outcome=expected_reader_close", "expected_close=true", `close_owner="proc_wait"`)
	assertNoLogContains(t, logger, "stderr scan error")
}

func TestStderrPumpDoesNotSuppressNonClosedFailureAfterExpectedMarker(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: errors.New("reader exploded")}, logger)
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "stderr scan error", "reader exploded", `close_expected=true`, `close_owner="proc_wait"`)
	assertLogContains(t, logger, "stderr scanner completed", "outcome=scanner_error", "expected_close=false")
}

func TestStderrPumpDoesNotReclassifyCleanEOFAfterExpectedMarker(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "stderr scanner completed", "outcome=clean_eof", "expected_close=false", `close_owner="proc_wait"`)
	assertNoLogContains(t, logger, "stderr scan error")
}

func TestStderrPumpRetainsSIGTERMStopAttributionThroughExpectedClose(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: os.ErrClosed}, logger)
	stop := Stop{Initiator: "session_controller_hibernate", Reason: "idle timeout"}
	if err := pump.signal(stop, func() error { return nil }); err != nil {
		t.Fatalf("signal: %v", err)
	}
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "stderr scanner completed", `shutdown_initiator="session_controller_hibernate"`, `shutdown_reason="idle timeout"`, "outcome=expected_reader_close", "expected_close=true")
}

func TestStderrPumpRetainsSessionDeletionStopAttributionThroughExpectedClose(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: os.ErrClosed}, logger)
	stop := Stop{Initiator: "session_controller_delete", Reason: "session deletion"}
	if err := pump.signal(stop, func() error { return nil }); err != nil {
		t.Fatalf("signal: %v", err)
	}
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "stderr scanner completed", `shutdown_initiator="session_controller_delete"`, `shutdown_reason="session deletion"`, `close_owner="proc_wait"`, "outcome=expected_reader_close")
}

func TestProcSignalFailureLogsCanonicalContextWithoutPartialAttribution(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)
	p := &Proc{logger: logger, stderrPump: pump, pid: 731, pgid: 739}
	stop := Stop{Initiator: "session_controller_delete", Reason: "session deletion"}

	err := p.signal(stop, "terminate", func() error { return errors.New("signal refused") })

	if err == nil || !strings.Contains(err.Error(), "signal refused") {
		t.Fatalf("signal error = %v, want the send failure", err)
	}
	lifecycle := pump.snapshot()
	if lifecycle.shutdownInitiator != "child_exit" || lifecycle.shutdownReason != "no deliberate stop requested" {
		t.Fatalf("failed signal committed shutdown attribution: %+v", lifecycle)
	}
	assertLogContains(t, logger, "shutdown signal failed", "verb=terminate", "pid=731", "pgid=739", `initiator="session_controller_delete"`, `reason="session deletion"`, "signal refused")
}

func TestProcSignalRejectsAndLogsInvalidStopWithoutSending(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)
	p := &Proc{logger: logger, stderrPump: pump, pid: 731, pgid: 739}
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
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)
	p := &Proc{logger: logger, stderrPump: pump, pid: 731, pgid: 739}
	stop := Stop{Initiator: "session_controller_hibernate", Reason: "idle timeout"}

	if err := p.signal(stop, "terminate", func() error { return nil }); err != nil {
		t.Fatalf("signal: %v", err)
	}

	lifecycle := pump.snapshot()
	if lifecycle.shutdownInitiator != stop.Initiator || lifecycle.shutdownReason != stop.Reason {
		t.Fatalf("successful signal did not commit shutdown attribution: %+v", lifecycle)
	}
	assertLogContains(t, logger, "shutdown requested", "verb=terminate", "pid=731", "pgid=739", `initiator="session_controller_hibernate"`, `reason="idle timeout"`)
}

func TestStderrPumpRejectsConflictingCloseOwners(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: io.EOF}, logger)
	pump.expectClose(stderrCloseOwnerCaller)

	defer func() {
		if recovered := recover(); recovered == nil || !strings.Contains(recovered.(string), "close owner already committed") {
			t.Fatalf("conflicting close owner panic = %#v", recovered)
		}
		assertLogContains(t, logger, "stderr close ownership invariant violated", "pid=731", "pgid=739", `committed_owner="caller"`, `requested_owner="proc_wait"`, "close_expected=true")
	}()
	pump.expectClose(stderrCloseOwnerProcWait)
}

func TestStderrPumpReportsUnexpectedReaderFailureBeforeShutdown(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{terminal: errors.New("read exploded")}, logger)

	pump.run()

	assertLogContains(t, logger, "stderr scan error", "read exploded", `close_expected=false`, `close_owner="unclaimed"`)
	assertLogContains(t, logger, "stderr scanner completed", "outcome=scanner_error", "expected_close=false")
}

func TestStderrPumpKeepsMalformedStderrButSuppressesExpectedCloseError(t *testing.T) {
	logger := &recordingLogger{}
	pump := newLifecycleTestPump(&scriptedStderrReader{
		chunks:   [][]byte{[]byte("not a canonical shim record\n")},
		terminal: os.ErrClosed,
	}, logger)
	pump.expectClose(stderrCloseOwnerProcWait)

	pump.run()

	assertLogContains(t, logger, "shim stderr malformed", "not a canonical shim record")
	assertLogContains(t, logger, "stderr scanner completed", "outcome=expected_reader_close", "expected_close=true")
	assertNoLogContains(t, logger, "stderr scan error")
}

func TestStderrPumpWaitReturnsOnlyAfterScannerCompletion(t *testing.T) {
	logger := &recordingLogger{}
	reader := &blockedStderrReader{started: make(chan struct{}), release: make(chan struct{})}
	pump := newLifecycleTestPump(reader, logger)
	finished := make(chan struct{})
	go func() {
		pump.run()
		close(finished)
	}()
	<-reader.started

	select {
	case <-pump.done:
		t.Fatal("pump completed before its reader reached a terminal condition")
	default:
	}
	close(reader.release)
	pump.wait()
	<-finished

	assertLogContains(t, logger, "stderr scanner completed", "outcome=clean_eof")
}
