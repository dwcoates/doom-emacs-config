package shim

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/protocol"
)

func TestSpawnPassesExtraFileAndDirectsStderrWithoutDaemonPersistence(t *testing.T) {
	fdTarget, err := os.CreateTemp(t.TempDir(), "shim-fd-*")
	if err != nil {
		t.Fatal(err)
	}
	defer fdTarget.Close()
	terminal, err := os.CreateTemp(t.TempDir(), "shim-stderr-*")
	if err != nil {
		t.Fatal(err)
	}
	defer terminal.Close()
	logger := &recordingLogger{}
	p, err := Spawn(Options{
		Argv:       []string{"/bin/sh", "-c", "printf fd3 >&3; printf stderr >&2"},
		Logger:     logger,
		ExtraFiles: []*os.File{fdTarget},
		Stderr:     terminal,
	})
	if err != nil {
		t.Fatal(err)
	}
	expectEventsClosed(t, p)
	if err := p.Wait(); err != nil {
		t.Fatal(err)
	}
	if err := fdTarget.Sync(); err != nil {
		t.Fatal(err)
	}
	if err := terminal.Sync(); err != nil {
		t.Fatal(err)
	}
	if got, err := os.ReadFile(fdTarget.Name()); err != nil || string(got) != "fd3" {
		t.Fatalf("fd target=%q err=%v", got, err)
	}
	if got, err := os.ReadFile(terminal.Name()); err != nil || string(got) != "stderr" {
		t.Fatalf("terminal=%q err=%v", got, err)
	}
	assertLogContains(t, logger, "child reaped", `close_owner="caller"`, `shutdown_initiator="child_exit"`)
	assertNoLogContains(t, logger, "shim stderr")
}

const recvTimeout = 5 * time.Second

type loggedRecord struct {
	level string
	line  string
}

// recordingLogger collects what the daemon-side logger was told.
//
// IT IS WRITTEN FROM TWO GOROUTINES: Spawn starts a stdout pump and a stderr
// pump and hands both the same Logger, so the recording has to be synchronized
// like the production loggers it stands in for. Reads go through logged() for
// the same reason.
type recordingLogger struct {
	mu      sync.Mutex
	records []loggedRecord
}

// logged returns a snapshot of everything recorded so far.
func (l *recordingLogger) logged() []loggedRecord {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]loggedRecord(nil), l.records...)
}

type mirroringLogger struct {
	recordingLogger
	mirrored []string
}

type failingReader struct{}

func (failingReader) Read([]byte) (int, error) { return 0, errors.New("stderr failed") }

func canonicalShimRecord(verbosity string) string {
	return fmt.Sprintf(`{"timestamp":"2026-07-28T12:00:00.123Z","runtime":"shim","level":"info","verbosity":"%s","operation":"shim.event","message":"started","context":{},"pid":42,"workspace_dir":"/tmp/workspace","workspace_id":"9cc7d801","agent_repl_session_id":"s1"}`, verbosity)
}

func (l *mirroringLogger) MirrorShimRecord(line string) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.mirrored = append(l.mirrored, line)
}

// mirroredRecords snapshots the canonical shim lines mirrored to the terminal.
func (l *mirroringLogger) mirroredRecords() []string {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]string(nil), l.mirrored...)
}

func (l *recordingLogger) Log(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.records = append(l.records, loggedRecord{level: "normal", line: fmt.Sprintf(format, args...)})
}

func (l *recordingLogger) LogVerbose(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.records = append(l.records, loggedRecord{level: "verbose", line: fmt.Sprintf(format, args...)})
}

func (l *recordingLogger) LogLifecycle(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.records = append(l.records, loggedRecord{level: "lifecycle", line: fmt.Sprintf(format, args...)})
}

func (l *recordingLogger) record(t *testing.T, index int, level, contains string) {
	t.Helper()
	records := l.logged()
	if len(records) <= index {
		t.Fatalf("records = %#v, missing index %d", records, index)
	}
	record := records[index]
	if record.level != level || !strings.Contains(record.line, contains) {
		t.Errorf("record[%d] = %#v, want level=%q containing %q", index, record, level, contains)
	}
}

func recvEvent(t *testing.T, p *Proc) *protocol.L1Event {
	t.Helper()
	select {
	case evt, ok := <-p.Events():
		if !ok {
			t.Fatal("events channel closed while expecting an event")
		}
		return evt
	case <-time.After(recvTimeout):
		t.Fatal("timed out waiting for an event")
		return nil
	}
}

func expectEventsClosed(t *testing.T, p *Proc) {
	t.Helper()
	for {
		select {
		case _, ok := <-p.Events():
			if !ok {
				return
			}
		case <-time.After(recvTimeout):
			t.Fatal("timed out waiting for events channel to close")
		}
	}
}

func spawnScript(t *testing.T, script string) *Proc {
	t.Helper()
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", script},
		Logger: &recordingLogger{},
	})
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	t.Cleanup(func() {
		_ = p.CloseStdin()
		_ = p.Wait()
	})
	return p
}

func TestSpawnDecodesEventLines(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `echo '{"type":"ready","session_id":"s1","shim_version":"1","sdk_version":"2","permission_mode":"default"}'`)
	// Assert
	evt := recvEvent(t, p)
	if evt.Type != "ready" || evt.SessionID != "s1" {
		t.Errorf("evt = %+v", evt)
	}
}

func TestSpawnIgnoresUnknownEventTypes(t *testing.T) {
	// Arrange + Act — unknown type first, known type second.
	p := spawnScript(t, `echo '{"type":"mystery"}'; echo '{"type":"ack","session_id":"s1","request_id":"r1"}'`)
	// Assert — first event received is the ack.
	evt := recvEvent(t, p)
	if evt.Type != "ack" {
		t.Errorf("evt = %+v, want ack", evt)
	}
}

func TestSpawnSurfacesMalformedLinesAsTransportErrors(t *testing.T) {
	// Arrange + Act
	logger := &recordingLogger{}
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", `echo 'this is not json'`},
		Logger: logger,
	})
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	t.Cleanup(func() {
		_ = p.CloseStdin()
		_ = p.Wait()
	})
	// Assert
	evt := recvEvent(t, p)
	if evt.Type != "error" || evt.Code != "transport" {
		t.Errorf("evt = %+v, want synthetic transport error", evt)
	}
	assertLogContains(t, logger, "undecodable event line")
}

func TestSpawnClosesEventsOnProcessExit(t *testing.T) {
	// Arrange + Act
	p := spawnScript(t, `true`)
	// Assert
	expectEventsClosed(t, p)
}

func TestSendRawReachesChildStdin(t *testing.T) {
	// Arrange — child echoes stdin back to stdout.
	p := spawnScript(t, `read line; echo "$line"`)
	// Act
	if err := p.SendRaw([]byte(`{"type":"ack","session_id":"s1","request_id":"echoed"}` + "\n")); err != nil {
		t.Fatalf("SendRaw: %v", err)
	}
	// Assert
	evt := recvEvent(t, p)
	if evt.Type != "ack" || evt.RequestID != "echoed" {
		t.Errorf("evt = %+v", evt)
	}
}

func TestSendEncodesCommandAsNDJSON(t *testing.T) {
	// Arrange
	p := spawnScript(t, `read line; echo "$line"`)
	// Act — a shutdown command echoes back; the decoder ignores it
	// (commands are not events), so use the closed-events signal instead.
	if err := p.Send(protocol.NewShutdownCmd("r1", "test")); err != nil {
		t.Fatalf("Send: %v", err)
	}
	// Assert — the child read one full line and exited.
	expectEventsClosed(t, p)
}

func TestSendRawAfterCloseStdinErrors(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	if err := p.CloseStdin(); err != nil {
		t.Fatalf("CloseStdin: %v", err)
	}
	// Act + Assert
	if err := p.SendRaw([]byte("{}\n")); err == nil {
		t.Fatal("SendRaw after CloseStdin should error")
	}
}

func TestCloseStdinIsIdempotent(t *testing.T) {
	// Arrange
	p := spawnScript(t, `cat >/dev/null`)
	// Act + Assert
	if err := p.CloseStdin(); err != nil {
		t.Fatalf("first CloseStdin: %v", err)
	}
	if err := p.CloseStdin(); err != nil {
		t.Fatalf("second CloseStdin: %v", err)
	}
}

func TestSpawnRejectsEmptyArgv(t *testing.T) {
	// Arrange + Act
	_, err := Spawn(Options{})
	// Assert
	if err == nil {
		t.Fatal("Spawn with empty argv should error")
	}
}

func TestSpawnRejectsNilLogger(t *testing.T) {
	_, err := Spawn(Options{Argv: []string{"/bin/true"}})
	if err == nil || !strings.Contains(err.Error(), "Logger is required") {
		t.Fatalf("Spawn nil logger error = %v, want required logger error", err)
	}
}

func TestPumpStderrRoutesNormalAndVerboseRecords(t *testing.T) {
	logger := &recordingLogger{}
	pumpStderr(strings.NewReader(
		canonicalShimRecord("normal")+"\n"+
			canonicalShimRecord("verbose")+"\n"+
			"{\"runtime\":\"shim\",\"verbosity\":\"normal\",\"message\":\"verbosity=verbose\"}\n"+
			"malformed verbosity=verbose\n",
	), logger, &stderrTail{})

	logger.record(t, 0, "normal", "\"verbosity\":\"normal\"")
	logger.record(t, 1, "verbose", "\"verbosity\":\"verbose\"")
	logger.record(t, 2, "normal", "shim stderr malformed")
	logger.record(t, 3, "normal", "malformed verbosity=verbose")
}

func TestPumpStderrMirrorsCanonicalShimRecordsWithoutDaemonPersistence(t *testing.T) {
	logger := &mirroringLogger{}
	pumpStderr(strings.NewReader(
		canonicalShimRecord("normal")+"\n"+
			canonicalShimRecord("verbose")+"\n"+
			"malformed\n",
	), logger, &stderrTail{})
	if len(logger.mirroredRecords()) != 2 || !strings.Contains(logger.mirroredRecords()[0], `"verbosity":"normal"`) || !strings.Contains(logger.mirroredRecords()[1], `"verbosity":"verbose"`) {
		t.Fatalf("mirrored=%q", logger.mirroredRecords())
	}
	if len(logger.logged()) != 1 || logger.logged()[0].level != "normal" || !strings.Contains(logger.logged()[0].line, "malformed") {
		t.Fatalf("daemon records=%#v", logger.logged())
	}
}

func TestShimRecordRejectsIncompleteOrIllTypedCanonicalEnvelope(t *testing.T) {
	for _, line := range []string{
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":"m","context":{},"pid":1,"workspace_dir":"/w","workspace_id":"1cce3e60","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00.123Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":"m","context":{},"pid":1,"workspace_dir":"/tmp/workspace","workspace_id":"deadbeef","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"trace","verbosity":"normal","operation":"op","message":"m","context":{},"pid":1,"workspace_dir":"/w","workspace_id":"id","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":{},"context":{},"pid":1,"workspace_dir":"/w","workspace_id":"id","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":"m","context":[],"pid":1,"workspace_dir":"/w","workspace_id":"id","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":"m","context":{},"pid":0,"workspace_dir":"/w","workspace_id":"id","agent_repl_session_id":"s"}`,
		`{"timestamp":"2026-07-28T12:00:00Z","runtime":"shim","level":"info","verbosity":"normal","operation":"op","message":"m","context":{},"pid":1,"workspace_dir":"","workspace_id":"id","agent_repl_session_id":"s"}`,
	} {
		if _, ok := shimRecord(line); ok {
			t.Fatalf("incomplete envelope accepted: %s", line)
		}
	}
}

func TestPumpStderrReportsScannerFailureAsDaemonError(t *testing.T) {
	logger := &recordingLogger{}
	pumpStderr(failingReader{}, logger, &stderrTail{})
	logger.record(t, 0, "normal", "stderr scan error")
}

func TestPumpStdoutLogsScanErrors(t *testing.T) {
	logger := &recordingLogger{}
	p := &Proc{events: make(chan *protocol.L1Event, 1)}
	p.pumpStdout(bytes.NewReader([]byte(strings.Repeat("x", maxEventLine+1))), logger)

	logger.record(t, 0, "normal", "stdout scan error")
	if _, ok := <-p.Events(); ok {
		t.Fatal("events channel remained open after stdout scan error")
	}
}

func TestWaitReportsExitError(t *testing.T) {
	// Arrange
	p, err := Spawn(Options{
		Argv:   []string{"/bin/sh", "-c", "exit 3"},
		Logger: &recordingLogger{},
	})
	if err != nil {
		t.Fatalf("Spawn: %v", err)
	}
	expectEventsClosed(t, p)
	// Act + Assert
	if err := p.Wait(); err == nil {
		t.Fatal("Wait should report the non-zero exit")
	}
}
