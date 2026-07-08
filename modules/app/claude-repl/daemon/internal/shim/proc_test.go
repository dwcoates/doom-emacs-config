package shim

import (
	"testing"
	"time"

	"claude-repld/internal/protocol"
)

const recvTimeout = 5 * time.Second

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
		Argv: []string{"/bin/sh", "-c", script},
		Logf: func(string, ...any) {},
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
	p := spawnScript(t, `echo 'this is not json'`)
	// Assert
	evt := recvEvent(t, p)
	if evt.Type != "error" || evt.Code != "transport" {
		t.Errorf("evt = %+v, want synthetic transport error", evt)
	}
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

func TestWaitReportsExitError(t *testing.T) {
	// Arrange
	p, err := Spawn(Options{
		Argv: []string{"/bin/sh", "-c", "exit 3"},
		Logf: func(string, ...any) {},
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
