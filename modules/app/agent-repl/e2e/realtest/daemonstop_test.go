//go:build realtest

package realtest

import (
	"context"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// stopDaemonServer is a daemon door that answers exactly one way. It is the
// generated handler, so the frame this harness sends is validated by the same
// schema the real daemon validates it with.
type stopDaemonServer struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	answer *agentreplv1.UpdateShutdownScheduleResponse
	seen   *agentreplv1.UpdateShutdownScheduleRequest
}

func (s *stopDaemonServer) UpdateShutdownSchedule(_ context.Context, req *connect.Request[agentreplv1.UpdateShutdownScheduleRequest]) (*connect.Response[agentreplv1.UpdateShutdownScheduleResponse], error) {
	s.seen = req.Msg
	return connect.NewResponse(s.answer), nil
}

// startStopDaemonServer serves one door over h2c and writes its address into a
// scratch state directory's daemon.addr, which is the only way the code under
// test ever learns where to dial.
func startStopDaemonServer(t *testing.T, answer *agentreplv1.UpdateShutdownScheduleResponse) (stateDir string, server *stopDaemonServer) {
	t.Helper()
	server = &stopDaemonServer{answer: answer}
	path, handler := agentreplv1connect.NewAgentReplHandler(server)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	srv := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(srv.Close)

	stateDir = t.TempDir()
	addr := strings.TrimPrefix(srv.URL, "http://")
	if err := os.WriteFile(DaemonAddrPath(stateDir), []byte(addr+"\npid=4242\n"), 0o600); err != nil {
		t.Fatalf("write the advertisement the harness reads: %v", err)
	}
	return stateDir, server
}

func TestDaemonStopAddressReadsTheAdvertisedAddress(t *testing.T) {
	// Arrange: an advertisement in the two-line form a daemon publishes.
	stateDir := t.TempDir()
	if err := os.WriteFile(DaemonAddrPath(stateDir), []byte("127.0.0.1:51515\npid=4242\n"), 0o600); err != nil {
		t.Fatalf("write the advertisement: %v", err)
	}

	// Act.
	addr, err := DaemonStopAddress(stateDir)

	// Assert: the address line, with the pid line ignored.
	if err != nil {
		t.Fatalf("DaemonStopAddress = %v, want the advertised address", err)
	}
	if addr != "127.0.0.1:51515" {
		t.Errorf("DaemonStopAddress = %q, want 127.0.0.1:51515", addr)
	}
}

func TestDaemonStopAddressRefusesAnAbsentAdvertisement(t *testing.T) {
	// Arrange: a state directory with no daemon.addr in it.
	stateDir := t.TempDir()

	// Act.
	_, err := DaemonStopAddress(stateDir)

	// Assert: the absence is an error naming the file, never an empty address.
	if err == nil {
		t.Fatal("DaemonStopAddress = nil error, want a refusal naming the missing advertisement")
	}
	if !strings.Contains(err.Error(), "daemon.addr") {
		t.Errorf("the error does not name the file it could not read: %v", err)
	}
}

func TestDaemonStopAddressRefusesAnAdvertisementNamingNoAddress(t *testing.T) {
	// Arrange: a file that exists and names nobody.
	stateDir := t.TempDir()
	if err := os.WriteFile(DaemonAddrPath(stateDir), []byte("\npid=4242\n"), 0o600); err != nil {
		t.Fatalf("write the advertisement: %v", err)
	}

	// Act.
	_, err := DaemonStopAddress(stateDir)

	// Assert.
	if err == nil {
		t.Fatal("DaemonStopAddress = nil error, want a refusal over an advertisement that names no address")
	}
	if !strings.Contains(err.Error(), "names no address") {
		t.Errorf("the error does not say what is wrong with the advertisement: %v", err)
	}
}

func TestStopDaemonOrderlyAcceptsTheSuccessArm(t *testing.T) {
	// Arrange: a door that accepts the immediate shutdown.
	stateDir, _ := startStopDaemonServer(t, &agentreplv1.UpdateShutdownScheduleResponse{
		Result: &agentreplv1.UpdateShutdownScheduleResponse_Success{Success: &agentreplv1.UpdateShutdownScheduleSuccess{}},
	})

	// Act.
	err := StopDaemonOrderly(context.Background(), stateDir, func(string) {})

	// Assert.
	if err != nil {
		t.Fatalf("StopDaemonOrderly = %v, want the accepted stop reported as success", err)
	}
}

func TestStopDaemonOrderlySendsTheImmediateArm(t *testing.T) {
	// Arrange: a door that records what it was asked.
	stateDir, server := startStopDaemonServer(t, &agentreplv1.UpdateShutdownScheduleResponse{
		Result: &agentreplv1.UpdateShutdownScheduleResponse_Success{Success: &agentreplv1.UpdateShutdownScheduleSuccess{}},
	})

	// Act.
	if err := StopDaemonOrderly(context.Background(), stateDir, func(string) {}); err != nil {
		t.Fatalf("StopDaemonOrderly = %v, want the stop accepted", err)
	}

	// Assert: `now`, not a schedule — a scheduled drain would leave the daemon
	// serving and the sweep waiting on a stop that never came.
	if server.seen.GetNow() == nil {
		t.Fatalf("the daemon was asked for %T, want UpdateShutdownScheduleNow", server.seen.GetAction())
	}
}

func TestStopDaemonOrderlyNamesTheHarnessAsTheOperator(t *testing.T) {
	// Arrange.
	stateDir, server := startStopDaemonServer(t, &agentreplv1.UpdateShutdownScheduleResponse{
		Result: &agentreplv1.UpdateShutdownScheduleResponse_Success{Success: &agentreplv1.UpdateShutdownScheduleSuccess{}},
	})

	// Act.
	if err := StopDaemonOrderly(context.Background(), stateDir, func(string) {}); err != nil {
		t.Fatalf("StopDaemonOrderly = %v, want the stop accepted", err)
	}

	// Assert: the reason the daemon announces names who asked, and it is not
	// the editor's "emacs".
	if got := server.seen.GetNow().GetReason().GetOperator().GetNote(); got != DaemonStopNote {
		t.Errorf("the operator note was %q, want %q", got, DaemonStopNote)
	}
}

func TestStopDaemonOrderlyReportsARefusal(t *testing.T) {
	// Arrange: a door that refuses.
	stateDir, _ := startStopDaemonServer(t, &agentreplv1.UpdateShutdownScheduleResponse{
		Result: &agentreplv1.UpdateShutdownScheduleResponse_Error{Error: &agentreplv1.UpdateShutdownScheduleError{}},
	})

	// Act.
	err := StopDaemonOrderly(context.Background(), stateDir, func(string) {})

	// Assert: a refusal is surfaced, never read as an acceptance.
	if err == nil {
		t.Fatal("StopDaemonOrderly = nil error, want the daemon's refusal surfaced")
	}
	if !strings.Contains(err.Error(), "refused") {
		t.Errorf("the error does not say the daemon refused: %v", err)
	}
}

func TestStopDaemonOrderlyReportsAnUnreadableArm(t *testing.T) {
	// Arrange: an answer with no arm set at all.
	stateDir, _ := startStopDaemonServer(t, &agentreplv1.UpdateShutdownScheduleResponse{})

	// Act.
	err := StopDaemonOrderly(context.Background(), stateDir, func(string) {})

	// Assert: an answer this harness cannot read is not an acceptance.
	if err == nil {
		t.Fatal("StopDaemonOrderly = nil error, want an unreadable answer refused")
	}
	if !strings.Contains(err.Error(), "cannot read") {
		t.Errorf("the error does not say the answer was unreadable: %v", err)
	}
}

func TestStopDaemonOrderlyReportsATransportFailure(t *testing.T) {
	// Arrange: an advertisement naming a port nothing is listening on.
	stateDir := t.TempDir()
	if err := os.WriteFile(DaemonAddrPath(stateDir), []byte("127.0.0.1:1\npid=4242\n"), 0o600); err != nil {
		t.Fatalf("write the advertisement: %v", err)
	}

	// Act.
	err := StopDaemonOrderly(context.Background(), stateDir, func(string) {})

	// Assert: the unreachable door is an error, which is the sweep's cue to
	// state the fallback rather than take it silently.
	if err == nil {
		t.Fatal("StopDaemonOrderly = nil error, want the unreachable door reported")
	}
	if !strings.Contains(err.Error(), "UpdateShutdownSchedule{now}") {
		t.Errorf("the error does not name the call that could not be made: %v", err)
	}
}

func TestDaemonAddrPathIsUnderTheStateRoot(t *testing.T) {
	// Arrange.
	stateDir := "/somewhere/.claude-emacs"

	// Act.
	got := DaemonAddrPath(stateDir)

	// Assert.
	if want := filepath.Join(stateDir, "daemon.addr"); got != want {
		t.Errorf("DaemonAddrPath = %q, want %q", got, want)
	}
}
