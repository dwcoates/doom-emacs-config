package daemonclient

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

type clientLogServer struct {
	response       *agentreplv1.ClientLogResponse
	workspace      *workspacev1.WorkspaceRef
	request        *agentreplv1.ClientLogRequest
	rosterRequests int
}

func serveClientLog(t *testing.T, response *agentreplv1.ClientLogResponse) (*Client, *clientLogServer) {
	t.Helper()
	stateDir := t.TempDir()
	workspaceDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize fake daemon workspace: %v", err)
	}
	recorder := &clientLogServer{
		response: response,
		workspace: &workspacev1.WorkspaceRef{
			Id: "daemon-workspace-id", Dir: workspaceDir,
		},
	}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
			func(_ context.Context, _ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest], stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
				recorder.rosterRequests++
				return stream.Send(rosterResponse(recorder.workspace))
			}))
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure,
			func(_ context.Context, req *connect.Request[agentreplv1.ClientLogRequest]) (*connect.Response[agentreplv1.ClientLogResponse], error) {
				recorder.request = proto.Clone(req.Msg).(*agentreplv1.ClientLogRequest)
				return connect.NewResponse(recorder.response), nil
			}))
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake daemon: %v", err)
	}
	server := &http.Server{Handler: mux}
	go func() { _ = server.Serve(listener) }()
	t.Cleanup(func() {
		_ = server.Shutdown(context.Background())
	})
	// The daemon publishes the address on the first line and its pid on the
	// second; the forwarder must resolve the address from that shape.
	advertisement := listener.Addr().String() + "\npid=" + strconv.Itoa(os.Getpid()) + "\n"
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte(advertisement), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	return New(stateDir), recorder
}

func TestForwardSendsACompleteSidecarClientLogRequest(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})
	record := logging.ForwardRecord{
		Timestamp: "2026-09-10T12:34:56.789000-04:00", PID: 4242,
		Level: "warn", Verbose: true, Operation: "sidecar.tail.read",
		Message: "the transcript could not be decoded", WorkspaceDir: server.workspace.GetDir(),
		WorkspaceID: "deadbeef", ClaudeSessionID: "claude-1",
		Context: map[string]any{
			"path": "/work/repo/session.jsonl", "pid": 4242.0,
			"write_ids": []string{"write-1", "write-2"},
		},
	}

	// Act.
	_, err := client.Forward(record)

	// Assert.
	if err != nil {
		t.Fatalf("Forward returned %v", err)
	}
	got := server.request
	if got.GetWorkspace().GetId() != "daemon-workspace-id" || got.GetWorkspace().GetDir() != record.WorkspaceDir {
		t.Fatalf("workspace ref = %v, want the daemon-minted complete ref", got.GetWorkspace())
	}
	if got.GetRecord().GetSidecar() == nil || got.GetRecord().GetWarn() == nil {
		t.Fatalf("record arms = runtime %T level %T, want sidecar/warn", got.GetRecord().GetRuntime(), got.GetRecord().GetLevel())
	}
	if got.GetRecord().GetTimestamp() != record.Timestamp || !got.GetRecord().GetVerbose() {
		t.Fatalf("record clock/class = %q/%t, want %q/true", got.GetRecord().GetTimestamp(), got.GetRecord().GetVerbose(), record.Timestamp)
	}
	if got.GetRecord().GetContext().AsMap()["path"] != record.Context["path"] {
		t.Fatalf("record context = %v, want path preserved", got.GetRecord().GetContext().AsMap())
	}
	writeIDs := got.GetRecord().GetContext().GetFields()["write_ids"].GetListValue().GetValues()
	if len(writeIDs) != 2 || writeIDs[0].GetStringValue() != "write-1" || writeIDs[1].GetStringValue() != "write-2" {
		t.Fatalf("record context.write_ids = %v, want the complete typed string list", writeIDs)
	}
}

func TestForwardCachesTheRosterRefForTheSameDaemonAndWorkspace(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})
	// The cache is only consulted INSIDE the freshness bound, so the bound is
	// injected rather than raced: a machine slow enough to spend a second
	// between two loopback calls would otherwise read the roster twice.
	onFakeClock(client, newFakeClock())
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("first Forward returned %v", err)
	}
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("second Forward returned %v", err)
	}

	// Assert.
	if server.rosterRequests != 1 {
		t.Fatalf("WatchWorkspaceRoster requests = %d, want one cached lookup", server.rosterRequests)
	}
}

func TestNormalizeWorkspaceDirResolvesTheDeepestExistingAncestor(t *testing.T) {
	// Arrange.
	base := t.TempDir()
	requested := filepath.Join(base, "deleted", "workspace")
	resolvedBase, err := filepath.EvalSymlinks(base)
	if err != nil {
		t.Fatalf("resolve fixture base: %v", err)
	}

	// Act.
	got, err := normalizeWorkspaceDir(requested)

	// Assert.
	if err != nil {
		t.Fatalf("normalizeWorkspaceDir returned %v", err)
	}
	want := filepath.Join(resolvedBase, "deleted", "workspace")
	if got != want {
		t.Fatalf("normalizeWorkspaceDir = %q, want %q", got, want)
	}
}

func TestForwardRejectsANonLoopbackDaemonAddress(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte("192.0.2.10:8123\n"), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}

	// Act.
	address, err := New(stateDir).Forward(logging.ForwardRecord{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not a loopback IP") {
		t.Fatalf("Forward error = %v, want a loopback refusal", err)
	}
	if address != "192.0.2.10:8123" {
		t.Fatalf("failure address = %q, want the refused daemon address", address)
	}
}

func TestForwardRefusesAResponseWithNoResultArm(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	_, err := client.Forward(record)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "neither success nor error") {
		t.Fatalf("Forward error = %v, want an unset-result refusal", err)
	}
}

func TestForwardInvalidatesTheRosterRefAfterClientLogRefusal(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Error{Error: &agentreplv1.ClientLogError{}},
	})
	record := logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	}

	// Act.
	if _, err := client.Forward(record); err == nil {
		t.Fatal("first Forward succeeded, want the fake daemon's refusal")
	}
	server.response = &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	}
	if _, err := client.Forward(record); err != nil {
		t.Fatalf("second Forward returned %v", err)
	}

	// Assert.
	if server.rosterRequests != 2 {
		t.Fatalf("WatchWorkspaceRoster requests = %d, want the refused ref resolved again", server.rosterRequests)
	}
}

// TestForwardConcludesUnresolvableOnTheUnknownWorkspaceArm pins the departure
// RACE: the roster named the workspace when the ref was resolved, and the
// daemon had forgotten it by the time the record landed. The refusal reaches
// the same conclusion the roster path reaches when the row is already gone, so
// forwardLoop stops retrying for it, narrates at DEBUG, and persists the record
// unattributed centrally rather than losing it.
func TestForwardConcludesUnresolvableOnTheUnknownWorkspaceArm(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Error{Error: &agentreplv1.ClientLogError{
			Cause: &agentreplv1.ClientLogError_UnknownWorkspace{
				UnknownWorkspace: &agentreplv1.ClientLogUnknownWorkspace{},
			},
		}},
	})

	// Act.
	_, err := client.Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	})

	// Assert.
	if !errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("Forward against a departed workspace = %v, want ErrForwardWorkspaceUnresolvable", err)
	}
}

// TestForwardKeepsAnArmlessRefusalARealFailure pins the other side of that
// fork: a refusal that is NOT unknown_workspace is still a genuine failure, so
// the forwarder retries it rather than concluding the workspace departed.
func TestForwardKeepsAnArmlessRefusalARealFailure(t *testing.T) {
	// Arrange.
	client, server := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Error{Error: &agentreplv1.ClientLogError{}},
	})

	// Act.
	_, err := client.Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: server.workspace.GetDir(), WorkspaceID: "deadbeef",
	})

	// Assert.
	if err == nil {
		t.Fatal("Forward against an armless refusal succeeded, want a failure")
	}
	if errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("Forward against an armless refusal = %v, want an ordinary failure", err)
	}
}

// startFakeDaemon stands up a loopback AgentRepl whose roster stream runs the
// supplied handler, publishes its address in daemon.addr, and records whether
// ClientLog was ever reached. It is the seam for exercising resolveWorkspace's
// "roster delivered, dir absent" vs "roster stream errored" fork without the
// canned single-send handler serveClientLog uses.
func startFakeDaemon(
	t *testing.T,
	roster func(ctx context.Context, stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error,
) (client *Client, clientLogReached *bool) {
	t.Helper()
	stateDir := t.TempDir()
	reached := new(bool)
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
			func(ctx context.Context, _ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest], stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
				return roster(ctx, stream)
			}))
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure,
			func(_ context.Context, _ *connect.Request[agentreplv1.ClientLogRequest]) (*connect.Response[agentreplv1.ClientLogResponse], error) {
				*reached = true
				return connect.NewResponse(&agentreplv1.ClientLogResponse{
					Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
				}), nil
			}))
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake daemon: %v", err)
	}
	server := &http.Server{Handler: mux}
	go func() { _ = server.Serve(listener) }()
	t.Cleanup(func() { _ = server.Shutdown(context.Background()) })
	writeAdvertisement(t, stateDir, listener.Addr().String(), os.Getpid())
	return New(stateDir), reached
}

// A workspace absent from a HEALTHY, fully-delivered roster is UNRESOLVABLE,
// concluded the instant the current snapshot arrives -- not waited out to the
// request deadline. The roster handler delivers a snapshot naming a DIFFERENT
// workspace and then holds the standing stream open (as the real watch does);
// Forward must return promptly with ErrForwardWorkspaceUnresolvable and must
// never reach ClientLog, because there is no ref to attribute the record to.
func TestForwardConcludesUnresolvableWhenDirAbsentFromDeliveredRoster(t *testing.T) {
	// Arrange: the roster names some other workspace, never the record's dir.
	otherDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize the roster's workspace: %v", err)
	}
	recordDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize the record's workspace: %v", err)
	}
	client, clientLogReached := startFakeDaemon(t, func(ctx context.Context, stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
		if err := stream.Send(rosterResponse(&workspacev1.WorkspaceRef{Id: "other-id", Dir: otherDir})); err != nil {
			return err
		}
		// Hold the standing stream open exactly as the real watch does, so a
		// resolver that failed to conclude on the delivered snapshot would
		// block to its deadline rather than return promptly.
		<-ctx.Done()
		return ctx.Err()
	})

	// Act.
	_, err = client.Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: recordDir, WorkspaceID: "deadbeef",
	})

	// Assert.
	if !errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("Forward against an absent-from-roster dir = %v, want ErrForwardWorkspaceUnresolvable", err)
	}
	if *clientLogReached {
		t.Fatal("ClientLog was reached for an unresolvable workspace, want it never attempted")
	}
}

// A roster stream whose FIRST frame is the daemon's planned ending delivered
// no roster at all: the daemon is standing down. That says nothing about the
// dir, so it is the restart transient, never an unresolvable workspace.
func TestForwardTreatsAPlannedEndingBeforeAnyRosterAsTheDaemonLeaving(t *testing.T) {
	// Arrange: the roster handler's first and last frame is the planned ending.
	recordDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize the record's workspace: %v", err)
	}
	client, clientLogReached := startFakeDaemon(t, func(_ context.Context, stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
		return stream.Send(&agentreplv1.WatchWorkspaceRosterResponse{
			Push: &agentreplv1.WatchWorkspaceRosterResponse_Ending{Ending: &agentreplv1.DaemonStreamEnding{}},
		})
	})

	// Act.
	_, err = client.Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: recordDir, WorkspaceID: "deadbeef",
	})

	// Assert.
	if errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("a planned ending before any roster = %v, want it OFF the unresolvable path", err)
	}
	if !errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("a planned ending before any roster = %v, want the daemon-leaving transient", err)
	}
	if *clientLogReached {
		t.Fatal("ClientLog was reached with no roster delivered, want it never attempted")
	}
}

// A roster stream that ERRORS before delivering any snapshot is a TRANSPORT
// failure, not an unresolvable workspace: the daemon could be booting or gone,
// and the pid/boot sentinels -- not the unresolvable one -- must classify it.
func TestForwardTakesTransportPathWhenRosterStreamErrors(t *testing.T) {
	// Arrange: the roster handler fails the stream with Unavailable before ever
	// sending a snapshot, the connect-go shape of a dial/connection fault.
	recordDir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize the record's workspace: %v", err)
	}
	client, clientLogReached := startFakeDaemon(t, func(_ context.Context, _ *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
		return connect.NewError(connect.CodeUnavailable, errors.New("the roster topic is not ready"))
	})

	// Act.
	_, err = client.Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", Message: "read",
		WorkspaceDir: recordDir, WorkspaceID: "deadbeef",
	})

	// Assert: it is classified on the transport path (a never-served, live-pid
	// address is a boot transient), never as an unresolvable workspace.
	if errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("a roster-stream transport error = %v, want it OFF the unresolvable path", err)
	}
	if !errors.Is(err, logging.ErrForwardTargetBooting) {
		t.Fatalf("a roster-stream Unavailable against a never-served address = %v, want the transport boot transient", err)
	}
	if *clientLogReached {
		t.Fatal("ClientLog was reached after the roster stream errored, want it never attempted")
	}
}

func rosterResponse(ref *workspacev1.WorkspaceRef) *agentreplv1.WatchWorkspaceRosterResponse {
	return &agentreplv1.WatchWorkspaceRosterResponse{Push: &agentreplv1.WatchWorkspaceRosterResponse_Roster{Roster: &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{
			Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{{
				Workspace: &frontendv1.RosterRowWorkspace{Workspace: copyWorkspaceRef(ref)},
			}}},
		}}},
	}}}
}

func TestAddressLineTakesTheFirstLineOfTheAdvertisement(t *testing.T) {
	tests := []struct {
		name string
		raw  string
		want string
	}{
		{name: "legacy bare address", raw: "127.0.0.1:41234\n", want: "127.0.0.1:41234"},
		{name: "address with a pid line", raw: "127.0.0.1:41234\npid=4242\n", want: "127.0.0.1:41234"},
		{name: "no trailing newline", raw: "127.0.0.1:9", want: "127.0.0.1:9"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := addressLine(tc.raw)

			// Assert.
			if got != tc.want {
				t.Fatalf("addressLine(%q) = %q, want %q", tc.raw, got, tc.want)
			}
		})
	}
}

func TestReadyReportsALivePublishedDaemon(t *testing.T) {
	// Arrange: serveClientLog publishes daemon.addr and starts a listener.
	client, _ := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})

	// Act.
	address, ready := client.Ready()

	// Assert.
	if !ready {
		t.Fatalf("Ready = false for a published, listening daemon (%q)", address)
	}
	if err := validateAddress(address); err != nil {
		t.Fatalf("Ready address %q is not a resolved loopback address: %v", address, err)
	}
}

func TestReadyReportsNotYetServingWhenTheAddressIsUnpublished(t *testing.T) {
	// Arrange: a state root with no daemon.addr yet.
	stateDir := t.TempDir()

	// Act.
	address, ready := New(stateDir).Ready()

	// Assert.
	if ready {
		t.Fatalf("Ready = true before daemon.addr was published")
	}
	if address != filepath.Join(stateDir, "daemon.addr") {
		t.Fatalf("probe address = %q, want the daemon.addr path fallback", address)
	}
}

func TestReadyReportsNotYetServingWhenTheListenerIsNotAccepting(t *testing.T) {
	// Arrange: publish an address whose listener has been closed, so the port
	// is published but nothing accepts — the booting-daemon shape.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("reserve a port: %v", err)
	}
	address := listener.Addr().String()
	if err := listener.Close(); err != nil {
		t.Fatalf("close the reserved listener: %v", err)
	}
	stateDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte(address+"\n"), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}

	// Act.
	probed, ready := New(stateDir).Ready()

	// Assert.
	if ready {
		t.Fatalf("Ready = true for a published address with no listener (%q)", probed)
	}
	if probed != address {
		t.Fatalf("probe address = %q, want the published address %q", probed, address)
	}
}

func TestReadyRejectsANonLoopbackDaemonAddress(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	if err := os.WriteFile(filepath.Join(stateDir, "daemon.addr"), []byte("192.0.2.10:8123\n"), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}

	// Act.
	_, ready := New(stateDir).Ready()

	// Assert.
	if ready {
		t.Fatalf("Ready = true for a non-loopback daemon address")
	}
}

// deadPID spawns a trivial child process and waits for it to exit, reaping
// it. The returned pid then names no live process for the rest of the test —
// unlike an arbitrary large number, which risks colliding with a real process
// on a loaded machine, cmd.Wait's reap is a deterministic "this pid is gone".
func deadPID(t *testing.T) int {
	t.Helper()
	cmd := exec.Command("/bin/sh", "-c", "true")
	if err := cmd.Run(); err != nil {
		t.Fatalf("run a throwaway child to reap: %v", err)
	}
	return cmd.Process.Pid
}

// writeAdvertisement writes a daemon.addr payload with an optional pid line,
// mirroring the on-disk shape daemonaddr.Publish writes.
func writeAdvertisement(t *testing.T, dir, address string, pid int) {
	t.Helper()
	payload := address + "\npid=" + strconv.Itoa(pid) + "\n"
	if err := os.WriteFile(filepath.Join(dir, "daemon.addr"), []byte(payload), 0o600); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
}

// dialUnavailable is the connect-go shape a real dial/connection-refused
// failure takes: client.go's own transport wraps such a failure as
// CodeUnavailable (see connectrpc.com/connect's client.go and
// duplex_http_call.go), which is exactly the class classifyForward acts on.
func dialUnavailable() error {
	return connect.NewError(connect.CodeUnavailable, errors.New("dial tcp 127.0.0.1:9: connect: connection refused"))
}

func TestClassifyForwardMarksADeadAdvertiserPidAsTargetNotThere(t *testing.T) {
	// Arrange: daemon.addr still names the pid this attempt dialed, but that
	// pid is now dead — the daemon exited or was replaced without withdrawing.
	stateDir := t.TempDir()
	pid := deadPID(t)
	writeAdvertisement(t, stateDir, "127.0.0.1:9999", pid)
	client := New(stateDir)

	// Act.
	err := client.classifyForward(dialUnavailable(), "127.0.0.1:9999", pid, true)

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward against a dead advertiser pid = %v, want ErrForwardTargetNotThere", err)
	}
}

func TestClassifyForwardMarksAChangedAdvertisementAsTargetNotThere(t *testing.T) {
	// Arrange: a replacement daemon published a DIFFERENT address after this
	// attempt started dialing the old one.
	stateDir := t.TempDir()
	writeAdvertisement(t, stateDir, "127.0.0.1:8888", os.Getpid())
	client := New(stateDir)

	// Act.
	err := client.classifyForward(dialUnavailable(), "127.0.0.1:9999", os.Getpid(), true)

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward against a changed advertisement = %v, want ErrForwardTargetNotThere", err)
	}
}

func TestClassifyForwardMarksAVanishedAdvertisementAsTargetNotThere(t *testing.T) {
	// Arrange: daemon.addr itself is gone by the time the failure is
	// classified — the "addr file is absent ... since the attempt began" half
	// of the invariant.
	stateDir := t.TempDir()
	client := New(stateDir)

	// Act.
	err := client.classifyForward(dialUnavailable(), "127.0.0.1:9999", os.Getpid(), true)

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward against a vanished advertisement = %v, want ErrForwardTargetNotThere", err)
	}
}

func TestClassifyForwardKeepsAnAliveUnreachableTargetAsARealFailure(t *testing.T) {
	// Arrange: daemon.addr still names the exact address and a still-live pid
	// this attempt dialed — a genuinely stuck daemon, not a restart — and the
	// address WAS previously seen accepting, so the boot-tolerance check must
	// not demote this to a transient either.
	stateDir := t.TempDir()
	pid := os.Getpid()
	writeAdvertisement(t, stateDir, "127.0.0.1:9999", pid)
	client := New(stateDir)
	client.markServing("127.0.0.1:9999")
	dialErr := dialUnavailable()

	// Act.
	err := client.classifyForward(dialErr, "127.0.0.1:9999", pid, true)

	// Assert.
	if errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward against a live, unchanged advertiser = %v, want the failure left unchanged", err)
	}
	if errors.Is(err, logging.ErrForwardTargetBooting) {
		t.Fatalf("classifyForward against a previously-serving address = %v, want the failure left unchanged", err)
	}
	if !errors.Is(err, dialErr) {
		t.Fatalf("classifyForward changed the underlying error: got %v, want %v unchanged", err, dialErr)
	}
}

// TestClassifyForwardMarksANeverServedAddressAsBooting is the per-address
// boot-tolerance invariant: an address whose advertisement is unchanged and
// whose pid is alive, but which this Client has NEVER seen accepting, is a
// startup transient — not a stuck daemon — even when a wholly different
// address was seen serving earlier (the realtest 1 shape: daemon A was seen
// serving, daemon B's boot window then refuses).
func TestClassifyForwardMarksANeverServedAddressAsBooting(t *testing.T) {
	// Arrange: daemon.addr names the exact address and a live pid this attempt
	// dialed, but this address has never been marked as seen serving.
	stateDir := t.TempDir()
	pid := os.Getpid()
	writeAdvertisement(t, stateDir, "127.0.0.1:9999", pid)
	client := New(stateDir)
	// A different address was seen serving earlier; it must not vouch for the
	// one this attempt dialed.
	client.markServing("127.0.0.1:8888")

	// Act.
	err := client.classifyForward(dialUnavailable(), "127.0.0.1:9999", pid, true)

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetBooting) {
		t.Fatalf("classifyForward against a never-served address = %v, want ErrForwardTargetBooting", err)
	}
	if errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward against a never-served, live-pid address = %v, want it left OUT of the gone sentinel", err)
	}
}

// TestReadyMarksItsAddressAsSeenServing exercises Ready() itself as the
// serving-witness path: a subsequent classifyForward against the same address
// must then treat a failure there as a real WARN candidate, not a boot
// transient.
func TestReadyMarksItsAddressAsSeenServing(t *testing.T) {
	// Arrange: serveClientLog starts a real listener and publishes its address.
	client, _ := serveClientLog(t, &agentreplv1.ClientLogResponse{
		Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
	})
	address, ready := client.Ready()
	if !ready {
		t.Fatalf("Ready = false, want the fake daemon to answer")
	}

	// Act.
	err := client.classifyForward(dialUnavailable(), address, os.Getpid(), true)

	// Assert.
	if errors.Is(err, logging.ErrForwardTargetBooting) {
		t.Fatalf("classifyForward after a successful Ready probe = %v, want the address treated as previously served", err)
	}
}

func TestClassifyForwardIgnoresNonDialFailures(t *testing.T) {
	// Arrange: an app-level refusal, not a connection/dial failure, against a
	// dead pid. Only connection/dial failures earn the not-there check.
	stateDir := t.TempDir()
	pid := deadPID(t)
	writeAdvertisement(t, stateDir, "127.0.0.1:9999", pid)
	client := New(stateDir)
	appErr := connect.NewError(connect.CodeInvalidArgument, errors.New("bad request"))

	// Act.
	err := client.classifyForward(appErr, "127.0.0.1:9999", pid, true)

	// Assert.
	if errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("classifyForward reclassified a non-dial failure against a dead pid = %v, want it left unchanged", err)
	}
	if err != appErr {
		t.Fatalf("classifyForward changed a non-dial error: got %v, want %v", err, appErr)
	}
}

// TestForwardMarksADeadAdvertiserAsTargetNotThereEndToEnd exercises the whole
// Forward path — not just classifyForward directly — against a daemon.addr
// whose listener is closed (nothing answers) and whose advertised pid is
// dead: the shape a realtest hit when the deployed daemon died and a
// persistent sidecar kept retrying its stale advertisement.
func TestForwardMarksADeadAdvertiserAsTargetNotThereEndToEnd(t *testing.T) {
	// Arrange.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("reserve a port: %v", err)
	}
	address := listener.Addr().String()
	if err := listener.Close(); err != nil {
		t.Fatalf("close the reserved listener: %v", err)
	}
	stateDir := t.TempDir()
	pid := deadPID(t)
	writeAdvertisement(t, stateDir, address, pid)

	// Act.
	_, err = New(stateDir).Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", WorkspaceDir: t.TempDir(), WorkspaceID: "deadbeef",
	})

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("Forward against a dead advertiser = %v, want ErrForwardTargetNotThere", err)
	}
}

// TestForwardMarksANeverServedAddressAsBootingEndToEnd exercises the whole
// Forward path against a daemon.addr whose listener is closed (connection
// refused) and whose advertised pid IS alive: the realtest 1 shape, where
// daemon B publishes its address and a live pid before its listener answers.
// A fresh Client — one that has never seen this address accept — must treat
// the refusal as a boot transient, not a WARN.
func TestForwardMarksANeverServedAddressAsBootingEndToEnd(t *testing.T) {
	// Arrange.
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("reserve a port: %v", err)
	}
	address := listener.Addr().String()
	if err := listener.Close(); err != nil {
		t.Fatalf("close the reserved listener: %v", err)
	}
	stateDir := t.TempDir()
	writeAdvertisement(t, stateDir, address, os.Getpid())

	// Act.
	_, err = New(stateDir).Forward(logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.read", WorkspaceDir: t.TempDir(), WorkspaceID: "deadbeef",
	})

	// Assert.
	if !errors.Is(err, logging.ErrForwardTargetBooting) {
		t.Fatalf("Forward against a never-served, live-pid address = %v, want ErrForwardTargetBooting", err)
	}
	if errors.Is(err, logging.ErrForwardTargetNotThere) {
		t.Fatalf("Forward against a never-served, live-pid address = %v, want it left OUT of the gone sentinel", err)
	}
}

func TestParseAdvertisementReadsTheAddressAndPid(t *testing.T) {
	tests := []struct {
		name         string
		raw          string
		wantAddress  string
		wantPID      int
		wantPIDKnown bool
	}{
		{name: "legacy bare address", raw: "127.0.0.1:41234\n", wantAddress: "127.0.0.1:41234"},
		{name: "address with a pid line", raw: "127.0.0.1:41234\npid=4242\n", wantAddress: "127.0.0.1:41234", wantPID: 4242, wantPIDKnown: true},
		{name: "malformed pid line", raw: "127.0.0.1:41234\npid=not-a-number\n", wantAddress: "127.0.0.1:41234"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			address, pid, pidKnown := parseAdvertisement(tc.raw)

			// Assert.
			if address != tc.wantAddress || pid != tc.wantPID || pidKnown != tc.wantPIDKnown {
				t.Fatalf("parseAdvertisement(%q) = (%q, %d, %t), want (%q, %d, %t)",
					tc.raw, address, pid, pidKnown, tc.wantAddress, tc.wantPID, tc.wantPIDKnown)
			}
		})
	}
}

func TestProcessAliveReportsTheOwnProcessAliveAndAReapedChildDead(t *testing.T) {
	if processAlive(deadPID(t)) {
		t.Fatal("processAlive(deadPID) = true, want a reaped child to read as dead")
	}
	if !processAlive(os.Getpid()) {
		t.Fatal("processAlive(os.Getpid()) = false, want the running test process to read as alive")
	}
}

// --- the roster moves under a cached ref ------------------------------------
//
// A DIRECTORY'S WORKSPACE REF IS ONLY EVER USED WHILE THE ROSTER STILL HOLDS
// IT. Realtest 9, sweep rt-run39: realtest 7 registered a scratch directory as
// workspace af24557b1ddd4b9c and forgot it, realtest 9 registered THE SAME
// directory as 54578ede3d834dea half a minute later, and the sidecar forwarded
// that directory's whole session against the dead id — refused, and written
// unattributed. The cases below are the four edges of the invariant that
// closed it.

// fakeClock is the injected time source every freshness assertion below is
// decided by, so a bound is STATED rather than waited out.
type fakeClock struct {
	mu sync.Mutex
	at time.Time
}

func newFakeClock() *fakeClock {
	return &fakeClock{at: time.Date(2026, 9, 14, 0, 12, 0, 0, time.UTC)}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.at
}

func (c *fakeClock) Advance(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.at = c.at.Add(d)
}

// rosterDaemon is a fake daemon whose roster CONTENTS and ClientLog verdict
// both change between calls, which is the whole shape a re-registration takes:
// the same directory, a new id, and a refusal for the id that just died.
type rosterDaemon struct {
	mu sync.Mutex
	// refs is the roster's current snapshot, replayed whole to every fresh
	// subscriber exactly as the daemon's roster topic does.
	refs []*workspacev1.WorkspaceRef
	// unknown is the set of workspace ids ClientLog answers unknown_workspace
	// for; every other id succeeds.
	unknown        map[string]struct{}
	rosterRequests int
	requests       []*agentreplv1.ClientLogRequest
}

func (d *rosterDaemon) setRoster(refs ...*workspacev1.WorkspaceRef) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.refs = refs
}

func (d *rosterDaemon) refuse(ids ...string) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.unknown = map[string]struct{}{}
	for _, id := range ids {
		d.unknown[id] = struct{}{}
	}
}

func (d *rosterDaemon) rosterReads() int {
	d.mu.Lock()
	defer d.mu.Unlock()
	return d.rosterRequests
}

func (d *rosterDaemon) lastRequest(t *testing.T) *agentreplv1.ClientLogRequest {
	t.Helper()
	d.mu.Lock()
	defer d.mu.Unlock()
	if len(d.requests) == 0 {
		t.Fatal("the fake daemon received no ClientLog request")
	}
	return d.requests[len(d.requests)-1]
}

// startRosterDaemon stands one rosterDaemon up on loopback and publishes its
// address into stateDir, so a test can point one Client at two daemons in turn.
func startRosterDaemon(t *testing.T, stateDir string) *rosterDaemon {
	t.Helper()
	daemon := &rosterDaemon{}
	mux := http.NewServeMux()
	mux.Handle(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
		connect.NewServerStreamHandler(agentreplv1connect.AgentReplWatchWorkspaceRosterProcedure,
			func(_ context.Context, _ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest], stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
				daemon.mu.Lock()
				daemon.rosterRequests++
				refs := append([]*workspacev1.WorkspaceRef(nil), daemon.refs...)
				daemon.mu.Unlock()
				return stream.Send(rosterResponseFor(refs))
			}))
	mux.Handle(agentreplv1connect.AgentReplClientLogProcedure,
		connect.NewUnaryHandler(agentreplv1connect.AgentReplClientLogProcedure,
			func(_ context.Context, req *connect.Request[agentreplv1.ClientLogRequest]) (*connect.Response[agentreplv1.ClientLogResponse], error) {
				daemon.mu.Lock()
				daemon.requests = append(daemon.requests, proto.Clone(req.Msg).(*agentreplv1.ClientLogRequest))
				_, refused := daemon.unknown[req.Msg.GetWorkspace().GetId()]
				daemon.mu.Unlock()
				if refused {
					return connect.NewResponse(&agentreplv1.ClientLogResponse{
						Result: &agentreplv1.ClientLogResponse_Error{Error: &agentreplv1.ClientLogError{
							Cause: &agentreplv1.ClientLogError_UnknownWorkspace{
								UnknownWorkspace: &agentreplv1.ClientLogUnknownWorkspace{},
							},
						}},
					}), nil
				}
				return connect.NewResponse(&agentreplv1.ClientLogResponse{
					Result: &agentreplv1.ClientLogResponse_Success{Success: &agentreplv1.ClientLogSuccess{}},
				}), nil
			}))
	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatalf("listen for fake daemon: %v", err)
	}
	server := &http.Server{Handler: mux}
	go func() { _ = server.Serve(listener) }()
	t.Cleanup(func() { _ = server.Shutdown(context.Background()) })
	writeAdvertisement(t, stateDir, listener.Addr().String(), os.Getpid())
	return daemon
}

// rosterResponseFor replays a whole snapshot, which is what the roster topic
// hands a fresh subscriber: one row per registered workspace, or none at all.
func rosterResponseFor(refs []*workspacev1.WorkspaceRef) *agentreplv1.WatchWorkspaceRosterResponse {
	rows := make([]*frontendv1.RosterRow, 0, len(refs))
	for _, ref := range refs {
		rows = append(rows, &frontendv1.RosterRow{
			Workspace: &frontendv1.RosterRowWorkspace{Workspace: copyWorkspaceRef(ref)},
		})
	}
	return &agentreplv1.WatchWorkspaceRosterResponse{Push: &agentreplv1.WatchWorkspaceRosterResponse_Roster{Roster: &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{
			Rows: &frontendv1.RosterRows{Rows: rows},
		}}},
	}}}
}

// tempWorkspaceDir is a directory spelled the way the roster spells it, so a
// macOS /var -> /private/var symlink cannot make a match look like a miss.
func tempWorkspaceDir(t *testing.T) string {
	t.Helper()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("normalize a fixture workspace dir: %v", err)
	}
	return dir
}

func forwardFor(dir string) logging.ForwardRecord {
	return logging.ForwardRecord{
		Level: "info", Operation: "sidecar.tail.pickup", Message: "pickup",
		WorkspaceDir: dir, WorkspaceID: "deadbeef",
	}
}

// onFakeClock points a Client at an injected clock and states the freshness
// bound the case is exercising, so no test waits out a real one.
func onFakeClock(client *Client, clock *fakeClock) {
	client.now = clock.Now
	client.freshness = time.Second
}

// A directory FORGOTTEN AND REGISTERED AGAIN is attributed to the id the roster
// holds now, not the one it held when the ref was first read.
func TestForwardAttributesTheNewRefAfterTheSameDirIsRegisteredAgain(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "af24557b1ddd4b9c", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	var replacements [][3]string
	client.SetRefReplacedObserver(func(d, oldID, newID string) {
		replacements = append(replacements, [3]string{d, oldID, newID})
	})
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the first Forward returned %v", err)
	}

	// Act: the workspace is forgotten and the SAME dir registered under a new
	// id, and the cached ref ages past the freshness bound.
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "54578ede3d834dea", Dir: dir})
	clock.Advance(2 * time.Second)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the Forward after re-registration returned %v", err)
	}

	// Assert.
	if got := daemon.lastRequest(t).GetWorkspace().GetId(); got != "54578ede3d834dea" {
		t.Fatalf("forwarded workspace id = %q, want the re-registered %q", got, "54578ede3d834dea")
	}
	want := [3]string{dir, "af24557b1ddd4b9c", "54578ede3d834dea"}
	if len(replacements) != 1 || replacements[0] != want {
		t.Fatalf("ref-replaced observations = %v, want exactly %v", replacements, want)
	}
}

// A REFUSED forward whose ref the daemon no longer holds re-resolves ONCE and
// lands, rather than reporting the record undelivered.
func TestForwardRetriesOnceWithAFreshRefWhenTheCachedOneIsRefused(t *testing.T) {
	// Arrange: the ref is cached and still inside the freshness bound, so only
	// the refusal itself can make the forwarder look at the roster again.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "af24557b1ddd4b9c", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the first Forward returned %v", err)
	}
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "54578ede3d834dea", Dir: dir})
	daemon.refuse("af24557b1ddd4b9c")

	// Act.
	_, err := client.Forward(forwardFor(dir))

	// Assert.
	if err != nil {
		t.Fatalf("Forward after a refused cached ref = %v, want the retry to have landed", err)
	}
	if got := daemon.lastRequest(t).GetWorkspace().GetId(); got != "54578ede3d834dea" {
		t.Fatalf("retried workspace id = %q, want the ref the roster holds now", got)
	}
	if got := daemon.rosterReads(); got != 2 {
		t.Fatalf("roster reads = %d, want the cached read plus exactly one re-read", got)
	}
}

// A directory the roster GENUINELY no longer holds still reports undelivered,
// after the one re-read — the refusal classification is not weakened by it.
func TestForwardStaysUnresolvableWhenTheFreshRosterLacksTheDir(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "af24557b1ddd4b9c", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the first Forward returned %v", err)
	}
	daemon.setRoster()
	daemon.refuse("af24557b1ddd4b9c")

	// Act.
	_, err := client.Forward(forwardFor(dir))

	// Assert.
	if !errors.Is(err, logging.ErrForwardWorkspaceUnresolvable) {
		t.Fatalf("Forward for a dir the roster dropped = %v, want ErrForwardWorkspaceUnresolvable", err)
	}
}

// A DIFFERENT DAEMON IS A DIFFERENT ROSTER: ids are minted per daemon, so a
// handover drops every cached ref rather than carrying one across.
func TestForwardDropsEveryCachedRefWhenTheDaemonAddressChanges(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	first := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	first.setRoster(&workspacev1.WorkspaceRef{Id: "first-daemon-id", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the Forward against the first daemon returned %v", err)
	}

	// Act: a successor daemon publishes over daemon.addr, well inside the
	// freshness bound the cached ref would otherwise still be used under.
	second := startRosterDaemon(t, stateDir)
	second.setRoster(&workspacev1.WorkspaceRef{Id: "second-daemon-id", Dir: dir})
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the Forward against the successor daemon returned %v", err)
	}

	// Assert.
	if got := second.lastRequest(t).GetWorkspace().GetId(); got != "second-daemon-id" {
		t.Fatalf("forwarded workspace id = %q, want the successor daemon's own ref", got)
	}
}

// THE CACHE IS KEYED PER DIRECTORY. One slot meant two directories forwarding
// in turn evicted each other and re-read the roster for every single record.
func TestForwardKeepsOneCachedRefPerDirectory(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	one, two := tempWorkspaceDir(t), tempWorkspaceDir(t)
	daemon.setRoster(
		&workspacev1.WorkspaceRef{Id: "workspace-one", Dir: one},
		&workspacev1.WorkspaceRef{Id: "workspace-two", Dir: two},
	)
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)

	// Act: alternate between the two directories inside one freshness window.
	for _, dir := range []string{one, two, one, two} {
		if _, err := client.Forward(forwardFor(dir)); err != nil {
			t.Fatalf("Forward for %q returned %v", dir, err)
		}
	}

	// Assert.
	if got := daemon.rosterReads(); got != 2 {
		t.Fatalf("roster reads = %d, want one per directory", got)
	}
}

// A CACHED REF OLDER THAN THE FRESHNESS BOUND IS RE-READ before it is used
// again, which is what makes the staleness window finite when no refusal ever
// arrives to expose it.
func TestForwardRereadsTheRosterForARefOlderThanTheFreshnessBound(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "af24557b1ddd4b9c", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the first Forward returned %v", err)
	}

	// Act.
	clock.Advance(time.Second)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the Forward past the freshness bound returned %v", err)
	}

	// Assert.
	if got := daemon.rosterReads(); got != 2 {
		t.Fatalf("roster reads = %d, want the aged ref read again", got)
	}
}

// AN UNCHANGED REF IS NOT A REPLACEMENT. The INFO exists to name a roster that
// MOVED, so a re-read that confirms the same id must state nothing at all.
func TestForwardStatesNoReplacementWhenARereadConfirmsTheSameRef(t *testing.T) {
	// Arrange.
	stateDir := t.TempDir()
	daemon := startRosterDaemon(t, stateDir)
	dir := tempWorkspaceDir(t)
	daemon.setRoster(&workspacev1.WorkspaceRef{Id: "af24557b1ddd4b9c", Dir: dir})
	clock := newFakeClock()
	client := New(stateDir)
	onFakeClock(client, clock)
	replacements := 0
	client.SetRefReplacedObserver(func(_, _, _ string) { replacements++ })
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the first Forward returned %v", err)
	}

	// Act.
	clock.Advance(2 * time.Second)
	if _, err := client.Forward(forwardFor(dir)); err != nil {
		t.Fatalf("the Forward past the freshness bound returned %v", err)
	}

	// Assert.
	if replacements != 0 {
		t.Fatalf("ref-replaced observations = %d, want none for an unchanged ref", replacements)
	}
}
