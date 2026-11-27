package wifirpc

import (
	"context"
	"errors"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	persistentwifiv1 "agentrepl/proto/persistentwifi/v1"
	"agentrepl/proto/persistentwifi/v1/persistentwifiv1connect"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/publish"
	"agent-repld/internal/tempdirs/tempdirstest"
)

// fakeController answers every Update with response and records each request.
type fakeController struct {
	mu       sync.Mutex
	topic    publish.Topic[*agentreplv1.PersistentWifiState]
	requests []*agentreplv1.UpdatePersistentWifiModeRequest
	response *agentreplv1.UpdatePersistentWifiModeResponse
}

func (f *fakeController) Update(_ context.Context, update persistentwifi.ValidUpdate) *agentreplv1.UpdatePersistentWifiModeResponse {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.requests = append(f.requests, update.Request())
	return f.response
}

func (f *fakeController) Topic() *publish.Topic[*agentreplv1.PersistentWifiState] { return &f.topic }

var home = "Home"

func standing(on bool) *agentreplv1.PersistentWifiState {
	s := &agentreplv1.PersistentWifiState{Wifi: &agentreplv1.PersistentWifiState_Joined{Joined: &agentreplv1.PersistentWifiJoined{NetworkName: &home}}}
	if on {
		s.Mode = &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}
	} else {
		s.Mode = &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}}
	}
	return s
}

func turnOn() *agentreplv1.UpdatePersistentWifiModeRequest {
	return &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}}}
}

// dialRPC is a service client for a test that calls the service directly.
func dialRPC(socket string) persistentwifiv1connect.PersistentWifiClient {
	rpc, _ := Dial(socket)
	return rpc
}

// shortDir is a directory whose path fits a unix socket's length limit.
func shortDir(t *testing.T) string { return tempdirstest.ShortDir(t, "wr") }

// heldListener is a unix listener a server's Shutdown stops accepting on
// without closing it, as a socket holder keeps its sockets open across a
// service's restart: a connect made while no server accepts queues in its
// backlog, and a later server serves the same listener again (Reopen).
type heldListener struct{ *net.UnixListener }

// Close ends the Accept in flight and leaves the socket open.
func (h heldListener) Close() error { return h.SetDeadline(time.Now()) }

// Reopen lets the next server accept on the socket again.
func (h heldListener) Reopen() error { return h.SetDeadline(time.Time{}) }

// served starts a Server over controller on a held unix socket and answers
// the socket's path and the held listener, so a test can serve it again.
func served(t *testing.T, controller Controller, log dlog.Logger) (string, heldListener, *Server) {
	t.Helper()
	ln, err := net.Listen("unix", filepath.Join(shortDir(t), "w.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	held := heldListener{ln.(*net.UnixListener)}
	t.Cleanup(func() { _ = ln.Close() })
	srv := NewServer(controller, log, NewPeers())
	go func() { _ = srv.Serve(held) }()
	t.Cleanup(func() { _ = srv.Shutdown(context.Background()) })
	return ln.Addr().String(), held, srv
}

func TestServerUpdateModePassesTheRequestThroughAndAnswersTheControllersResponse(t *testing.T) {
	// Arrange
	controller := &fakeController{response: &agentreplv1.UpdatePersistentWifiModeResponse{Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{
		Success: &agentreplv1.UpdatePersistentWifiModeSuccess{State: standing(true)}}}}
	socket, _, _ := served(t, controller, dlog.NewTestLogger())
	rpc := dialRPC(socket)

	// Act
	resp, err := rpc.UpdateMode(context.Background(), connect.NewRequest(&persistentwifiv1.UpdateModeRequest{Request: turnOn()}))

	// Assert
	if err != nil {
		t.Fatalf("UpdateMode() error = %v", err)
	}
	if resp.Msg.GetResponse().GetSuccess().GetState().GetOn() == nil {
		t.Fatalf("response = %v, want the controller's success", resp.Msg)
	}
	if len(controller.requests) != 1 || controller.requests[0].GetOn() == nil {
		t.Fatalf("controller saw %v, want the one on request", controller.requests)
	}
}

func TestServerUpdateModeRefusesARequestWithNoActionAndRecordsIt(t *testing.T) {
	// Arrange
	controller := &fakeController{}
	log := dlog.NewTestLogger()
	socket, _, _ := served(t, controller, log)

	// Act
	_, err := dialRPC(socket).UpdateMode(context.Background(), connect.NewRequest(&persistentwifiv1.UpdateModeRequest{Request: &agentreplv1.UpdatePersistentWifiModeRequest{}}))

	// Assert
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeInvalidArgument {
		t.Fatalf("error = %v, want InvalidArgument", err)
	}
	if len(controller.requests) != 0 {
		t.Fatalf("the controller was handed %v, want nothing", controller.requests)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opUpdate}); len(got) != 1 {
		t.Fatalf("ERROR records = %+v, want the one refusal", got)
	}
}

func TestServerWatchStandingSendsTheCurrentStandingThenEveryChange(t *testing.T) {
	// Arrange
	controller := &fakeController{}
	controller.topic.Publish(standing(false))
	socket, _, _ := served(t, controller, dlog.NewTestLogger())
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := dialRPC(socket).WatchStanding(ctx, connect.NewRequest(&persistentwifiv1.WatchStandingRequest{}))
	if err != nil {
		t.Fatalf("WatchStanding() error = %v", err)
	}
	defer func() { cancel(); _ = stream.Close() }()

	// Act
	first := stream.Receive()
	firstOff := stream.Msg().GetStanding().GetOff() != nil
	controller.topic.Publish(standing(true))
	second := stream.Receive()

	// Assert
	if !first || !firstOff {
		t.Fatalf("first message: received %v, off %v; want the current standing, off", first, firstOff)
	}
	if !second || stream.Msg().GetStanding().GetOn() == nil {
		t.Fatalf("second message: received %v, standing %v; want the change, on", second, stream.Msg().GetStanding())
	}
}

// newClient builds a Client over the socket, collecting every standing its
// receiver is handed on heard.
func newClient(t *testing.T, socket string, log dlog.Logger, heard chan<- *agentreplv1.PersistentWifiState) *Client {
	t.Helper()
	return newClientWith(t, socket, log, heard, clocktest.New(time.Unix(0, 0)))
}

// newClientWith is newClient on the given scripted clock, which times the
// first-standing bound.
func newClientWith(t *testing.T, socket string, log dlog.Logger, heard chan<- *agentreplv1.PersistentWifiState, clk *clocktest.Scripted) *Client {
	t.Helper()
	rpc, closeIdle := Dial(socket)
	c, err := New(Deps{RPC: rpc, CloseIdle: closeIdle, Log: log, Clock: clk, OnStanding: func(s *agentreplv1.PersistentWifiState) { heard <- s }})
	if err != nil {
		t.Fatalf("New() error = %v", err)
	}
	return c
}

func TestServerShutdownEndsAStandingWatchInFlight(t *testing.T) {
	// Arrange
	controller := &fakeController{}
	controller.topic.Publish(standing(false))
	socket, _, srv := served(t, controller, dlog.NewTestLogger())
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := dialRPC(socket).WatchStanding(ctx, connect.NewRequest(&persistentwifiv1.WatchStandingRequest{}))
	if err != nil {
		t.Fatalf("WatchStanding() error = %v", err)
	}
	defer func() { cancel(); _ = stream.Close() }()
	if !stream.Receive() {
		t.Fatalf("the watch served no standing: %v", stream.Err())
	}

	// Act
	shutdown := make(chan error, 1)
	go func() { shutdown <- srv.Shutdown(context.Background()) }()
	ended := !stream.Receive()

	// Assert
	if !ended {
		t.Fatal("the watch was told another standing instead of ending")
	}
	if err := <-shutdown; err != nil {
		t.Fatalf("Shutdown() error = %v", err)
	}
}

func TestNewRefusesIncompleteDeps(t *testing.T) {
	full := func() Deps {
		return Deps{RPC: persistentwifiv1connect.NewPersistentWifiClient(http.DefaultClient, "http://x"), CloseIdle: func() {}, Log: dlog.NewTestLogger(), Clock: clocktest.New(time.Unix(0, 0)), OnStanding: func(*agentreplv1.PersistentWifiState) {}}
	}
	tests := []struct {
		name   string
		break_ func(*Deps)
	}{
		{"no service client", func(d *Deps) { d.RPC = nil }},
		{"no way to close idle connections", func(d *Deps) { d.CloseIdle = nil }},
		{"no logger", func(d *Deps) { d.Log = nil }},
		{"no receiver", func(d *Deps) { d.OnStanding = nil }},
		{"no clock", func(d *Deps) { d.Clock = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d := full()
			tt.break_(&d)

			// Act
			_, err := New(d)

			// Assert
			if err == nil {
				t.Fatal("New() accepted incomplete deps")
			}
		})
	}
}

func TestClientUpdatePassesThroughAndAnswersTheServicesResponse(t *testing.T) {
	// Arrange
	controller := &fakeController{response: &agentreplv1.UpdatePersistentWifiModeResponse{Result: &agentreplv1.UpdatePersistentWifiModeResponse_Success{
		Success: &agentreplv1.UpdatePersistentWifiModeSuccess{State: standing(true)}}}}
	socket, _, _ := served(t, controller, dlog.NewTestLogger())
	c := newClient(t, socket, dlog.NewTestLogger(), make(chan *agentreplv1.PersistentWifiState, 8))

	// Act
	resp, err := c.Update(context.Background(), turnOn())

	// Assert
	if err != nil || resp.GetSuccess().GetState().GetOn() == nil {
		t.Fatalf("Update() = %v, %v; want the service's success", resp, err)
	}
}

func TestClientUpdateAnswersUnavailableAndRecordsWhenTheServiceCannotBeReached(t *testing.T) {
	// Arrange: a socket nothing listens on.
	log := dlog.NewTestLogger()
	c := newClient(t, filepath.Join(shortDir(t), "absent.sock"), log, make(chan *agentreplv1.PersistentWifiState, 8))

	// Act
	resp, err := c.Update(context.Background(), turnOn())

	// Assert
	var cerr *connect.Error
	if !errors.As(err, &cerr) || cerr.Code() != connect.CodeUnavailable || !errors.Is(err, ErrUnavailable) || resp != nil {
		t.Fatalf("Update() = %v, %v; want an Unavailable error wrapping ErrUnavailable", resp, err)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opClientUpdate}); len(got) != 1 {
		t.Fatalf("ERROR records = %+v, want the one failed call", got)
	}
}

func TestClientStartPublishesTheUnreachableStandingWithoutWaitingOnTheService(t *testing.T) {
	// Arrange: a socket nothing listens on, so any wait on the service would fail.
	heard := make(chan *agentreplv1.PersistentWifiState, 8)
	c := newClient(t, filepath.Join(shortDir(t), "absent.sock"), dlog.NewTestLogger(), heard)

	// Act
	c.Start()

	// Assert
	got := <-heard
	if got.GetServiceUnreachable() == nil || got.GetQuiet() != nil || got.GetOn() != nil {
		t.Fatalf("standing = %v, want only the service-unreachable arm", got)
	}
	if latest, ok := c.Topic().Latest(); !ok || latest.GetServiceUnreachable() == nil {
		t.Fatalf("topic holds %v (%v), want the unreachable standing", latest, ok)
	}
}

func TestClientRunFollowsEveryStandingAndReopensAfterTheServiceRestarts(t *testing.T) {
	// Arrange: the first server serves the off standing.
	first := &fakeController{}
	first.topic.Publish(standing(false))
	socket, held, firstSrv := served(t, first, dlog.NewTestLogger())
	heard := make(chan *agentreplv1.PersistentWifiState, 8)
	log := dlog.NewTestLogger()
	c := newClient(t, socket, log, heard)
	ctx, cancel := context.WithCancel(context.Background())
	runErr := make(chan error, 1)
	go func() { runErr <- c.Run(ctx) }()

	// Act and assert: the current standing, then a change, are heard.
	if got := <-heard; got.GetOff() == nil {
		t.Fatalf("first standing = %v, want off", got)
	}
	first.topic.Publish(standing(true))
	if got := <-heard; got.GetOn() == nil {
		t.Fatalf("second standing = %v, want on", got)
	}

	// Act: the service restarts, a second one taking the held socket.
	if err := firstSrv.Shutdown(context.Background()); err != nil {
		t.Fatalf("Shutdown() error = %v", err)
	}
	// Assert: the moment the watch ended the standing became unreachable, with
	// no quiet state, before the watch was reopened.
	if got := <-heard; got.GetServiceUnreachable() == nil || got.GetQuiet() != nil {
		t.Fatalf("standing after the watch ended = %v, want the unreachable standing", got)
	}
	if err := held.Reopen(); err != nil {
		t.Fatalf("Reopen() error = %v", err)
	}
	second := &fakeController{}
	second.topic.Publish(standing(false))
	secondSrv := NewServer(second, dlog.NewTestLogger(), NewPeers())
	go func() { _ = secondSrv.Serve(held) }()
	t.Cleanup(func() { _ = secondSrv.Shutdown(context.Background()) })

	// Assert: the watch reopened and was told the new service's standing.
	if got := <-heard; got.GetOff() == nil {
		t.Fatalf("standing after the restart = %v, want the new service's off", got)
	}
	cancel()
	if err := <-runErr; err != nil {
		t.Fatalf("Run() error = %v, want nil once its context ends", err)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opClientWatch}); len(got) != 1 {
		t.Fatalf("INFO records = %+v, want the one reopened watch", got)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelWarn}); len(got) != 0 {
		t.Fatalf("WARN records = %+v, want none: a served watch reopening after a restart is no fault", got)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); len(got) != 0 {
		t.Fatalf("ERROR records = %+v, want none", got)
	}
}

func TestClientRunEndsWithAnErrorWhenAConnectionServesNothing(t *testing.T) {
	// Arrange: nothing listens, so the connection never serves a standing.
	log := dlog.NewTestLogger()
	c := newClient(t, filepath.Join(shortDir(t), "absent.sock"), log, make(chan *agentreplv1.PersistentWifiState, 8))

	// Act
	err := c.Run(context.Background())

	// Assert
	if !errors.Is(err, ErrUnavailable) {
		t.Fatalf("Run() error = %v, want ErrUnavailable", err)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opClientWatch}); len(got) != 1 {
		t.Fatalf("ERROR records = %+v, want the one ended follow", got)
	}
}

// awaitPeers waits, on the set's own change signal, until pred holds of its
// pids, or fails the test when ctx ends first.
func awaitPeers(t *testing.T, ctx context.Context, peers *Peers, what string, pred func([]int) bool) {
	t.Helper()
	for {
		changed := peers.Changed()
		if pred(peers.PIDs()) {
			return
		}
		select {
		case <-changed:
		case <-ctx.Done():
			t.Fatalf("never saw %s; the peers were %v", what, peers.PIDs())
		}
	}
}

func TestServerHoldsTheKernelRecordedPidOfEveryConnectedPeerWhileItsConnectionLives(t *testing.T) {
	// Arrange: a server whose peer set we watch, and a client of this process.
	peers := NewPeers()
	ln, err := net.Listen("unix", filepath.Join(shortDir(t), "p.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	srv := NewServer(&fakeController{response: &agentreplv1.UpdatePersistentWifiModeResponse{}}, dlog.NewTestLogger(), peers)
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Shutdown(context.Background()) })
	transport := h2cclient.UnixTransport(ln.Addr().String(), nil)
	rpc := persistentwifiv1connect.NewPersistentWifiClient(&http.Client{Transport: transport}, "http://persistent-wifi.uds")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act: one call opens the connection.
	if _, err := rpc.UpdateMode(ctx, connect.NewRequest(&persistentwifiv1.UpdateModeRequest{Request: turnOn()})); err != nil {
		t.Fatalf("UpdateMode() error = %v", err)
	}

	// Assert: this process is the connected peer, until its connection ends.
	awaitPeers(t, ctx, peers, "this process connected", func(p []int) bool { return len(p) == 1 && p[0] == os.Getpid() })
	transport.CloseIdleConnections()
	awaitPeers(t, ctx, peers, "the peer gone once its connection ended", func(p []int) bool { return len(p) == 0 })
}

func TestServerRefusesAConnectionWhosePeerPidCannotBeReadAndRecordsIt(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	peers := NewPeers()
	ln, err := net.Listen("unix", filepath.Join(shortDir(t), "q.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	srv := NewServer(&fakeController{}, log, peers)
	srv.peerPID = func(net.Conn) (int, error) { return 0, errors.New("the kernel names no peer") }
	go func() { _ = srv.Serve(ln) }()
	t.Cleanup(func() { _ = srv.Shutdown(context.Background()) })
	rpc := dialRPC(ln.Addr().String())

	// Act
	_, err = rpc.UpdateMode(context.Background(), connect.NewRequest(&persistentwifiv1.UpdateModeRequest{Request: turnOn()}))

	// Assert
	if err == nil {
		t.Fatal("UpdateMode() was served over a connection whose peer is unnamed")
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opServe}); len(got) == 0 {
		t.Fatalf("ERROR records = %+v, want the refused connection recorded", got)
	}
	if pids := peers.PIDs(); len(pids) != 0 {
		t.Fatalf("peers = %v, want none held for a refused connection", pids)
	}
}

func TestClientRecordsAConnectionThatServesNothingWithinTheBoundAndKeepsWaiting(t *testing.T) {
	// Arrange: a service that accepts but serves no standing (its topic is empty).
	socket, _, _ := served(t, &fakeController{}, dlog.NewTestLogger())
	log := dlog.NewTestLogger()
	clk := clocktest.New(time.Unix(0, 0))
	heard := make(chan *agentreplv1.PersistentWifiState, 8)
	c := newClientWith(t, socket, log, heard, clk)
	ctx, cancel := context.WithCancel(context.Background())
	runErr := make(chan error, 1)
	go func() { runErr <- c.Run(ctx) }()

	// Act: the bound passes with no standing served, then the daemon stands down.
	if got := <-clk.Asked; got != DefaultFirstStandingBound {
		t.Fatalf("the first-standing bound asked for %v, want %v", got, DefaultFirstStandingBound)
	}
	clk.Fire <- time.Unix(0, 0)
	if _, err := log.Await(ctx, dlog.RecordQuery{Level: dlog.LevelError, Operation: opClientWatch}); err != nil {
		t.Fatalf("the bound was never recorded: %v", err)
	}
	cancel()
	err := <-runErr

	// Assert: recorded once at ERROR, and the follow kept waiting rather than failing or reopening.
	if err != nil {
		t.Fatalf("Run() error = %v, want nil: the connection was kept, not failed", err)
	}
	if got := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opClientWatch}); len(got) != 1 {
		t.Fatalf("ERROR records = %+v, want the one bound record", got)
	}
	if len(heard) != 0 {
		t.Fatalf("the receiver heard %d standings from a service that served none", len(heard))
	}
}

func TestServerRefusesAStandingWatchAskedWhileItDrains(t *testing.T) {
	// Arrange: a server that has begun to drain, its listener still accepting.
	controller := &fakeController{}
	controller.topic.Publish(standing(false))
	socket, _, srv := served(t, controller, dlog.NewTestLogger())
	close(srv.draining)
	srv.drainingOnce.Do(func() {})

	// Act
	ctx, cancel := context.WithCancel(context.Background())
	stream, err := dialRPC(socket).WatchStanding(ctx, connect.NewRequest(&persistentwifiv1.WatchStandingRequest{}))
	if err != nil {
		t.Fatalf("WatchStanding() error = %v", err)
	}
	defer func() { cancel(); _ = stream.Close() }()
	received := stream.Receive()

	// Assert: an answer from the service, Unavailable, and no standing.
	var cerr *connect.Error
	if received || !errors.As(stream.Err(), &cerr) || cerr.Code() != connect.CodeUnavailable {
		t.Fatalf("received %v, error %v; want no standing and an Unavailable refusal", received, stream.Err())
	}
	if len(stream.ResponseHeader()) == 0 {
		t.Fatal("the refusal carried no response headers, so a caller cannot tell it from a failed dial")
	}
}
