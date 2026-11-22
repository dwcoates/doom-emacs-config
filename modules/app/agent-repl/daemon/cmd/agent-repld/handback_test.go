package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/sockethold"
	"agentrepl/logging/sockethold/holdtest"

	"agent-repld/internal/dlog"
	"agent-repld/internal/server"
)

// exchangeBound bounds one holder exchange against a holder this test runs,
// a kernel-local round trip; it guards against a hang and is never a wait.
const exchangeBound = 5 * time.Second

// acceptsGate is a recordingGate whose listener tells, on its second Accept,
// that the first connection is net/http's: Serve records a connection's
// StateNew before it asks for the next.
type acceptsGate struct {
	recordingGate
	second chan struct{}
	once   sync.Once
}

func (g *acceptsGate) Listener(inner net.Listener) net.Listener {
	return &countingListener{Listener: inner, g: g}
}

type countingListener struct {
	net.Listener
	g     *acceptsGate
	calls int
}

func (l *countingListener) Accept() (net.Conn, error) {
	l.calls++
	if l.calls == 2 {
		l.g.once.Do(func() { close(l.g.second) })
	}
	return l.Listener.Accept()
}

// daemonOnHolder is one daemon serving the holder's shared listener the way
// run does: the listener and the accept stream taken from CONTROL, and every
// unread connection handed back at its exit. It answers with NAME.
type daemonOnHolder struct {
	cancel context.CancelFunc
	served chan error
	gate   *acceptsGate
}

func startDaemonOnHolder(t *testing.T, control, name string) *daemonOnHolder {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), exchangeBound)
	shared, err := sockethold.Take(ctx, control, "daemon")
	if err != nil {
		t.Fatalf("Take: %v", err)
	}
	returned, err := sockethold.Subscribe(ctx, control, "daemon")
	cancel()
	if err != nil {
		t.Fatalf("Subscribe: %v", err)
	}
	private, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	log := dlog.NewTestLogger()
	accept := server.NewSharedAccept(private)
	if err := accept.AddShared(shared, returned, log); err != nil {
		t.Fatalf("AddShared: %v", err)
	}
	gate := &acceptsGate{second: make(chan struct{})}
	gate.Handler = http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { fmt.Fprint(w, name) })
	serveCtx, stop := context.WithCancel(context.Background())
	d := &daemonOnHolder{cancel: stop, served: make(chan error, 1), gate: gate}
	go func() {
		d.served <- serve(serveCtx, accept, gate, serveExit{
			// THE SHARED ACCEPT STOPS FIRST, as beginShutdown does.
			OnShuttingDown: func() { _ = accept.StopShared() },
			Log:            log,
			HandBack: func(f *os.File) error {
				c, cc := context.WithTimeout(context.Background(), exchangeBound)
				defer cc()
				return sockethold.Return(c, control, "daemon", f)
			},
		})
	}()
	t.Cleanup(func() { stop(); <-d.served; accept.Close() })
	return d
}

func TestAConnectionAcceptedJustBeforeTheExitIsAnsweredByTheSuccessor(t *testing.T) {
	// Arrange: the incumbent accepted a client's connection, and the client
	// has not written its call yet.
	file, address := holdtest.TCP(t)
	control := holdtest.Start(t, map[string]*os.File{"daemon": file}, nil)
	incumbent := startDaemonOnHolder(t, control, "incumbent")
	client, err := net.Dial("tcp", address)
	if err != nil {
		t.Fatal(err)
	}
	defer client.Close()
	<-incumbent.gate.second

	// Act: the incumbent exits, a successor takes over, and only then does
	// the client write its call on the connection it already had.
	incumbent.cancel()
	if err := <-incumbent.served; err != nil {
		t.Fatalf("the incumbent's serve = %v, want an orderly exit", err)
	}
	incumbent.served <- nil
	startDaemonOnHolder(t, control, "successor")
	if _, err := io.WriteString(client, "GET / HTTP/1.1\r\nHost: daemon\r\n\r\n"); err != nil {
		t.Fatalf("the client's call: %v", err)
	}
	if err := client.SetReadDeadline(time.Now().Add(exchangeBound)); err != nil {
		t.Fatal(err)
	}
	resp, err := http.ReadResponse(bufio.NewReader(client), nil)

	// Assert: the successor answered it.
	if err != nil {
		t.Fatalf("the call's answer: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)
	if strings.TrimSpace(string(body)) != "successor" {
		t.Fatalf("answered by %q, want the successor", body)
	}
}
