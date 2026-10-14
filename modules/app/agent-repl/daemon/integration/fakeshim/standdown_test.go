package main

import (
	"context"
	"crypto/tls"
	"io"
	"net"
	"net/http"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	"agent-repld/internal/hangup"
)

// pipeListener hands the server the far end of every connection the test's
// client dials, so no socket path is involved.
type pipeListener struct {
	conns chan net.Conn
	done  chan struct{}
}

func (l *pipeListener) Accept() (net.Conn, error) {
	select {
	case c := <-l.conns:
		return c, nil
	case <-l.done:
		return nil, net.ErrClosed
	}
}
func (l *pipeListener) Close() error   { return nil }
func (l *pipeListener) Addr() net.Addr { return &net.UnixAddr{Name: "pipe", Net: "unix"} }

// standDownServer serves standDownOnHangUp over h2c, as run wires it, and
// answers an h2c client of it. served receives, per request, the channel
// closed when the client hangs up that request's connection.
func standDownServer(t *testing.T, killed bool, end func()) (*http.Client, <-chan (<-chan struct{})) {
	t.Helper()
	served := make(chan (<-chan struct{}), 4)
	inner := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		served <- hangup.Of(r.Context())
		w.WriteHeader(http.StatusOK)
	})
	l := &pipeListener{conns: make(chan net.Conn), done: make(chan struct{})}
	srv := &http.Server{
		Handler:     h2c.NewHandler(standDownOnHangUp(inner, func() bool { return killed }, end), &http2.Server{}),
		ConnContext: hangup.ConnContext,
	}
	go func() { _ = srv.Serve(hangup.Listener(l)) }()
	t.Cleanup(func() {
		close(l.done)
		_ = srv.Close()
	})
	client := &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(context.Context, string, string, *tls.Config) (net.Conn, error) {
			near, far := net.Pipe()
			l.conns <- far
			return near, nil
		},
	}}
	return client, served
}

// call posts to path and reads the whole answer.
func call(t *testing.T, client *http.Client, path string) {
	t.Helper()
	resp, err := client.Post("http://shim"+path, "application/proto", strings.NewReader(""))
	if err != nil {
		t.Fatalf("POST %s error = %v", path, err)
	}
	if _, err := io.Copy(io.Discard, resp.Body); err != nil {
		t.Fatalf("read the answer to %s: %v", path, err)
	}
	if err := resp.Body.Close(); err != nil {
		t.Fatalf("close the answer to %s: %v", path, err)
	}
}

// hungUpWait bounds a wait the test expects to end at once.
const hungUpWait = 5 * time.Second

func TestStandDownOnHangUpDoesNotEndBeforeTheHangUp(t *testing.T) {
	// Arrange.
	var ended atomic.Int32
	client, served := standDownServer(t, true, func() { ended.Add(1) })

	// Act.
	call(t, client, "/shim.v1.Shim/KillSession")

	// Assert.
	<-served
	if n := ended.Load(); n != 0 {
		t.Fatalf("the process ended %d time(s) with the daemon still connected", n)
	}
}

func TestStandDownOnHangUpEndsOnTheHangUp(t *testing.T) {
	// Arrange.
	ended := make(chan struct{})
	client, served := standDownServer(t, true, func() { close(ended) })
	call(t, client, "/shim.v1.Shim/KillSession")
	<-served

	// Act.
	client.CloseIdleConnections()

	// Assert.
	select {
	case <-ended:
	case <-time.After(hungUpWait):
		t.Fatal("the process did not end once the daemon hung up")
	}
}

// endsAfterHangUp answers whether end ran once the client hung up the
// connection one request to path came in on.
func endsAfterHangUp(t *testing.T, killed bool, path string) bool {
	t.Helper()
	var ended atomic.Int32
	client, served := standDownServer(t, killed, func() { ended.Add(1) })
	call(t, client, path)
	hungUp := <-served
	client.CloseIdleConnections()
	select {
	case <-hungUp:
	case <-time.After(hungUpWait):
		t.Fatal("the server never saw the hang-up")
	}
	return ended.Load() != 0
}

func TestStandDownOnHangUpIgnoresARefusedKill(t *testing.T) {
	// Arrange, Act.
	ended := endsAfterHangUp(t, false, "/shim.v1.Shim/KillSession")

	// Assert.
	if ended {
		t.Fatal("a KillSession the shim refused ended the process on the hang-up")
	}
}

func TestStandDownOnHangUpIgnoresAnotherVerb(t *testing.T) {
	// Arrange, Act.
	ended := endsAfterHangUp(t, true, "/shim.v1.Shim/StartTurn")

	// Assert.
	if ended {
		t.Fatal("a verb other than KillSession ended the process on the hang-up")
	}
}
