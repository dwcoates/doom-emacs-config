package main

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// newHandler builds the one mux serving agentrepl.v1 and the /_fake/ control
// plane on the same origin, wrapped in h2c so both HTTP/1.1 and prior-
// knowledge HTTP/2 clients reach it (COMMON.md, "DAEMON ADDRESS").
func newHandler(s *fakeServer, exit func()) http.Handler {
	mux := http.NewServeMux()
	path, handler := agentreplv1connect.NewAgentReplHandler(s, strictJSONOptions()...)
	mux.Handle(path, handler)
	_ = exit
	return h2c.NewHandler(mux, &http2.Server{})
}

func run() error {
	dir, err := stateDir()
	if err != nil {
		return err
	}

	listener, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return fmt.Errorf("bind loopback listener: %w", err)
	}
	address := fmt.Sprintf("127.0.0.1:%d", listener.Addr().(*net.TCPAddr).Port)

	server := newFakeServer()
	shutdown := make(chan struct{})
	var closeOnce = make(chan struct{}, 1)
	exit := func() {
		select {
		case closeOnce <- struct{}{}:
			close(shutdown)
		default:
		}
	}

	httpServer := &http.Server{
		Handler:     newHandler(server, exit),
		ConnContext: connContext,
	}

	if err := writeAddrFile(dir, address); err != nil {
		listener.Close()
		return err
	}
	logInfo("fakedaemon.boot.listening", "fake daemon listening",
		map[string]any{"address": address, "state_dir": dir, "addr_file": addrFilePath(dir)})

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, syscall.SIGTERM, syscall.SIGINT)

	serveErr := make(chan error, 1)
	go func() { serveErr <- httpServer.Serve(listener) }()

	var result error
	select {
	case sig := <-signals:
		logInfo("fakedaemon.exit.signal", "exiting on a signal", map[string]any{"signal": sig.String()})
	case <-shutdown:
	case err := <-serveErr:
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			result = fmt.Errorf("serve: %w", err)
			logError("fakedaemon.exit.serve-failed", "the HTTP server stopped with an error",
				map[string]any{"error": err.Error()})
		}
	}

	// Orderly exit: the address file goes away BEFORE the listener closes, so
	// a client that re-reads daemon.addr never finds a stale address pointing
	// at a socket that is already refusing.
	if err := removeAddrFile(dir); err != nil {
		logError("fakedaemon.exit.addr-remove-failed", "could not remove daemon.addr",
			map[string]any{"error": err.Error()})
		if result == nil {
			result = err
		}
	} else {
		logInfo("fakedaemon.exit.addr-removed", "removed daemon.addr", map[string]any{"addr_file": addrFilePath(dir)})
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()
	if err := httpServer.Shutdown(ctx); err != nil {
		logWarn("fakedaemon.exit.shutdown-timeout", "graceful shutdown did not finish",
			map[string]any{"error": err.Error()})
		httpServer.Close()
	}
	logInfo("fakedaemon.exit.done", "fake daemon exited", nil)
	return result
}

func main() {
	if err := run(); err != nil {
		logError("fakedaemon.boot.failed", "fake daemon refused to run", map[string]any{"error": err.Error()})
		fmt.Fprintln(os.Stderr, "fakedaemon:", err)
		os.Exit(1)
	}
}
