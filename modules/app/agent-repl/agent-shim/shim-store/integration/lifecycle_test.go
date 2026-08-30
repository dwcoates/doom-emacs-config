// lifecycle_test.go — SUBJECT 13: how the process starts and how it stops.
//
// Two obligations at the edges of the process's life. On the way down: an open
// stream ends CLEANLY rather than hanging a caller, and the socket file is
// removed so the next start is not fighting a corpse. On the way up: the pprof
// surface opens BEFORE the database, so a boot that wedges on the database is
// still diagnosable — which is only demonstrable by wedging one.
package integration

import (
	"context"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"syscall"
	"testing"
	"time"
)

// TestSigtermEndsAnOpenWatchCleanly: a shutdown is not a way for a caller to
// hang forever waiting on a stream nobody will ever write to again.
func TestSigtermEndsAnOpenWatchCleanly(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t,
		shim.agentEntry("w-term-1", "u-term-1", frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))),
	)
	opened := openSession(ctx, t, cli, "main", 10, nil)
	stream := watchStream(ctx, t, cli, opened.GetWatch())
	defer stream.Close()
	// Prove the stream is really live before taking the store down.
	shim.write(ctx, t,
		shim.agentEntry("w-term-2", "u-term-2", frameLine(agentID("main"), responseFrame("main", "act-2", "L2"))),
	)
	assertTexts(t, "the live tail", receivedTexts(receiveLines(t, stream, 1)), []string{"L2"})

	// Act.
	store.signal(syscall.SIGTERM)

	// Assert: the stream ENDS. Whether it ends at EOF or with a cancellation
	// is the transport's business; hanging is the only wrong answer, and
	// awaitStreamEnd fails the test if it does.
	_ = awaitStreamEnd(t, stream)
	if err := store.awaitExit(); err != nil {
		t.Errorf("SIGTERM with an open watch was not an orderly exit: %v", err)
	}
}

// TestOrderlyExitRemovesTheSocketFile.
func TestOrderlyExitRemovesTheSocketFile(t *testing.T) {
	// Arrange.
	store := startStore(t, storeOptions{})
	if !store.socketExists() {
		t.Fatalf("the store is running but its socket %q is not on disk", store.socket)
	}

	// Act.
	store.signal(syscall.SIGTERM)
	if err := store.awaitExit(); err != nil {
		t.Fatalf("SIGTERM was not an orderly exit: %v", err)
	}

	// Assert.
	if store.socketExists() {
		t.Errorf("the socket %q survived an orderly exit; the next start would fight a corpse", store.socket)
	}
}

// TestPprofSurfaceComesUpBeforeTheDatabase is the boot-order obligation, shown
// the only way it can be: by making the database step fail and finding the
// profiling surface already serving.
func TestPprofSurfaceComesUpBeforeTheDatabase(t *testing.T) {
	// Arrange: a --db path that cannot be created, and a pprof unix socket.
	work := t.TempDir()
	blocker := filepath.Join(work, "not-a-directory")
	if err := os.WriteFile(blocker, []byte("this is a file, not a directory"), 0o644); err != nil {
		t.Fatalf("creating the database blocker: %v", err)
	}
	pprofSocket := shortSocketPath(t)

	// Act.
	store := startStore(t, storeOptions{
		dbPath:    filepath.Join(blocker, "events.db"),
		pprofAddr: pprofSocket,
		noWait:    true,
	})

	// Assert: the profiling surface answered before the boot failed...
	assertPprofServed(t, pprofSocket)

	// ...and the boot did fail, loudly.
	err := store.awaitExit()
	if err == nil {
		t.Fatalf("the store exited zero with an unopenable database")
	}
	if !store.socketExists() {
		return
	}
	t.Errorf("a failed boot left the service socket %q behind", store.socket)
}

// assertPprofServed dials the profiling surface under a deadline and asserts
// that /debug/pprof/ answers. The surface races the doomed database step, so
// the dial is a bounded retry on a ticker rather than one attempt.
func assertPprofServed(t *testing.T, socket string) {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), readyTimeout)
	defer cancel()

	client := &http.Client{
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
				var d net.Dialer
				return d.DialContext(ctx, "unix", socket)
			},
		},
	}

	ticker := time.NewTicker(2 * time.Millisecond)
	defer ticker.Stop()

	var lastErr error
	for {
		req, err := http.NewRequestWithContext(ctx, http.MethodGet, "http://pprof.localhost/debug/pprof/", nil)
		if err != nil {
			t.Fatalf("building the pprof request: %v", err)
		}
		resp, err := client.Do(req)
		if err == nil {
			status := resp.StatusCode
			resp.Body.Close()
			if status == http.StatusOK {
				return
			}
			lastErr = nil
			t.Fatalf("the pprof surface answered HTTP %d, want 200", status)
		}
		lastErr = err

		select {
		case <-ctx.Done():
			t.Fatalf("the pprof surface never served /debug/pprof/ on %q within %s (last error: %v)", socket, readyTimeout, lastErr)
		case <-ticker.C:
		}
	}
}

// TestSigtermTheInstantTheSocketAcceptsIsStillAnOrderlyExit pins the window
// between binding the socket and being able to answer a signal.
//
// The kernel queues connections from listen(2) onward, so a supervisor — or
// this harness — sees a READY store the moment the socket is bound. If the
// signal handler is installed after that, SIGTERM in the gap still has its
// default disposition and kills the process outright: no listener close, so the
// socket file survives and the successor meets a corpse. The handler is
// therefore installed before anything is bound, which makes "the socket
// accepts" imply "signals are answered" — and makes this subject deterministic
// rather than a race. It is hammered because the pre-fix window is narrow.
func TestSigtermTheInstantTheSocketAcceptsIsStillAnOrderlyExit(t *testing.T) {
	for attempt := 0; attempt < 8; attempt++ {
		// Arrange: startStore returns only once the socket has accepted.
		store := startStore(t, storeOptions{})

		// Act.
		store.signal(syscall.SIGTERM)

		// Assert.
		if err := store.awaitExit(); err != nil {
			t.Fatalf("attempt %d: SIGTERM right after the socket accepted was not an orderly exit: %v", attempt, err)
		}
		if store.socketExists() {
			t.Fatalf("attempt %d: the socket %q survived; a hard kill left it behind", attempt, store.socket)
		}
	}
}
