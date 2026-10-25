// zettel_world_test.go — the real zettel service a chat world runs beside its
// daemon (WorldOpts.Zettel): built once per run, started on a socket and a
// graph database of the world's own, and held to the same guarantee as the
// store and the sidecar (an exit the test did not ask for fails it).
package e2e

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	"connectrpc.com/connect"

	zettelv1 "agentrepl/proto/zettel/v1"
	"agentrepl/proto/zettel/v1/zettelv1connect"

	"agent-repld/integration/grantedclock"
)

var zettelProgram = &programOnce{name: "zettel", build: func() (string, error) {
	return goBuildCovered(repo.zettelDir, filepath.Join(e2eBinDir(), "zettel"))
}}

// requireZettelBinary builds the real zettel, once per run.
func requireZettelBinary(t testing.TB) string {
	t.Helper()
	return zettelProgram.require(t, "a chat world runs against the REAL zettel, which does not build")
}

// Zettel is one running zettel service.
type Zettel struct {
	t       testing.TB
	Socket  string
	DBPath  string
	LogPath string
	// Client is a zettel.v1 client of the service, for a test that reads the
	// graph the daemon wrote.
	Client  zettelv1connect.ZettelClient
	cmd     *exec.Cmd
	exit    *processExit
	stopped bool
}

// startZettel starts the real zettel on SOCKET over the graph at DBPATH,
// logging to LOGPATH and reporting its build into LOCKDIR, and answers once
// it serves.
func startZettel(t testing.TB, socket, dbPath, logPath, lockDir string) *Zettel {
	t.Helper()
	bin := requireZettelBinary(t)
	if lockDir == "" {
		t.Fatal("e2e: zettel needs the world's lock dir; an empty one would write its build report over the owner's own in ~/.cache/agent-repl/run")
	}
	cmd := startService(t, "zettel", "start", bin, []string{"--socket", socket, "--db", dbPath, "--log", logPath}, lockDir)
	z := &Zettel{
		t: t, Socket: socket, DBPath: dbPath, LogPath: logPath,
		Client: zettelv1connect.NewZettelClient(udsHTTPClient(socket), "http://zettel"),
		cmd:    cmd, exit: watchProcess(cmd),
	}
	t.Cleanup(func() {
		if !z.Exited() {
			z.Stop()
		}
	})
	z.awaitReady()
	return z
}

// awaitReady waits until zettel answers a read: its listener comes up after
// its build report and its graph open, so only an answer says it serves.
func (z *Zettel) awaitReady() {
	z.t.Helper()
	ctx, cancel := grantedclock.WithBound(context.Background(), DefaultTimeout)
	defer cancel()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if _, err := z.Client.GetThread(ctx, connect.NewRequest(&zettelv1.GetThreadRequest{Thread: &zettelv1.ThreadId{Id: "readiness"}})); err == nil {
			return
		}
		select {
		case <-z.exit.done:
			z.t.Fatalf("e2e: zettel exited before readiness (%s)", z.exit.status())
		case <-ctx.Done():
			z.t.Fatalf("e2e: zettel never answered GetThread within %s; process was %s", DefaultTimeout, z.exit.status())
		case <-ticker.C:
		}
	}
}

// Exited reports whether the zettel process has already left.
func (z *Zettel) Exited() bool {
	select {
	case <-z.exit.done:
		return true
	default:
		return false
	}
}

// Stop terminates zettel and waits for it, recording the exit as the test's.
func (z *Zettel) Stop() {
	z.t.Helper()
	if z.stopped {
		return
	}
	z.stopped = true
	stopProcess(z.t, "zettel", z.cmd, z.exit, z.LogPath)
	if err := os.Remove(z.Socket); err != nil && !errors.Is(err, os.ErrNotExist) {
		z.t.Errorf("e2e: remove zettel socket %s: %v", z.Socket, err)
	}
}

// requireZettelOutlivedTheTest fails T when Z exited before the test's
// cleanup without the test stopping it.
func requireZettelOutlivedTheTest(t testing.TB, z *Zettel) {
	t.Helper()
	if z.Exited() && !z.stopped {
		t.Errorf("e2e: zettel exited before test cleanup (%s), and was never stopped by the test", z.exit.status())
	}
}
