package frontend

import (
	"io"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/frontend/rostertest"
)

// THE EDITOR-GLOBAL ROSTER PUBLICATION.
//
// Emacs republishes the whole roster on every change to it, and at startup that
// is a burst: one publication per restored workspace, each carrying the full
// fleet, each fanned out to every connected client, and each owing the
// publisher an ack it is BLOCKED on (the ack means completion — see lanes.go).
// This benchmark pins what one such publication costs at fleet scale, on the
// real path: validation, retention under the delivery lock, per-client scoping
// and marshalling, and the canonical log record the publication emits through a
// real dlog Logger.
//
// It is the standing guard on that path. A change to roster validation, to
// frontend delivery, or to the daemon's logging sinks runs it and reports the
// before/after — the same discipline BenchmarkReplayDrain and
// BenchmarkCursorWrite hold over bring-up.
//
// It exists because that cost was once 6957ms for ONE publication of sixteen
// workspaces, none of which was the publication's own work: the daemon's
// terminal mirror is a pty Emacs drains, and while Emacs was busy booting, a
// blocked mirror write held the durable sink's mutex that this path's final log
// record needs. See TestARosterPublicationDoesNotWaitOnABlockedTerminalMirror.

// benchRosterClients is the client fan-out one publication reaches: the Emacs
// UDS host plus the session webviews a working fleet has open. The host is
// unscoped and takes the shared marshalling; each webview is scoped and is
// filtered and marshalled on its own.
const benchRosterClients = 4

// newBenchRosterServer returns a server holding benchRosterClients connected
// clients and logging through a real dlog Logger, so the benchmark measures the
// production emission path rather than a no-op callback.
func newBenchRosterServer(b *testing.B) (*Server, []*client) {
	b.Helper()
	terminal := dlog.NewTerminalSink(io.Discard, dlog.DefaultTerminalBufferBytes)
	b.Cleanup(func() {
		if err := terminal.Close(); err != nil {
			b.Fatalf("close terminal sink: %v", err)
		}
	})
	logger := dlog.New(discardSyncWriter{}, terminal, true)
	s := New(Config{
		Logf:        dlog.Legacy(logger),
		LogVerbosef: logger.LogVerbose,
		State:       staticState{snap: &frontendv1.StateSnapshot{}},
		Handler:     &mockHandler{},
		BufSize:     4096,
	})
	clients := make([]*client, 0, benchRosterClients)
	host := newClient(4096, nil, ClientKindHost)
	s.clients[host] = struct{}{}
	clients = append(clients, host)
	for i := 1; i < benchRosterClients; i++ {
		scope := Scope{Workspace: rostertest.CurrentRowDir, SessionID: "s-bench"}
		cl := newClient(4096, &scope, ClientKindGUIStream)
		s.clients[cl] = struct{}{}
		clients = append(clients, cl)
	}
	return s, clients
}

// discardSyncWriter is a durable sink that costs nothing and is safe for the
// concurrent emitters a Logger allows.
type discardSyncWriter struct{}

func (discardSyncWriter) Write(p []byte) (int, error) { return len(p), nil }

// BenchmarkRosterPublish measures one full-fleet roster publication — the
// command a startup issues once per restored workspace.
func BenchmarkRosterPublish(b *testing.B) {
	s, clients := newBenchRosterServer(b)
	rosters := make([]*frontendv1.WorkspaceRoster, b.N)
	for i := range rosters {
		rosters[i] = rostertest.FleetRoster("boot-bench", int64(i+1), rostertest.FleetRosterWorkspaces)
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		if err := s.PublishWorkspaceRoster(rosters[i]); err != nil {
			b.Fatalf("publish revision %d: %v", i+1, err)
		}
		// Real clients drain concurrently through their write loops; draining
		// here only keeps the bounded outboxes from filling, and is not part
		// of what a publication costs.
		b.StopTimer()
		for _, cl := range clients {
			for {
				if _, ok := cl.out.pop(); !ok {
					break
				}
			}
		}
		b.StartTimer()
	}
	b.ReportMetric(float64(rostertest.FleetRosterWorkspaces), "workspaces/publish")
}

// blockedTerminal is a terminal that consumes nothing until it is released,
// standing in for the production pty whose reader — Emacs — has stopped
// draining it.
type blockedTerminal struct {
	release chan struct{}
	entered chan struct{}
	mu      sync.Mutex
	lines   int
}

func newBlockedTerminal() *blockedTerminal {
	return &blockedTerminal{release: make(chan struct{}), entered: make(chan struct{}, 1)}
}

func (t *blockedTerminal) Write(p []byte) (int, error) {
	select {
	case t.entered <- struct{}{}:
	default:
	}
	<-t.release
	t.mu.Lock()
	defer t.mu.Unlock()
	t.lines++
	return len(p), nil
}
