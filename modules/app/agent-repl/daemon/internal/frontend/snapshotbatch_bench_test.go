package frontend

import (
	"bufio"
	"net"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// benchFleet builds the shape measured live: 178 workspaces, 12 of them live.
func benchFleet() *frontendv1.StateSnapshot {
	states := make([]*frontendv1.WorkspaceState, 0, 178)
	for i := 0; i < 178; i++ {
		ws := &frontendv1.WorkspaceState{
			Workspace:              pad("/Users/x/workspace/ws", i),
			SessionId:              pad("sess-0000-0000-0000-", i),
			Fence:                  pad("fence-", i),
			AtMs:                   int64(i),
			ControllerGenerationId: pad("gen-", i),
			State:                  frontendv1.RenderState_RENDER_STATE_IDLE,
			Connectivity:           frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED,
		}
		if i%15 == 0 {
			ws.Connectivity = frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL
		}
		states = append(states, ws)
	}
	return &frontendv1.StateSnapshot{Workspaces: states}
}

// TestConnectLeadBatchArrivesBeforeTheFleet measures, on a real socket, how
// much of the fleet the host can start applying at the FIRST readable frame.
// The recovery budget is per workspace, so what matters is when the first
// batch lands, not when the last one does.
func TestConnectLeadBatchArrivesBeforeTheFleet(t *testing.T) {
	// Arrange.
	sock := shortSock(t, "bench.sock")
	l, err := ListenUDS(sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer l.Close()
	s := New(Config{
		Logf:        func(string, ...any) {},
		LogVerbosef: func(string, ...any) {},
		State:       staticState{snap: benchFleet()},
		Handler:     &mockHandler{},
	})
	defer s.Close()
	go s.Serve(l)

	// Act.
	start := time.Now()
	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)
	line, err := r.ReadBytes('\n')
	if err != nil {
		t.Fatalf("read lead batch: %v", err)
	}
	lead := time.Since(start)
	leadBytes := len(line)

	// Assert: the lead frame is a small fraction of the fleet's bytes, which is
	// what lets the host start applying immediately.
	batches := splitConnectSnapshot(benchFleet())
	t.Logf("lead batch: %d workspaces, %d bytes, readable %s after dial; delivery is %d batches",
		len(batches[0].GetWorkspaces()), leadBytes, lead, len(batches))
	if len(batches[0].GetWorkspaces()) >= 178 {
		t.Fatalf("lead batch carries the whole fleet (%d); batching bought nothing",
			len(batches[0].GetWorkspaces()))
	}
}

func BenchmarkSplitConnectSnapshot(b *testing.B) {
	snap := benchFleet()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		splitConnectSnapshot(snap)
	}
}
