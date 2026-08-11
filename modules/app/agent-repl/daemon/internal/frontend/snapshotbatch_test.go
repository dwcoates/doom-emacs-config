package frontend

import (
	"bufio"
	"net"
	"net/http"
	"net/http/httptest"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"github.com/gorilla/websocket"
	"google.golang.org/protobuf/encoding/protojson"
)

func liveState(ws string, atMs int64) *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		Workspace:    ws,
		SessionId:    "s-" + ws,
		AtMs:         atMs,
		Connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL,
	}
}

func dormantState(ws string, atMs int64) *frontendv1.WorkspaceState {
	return &frontendv1.WorkspaceState{
		Workspace:    ws,
		AtMs:         atMs,
		Connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED,
	}
}

func fleet(t *testing.T, live, dormant int) []*frontendv1.WorkspaceState {
	t.Helper()
	states := make([]*frontendv1.WorkspaceState, 0, live+dormant)
	// Dormant first on the wire, so a passing ordering assertion cannot be the
	// input order surviving untouched.
	for i := 0; i < dormant; i++ {
		states = append(states, dormantState(pad("/dormant", i), int64(i)))
	}
	for i := 0; i < live; i++ {
		states = append(states, liveState(pad("/live", i), int64(1000+i)))
	}
	return states
}

func pad(prefix string, i int) string {
	digits := []byte{byte('0' + (i/100)%10), byte('0' + (i/10)%10), byte('0' + i%10)}
	return prefix + string(digits)
}

func TestSplitConnectSnapshotPutsEveryLiveWorkspaceInTheFirstBatch(t *testing.T) {
	// Arrange: more live workspaces than the lead batch floor, buried behind a
	// fleet-scale run of dormant ones.
	snap := &frontendv1.StateSnapshot{Workspaces: fleet(t, leadBatchFloor+9, 150)}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert: not one live workspace waits for a later batch.
	if len(batches) < 2 {
		t.Fatalf("batches = %d, want the fleet split across several", len(batches))
	}
	lead := map[string]bool{}
	for _, ws := range batches[0].GetWorkspaces() {
		lead[ws.GetWorkspace()] = true
	}
	for _, ws := range snap.GetWorkspaces() {
		if workspaceLiveOnConnect(ws) && !lead[ws.GetWorkspace()] {
			t.Fatalf("live workspace %q is not in the first batch", ws.GetWorkspace())
		}
	}
}

func TestSplitConnectSnapshotOrdersLiveWorkspacesMostRecentFirst(t *testing.T) {
	// Arrange.
	snap := &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{
		liveState("/old", 10),
		dormantState("/dormant", 99),
		liveState("/new", 20),
	}}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert: live before dormant, newest live first.
	got := []string{}
	for _, ws := range batches[0].GetWorkspaces() {
		got = append(got, ws.GetWorkspace())
	}
	want := []string{"/new", "/old", "/dormant"}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("connect order = %v, want %v", got, want)
		}
	}
}

func TestSplitConnectSnapshotStatesTheSameTotalOnEveryBatch(t *testing.T) {
	// Arrange: the real fleet's shape.
	snap := &frontendv1.StateSnapshot{Workspaces: fleet(t, 12, 166)}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert: every batch names the whole delivery, and the batches together
	// carry every workspace exactly once.
	seen := map[string]int{}
	for i, batch := range batches {
		if batch.GetWorkspaceTotal() != 178 {
			t.Fatalf("batch %d total = %d, want 178", i, batch.GetWorkspaceTotal())
		}
		if int(batch.GetWorkspaceBatchIndex()) != i {
			t.Fatalf("batch %d index = %d", i, batch.GetWorkspaceBatchIndex())
		}
		for _, ws := range batch.GetWorkspaces() {
			seen[ws.GetWorkspace()]++
		}
	}
	if len(seen) != 178 {
		t.Fatalf("distinct workspaces delivered = %d, want 178", len(seen))
	}
	for ws, n := range seen {
		if n != 1 {
			t.Fatalf("workspace %q delivered %d times, want exactly once", ws, n)
		}
	}
}

func TestSplitConnectSnapshotKeepsWholesaleFieldsOnTheLeadBatchOnly(t *testing.T) {
	// Arrange: the fields the host rebuilds wholesale must be stated once.
	snap := &frontendv1.StateSnapshot{
		Workspaces: fleet(t, 2, 100),
		Sessions:   []*frontendv1.SessionView{{Workspace: "/live000"}},
		Inits:      []*frontendv1.SessionInitView{{Workspace: "/live000"}},
		Daemon:     &frontendv1.DaemonView{},
	}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert.
	if len(batches[0].GetSessions()) != 1 || len(batches[0].GetInits()) != 1 || batches[0].GetDaemon() == nil {
		t.Fatalf("lead batch dropped a wholesale field: %+v", batches[0])
	}
	for i, batch := range batches[1:] {
		if len(batch.GetSessions()) != 0 || len(batch.GetInits()) != 0 || batch.GetDaemon() != nil {
			t.Fatalf("continuation batch %d restates a wholesale field: %+v", i+1, batch)
		}
	}
}

func TestSplitConnectSnapshotOfASmallFleetIsOneBatch(t *testing.T) {
	// Arrange.
	snap := &frontendv1.StateSnapshot{Workspaces: fleet(t, 1, 1)}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert: one batch, and it names itself the whole delivery.
	if len(batches) != 1 {
		t.Fatalf("batches = %d, want 1", len(batches))
	}
	if batches[0].GetWorkspaceTotal() != 2 {
		t.Fatalf("total = %d, want 2", batches[0].GetWorkspaceTotal())
	}
}

func TestSplitConnectSnapshotOfAnEmptyFleetStillDeliversTheGlobals(t *testing.T) {
	// Arrange: no workspaces at all.
	snap := &frontendv1.StateSnapshot{Daemon: &frontendv1.DaemonView{}}

	// Act.
	batches := splitConnectSnapshot(snap)

	// Assert: one batch, carrying the globals, declaring an empty fleet.
	if len(batches) != 1 || batches[0].GetDaemon() == nil || batches[0].GetWorkspaceTotal() != 0 {
		t.Fatalf("empty fleet batches = %+v", batches)
	}
}

// A GUI CLIENT IS NEVER BATCHED. Its reader ADOPTS a snapshot as the whole
// state, so a second batch would replace the first rather than extend it.
func TestGUIClientReceivesTheWholeFleetInOneFrame(t *testing.T) {
	// Arrange: an UNSCOPED GUI client, which sees every workspace.
	s := New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		State:       staticState{snap: &frontendv1.StateSnapshot{Workspaces: fleet(t, 3, 120)}},
		Handler:     &mockHandler{},
	})
	defer s.Close()

	// Act: the unscoped bootstrap socket, the one GUI client that sees the
	// whole fleet.
	httpSrv := httptest.NewServer(http.HandlerFunc(s.ServeWS))
	defer httpSrv.Close()
	conn, _, err := websocket.DefaultDialer.Dial("ws"+httpSrv.URL[len("http"):], nil)
	if err != nil {
		t.Fatalf("ws dial: %v", err)
	}
	defer conn.Close()
	snap := readWSSnapshot(t, conn)

	// Assert: one frame, the whole fleet.
	if got := len(snap.GetWorkspaces()); got != 123 {
		t.Fatalf("GUI connect snapshot workspaces = %d, want the whole fleet (123)", got)
	}
}

// A connect delivers EVERY batch on the wire, in order, with the lead first.
func TestConnectDeliversEveryBatchInOrder(t *testing.T) {
	// Arrange.
	sock := shortSock(t, "batch.sock")
	l, err := ListenUDS(sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer l.Close()
	s := New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		State:       staticState{snap: &frontendv1.StateSnapshot{Workspaces: fleet(t, 3, 120)}},
		Handler:     &mockHandler{},
	})
	defer s.Close()
	go s.Serve(l)

	// Act.
	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatalf("dial: %v", err)
	}
	defer conn.Close()
	r := bufio.NewReader(conn)

	// Assert: batches arrive in index order and converge on the full fleet.
	seen := map[string]bool{}
	total := 0
	for next := 0; ; next++ {
		line, err := r.ReadBytes('\n')
		if err != nil {
			t.Fatalf("read batch %d: %v", next, err)
		}
		frame := &frontendv1.FrontendFrame{}
		if err := protojson.Unmarshal(line, frame); err != nil {
			t.Fatalf("decode batch %d: %v", next, err)
		}
		batch := frame.GetSnapshot()
		if batch == nil {
			t.Fatalf("frame %d is not a snapshot: %v", next, frame)
		}
		if int(batch.GetWorkspaceBatchIndex()) != next {
			t.Fatalf("batch index = %d, want %d", batch.GetWorkspaceBatchIndex(), next)
		}
		total = int(batch.GetWorkspaceTotal())
		for _, ws := range batch.GetWorkspaces() {
			seen[ws.GetWorkspace()] = true
		}
		if len(seen) == total {
			break
		}
	}
	if total != 123 || len(seen) != 123 {
		t.Fatalf("converged view = %d workspaces (total %d), want 123", len(seen), total)
	}
}
