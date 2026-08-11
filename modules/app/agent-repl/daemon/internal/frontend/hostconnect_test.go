package frontend

import (
	"bufio"
	"net"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

func newStaticServer(t *testing.T) *Server {
	t.Helper()
	return New(Config{
		Logf:        testLogf(t),
		LogVerbosef: testLogf(t),
		State: staticState{snap: &frontendv1.StateSnapshot{Workspaces: []*frontendv1.WorkspaceState{
			{Workspace: "/w", SessionId: "s1", AtMs: 1, State: frontendv1.RenderState_RENDER_STATE_IDLE},
		}}},
		Handler: &mockHandler{},
	})
}

// A CLIENT THAT CONNECTS BEFORE THE DAEMON CAN SERVE IT IS ACCEPTED AND MADE TO
// WAIT, then served the moment the transport starts accepting. This is the
// whole point of binding the frontend socket at the top of boot: the dial must
// never be refused, and it must never be answered with a partial view.
func TestClientConnectingBeforeServeIsServedOnceReady(t *testing.T) {
	// Arrange: the socket is listening, but nothing is accepting on it yet.
	sock := shortSock(t, "f.sock")
	l, err := ListenUDS(sock)
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	defer l.Close()
	conn, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatalf("dial before serve must succeed, not be refused: %v", err)
	}
	defer conn.Close()

	// Act: the transport begins accepting only now.
	s := newStaticServer(t)
	defer s.Close()
	go s.Serve(l)

	// Assert: the waiting connection gets its whole connect snapshot.
	line, err := bufio.NewReader(conn).ReadBytes('\n')
	if err != nil {
		t.Fatalf("read connect snapshot: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(line, frame); err != nil {
		t.Fatalf("decode connect frame: %v", err)
	}
	if got := len(frame.GetSnapshot().GetWorkspaces()); got != 1 {
		t.Fatalf("connect snapshot workspaces = %d, want the whole view (1)", got)
	}
}

// The host-connect signal fires once the host's connect snapshot has been
// served. Deferred boot work waits on exactly this.
func TestHostConnectSnapshotServedFiresForAHostConnect(t *testing.T) {
	// Arrange.
	s := newStaticServer(t)
	c := newRecordingConn()

	// Act.
	go s.serveClient(c, nil, ClientKindHost)
	c.nextFrame(t)
	defer c.close(causeServerClosed)

	// Assert.
	<-s.HostConnectSnapshotServed()
}

// A GUI connection is not the host, so it must not release work that is
// holding for the host: that is precisely the contention this gate removes.
func TestHostConnectSnapshotServedDoesNotFireForAGUIConnect(t *testing.T) {
	// Arrange.
	s := newStaticServer(t)
	c := newRecordingConn()

	// Act.
	go s.serveClient(c, nil, ClientKindGUIStream)
	c.nextFrame(t)
	defer c.close(causeServerClosed)

	// Assert.
	select {
	case <-s.HostConnectSnapshotServed():
		t.Fatal("a GUI connect released the host-connect signal")
	default:
	}
}
