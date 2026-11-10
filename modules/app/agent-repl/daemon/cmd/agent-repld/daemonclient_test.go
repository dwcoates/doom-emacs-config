package main

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync/atomic"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/server"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/stateroot"
)

func TestServingAddressAnswersTheAdvertisedAddress(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "daemon.addr"), []byte("127.0.0.1:4242"), 0o644); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	address, err := servingAddress(layout)

	// Assert.
	if err != nil || address != "127.0.0.1:4242" {
		t.Fatalf("servingAddress = %q, %v; want the advertised address", address, err)
	}
}

func TestServingAddressWithNoAdvertisementIsNoDaemonServing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	_, err = servingAddress(layout)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no daemon is serving: read") {
		t.Fatalf("servingAddress error = %v, want no daemon serving", err)
	}
}

func TestServingAddressWithAnEmptyAdvertisementIsNoDaemonServing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "daemon.addr"), nil, 0o644); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	_, err = servingAddress(layout)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "names no address") {
		t.Fatalf("servingAddress error = %v, want an advertisement naming no address", err)
	}
}

// standingDaemon is a daemon whose roster and footer streams push one frame
// and then stand open, never ending and never pushing 4MiB, as the real ones
// do between changes.
type standingDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	roster *frontendv1.WorkspaceRoster
	footer *frontendv1.FooterView
	// ended is closed once the daemon sees a stream's context end: the
	// reader ended the stream rather than leaving it standing.
	ended chan struct{}
}

func (d *standingDaemon) WatchWorkspaceRoster(ctx context.Context, _ *connect.Request[agentreplv1.WatchWorkspaceRosterRequest], out *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse]) error {
	if err := out.Send(&agentreplv1.WatchWorkspaceRosterResponse{
		Push: &agentreplv1.WatchWorkspaceRosterResponse_Roster{Roster: d.roster},
	}); err != nil {
		return err
	}
	<-ctx.Done()
	close(d.ended)
	return ctx.Err()
}

func (d *standingDaemon) WatchFooter(ctx context.Context, _ *connect.Request[agentreplv1.WatchFooterRequest], out *connect.ServerStream[agentreplv1.WatchFooterResponse]) error {
	if err := out.Send(&agentreplv1.WatchFooterResponse{
		Footer: d.footer,
	}); err != nil {
		return err
	}
	<-ctx.Done()
	close(d.ended)
	return ctx.Err()
}

// serveStandingDaemon serves a standing daemon over h2c, the transport every
// verb dials, and answers it and its loopback address.
func serveStandingDaemon(t *testing.T, roster *frontendv1.WorkspaceRoster) (*standingDaemon, string) {
	t.Helper()
	daemon := &standingDaemon{roster: roster, footer: &frontendv1.FooterView{}, ended: make(chan struct{})}
	path, handler := agentreplv1connect.NewAgentReplHandler(daemon)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	server := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(server.Close)
	return daemon, server.Listener.Addr().String()
}

// assertStreamEnded fails unless the daemon saw the reader end its stream.
//
// NO WALL-CLOCK BOUND DECIDES A DRAIN. A standingDaemon never ends a stream,
// so a reader whose Close drains one never returns: the test hangs until go
// test's -timeout, whose goroutine dump names the blocked Close. A bound on the
// healthy open, read and close instead failed whenever the host was loaded
// past it (raised twice, then exceeded again at 780ms under the merge gate,
// 2026-10-19), so it failed healthy readers and decided nothing a hang does
// not.
func assertStreamEnded(t *testing.T, daemon *standingDaemon) {
	t.Helper()
	<-daemon.ended
}

// rosterRow is the row of workspace ws-1 at DIR, named NAME.
func rosterRow(dir, name string) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-1", Dir: dir}},
		Name:      &frontendv1.RosterRowName{Text: name},
	}
}

// roster is a roster of ROWS, each a top-level open row, in one repository
// section.
func roster(rows ...*frontendv1.RosterRow) *frontendv1.WorkspaceRoster {
	open := make([]*frontendv1.RosterOpenRow, 0, len(rows))
	for _, row := range rows {
		open = append(open, &frontendv1.RosterOpenRow{Row: row})
	}
	return &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{
		Sections: []*frontendv1.RosterRepoSection{{Entries: rosterwalk.RowEntries(open)}},
	}}
}

func TestReadRosterReturnsFromAStandingRosterStream(t *testing.T) {
	// Arrange.
	dir := canonicalDir(t.TempDir())
	daemon, address := serveStandingDaemon(t, roster(rosterRow(dir, "one")))
	ctx := t.Context()

	// Act.
	got, err := readRoster(ctx, newDaemonClient(address))

	// Assert.
	if err != nil || findRosterRow(got, dir) == nil {
		t.Fatalf("readRoster = %v, %v; want the roster holding ws-1", got, err)
	}
	assertStreamEnded(t, daemon)
}

func TestDaemonStreamCloseEndsAStandingStream(t *testing.T) {
	// Arrange.
	daemon, address := serveStandingDaemon(t, roster())
	ctx := t.Context()
	stream, err := openDaemonStream(ctx, newDaemonClient(address).WatchWorkspaceRoster, &agentreplv1.WatchWorkspaceRosterRequest{})
	if err != nil {
		t.Fatalf("openDaemonStream = %v", err)
	}
	if !stream.Receive() {
		t.Fatalf("no first frame: %v", stream.Err())
	}

	// Act.
	_ = stream.Close()

	// Assert.
	assertStreamEnded(t, daemon)
}

func TestOpenDaemonStreamAnswersTheOpenError(t *testing.T) {
	// Arrange.
	refused := errors.New("refused")
	open := func(context.Context, *connect.Request[agentreplv1.WatchFooterRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchFooterResponse], error) {
		return nil, refused
	}

	// Act.
	_, err := openDaemonStream(context.Background(), open, &agentreplv1.WatchFooterRequest{})

	// Assert.
	if !errors.Is(err, refused) {
		t.Fatalf("openDaemonStream error = %v, want the open's own", err)
	}
}

// serverStreamMethods names the daemon client's server-streaming methods.
func serverStreamMethods() map[string]bool {
	client := reflect.TypeOf((*agentreplv1connect.AgentReplClient)(nil)).Elem()
	methods := map[string]bool{}
	for i := 0; i < client.NumMethod(); i++ {
		m := client.Method(i)
		if m.Type.NumOut() > 0 && strings.Contains(m.Type.Out(0).String(), "ServerStreamForClient") {
			methods[m.Name] = true
		}
	}
	return methods
}

func TestEveryVerbOpensItsServerStreamsThroughOpenDaemonStream(t *testing.T) {
	// Arrange.
	streaming := serverStreamMethods()
	if len(streaming) == 0 {
		t.Fatal("the daemon client has no server-streaming method; the scan below would pass vacuously")
	}
	fset := token.NewFileSet()
	var direct []string

	// Act.
	for _, source := range sourcescan.Production(t) {
		file, err := parser.ParseFile(fset, source.Name, source.Source, 0)
		if err != nil {
			t.Fatalf("parse %s: %v", source.Name, err)
		}
		ast.Inspect(file, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			if sel, ok := call.Fun.(*ast.SelectorExpr); ok && streaming[sel.Sel.Name] {
				direct = append(direct, fset.Position(call.Pos()).String()+" "+sel.Sel.Name)
			}
			return true
		})
	}

	// Assert.
	if len(direct) > 0 {
		t.Fatalf("server streams opened by a direct call, so their Close can drain a standing stream; open them with openDaemonStream:\n%s",
			strings.Join(direct, "\n"))
	}
}

// createDaemon is a daemon at a test server that answers every create with
// ANSWER, counting the creates it was sent.
type createDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	answer  *agentreplv1.CreateWorkspaceResponse
	creates atomic.Int32
}

func (d *createDaemon) CreateWorkspace(context.Context, *connect.Request[agentreplv1.CreateWorkspaceRequest]) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	d.creates.Add(1)
	return connect.NewResponse(d.answer), nil
}

// serveCreates runs D at a test server and answers its address.
func serveCreates(t *testing.T, d *createDaemon) string {
	t.Helper()
	path, handler := agentreplv1connect.NewAgentReplHandler(d)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	server := httptest.NewServer(mux)
	t.Cleanup(server.Close)
	return strings.TrimPrefix(server.URL, "http://")
}

func created() *agentreplv1.CreateWorkspaceResponse {
	return &agentreplv1.CreateWorkspaceResponse{Result: &agentreplv1.CreateWorkspaceResponse_Success{Success: &agentreplv1.CreateWorkspaceSuccess{}}}
}

func TestACallRefusedByAHandingOverDaemonIsSentToItsSuccessor(t *testing.T) {
	// Arrange
	successor := &createDaemon{answer: created()}
	successorAddr := serveCreates(t, successor)
	incumbent := &createDaemon{answer: transferringAway(successorAddr)}
	var note strings.Builder
	client := newRedirectingClient(http.DefaultTransport, serveCreates(t, incumbent), &note)

	// Act
	resp, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace = %v, %v; want the successor's success", resp, err)
	}
	if incumbent.creates.Load() != 1 || successor.creates.Load() != 1 {
		t.Fatalf("creates: incumbent %d, successor %d; want one each", incumbent.creates.Load(), successor.creates.Load())
	}
	if !strings.Contains(note.String(), "sending the call to its successor at "+successorAddr) {
		t.Fatalf("note %q, want the resend named", note.String())
	}
}

func TestACallAfterARedirectGoesStraightToTheSuccessor(t *testing.T) {
	// Arrange
	successor := &createDaemon{answer: created()}
	successorAddr := serveCreates(t, successor)
	incumbent := &createDaemon{answer: transferringAway(successorAddr)}
	client := newRedirectingClient(http.DefaultTransport, serveCreates(t, incumbent), io.Discard)
	if _, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{})); err != nil {
		t.Fatalf("first CreateWorkspace: %v", err)
	}

	// Act
	_, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{}))

	// Assert
	if err != nil || incumbent.creates.Load() != 1 || successor.creates.Load() != 2 {
		t.Fatalf("err %v; creates: incumbent %d, successor %d; want the second call at the successor only", err, incumbent.creates.Load(), successor.creates.Load())
	}
}

func TestASuccessorThatIsHandingOverTooIsNotChased(t *testing.T) {
	// Arrange
	beyond := &createDaemon{answer: created()}
	successor := &createDaemon{answer: transferringAway(serveCreates(t, beyond))}
	incumbent := &createDaemon{answer: transferringAway(serveCreates(t, successor))}
	client := newRedirectingClient(http.DefaultTransport, serveCreates(t, incumbent), io.Discard)

	// Act
	resp, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{}))

	// Assert
	if err != nil || resp.Msg.GetError().GetTransferringAway() == nil || beyond.creates.Load() != 0 {
		t.Fatalf("CreateWorkspace = %v, %v; beyond %d; want the successor's refusal and nothing sent further", resp, err, beyond.creates.Load())
	}
}

func TestAnAnswerThatIsNoHandoverIsNotResent(t *testing.T) {
	// Arrange
	daemon := &createDaemon{answer: created()}
	client := newRedirectingClient(http.DefaultTransport, serveCreates(t, daemon), io.Discard)

	// Act
	_, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{}))

	// Assert
	if err != nil || daemon.creates.Load() != 1 {
		t.Fatalf("err %v, creates %d; want the one call", err, daemon.creates.Load())
	}
}

func TestTransferredToFindsTheRefusalsSuccessor(t *testing.T) {
	tests := []struct {
		name   string
		answer *agentreplv1.CreateWorkspaceResponse
		want   string
	}{
		{name: "a handover refusal names its successor", answer: transferringAway("127.0.0.1:2"), want: "127.0.0.1:2"},
		{name: "a success names none", answer: created(), want: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := transferredTo(tt.answer.ProtoReflect(), 0)

			// Assert
			if got != tt.want {
				t.Fatalf("transferredTo = %q, want %q", got, tt.want)
			}
		})
	}
}

// erroringDaemon answers every create with a transferring_away refusal in its
// error shape, naming SUCCESSOR in its metadata, as a verb with no typed arm
// does.
type erroringDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	successor string
}

func (d *erroringDaemon) CreateWorkspace(context.Context, *connect.Request[agentreplv1.CreateWorkspaceRequest]) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	err := connect.NewError(connect.CodeFailedPrecondition, errors.New("intended arm: CreateWorkspaceError.transferring_away: handed over"))
	err.Meta().Set(server.SuccessorHeader, d.successor)
	return nil, err
}

func TestACallRefusedAsAnErrorByAHandingOverDaemonIsSentToItsSuccessor(t *testing.T) {
	// Arrange
	successor := &createDaemon{answer: created()}
	incumbent := &erroringDaemon{successor: serveCreates(t, successor)}
	path, handler := agentreplv1connect.NewAgentReplHandler(incumbent)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	srv := httptest.NewServer(mux)
	t.Cleanup(srv.Close)
	client := newRedirectingClient(http.DefaultTransport, strings.TrimPrefix(srv.URL, "http://"), io.Discard)

	// Act
	resp, err := client.CreateWorkspace(context.Background(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil || successor.creates.Load() != 1 {
		t.Fatalf("CreateWorkspace = %v, %v; successor creates %d; want the successor's success", resp, err, successor.creates.Load())
	}
}
