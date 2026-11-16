package main

import (
	"context"
	"errors"
	"go/ast"
	"go/parser"
	"go/token"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"agentrepl/protohelpers/rosterwalk"

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
		Push: &agentreplv1.WatchWorkspaceRosterResponse_Streams{Streams: &frontendv1.RosterPush{
			Revision: 1, Workspaces: d.roster, MergeQueue: &frontendv1.RosterMergeQueueSection{},
			RecentlyMerged: &frontendv1.RosterMergedSection{}, Closed: &frontendv1.RosterClosedRows{}}},
	}); err != nil {
		return err
	}
	<-ctx.Done()
	close(d.ended)
	return ctx.Err()
}

// LocateWorkspace answers the roster's rows at the request's exact directory,
// as the registry would.
func (d *standingDaemon) LocateWorkspace(_ context.Context, req *connect.Request[agentreplv1.LocateWorkspaceRequest]) (*connect.Response[agentreplv1.LocateWorkspaceResponse], error) {
	var refs []*workspacev1.WorkspaceRef
	for _, row := range rosterwalk.OpenRows(d.roster) {
		row := row.GetRow()
		if ref := row.GetWorkspace().GetWorkspace(); ref.GetDir() == req.Msg.GetAt().GetDir() {
			refs = append(refs, ref)
		}
	}
	return connect.NewResponse(&agentreplv1.LocateWorkspaceResponse{Result: &agentreplv1.LocateWorkspaceResponse_Success{
		Success: &agentreplv1.LocateWorkspaceSuccess{Workspaces: refs}}}), nil
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
	if err != nil || len(rosterwalk.OpenRows(got)) != 1 || rosterwalk.OpenRows(got)[0].GetRow().GetWorkspace().GetWorkspace().GetDir() != dir {
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

func TestReadRosterRefusesAFirstFrameWithNoWorkspacesStream(t *testing.T) {
	// Arrange: the first frame leaves the workspaces stream out, which the
	// contract forbids of a subscription's first push.
	_, address := serveStandingDaemon(t, nil)

	// Act.
	got, err := readRoster(t.Context(), newDaemonClient(address))

	// Assert.
	if err == nil || got != nil || !strings.Contains(err.Error(), "carried no workspaces stream") {
		t.Fatalf("readRoster = %v, %v; want a refusal naming the missing stream", got, err)
	}
}
