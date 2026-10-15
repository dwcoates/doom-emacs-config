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
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"

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

// standingStreamBound bounds every standing-stream test: just over twice the
// healthy open, read, and close of one stream on the loopback under the merge
// gate's load (measured 2026-10-15 at worst 322ms over 400 footer reads at
// background priority with a load average near 115; ~3ms on an idle host). A
// Close that drains a standing stream blocks until the bound runs out, so the
// reader's context ending first is the failure, however loaded the host.
const standingStreamBound = 750 * time.Millisecond

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
	daemon := &standingDaemon{roster: roster, footer: failedFooter("tests"), ended: make(chan struct{})}
	path, handler := agentreplv1connect.NewAgentReplHandler(daemon)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	server := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(server.Close)
	return daemon, server.Listener.Addr().String()
}

// boundedContext is a context that ends at standingStreamBound.
func boundedContext(t *testing.T) context.Context {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), standingStreamBound)
	t.Cleanup(cancel)
	return ctx
}

// assertStreamEnded fails unless the daemon saw the reader end its stream
// and the reader finished before CTX's bound.
func assertStreamEnded(t *testing.T, ctx context.Context, daemon *standingDaemon) {
	t.Helper()
	if ctx.Err() != nil {
		t.Fatalf("the reader returned only once its %s bound ran out: it drained a standing stream", standingStreamBound)
	}
	select {
	case <-daemon.ended:
	case <-ctx.Done():
		t.Fatalf("the daemon never saw the stream end: %v", ctx.Err())
	}
}

func TestDaemonStreamCloseEndsAStandingStream(t *testing.T) {
	// Arrange.
	daemon, address := serveStandingDaemon(t, roster())
	ctx := boundedContext(t)
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
	assertStreamEnded(t, ctx, daemon)
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
