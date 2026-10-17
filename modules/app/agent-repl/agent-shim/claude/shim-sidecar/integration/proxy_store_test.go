package integration

// proxy_store_test.go — a recording pass-through in front of a test's store,
// for the subjects that read what the sidecar put on the WIRE as well as what
// the store kept.

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"sync"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/proto"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/testclose"
)

// proxyStore is a RECORDING PASS-THROUGH in front of the store the test starts.
//
// Why it exists: the residue arms (`unparsed`, `unknown`, `vendor_specific`)
// are DURABLE but have no read verb — store/v1 serves pages, bash runs, live
// work and cursors, and nothing enumerates unserved items. A subject that must
// assert "this scenario produced ZERO unparsed residue" therefore has to read
// the WIRE. The proxy records every `WriteBatch` and forwards it verbatim, so
// the same run is both fully visible on the wire AND fully durable in the real
// store, and the book/bash assertions still read back through the store's own
// verbs.
//
// It implements the verbs the sidecar's contract allows it to call and
// nothing else: a sidecar reaching any other verb is a contract violation, and
// an `unimplemented` answer states that instead of hiding it.
type proxyStore struct {
	t      *testing.T
	Socket string
	up     storev1connect.ShimStoreClient

	mu      sync.Mutex
	batches []*storev1.WriteBatchRequest

	// gate, when armed, WITHHOLDS the answer to the first batch that matches
	// it — the same interlock the fake store offers, for the subjects that
	// need the REAL store's behavior behind it. See writeGate.
	gate *writeGate

	srv     *http.Server
	ln      net.Listener
	done    chan struct{}
	stopped bool
}

var _ storev1connect.ShimStoreHandler = (*proxyStore)(nil)

// gateOnBatch arms the proxy's one-shot write gate, released at cleanup
// whatever the subject does so a failure can never leave a producer wedged in
// a write.
func (p *proxyStore) gateOnBatch(t *testing.T, match func(*storev1.WriteBatchRequest) bool) *writeGate {
	t.Helper()
	g := &writeGate{match: match, reached: make(chan struct{}), released: make(chan struct{})}
	p.mu.Lock()
	p.gate = g
	p.mu.Unlock()
	t.Cleanup(g.release)
	return g
}

func startProxyStore(t *testing.T, upstream string) *proxyStore {
	t.Helper()
	socket := shortSocketPath(t, "proxy")
	p := &proxyStore{t: t, Socket: socket, up: storeClient(upstream), done: make(chan struct{})}
	mux := http.NewServeMux()
	path, handler := storev1connect.NewShimStoreHandler(p)
	mux.Handle(path, handler)
	ln, err := net.Listen("unix", socket)
	if err != nil {
		t.Fatalf("listen on %s: %v", socket, err)
	}
	p.ln = ln
	p.srv = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{})}
	go func() {
		defer close(p.done)
		_ = p.srv.Serve(ln)
	}()
	t.Cleanup(p.Stop)
	return p
}

func (p *proxyStore) Stop() {
	p.t.Helper()
	p.mu.Lock()
	if p.stopped {
		p.mu.Unlock()
		return
	}
	p.stopped = true
	p.mu.Unlock()
	ctx, cancel := context.WithTimeout(context.Background(), waitBudget)
	defer cancel()
	_ = p.srv.Shutdown(ctx)
	<-p.done
	testclose.RemoveOrFail(p.t, p.Socket)
}

func (p *proxyStore) WriteBatch(ctx context.Context, req *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	recorded := proto.Clone(req.Msg).(*storev1.WriteBatchRequest)
	p.mu.Lock()
	p.batches = append(p.batches, recorded)
	gate := p.gate
	p.mu.Unlock()
	resp, err := p.up.WriteBatch(ctx, connect.NewRequest(req.Msg))
	if err != nil {
		return nil, err
	}
	// The upstream store has already COMMITTED the batch and only the answer is
	// withheld, so a subject waiting on the gate reads the same durable state
	// the producer just wrote.
	gate.hold(recorded)
	return connect.NewResponse(resp.Msg), nil
}

func (p *proxyStore) GetSidecarCursors(ctx context.Context, req *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	resp, err := p.up.GetSidecarCursors(ctx, connect.NewRequest(req.Msg))
	if err != nil {
		return nil, err
	}
	return connect.NewResponse(resp.Msg), nil
}

func (p *proxyStore) GetShellRunClaims(ctx context.Context, req *connect.Request[storev1.GetShellRunClaimsRequest]) (*connect.Response[storev1.GetShellRunClaimsResponse], error) {
	resp, err := p.up.GetShellRunClaims(ctx, connect.NewRequest(req.Msg))
	if err != nil {
		return nil, err
	}
	return connect.NewResponse(resp.Msg), nil
}

func (p *proxyStore) GetRunSettlements(ctx context.Context, req *connect.Request[storev1.GetRunSettlementsRequest]) (*connect.Response[storev1.GetRunSettlementsResponse], error) {
	resp, err := p.up.GetRunSettlements(ctx, connect.NewRequest(req.Msg))
	if err != nil {
		return nil, err
	}
	return connect.NewResponse(resp.Msg), nil
}

func (p *proxyStore) unexpected(verb string) error {
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("the sidecar called %s, which its store contract forbids", verb))
}

func (p *proxyStore) OpenAgentSession(context.Context, *connect.Request[storev1.OpenAgentSessionRequest]) (*connect.Response[storev1.OpenAgentSessionResponse], error) {
	return nil, p.unexpected("OpenAgentSession")
}

func (p *proxyStore) WatchAgentSession(context.Context, *connect.Request[storev1.WatchAgentSessionRequest], *connect.ServerStream[storev1.WatchAgentSessionResponse]) error {
	return p.unexpected("WatchAgentSession")
}

func (p *proxyStore) ReadAgentPage(context.Context, *connect.Request[storev1.ReadAgentPageRequest]) (*connect.Response[storev1.ReadAgentPageResponse], error) {
	return nil, p.unexpected("ReadAgentPage")
}

func (p *proxyStore) WatchBashRun(context.Context, *connect.Request[storev1.WatchBashRunRequest], *connect.ServerStream[storev1.WatchBashRunResponse]) error {
	return p.unexpected("WatchBashRun")
}

func (p *proxyStore) GetWorkflow(context.Context, *connect.Request[storev1.GetWorkflowRequest]) (*connect.Response[storev1.GetWorkflowResponse], error) {
	return nil, p.unexpected("GetWorkflow")
}

func (p *proxyStore) GetLiveWork(context.Context, *connect.Request[storev1.GetLiveWorkRequest]) (*connect.Response[storev1.GetLiveWorkResponse], error) {
	return nil, p.unexpected("GetLiveWork")
}

func (p *proxyStore) GetAgentByVendorTask(context.Context, *connect.Request[storev1.GetAgentByVendorTaskRequest]) (*connect.Response[storev1.GetAgentByVendorTaskResponse], error) {
	return nil, p.unexpected("GetAgentByVendorTask")
}

func (p *proxyStore) GetDetachedWork(context.Context, *connect.Request[storev1.GetDetachedWorkRequest]) (*connect.Response[storev1.GetDetachedWorkResponse], error) {
	return nil, p.unexpected("GetDetachedWork")
}

func (p *proxyStore) GetPeerDelivery(context.Context, *connect.Request[storev1.GetPeerDeliveryRequest]) (*connect.Response[storev1.GetPeerDeliveryResponse], error) {
	return nil, p.unexpected("GetPeerDelivery")
}

func (p *proxyStore) ListResidueShapes(context.Context, *connect.Request[storev1.ListResidueShapesRequest]) (*connect.Response[storev1.ListResidueShapesResponse], error) {
	return nil, p.unexpected("ListResidueShapes")
}

// Batches answers every batch the sidecar wrote, in write order.
func (p *proxyStore) Batches() []*storev1.WriteBatchRequest {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]*storev1.WriteBatchRequest(nil), p.batches...)
}
