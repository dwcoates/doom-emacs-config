package server

import (
	"context"
	"errors"
	"net"
	"net/http"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/history"
	"agentrepl/shim-store/internal/logging"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// ReadStore is what the store's read-only face may do to the database: the
// three page reads of db.ReadOnly, and nothing else.
type ReadStore interface {
	NewestPage(ctx context.Context, agentID string, stated bool) (*storev1.AgentSessionPage, error)
	PageBefore(ctx context.Context, agentID string, after *storev1.StoreItemPointer) (*storev1.ReadAgentPageSuccess, error)
	PageThrough(ctx context.Context, agentID string, throughAtMs int64) (*storev1.ReadAgentPageSuccess, error)
}

// The database's read-only face satisfies the contract as written.
var _ ReadStore = db.ReadOnly{}

// ReadServer serves storeread.v1.StoreRead: the store as the daemon sees it.
//
// THE DAEMON'S RELATIONSHIP WITH THE STORE IS READ-ONLY (owner ruling,
// 2026-10-27), and this server is built so it cannot be otherwise. It holds a
// ReadStore and nothing that can write, its mux mounts the StoreRead service
// alone, and it is served on a socket of its own, so a client there reaches
// no producer verb whatever it sends. The producers' service (Server) is a
// separate value on a separate socket.
type ReadServer struct {
	store ReadStore
	log   *logging.Logger
	http  *http.Server
}

var _ storereadv1connect.StoreReadHandler = (*ReadServer)(nil)

// NewRead builds the read-only service over store, logging through log.
func NewRead(store ReadStore, log *logging.Logger) *ReadServer {
	if store == nil {
		panic("shim-store server: nil read store")
	}
	if log == nil {
		panic("shim-store server: nil logger")
	}
	s := &ReadServer{store: store, log: log}
	mux := http.NewServeMux()
	path, handler := storereadv1connect.NewStoreReadHandler(s)
	mux.Handle(path, handler)
	s.http = &http.Server{Handler: h2c.NewHandler(mux, &http2.Server{}), ReadHeaderTimeout: 10 * time.Second}
	log.Log(logging.Fields{Operation: "store.read.new"}, "storeread.v1.StoreRead ready")
	return s
}

// Handler exposes the routed handler so an in-process test can mount it on an
// httptest server instead of a socket.
func (s *ReadServer) Handler() http.Handler { return s.http.Handler }

// Serve runs until Shutdown. It returns http.ErrServerClosed after an orderly
// stop, exactly as http.Server does.
func (s *ReadServer) Serve(ln net.Listener) error {
	s.log.Log(logging.Fields{Operation: "store.read.serve"}, "serving storeread.v1.StoreRead")
	err := s.http.Serve(ln)
	if err != nil && !errors.Is(err, http.ErrServerClosed) {
		s.log.Log(logging.Fields{Operation: "store.read.serve", Level: "error"}, "serving ended: %v", err)
		return err
	}
	s.log.Log(logging.Fields{Operation: "store.read.serve"}, "serving stopped")
	return err
}

// Shutdown drains the HTTP server within ctx. The service holds no standing
// stream, so nothing else needs ending first.
func (s *ReadServer) Shutdown(ctx context.Context) error {
	if err := s.http.Shutdown(ctx); err != nil {
		s.log.Log(logging.Fields{Operation: "store.read.shutdown", Level: "error"}, "draining the HTTP server failed: %v", err)
		return err
	}
	s.log.Log(logging.Fields{Operation: "store.read.shutdown"}, "storeread.v1.StoreRead stopped")
	return nil
}

// ---- ReadHistoryPage ----

const opReadHistoryPage = "store.read.read-history-page"

// ReadHistoryPage serves one page of one agent's book in the conversation
// vocabulary: the newest page, the page below a served pointer, or the book as
// it stood at an instant.
func (s *ReadServer) ReadHistoryPage(ctx context.Context, req *connect.Request[storereadv1.ReadHistoryPageRequest]) (*connect.Response[storereadv1.ReadHistoryPageResponse], error) {
	msg := req.Msg
	agentID := msg.GetBook().GetValue()
	log := s.log.With(logging.Fields{RPC: storereadv1connect.StoreReadReadHistoryPageProcedure, RequestID: req.Header().Get(RequestIDHeader), AgentID: agentID, BookAgentID: agentID})
	if ref := validateReadHistoryPageRequest(msg); ref != nil {
		logRefusal(log, opReadHistoryPage, ref, logging.Fields{AgentID: agentID})
		return readHistoryPageFailure(ref), nil
	}
	ctx = correlated(ctx, req.Header())
	var page *conversationv1.HistoryPage
	var err error
	switch position := msg.GetPosition().(type) {
	case *storereadv1.ReadHistoryPageRequest_Newest:
		stated := position.Newest.GetStated() != nil
		opened, readErr := s.store.NewestPage(ctx, agentID, stated)
		page, err = renderPage(readErr, func() (*conversationv1.HistoryPage, error) { return history.OpeningPage(opened) })
	case *storereadv1.ReadHistoryPageRequest_After:
		older, readErr := s.store.PageBefore(ctx, agentID, &storev1.StoreItemPointer{Value: position.After.GetValue()})
		page, err = renderPage(readErr, func() (*conversationv1.HistoryPage, error) { return history.ContinuationPage(older) })
	case *storereadv1.ReadHistoryPageRequest_Through:
		asItStood, readErr := s.store.PageThrough(ctx, agentID, position.Through.GetAtMs())
		page, err = renderPage(readErr, func() (*conversationv1.HistoryPage, error) { return history.ContinuationPage(asItStood) })
	}
	if err != nil {
		var ref *refusal
		if errors.Is(err, history.ErrUndrawable) {
			// THE STORE'S OWN PAGE COULD NOT BE DRAWN: no lower layer saw this,
			// so it is recorded here, once, at ERROR.
			ref = refuseClass(classStorage, SiteDatabaseFailure, "", err.Error())
			logOwnFailure(log, opReadHistoryPage, ref, logging.Fields{AgentID: agentID})
		} else {
			ref = storeFailure(log, opReadHistoryPage, err, logging.Fields{AgentID: agentID, Position: msg.GetAfter().GetValue()})
		}
		return readHistoryPageFailure(ref), nil
	}
	log.LogVerbose(logging.Fields{Operation: opReadHistoryPage, AgentID: agentID}, "page served entries=%d heads=%d", len(page.GetEntries()), len(page.GetHeads()))
	return connect.NewResponse(&storereadv1.ReadHistoryPageResponse{
		Result: &storereadv1.ReadHistoryPageResponse_Success{Success: &storereadv1.ReadHistoryPageSuccess{Page: page}},
	}), nil
}

// renderPage is a read's outcome rendered: the read's own failure, or the page
// drawn in the conversation vocabulary.
func renderPage(readErr error, render func() (*conversationv1.HistoryPage, error)) (*conversationv1.HistoryPage, error) {
	if readErr != nil {
		return nil, readErr
	}
	return render()
}

// validateReadHistoryPageRequest refuses a request naming no book or no
// position. A through instant is the storage layer's to judge, as it is for
// ReadAgentPage.
func validateReadHistoryPageRequest(req *storereadv1.ReadHistoryPageRequest) *refusal {
	if ref := validateAgentID(req.GetBook(), "book"); ref != nil {
		return ref
	}
	switch position := req.GetPosition().(type) {
	case *storereadv1.ReadHistoryPageRequest_Newest:
		if position.Newest.GetKnownAs() == nil {
			return refuse(SitePositionUnset, "newest.known_as", "newest.known_as: neither `stated` nor `named` is set, so whether an absent book is empty or unknown is undecided")
		}
		return nil
	case *storereadv1.ReadHistoryPageRequest_After:
		if position.After.GetValue() == "" {
			return refuse(SitePointerEmpty, "after", "after: a history pointer with no value names no line")
		}
		return nil
	case *storereadv1.ReadHistoryPageRequest_Through:
		return nil
	default:
		return refuse(SitePositionUnset, "position", "position: none of `newest`, `after` or `through` is set")
	}
}

// readHistoryPageFailure is a refusal as the failure arm.
func readHistoryPageFailure(ref *refusal) *connect.Response[storereadv1.ReadHistoryPageResponse] {
	failure := &storereadv1.ReadHistoryPageFailure{Detail: ref.detail}
	switch ref.class {
	case classStalePointer:
		failure.Kind = &storereadv1.ReadHistoryPageFailure_StalePointer{StalePointer: &storereadv1.ReadHistoryPageStalePointer{}}
	case classStorage:
		failure.Kind = &storereadv1.ReadHistoryPageFailure_StorageFailure{StorageFailure: &storereadv1.ReadHistoryPageStorageFailure{}}
	case classUnknownAgent:
		failure.Kind = &storereadv1.ReadHistoryPageFailure_UnknownAgent{UnknownAgent: &storereadv1.ReadHistoryPageUnknownAgent{}}
	default:
		failure.Kind = &storereadv1.ReadHistoryPageFailure_InvalidRequest{
			InvalidRequest: &storereadv1.ReadHistoryPageInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storereadv1.ReadHistoryPageResponse{
		Result: &storereadv1.ReadHistoryPageResponse_Failure{Failure: failure},
	})
}
