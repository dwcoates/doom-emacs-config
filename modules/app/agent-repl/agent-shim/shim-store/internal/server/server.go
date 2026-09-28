package server

import (
	"context"
	"errors"
	"net"
	"net/http"
	"sort"
	"sync"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-store/internal/db"
	"agentrepl/shim-store/internal/logging"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
)

// RequestIDHeader carries a caller's correlation id. When present it is logged
// as `request_id` on every record the request produces.
const RequestIDHeader = "X-Agent-Repl-Request-Id"

// Server serves store.v1.ShimStore.
//
// It implements storev1connect.ShimStoreHandler, so the same object answers
// Connect, gRPC and gRPC-Web with both the binary and JSON codecs; wrapping the
// mux in h2c means an HTTP/1.1 caller and a prior-knowledge HTTP/2 caller reach
// it over the one unix socket with no TLS anywhere.
type Server struct {
	store  Store
	log    *logging.Logger
	tokens *tokenRegistry
	fan    *fanout[LineWritten]
	// bashFan is the WatchBashRun registry. A SECOND REGISTRY, not a second
	// key on the first: a bash row is not a page line, and a book watcher must
	// never be handed one.
	bashFan *fanout[BashRowWritten]
	http    *http.Server

	// done is CLOSED by Shutdown before the HTTP server is drained. Standing
	// watch streams select on it and return cleanly, which is what lets
	// http.Server.Shutdown finish: it waits for handlers, and a pure tail would
	// otherwise never return.
	//
	// THIS IS THE OLD Serve/Close RACE'S REPLACEMENT. Nothing tracks
	// connections by hand any more (the old trackConn-after-Accept snapshot
	// raced with Close); http.Server owns connection lifetime and Shutdown is
	// the only stop.
	done     chan struct{}
	stopOnce sync.Once
}

var _ storev1connect.ShimStoreHandler = (*Server)(nil)

// New builds the service over store, logging through log, giving every watch a
// buffer of watchBuffer frames (non-positive selects DefaultWatchBuffer).
func New(store Store, log *logging.Logger, watchBuffer int) *Server {
	if store == nil {
		panic("shim-store server: nil store")
	}
	if log == nil {
		panic("shim-store server: nil logger")
	}
	s := &Server{
		store:   store,
		log:     log,
		tokens:  newTokenRegistry(),
		fan:     newFanout(watchBuffer, lineKey),
		bashFan: newFanout(watchBuffer, bashRowKey),
		done:    make(chan struct{}),
	}
	s.http = &http.Server{Handler: s.handler(), ReadHeaderTimeout: 10 * time.Second}
	log.Log(logging.Fields{Operation: "store.server.new"}, "store.v1.ShimStore ready watch_buffer=%d", s.fan.buffer)
	return s
}

func (s *Server) handler() http.Handler {
	mux := http.NewServeMux()
	path, connectHandler := storev1connect.NewShimStoreHandler(s)
	// The flusher middleware is INSIDE the mux so the writer it captures is
	// the one Connect writes the stream through.
	mux.Handle(path, s.withResponseFlusher(connectHandler))
	return h2c.NewHandler(mux, &http2.Server{})
}

// Handler exposes the routed handler so an in-process test can mount it on an
// httptest server instead of a socket.
func (s *Server) Handler() http.Handler { return s.http.Handler }

// Serve runs until Shutdown. It returns http.ErrServerClosed after an orderly
// stop, exactly as http.Server does.
func (s *Server) Serve(ln net.Listener) error {
	s.log.Log(logging.Fields{Operation: "store.serve"}, "serving store.v1.ShimStore")
	err := s.http.Serve(ln)
	if err != nil && !errors.Is(err, http.ErrServerClosed) {
		s.log.Log(logging.Fields{Operation: "store.serve", Level: "error"}, "serving ended: %v", err)
		return err
	}
	s.log.Log(logging.Fields{Operation: "store.serve"}, "serving stopped")
	return err
}

// Shutdown ends every standing watch, then drains the HTTP server within ctx.
func (s *Server) Shutdown(ctx context.Context) error {
	s.stopOnce.Do(func() {
		close(s.done)
		s.log.Log(logging.Fields{Operation: "store.shutdown"}, "ending standing watches watchers=%d bash_watchers=%d outstanding_tokens=%d", s.fan.subscribers(), s.bashFan.subscribers(), s.tokens.outstanding())
	})
	if err := s.http.Shutdown(ctx); err != nil {
		s.log.Log(logging.Fields{Operation: "store.shutdown", Level: "error"}, "draining the HTTP server failed: %v", err)
		return err
	}
	s.log.Log(logging.Fields{Operation: "store.shutdown"}, "store.v1.ShimStore stopped")
	return nil
}

// ---- correlation and refusal plumbing ----

// rpcLogger binds the procedure and the caller's correlation id to every record
// one request produces.
func (s *Server) rpcLogger(procedure string, header http.Header) *logging.Logger {
	return s.log.With(logging.Fields{RPC: procedure, RequestID: header.Get(RequestIDHeader)})
}

// correlated carries the caller's request id down to the storage layer.
//
// THE STORAGE LAYER'S RECORDS NEED IT TOO. Its logger is built once at boot and
// belongs to the process, so without this a db record could never say which call
// it belonged to — and "no statement ran for this request" would be
// unassertable, which is exactly the hole that made the suite's
// no-database-touch check vacuous.
func correlated(ctx context.Context, header http.Header) context.Context {
	return logging.ContextWithRequestID(ctx, header.Get(RequestIDHeader))
}

// logRefusal records a refusal exactly once, at its owning layer, with the site
// in its own context key.
//
// THE LEVEL COMES FROM THE CLASS (`refusalClass.logLevel`), never from this
// call site. Every site that says no reaches this one function, so a level
// written here would be one severity for refusals that mean something is wrong
// and refusals that are a verb's ordinary answer alike.
func (s *Server) logRefusal(log *logging.Logger, operation string, ref *refusal, fields logging.Fields) {
	fields.Operation = operation
	fields.Level = ref.class.logLevel()
	fields.RefusalSite = ref.site
	fields.RefusalKind = ref.class.armName()
	log.Log(fields, "refused: %s", ref.detail)
}

// storeRefusal maps a storage-layer error onto this layer's refusal.
//
// THE SITE AND THE FIELD COME FROM THE STORAGE LAYER when it named them,
// because it is the layer that decided them: this one never opens a frame, so
// it cannot know that the upsert changed a row's book or that the residue
// carried no raw record. A failure the storage layer did not classify at all is
// a database failure — never softened into a success, and never guessed at.
func storeRefusal(err error) *refusal {
	site := db.RefusalSite(err)
	field := db.RefusalField(err)
	switch {
	case errors.Is(err, ErrUnknownAgent):
		if site == "" {
			site = SiteUnknownAgent
		}
		return refuseClass(classUnknownAgent, site, field, err.Error())
	case errors.Is(err, ErrStalePointer):
		if site == "" {
			site = SiteStalePointer
		}
		return refuseClass(classStalePointer, site, field, err.Error())
	case errors.Is(err, ErrInvalid):
		if site == "" {
			site = SiteStoreRefusedRequest
		}
		return &refusal{site: site, field: field, detail: err.Error(), class: classInvalid}
	default:
		return refuseClass(classStorage, SiteDatabaseFailure, "", err.Error())
	}
}

// storeFailure classifies a storage-layer error, records it at the weight its
// class deserves, and returns the refusal the failure arm carries.
//
// A REFUSED REQUEST IS NOT A DATABASE FAILURE, and the refusal belongs to the
// CALL. Only this layer knows the call — its procedure, its request id, its
// producer — so this is where a refused request gets its ONE normal-level
// record, whether the storage layer classified it as malformed (ErrInvalid) or
// as a pointer that has moved (ErrStalePointer). The storage layer traces both
// at verbose with its statement and table, which is context, not a second
// record.
//
// A DATABASE FAILURE IS THE OTHER WAY AROUND: internal/db already recorded it at
// `error` with the statement that failed, so this layer adds only a verbose
// trace tying the rpc to it.
func (s *Server) storeFailure(log *logging.Logger, operation string, err error, fields logging.Fields) *refusal {
	ref := storeRefusal(err)
	if ref.class == classInvalid || ref.class == classStalePointer || ref.class == classUnknownAgent {
		s.logRefusal(log, operation, ref, fields)
		return ref
	}
	s.logStoreFailure(log, operation, ref, fields)
	return ref
}

// logStoreFailure records that a request ended in the failure arm BECAUSE of
// the storage layer.
//
// EVERY ERROR IS LOGGED EXACTLY ONCE BY ITS OWNING LAYER, and internal/db
// already logged this one with its own statement and table context — so this
// is a VERBOSE trace that ties the rpc to it, not a second error record. The
// refusals this layer owns (validation, tokens, overflow) go through
// logRefusal instead, at the level their class warrants.
func (s *Server) logStoreFailure(log *logging.Logger, operation string, ref *refusal, fields logging.Fields) {
	fields.Operation = operation
	fields.RefusalSite = ref.site
	fields.Level = "debug"
	log.LogVerbose(fields, "answering the failure arm: %s", ref.detail)
}

// logOwnFailure records a failure this layer OWNS — one no lower layer saw, so
// nothing else will record it.
func (s *Server) logOwnFailure(log *logging.Logger, operation string, ref *refusal, fields logging.Fields) {
	fields.Operation = operation
	fields.RefusalSite = ref.site
	fields.Level = "error"
	log.Log(fields, "failed: %s", ref.detail)
}

// ---- WriteBatch ----

func (s *Server) WriteBatch(ctx context.Context, req *connect.Request[storev1.WriteBatchRequest]) (*connect.Response[storev1.WriteBatchResponse], error) {
	msg := req.Msg
	attribution := batchAttribution(msg.GetBatch())
	attribution.WriteClass = writeClassOf(msg.GetWriteClass()).String()
	log := s.rpcLogger(storev1connect.ShimStoreWriteBatchProcedure, req.Header()).With(attribution)
	log.LogVerbose(logging.Fields{Operation: "store.rpc.write-batch", Producer: msg.GetProducer()},
		"write batch entries=%d shapes=%d cursor_advance=%t", len(msg.GetBatch().GetEntries()), len(msg.GetShapes()), msg.GetBatch().GetCursorAdvance() != nil)

	if ref := validateWriteBatchRequest(msg); ref != nil {
		s.logRefusal(log, "store.rpc.write-batch", ref, logging.Fields{Producer: msg.GetProducer()})
		return writeBatchFailure(ref), nil
	}

	class := writeClassOf(msg.GetWriteClass())
	result, err := s.store.WriteBatch(correlated(ctx, req.Header()), msg.GetProducer(), class, msg.GetBatch(), msg.GetShapes())
	if err != nil {
		// WHAT COMMITTED BEFORE THE FAILURE IS STILL PUBLISHED. A split bulk
		// batch can fail after its leading transactions made lines durable;
		// the retry absorbs them by write_id and publishes nothing, so this is
		// the only moment a live watcher can be told about them.
		s.publish(log, msg.GetProducer(), result.Lines)
		s.publishBashRows(log, msg.GetProducer(), result.BashRows)
		ref := s.storeFailure(log, "store.rpc.write-batch", err, logging.Fields{Producer: msg.GetProducer()})
		return writeBatchFailure(ref), nil
	}

	log.LogVerbose(logging.Fields{Operation: "store.rpc.write-batch", Producer: msg.GetProducer()},
		"batch durable written=%d absorbed=%d skipped=%d page_lines=%d bash_rows=%d shapes=%d",
		result.Written, result.Absorbed, len(result.Skipped), len(result.Lines), len(result.BashRows), result.Shapes)
	s.publish(log, msg.GetProducer(), result.Lines)
	s.publishBashRows(log, msg.GetProducer(), result.BashRows)
	return connect.NewResponse(&storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Success{Success: &storev1.WriteBatchSuccess{
			Skipped: skippedEntries(result.Skipped),
		}},
	}), nil
}

// writeClassOf reads the class the caller stated. Validation has already
// refused a request that stated none, so the unset answer here is only ever
// reached by a caller that skipped validation — and the storage layer refuses
// it again rather than guessing.
func writeClassOf(class *storev1.WriteClass) WriteClass {
	switch class.GetWriteClass().(type) {
	case *storev1.WriteClass_Interactive:
		return WriteInteractive
	case *storev1.WriteClass_Bulk:
		return WriteBulk
	default:
		return db.WriteClassUnset
	}
}

// skippedEntries carries the store's per-entry legacy book-conflict skips onto
// the success arm so the producer can fold them into its own catch-up summary.
// A skip is not a refusal — the batch was durable — so it rides success, never
// failure.
func skippedEntries(skipped []SkippedEntry) []*storev1.WriteBatchSkippedEntry {
	if len(skipped) == 0 {
		return nil
	}
	out := make([]*storev1.WriteBatchSkippedEntry, 0, len(skipped))
	for _, s := range skipped {
		out = append(out, &storev1.WriteBatchSkippedEntry{
			UpsertKey: s.UpsertKey,
			FromBook:  s.FromBook,
			ToBook:    s.ToBook,
		})
	}
	return out
}

// batchAttribution names every agent and book the aggregate write concerns.
// The ordinary singular keys remain available when a request is unambiguous;
// a mixed-agent batch uses arrays rather than lying about one arbitrary entry.
func batchAttribution(batch *storev1.EntryBatch) logging.Fields {
	agents := map[string]struct{}{}
	books := map[string]struct{}{}
	for _, entry := range batch.GetEntries() {
		update := entry.GetAgentUpdate()
		if update == nil {
			continue
		}
		agentID := update.GetTopLevel().GetValue()
		if bookID := update.GetServeableFrame().GetPageAgentId().GetValue(); bookID != "" {
			books[bookID] = struct{}{}
		}
		if agentID != "" {
			agents[agentID] = struct{}{}
		}
	}
	fields := logging.Fields{}
	fields.AgentID, fields.AgentIDs = oneOrMany(agents)
	fields.BookAgentID, fields.BookAgentIDs = oneOrMany(books)
	return fields
}

func oneOrMany(values map[string]struct{}) (string, []string) {
	items := make([]string, 0, len(values))
	for value := range values {
		items = append(items, value)
	}
	sort.Strings(items)
	if len(items) == 1 {
		return items[0], nil
	}
	return "", items
}

// writeBatchFailure builds the typed failure. THE ARM IS WHY, and it is never
// left unset: a caller that received a failure with no kind would have to parse
// `detail` to decide whether retrying its bytes could ever help.
//
// WriteBatch has exactly two arms, and a stale pointer is unreachable on it —
// the verb names no position — so everything that is not a storage failure is a
// request the caller must fix.
func writeBatchFailure(ref *refusal) *connect.Response[storev1.WriteBatchResponse] {
	failure := &storev1.WriteBatchFailure{Detail: ref.detail}
	if ref.class == classStorage {
		failure.Kind = &storev1.WriteBatchFailure_StorageFailure{StorageFailure: &storev1.WriteBatchStorageFailure{}}
	} else {
		failure.Kind = &storev1.WriteBatchFailure_InvalidRequest{
			InvalidRequest: &storev1.WriteBatchInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.WriteBatchResponse{
		Result: &storev1.WriteBatchResponse_Failure{Failure: failure},
	})
}

// publish fans the committed page lines out and warns for every watcher that
// could not keep up. It runs AFTER the commit: nothing is ever published that
// is not already durable.
func (s *Server) publish(log *logging.Logger, producer string, lines []LineWritten) {
	if len(lines) == 0 {
		log.LogVerbose(logging.Fields{Operation: "store.fanout.publish", Producer: producer}, "batch produced no page lines")
		return
	}
	overflowed := s.fan.publish(lines)
	log.LogVerbose(logging.Fields{Operation: "store.fanout.publish", Producer: producer},
		"published lines=%d watchers=%d", len(lines), s.fan.subscribers())
	for _, sub := range overflowed {
		log.Log(logging.Fields{Operation: "store.fanout.overflow", Level: "warn", BookAgentID: sub.key, WatchTokenHash: sub.tokenHash},
			"watch buffer overflowed; ending this subscriber's stream buffer=%d dropped=%d", s.fan.buffer, sub.dropped)
	}
}

// ---- OpenAgentSession ----

func (s *Server) OpenAgentSession(ctx context.Context, req *connect.Request[storev1.OpenAgentSessionRequest]) (*connect.Response[storev1.OpenAgentSessionResponse], error) {
	msg := req.Msg
	agentID := msg.GetAgent().GetValue()
	log := s.rpcLogger(storev1connect.ShimStoreOpenAgentSessionProcedure, req.Header()).With(logging.Fields{AgentID: agentID, BookAgentID: agentID})
	log.LogVerbose(logging.Fields{Operation: "store.rpc.open-agent-session", AgentID: agentID},
		"open page_size=%d known_through=%t page_only=%t", msg.GetPageSize(), msg.KnownThrough != nil, msg.GetPageOnly())

	if ref := validateOpenAgentSessionRequest(msg); ref != nil {
		s.logRefusal(log, "store.rpc.open-agent-session", ref, logging.Fields{AgentID: agentID})
		return openFailure(ref), nil
	}

	opened, err := s.store.OpenPage(correlated(ctx, req.Header()), agentID, msg.GetPageSize(), msg.GetKnownThrough())
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.open-agent-session", err, logging.Fields{AgentID: agentID})
		return openFailure(ref), nil
	}
	if opened.Page == nil {
		ref := refuseClass(classStorage, SiteDatabaseFailure, "", "the store produced no page for this open")
		s.logOwnFailure(log, "store.rpc.open-agent-session", ref, logging.Fields{AgentID: agentID})
		return openFailure(ref), nil
	}

	// A PAGE-ONLY OPEN MINTS NOTHING. OpenAgentSession is unary and the service
	// has no close, so the store cannot learn that a page was abandoned: a token
	// minted for a read that never watched lived for the whole process lifetime.
	// The caller states at the open whether a watch follows, and a page-only
	// open answers with `watch` UNSET — there is nothing to present later, and a
	// watch attempted from it meets the ordinary unknown-token refusal.
	if msg.GetPageOnly() {
		log.LogVerbose(logging.Fields{Operation: "store.rpc.open-agent-session", AgentID: agentID, WriteSeq: opened.PinSeq},
			"page-only read served; no watch token minted lines=%d", len(opened.Page.GetLines()))
		return connect.NewResponse(&storev1.OpenAgentSessionResponse{
			Result: &storev1.OpenAgentSessionResponse_Success{Success: &storev1.OpenAgentSessionSuccess{
				Page: opened.Page,
			}},
		}), nil
	}

	token, err := s.tokens.mint(agentID, opened.PinSeq)
	if err != nil {
		ref := refuseClass(classStorage, SiteDatabaseFailure, "", err.Error())
		s.logOwnFailure(log, "store.rpc.open-agent-session", ref, logging.Fields{AgentID: agentID})
		return openFailure(ref), nil
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.open-agent-session", AgentID: agentID, WatchTokenHash: tokenHash(token), WriteSeq: opened.PinSeq},
		"reading session opened lines=%d", len(opened.Page.GetLines()))
	return connect.NewResponse(&storev1.OpenAgentSessionResponse{
		Result: &storev1.OpenAgentSessionResponse_Success{Success: &storev1.OpenAgentSessionSuccess{
			Page:  opened.Page,
			Watch: &storev1.AgentSessionToken{Value: token},
		}},
	}), nil
}

func openFailure(ref *refusal) *connect.Response[storev1.OpenAgentSessionResponse] {
	failure := &storev1.OpenAgentSessionFailure{Detail: ref.detail}
	switch ref.class {
	case classUnknownAgent:
		failure.Kind = &storev1.OpenAgentSessionFailure_UnknownAgent{UnknownAgent: &storev1.OpenAgentSessionUnknownAgent{}}
	case classStalePointer:
		failure.Kind = &storev1.OpenAgentSessionFailure_StalePointer{StalePointer: &storev1.OpenAgentSessionStalePointer{}}
	case classStorage:
		failure.Kind = &storev1.OpenAgentSessionFailure_StorageFailure{StorageFailure: &storev1.OpenAgentSessionStorageFailure{}}
	default:
		failure.Kind = &storev1.OpenAgentSessionFailure_InvalidRequest{
			InvalidRequest: &storev1.OpenAgentSessionInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.OpenAgentSessionResponse{
		Result: &storev1.OpenAgentSessionResponse_Failure{Failure: failure},
	})
}

// ---- WatchAgentSession ----

// WatchAgentSession is the pure tail of one opened reading session.
//
// THERE IS NO FAILURE ARM BY DESIGN (project lead, 2026-08-29): a refused watch
// — a token never minted, a token already spent, a token from a store that has
// since restarted — closes at the TRANSPORT with connect.CodeNotFound, and the
// caller re-opens. That is the contract's refused-open convention.
func (s *Server) WatchAgentSession(ctx context.Context, req *connect.Request[storev1.WatchAgentSessionRequest], stream *connect.ServerStream[storev1.WatchAgentSessionResponse]) error {
	log := s.rpcLogger(storev1connect.ShimStoreWatchAgentSessionProcedure, req.Header())
	if ref := validateWatchAgentSessionRequest(req.Msg); ref != nil {
		s.logRefusal(log, "store.rpc.watch-agent-session", ref, logging.Fields{})
		return connect.NewError(connect.CodeNotFound, ref)
	}

	token := req.Msg.GetWatch().GetValue()
	hash := tokenHash(token)
	entry, ok := s.tokens.consume(token)
	if !ok {
		ref := refuse(SiteUnknownWatchToken, "watch", "watch: this token was never minted by this store, or has already been spent")
		s.logRefusal(log, "store.rpc.watch-agent-session", ref, logging.Fields{WatchTokenHash: hash})
		return connect.NewError(connect.CodeNotFound, ref)
	}
	log = log.With(logging.Fields{AgentID: entry.agentID, BookAgentID: entry.agentID, WatchTokenHash: hash})

	// SUBSCRIBE BEFORE THE REPLAY QUERY. Everything committed from this instant
	// on reaches the channel, so the replay can only overlap the live stream,
	// never leave a hole in it; the overlap is removed below by write ordinal.
	sub := s.fan.subscribe(entry.agentID, hash)
	defer s.fan.unsubscribe(sub)

	replay, err := s.store.LinesSince(correlated(ctx, req.Header()), entry.agentID, entry.pinSeq)
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.watch-agent-session", err, logging.Fields{WriteSeq: entry.pinSeq})
		return connect.NewError(connect.CodeInternal, ref)
	}
	replayed := make(map[uint64]struct{}, len(replay))
	for _, line := range replay {
		if err := s.send(log, stream, line); err != nil {
			return err
		}
		replayed[line.WriteSeq] = struct{}{}
	}
	// THE HEADERS GO OUT BEFORE THE TAIL BLOCKS. Until they do, the caller's
	// WatchAgentSession call has not returned, so the producer that would write
	// the next line is itself still waiting on this stream. A replay that sent
	// frames has flushed already; this is what covers the empty replay, which
	// is the ordinary case for a watch pinned exactly after its page.
	if err := s.openStream(ctx, log, "store.rpc.watch-agent-session"); err != nil {
		return err
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session", WriteSeq: entry.pinSeq},
		"watch live after replay replayed=%d", len(replay))

	for {
		// Overflow is checked FIRST and on its own, so a subscriber that has
		// already been dropped ends deterministically instead of racing the
		// frames still sitting in its buffer.
		select {
		case <-sub.overflow:
			return s.endOverflowed(log, sub)
		default:
		}
		select {
		case <-sub.overflow:
			return s.endOverflowed(log, sub)
		case <-s.done:
			log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session"}, "watch ended: the store is shutting down")
			return nil
		case <-ctx.Done():
			log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session"}, "watch ended: the caller went away")
			return nil
		case line := <-sub.items:
			if line.WriteSeq <= entry.pinSeq {
				log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session", WriteSeq: line.WriteSeq}, "dropping a line at or below the pin")
				continue
			}
			if _, duplicate := replayed[line.WriteSeq]; duplicate {
				delete(replayed, line.WriteSeq)
				log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session", WriteSeq: line.WriteSeq}, "dropping a line the replay already delivered")
				continue
			}
			if err := s.send(log, stream, line); err != nil {
				return err
			}
		}
	}
}

func (s *Server) send(log *logging.Logger, stream *connect.ServerStream[storev1.WatchAgentSessionResponse], line LineWritten) error {
	// THE ARM IS WHAT HAPPENED TO THE LINE: a retired row goes out on
	// `retired`, carrying the line as it was last served, so the watcher
	// withdraws what it drew rather than drawing it again.
	response := &storev1.WatchAgentSessionResponse{Frame: &storev1.WatchAgentSessionResponse_Line{Line: line.Line}}
	what := "line"
	if line.Retired {
		response = &storev1.WatchAgentSessionResponse{Frame: &storev1.WatchAgentSessionResponse_Retired{Retired: line.Line}}
		what = "retirement"
	}
	if err := stream.Send(response); err != nil {
		log.Log(logging.Fields{Operation: "store.rpc.watch-agent-session", Level: "warn", WriteSeq: line.WriteSeq, Position: line.Line.GetAt().GetValue()},
			"sending a %s to the watcher failed: %v", what, err)
		return err
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.watch-agent-session", WriteSeq: line.WriteSeq, Position: line.Line.GetAt().GetValue()}, "%s delivered", what)
	return nil
}

func (s *Server) endOverflowed(log *logging.Logger, sub *sink[LineWritten]) error {
	log.Log(logging.Fields{Operation: "store.rpc.watch-agent-session", Level: "warn"},
		"watch ended: this subscriber overflowed its buffer and must re-open with known_through dropped=%d", sub.dropped)
	return connect.NewError(connect.CodeResourceExhausted, refuse(SiteWatchBufferOverflow, "watch",
		"watch: the subscriber fell too far behind its buffer; re-open with known_through"))
}

// ---- ReadAgentPage ----

func (s *Server) ReadAgentPage(ctx context.Context, req *connect.Request[storev1.ReadAgentPageRequest]) (*connect.Response[storev1.ReadAgentPageResponse], error) {
	msg := req.Msg
	agentID := msg.GetBook().GetValue()
	log := s.rpcLogger(storev1connect.ShimStoreReadAgentPageProcedure, req.Header()).With(logging.Fields{AgentID: agentID, BookAgentID: agentID})
	log.LogVerbose(logging.Fields{Operation: "store.rpc.read-agent-page", AgentID: agentID, BookAgentID: agentID, Position: msg.GetAfter().GetValue()},
		"read page page_size=%d", msg.GetPageSize())

	if ref := validateReadAgentPageRequest(msg); ref != nil {
		s.logRefusal(log, "store.rpc.read-agent-page", ref, logging.Fields{AgentID: agentID})
		return readPageFailure(ref), nil
	}

	page, err := s.store.ReadPage(correlated(ctx, req.Header()), agentID, msg.GetPageSize(), msg.GetAfter())
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.read-agent-page", err, logging.Fields{AgentID: agentID, Position: msg.GetAfter().GetValue()})
		return readPageFailure(ref), nil
	}
	if page == nil {
		ref := refuseClass(classStorage, SiteDatabaseFailure, "", "the store produced no page for this read")
		s.logOwnFailure(log, "store.rpc.read-agent-page", ref, logging.Fields{AgentID: agentID})
		return readPageFailure(ref), nil
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.read-agent-page", AgentID: agentID}, "page served lines=%d", len(page.GetLines()))
	return connect.NewResponse(&storev1.ReadAgentPageResponse{
		Result: &storev1.ReadAgentPageResponse_Success{Success: page},
	}), nil
}

func readPageFailure(ref *refusal) *connect.Response[storev1.ReadAgentPageResponse] {
	failure := &storev1.ReadAgentPageFailure{Detail: ref.detail}
	switch ref.class {
	case classStalePointer:
		failure.Kind = &storev1.ReadAgentPageFailure_StalePointer{StalePointer: &storev1.ReadAgentPageStalePointer{}}
	case classStorage:
		failure.Kind = &storev1.ReadAgentPageFailure_StorageFailure{StorageFailure: &storev1.ReadAgentPageStorageFailure{}}
	default:
		failure.Kind = &storev1.ReadAgentPageFailure_InvalidRequest{
			InvalidRequest: &storev1.ReadAgentPageInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.ReadAgentPageResponse{
		Result: &storev1.ReadAgentPageResponse_Failure{Failure: failure},
	})
}

// ---- GetWorkflow ----

// GetWorkflow is NOT IMPLEMENTED THIS WAVE and says so in the typed failure
// arm. The workflow table exists and nothing routes into it, so serving a
// synthesized answer would be an invention; the refusal is the honest reply.
func (s *Server) GetWorkflow(_ context.Context, req *connect.Request[storev1.GetWorkflowRequest]) (*connect.Response[storev1.GetWorkflowResponse], error) {
	log := s.rpcLogger(storev1connect.ShimStoreGetWorkflowProcedure, req.Header())
	ref := refuseClass(classNotImplemented, SiteWorkflowNotImplemented, "work", "workflow is not implemented this wave")
	s.logRefusal(log, "store.rpc.get-workflow", ref, logging.Fields{TaskID: req.Msg.GetWork().GetValue()})
	return connect.NewResponse(&storev1.GetWorkflowResponse{
		Result: &storev1.GetWorkflowResponse_Failure{Failure: &storev1.GetWorkflowFailure{
			Detail: ref.detail,
			// `unknown_run` and `invalid_request` are RESERVED FOR THE WORKFLOW
			// WAVE. Nothing routes into the workflow table, so this verb has
			// exactly one honest answer and answering any other arm would be an
			// invention.
			Kind: &storev1.GetWorkflowFailure_NotImplemented{NotImplemented: &storev1.GetWorkflowNotImplemented{}},
		}},
	}), nil
}

// ---- GetLiveWork ----

func (s *Server) GetLiveWork(ctx context.Context, req *connect.Request[storev1.GetLiveWorkRequest]) (*connect.Response[storev1.GetLiveWorkResponse], error) {
	session := req.Msg.GetSession().GetValue()
	log := s.rpcLogger(storev1connect.ShimStoreGetLiveWorkProcedure, req.Header()).With(logging.Fields{AgentID: session})
	log.LogVerbose(logging.Fields{Operation: "store.rpc.get-live-work", AgentID: session}, "reading one session's open obligations")

	if ref := validateGetLiveWorkRequest(req.Msg); ref != nil {
		s.logRefusal(log, "store.rpc.get-live-work", ref, logging.Fields{})
		return liveWorkFailure(ref), nil
	}

	live, err := s.store.LiveWork(correlated(ctx, req.Header()), session)
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.get-live-work", err, logging.Fields{AgentID: session})
		return liveWorkFailure(ref), nil
	}
	if live == nil {
		ref := refuseClass(classStorage, SiteDatabaseFailure, "", "the store produced no live-work answer")
		s.logOwnFailure(log, "store.rpc.get-live-work", ref, logging.Fields{AgentID: session})
		return liveWorkFailure(ref), nil
	}
	log.LogVerbose(logging.Fields{Operation: "store.rpc.get-live-work", AgentID: session},
		"open obligations served agents=%d workflows=%d detached=%d", len(live.GetLiveAgents()), len(live.GetLiveWorkflows()), len(live.GetLiveDetached()))
	return connect.NewResponse(&storev1.GetLiveWorkResponse{
		Result: &storev1.GetLiveWorkResponse_Success{Success: live},
	}), nil
}

// liveWorkFailure has TWO arms: the request named no session (the store never
// answers unscoped), or the database failed.
func liveWorkFailure(ref *refusal) *connect.Response[storev1.GetLiveWorkResponse] {
	failure := &storev1.GetLiveWorkFailure{Detail: ref.detail}
	if ref.class == classStorage {
		failure.Kind = &storev1.GetLiveWorkFailure_StorageFailure{StorageFailure: &storev1.GetLiveWorkStorageFailure{}}
	} else {
		failure.Kind = &storev1.GetLiveWorkFailure_InvalidRequest{
			InvalidRequest: &storev1.GetLiveWorkInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.GetLiveWorkResponse{
		Result: &storev1.GetLiveWorkResponse_Failure{Failure: failure},
	})
}

// ---- ListResidueShapes ----

// ListResidueShapes serves the catalog of key structures observed on lines no
// producer stored.
//
// IT IS A DISCOVERY SURFACE, NOT A DATA PATH. Nothing in the running system
// reads it; it exists so a human can ask what the vendor is emitting that this
// system does not model, without opening the database by hand.
func (s *Server) ListResidueShapes(ctx context.Context, req *connect.Request[storev1.ListResidueShapesRequest]) (*connect.Response[storev1.ListResidueShapesResponse], error) {
	log := s.rpcLogger(storev1connect.ShimStoreListResidueShapesProcedure, req.Header())
	msg := req.Msg
	log.LogVerbose(logging.Fields{Operation: "store.rpc.list-residue-shapes"},
		"reading the residue shape catalog scoped=%t limit=%d example=%t", msg.Kind != nil, msg.GetLimit(), msg.GetIncludeExample())

	if ref := validateListResidueShapesRequest(msg); ref != nil {
		s.logRefusal(log, "store.rpc.list-residue-shapes", ref, logging.Fields{})
		return residueShapesFailure(ref), nil
	}

	shapes, err := s.store.ResidueShapes(correlated(ctx, req.Header()), msg.Kind, msg.GetLimit(), msg.GetIncludeExample())
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.list-residue-shapes", err, logging.Fields{})
		return residueShapesFailure(ref), nil
	}
	// An empty answer is the fresh-catalog answer, not a failure: a store that
	// has observed no unstored residue has no shapes.
	log.LogVerbose(logging.Fields{Operation: "store.rpc.list-residue-shapes"}, "catalog served shapes=%d", len(shapes))
	return connect.NewResponse(&storev1.ListResidueShapesResponse{
		Result: &storev1.ListResidueShapesResponse_Success{Success: &storev1.ListResidueShapesSuccess{Shapes: shapes}},
	}), nil
}

func residueShapesFailure(ref *refusal) *connect.Response[storev1.ListResidueShapesResponse] {
	failure := &storev1.ListResidueShapesFailure{Detail: ref.detail}
	if ref.class == classStorage {
		failure.Kind = &storev1.ListResidueShapesFailure_StorageFailure{StorageFailure: &storev1.ListResidueShapesStorageFailure{}}
	} else {
		failure.Kind = &storev1.ListResidueShapesFailure_InvalidRequest{
			InvalidRequest: &storev1.ListResidueShapesInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.ListResidueShapesResponse{
		Result: &storev1.ListResidueShapesResponse_Failure{Failure: failure},
	})
}

// ---- GetSidecarCursors ----

func (s *Server) GetSidecarCursors(ctx context.Context, req *connect.Request[storev1.GetSidecarCursorsRequest]) (*connect.Response[storev1.GetSidecarCursorsResponse], error) {
	log := s.rpcLogger(storev1connect.ShimStoreGetSidecarCursorsProcedure, req.Header())
	msg := req.Msg
	log.LogVerbose(logging.Fields{Operation: "store.rpc.get-sidecar-cursors", FileID: msg.GetFileId()},
		"reading cursors scoped=%t", msg.FileId != nil)

	if ref := validateGetSidecarCursorsRequest(msg); ref != nil {
		s.logRefusal(log, "store.rpc.get-sidecar-cursors", ref, logging.Fields{})
		return cursorsFailure(ref), nil
	}

	cursors, err := s.store.Cursors(correlated(ctx, req.Header()), msg.FileId)
	if err != nil {
		ref := s.storeFailure(log, "store.rpc.get-sidecar-cursors", err, logging.Fields{FileID: msg.GetFileId()})
		return cursorsFailure(ref), nil
	}
	// An empty answer is the fresh-store answer, not a failure: every tailed
	// file starts from zero.
	log.LogVerbose(logging.Fields{Operation: "store.rpc.get-sidecar-cursors", FileID: msg.GetFileId()}, "cursors served cursors=%d", len(cursors))
	return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Success{Success: &storev1.GetSidecarCursorsSuccess{Cursors: cursors}},
	}), nil
}

func cursorsFailure(ref *refusal) *connect.Response[storev1.GetSidecarCursorsResponse] {
	failure := &storev1.GetSidecarCursorsFailure{Detail: ref.detail}
	if ref.class == classStorage {
		failure.Kind = &storev1.GetSidecarCursorsFailure_StorageFailure{StorageFailure: &storev1.GetSidecarCursorsStorageFailure{}}
	} else {
		failure.Kind = &storev1.GetSidecarCursorsFailure_InvalidRequest{
			InvalidRequest: &storev1.GetSidecarCursorsInvalidRequest{Field: ref.field},
		}
	}
	return connect.NewResponse(&storev1.GetSidecarCursorsResponse{
		Result: &storev1.GetSidecarCursorsResponse_Failure{Failure: failure},
	})
}
