// Package storeclient is the sidecar's Connect client to the store: it recovers
// file cursors (GetSidecarCursors) and writes record batches (WriteBatch).
// Those two verbs are the sidecar's WHOLE store surface — the read side
// (OpenAgentSession/WatchAgentSession/ReadAgentPage/GetWorkflow/GetLiveWork)
// belongs to the shim and is never called from here.
//
// TRANSPORT. Plain Connect protocol with the binary (protobuf) codec over an
// http.Transport whose DialContext opens the store's unix socket. Both verbs
// are unary, so HTTP/1.1 carries them and no h2c upgrade is needed.
//
// THERE IS NO CONNECTION TO HOLD, AND NO HEALTH VERB TO ASK. The old
// length-prefixed Any-over-UDS framing, its Subscribe/Ack dial protocol, the
// ConnectionHeartbeat and the Health probe are all deleted from the contract:
// streams and the transport own liveness, so LIVENESS IS SIMPLY WHETHER THE
// LAST RPC WORKED. A client value is therefore always usable; what varies is
// whether a call succeeds.
//
// THE RESPONSE IS ALWAYS READ. Every response is oneof{success|failure}, and a
// failure is a REFUSAL on a healthy transport rather than a transport error —
// the two are distinguished because the cycle reacts to both by suspending
// production but reports them differently. A response carrying neither arm is
// itself an error: an unset oneof is illegal, never an empty success.
//
// Sad path: nothing is buffered and nothing is spilled. A failed WriteBatch
// means NOTHING was committed, so the caller simply does not advance its
// cursor and re-reads the same durable bytes from the last committed position.
// A call this producer ABANDONED is weaker than a failure and is not the same
// thing: the store may commit it anyway, so the outcome is unknown rather than
// negative — and the same re-read from the store's cursor covers both.
package storeclient

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"connectrpc.com/connect"
)

// Producer is the sidecar's fixed WriteBatchRequest producer identity.
const Producer = "shim-claude-sidecar"

// bulkWriteClass is the class every sidecar write states.
var bulkWriteClass = &storev1.WriteClass{WriteClass: &storev1.WriteClass_Bulk{Bulk: &storev1.WriteClassBulk{}}}

// SkippedEntry is one entry the store left UNCHANGED because its upsert_key
// already names a row under a different book — the store's re-ingest idempotency
// answer, carried on the WriteBatch success arm. It is NOT a refusal: the batch
// was durable and every other entry committed. The producer folds these into
// its own startup catch-up summary; a skip in steady state is unexpected.
type SkippedEntry struct {
	UpsertKey string
	FromBook  string
	ToBook    string
}

// RPC names, carried in the `rpc` log key so a sidecar record joins against the
// store's record for the same call.
const (
	rpcWriteBatch        = storev1connect.ShimStoreWriteBatchProcedure
	rpcGetSidecarCursors = storev1connect.ShimStoreGetSidecarCursorsProcedure
	rpcGetShellRunClaims = storev1connect.ShimStoreGetShellRunClaimsProcedure

	// WriteBatchSite is the `refusal_site` a refused write is reported under.
	// A caller that states the refusal itself — the cycle parking a file on an
	// invalid_request — names the same site, so the two records join.
	WriteBatchSite = rpcWriteBatch
)

// baseURL is a syntactic requirement of the Connect client: the unix socket is
// selected by the transport's dialer, so the authority never reaches a network.
const baseURL = "http://store"

// dialTimeout bounds opening the store socket. A store that is not listening
// fails immediately; this only bounds a socket that accepts and then stalls.
const dialTimeout = 5 * time.Second

// RefusalError is a store REFUSAL: the rpc completed and the store answered
// with its failure arm. It is distinct from a transport error because the store
// was reachable and said no, which is a different thing to investigate.
//
// THE KIND IS THE WHOLE POINT OF THE ARM (endpoint_write_batch.proto): a
// storage_failure is a transaction that failed and a retry may succeed, while
// an invalid_request is a batch the store can never accept — retrying the same
// bytes is a tight identical loop that makes no progress and drowns the log.
// The caller reacts differently to each, so the kind travels with the error
// rather than being re-derived from the detail text, which is documented as
// never switched on.
type RefusalError struct {
	RPC    string
	Detail string
	// Kind is the failure's oneof arm. It is empty ONLY for a store that sent
	// no arm at all, which is itself a contract violation (see WriteBatch).
	Kind RefusalKind
	// Field is the offending field an invalid_request names, empty otherwise.
	Field string
}

// RefusalKind names a WriteBatchFailure's oneof arm.
type RefusalKind string

const (
	// RefusalStorageFailure: the transaction failed in the database. A retry may
	// succeed, so it suspends production and is recovered like any outage.
	RefusalStorageFailure RefusalKind = "storage_failure"
	// RefusalInvalidRequest: the batch violated validation. Nothing was
	// committed and a retry of the same bytes CANNOT help, so it is a producer
	// defect rather than an outage.
	RefusalInvalidRequest RefusalKind = "invalid_request"
	// RefusalKindUnset is a failure carrying neither arm — illegal on this
	// contract, and treated as a storage failure so the sidecar still recovers.
	RefusalKindUnset RefusalKind = ""
)

func (e *RefusalError) Error() string {
	if e.Kind == RefusalInvalidRequest && e.Field != "" {
		return fmt.Sprintf("storeclient: store refused %s as invalid_request(field=%s): %s", e.RPC, e.Field, e.Detail)
	}
	if e.Kind != RefusalKindUnset {
		return fmt.Sprintf("storeclient: store refused %s as %s: %s", e.RPC, e.Kind, e.Detail)
	}
	return fmt.Sprintf("storeclient: store refused %s: %s", e.RPC, e.Detail)
}

// IsRefusal reports whether err is a store refusal rather than a transport
// failure.
func IsRefusal(err error) bool {
	var target *RefusalError
	return errors.As(err, &target)
}

// InvalidRequest reports whether err is the refusal a RETRY CANNOT HELP WITH,
// and answers the field the store named. It is what separates the producer
// defect from the outage at every call site that has to choose.
func InvalidRequest(err error) (string, bool) {
	var target *RefusalError
	if !errors.As(err, &target) || target.Kind != RefusalInvalidRequest {
		return "", false
	}
	return target.Field, true
}

// Client is the sidecar's store surface. It is safe for concurrent use; the
// underlying http.Client pools connections to the socket.
type Client struct {
	socket string
	http   *http.Client
	rpc    storev1connect.ShimStoreClient
	log    *logging.Bound
}

// New builds a Client for the store listening on socket.
func New(socket string, log *logging.Bound) *Client {
	transport := &http.Transport{
		DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) {
			return (&net.Dialer{Timeout: dialTimeout}).DialContext(ctx, "unix", socket)
		},
	}
	httpClient := &http.Client{Transport: transport}
	log.With(logging.Context{Operation: "storeclient-new", StoreSocket: socket}).
		LogVerbose("constructing store connect client producer=%s", Producer)
	return &Client{
		socket: socket,
		http:   httpClient,
		rpc:    storev1connect.NewShimStoreClient(httpClient, baseURL),
		log:    log,
	}
}

// Cursors recovers the sidecar's persisted file cursors. An empty fileID asks
// for every cursor.
//
// AN EMPTY SUCCESS IS THE FRESH-STORE ANSWER and is returned as such: the
// caller starts every file from zero, honestly. A FAILURE is never softened
// into that, because taking a refusal for an empty cursor set is exactly the
// silent re-read of every watched file this client exists to prevent.
func (c *Client) Cursors(ctx context.Context, fileID string) ([]*storev1.CursorState, error) {
	bound := c.log.With(logging.Context{
		Operation: "storeclient-cursors", StoreSocket: c.socket, RPC: rpcGetSidecarCursors, FileID: fileID,
	})
	bound.LogVerbose("cursor recovery requested")
	request := &storev1.GetSidecarCursorsRequest{}
	if fileID != "" {
		request.FileId = &fileID
	}
	response, err := c.rpc.GetSidecarCursors(ctx, connect.NewRequest(request))
	if err != nil {
		// A RECOVERY THIS PROCESS WITHDREW IS NOT A TRANSPORT FAILURE — the same
		// rule WriteBatch below already follows, on the other verb. The one way
		// this call sees context.Canceled is the sidecar cancelling its own
		// cycle context on the way out, and the outcome is the ordinary one: no
		// cursor was recovered, so nothing is read and no tailer is built, and
		// the next boot asks again. Calling it a transport failure made every
		// shutdown that landed mid-recovery accuse a store that was fine — six
		// ERRORs in the 2026-09-13 15:28 gap scan alone, every one of them
		// inside a shutdown.
		//
		// A DEADLINE IS STILL A FAILURE and keeps the error: the store was asked
		// and did not answer in time, which is a fact about the store. The error
		// value is returned to the caller unchanged either way.
		//
		// THE NARRATION BELONGS TO THE CALLER, so this is the per-call DETAIL
		// and not a second normal-level record: the sidecar's `attempt` and
		// `cursorFor` are the layers that know a shutdown is in progress and
		// state the one INFO record for it.
		if errors.Is(err, context.Canceled) {
			bound.LogVerbose("cursor recovery abandoned: the caller cancelled the request, so no position was recovered and nothing was read")
			return nil, fmt.Errorf("storeclient: %s: %w", rpcGetSidecarCursors, err)
		}
		bound.With(logging.Context{Level: "error"}).Log("cursor recovery transport failure: %v", err)
		return nil, fmt.Errorf("storeclient: %s: %w", rpcGetSidecarCursors, err)
	}
	switch result := response.Msg.GetResult().(type) {
	case *storev1.GetSidecarCursorsResponse_Success:
		cursors := result.Success.GetCursors()
		bound.LogVerbose("cursor recovery succeeded cursors=%d", len(cursors))
		return cursors, nil
	case *storev1.GetSidecarCursorsResponse_Failure:
		refusal := &RefusalError{RPC: rpcGetSidecarCursors, Detail: result.Failure.GetDetail()}
		bound.With(logging.Context{Level: "error"}).Log("cursor recovery refused: %s", refusal.Detail)
		return nil, refusal
	default:
		// An unset oneof is illegal on this contract: it is neither an answer
		// nor a refusal, so it is raised rather than read as an empty success.
		bound.With(logging.Context{Level: "error"}).Log("cursor recovery answer carries neither success nor failure")
		return nil, fmt.Errorf("storeclient: %s response carries neither success nor failure", rpcGetSidecarCursors)
	}
}

// ShellRunClaims answers the shim's claims on record for the given spool task
// ids, each with the book holding its run's launching call when that is on
// record. An id with no claim is absent from the answer.
//
// A FAILURE IS NEVER SOFTENED INTO AN EMPTY ANSWER: "no claim yet" keeps a
// spool held and asked again, while a store that could not answer is an
// outage the caller must see.
func (c *Client) ShellRunClaims(ctx context.Context, vendorTaskIDs []string) ([]*storev1.ShellRunClaimed, error) {
	bound := c.log.With(logging.Context{
		Operation: "storeclient-shell-run-claims", StoreSocket: c.socket, RPC: rpcGetShellRunClaims,
	})
	bound.LogVerbose("shell run claims requested for %d spool(s)", len(vendorTaskIDs))
	response, err := c.rpc.GetShellRunClaims(ctx, connect.NewRequest(&storev1.GetShellRunClaimsRequest{VendorTaskIds: vendorTaskIDs}))
	if err != nil {
		if errors.Is(err, context.Canceled) {
			bound.LogVerbose("shell run claims abandoned: the caller cancelled the request, so no spool was claimed")
			return nil, fmt.Errorf("storeclient: %s: %w", rpcGetShellRunClaims, err)
		}
		bound.With(logging.Context{Level: "error"}).Log("shell run claims transport failure: %v", err)
		return nil, fmt.Errorf("storeclient: %s: %w", rpcGetShellRunClaims, err)
	}
	switch result := response.Msg.GetResult().(type) {
	case *storev1.GetShellRunClaimsResponse_Success:
		claims := result.Success.GetClaims()
		bound.LogVerbose("shell run claims answered claims=%d", len(claims))
		return claims, nil
	case *storev1.GetShellRunClaimsResponse_Failure:
		refusal := &RefusalError{RPC: rpcGetShellRunClaims, Detail: result.Failure.GetDetail()}
		switch kind := result.Failure.GetKind().(type) {
		case *storev1.GetShellRunClaimsFailure_InvalidRequest:
			refusal.Kind, refusal.Field = RefusalInvalidRequest, kind.InvalidRequest.GetField()
		case *storev1.GetShellRunClaimsFailure_StorageFailure:
			refusal.Kind = RefusalStorageFailure
		}
		bound.With(logging.Context{Level: "error"}).Log("shell run claims refused: %s", refusal.Detail)
		return nil, refusal
	default:
		bound.With(logging.Context{Level: "error"}).Log("shell run claims answer carries neither success nor failure")
		return nil, fmt.Errorf("storeclient: %s response carries neither success nor failure", rpcGetShellRunClaims)
	}
}

// WriteBatch writes one batch — the records plus the cursor advance that must
// become durable WITH them.
//
// SUCCESS MEANS DURABLE: records and cursor committed in one transaction, and a
// replayed batch fully absorbed by write_id is the SAME success arm. FAILURE
// means nothing was committed, so the caller must not advance; it holds no
// retry buffer and spills nothing, because its sources are durable files it
// re-reads from the last committed cursor.
//
// A DURABLE SUCCESS MAY STILL NAME SKIPS: an entry whose upsert_key already
// names a row under a different book is kept-and-skipped rather than refused, so
// re-ingesting already-stored content is idempotent. Those entries are returned
// so the caller can fold them into its own catch-up summary; they are not a
// failure and the cursor still advances.
func (c *Client) WriteBatch(ctx context.Context, batch *storev1.EntryBatch, shapes []*storev1.ShapeObservation) ([]SkippedEntry, error) {
	if batch == nil {
		return nil, errors.New("storeclient: WriteBatch requires a batch")
	}
	bound := c.log.With(logging.Context{
		Operation: "storeclient-write-batch", StoreSocket: c.socket, RPC: rpcWriteBatch, Producer: Producer,
	})
	if cursor := batch.GetCursorAdvance(); cursor != nil {
		bound = bound.With(logging.Context{
			FileID: cursor.GetFileId(), Path: cursor.GetPath(), Offset: logging.Off(cursor.GetOffset()),
		})
	}
	bound.LogVerbose("write requested entries=%d shapes=%d cursor_advance=%t", len(batch.GetEntries()), len(shapes), batch.GetCursorAdvance() != nil)
	response, err := c.rpc.WriteBatch(ctx, connect.NewRequest(&storev1.WriteBatchRequest{
		Producer: Producer,
		// EVERY SIDECAR WRITE IS BULK: it copies what the vendor already wrote
		// to disk, so the store takes any queued interactive write ahead of it
		// and splits it into bounded transactions. The store refuses a write
		// that states no class.
		WriteClass: bulkWriteClass,
		Batch:      batch,
		// THE SHAPE CATALOG RIDES THE SAME REQUEST as the records and the cursor
		// advance, so an observation taken from bytes this advance consumes
		// becomes durable with it or not at all.
		Shapes: shapes,
	}))
	if err != nil {
		// A WRITE THIS PROCESS WITHDREW IS NOT A TRANSPORT FAILURE. The one way
		// this call sees context.Canceled is the sidecar ending its own call on
		// the way out, and the outcome is the ordinary one the contract already
		// promises: the store holds the records and the cursor advance together
		// or holds neither, so the next boot resumes from whichever cursor it
		// holds and re-reads the durable bytes past it. WITHDRAWING IS NOT
		// RECALLING — a batch already on the wire may still commit — which is
		// why the caller waits out its settle before cancelling and why nothing
		// here claims the write was undone. Calling this an error made every
		// deploy restart that landed mid-batch write one — on the owner's
		// machine, 191 entries at 2026-09-13T01:21:08, seconds before the
		// process exited.
		//
		// A DEADLINE IS STILL A FAILURE and keeps the error: the store was asked
		// and did not answer in time, which is a fact about the store. Only a
		// cancellation this process issued is exempt, and the error itself is
		// returned to the caller unchanged either way.
		//
		// THE NARRATION BELONGS TO THE CALLER, so this is the per-call DETAIL
		// and not a second normal-level record: the sidecar's storeWrite is the
		// layer that knows a shutdown is in progress and states the one INFO
		// `shutdown` record for it. Two records for one fact is what the
		// exactly-once rule forbids.
		if errors.Is(err, context.Canceled) {
			bound.LogVerbose("write abandoned for %d entrie(s): the caller ended the request, so this producer never learned whether the store committed it and its own cursor did not advance", len(batch.GetEntries()))
			return nil, fmt.Errorf("storeclient: %s: %w", rpcWriteBatch, err)
		}
		bound.With(logging.Context{Level: "error"}).Log("write transport failure for %d entrie(s): %v", len(batch.GetEntries()), err)
		return nil, fmt.Errorf("storeclient: %s: %w", rpcWriteBatch, err)
	}
	switch result := response.Msg.GetResult().(type) {
	case *storev1.WriteBatchResponse_Success:
		skipped := skippedFrom(result.Success.GetSkipped())
		bound.LogVerbose("write durable entries=%d skipped=%d", len(batch.GetEntries()), len(skipped))
		return skipped, nil
	case *storev1.WriteBatchResponse_Failure:
		refusal := writeRefusal(result.Failure)
		if refusal.Kind == RefusalKindUnset {
			// AN UNSET ONEOF IS ILLEGAL on this contract. The kind is the arm
			// that says whether a retry can help, so a store that omits it has
			// told the producer nothing actionable; that is a CONTRACT
			// VIOLATION and is stated as one, then treated as the recoverable
			// kind so the sidecar still tries rather than parking a file on a
			// verdict the store never actually gave.
			bound.With(logging.Context{Level: "error"}).Log(
				"write refused with NO failure kind, which this contract forbids; treating it as %s so recovery still runs: %s",
				RefusalStorageFailure, refusal.Detail)
			refusal.Kind = RefusalStorageFailure
			return nil, refusal
		}
		// THE SITE RIDES WITH THE KIND. The kind says whether a retry can help;
		// the site says which call was refused, which is what a reader joins
		// against the store's own refusal record for this batch.
		bound.With(logging.Context{
			Level: "error", RefusalKind: string(refusal.Kind),
			RefusalSite: rpcWriteBatch, Field: refusal.Field,
		}).Log(
			"write refused as %s for %d entrie(s), nothing committed: %s", refusal.Kind, len(batch.GetEntries()), refusal.Detail)
		return nil, refusal
	default:
		bound.With(logging.Context{Level: "error"}).Log("write answer carries neither success nor failure; the batch's durability is unknown")
		return nil, fmt.Errorf("storeclient: %s response carries neither success nor failure", rpcWriteBatch)
	}
}

// skippedFrom maps the store's per-entry legacy book-conflict skips off the
// success arm into the caller's own type, so nothing downstream imports the
// store proto to read a skip.
func skippedFrom(skipped []*storev1.WriteBatchSkippedEntry) []SkippedEntry {
	if len(skipped) == 0 {
		return nil
	}
	out := make([]SkippedEntry, 0, len(skipped))
	for _, s := range skipped {
		out = append(out, SkippedEntry{
			UpsertKey: s.GetUpsertKey(),
			FromBook:  s.GetFromBook(),
			ToBook:    s.GetToBook(),
		})
	}
	return out
}

// writeRefusal reads a WriteBatchFailure's arm into the typed refusal.
func writeRefusal(failure *storev1.WriteBatchFailure) *RefusalError {
	out := &RefusalError{RPC: rpcWriteBatch, Detail: failure.GetDetail()}
	switch kind := failure.GetKind().(type) {
	case *storev1.WriteBatchFailure_InvalidRequest:
		out.Kind = RefusalInvalidRequest
		out.Field = kind.InvalidRequest.GetField()
	case *storev1.WriteBatchFailure_StorageFailure:
		out.Kind = RefusalStorageFailure
	}
	return out
}

// Close releases pooled connections to the store socket. There is no session to
// tear down: the client holds no store-side state.
func (c *Client) Close() {
	c.log.With(logging.Context{Operation: "storeclient-close", StoreSocket: c.socket}).LogVerbose("releasing pooled store connections")
	c.http.CloseIdleConnections()
}
