// Package storereader is the daemon's ONE way into the store: the store's
// read-only face, storeread.v1.StoreRead, dialled on its own socket.
//
// THE DAEMON'S RELATIONSHIP WITH THE STORE IS READ-ONLY (owner ruling,
// 2026-10-27), and it is so by construction rather than by care:
//
//   - storeread.v1 holds no write verb and imports only the conversation
//     vocabulary, which the proto build's isolation gate enforces (invariant
//     I8), and the same gate refuses any daemon import of store.v1 (I7);
//   - this package dials the read-only socket (storesocket.ReadPath), where
//     the store mounts StoreRead alone, so a request sent there reaches no
//     producer verb whatever it is;
//   - the store answers it from a handler holding only its read-only pool,
//     which SQLite itself refuses a write on.
//
// A fact the daemon wants to attach to a conversation is the daemon's own
// state (wsm), never a write to the store.
package storereader

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"

	"agentrepl/logging/storesocket"

	"agent-repld/internal/h2cclient"
)

// The arms a refusal is named by, as the store's failure arm names them.
const (
	ArmInvalidRequest = "invalid_request"
	ArmUnknownAgent   = "unknown_agent"
	ArmStalePointer   = "stale_pointer"
	ArmStorageFailure = "storage_failure"
	// ArmUnnamed is a failure arm this daemon's schema does not name.
	ArmUnnamed = "unnamed"
)

// Refusal is the store declining a read, named by its failure arm.
type Refusal struct {
	Arm    string
	Detail string
}

func (r *Refusal) Error() string {
	return fmt.Sprintf("storereader: the store refused the history read (%s): %s", r.Arm, r.Detail)
}

// IsUnknownAgent reports a refusal naming a book the store holds nothing for.
func IsUnknownAgent(err error) bool {
	var refusal *Refusal
	return errors.As(err, &refusal) && refusal.Arm == ArmUnknownAgent
}

// ErrUnanswered is a read the store never answered: its read-only socket could
// not be reached, or it answered neither a page nor a refusal.
var ErrUnanswered = errors.New("storereader: the store did not answer the history read")

// Reader reads one store's history through its read-only face.
type Reader struct {
	socket string
	client storereadv1connect.StoreReadClient
}

// New is the reader of the store serving its producers on STORESOCKET: it
// dials that store's read-only socket beside it, exactly where the store binds
// it.
func New(storeSocket string) *Reader {
	socket := storesocket.ReadPath(storeSocket)
	return &Reader{socket: socket, client: storereadv1connect.NewStoreReadClient(h2cclient.UnixClient(socket), "http://store")}
}

// Socket is the read-only socket this reader dials, for records.
func (r *Reader) Socket() string { return r.socket }

// Newest reads BOOK's newest page. STATED is the daemon holding a shim's word
// for the book (the workspace's recorded book), so an absent book is the empty
// page it is; any other book the store holds nothing for is refused
// ArmUnknownAgent.
func (r *Reader) Newest(ctx context.Context, book string, stated bool) (*conversationv1.HistoryPage, error) {
	newest := &storereadv1.ReadHistoryPageNewest{KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}}}
	if stated {
		newest.KnownAs = &storereadv1.ReadHistoryPageNewest_Stated{Stated: &storereadv1.ReadHistoryPageNewestStated{}}
	}
	return r.read(ctx, &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: book}, Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: newest}})
}

// Before reads the page of BOOK placed strictly before the line AFTER names.
func (r *Reader) Before(ctx context.Context, book string, after *conversationv1.HistoryPointer) (*conversationv1.HistoryPage, error) {
	return r.read(ctx, &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: book}, Position: &storereadv1.ReadHistoryPageRequest_After{After: after}})
}

// Through reads BOOK's newest lines placed at or before THROUGHMS: the book as
// it stood then.
func (r *Reader) Through(ctx context.Context, book string, throughMs int64) (*conversationv1.HistoryPage, error) {
	return r.read(ctx, &storereadv1.ReadHistoryPageRequest{Book: &conversationv1.AgentId{Value: book}, Position: &storereadv1.ReadHistoryPageRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: throughMs}}})
}

func (r *Reader) read(ctx context.Context, req *storereadv1.ReadHistoryPageRequest) (*conversationv1.HistoryPage, error) {
	resp, err := r.client.ReadHistoryPage(ctx, connect.NewRequest(req))
	if err != nil {
		return nil, fmt.Errorf("%w on %s: %w", ErrUnanswered, r.socket, err)
	}
	if failure := resp.Msg.GetFailure(); failure != nil {
		return nil, &Refusal{Arm: failureArm(failure), Detail: failure.GetDetail()}
	}
	page := resp.Msg.GetSuccess().GetPage()
	if page == nil {
		return nil, fmt.Errorf("%w: it answered neither a page nor a refusal for book %q", ErrUnanswered, req.GetBook().GetValue())
	}
	return page, nil
}

// failureArm names a failure by its arm.
func failureArm(failure *storereadv1.ReadHistoryPageFailure) string {
	switch {
	case failure.GetInvalidRequest() != nil:
		return ArmInvalidRequest
	case failure.GetUnknownAgent() != nil:
		return ArmUnknownAgent
	case failure.GetStalePointer() != nil:
		return ArmStalePointer
	case failure.GetStorageFailure() != nil:
		return ArmStorageFailure
	default:
		return ArmUnnamed
	}
}

