package storehistory

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"time"

	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/wire"

	"github.com/google/uuid"
)

// THE BOUNDED, BACKWARD-ANCHORED READ (protocol/v1/message-page.proto),
// performed against the same store socket ReplayHistory subscribes to.
//
// WHAT IT SAYS THAT Subscribe COULD NOT. Subscribe and ReplayRequest both read
// FORWARD FROM A LOWER BOUND, so neither can express "the newest ten
// messages": a reader wanting recent history has to GUESS a from_seq low
// enough to cover it, and since one message can own hundreds of records that
// guess cannot be computed. The guess IS the unbounded scan. MessagePageHead
// asks for the newest page without naming a seq at all, and before_seq
// continues below a page already received by copying THAT page's last_page_seq
// verbatim — a place the caller has demonstrably been.
//
// THE CALLER NEVER COMPUTES A POSITION. Nothing here derives a seq from the
// records on a page, and nothing subtracts one from anything: the only
// position that travels is one the store minted.
//
// EVERY FAILURE IS LOUD. A dial failure, a closed connection, a framing error
// and a quiet store are errors, never an empty page — an empty page is a claim
// about the CONVERSATION and a failure is a claim about the LINK, and a
// frontend that cannot tell them apart is the blank-feed bug this whole path
// exists to close.

// DefaultPageTimeout is how long one page read waits for its answer.
//
// A page is ONE bounded indexed query, not a stream, so the store either
// answers it promptly or something is wrong. It is shorter than DefaultIdle
// for exactly that reason: there is no drain to wait out here.
const DefaultPageTimeout = 10 * time.Second

// PageAnchor names where a page read is anchored, and neither arm is a
// position the caller authored: Head names nothing at all, and BeforeSeq
// carries a value the store minted.
type PageAnchor struct {
	// Head anchors at the newest message held. The cold reader's verb.
	Head bool
	// BeforeSeq is a prior page's last_page_seq, copied VERBATIM. Ignored when
	// Head is set.
	BeforeSeq uint64
}

// MessagePage fetches ONE page of messages for the session, running BACKWARD
// from the anchor.
//
// The connection is a throwaway, exactly as ReplayHistory's is: a page is a
// one-shot request/response and has no business sharing a subscription's
// lifetime.
func (r *Reader) MessagePage(ctx context.Context, workspace, sessionID string, anchor PageAnchor) (*protocolv1.MessagePage, error) {
	if r.Logf == nil {
		return nil, fmt.Errorf("storehistory: message page for ws %q needs a logger", workspace)
	}
	if r.Socket == "" {
		return nil, fmt.Errorf("storehistory: message page for ws %q has no store socket configured", workspace)
	}
	if r.Vendor == nil {
		return nil, fmt.Errorf("storehistory: message page for ws %q has no vendor session resolver configured", workspace)
	}
	vendor, ok := r.Vendor(sessionID)
	if !ok || vendor == "" {
		return nil, fmt.Errorf("storehistory: message page for ws %q session %s has no vendor session uuid recorded, which is the key the store's seq space is under — its history cannot be located", workspace, sessionID)
	}
	timeout := r.Idle
	if timeout <= 0 {
		timeout = DefaultPageTimeout
	}
	requestID := uuid.NewString()
	req := &protocolv1.MessagePageRequest{RequestId: requestID, SessionId: vendor}
	if anchor.Head {
		req.Anchor = &protocolv1.MessagePageRequest_Head{Head: &protocolv1.MessagePageHead{}}
	} else {
		req.Anchor = &protocolv1.MessagePageRequest_BeforeSeq{BeforeSeq: anchor.BeforeSeq}
	}
	started := time.Now()
	r.Logf("storehistory: requesting ONE bounded message page ws=%q session=%s vendor_session=%s socket=%q request_id=%s anchor=%s before_seq=%d",
		workspace, sessionID, vendor, r.Socket, requestID, anchorName(anchor), anchor.BeforeSeq)

	conn, err := net.Dial("unix", r.Socket)
	if err != nil {
		r.Logf("storehistory: message page UNREADABLE ws=%q session=%s vendor_session=%s socket=%q request_id=%s: dial failed: %v",
			workspace, sessionID, vendor, r.Socket, requestID, err)
		return nil, fmt.Errorf("storehistory: dialling the store at %q for a message page for ws %q: %w", r.Socket, workspace, err)
	}
	defer conn.Close()

	// The context owns the connection's lifetime, so a cancelled page read
	// unblocks a read parked on a store that stopped answering.
	readDone := make(chan struct{})
	defer close(readDone)
	go func() {
		select {
		case <-ctx.Done():
			_ = conn.Close()
		case <-readDone:
		}
	}()

	if err := wire.WriteAny(conn, req); err != nil {
		r.Logf("storehistory: message page UNREADABLE ws=%q session=%s vendor_session=%s request_id=%s: request write failed: %v",
			workspace, sessionID, vendor, requestID, err)
		return nil, fmt.Errorf("storehistory: writing a message page request for ws %q (vendor session %s): %w", workspace, vendor, err)
	}

	deadline := time.Now().Add(timeout)
	for {
		if err := conn.SetReadDeadline(deadline); err != nil {
			return nil, fmt.Errorf("storehistory: arming the store read deadline for a message page for ws %q: %w", workspace, err)
		}
		msg, err := wire.ReadAny(conn)
		if err != nil {
			if ctxErr := ctx.Err(); ctxErr != nil {
				return nil, fmt.Errorf("storehistory: message page for ws %q cancelled: %w", workspace, ctxErr)
			}
			var netErr net.Error
			if errors.As(err, &netErr) && netErr.Timeout() {
				r.Logf("storehistory: message page TIMED OUT ws=%q session=%s vendor_session=%s request_id=%s timeout_ms=%d",
					workspace, sessionID, vendor, requestID, timeout.Milliseconds())
				return nil, fmt.Errorf("storehistory: no MessagePage for ws %q (vendor session %s) within %s", workspace, vendor, timeout)
			}
			if errors.Is(err, io.EOF) {
				r.Logf("storehistory: message page UNANSWERED ws=%q session=%s vendor_session=%s request_id=%s: the store closed the connection",
					workspace, sessionID, vendor, requestID)
				return nil, fmt.Errorf("storehistory: the store closed the connection for ws %q before answering the message page", workspace)
			}
			return nil, fmt.Errorf("storehistory: reading the store's message page for ws %q: %w", workspace, err)
		}
		page, isPage := msg.(*protocolv1.MessagePage)
		if !isPage {
			// Routing is by frame type: anything else sharing this connection
			// (a heartbeat) is simply not this page.
			continue
		}
		if page.GetRequestId() != requestID {
			// A page for a request this call is not awaiting is DISCARDED,
			// never applied: request_id is what correlates a page with the
			// request that asked for it.
			r.Logf("storehistory: DISCARDED a MessagePage for another request ws=%q session=%s request_id=%s page_request_id=%s",
				workspace, sessionID, requestID, page.GetRequestId())
			continue
		}
		r.Logf("storehistory: message page SERVED ws=%q session=%s vendor_session=%s request_id=%s last_page_seq=%d boundary=%s elapsed_ms=%d",
			workspace, sessionID, vendor, requestID, page.GetLastPageSeq(), boundaryName(page), time.Since(started).Milliseconds())
		return page, nil
	}
}

// anchorName names the anchor arm for the log line.
func anchorName(a PageAnchor) string {
	if a.Head {
		return "head"
	}
	return "before_seq"
}

// boundaryName names the boundary arm for the log line, keeping the UNSET case
// visible rather than folding it into either answer.
func boundaryName(page *protocolv1.MessagePage) string {
	switch page.GetBoundary().(type) {
	case *protocolv1.MessagePage_More:
		return "more"
	case *protocolv1.MessagePage_Floor:
		return "retained_floor"
	default:
		return "unset"
	}
}
