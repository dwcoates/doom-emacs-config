package shimclient

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"time"

	protocolv1 "agentrepl/proto/protocol/v1"
)

// THE BOUNDED, BACKWARD-ANCHORED READ, ASKED OF THE SHIM
// (protocol/v1/message-page.proto).
//
// # Why it goes through the shim at all
//
// storehistory.Reader can dial the store directly, and for an UNWIRED
// workspace that is the only route there is. For a workspace whose shim is UP,
// dialling the store would be serving history through a side door while the
// session's own transport is the thing that is supposed to answer — the
// fallback repull.go's header forbids, because it masks a shim outage instead
// of surfacing it. So the daemon asks the shim, the shim asks the store, and
// the page comes back re-stamped with this request's id.
//
// # A FAILURE ARRIVES AS A Nack, AND IT IS NEVER AN EMPTY PAGE
//
// The shim reports every page failure as a Nack bearing the page request's own
// id (uds-session.ts serveMessagePage / failMessagePage). That is the failure
// arm of this exchange and it is surfaced as an error, because an empty page is
// a claim about the CONVERSATION — "there is nothing here" — while a Nack is a
// claim about the READ. A caller that could not tell them apart would render a
// blank feed over a conversation the store still holds, which is the bug this
// whole path exists to close.
//
// # ONE correlation map, because there is ONE request id
//
// The success frame is a MessagePage and the failure frame is a Nack, and both
// carry this request's id. They are therefore delivered through the SAME
// pending-waiter map the control exchanges use, so the two arms cannot be
// registered inconsistently, and a connection teardown resolves a page waiter
// through failPending exactly as it resolves a control one.

// ErrMessagePageNotConnected reports a page asked for on a session with no live
// shim connection. There is deliberately no second route: see the header.
var ErrMessagePageNotConnected = errors.New("shimclient: no live shim connection to read a bounded message page from")

// ErrMessagePageRefused reports the shim's own Nack on a page request.
//
// It is a distinct sentinel from every other failure here because it is the
// shim's VERDICT rather than a broken link, and it is emphatically not an empty
// page.
var ErrMessagePageRefused = errors.New("shimclient: the shim refused the bounded message page")

// ErrMessagePageLinkLost reports that the shim connection went away UNDER an
// in-flight page request, so the question was never finished being asked.
var ErrMessagePageLinkLost = errors.New("shimclient: the shim connection was lost under an in-flight message page request")

// ErrMessagePageTimeout reports that neither a page nor a Nack arrived in time.
// A page is ONE bounded indexed query at the far end, not a stream, so silence
// is a fault rather than a slow drain.
var ErrMessagePageTimeout = errors.New("shimclient: no MessagePage or Nack arrived for the request")

// MessagePageAnchor names where a page read is anchored. NEITHER ARM IS A
// POSITION THE CALLER AUTHORED: Head names nothing at all, and BeforeSeq
// carries a value the store minted and the daemon merely kept.
type MessagePageAnchor struct {
	// Head anchors at the newest message held. The cold reader's verb, and the
	// one the old vocabulary could not express.
	Head bool
	// BeforeSeq is a prior page's last_page_seq, copied VERBATIM. Ignored when
	// Head is set.
	BeforeSeq uint64
}

// MessagePage asks the shim for ONE bounded page of messages, running BACKWARD
// from the anchor, and returns the store's page re-stamped with this request's
// id.
//
// EVERY failure is an error and none of them is an empty page.
func (c *Client) MessagePage(ctx context.Context, anchor MessagePageAnchor) (*protocolv1.MessagePage, error) {
	ac := c.currentConn()
	if ac == nil {
		return nil, fmt.Errorf("%w (session %s)", ErrMessagePageNotConnected, c.cfg.SessionID)
	}

	requestID := newMessagePageID()
	req := &protocolv1.MessagePageRequest{RequestId: requestID}
	if anchor.Head {
		req.Anchor = &protocolv1.MessagePageRequest_Head{Head: &protocolv1.MessagePageHead{}}
	} else {
		req.Anchor = &protocolv1.MessagePageRequest_BeforeSeq{BeforeSeq: anchor.BeforeSeq}
	}

	ch := make(chan ackResult, 1)
	ac.pendMu.Lock()
	ac.pending[requestID] = ch
	ac.pendMu.Unlock()
	defer func() {
		ac.pendMu.Lock()
		delete(ac.pending, requestID)
		ac.pendMu.Unlock()
	}()

	if err := ac.writeMsg(req); err != nil {
		return nil, fmt.Errorf("shimclient: sending MessagePageRequest (session %s request_id=%s): %w", c.cfg.SessionID, requestID, err)
	}
	c.logf("message page requested request_id=%s anchor=%s before_seq=%d", requestID, messagePageAnchorName(anchor), anchor.BeforeSeq)

	timer := time.NewTimer(c.cfg.AckTimeout)
	defer timer.Stop()
	select {
	case <-ctx.Done():
		return nil, fmt.Errorf("shimclient: message page request_id=%s (session %s) cancelled: %w", requestID, c.cfg.SessionID, ctx.Err())
	case <-timer.C:
		return nil, fmt.Errorf("%w: request_id=%s (session %s) after %s", ErrMessagePageTimeout, requestID, c.cfg.SessionID, c.cfg.AckTimeout)
	case res := <-ch:
		if res.err != nil {
			c.logf("message page request_id=%s lost its connection before an answer: %v", requestID, res.err)
			return nil, fmt.Errorf("%w: request_id=%s (session %s): %w", ErrMessagePageLinkLost, requestID, c.cfg.SessionID, res.err)
		}
		if res.nack != nil {
			// THE FAILURE ARM. Loud, and never mistaken for a page with no
			// messages in it.
			c.logf("message page REFUSED request_id=%s reason=%q", requestID, res.nack.GetReason())
			return nil, fmt.Errorf("%w: request_id=%s (session %s) reason=%q", ErrMessagePageRefused, requestID, c.cfg.SessionID, res.nack.GetReason())
		}
		if res.page == nil {
			return nil, fmt.Errorf("shimclient: the answer to message page request_id=%s (session %s) was neither a page nor a refusal", requestID, c.cfg.SessionID)
		}
		c.logf("message page SERVED request_id=%s last_page_seq=%d", requestID, res.page.GetLastPageSeq())
		return res.page, nil
	}
}

// resolveMessagePage delivers a page to the request its id names. A page with
// no waiter is a late answer to a request this daemon already gave up on: it is
// loud-logged and dropped, never applied.
func (c *Client) resolveMessagePage(ac *activeConn, page *protocolv1.MessagePage) {
	if !ac.deliver(page.GetRequestId(), ackResult{page: page}) {
		c.logf("received MessagePage for unknown request_id=%s (stray or late); dropped", page.GetRequestId())
	}
}

// messagePageAnchorName names the anchor arm for the log line.
func messagePageAnchorName(a MessagePageAnchor) string {
	if a.Head {
		return "head"
	}
	return "before_seq"
}

// newMessagePageID mints a process-unique page correlation id.
func newMessagePageID() string {
	var b [6]byte
	_, _ = rand.Read(b[:])
	return "page-" + hex.EncodeToString(b[:])
}
