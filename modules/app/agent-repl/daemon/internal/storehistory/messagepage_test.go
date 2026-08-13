package storehistory

import (
	"context"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	protocolv1 "agentrepl/proto/protocol/v1"
	"agentrepl/wire"
)

// THE PAGE READ IS ONE REQUEST AND ONE ANSWER.
//
// Each case here is one property of that round trip: the request names the
// VENDOR session the store keys its seq space on, the anchor arms travel
// distinctly, and a store that cannot answer produces an ERROR rather than an
// empty page.

// fakePageStore is a store-shaped server that answers exactly one
// MessagePageRequest.
type fakePageStore struct {
	ln net.Listener

	mu sync.Mutex
	// requested is the request frame the reader sent.
	requested *protocolv1.MessagePageRequest
	// page is the answer, or nil to close without answering.
	page *protocolv1.MessagePage
	// decoy is written BEFORE the answer, carrying a request id this reader is
	// not awaiting.
	decoy *protocolv1.MessagePage
	// done releases the server goroutine at the end of the test, so the
	// connection outlives the read rather than being closed underneath it.
	done chan struct{}
}

func newFakePageStore(t *testing.T, page *protocolv1.MessagePage) *fakePageStore {
	t.Helper()
	dir, err := os.MkdirTemp("/tmp", "storepage-")
	if err != nil {
		t.Fatalf("temp socket dir: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	ln, err := net.Listen("unix", filepath.Join(dir, "store.sock"))
	if err != nil {
		t.Fatalf("listen: %v", err)
	}
	t.Cleanup(func() { _ = ln.Close() })
	s := &fakePageStore{ln: ln, page: page, done: make(chan struct{})}
	t.Cleanup(func() { close(s.done) })
	go s.serve()
	return s
}

func (s *fakePageStore) path() string { return s.ln.Addr().String() }

func (s *fakePageStore) serve() {
	conn, err := s.ln.Accept()
	if err != nil {
		return
	}
	defer conn.Close()
	msg, err := wire.ReadAny(conn)
	if err != nil {
		return
	}
	req, ok := msg.(*protocolv1.MessagePageRequest)
	if !ok {
		return
	}
	s.mu.Lock()
	s.requested = req
	page, decoy, done := s.page, s.decoy, s.done
	s.mu.Unlock()
	if decoy != nil {
		_ = wire.WriteAny(conn, decoy)
	}
	if page == nil {
		// A store that closes without answering: the page will never arrive.
		return
	}
	page.RequestId = req.GetRequestId()
	_ = wire.WriteAny(conn, page)
	// The reader returns on the frame it awaits, so the close that follows is
	// never what ends the read.
	<-done
}

func (s *fakePageStore) request() *protocolv1.MessagePageRequest {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.requested
}

func TestAMessagePageRequestNamesTheVendorSession(t *testing.T) {
	// Arrange — the store keys its seq space on the VENDOR uuid, so a request
	// under any other id asks about a conversation the store does not hold.
	var logged []string
	store := newFakePageStore(t, &protocolv1.MessagePage{LastPageSeq: 7})
	r := newReader(t, store.path(), &logged)

	// Act.
	if _, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true}); err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert.
	if got := store.request().GetSessionId(); got != "vendor-uuid" {
		t.Fatalf("request session_id = %q, want the vendor uuid", got)
	}
}

func TestAHeadAnchorTravelsAsTheHeadArm(t *testing.T) {
	// Arrange — the head is a fact the STORE resolves; a caller that named a
	// seq for it would be authoring the position this contract removes.
	var logged []string
	store := newFakePageStore(t, &protocolv1.MessagePage{})
	r := newReader(t, store.path(), &logged)

	// Act.
	if _, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true}); err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert.
	if _, ok := store.request().GetAnchor().(*protocolv1.MessagePageRequest_Head); !ok {
		t.Fatalf("anchor = %T, want the head arm", store.request().GetAnchor())
	}
}

func TestABeforeSeqAnchorTravelsVerbatim(t *testing.T) {
	// Arrange — a continuation copies a prior page's last_page_seq exactly. Any
	// arithmetic on it here would be a position of the caller's own.
	var logged []string
	store := newFakePageStore(t, &protocolv1.MessagePage{})
	r := newReader(t, store.path(), &logged)

	// Act.
	if _, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{BeforeSeq: 4242}); err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert.
	before, ok := store.request().GetAnchor().(*protocolv1.MessagePageRequest_BeforeSeq)
	if !ok {
		t.Fatalf("anchor = %T, want the before_seq arm", store.request().GetAnchor())
	}
	if before.BeforeSeq != 4242 {
		t.Fatalf("before_seq = %d, want 4242 carried verbatim", before.BeforeSeq)
	}
}

func TestAStoreThatNeverAnswersIsAnErrorRatherThanAnEmptyPage(t *testing.T) {
	// Arrange — the store closes without a page. An empty page is a claim about
	// the CONVERSATION; this is a claim about the LINK, and collapsing the two
	// is the blank-feed bug.
	var logged []string
	store := newFakePageStore(t, nil)
	r := newReader(t, store.path(), &logged)

	// Act.
	page, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true})

	// Assert.
	if err == nil {
		t.Fatalf("MessagePage returned a page (%v) for a store that never answered", page)
	}
	if page != nil {
		t.Fatalf("a failed page read still produced a page: %v", page)
	}
}

func TestAMessagePageForASessionWithNoVendorUUIDIsRefused(t *testing.T) {
	// Arrange — with no vendor uuid there is no key the store's seq space is
	// under, so the history cannot be located at all.
	var logged []string
	store := newFakePageStore(t, &protocolv1.MessagePage{})
	r := newReader(t, store.path(), &logged)
	r.Vendor = func(string) (string, bool) { return "", false }

	// Act.
	_, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "vendor session uuid") {
		t.Fatalf("err = %v, want a refusal naming the missing vendor session uuid", err)
	}
}

func TestAMessagePageWithNoStoreSocketIsRefused(t *testing.T) {
	// Arrange — an unconfigured socket cannot be dialled, and a page read that
	// quietly returned nothing would read as an empty conversation.
	var logged []string
	r := newReader(t, "", &logged)

	// Act.
	_, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "store socket") {
		t.Fatalf("err = %v, want a refusal naming the missing store socket", err)
	}
}

func TestAMessagePageForAnotherRequestIdIsDiscarded(t *testing.T) {
	// Arrange — request_id is what correlates a page with the request that
	// asked for it, so a page for another request is never applied.
	var logged []string
	store := newFakePageStore(t, &protocolv1.MessagePage{LastPageSeq: 9})
	store.decoy = &protocolv1.MessagePage{RequestId: "somebody-else", LastPageSeq: 111}

	r := newReader(t, store.path(), &logged)

	// Act.
	page, err := r.MessagePage(context.Background(), "/ws", "s1", PageAnchor{Head: true})
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}

	// Assert — the foreign page was skipped, not applied.
	if page.GetLastPageSeq() != 9 {
		t.Fatalf("last_page_seq = %d, want the awaited page's 9 rather than the decoy's 111", page.GetLastPageSeq())
	}
}
