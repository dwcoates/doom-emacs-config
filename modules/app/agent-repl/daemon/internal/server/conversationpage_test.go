package server

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
)

// THE COMMAND HANDLER'S HALF OF A CONVERSATION PAGE.
//
// It owns three decisions and nothing else: which conversation the echoed
// fence names, which anchor the client asked for, and what a refusal is
// classified as. The reader itself is proved in sessioncontroller; what these
// cases pin is that a malformed or stale REQUEST never reaches it.

// recordingPager captures what the handler asked the reader for.
type recordingPager struct {
	gotWorkspace string
	// gotFence is the token as the handler passed it on. The whole point of
	// the port's shape is that it arrives UNSPLIT, so this records the string
	// rather than a pair reconstructed from it.
	gotFence  string
	gotAnchor sessioncontroller.PageAnchor
	page      *frontendv1.ConversationPage
	err       error
}

func (recordingPager) ResyncForFence(string, string, uint64) error { return nil }

func (p *recordingPager) ConversationPage(_ context.Context, workspace, echoedFence string, anchor sessioncontroller.PageAnchor) (*frontendv1.ConversationPage, error) {
	p.gotWorkspace, p.gotFence, p.gotAnchor = workspace, echoedFence, anchor
	if p.err != nil {
		return nil, p.err
	}
	return p.page, nil
}

func tailPageRequest(limit uint32, fence string) *frontendv1.ConversationPageCmd {
	return &frontendv1.ConversationPageCmd{
		Anchor: &frontendv1.ConversationPageCmd_Tail{Tail: &frontendv1.ConversationPageTail{Limit: limit}},
		Fence:  fence,
	}
}

func beforePageRequest(cursor string, limit uint32, fence string) *frontendv1.ConversationPageCmd {
	return &frontendv1.ConversationPageCmd{
		Anchor: &frontendv1.ConversationPageCmd_Before{Before: &frontendv1.ConversationPageBefore{Cursor: cursor, Limit: limit}},
		Fence:  fence,
	}
}

func TestAPageRequestCarriesItsFenceWhole(t *testing.T) {
	// Arrange — the token is opaque and byte-compared by its receiver, and the
	// daemon reads inside it in exactly ONE place. A handler that split it here
	// would be a second reader with its own semantics, which is the divergence
	// that left an unwired workspace's history unreachable once already.
	pager := &recordingPager{page: &frontendv1.ConversationPage{}}
	var lines []string
	h := newResyncHandler(t, pager, &lines)
	fence := ssm.Fence("s1", "g1")

	// Act.
	if _, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", tailPageRequest(10, fence)); err != nil {
		t.Fatalf("ConversationPage: %v", err)
	}

	// Assert.
	if pager.gotFence != fence {
		t.Fatalf("reader was asked with fence %q, want the client's echo %q passed through unsplit", pager.gotFence, fence)
	}
	if pager.gotWorkspace != "/ws/a" {
		t.Fatalf("reader was asked for ws=%q, want /ws/a", pager.gotWorkspace)
	}
}

func TestAnEmptyFenceReachesTheReaderAsAnEmptyFence(t *testing.T) {
	// Arrange — an ungenerated workspace publishes an ABSENT fence, and a
	// client echoing it is doing exactly the right thing. A handler that split
	// and recomposed would turn "" into "|", a token nobody was ever shown.
	pager := &recordingPager{page: &frontendv1.ConversationPage{}}
	var lines []string
	h := newResyncHandler(t, pager, &lines)

	// Act.
	if _, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", tailPageRequest(10, "")); err != nil {
		t.Fatalf("ConversationPage: %v", err)
	}

	// Assert.
	if pager.gotFence != "" {
		t.Fatalf("reader was asked with fence %q, want the empty echo preserved", pager.gotFence)
	}
}

func TestEachAnchorReachesTheReaderAsTheRequestMadeIt(t *testing.T) {
	// Arrange — the two questions a paging feed asks.
	tests := []struct {
		name string
		cmd  *frontendv1.ConversationPageCmd
		want sessioncontroller.PageAnchor
	}{
		{
			name: "the cold open asks for the tail",
			cmd:  tailPageRequest(7, ssm.Fence("s1", "g1")),
			want: sessioncontroller.PageAnchor{Tail: true, Limit: 7},
		},
		{
			name: "load-more asks for the page before a cursor",
			cmd:  beforePageRequest("opaque-token", 7, ssm.Fence("s1", "g1")),
			want: sessioncontroller.PageAnchor{Cursor: "opaque-token", Limit: 7},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			pager := &recordingPager{page: &frontendv1.ConversationPage{}}
			var lines []string
			h := newResyncHandler(t, pager, &lines)

			// Act.
			if _, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", tt.cmd); err != nil {
				t.Fatalf("ConversationPage: %v", err)
			}

			// Assert.
			if pager.gotAnchor != tt.want {
				t.Fatalf("reader anchor = %+v, want %+v", pager.gotAnchor, tt.want)
			}
		})
	}
}

func TestAPageRequestWithNoAnchorIsRefusedRatherThanReadAsATail(t *testing.T) {
	// Arrange — defaulting would turn a client bug into a silent full-tail read
	// on every workspace it reached, with no record that anything was wrong.
	pager := &recordingPager{page: &frontendv1.ConversationPage{}}
	var lines []string
	h := newResyncHandler(t, pager, &lines)

	// Act.
	_, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", &frontendv1.ConversationPageCmd{Fence: ssm.Fence("s1", "g1")})

	// Assert — refused, and the reader was never asked.
	if err == nil {
		t.Fatalf("ConversationPage error = nil, want a refusal for a command carrying neither anchor")
	}
	if pager.gotWorkspace != "" {
		t.Fatalf("the reader was asked for ws=%q despite an anchorless command", pager.gotWorkspace)
	}
}

func TestASupersededPageRefusalCarriesTheReloadRemedy(t *testing.T) {
	// Arrange — a page is a VIEW's history, exactly as a resync is, so a
	// refused one leaves the client permanently behind unless it is told to
	// remount. It takes the resync's remedy for that reason.
	pager := &recordingPager{err: errclass.ErrSessionSuperseded}
	var lines []string
	h := newResyncHandler(t, pager, &lines)

	// Act.
	_, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", tailPageRequest(10, ssm.Fence("s1", "g-old")))

	// Assert.
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("ConversationPage error = %v, want ErrSessionSuperseded", err)
	}
	var remedied interface{ FailureRemedy() string }
	if !errors.As(err, &remedied) || !strings.Contains(remedied.FailureRemedy(), "reload") {
		t.Fatalf("refusal offered no reload remedy, so a refused view has nothing to act on: %v", err)
	}
}

func TestAPageIsRefusedWhenNoReaderIsWired(t *testing.T) {
	// Arrange — the command exists, so something must answer it. A nil reader
	// is a construction error rather than a degraded mode.
	var lines []string
	h := newResyncHandler(t, nil, &lines)

	// Act.
	_, err := h.ConversationPage(context.Background(), "/ws/a", "r-1", tailPageRequest(10, ssm.Fence("s1", "g1")))

	// Assert.
	if err == nil {
		t.Fatalf("ConversationPage error = nil, want a refusal when no reader is wired")
	}
}
