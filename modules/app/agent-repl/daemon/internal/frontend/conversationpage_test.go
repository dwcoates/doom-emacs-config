package frontend

import (
	"context"
	"errors"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"
)

// THE ONE COMMAND THIS SERVER ACKS BEFORE IT RUNS.
//
// A page is a bounded read whose answer is a PUSH, so the ack has one job:
// stop the request looking unanswered while the read is in progress. That
// matters here specifically, because an unanswered history request is what
// produced the 5,069-deep command queue this daemon has already been repaired
// for once (lanes.go). These cases pin the shape that removes the trigger —
// acceptance first, page second, and a refusal only when the read actually
// failed.

// tailPageCmd builds one cold-open conversation page request.
func tailPageCmd(requestID, workspace string, limit uint32) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_ConversationPage{
			ConversationPage: &frontendv1.ConversationPageCmd{
				Anchor: &frontendv1.ConversationPageCmd_Tail{Tail: &frontendv1.ConversationPageTail{Limit: limit}},
				Fence:  "f1",
			},
		},
	}
}

// pageExchange runs one page command to completion against a recording
// connection and returns every frame the client actually received, in order.
func pageExchange(t *testing.T, h *mockHandler, cmd *frontendv1.FrontendCommand) []*frontendv1.FrontendFrame {
	t.Helper()
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: h,
		AckDeadline: time.Hour,
	})
	cl := newClient(defaultClientBuffer, nil, ClientKindGUIStream)
	c := &stormConn{cl: cl}
	s.processCommand(s.newCommandTicket(cl, cmd, time.Now(), s.inflight.Add(1)))
	if err := s.drainOutbox(c, cl); err != nil {
		t.Fatalf("drainOutbox: %v", err)
	}
	var frames []*frontendv1.FrontendFrame
	for _, data := range c.wrote {
		var f frontendv1.FrontendFrame
		if err := protojson.Unmarshal(data, &f); err != nil {
			t.Fatalf("unmarshal frame %q: %v", data, err)
		}
		frames = append(frames, &f)
	}
	return frames
}

func TestAPageCommandIsAckedBeforeThePageIsAssembled(t *testing.T) {
	// Arrange — a handler that serves an ordinary page.
	h := &mockHandler{page: &frontendv1.ConversationPage{Workspace: "/ws/a", Fence: "f1"}}

	// Act.
	frames := pageExchange(t, h, tailPageCmd("r-1", "/ws/a", 10))

	// Assert — the ACCEPTANCE reaches the client first, so there is no window
	// in which the client is waiting on an ack at all.
	if len(frames) != 2 {
		t.Fatalf("frames = %d, want the acceptance ack and the page", len(frames))
	}
	ack := frames[0].GetCommandAck()
	if ack == nil || !ack.GetOk() || ack.GetRequestId() != "r-1" {
		t.Fatalf("first frame = %v, want an ok acceptance ack for r-1", frames[0])
	}
	if frames[1].GetConversationPage() == nil {
		t.Fatalf("second frame = %v, want the conversation page", frames[1])
	}
}

func TestAServedPageEchoesTheRequestItAnswers(t *testing.T) {
	// Arrange — a client with a cold open and a load-more both in flight has
	// two pages coming, and only the echo distinguishes them.
	h := &mockHandler{page: &frontendv1.ConversationPage{Workspace: "/ws/a", Fence: "f1"}}

	// Act.
	frames := pageExchange(t, h, tailPageCmd("r-load-more", "/ws/a", 10))

	// Assert.
	page := frames[len(frames)-1].GetConversationPage()
	if page.GetRequestId() != "r-load-more" {
		t.Fatalf("page request_id = %q, want the requesting command's id", page.GetRequestId())
	}
}

func TestAPageIsSentToTheRequestingClientOnly(t *testing.T) {
	// Arrange — a second connection that asked for nothing. A page broadcast to
	// it would have it adopt history it never requested, under a request id it
	// has no record of.
	h := &mockHandler{page: &frontendv1.ConversationPage{Workspace: "/ws/a", Fence: "f1"}}
	s := New(Config{
		Logf: testLogf(t), LogVerbosef: testLogf(t),
		State: staticState{snap: sampleSnapshot()}, Handler: h,
		AckDeadline: time.Hour,
	})
	asker := newClient(defaultClientBuffer, nil, ClientKindGUIStream)
	bystander := newClient(defaultClientBuffer, nil, ClientKindGUIStream)

	// Act.
	s.processCommand(s.newCommandTicket(asker, tailPageCmd("r-1", "/ws/a", 10), time.Now(), s.inflight.Add(1)))

	// Assert.
	c := &stormConn{cl: bystander}
	if err := s.drainOutbox(c, bystander); err != nil {
		t.Fatalf("drainOutbox: %v", err)
	}
	if len(c.wrote) != 0 {
		t.Fatalf("a client that asked for nothing received %d frame(s), want none", len(c.wrote))
	}
}

func TestAFailedPageReadAnswersWithARefusalUnderTheSameRequestID(t *testing.T) {
	// Arrange — the read fails after the acceptance already went out. Saying
	// nothing here would leave the client's load-more spinning against a
	// request that will never answer.
	h := &mockHandler{err: errors.New("the store could not be read")}

	// Act.
	frames := pageExchange(t, h, tailPageCmd("r-1", "/ws/a", 10))

	// Assert — acceptance, then the refusal; no page.
	if len(frames) != 2 {
		t.Fatalf("frames = %d, want the acceptance and the refusal", len(frames))
	}
	if accepted := frames[0].GetCommandAck(); accepted == nil || !accepted.GetOk() {
		t.Fatalf("first frame = %v, want the ok acceptance ack", frames[0])
	}
	refusal := frames[1].GetCommandAck()
	if refusal == nil || refusal.GetOk() || refusal.GetRequestId() != "r-1" {
		t.Fatalf("second frame = %v, want a failing ack for r-1", frames[1])
	}
	if refusal.GetError() == "" {
		t.Fatalf("refusal carried no account of what failed")
	}
}

func TestASuccessfulPageSendsNoSecondAck(t *testing.T) {
	// Arrange — the acceptance already answered the command. A trailing ok ack
	// would be a second settle for one request, which is exactly what a client
	// correlating by request id must never see.
	h := &mockHandler{page: &frontendv1.ConversationPage{Workspace: "/ws/a", Fence: "f1"}}

	// Act.
	frames := pageExchange(t, h, tailPageCmd("r-1", "/ws/a", 10))

	// Assert.
	acks := 0
	for _, f := range frames {
		if f.GetCommandAck() != nil {
			acks++
		}
	}
	if acks != 1 {
		t.Fatalf("acks = %d, want exactly the acceptance", acks)
	}
}

func TestAPageHandlerThatProducesNeitherPageNorErrorIsRefused(t *testing.T) {
	// Arrange — a construction defect. A client cannot tell an absent page from
	// an empty conversation, and that ambiguity is the blank-feed bug this
	// protocol's whole history has been spent closing.
	h := &mockHandler{}

	// Act.
	ack := Dispatch(context.Background(), testLogf(t), h, nil, tailPageCmd("r-1", "/ws/a", 10))

	// Assert.
	if ack.GetOk() {
		t.Fatalf("ack = %v, want a refusal for a handler that produced no page and no error", ack)
	}
}

func TestAPageCommandRidesItsWorkspaceLane(t *testing.T) {
	// Arrange — a page is a per-workspace read, so it must serialize behind
	// that workspace's other commands and behind nothing else.
	cmd := tailPageCmd("r-1", "/ws/a", 10)

	// Act.
	got := laneKey(cmd)

	// Assert.
	if got != "/ws/a" {
		t.Fatalf("laneKey = %q, want the command's own workspace lane", got)
	}
}
