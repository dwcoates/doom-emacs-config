package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// stampedSubmit is a well-formed submission the editor stamped at AT.
func stampedSubmit(at int64) *agentreplv1.SubmitPromptRequest {
	req := submitRequest()
	req.SubmittedAtUnixMs = proto.Int64(at)
	return req
}

func TestPromptEchoOfHandsBackOnlyAStampedPromptDrawnAtSubmit(t *testing.T) {
	delivered := prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-7",
		Disposition: promptqueue.Disposition{Delivered: true},
	}
	held := delivered
	kind := wsm.HoldKind(1)
	held.Disposition = promptqueue.Disposition{Held: &kind}
	refused := delivered
	refused.Disposition = promptqueue.Disposition{RefusedArm: promptqueue.ArmMerging}
	panel := prompthandler.Outcome{Recognition: prompthandler.RecognizedPanel}
	turnlessAct := prompthandler.Outcome{Recognition: prompthandler.RecognizedAct}
	tests := []struct {
		name    string
		req     *agentreplv1.SubmitPromptRequest
		outcome prompthandler.Outcome
		want    bool
	}{
		{"a stamped delivered prompt", stampedSubmit(1_700_000_000_123), delivered, true},
		{"an unstamped delivered prompt", submitRequest(), delivered, false},
		{"a stamped held prompt", stampedSubmit(1_700_000_000_123), held, false},
		{"a stamped refused prompt", stampedSubmit(1_700_000_000_123), refused, false},
		{"a stamped command panel", stampedSubmit(1_700_000_000_123), panel, false},
		{"a stamped turnless act", stampedSubmit(1_700_000_000_123), turnlessAct, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			echo, ok := promptEchoOf(tt.req, tt.outcome)

			// Assert.
			if ok != tt.want {
				t.Fatalf("handed back = %v, want %v", ok, tt.want)
			}
			if ok && (echo.GetTurn().GetValue() != "turn-7" || echo.GetSubmittedAtUnixMs() != 1_700_000_000_123) {
				t.Fatalf("echo = %v, want turn-7 submitted at the stamp", echo)
			}
		})
	}
}

// openWebAttached opens the test workspace's web stream and returns once its
// serving loop runs, so every event emitted afterwards reaches it: the
// transfer notice is replayed to a late subscriber, and the loop reads it only
// after every topic of the stream is subscribed.
func openWebAttached(t *testing.T, h *harness) *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse] {
	t.Helper()
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	stream := openWeb(t, h)
	if receiveWebEvent(t, stream).GetTransferred() == nil {
		t.Fatal("the web stream's first event was not the transfer notice")
	}
	return stream
}

func TestASubmittedPromptsEchoIsHandedToItsPage(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Prompts.outcome = prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-7",
		Disposition: promptqueue.Disposition{Delivered: true},
	}
	stream := openWebAttached(t, h)

	// Act.
	if _, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(stampedSubmit(1_700_000_000_123))); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	echo := receiveWebEvent(t, stream).GetPromptEcho()
	if echo.GetTurn().GetValue() != "turn-7" || echo.GetSubmittedAtUnixMs() != 1_700_000_000_123 {
		t.Fatalf("echo = %v, want turn-7 submitted at the stamp", echo)
	}
}

func TestAPromptEchoIsRecorded(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	log := &recordingLogger{}
	echo, _ := promptEchoOf(stampedSubmit(1_700_000_000_123), prompthandler.Outcome{
		Recognition: prompthandler.RecognizedNone,
		Turn:        "turn-7",
		Disposition: promptqueue.Disposition{Delivered: true},
	})

	// Act.
	h.Server.(*server).pushPromptEcho(log, testWorkspaceID, echo)

	// Assert.
	records := log.at("DEBUG")
	if len(records) != 1 || records[0].Operation != "daemon.server.prompt_echo" ||
		records[0].Context["turn"] != "turn-7" || records[0].Context["submitted_at_unix_ms"] != int64(1_700_000_000_123) ||
		records[0].Context["pages"] != 0 {
		t.Fatalf("records = %+v, want one prompt_echo record naming the turn, the instant and no listening page", records)
	}
}

func TestSubmitPromptRefusesANonPositiveSubmitInstant(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.SubmitPrompt(context.Background(), connect.NewRequest(stampedSubmit(0)))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
}
