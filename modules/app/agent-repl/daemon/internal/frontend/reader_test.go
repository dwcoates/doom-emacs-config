package frontend

import (
	"context"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// THE READER IS MINTED BY THE TRANSPORT, never read off the wire. These cases
// pin what the dispatch arms do with it: they pass it through, and they refuse
// loudly when there is none rather than filing a position under the empty key.

// historyCommand wraps one positionless verb into a FrontendCommand.
func firstPageCommand(requestID, workspace string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_FirstPage{FirstPage: &frontendv1.FirstPageCmd{Workspace: workspace}},
	}
}

func nextPageCommand(requestID, workspace string) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID, Workspace: workspace,
		Command: &frontendv1.FrontendCommand_NextPage{NextPage: &frontendv1.NextPageCmd{Workspace: workspace}},
	}
}

func TestAFirstPageCarriesTheTransportsReaderToTheHandler(t *testing.T) {
	// Arrange — the identity that selects the position is the connection's, so
	// it has to arrive at the handler as the connection minted it.
	h := &mockHandler{historyPage: &frontendv1.ConversationHistoryPage{}}
	ctx := ContextWithReader(context.Background(), connectionReader(7))

	// Act.
	ack, _ := DispatchWithResponse(ctx, nil, h, nil, firstPageCommand("r1", "/ws"))

	// Assert.
	if !ack.GetOk() {
		t.Fatalf("first_page ack = %q", ack.GetError())
	}
	if h.lastReader != "conn-7" {
		t.Fatalf("handler saw reader %q, want conn-7", h.lastReader)
	}
}

func TestAFirstPageWithNoReaderIdentityIsRefused(t *testing.T) {
	// Arrange — a context with no reader. Serving would file this reader's
	// place under the empty key, where the next unidentified reader inherits
	// it.
	h := &mockHandler{historyPage: &frontendv1.ConversationHistoryPage{}}

	// Act.
	ack, response := DispatchWithResponse(context.Background(), nil, h, nil, firstPageCommand("r1", "/ws"))

	// Assert.
	if ack.GetOk() {
		t.Fatal("first_page with no reader identity was served")
	}
	if response != nil {
		t.Fatal("a refused first_page still produced a page frame")
	}
	if !strings.Contains(ack.GetError(), "reader identity") {
		t.Fatalf("refusal = %q, want it to name the missing reader identity", ack.GetError())
	}
}

func TestANextPageWithNoReaderIdentityIsRefused(t *testing.T) {
	// Arrange — the same rule on the other verb; a next page cannot be filed
	// against nobody either.
	h := &mockHandler{historyPage: &frontendv1.ConversationHistoryPage{}}

	// Act.
	ack, _ := DispatchWithResponse(context.Background(), nil, h, nil, nextPageCommand("r1", "/ws"))

	// Assert.
	if ack.GetOk() {
		t.Fatal("next_page with no reader identity was served")
	}
}

func TestAHistoryPageEchoesTheRequestIdItAnswers(t *testing.T) {
	// Arrange — the echo is the WHOLE mechanism by which a page in flight
	// across a generation change is discarded; there is no fence on this
	// surface.
	h := &mockHandler{historyPage: &frontendv1.ConversationHistoryPage{}}
	ctx := ContextWithReader(context.Background(), connectionReader(3))

	// Act.
	_, response := DispatchWithResponse(ctx, nil, h, nil, nextPageCommand("req-42", "/ws"))

	// Assert.
	if response.GetConversationHistoryPage().GetRequestId() != "req-42" {
		t.Fatalf("page request_id = %q, want req-42", response.GetConversationHistoryPage().GetRequestId())
	}
}

func TestAnAbsentHistoryPageWithNoErrorIsRefused(t *testing.T) {
	// Arrange — a nil page with a nil error is a construction defect: a client
	// cannot tell an absent page from an empty conversation.
	h := &mockHandler{}
	ctx := ContextWithReader(context.Background(), connectionReader(1))

	// Act.
	ack, _ := DispatchWithResponse(ctx, nil, h, nil, firstPageCommand("r1", "/ws"))

	// Assert.
	if ack.GetOk() {
		t.Fatal("a first_page that produced neither a page nor an error was acked ok")
	}
}
