package integration

import (
	"testing"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/proto/store/v1/storev1connect"
	storereadv1 "agentrepl/proto/storeread/v1"
	"agentrepl/proto/storeread/v1/storereadv1connect"

	"agentrepl/logging/storesocket"
)

// readClient is the read-only face's client, dialled where the daemon dials
// it: beside the producers' socket.
func (s *storeProcess) readClient() storereadv1connect.StoreReadClient {
	return storereadv1connect.NewStoreReadClient(h2cHTTPClient(storesocket.ReadPath(s.socket)), baseURL)
}

// TestTheReadOnlySocketServesWhatTheProducersWrote: a line a producer wrote
// through the producers' socket is read back, rendered as a history page,
// through the read-only socket.
func TestTheReadOnlySocketServesWhatTheProducersWrote(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	seedBook(ctx, t, streamProducer(store.client()), "main", "read-only")

	// Act
	resp, err := store.readClient().ReadHistoryPage(ctx, connect.NewRequest(&storereadv1.ReadHistoryPageRequest{
		Book: &conversationv1.AgentId{Value: "main"},
		Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: &storereadv1.ReadHistoryPageNewest{
			KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}},
		}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ReadHistoryPage: %v", err)
	}
	page := resp.Msg.GetSuccess().GetPage()
	if len(page.GetEntries()) != 1 || page.GetEntries()[0].GetEntry().GetAgentFrame() == nil || page.GetFloor() == nil {
		t.Fatalf("page = %v, want the one seeded frame at the floor", resp.Msg)
	}
}

// TestTheReadOnlySocketRefusesAWrite: a producer's write sent to the read-only
// socket finds no such procedure, and writes nothing.
func TestTheReadOnlySocketRefusesAWrite(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	onReadSocket := storev1connect.NewShimStoreClient(h2cHTTPClient(storesocket.ReadPath(store.socket)), baseURL)
	shim := streamProducer(onReadSocket)

	// Act
	_, err := shim.attempt(ctx, &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		shim.agentEntry("w-sneak", "u-sneak", frameLine(agentID("main"), responseFrame("main", "act-sneak", "sneak"))),
	}})

	// Assert
	if code := connect.CodeOf(err); code != connect.CodeNotFound && code != connect.CodeUnimplemented {
		t.Fatalf("WriteBatch on the read-only socket err = %v (code %v), want no such procedure", err, code)
	}
	resp, readErr := store.readClient().ReadHistoryPage(ctx, connect.NewRequest(&storereadv1.ReadHistoryPageRequest{
		Book: &conversationv1.AgentId{Value: "main"},
		Position: &storereadv1.ReadHistoryPageRequest_Newest{Newest: &storereadv1.ReadHistoryPageNewest{
			KnownAs: &storereadv1.ReadHistoryPageNewest_Named{Named: &storereadv1.ReadHistoryPageNewestNamed{}},
		}},
	}))
	if readErr != nil {
		t.Fatalf("ReadHistoryPage: %v", readErr)
	}
	if resp.Msg.GetFailure().GetUnknownAgent() == nil {
		t.Fatalf("after the refused write the book reads %v, want no such agent", resp.Msg)
	}
}
