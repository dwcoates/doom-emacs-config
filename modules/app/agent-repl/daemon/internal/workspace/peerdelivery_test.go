package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/resolve/feed"
)

// fakePeerDeliveryResolver answers one scripted ResolvePeerDelivery and records
// the request.
type fakePeerDeliveryResolver struct {
	req  *shimv1.ResolvePeerDeliveryRequest
	resp *shimv1.ResolvePeerDeliveryResponse
	err  error
}

func (f *fakePeerDeliveryResolver) ResolvePeerDelivery(_ context.Context, req *shimv1.ResolvePeerDeliveryRequest) (*shimv1.ResolvePeerDeliveryResponse, error) {
	f.req = req
	return f.resp, f.err
}

// deliveryEnd is one answered end: SESSION, or not recorded for "".
func deliveryEnd(session string) *shimv1.ResolvePeerDeliveryEnd {
	if session == "" {
		return &shimv1.ResolvePeerDeliveryEnd{End: &shimv1.ResolvePeerDeliveryEnd_NotRecorded{NotRecorded: &shimv1.ResolvePeerDeliveryNotRecorded{}}}
	}
	return &shimv1.ResolvePeerDeliveryEnd{End: &shimv1.ResolvePeerDeliveryEnd_Session{Session: &conversationv1.AgentId{Value: session}}}
}

// endsResponse is a success naming SENDER and RECIPIENT.
func endsResponse(sender, recipient *shimv1.ResolvePeerDeliveryEnd) *shimv1.ResolvePeerDeliveryResponse {
	return &shimv1.ResolvePeerDeliveryResponse{Result: &shimv1.ResolvePeerDeliveryResponse_Success{Success: &shimv1.ResolvePeerDeliverySuccess{
		Sender: sender, Recipient: recipient,
	}}}
}

func TestResolvePeerDeliveryAsksForTheDeliveryItWasGiven(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: endsResponse(deliveryEnd("a"), deliveryEnd("b"))}

	// Act.
	_, _ = resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if got := client.req.GetDeliveryId().GetValue(); got != "m-1" {
		t.Fatalf("delivery id = %q, want m-1", got)
	}
}

func TestResolvePeerDeliveryAnswersBothEnds(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: endsResponse(deliveryEnd("session-a"), deliveryEnd("session-b"))}

	// Act.
	ends, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if err != nil || ends != (feed.PeerDeliveryEnds{Sender: "session-a", Recipient: "session-b"}) {
		t.Fatalf("ends = %+v, %v; want both sessions", ends, err)
	}
}

func TestResolvePeerDeliveryAnswersAnEndNotRecordedAsEmpty(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: endsResponse(deliveryEnd("session-a"), deliveryEnd(""))}

	// Act.
	ends, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if err != nil || ends != (feed.PeerDeliveryEnds{Sender: "session-a"}) {
		t.Fatalf("ends = %+v, %v; want the recipient empty", ends, err)
	}
}

func TestResolvePeerDeliveryRefusalIsATypedShimRefusal(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: &shimv1.ResolvePeerDeliveryResponse{Result: &shimv1.ResolvePeerDeliveryResponse_Failure{
		Failure: &shimv1.ResolvePeerDeliveryFailure{
			Detail: "database is locked",
			Kind:   &shimv1.ResolvePeerDeliveryFailure_StoreUnavailable{StoreUnavailable: &shimv1.ResolvePeerDeliveryStoreUnavailable{}},
		},
	}}}

	// Act.
	_, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "store_unavailable" {
		t.Fatalf("err = %v, want a store_unavailable ShimRefusal", err)
	}
}

func TestResolvePeerDeliveryTransportErrorPassesThrough(t *testing.T) {
	// Arrange.
	cause := errors.New("connection refused")
	client := &fakePeerDeliveryResolver{err: cause}

	// Act.
	_, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the transport error", err)
	}
}

func TestResolvePeerDeliveryAnswerWithNoArmIsAnError(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: &shimv1.ResolvePeerDeliveryResponse{}}

	// Act.
	_, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if err == nil {
		t.Fatal("resolvePeerDelivery accepted an answer with no arm")
	}
}

func TestResolvePeerDeliveryEndWithNoArmIsAnError(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: endsResponse(deliveryEnd("session-a"), &shimv1.ResolvePeerDeliveryEnd{})}

	// Act.
	_, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if err == nil {
		t.Fatal("resolvePeerDelivery accepted an end with no arm")
	}
}

func TestResolvePeerDeliveryEndNamingNoSessionIsAnError(t *testing.T) {
	// Arrange.
	client := &fakePeerDeliveryResolver{resp: endsResponse(deliveryEnd("session-a"), &shimv1.ResolvePeerDeliveryEnd{
		End: &shimv1.ResolvePeerDeliveryEnd_Session{Session: &conversationv1.AgentId{}},
	})}

	// Act.
	_, err := resolvePeerDelivery(context.Background(), client, "m-1")

	// Assert.
	if err == nil {
		t.Fatal("resolvePeerDelivery accepted an end naming an empty session")
	}
}

func TestFleetResolvePeerDeliveryWithNoShimHasNoSource(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, err := f.fleet.ResolvePeerDelivery(context.Background(), ws.ID, "m-1")

	// Assert.
	if !errors.Is(err, feed.ErrNoHistorySource) {
		t.Fatalf("err = %v, want ErrNoHistorySource", err)
	}
}
