package workspace

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
)

// THE FEED'S PEER DELIVERY SOURCE (feed.PeerDeliverySource): which sessions
// hold the two ends of one cross-session message, read from the host's one store
// through a workspace's shim. Any shim answers for any session, because the
// store is shared, so the feed asks through the shim of the workspace whose row
// needs the answer.

// peerDeliveryResolver is the one shim call ResolvePeerDelivery needs.
type peerDeliveryResolver interface {
	ResolvePeerDelivery(ctx context.Context, req *shimv1.ResolvePeerDeliveryRequest) (*shimv1.ResolvePeerDeliveryResponse, error)
}

// ResolvePeerDelivery asks WS's shim which sessions hold the two ends of
// DELIVERYID. A workspace with no shim answers feed.ErrNoHistorySource, exactly
// as a history read does.
func (f *Fleet) ResolvePeerDelivery(ctx context.Context, ws ids.WorkspaceID, deliveryID string) (feed.PeerDeliveryEnds, error) {
	return throughShim(f, ws, func(client shimclient.Client) (feed.PeerDeliveryEnds, error) {
		return resolvePeerDelivery(ctx, client, deliveryID)
	})
}

// throughShim runs READ against WS's shim: the one shape of every store-backed
// lookup the feed asks through a workspace. A workspace with no shim answers
// feed.ErrNoHistorySource, and a read a stand-down this daemon ordered cut
// short answers the same, recorded at INFO, exactly as a history read does.
func throughShim[T any](f *Fleet, ws ids.WorkspaceID, read func(shimclient.Client) (T, error)) (T, error) {
	var none T
	client, ok := f.Client(ws)
	if !ok {
		return none, feed.ErrNoHistorySource
	}
	answer, err := read(client)
	if errors.Is(err, shimclient.ErrStandDownOrdered) {
		return none, f.readEndedInStandDown(ws, err)
	}
	return answer, err
}

// resolvePeerDelivery makes the one ResolvePeerDelivery call and unwraps its
// outcome.
func resolvePeerDelivery(ctx context.Context, client peerDeliveryResolver, deliveryID string) (feed.PeerDeliveryEnds, error) {
	response, err := client.ResolvePeerDelivery(ctx, &shimv1.ResolvePeerDeliveryRequest{
		DeliveryId: &conversationv1.PeerDeliveryId{Value: deliveryID},
	})
	if err != nil {
		return feed.PeerDeliveryEnds{}, err
	}
	if failure := response.GetFailure(); failure != nil {
		return feed.PeerDeliveryEnds{}, &ShimRefusal{Verb: "ResolvePeerDelivery", Arm: resolvePeerDeliveryArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil {
		return feed.PeerDeliveryEnds{}, fmt.Errorf("shim ResolvePeerDelivery answered neither its ends nor a failure for delivery %q", deliveryID)
	}
	sender, err := peerDeliveryEnd(success.GetSender(), "sender")
	if err != nil {
		return feed.PeerDeliveryEnds{}, err
	}
	recipient, err := peerDeliveryEnd(success.GetRecipient(), "recipient")
	if err != nil {
		return feed.PeerDeliveryEnds{}, err
	}
	return feed.PeerDeliveryEnds{Sender: sender, Recipient: recipient}, nil
}

// peerDeliveryEnd reads one end: its session's main agent id, "" when not
// recorded, and a shim defect for an end stating neither.
func peerDeliveryEnd(end *shimv1.ResolvePeerDeliveryEnd, which string) (string, error) {
	switch arm := end.GetEnd().(type) {
	case *shimv1.ResolvePeerDeliveryEnd_Session:
		if arm.Session.GetValue() == "" {
			return "", fmt.Errorf("shim ResolvePeerDelivery named the %s end's session empty", which)
		}
		return arm.Session.GetValue(), nil
	case *shimv1.ResolvePeerDeliveryEnd_NotRecorded:
		return "", nil
	default:
		return "", fmt.Errorf("shim ResolvePeerDelivery stated no arm for the %s end", which)
	}
}

// resolvePeerDeliveryArm names a ResolvePeerDelivery refusal's arm.
func resolvePeerDeliveryArm(failure *shimv1.ResolvePeerDeliveryFailure) string {
	if failure.GetStoreUnavailable() != nil {
		return "store_unavailable"
	}
	return ArmShimUnspecified
}
