package prompthandler

import (
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/wsm"
)

// DeliveryOf maps agentrepl.v1 SubmitPromptRequest.delivery onto the queue's
// delivery. It is the ONE mapping: the rpc's validation, the rpc's body and
// the held-prompt ingress's entry all read a delivery through it, so no two
// of them can disagree about what a value means.
//
// An ABSENT delivery (nil) is the ordinary one. UNSPECIFIED is never sent and
// a value this build does not know is never read as the ordinary delivery --
// a deferred prompt misread would be classified and could interject -- so
// both are errors.
func DeliveryOf(delivery *agentreplv1.SubmitPromptDelivery) (wsm.Delivery, error) {
	if delivery == nil {
		return wsm.DeliveryOrdinary, nil
	}
	switch *delivery {
	case agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_DEFERRED:
		return wsm.DeliveryDeferred, nil
	case agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_UNSPECIFIED:
		return 0, fmt.Errorf("a delivery, when set, is never UNSPECIFIED: the ordinary delivery is an absent field")
	default:
		return 0, fmt.Errorf("delivery %d is not a delivery this daemon honors", int32(*delivery))
	}
}
