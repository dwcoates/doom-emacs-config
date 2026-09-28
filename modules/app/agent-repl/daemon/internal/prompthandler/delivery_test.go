package prompthandler

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/wsm"
)

func TestDeliveryOfMapsEveryValue(t *testing.T) {
	deferred := agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_DEFERRED
	unspecified := agentreplv1.SubmitPromptDelivery_SUBMIT_PROMPT_DELIVERY_UNSPECIFIED
	unknown := agentreplv1.SubmitPromptDelivery(99)
	tests := []struct {
		name    string
		in      *agentreplv1.SubmitPromptDelivery
		want    wsm.Delivery
		wantErr bool
	}{
		{name: "an absent delivery is ordinary", in: nil, want: wsm.DeliveryOrdinary},
		{name: "DEFERRED is deferred", in: &deferred, want: wsm.DeliveryDeferred},
		{name: "UNSPECIFIED is never sent", in: &unspecified, wantErr: true},
		{name: "an unknown value is never read as ordinary", in: &unknown, wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := DeliveryOf(tc.in)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("DeliveryOf error = %v, want error %v", err, tc.wantErr)
			}
			if err == nil && got != tc.want {
				t.Fatalf("DeliveryOf = %v, want %v", got, tc.want)
			}
		})
	}
}
