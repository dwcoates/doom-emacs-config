package errclass

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

func TestCardWithFactsSetsExactlyOneKindArm(t *testing.T) {
	// Arrange — populate fills fields INSIDE the arm kindFor chose; it can
	// neither pick a different arm nor set a second one, and this is the
	// assertion that keeps that structural rather than reviewed.
	facts := Facts{
		RequestID: "r1", Reason: "no", Cause: "boom", Component: "c",
		WaitedMs: 1, DroppedCount: 2, HTTPStatus: 500, Attempts: 3,
		Model: "m", StopReason: "s", UncachedInputTokens: 4, RawReason: "raw", SinceMs: 5,
		Vendor: &frontendv1.VendorFailureContext{ApiRequestId: "req"},
	}
	for _, typ := range AllTypes() {
		// Act.
		card := CardWithFacts(typ, "detail", facts)
		// Assert.
		if got := setOneofFields(card.GetKind()); got != 1 {
			t.Errorf("%s set %d arms of FailureKind, want exactly 1", typ, got)
		}
	}
}

// setOneofFields counts how many members of FailureKind's `kind` oneof are set.
func setOneofFields(kind *frontendv1.FailureKind) int {
	set := 0
	kind.ProtoReflect().Range(func(fd protoreflect.FieldDescriptor, _ protoreflect.Value) bool {
		if fd.ContainingOneof() != nil {
			set++
		}
		return true
	})
	return set
}

func TestCardWithFactsNeverDropsTheRawDetail(t *testing.T) {
	// Arrange — the typed fields are what a renderer acts on; the detail is
	// what a human reads when they turn out not to have been enough.
	for _, typ := range AllTypes() {
		// Act.
		card := CardWithFacts(typ, "the raw account", Facts{HTTPStatus: 429})
		// Assert.
		if card.GetDetail() != "the raw account" {
			t.Errorf("%s dropped its detail: got %q", typ, card.GetDetail())
		}
	}
}

func TestCardIsCardWithFactsCarryingNoEvidence(t *testing.T) {
	// Arrange — the plain constructor must stay the zero-evidence case of the
	// one construction site rather than a second site beside it.
	// Act.
	plain := Card(TypeShimAckTimeout, "d")
	empty := CardWithFacts(TypeShimAckTimeout, "d", Facts{})
	// Assert.
	if !proto.Equal(plain, empty) {
		t.Fatalf("Card = %v, CardWithFacts with no facts = %v", plain, empty)
	}
}

func TestAnAckTimeoutCarriesTheRequestItWaitedOn(t *testing.T) {
	// Arrange + Act.
	card := CardWithFacts(TypeShimAckTimeout, "d", Facts{RequestID: "r-77", WaitedMs: 5000})
	// Assert.
	arm := card.GetKind().GetShimAckTimeout()
	if arm.GetRequestId() != "r-77" || arm.GetWaitedMs() != 5000 {
		t.Fatalf("ack timeout arm = %v, want the request and the wait", arm)
	}
}

func TestAStoreWriteRejectionCarriesHowMuchWasDropped(t *testing.T) {
	// Arrange — how much conversation was lost is the single most useful thing
	// about a store outage.
	// Act.
	card := CardWithFacts(TypeShimStoreWriteRejected, "d", Facts{Component: "writer", Reason: "disk full", DroppedCount: 412})
	// Assert.
	if card.GetKind().GetShimStoreWriteRejected().GetDroppedCount() != 412 {
		t.Fatalf("dropped count = %d, want 412", card.GetKind().GetShimStoreWriteRejected().GetDroppedCount())
	}
}

func TestAVendorFailureCarriesItsRequestContext(t *testing.T) {
	// Arrange + Act.
	card := CardWithFacts(TypeAPIRateLimit, "d", Facts{
		Vendor:     &frontendv1.VendorFailureContext{ApiRequestId: "req_abc"},
		HTTPStatus: 429,
		Attempts:   3,
	})
	// Assert.
	arm := card.GetKind().GetApiRateLimit()
	if arm.GetVendor().GetApiRequestId() != "req_abc" || arm.GetHttpStatus() != 429 || arm.GetAttempts() != 3 {
		t.Fatalf("rate limit arm = %v, want the vendor context, status and attempts", arm)
	}
}

func TestAModelNotFoundNamesTheModelThatWasAskedFor(t *testing.T) {
	// Arrange + Act.
	card := CardWithFacts(TypeAPIModelNotFound, "d", Facts{Model: "opus-9"})
	// Assert.
	if card.GetKind().GetApiModelNotFound().GetModel() != "opus-9" {
		t.Fatalf("model = %q, want the model named", card.GetKind().GetApiModelNotFound().GetModel())
	}
}

func TestAnUnnamedTurnFailureCarriesTheVendorsOwnStopReason(t *testing.T) {
	// Arrange — this kind IS "the vendor stopped for a reason with no kind of
	// its own", so the raw reason is the whole content of the card.
	// Act.
	card := CardWithFacts(TypeAPITurnFailed, "d", Facts{StopReason: "weird_stop"})
	// Assert.
	if card.GetKind().GetApiTurnFailed().GetStopReason() != "weird_stop" {
		t.Fatalf("stop reason = %q", card.GetKind().GetApiTurnFailed().GetStopReason())
	}
}

func TestVendorContextIsAbsentWhenNothingWasObserved(t *testing.T) {
	// Arrange — three empty strings are not evidence, and carrying them would
	// claim the failure named a conversation it never did.
	// Act.
	got := vendorContext("", "", "")
	// Assert.
	if got != nil {
		t.Fatalf("vendorContext with no evidence = %v, want nil", got)
	}
}

func TestVendorContextIsBuiltFromWhateverOneFieldWasObserved(t *testing.T) {
	// Arrange + Act.
	got := vendorContext("", "req_1", "")
	// Assert.
	if got.GetApiRequestId() != "req_1" {
		t.Fatalf("vendorContext = %v, want the request id it was given", got)
	}
}
