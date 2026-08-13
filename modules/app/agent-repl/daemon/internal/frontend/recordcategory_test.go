package frontend

import (
	"strings"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/known/anypb"

	protocolv1 "agentrepl/proto/protocol/v1"
)

// TestEveryEventPayloadArmIsClassified is the totality check restated as a
// test, so a maintainer who adds a payload arm sees a named failure here in
// addition to the init() panic that stops the binary.
func TestEveryEventPayloadArmIsClassified(t *testing.T) {
	// Arrange.
	oneof := (&protocolv1.Event{}).ProtoReflect().Descriptor().Oneofs().ByName("payload")
	if oneof == nil {
		t.Fatal("agentshim.core.v1.Event has no `payload` oneof")
	}

	// Act / Assert.
	for i := 0; i < oneof.Fields().Len(); i++ {
		f := oneof.Fields().Get(i)
		class, ok := recordCategories[f.Number()]
		if !ok {
			t.Errorf("payload arm %s (field %d) has no A/B/C classification; an unclassified kind is a silent defect", f.Name(), f.Number())
			continue
		}
		if class.category == RecordCategoryUnset {
			t.Errorf("payload arm %s (field %d) is classified with the zero value, which is not a category", f.Name(), f.Number())
		}
	}
}

// TestNoOrphanedRecordCategory refuses a classification kept for a field the
// proto no longer declares, which a future arm reusing the number would inherit.
func TestNoOrphanedRecordCategory(t *testing.T) {
	// Arrange.
	oneof := (&protocolv1.Event{}).ProtoReflect().Descriptor().Oneofs().ByName("payload")
	declared := map[protoreflect.FieldNumber]bool{}
	for i := 0; i < oneof.Fields().Len(); i++ {
		declared[oneof.Fields().Get(i).Number()] = true
	}

	// Act / Assert.
	for number := range recordCategories {
		if !declared[number] {
			t.Errorf("field %d is classified but Event's payload oneof no longer declares it", number)
		}
	}
}

// categoryCVectors is EVERY category-C payload arm, one vector each, so a kind
// that quietly changes category fails on its own row rather than inside a
// bundle. The table is cross-checked against recordCategories below, so adding
// a C kind without a vector here is itself a failure.
var categoryCVectors = []struct {
	name    string
	payload func(*protocolv1.Event)
}{
	{"session_started", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_SessionStarted{SessionStarted: &protocolv1.SessionStarted{}} }},
	{"session_ended", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_SessionEnded{SessionEnded: &protocolv1.SessionEnded{}} }},
	{"turn_started", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_TurnStarted{TurnStarted: &protocolv1.TurnStarted{}} }},
	{"turn_ended", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_TurnEnded{TurnEnded: &protocolv1.TurnEnded{}} }},
	{"heartbeat_progress", func(e *protocolv1.Event) {
		e.Payload = &protocolv1.Event_HeartbeatProgress{HeartbeatProgress: &protocolv1.HeartbeatProgress{}}
	}},
	{"message_latency", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_MessageLatency{MessageLatency: &protocolv1.MessageLatency{}} }},
	{"file_plane_diagnostic", func(e *protocolv1.Event) {
		e.Payload = &protocolv1.Event_FilePlaneDiagnostic{FilePlaneDiagnostic: &protocolv1.FilePlaneDiagnostic{}}
	}},
	{"turn_claim_bridge", func(e *protocolv1.Event) {
		e.Payload = &protocolv1.Event_TurnClaimBridge{TurnClaimBridge: &protocolv1.TurnClaimBridge{}}
	}},
	{"query_lifecycle", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_QueryLifecycle{QueryLifecycle: &protocolv1.QueryLifecycle{}} }},
	{"account_usage_observation", func(e *protocolv1.Event) {
		e.Payload = &protocolv1.Event_AccountUsageObservation{AccountUsageObservation: &protocolv1.AccountUsageObservation{}}
	}},
	{"session_rewound", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_SessionRewound{SessionRewound: &protocolv1.SessionRewound{}} }},
	{"unparsed", func(e *protocolv1.Event) { e.Payload = &protocolv1.Event_Unparsed{Unparsed: &protocolv1.UnparsedEvent{}} }},
}

// TestEveryCategoryCKindCarriesNoOwnership walks every category-C kind
// individually: each must resolve to a STRUCTURALLY unowned record whose
// top_level_message_id accessor refuses.
func TestEveryCategoryCKindCarriesNoOwnership(t *testing.T) {
	for _, tc := range categoryCVectors {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			ev := &protocolv1.Event{Seq: 7}
			tc.payload(ev)

			// Act.
			own, err := ResolveRecordOwnership(ev, "")

			// Assert.
			if err != nil {
				t.Fatalf("resolving ownership for %s: %v", tc.name, err)
			}
			if !own.Unowned() {
				t.Fatalf("%s claims an owner; a category-C record with a top_level_message_id is a phantom page slot", tc.name)
			}
			if _, err := own.TopLevelMessageID(); err == nil {
				t.Fatalf("%s yielded a top_level_message_id instead of refusing; unowned must be structural, not an empty string", tc.name)
			}
		})
	}
}

// TestCategoryCVectorsCoverEveryCategoryCKind fails loudly when a kind is
// classified C in the table but has no vector above, so the per-kind coverage
// cannot silently stop being total.
func TestCategoryCVectorsCoverEveryCategoryCKind(t *testing.T) {
	// Arrange.
	covered := map[string]bool{}
	for _, tc := range categoryCVectors {
		covered[tc.name] = true
	}
	oneof := (&protocolv1.Event{}).ProtoReflect().Descriptor().Oneofs().ByName("payload")

	// Act / Assert.
	for i := 0; i < oneof.Fields().Len(); i++ {
		f := oneof.Fields().Get(i)
		if recordCategories[f.Number()].category != RecordCategoryNotAMessage {
			continue
		}
		if !covered[string(f.Name())] {
			t.Errorf("category-C kind %s has no vector in categoryCVectors, so nothing proves it carries no ownership", f.Name())
		}
	}
}

// TestCategoryCRecordRefusesAnOfferedOwner covers the other direction: a caller
// that hands a turn boundary an id must be refused, not quietly obeyed.
func TestCategoryCRecordRefusesAnOfferedOwner(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{Seq: 3, Payload: &protocolv1.Event_TurnStarted{TurnStarted: &protocolv1.TurnStarted{TurnId: "t1"}}}

	// Act.
	_, err := ResolveRecordOwnership(ev, "msg-1")

	// Assert.
	if err == nil {
		t.Fatal("a turn boundary accepted an owner; that write is exactly the phantom page slot the contract calls the highest-risk detail")
	}
}

// TestCategoryARecordNamesItsComposingMessage covers category A: a content
// delta's owner is the message id the producer already carries.
func TestCategoryARecordNamesItsComposingMessage(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{Seq: 11, Payload: &protocolv1.Event_ContentDelta{ContentDelta: &protocolv1.ContentDelta{Uuid: "msg-a"}}}

	// Act.
	own, err := ResolveRecordOwnership(ev, ev.GetContentDelta().GetUuid())

	// Assert.
	if err != nil {
		t.Fatalf("resolving ownership: %v", err)
	}
	got, err := own.TopLevelMessageID()
	if err != nil {
		t.Fatalf("reading owner: %v", err)
	}
	if got != "msg-a" {
		t.Fatalf("owner = %q, want the composed message id msg-a", got)
	}
}

// TestCategoryBRecordNamesItself covers category B: a clear marker IS a
// message, so its owner is its own id.
func TestCategoryBRecordNamesItself(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{Seq: 12, Payload: &protocolv1.Event_ContextCleared{ContextCleared: &protocolv1.ContextCleared{}}}

	// Act.
	own, err := ResolveRecordOwnership(ev, "msg-self")

	// Assert.
	if err != nil {
		t.Fatalf("resolving ownership: %v", err)
	}
	got, err := own.TopLevelMessageID()
	if err != nil {
		t.Fatalf("reading owner: %v", err)
	}
	if got != "msg-self" {
		t.Fatalf("owner = %q, want the record's own message id msg-self", got)
	}
}

// TestUnresolvableOwnerFailsLoudly covers the record whose owner cannot be
// resolved: a category-A record with no composing message id is refused rather
// than written unowned and lost to every page query.
func TestUnresolvableOwnerFailsLoudly(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{Seq: 13, Payload: &protocolv1.Event_ContentDelta{ContentDelta: &protocolv1.ContentDelta{}}}

	// Act.
	_, err := ResolveRecordOwnership(ev, "")

	// Assert.
	if err == nil {
		t.Fatal("a content delta with no owning message id resolved successfully; an unresolvable owner must fail loudly, never default")
	}
}

// TestVendorPayloadDefersToTheCurator covers the Any arm: the envelope cannot
// guess whether the payload was a message or composed one, so an unresolved
// vendor record is refused.
func TestVendorPayloadDefersToTheCurator(t *testing.T) {
	// Arrange.
	any, err := anypb.New(&protocolv1.ContextCleared{})
	if err != nil {
		t.Fatalf("building vendor payload: %v", err)
	}
	ev := &protocolv1.Event{Seq: 14, Payload: &protocolv1.Event_Vendor{Vendor: any}}

	// Act.
	_, err = ResolveRecordOwnership(ev, "")

	// Assert.
	if err == nil {
		t.Fatal("a vendor record resolved an owner without the curator supplying one; the envelope would have had to guess the category")
	}
}

// TestEventWithNoPayloadArmIsRefused covers the empty envelope: it is a
// producer fault, not an unowned record.
func TestEventWithNoPayloadArmIsRefused(t *testing.T) {
	// Arrange.
	ev := &protocolv1.Event{Seq: 15, SessionId: "s1"}

	// Act.
	_, err := CategorizeRecord(ev)

	// Assert.
	if err == nil {
		t.Fatal("an event with no payload arm was categorized; that answer could only be a guess")
	}
	if !strings.Contains(err.Error(), "NO payload arm") {
		t.Fatalf("refusal does not name the missing arm: %v", err)
	}
}

// TestNilEventIsRefused covers the nil record, which has no category to report.
func TestNilEventIsRefused(t *testing.T) {
	// Act.
	_, err := CategorizeRecord(nil)

	// Assert.
	if err == nil {
		t.Fatal("a nil event was categorized")
	}
}
