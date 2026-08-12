// recordcategory.go is the TOTAL classification of every durable record the
// daemon can be handed, into the three categories the pagination contract
// names, and the one place a record's `top_level_message_id` is resolved.
//
// THE THREE CATEGORIES (FROZEN-message-lineage, Part 3)
//
//	A — COMPOSES a message. The owner is knowable AT WRITE TIME from fields the
//	    producer already carries: ContentDelta.uuid IS the owning message id, a
//	    task-progress record names the detached work it accumulates into.
//	B — IS a message. Task lifecycle, failure cards, clear/compact markers,
//	    permission items, intercepted commands. The owner is ITSELF.
//	C — NOT a message at all. It renders as nothing and carries NO ownership.
//
// WHY C IS THE TRAP, AND WHY THIS FILE EXISTS. The page query is
// `SELECT DISTINCT top_level_message_id ... LIMIT 10`. A category-C record that
// acquires an owner becomes a PHANTOM PAGE SLOT: the query returns ten
// "messages", several of which are turn boundaries, and the user sees a short
// page with no error anywhere. That failure is SILENT, so "unowned" is modelled
// here as a STRUCTURAL absence — RecordOwnership with no id inside it, whose
// accessor REFUSES rather than yielding "" — and never as an empty string that
// would sort into the query alongside real messages.
//
// WHY A TABLE AND NOT A SWITCH WITH A DEFAULT. A `default` arm answers for
// kinds that do not exist yet. Whatever it answers is wrong for at least one of
// them, and being wrong in the C direction loses a message while being wrong in
// the A/B direction mints a phantom slot — both silently. So the classification
// is a table keyed by the payload oneof's FIELD NUMBERS, and package init walks
// the compiled descriptor and PANICS if the proto grew an arm this table does
// not answer for. The descriptor is compiled in, so that panic is deterministic
// on every run of every binary and every test: adding an Event payload arm
// without classifying it stops the daemon at start, which is the loudest
// failure Go affords for a fact it cannot check at compile time.
package frontend

import (
	"fmt"

	"google.golang.org/protobuf/reflect/protoreflect"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// RecordCategory is which of the contract's three categories one durable record
// falls in. There is no zero-value category on purpose: a variable that was
// never assigned is not silently "C".
type RecordCategory int

const (
	// RecordCategoryUnset is the zero value and is never a table entry. It
	// exists so that a RecordCategory nobody assigned cannot be mistaken for
	// RecordCategoryNotAMessage, which is the value that suppresses ownership.
	RecordCategoryUnset RecordCategory = iota

	// RecordCategoryComposesMessage is category A: the record contributes to a
	// message it does not itself constitute, and it names that message through
	// a field its producer already carries.
	RecordCategoryComposesMessage

	// RecordCategoryIsMessage is category B: the record IS a message, so its
	// top_level_message_id is its own message id.
	RecordCategoryIsMessage

	// RecordCategoryNotAMessage is category C: the record renders as nothing
	// and MUST carry no ownership at all.
	RecordCategoryNotAMessage

	// RecordCategoryVendorPayload is the `vendor` Any arm, whose category is a
	// property of the payload INSIDE it and not of the envelope.
	//
	// It is a distinct answer rather than a guess at A or B because the
	// envelope genuinely does not know: one Any carries an assistant message
	// (B) and the next carries a tool result composing one (A). Naming the
	// deferral keeps the table total without letting the envelope answer a
	// question only the vendor curator can.
	RecordCategoryVendorPayload
)

// String names the category in errors and logs.
func (c RecordCategory) String() string {
	switch c {
	case RecordCategoryUnset:
		return "unset"
	case RecordCategoryComposesMessage:
		return "A/composes-message"
	case RecordCategoryIsMessage:
		return "B/is-message"
	case RecordCategoryNotAMessage:
		return "C/not-a-message"
	case RecordCategoryVendorPayload:
		return "vendor-payload/deferred"
	default:
		return fmt.Sprintf("unknown(%d)", int(c))
	}
}

// recordClass is one row of the classification: the category and the REASON,
// which is carried so an error can say why a record was expected to have (or
// not have) an owner rather than only that it did not.
type recordClass struct {
	category RecordCategory
	why      string
}

// recordCategories classifies EVERY arm of agentshim.core.v1.Event's `payload`
// oneof, keyed by field number so a rename cannot silently orphan a row.
//
// Field numbers rather than the generated wrapper types because a wrapper type
// is a Go identifier the compiler will happily let this table omit, whereas the
// descriptor walk in init() compares against the numbers the proto itself
// declares — the same source of truth the store and the shim read.
var recordCategories = map[protoreflect.FieldNumber]recordClass{
	// ---- Category C: the eleven kinds the contract names, and one more ----
	10: {RecordCategoryNotAMessage, "session_started is a session boundary; it configures the view and renders no conversation content"},
	11: {RecordCategoryNotAMessage, "session_ended is a session boundary; it renders no conversation content"},
	12: {RecordCategoryNotAMessage, "turn_started is a turn boundary; owning a message would make every turn a phantom page slot"},
	13: {RecordCategoryNotAMessage, "turn_ended is a turn boundary; owning a message would make every turn a phantom page slot"},
	18: {RecordCategoryNotAMessage, "heartbeat_progress is a liveness signal relayed as HeartbeatView; it is never a feed row"},
	21: {RecordCategoryNotAMessage, "message_latency is a timing sample kept for analysis on replay; it renders as nothing"},
	24: {RecordCategoryNotAMessage, "file_plane_diagnostic is a sidecar runtime diagnostic written to sidecar.log; consumers must never render it as conversation material"},
	25: {RecordCategoryNotAMessage, "turn_claim_bridge is correlation evidence for the durable turn ledger and deliberately NOT a lifecycle boundary, let alone a message"},
	26: {RecordCategoryNotAMessage, "query_lifecycle records facts about one SDK query() invocation; it renders as nothing"},
	27: {RecordCategoryNotAMessage, "account_usage_observation is a subscription-usage measurement taken at a turn boundary; it renders as nothing"},
	28: {RecordCategoryNotAMessage, "session_rewound is durable correlation evidence explaining a vendor-session identity change; it renders as nothing"},
	// unparsed is NOT in the contract's enumerated C list, which enumerates the
	// kinds that existed when it was written rather than closing the category.
	// It is classified C on the same test the contract applies: it is a
	// conversion-failure diagnostic that the daemon LOGS and counts toward
	// backfill accounting, and no path in this package ever turns one into a
	// frontend Message. Giving it an owner would spend a page slot on a parse
	// error.
	20: {RecordCategoryNotAMessage, "unparsed is a conversion-failure diagnostic; the daemon logs it and counts it toward backfill accounting, and never renders it as conversation"},

	// ---- Category B: the record IS a message ----
	14: {RecordCategoryIsMessage, "task_started opens detached work, and detached work IS a message whose uuid is the work's id (FROZEN-message-lineage rule 5: it is a feed row naming itself)"},
	16: {RecordCategoryIsMessage, "task_ended closes detached work, and it is folded into that same message rather than minting a second one"},
	19: {RecordCategoryIsMessage, "degraded_state becomes a failure card, and a failure card is a message whose owner is itself"},
	22: {RecordCategoryIsMessage, "context_cleared is a clear marker, which the contract names as a message"},
	23: {RecordCategoryIsMessage, "context_compacted is a compact marker, which the contract names as a message"},

	// ---- Category A: the record composes a message it names ----
	15: {RecordCategoryComposesMessage, "task_progress accumulates into the detached-work message its task id names; an update is not a message"},
	17: {RecordCategoryComposesMessage, "content_delta composes the message whose id it carries — ContentDelta.uuid IS the owning message id"},

	// ---- The Any arm, whose category lives inside it ----
	30: {RecordCategoryVendorPayload, "vendor carries an opaque Any whose category is a property of the payload inside it; the vendor curator resolves it"},
}

// eventPayloadOneof is the descriptor of Event's `payload` oneof, resolved once.
var eventPayloadOneof protoreflect.OneofDescriptor

// init makes an UNCLASSIFIED payload arm a startup failure rather than a
// default.
//
// Go cannot make a map exhaustive over a proto oneof at compile time, so this
// is the earliest and loudest moment available: the descriptor is compiled into
// the binary, so a new arm added without a row here panics deterministically on
// every run of every binary and every test in this package's dependency graph.
// It is the same failure a compile error would be, one phase later.
//
// It checks BOTH directions. A row for a field number the proto no longer
// declares is equally a defect: it means a classification decision is being
// kept for a kind nothing produces, and the next arm to claim that number would
// silently inherit it.
func init() {
	eventPayloadOneof = (&corev1.Event{}).ProtoReflect().Descriptor().Oneofs().ByName("payload")
	if eventPayloadOneof == nil {
		panic("frontend: agentshim.core.v1.Event has no `payload` oneof, so no durable record can be classified into the contract's three categories and every record would silently become unowned")
	}
	declared := make(map[protoreflect.FieldNumber]protoreflect.Name, eventPayloadOneof.Fields().Len())
	for i := 0; i < eventPayloadOneof.Fields().Len(); i++ {
		f := eventPayloadOneof.Fields().Get(i)
		declared[f.Number()] = f.Name()
		class, ok := recordCategories[f.Number()]
		if !ok {
			panic(fmt.Sprintf("frontend: RECORD CATEGORY MISSING for Event payload arm %s (field %d): every durable record must be classified A (composes a message), B (is a message) or C (not a message), and an unclassified kind is a SILENT defect — classified C by accident it loses the message, classified A or B by accident it mints a phantom page slot. Add a row to recordCategories.", f.Name(), f.Number()))
		}
		if class.category == RecordCategoryUnset {
			panic(fmt.Sprintf("frontend: RECORD CATEGORY UNSET for Event payload arm %s (field %d): the zero value is not a category, and a record left at it would be treated as owning nothing without anyone having decided that", f.Name(), f.Number()))
		}
		if class.why == "" {
			panic(fmt.Sprintf("frontend: RECORD CATEGORY UNEXPLAINED for Event payload arm %s (field %d): the reason is carried into every ownership error, so a row without one produces a refusal nobody can act on", f.Name(), f.Number()))
		}
	}
	for number := range recordCategories {
		if _, ok := declared[number]; !ok {
			panic(fmt.Sprintf("frontend: RECORD CATEGORY ORPHANED: field %d is classified here but Event's `payload` oneof no longer declares it, so a future arm taking that number would inherit a decision made about a different kind", number))
		}
	}
}

// CategorizeRecord states which category one durable record falls in.
//
// It refuses rather than defaulting in every case it cannot answer: a nil
// event, an event with no payload arm set, and — defensively, since init()
// already made it unreachable — an arm with no classification. None of these
// returns a category, because the only category that could be returned by
// convention is C, and returning C for something unrecognized is exactly how a
// message gets silently dropped from paging.
func CategorizeRecord(ev *corev1.Event) (RecordCategory, error) {
	if ev == nil {
		return RecordCategoryUnset, fmt.Errorf("frontend: record categorization refused: no event was supplied, so there is no payload arm to classify and no identity to name in this error")
	}
	field := ev.ProtoReflect().WhichOneof(eventPayloadOneof)
	if field == nil {
		return RecordCategoryUnset, fmt.Errorf("frontend: record categorization refused seq=%d session=%s: the event sets NO payload arm, so it is neither a message, part of one, nor a recognized non-message record — an envelope with nothing in it is a producer fault, not an unowned record", ev.GetSeq(), ev.GetSessionId())
	}
	class, ok := recordCategories[field.Number()]
	if !ok {
		return RecordCategoryUnset, fmt.Errorf("frontend: record categorization refused seq=%d payload=%s (field %d): the arm carries no A/B/C classification, so its ownership cannot be resolved without guessing", ev.GetSeq(), field.Name(), field.Number())
	}
	return class.category, nil
}

// RecordOwnership is the answer to "which message does this record belong to",
// in a shape where "none" is STRUCTURAL.
//
// The unowned case carries no id field a caller could read as "". A caller that
// asks an unowned record for its top_level_message_id gets an ERROR, so the
// only way a turn boundary's ownership reaches a store column is if someone
// ignored an error on the way — which is loud — rather than by a zero value
// flowing through unnoticed, which is silent and is precisely the phantom page
// slot the contract calls the highest-risk detail in the design.
type RecordOwnership struct {
	owned             bool
	topLevelMessageID string
	// why is the classification reason, repeated back in the refusal so the
	// caller learns WHY this record has no owner rather than only that it does
	// not.
	why string
}

// Unowned reports whether this record belongs to no message at all — category
// C. A caller writing store columns branches on this and writes NO ownership
// column value, rather than writing an empty one.
func (o RecordOwnership) Unowned() bool { return !o.owned }

// TopLevelMessageID yields the value to write into `top_level_message_id`, and
// REFUSES when the record has no owner.
//
// The refusal is the whole point of the type. It cannot return "" for an
// unowned record, so a caller cannot accidentally persist an empty owner that
// `SELECT DISTINCT top_level_message_id` would return as an eleventh, invisible
// "message".
func (o RecordOwnership) TopLevelMessageID() (string, error) {
	if !o.owned {
		return "", fmt.Errorf("frontend: this record has NO top_level_message_id and must be written with none: %s. Asking for one means a category-C record was about to be given an owner, which turns it into a phantom page slot the user sees as a short page with no error anywhere", o.why)
	}
	if o.topLevelMessageID == "" {
		return "", fmt.Errorf("frontend: this record claims an owner but names none, which is corruption rather than a variant: %s", o.why)
	}
	return o.topLevelMessageID, nil
}

// Unowned is the constructor for a category-C record's ownership.
func unownedRecord(why string) RecordOwnership {
	return RecordOwnership{owned: false, why: why}
}

// ResolveRecordOwnership resolves, AT WRITE TIME, the top_level_message_id one
// durable record must carry.
//
// messageID is the id the producer ALREADY CARRIES for this record:
//   - category A: the id of the message the record composes (ContentDelta.uuid,
//     the task id a progress record accumulates into).
//   - category B: the record's OWN message id, which is also its owner.
//   - category C: ignored, and supplying one is refused — a caller that has an
//     id in hand for a turn boundary has confused provenance with containment,
//     and letting it through is how the phantom slot gets written.
//
// It NEVER derives ownership by walking parents. That traversal is unbounded
// and is the exact cost the denormalized column exists to remove; a record
// whose owner is not knowable from what the producer already holds is a
// producer fault and is refused here, loudly, rather than repaired by a walk.
func ResolveRecordOwnership(ev *corev1.Event, messageID string) (RecordOwnership, error) {
	category, err := CategorizeRecord(ev)
	if err != nil {
		return RecordOwnership{}, err
	}
	field := ev.ProtoReflect().WhichOneof(eventPayloadOneof)
	class := recordCategories[field.Number()]

	switch category {
	case RecordCategoryNotAMessage:
		if messageID != "" {
			return RecordOwnership{}, fmt.Errorf("frontend: ownership refused seq=%d payload=%s message_id=%s: this record is NOT a message (%s), so it must be written with no top_level_message_id at all; a caller holding an id for it has confused provenance with containment, and persisting that id would make this record a phantom page slot", ev.GetSeq(), field.Name(), messageID, class.why)
		}
		return unownedRecord(fmt.Sprintf("payload=%s is category C — %s", field.Name(), class.why)), nil

	case RecordCategoryComposesMessage:
		if messageID == "" {
			return RecordOwnership{}, fmt.Errorf("frontend: ownership refused seq=%d payload=%s: this record COMPOSES a message (%s) but the owning message id it should already carry is empty; the owner is not derived by walking parents, so an absent one is a producer fault and this record is refused rather than written unowned and lost to every page query", ev.GetSeq(), field.Name(), class.why)
		}
		return RecordOwnership{owned: true, topLevelMessageID: messageID, why: fmt.Sprintf("payload=%s is category A — %s", field.Name(), class.why)}, nil

	case RecordCategoryIsMessage:
		if messageID == "" {
			return RecordOwnership{}, fmt.Errorf("frontend: ownership refused seq=%d payload=%s: this record IS a message (%s) so its top_level_message_id is its OWN id, but no id was supplied and a message that names nothing cannot be selected by any page query", ev.GetSeq(), field.Name(), class.why)
		}
		return RecordOwnership{owned: true, topLevelMessageID: messageID, why: fmt.Sprintf("payload=%s is category B — %s", field.Name(), class.why)}, nil

	case RecordCategoryVendorPayload:
		// The envelope defers, so the CURATOR must have resolved the owner. An
		// empty id here means nothing resolved it, and guessing between "the
		// Any was an assistant message" and "the Any composed one" is exactly
		// the guess that writes a wrong owner.
		if messageID == "" {
			return RecordOwnership{}, fmt.Errorf("frontend: ownership refused seq=%d payload=vendor: the vendor Any's category is a property of the payload inside it (%s), so the curator that unmarshaled it must supply the owning message id; resolving it here would mean guessing whether the payload was a message or composed one", ev.GetSeq(), class.why)
		}
		return RecordOwnership{owned: true, topLevelMessageID: messageID, why: fmt.Sprintf("payload=vendor — %s", class.why)}, nil

	default:
		return RecordOwnership{}, fmt.Errorf("frontend: ownership refused seq=%d payload=%s: category %s has no ownership rule, so writing this record would require inventing one", ev.GetSeq(), field.Name(), category)
	}
}
