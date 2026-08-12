// durability.go is the CONSTRUCTION of a message's durability class, and the
// only place in the daemon where `Message.durability` is set.
//
// WHY A CONSTRUCTOR AND NOT A VALIDATOR. The frozen contract states the
// ephemeral lineage rules as structural, "enforced at the ephemeral
// constructor, refusing violations, rather than checked afterwards"
// (FROZEN-slash-command-durability, Part 4 rule 4). A validator run after the
// fact can only report a message that already exists, and the report arrives
// on a path — an outbound audit, a test — that the offending producer does not
// read. Here the invalid message never comes into being: an ephemeral message
// is built BY the constructor, which writes its lineage itself, so there is no
// parameter through which a caller could name a parent at all.
//
// THE THREE RULES, AND WHERE EACH IS CARRIED
//
//  1. "An ephemeral message is ALWAYS a feed row." Carried by the SHAPE:
//     NewEphemeralFeedRow writes FeedRowLineage over whatever it was handed and
//     refuses a body that arrived already claiming a parent, so the only
//     lineage an ephemeral message can leave here with is self-referential.
//
//  2. "An ephemeral message is NEVER a parent." Carried by NewDurableChild,
//     which reads the parent's OWN durability arm and refuses an ephemeral one.
//     A durable child of an ephemeral parent names a top_level_message_id no
//     store row will ever hold, so the child is unreachable by every page query
//     — data the store has, that no reader can ask for.
//
//  3. "An ephemeral message NEVER names a durable parent either." Falls out of
//     rule 1: naming a durable parent is naming A parent, and rule 1 admits
//     none. The refusal is stated separately anyway, because the message it
//     carries is what tells a maintainer WHICH mistake was made.
//
// WHERE GO CANNOT CARRY IT, THE CHECK IS LOUD. A oneof arm is a nilable field
// on a generated struct, so "the parent has a durability class at all" cannot
// be a compile-time fact. Every such case returns an error naming the message
// and the half that was wrong. None of them repairs the message, defaults it to
// durable, or drops it quietly: an unclassified message assumed durable is a
// message the store will later be blamed for losing.
package frontend

import (
	"fmt"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// NewEphemeralFeedRow classifies one message EPHEMERAL and, in the same act,
// makes it a feed row.
//
// The caller supplies identity, timestamp and payload; the LINEAGE is not the
// caller's to supply, which is how rules 1 and 3 stop being rules anyone has to
// remember. A body handed in with a lineage that is not already this message's
// own feed row is REFUSED rather than overwritten: silently correcting it would
// hide a producer that believed it was attaching the card somewhere, and that
// belief is the defect worth hearing about.
//
// It returns a NEW message rather than mutating in place, so a caller cannot
// end up holding a half-classified copy of the same pointer.
func NewEphemeralFeedRow(body *frontendv1.Message) (*frontendv1.Message, error) {
	if body == nil {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused: no message body was supplied, so there is nothing to classify and no identity to name in this error")
	}
	uuid := body.GetUuid()
	if uuid == "" {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused: the body carries no uuid, and an ephemeral message's top_level_message_id IS its own uuid, so a blank id would produce a feed row that names nothing")
	}
	if body.GetPayload() == nil {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused uuid=%s: the body carries no payload arm, so it would render as an empty card the user cannot act on", uuid)
	}
	if body.GetDurable() != nil {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused uuid=%s: the body is ALREADY classified durable, and a message cannot both have a store record and have none", uuid)
	}
	if parent := body.GetLineage().GetParentMessageId(); parent != "" {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused uuid=%s parent_message_id=%s: an ephemeral message is ALWAYS a feed row and NEVER names a parent — attaching one into a paged conversation would put a card in a thread it is guaranteed to vanish from on the next reload", uuid, parent)
	}
	if root := body.GetLineage().GetTopLevelMessageId(); root != "" && root != uuid {
		return nil, fmt.Errorf("frontend: ephemeral message construction refused uuid=%s top_level_message_id=%s: an ephemeral message's root is its OWN uuid, and naming another message's root claims membership in a feed row that will outlive this card", uuid, root)
	}

	out := cloneMessageShallow(body)
	out.Lineage = FeedRowLineage(uuid)
	out.Durability = &frontendv1.Message_Ephemeral{Ephemeral: &frontendv1.MessageEphemeral{}}
	return out, nil
}

// NewDurableFeedRow classifies one message DURABLE and seats it directly in the
// feed.
//
// It exists so that "neither arm set" cannot survive: every construction site
// has a durable path to reach, and a site that reached none is a site that
// omitted the class entirely rather than one that had no way to state it.
func NewDurableFeedRow(body *frontendv1.Message) (*frontendv1.Message, error) {
	if body == nil {
		return nil, fmt.Errorf("frontend: durable message construction refused: no message body was supplied, so there is nothing to classify and no identity to name in this error")
	}
	uuid := body.GetUuid()
	if uuid == "" {
		return nil, fmt.Errorf("frontend: durable message construction refused: the body carries no uuid, so nothing could ever correlate it with the store record that makes it durable")
	}
	if body.GetPayload() == nil {
		return nil, fmt.Errorf("frontend: durable message construction refused uuid=%s: the body carries no payload arm, so it would render as an empty card the user cannot act on", uuid)
	}
	if body.GetEphemeral() != nil {
		return nil, fmt.Errorf("frontend: durable message construction refused uuid=%s: the body is ALREADY classified ephemeral, and a message cannot both have a store record and have none", uuid)
	}
	if parent := body.GetLineage().GetParentMessageId(); parent != "" {
		return nil, fmt.Errorf("frontend: durable feed-row construction refused uuid=%s parent_message_id=%s: a feed row names no parent — a contained message is built with NewDurableChild, which is where the parent's own durability is checked", uuid, parent)
	}

	out := cloneMessageShallow(body)
	out.Lineage = FeedRowLineage(uuid)
	out.Durability = &frontendv1.Message_Durable{Durable: &frontendv1.MessageDurable{}}
	return out, nil
}

// ClassifyRecordDerived states the durability class of every message curated
// from a RECORD — a transcript line the CLI wrote, or an event the store holds
// — and is the single chokepoint the curation path passes through.
//
// WHY EVERY ONE OF THEM IS DURABLE. The contract's test for the class is
// whether a record exists, and nothing else: "A message is EPHEMERAL when no
// durable record exists for it, and never because the daemon minted its id."
// Everything reaching here EXISTS BECAUSE a record did — a prompt, an agent
// response, a reasoning block, a turn result, a clear, a compaction, a failure
// card read off a system line. The store serves each of them again on the next
// reload, which is exactly what the durable arm claims.
//
// WHY IT IS A CHOKEPOINT AND NOT A PARAMETER ON EACH CURATOR. The curators
// build a dozen shapes and every one of them shares one answer, so asking each
// to state it separately is a dozen chances to forget — and a message that
// reaches a frontend with no arm at all is malformed by the contract: a reader
// cannot tell "no record exists" from "the record was not found", which is the
// confusion the class exists to end.
//
// AN ALREADY-CLASSIFIED MESSAGE PASSES THROUGH UNTOUCHED. The one curated shape
// that is NOT durable — the harness's `system`/`local_command` record, which
// the contract puts in the ephemeral class — is classified at its own producer,
// which is the only place that knows the shape. Re-deciding it here would be a
// second authority over one fact, and the two would be free to disagree.
//
// A REFUSAL FAILS THE WHOLE DELTA rather than dropping the offending message.
// The refusal is a daemon defect, and a delta silently short one message is a
// conversation with a hole in it that nothing downstream can attribute; the
// error travels the curation path's existing error channel to the caller that
// can report it.
func ClassifyRecordDerived(items []*frontendv1.Message) ([]*frontendv1.Message, error) {
	out := make([]*frontendv1.Message, 0, len(items))
	for _, it := range items {
		if it.GetDurability() != nil {
			out = append(out, it)
			continue
		}
		classified, err := NewDurableFeedRow(it)
		if err != nil {
			return nil, fmt.Errorf("frontend: a record-derived message could not be classified, so the whole delta is refused rather than served one message short: %w", err)
		}
		out = append(out, classified)
	}
	return out, nil
}

// NewDurableChild classifies one message DURABLE and seats it INSIDE parent,
// which is the single place rule 2 is enforced.
//
// The parent is passed as the message itself rather than as an id, deliberately:
// an id cannot be asked what its durability class is, so a signature taking one
// would make the rule uncheckable at the only moment it can be checked. Reading
// the parent also lets the child inherit the parent's ROOT rather than
// recomputing it, so the denormalized top_level_message_id cannot drift between
// a parent and its children.
func NewDurableChild(parent, body *frontendv1.Message) (*frontendv1.Message, error) {
	if parent == nil {
		return nil, fmt.Errorf("frontend: durable child construction refused: no parent message was supplied, and a child whose parent could not be resolved is a producer fault, never an empty pointer")
	}
	if body == nil {
		return nil, fmt.Errorf("frontend: durable child construction refused parent=%s: no message body was supplied", parent.GetUuid())
	}
	uuid := body.GetUuid()
	if uuid == "" {
		return nil, fmt.Errorf("frontend: durable child construction refused parent=%s: the body carries no uuid", parent.GetUuid())
	}
	if body.GetPayload() == nil {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s parent=%s: the body carries no payload arm", uuid, parent.GetUuid())
	}
	if body.GetEphemeral() != nil {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s parent=%s: the body is ALREADY classified ephemeral, and an ephemeral message is always a feed row and never a child", uuid, parent.GetUuid())
	}
	if parent.GetUuid() == "" {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s: the parent carries no uuid, so the child's parent_message_id would point at nothing", uuid)
	}
	if parent.GetEphemeral() != nil {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s parent=%s: the parent is EPHEMERAL, so no store record for it will ever exist and this child's top_level_message_id would name a feed row no page query can return — the child would be stored and permanently unreachable", uuid, parent.GetUuid())
	}
	if parent.GetDurable() == nil {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s parent=%s: the parent states NO durability class at all, and an unclassified parent is not assumed durable — that assumption is exactly how an unreachable child gets written", uuid, parent.GetUuid())
	}
	root := parent.GetLineage().GetTopLevelMessageId()
	if root == "" {
		return nil, fmt.Errorf("frontend: durable child construction refused uuid=%s parent=%s: the parent's top_level_message_id is EMPTY, so the child has no root to inherit and would be invisible to the page query that selects by it", uuid, parent.GetUuid())
	}

	out := cloneMessageShallow(body)
	out.Lineage = &frontendv1.MessageLineage{TopLevelMessageId: root, ParentMessageId: parent.GetUuid()}
	out.Durability = &frontendv1.Message_Durable{Durable: &frontendv1.MessageDurable{}}
	return out, nil
}

// cloneMessageShallow copies the message so a constructor can write lineage and
// durability without mutating the caller's body.
//
// It goes through proto.Clone rather than copying the fields it knows about: a
// hand-written copy silently DROPS any field added to Message later, and the
// dropped field would be missing only on the classified path, which is the
// hardest kind of loss to attribute.
func cloneMessageShallow(body *frontendv1.Message) *frontendv1.Message {
	return proto.Clone(body).(*frontendv1.Message)
}
