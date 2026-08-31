// identity_test.go — SUBJECT: the three refusals that protect what a stored row
// IS.
//
// An upsert supersedes a row's CONTENT. `upsert_key` names one thing, and the
// page model rests on that: a pointer stays valid across every write of the row
// it names, so a caller holding one must still hold a line of the book it read
// it from. A write that moves the row to another book, or turns a served page
// line into unservable residue, is a different thing wearing the same key.
// Likewise a page line whose envelope and frame disagree about whose line it is,
// and residue with nothing verbatim in it — which is the drop residue exists to
// prevent, dressed up as durability.
package integration

import (
	"testing"

	storev1 "agentrepl/proto/store/v1"
)

func TestAnUpsertMovingARowToAnotherBookIsRefused(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())
	shim.write(ctx, t, shim.agentEntry("w-id-1", "u-id",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Act
	failure := shim.writeExpectingFailure(ctx, t, nil, shim.agentEntry("w-id-2", "u-id",
		frameLine(agentID("other"), responseFrame("other", "act-1", "moved"))))

	// Assert
	assertWriteInvalidRequest(t, failure, "entries[0].agent_update.serveable_frame.page_agent_id")
}

func TestARefusedIdentityChangeLeavesTheOriginalLineServed(t *testing.T) {
	// Arrange
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	cli := store.client()
	shim := streamProducer(cli)
	shim.write(ctx, t, shim.agentEntry("w-id-1", "u-id",
		frameLine(agentID("main"), responseFrame("main", "act-1", "L1"))))

	// Act
	shim.writeExpectingFailure(ctx, t, nil, shim.agentEntry("w-id-2", "u-id",
		frameLine(agentID("other"), responseFrame("other", "act-1", "moved"))))

	// Assert
	assertTexts(t, "the original book", pageTexts(openSession(ctx, t, cli, "main", 10, nil).GetPage()), []string{"L1"})
	assertTexts(t, "the book it tried to move to", pageTexts(openSession(ctx, t, cli, "other", 10, nil).GetPage()), nil)
}

func TestAPageLineWhoseEnvelopeAndFrameDisagreeIsRefused(t *testing.T) {
	// Arrange: the producer decides PAGEABILITY, not attribution, and the store
	// cannot pick a winner between two statements about one line.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())

	// Act
	failure := shim.writeExpectingFailure(ctx, t, nil, shim.agentEntry("w-mismatch", "u-mismatch",
		&storev1.StoreAgentUpdate{
			AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
				ServeableFrame: &storev1.StorePageLine{
					PageAgentId: agentID("main"),
					AgentItem:   frameItem(responseFrame("someone-else", "act-1", "x")),
				},
			},
		}))

	// Assert
	assertWriteInvalidRequest(t, failure, "entries[0].agent_update.serveable_frame.page_agent_id")
}

func TestResidueWithNoVerbatimRecordIsRefused(t *testing.T) {
	// Arrange
	tests := []struct {
		name      string
		item      *storev1.StoreUnservedItem
		wantField string
	}{
		{
			name: "vendor specific",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
				VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook"},
			}},
			wantField: "entries[0].agent_update.unserved_item.vendor_specific.raw",
		},
		{
			name: "unknown",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unknown{
				Unknown: &storev1.StoreUnknown{Discriminator: "widget"},
			}},
			wantField: "entries[0].agent_update.unserved_item.unknown.raw",
		},
		{
			name: "unparsed",
			item: &storev1.StoreUnservedItem{UnservedItem: &storev1.StoreUnservedItem_Unparsed{
				Unparsed: &storev1.StoreUnparsed{Source: "t.jsonl", ParseError: "unexpected EOF"},
			}},
			wantField: "entries[0].agent_update.unserved_item.unparsed.raw",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			store := startStore(t, storeOptions{})
			ctx, cancel := callContext(t)
			defer cancel()
			shim := streamProducer(store.client())

			// Act
			failure := shim.writeExpectingFailure(ctx, t, nil, shim.agentEntry("w-raw", "u-raw",
				&storev1.StoreAgentUpdate{
					AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{UnservedItem: tc.item},
				}))

			// Assert
			assertWriteInvalidRequest(t, failure, tc.wantField)
		})
	}
}

func TestResidueCarryingItsVerbatimRecordIsAccepted(t *testing.T) {
	// Arrange: the refusal must not have made residue unusable — the whole
	// point is that nothing unconvertible is DROPPED.
	store := startStore(t, storeOptions{})
	ctx, cancel := callContext(t)
	defer cancel()
	shim := streamProducer(store.client())

	// Act
	shim.write(ctx, t, shim.agentEntry("w-raw-ok", "u-raw-ok", vendorSpecificLine("hook")))

	// Assert
	store.assertNoErrorRecords()
}
