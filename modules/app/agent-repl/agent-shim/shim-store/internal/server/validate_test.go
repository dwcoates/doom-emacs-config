package server

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

func siteOf(ref *refusal) string {
	if ref == nil {
		return ""
	}
	return ref.site
}

func TestValidateAgentIDRefusesAnIdWithNoValue(t *testing.T) {
	// Arrange. Both spellings of absence: no message, and a message with "".
	tests := []struct {
		name string
		id   *conversationv1.AgentId
	}{
		{name: "no message", id: nil},
		{name: "empty value", id: &conversationv1.AgentId{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			ref := validateAgentID(tc.id, "agent")

			// Assert.
			if siteOf(ref) != SiteAgentIDEmpty {
				t.Fatalf("site = %q, want %q", siteOf(ref), SiteAgentIDEmpty)
			}
		})
	}
}

func TestValidateAgentIDAcceptsAValue(t *testing.T) {
	// Arrange.
	id := &conversationv1.AgentId{Value: "a1"}

	// Act.
	ref := validateAgentID(id, "agent")

	// Assert.
	if ref != nil {
		t.Fatalf("refusal = %v, want nil", ref)
	}
}

func TestValidatePageSizeRefusesZero(t *testing.T) {
	// Arrange. Zero is an unset required field, never "the server picks".

	// Act.
	ref := validatePageSize(0)

	// Assert.
	if siteOf(ref) != SitePageSizeZero {
		t.Fatalf("site = %q, want %q", siteOf(ref), SitePageSizeZero)
	}
}

func TestValidateStoreItemPointerRefusesAPointerWithNoValue(t *testing.T) {
	// Arrange.
	tests := []struct {
		name    string
		pointer *storev1.StoreItemPointer
	}{
		{name: "no message", pointer: nil},
		{name: "empty value", pointer: &storev1.StoreItemPointer{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			ref := validateStoreItemPointer(tc.pointer, "after")

			// Assert.
			if siteOf(ref) != SitePointerEmpty {
				t.Fatalf("site = %q, want %q", siteOf(ref), SitePointerEmpty)
			}
		})
	}
}

func TestValidateAgentSessionTokenRefusesATokenWithNoValue(t *testing.T) {
	// Arrange.

	// Act.
	ref := validateAgentSessionToken(&storev1.AgentSessionToken{})

	// Assert.
	if siteOf(ref) != SiteTokenEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteTokenEmpty)
	}
}

func TestValidatePlaneRefusesAnUnsetOneof(t *testing.T) {
	// Arrange. An unset oneof is an error; the store never guesses a producer.

	// Act.
	ref := validatePlane(&storev1.Plane{}, "entries[0]")

	// Assert.
	if siteOf(ref) != SiteEntryPlaneUnset {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteEntryPlaneUnset)
	}
}

func TestValidateCursorStateRefusesAnEmptyFileID(t *testing.T) {
	// Arrange. file_id is the row key; "" keys nothing.

	// Act.
	ref := validateCursorState(&storev1.CursorState{Path: "/t.jsonl"}, "cursor_advance")

	// Assert.
	if siteOf(ref) != SiteCursorFileIDEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteCursorFileIDEmpty)
	}
}

func TestValidateCursorStateAcceptsAZeroOffset(t *testing.T) {
	// Arrange. Offset zero is a legitimate start-of-file position, not absence.

	// Act.
	ref := validateCursorState(&storev1.CursorState{FileId: "1:2", Path: "/t.jsonl"}, "cursor_advance")

	// Assert.
	if ref != nil {
		t.Fatalf("refusal = %v, want nil", ref)
	}
}

func TestValidateStoreEntryRefusesAnEmptyWriteID(t *testing.T) {
	// Arrange.
	entry := validEntry("", "u1")

	// Act.
	ref := validateStoreEntry(entry, 0)

	// Assert.
	if siteOf(ref) != SiteEntryWriteIDEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteEntryWriteIDEmpty)
	}
}

func TestValidateStoreEntryRefusesAnEmptyUpsertKey(t *testing.T) {
	// Arrange.
	entry := validEntry("w1", "")

	// Act.
	ref := validateStoreEntry(entry, 0)

	// Assert.
	if siteOf(ref) != SiteEntryUpsertKeyEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteEntryUpsertKeyEmpty)
	}
}

func TestValidateStoreEntryRefusesAnUnsetEntryArm(t *testing.T) {
	// Arrange.
	entry := validEntry("w1", "u1")
	entry.Entry = nil

	// Act.
	ref := validateStoreEntry(entry, 0)

	// Assert.
	if siteOf(ref) != SiteEntryArmUnset {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteEntryArmUnset)
	}
}

func TestValidateStoreEntryNamesTheOffendingIndex(t *testing.T) {
	// Arrange. A batch failure detail must say WHICH entry was wrong.
	entry := validEntry("w7", "")

	// Act.
	ref := validateStoreEntry(entry, 3)

	// Assert.
	if ref == nil || !strings.Contains(ref.detail, "entries[3]") || !strings.Contains(ref.detail, "w7") {
		t.Fatalf("detail = %q, want it to name entries[3] and write_id w7", ref)
	}
}

func TestValidateEntryBatchRefusesAnEmptyBatch(t *testing.T) {
	// Arrange. A producer with nothing to write does not call.

	// Act.
	ref := validateEntryBatch(&storev1.EntryBatch{})

	// Assert.
	if siteOf(ref) != SiteBatchEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteBatchEmpty)
	}
}

func TestValidateEntryBatchRefusesAMissingBatch(t *testing.T) {
	// Arrange.

	// Act.
	ref := validateEntryBatch(nil)

	// Assert.
	if siteOf(ref) != SiteBatchMissing {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteBatchMissing)
	}
}

func TestValidateEntryBatchValidatesTheCursorAdvance(t *testing.T) {
	// Arrange. The cursor rides the batch, so it is validated with it.
	batch := &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{validEntry("w1", "u1")},
		CursorAdvance: &storev1.CursorState{Path: "/t.jsonl"},
	}

	// Act.
	ref := validateEntryBatch(batch)

	// Assert.
	if siteOf(ref) != SiteCursorFileIDEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteCursorFileIDEmpty)
	}
}

func TestValidateWriteBatchRequestRefusesAnEmptyProducer(t *testing.T) {
	// Arrange.
	req := &storev1.WriteBatchRequest{Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}}}

	// Act.
	ref := validateWriteBatchRequest(req)

	// Assert.
	if siteOf(ref) != SiteProducerEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteProducerEmpty)
	}
}

func TestValidateOpenAgentSessionRequestRefusesAPresentButEmptyKnownThrough(t *testing.T) {
	// Arrange. Absence is omission, never an empty value.
	req := &storev1.OpenAgentSessionRequest{
		Agent: &conversationv1.AgentId{Value: "a1"}, PageSize: 10,
		KnownThrough: &storev1.StoreItemPointer{},
	}

	// Act.
	ref := validateOpenAgentSessionRequest(req)

	// Assert.
	if siteOf(ref) != SitePointerEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SitePointerEmpty)
	}
}

func TestValidateOpenAgentSessionRequestAcceptsAnOmittedKnownThrough(t *testing.T) {
	// Arrange. UNSET is the full-repaint request.
	req := &storev1.OpenAgentSessionRequest{Agent: &conversationv1.AgentId{Value: "a1"}, PageSize: 10}

	// Act.
	ref := validateOpenAgentSessionRequest(req)

	// Assert.
	if ref != nil {
		t.Fatalf("refusal = %v, want nil", ref)
	}
}

func TestValidateReadAgentPageRequestRequiresTheAfterPointer(t *testing.T) {
	// Arrange. There is no first-page arm on this verb.
	req := &storev1.ReadAgentPageRequest{Book: &conversationv1.AgentId{Value: "a1"}, PageSize: 10}

	// Act.
	ref := validateReadAgentPageRequest(req)

	// Assert.
	if siteOf(ref) != SitePointerEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SitePointerEmpty)
	}
}

func TestValidateGetSidecarCursorsRequestRefusesAPresentButEmptyFileID(t *testing.T) {
	// Arrange.
	empty := ""
	req := &storev1.GetSidecarCursorsRequest{FileId: &empty}

	// Act.
	ref := validateGetSidecarCursorsRequest(req)

	// Assert.
	if siteOf(ref) != SiteFileIDEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteFileIDEmpty)
	}
}

func TestValidateGetSidecarCursorsRequestAcceptsAnOmittedFileID(t *testing.T) {
	// Arrange. Omitted asks for every cursor.

	// Act.
	ref := validateGetSidecarCursorsRequest(&storev1.GetSidecarCursorsRequest{})

	// Assert.
	if ref != nil {
		t.Fatalf("refusal = %v, want nil", ref)
	}
}
