package server

import (
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/db"
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

func TestValidateEntryBatchAcceptsACursorOnlyBatch(t *testing.T) {
	// Arrange. A sidecar that read bytes yielding no entries must still make
	// its file position durable, or it re-reads the same bytes forever.

	// Act.
	ref := validateEntryBatch(&storev1.EntryBatch{
		CursorAdvance: &storev1.CursorState{FileId: "16777232:424242", Path: "/t/a.jsonl", Offset: 100},
	}, false)

	// Assert.
	if ref != nil {
		t.Fatalf("validateEntryBatch = %q, want a cursor-only batch accepted", ref.detail)
	}
}

func TestValidateEntryBatchRefusesAnEmptyBatch(t *testing.T) {
	// Arrange. A batch carrying neither entries nor a cursor advance states
	// nothing at all.

	// Act.
	ref := validateEntryBatch(&storev1.EntryBatch{}, false)

	// Assert.
	if siteOf(ref) != SiteBatchEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteBatchEmpty)
	}
}

func TestValidateEntryBatchRefusesAMissingBatch(t *testing.T) {
	// Arrange.

	// Act.
	ref := validateEntryBatch(nil, false)

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
	ref := validateEntryBatch(batch, false)

	// Assert.
	if siteOf(ref) != SiteCursorFileIDEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteCursorFileIDEmpty)
	}
}

func TestValidateWriteBatchRequestRefusesAnEmptyProducer(t *testing.T) {
	// Arrange.
	req := &storev1.WriteBatchRequest{WriteClass: interactiveClass(), Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}}}

	// Act.
	ref := validateWriteBatchRequest(req)

	// Assert.
	if siteOf(ref) != SiteProducerEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteProducerEmpty)
	}
}

// TestValidateWriteBatchRequestRefusesAnUnstatedWriteClass: the class decides
// which queue a write takes, and the store refuses to guess it.
func TestValidateWriteBatchRequestRefusesAnUnstatedWriteClass(t *testing.T) {
	tests := []struct {
		name  string
		class *storev1.WriteClass
	}{
		{name: "no class message", class: nil},
		{name: "a class message with no arm", class: &storev1.WriteClass{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			req := &storev1.WriteBatchRequest{Producer: "claude-shim:s1", WriteClass: tc.class,
				Batch: &storev1.EntryBatch{Entries: []*storev1.StoreEntry{validEntry("w1", "u1")}}}

			// Act.
			ref := validateWriteBatchRequest(req)

			// Assert.
			if siteOf(ref) != SiteWriteClassUnset {
				t.Fatalf("site = %q, want %q", siteOf(ref), SiteWriteClassUnset)
			}
			if ref.field != "write_class" {
				t.Fatalf("field = %q, want %q", ref.field, "write_class")
			}
		})
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

func TestValidateGetLiveWorkRequestRefusesAnUnsetSession(t *testing.T) {
	// Arrange.
	req := &storev1.GetLiveWorkRequest{}

	// Act.
	ref := validateGetLiveWorkRequest(req)

	// Assert.
	if siteOf(ref) != SiteSessionEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteSessionEmpty)
	}
}

func TestValidateGetLiveWorkRequestRefusesAnEmptySessionValue(t *testing.T) {
	// Arrange.
	req := &storev1.GetLiveWorkRequest{Session: &conversationv1.AgentId{}}

	// Act.
	ref := validateGetLiveWorkRequest(req)

	// Assert.
	if siteOf(ref) != SiteSessionEmpty {
		t.Fatalf("site = %q, want %q", siteOf(ref), SiteSessionEmpty)
	}
}

func TestValidateGetLiveWorkRequestAcceptsANamedSession(t *testing.T) {
	// Arrange.
	req := &storev1.GetLiveWorkRequest{Session: &conversationv1.AgentId{Value: "main-1"}}

	// Act.
	ref := validateGetLiveWorkRequest(req)

	// Assert.
	if ref != nil {
		t.Fatalf("refusal = %v, want none", ref)
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

// ---- the failure arms ----

func TestWriteBatchFailureAnswersInvalidRequestWithItsField(t *testing.T) {
	// Arrange. A caller that received no arm would have to parse `detail` to
	// learn whether retrying the same bytes could ever help.
	ref := refuse(SiteEntryUpsertKeyEmpty, "entries[1].upsert_key", "upsert_key is empty")

	// Act.
	got := writeBatchFailure(ref).Msg.GetFailure()

	// Assert.
	if got.GetInvalidRequest() == nil {
		t.Fatalf("kind = %v, want invalid_request", got.GetKind())
	}
	if field := got.GetInvalidRequest().GetField(); field != "entries[1].upsert_key" {
		t.Fatalf("field = %q, want entries[1].upsert_key", field)
	}
}

func TestWriteBatchFailureAnswersStorageFailureForADatabaseError(t *testing.T) {
	// Arrange.
	ref := refuseClass(classStorage, SiteDatabaseFailure, "", "disk is on fire")

	// Act.
	got := writeBatchFailure(ref).Msg.GetFailure()

	// Assert.
	if got.GetStorageFailure() == nil {
		t.Fatalf("kind = %v, want storage_failure", got.GetKind())
	}
}

func TestOpenAgentSessionFailureAnswersEachClassItsOwnArm(t *testing.T) {
	// Arrange. Three classes, three arms: the caller's recovery differs for
	// each — fix the request, repaint, or retry.
	tests := []struct {
		name string
		ref  *refusal
		want string
	}{
		{name: "invalid request", ref: refuse(SiteAgentIDEmpty, "agent", "empty"), want: "invalid_request"},
		{name: "stale pointer", ref: refuseClass(classStalePointer, SiteStalePointer, "known_through", "gone"), want: "stale_pointer"},
		{name: "storage failure", ref: refuseClass(classStorage, SiteDatabaseFailure, "", "disk"), want: "storage_failure"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := openFailure(tc.ref).Msg.GetFailure()

			// Assert.
			var arm string
			switch {
			case got.GetInvalidRequest() != nil:
				arm = "invalid_request"
			case got.GetStalePointer() != nil:
				arm = "stale_pointer"
			case got.GetStorageFailure() != nil:
				arm = "storage_failure"
			}
			if arm != tc.want {
				t.Fatalf("arm = %q, want %q", arm, tc.want)
			}
		})
	}
}

func TestReadAgentPageFailureAnswersEachClassItsOwnArm(t *testing.T) {
	// Arrange.
	tests := []struct {
		name string
		ref  *refusal
		want string
	}{
		{name: "invalid request", ref: refuse(SitePointerEmpty, "after", "empty"), want: "invalid_request"},
		{name: "stale pointer", ref: refuseClass(classStalePointer, SiteStalePointer, "after", "gone"), want: "stale_pointer"},
		{name: "storage failure", ref: refuseClass(classStorage, SiteDatabaseFailure, "", "disk"), want: "storage_failure"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := readPageFailure(tc.ref).Msg.GetFailure()

			// Assert.
			var arm string
			switch {
			case got.GetInvalidRequest() != nil:
				arm = "invalid_request"
			case got.GetStalePointer() != nil:
				arm = "stale_pointer"
			case got.GetStorageFailure() != nil:
				arm = "storage_failure"
			}
			if arm != tc.want {
				t.Fatalf("arm = %q, want %q", arm, tc.want)
			}
		})
	}
}

func TestGetLiveWorkFailureHasOnlyTheStorageArm(t *testing.T) {
	// Arrange. The verb takes no request fields, so there is nothing a caller
	// can have sent wrong.
	ref := refuseClass(classStorage, SiteDatabaseFailure, "", "disk")

	// Act.
	got := liveWorkFailure(ref).Msg.GetFailure()

	// Assert.
	if got.GetStorageFailure() == nil {
		t.Fatalf("kind = %v, want storage_failure", got.GetKind())
	}
}

func TestGetSidecarCursorsFailureAnswersInvalidRequestWithItsField(t *testing.T) {
	// Arrange.
	ref := refuse(SiteFileIDEmpty, "file_id", "present but empty")

	// Act.
	got := cursorsFailure(ref).Msg.GetFailure()

	// Assert.
	if field := got.GetInvalidRequest().GetField(); field != "file_id" {
		t.Fatalf("field = %q, want file_id", field)
	}
}

func TestStoreRefusalKeepsTheSiteTheStorageLayerNamed(t *testing.T) {
	// Arrange. This layer never opens a frame, so it cannot know that an upsert
	// changed a row's identity — only the storage layer can name that site.
	err := db.ErrInvalid

	// Act.
	got := storeRefusal(err)

	// Assert: an unclassified ErrInvalid still falls back to the generic site.
	if got.site != SiteStoreRefusedRequest {
		t.Fatalf("site = %q, want %q", got.site, SiteStoreRefusedRequest)
	}
	if got.class != classInvalid {
		t.Fatalf("class = %v, want classInvalid", got.class)
	}
}

func TestStoreRefusalClassifiesAStalePointer(t *testing.T) {
	// Arrange.
	err := db.ErrStalePointer

	// Act.
	got := storeRefusal(err)

	// Assert.
	if got.class != classStalePointer {
		t.Fatalf("class = %v, want classStalePointer", got.class)
	}
}

func TestStoreRefusalClassifiesAnythingElseAsStorage(t *testing.T) {
	// Arrange. A failure the storage layer did not classify is never softened
	// into a success and never guessed at.
	err := db.ErrStorage

	// Act.
	got := storeRefusal(err)

	// Assert.
	if got.class != classStorage {
		t.Fatalf("class = %v, want classStorage", got.class)
	}
}

// TestRefusalClassLogLevel is the severity table itself: which refusal classes
// claim that something is WRONG, and which one is a verb answering an existence
// question. Every class is enumerated so a class added later without a level
// fails here rather than being recorded at whatever the logger defaults to.
func TestRefusalClassLogLevel(t *testing.T) {
	// Arrange.
	tests := []struct {
		name  string
		class refusalClass
		want  string
	}{
		{name: "an illegal request is a warning", class: classInvalid, want: "warn"},
		{name: "a pointer that must be repainted is a warning", class: classStalePointer, want: "warn"},
		{name: "a database failure is a warning here and an error in db", class: classStorage, want: "warn"},
		{name: "a verb this wave cannot answer is a warning", class: classNotImplemented, want: "warn"},
		{name: "an agent id naming no book is the ordinary answer", class: classUnknownAgent, want: "info"},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act.
			got := test.class.logLevel()

			// Assert.
			if got != test.want {
				t.Fatalf("logLevel() = %q, want %q", got, test.want)
			}
		})
	}
}

// TestRefusalClassLogLevelPanicsForAClassItDoesNotKnow: a refusal recorded at
// the logger's default level is a severity nobody chose, so an unmapped class
// is a programming error the process reports rather than absorbs.
func TestRefusalClassLogLevelPanicsForAClassItDoesNotKnow(t *testing.T) {
	// Arrange.
	unmapped := refusalClass(len([]string{"invalid", "stale", "storage", "not_implemented", "unknown_agent"}) + 1)

	// Act & Assert.
	defer func() {
		if recovered := recover(); recovered == nil {
			t.Fatalf("logLevel() of an unmapped class returned instead of panicking")
		}
	}()
	_ = unmapped.logLevel()
}
