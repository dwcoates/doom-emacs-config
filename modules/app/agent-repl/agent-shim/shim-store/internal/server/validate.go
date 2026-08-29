package server

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
)

// The refusal sites. THESE ARE THE VOCABULARY the failure `kind` arms are
// derived from later, so each names one real place the store says no — never a
// grab-bag "invalid request".
//
// THE VALIDATION INVARIANT: an unset non-optional field is illegal, and
// absence is spelled with optional presence, never with "" or 0. Every site
// below is one field that must be there and is not, or one identity the store
// does not recognize.
const (
	SiteProducerEmpty          = "producer_empty"
	SiteBatchMissing           = "batch_missing"
	SiteBatchEmpty             = "batch_empty"
	SiteEntryPlaneUnset        = "entry_plane_unset"
	SiteEntryWriteIDEmpty      = "entry_write_id_empty"
	SiteEntryUpsertKeyEmpty    = "entry_upsert_key_empty"
	SiteEntryArmUnset          = "entry_arm_unset"
	SiteCursorFileIDEmpty      = "cursor_file_id_empty"
	SiteAgentIDEmpty           = "agent_id_empty"
	SitePageSizeZero           = "page_size_zero"
	SitePointerEmpty           = "pointer_empty"
	SiteTokenEmpty             = "token_empty"
	SiteUnknownWatchToken      = "unknown_watch_token"
	SiteFileIDEmpty            = "file_id_empty"
	SiteStoreRefusedRequest    = "store_refused_request"
	SiteStalePointer           = "stale_pointer"
	SiteDatabaseFailure        = "database_failure"
	SiteWorkflowNotImplemented = "workflow_not_implemented"
	SiteStreamNotFlushable     = "stream_not_flushable"
)

// refusal is one typed refusal: the site the server logs, and the human detail
// the failure arm carries. `detail` is for humans and logs and is NEVER
// switched on by a caller.
type refusal struct {
	site   string
	detail string
}

func (r *refusal) Error() string { return r.detail }

func refuse(site, format string, args ...any) *refusal {
	return &refusal{site: site, detail: fmt.Sprintf(format, args...)}
}

// ---- base functions: one per message, its validation lives here once ----

// validateAgentID is the base validation of conversation.v1.AgentId: the
// message must be present and its opaque value non-empty. `what` names the
// field for the human detail, because a request may carry more than one.
func validateAgentID(id *conversationv1.AgentId, what string) *refusal {
	if id == nil || id.GetValue() == "" {
		return refuse(SiteAgentIDEmpty, "%s: an AgentId with no value is not an agent", what)
	}
	return nil
}

// validateStoreItemPointer is the base validation of a pointer: present with a
// non-empty opaque value. Whether the position it names still exists in the
// book is the storage layer's answer (SiteStalePointer), not this one.
func validateStoreItemPointer(p *storev1.StoreItemPointer, what string) *refusal {
	if p == nil || p.GetValue() == "" {
		return refuse(SitePointerEmpty, "%s: a StoreItemPointer with no value names no position", what)
	}
	return nil
}

// validateAgentSessionToken is the base validation of a watch token: present
// with a non-empty opaque value. Whether the token was ever minted, and
// whether it has already been consumed, is the registry's answer.
func validateAgentSessionToken(t *storev1.AgentSessionToken) *refusal {
	if t == nil || t.GetValue() == "" {
		return refuse(SiteTokenEmpty, "watch: an AgentSessionToken with no value addresses no session")
	}
	return nil
}

// validatePageSize is the base validation of a page budget. Zero is not "the
// server picks": it is an unset required field.
func validatePageSize(size uint32) *refusal {
	if size == 0 {
		return refuse(SitePageSizeZero, "page_size: a page budget of zero asks for nothing")
	}
	return nil
}

// validatePlane is the base validation of store.v1.Plane: the oneof arm must
// be set. An unset oneof is an error — the store never guesses a producer.
func validatePlane(p *storev1.Plane, what string) *refusal {
	if p == nil || p.GetPlane() == nil {
		return refuse(SiteEntryPlaneUnset, "%s: plane names neither stream nor file", what)
	}
	return nil
}

// validateCursorState is the base validation of store.v1.CursorState. The
// file's stable identity is the row key, so an empty one is unset; `path`,
// `offset` and `carry` are the cursor's payload and carry no presence rule
// (offset zero is a legitimate start-of-file position).
func validateCursorState(c *storev1.CursorState, what string) *refusal {
	if c.GetFileId() == "" {
		return refuse(SiteCursorFileIDEmpty, "%s: a CursorState with no file_id keys no row", what)
	}
	return nil
}

// validateStoreEntry is the base validation of one write's ENVELOPE — the only
// part of a StoreEntry this layer opens. The frame inside is opaque here; the
// storage layer routes it by arm.
func validateStoreEntry(e *storev1.StoreEntry, index int) *refusal {
	what := fmt.Sprintf("entries[%d]", index)
	if e == nil {
		return refuse(SiteEntryArmUnset, "%s: no entry", what)
	}
	if ref := validatePlane(e.GetPlane(), what); ref != nil {
		return ref
	}
	if e.GetWriteId() == "" {
		return refuse(SiteEntryWriteIDEmpty, "%s: write_id is empty, so the write cannot be deduped on replay", what)
	}
	if e.GetUpsertKey() == "" {
		return refuse(SiteEntryUpsertKeyEmpty, "%s (write_id %s): upsert_key is empty, so the write identifies no row", what, e.GetWriteId())
	}
	if e.GetEntry() == nil {
		return refuse(SiteEntryArmUnset, "%s (write_id %s): the entry oneof names neither agent_update nor session_update", what, e.GetWriteId())
	}
	return nil
}

// validateEntryBatch is the base validation of store.v1.EntryBatch: at least
// one entry, every entry's envelope well-formed, and a cursor advance (when
// the producer sent one) that names a file.
//
// AN EMPTY BATCH IS A REFUSAL, not a cheap success: a producer with nothing to
// write does not call, and a batch that lost its entries on the way is a defect
// the store must name rather than acknowledge as durable.
func validateEntryBatch(b *storev1.EntryBatch) *refusal {
	if b == nil {
		return refuse(SiteBatchMissing, "batch: the request carries no EntryBatch")
	}
	if len(b.GetEntries()) == 0 {
		return refuse(SiteBatchEmpty, "batch: the EntryBatch carries no entries")
	}
	for i, entry := range b.GetEntries() {
		if ref := validateStoreEntry(entry, i); ref != nil {
			return ref
		}
	}
	if b.GetCursorAdvance() != nil {
		if ref := validateCursorState(b.GetCursorAdvance(), "cursor_advance"); ref != nil {
			return ref
		}
	}
	return nil
}

// ---- use sites: one per request, each delegating to the child's base ----

func validateWriteBatchRequest(req *storev1.WriteBatchRequest) *refusal {
	if req.GetProducer() == "" {
		return refuse(SiteProducerEmpty, "producer: the write names no producer, so nothing can be attributed")
	}
	return validateEntryBatch(req.GetBatch())
}

func validateOpenAgentSessionRequest(req *storev1.OpenAgentSessionRequest) *refusal {
	if ref := validateAgentID(req.GetAgent(), "agent"); ref != nil {
		return ref
	}
	if ref := validatePageSize(req.GetPageSize()); ref != nil {
		return ref
	}
	// known_through is OPTIONAL: absent means repaint. Present but empty is a
	// sentinel pretending to be absence, which is the one thing it may not be.
	if req.KnownThrough != nil {
		if ref := validateStoreItemPointer(req.GetKnownThrough(), "known_through"); ref != nil {
			return ref
		}
	}
	return nil
}

func validateReadAgentPageRequest(req *storev1.ReadAgentPageRequest) *refusal {
	if ref := validateAgentID(req.GetBook(), "book"); ref != nil {
		return ref
	}
	if ref := validatePageSize(req.GetPageSize()); ref != nil {
		return ref
	}
	// `after` is REQUIRED: this verb only ever walks older, and the first page
	// is OpenAgentSession's answer.
	return validateStoreItemPointer(req.GetAfter(), "after")
}

func validateWatchAgentSessionRequest(req *storev1.WatchAgentSessionRequest) *refusal {
	return validateAgentSessionToken(req.GetWatch())
}

func validateGetSidecarCursorsRequest(req *storev1.GetSidecarCursorsRequest) *refusal {
	// file_id is OPTIONAL: absent asks for every cursor. Present but empty is
	// again a sentinel standing in for absence.
	if req.FileId != nil && req.GetFileId() == "" {
		return refuse(SiteFileIDEmpty, "file_id: present but empty; omit the field to ask for every cursor")
	}
	return nil
}
