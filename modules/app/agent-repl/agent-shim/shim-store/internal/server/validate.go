package server

import (
	"fmt"

	"agentrepl/shim-store/internal/db"

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
	SiteShapeHashEmpty         = "shape_hash_empty"
	SiteShapeKindEmpty         = "shape_kind_empty"
	SiteShapeStructureEmpty    = "shape_structure_empty"
	SiteShapeSeenMsUnset       = "shape_seen_ms_unset"
	SiteShapeUnset             = "shape_unset"
	SiteShapeKindFilterEmpty   = "shape_kind_filter_empty"
	SiteDatabaseFailure        = "database_failure"
	SiteWorkflowNotImplemented = "workflow_not_implemented"
	SiteStreamNotFlushable     = "stream_not_flushable"
	SiteRunEmpty               = "run_empty"
	SiteUnknownBashRun         = "unknown_bash_run"
	SiteWatchBufferOverflow    = "watch_buffer_overflow"
)

// The sites internal/db decides, ALIASED rather than restated.
//
// The storage layer owns every refusal about what is INSIDE a frame, because
// this layer opens only the envelope. Spelling the strings twice would let the
// site a record is logged under drift from the site the layer that raised it
// meant, so there is exactly one definition and this is a reference to it.
const (
	SiteStoreRefusedRequest   = db.SiteStoreRefusedRequest
	SiteStalePointer          = db.SiteStalePointer
	SiteUpsertChangesIdentity = db.SiteUpsertChangesIdentity
	SitePageBookMismatch      = db.SitePageBookMismatch
	SiteResidueRawUnset       = db.SiteResidueRawUnset
	SiteUnknownAgent          = db.SiteUnknownAgent
	SiteSessionEmpty          = db.SiteSessionEmpty
)

// refusalClass is WHICH FAILURE ARM a refusal becomes.
//
// It is not the same thing as the site. A site says which of the store's many
// checks said no — the vocabulary an operator counts by — while the class says
// which typed arm the caller receives, and several sites map to one arm. Keeping
// them apart is what lets a new site be added without inventing a wire arm for
// it.
type refusalClass int

const (
	// classInvalid is a request the caller must FIX; retrying the same bytes
	// cannot help.
	classInvalid refusalClass = iota
	// classStalePointer is a well-formed pointer naming no row of its book —
	// an ordinary race the caller recovers from by re-opening.
	classStalePointer
	// classStorage is the database failing; a retry may succeed.
	classStorage
	// classNotImplemented is a verb this wave does not answer.
	classNotImplemented
	// classUnknownAgent is a well-formed agent id naming no book of this store.
	// It is neither a request to fix nor a race to repaint: the target does not
	// exist, and the shim maps it to NotFound.
	classUnknownAgent
)

// armName is the WIRE ARM this class becomes, spelled as the proto spells it.
//
// It is what `refusal_kind` carries, so a log reader counts refusals by the
// answer the caller actually received rather than by re-deriving it from the
// site. A class with no arm would be a refusal the store could not answer, so
// the default is deliberately unreachable rather than an empty string quietly
// dropped from the record.
func (c refusalClass) armName() string {
	switch c {
	case classInvalid:
		return "invalid_request"
	case classStalePointer:
		return "stale_pointer"
	case classStorage:
		return "storage_failure"
	case classNotImplemented:
		return "not_implemented"
	case classUnknownAgent:
		return "unknown_agent"
	default:
		panic(fmt.Sprintf("shim-store server: refusal class %d has no wire arm", int(c)))
	}
}

// logLevel is the SEVERITY the one normal-level record of a refusal of this
// class is written at.
//
// THE LEVEL IS A PROPERTY OF THE CLASS, never of the call site and never of the
// message text. A refusal's severity is a claim about whether something is
// WRONG, and only the class knows: switching on a site would give one arm two
// severities depending on which check happened to fire, and switching on the
// detail string would make the log level depend on prose.
//
// EVERY CLASS IS LOUD EXCEPT classUnknownAgent, and that one is not an
// exemption granted to make a log quiet — it is the one class whose refusal is
// the verb's ORDINARY ANSWER rather than a report of a fault:
//
//   - classInvalid is a caller that sent something illegal, classStalePointer a
//     caller that must repaint, classStorage the database failing,
//     classNotImplemented a verb this wave cannot answer. Each is a warn (the
//     storage failure's own error record is written by internal/db) because
//     each says something is wrong somewhere.
//   - classUnknownAgent answers an EXISTENCE QUESTION with "no". OpenAgentSession
//     is the only verb that asks it, and both of the populations that reach it —
//     a consumer opening a page against an agent whose first row has not landed
//     yet, and a consumer holding a stale or mistyped target — send byte-identical
//     requests. The store cannot tell them apart, because the expectation lives
//     in the CALLER: only the caller knows whether it vouches for the agent. The
//     caller is also where the distinction is already acted on and recorded —
//     the shim serves an empty page and traces it when it vouches, and surfaces
//     the refusal as a typed error to the engine when it does not. A warn here
//     asserted "something is wrong" on every cold bring-up, which is a severity
//     the store is not in a position to claim.
//
// It is `info` and not verbose: the record is still written at the default
// threshold, carrying `refusal_site` and `refusal_kind` like every other, so an
// operator counting refusals loses nothing. Only the severity claim changes.
//
// A class with no level would be a refusal recorded at the logger's default
// rather than at a level anybody chose, so the default is deliberately
// unreachable rather than an empty string quietly filled in.
func (c refusalClass) logLevel() string {
	switch c {
	case classUnknownAgent:
		return "info"
	case classInvalid, classStalePointer, classStorage, classNotImplemented:
		return "warn"
	default:
		panic(fmt.Sprintf("shim-store server: refusal class %d has no log level", int(c)))
	}
}

// refusal is one typed refusal: the site the server logs, the store's own name
// for the field at fault, the class that selects the wire arm, and the human
// detail. `detail` is for humans and logs and is NEVER switched on by a caller;
// `field` is machine-readable and is what a producer's own logs quote.
type refusal struct {
	site   string
	field  string
	detail string
	class  refusalClass
}

func (r *refusal) Error() string { return r.detail }

// refuse builds a VALIDATION refusal. Every call names the field, because the
// failure arm carries it and an unnamed invalid_request tells the producer
// nothing it can act on.
func refuse(site, field, format string, args ...any) *refusal {
	return &refusal{site: site, field: field, detail: fmt.Sprintf(format, args...), class: classInvalid}
}

// refuseClass builds a refusal of a class other than validation.
func refuseClass(class refusalClass, site, field, detail string) *refusal {
	return &refusal{site: site, field: field, detail: detail, class: class}
}

// ---- base functions: one per message, its validation lives here once ----

// validateAgentID is the base validation of conversation.v1.AgentId: the
// message must be present and its opaque value non-empty. `what` names the
// field for the human detail, because a request may carry more than one.
func validateAgentID(id *conversationv1.AgentId, what string) *refusal {
	if id == nil || id.GetValue() == "" {
		return refuse(SiteAgentIDEmpty, what, "%s: an AgentId with no value is not an agent", what)
	}
	return nil
}

// validateStoreItemPointer is the base validation of a pointer: present with a
// non-empty opaque value. Whether the position it names still exists in the
// book is the storage layer's answer (SiteStalePointer), not this one.
func validateStoreItemPointer(p *storev1.StoreItemPointer, what string) *refusal {
	if p == nil || p.GetValue() == "" {
		return refuse(SitePointerEmpty, what, "%s: a StoreItemPointer with no value names no position", what)
	}
	return nil
}

// validateAgentSessionToken is the base validation of a watch token: present
// with a non-empty opaque value. Whether the token was ever minted, and
// whether it has already been consumed, is the registry's answer.
func validateAgentSessionToken(t *storev1.AgentSessionToken) *refusal {
	if t == nil || t.GetValue() == "" {
		return refuse(SiteTokenEmpty, "watch", "watch: an AgentSessionToken with no value addresses no session")
	}
	return nil
}

// validatePageSize is the base validation of a page budget. Zero is not "the
// server picks": it is an unset required field.
func validatePageSize(size uint32) *refusal {
	if size == 0 {
		return refuse(SitePageSizeZero, "page_size", "page_size: a page budget of zero asks for nothing")
	}
	return nil
}

// validatePlane is the base validation of store.v1.Plane: the oneof arm must
// be set. An unset oneof is an error — the store never guesses a producer.
func validatePlane(p *storev1.Plane, what string) *refusal {
	if p == nil || p.GetPlane() == nil {
		return refuse(SiteEntryPlaneUnset, what+".plane", "%s: plane names neither stream nor file", what)
	}
	return nil
}

// validateCursorState is the base validation of store.v1.CursorState. The
// file's stable identity is the row key, so an empty one is unset; `path`,
// `offset` and `carry` are the cursor's payload and carry no presence rule
// (offset zero is a legitimate start-of-file position).
func validateCursorState(c *storev1.CursorState, what string) *refusal {
	if c.GetFileId() == "" {
		return refuse(SiteCursorFileIDEmpty, what+".file_id", "%s: a CursorState with no file_id keys no row", what)
	}
	return nil
}

// validateStoreEntry is the base validation of one write's ENVELOPE — the only
// part of a StoreEntry this layer opens. The frame inside is opaque here; the
// storage layer routes it by arm.
func validateStoreEntry(e *storev1.StoreEntry, index int) *refusal {
	what := fmt.Sprintf("entries[%d]", index)
	if e == nil {
		return refuse(SiteEntryArmUnset, what, "%s: no entry", what)
	}
	if ref := validatePlane(e.GetPlane(), what); ref != nil {
		return ref
	}
	if e.GetWriteId() == "" {
		return refuse(SiteEntryWriteIDEmpty, what+".write_id", "%s: write_id is empty, so the write cannot be deduped on replay", what)
	}
	if e.GetUpsertKey() == "" {
		return refuse(SiteEntryUpsertKeyEmpty, what+".upsert_key", "%s (write_id %s): upsert_key is empty, so the write identifies no row", what, e.GetWriteId())
	}
	if e.GetEntry() == nil {
		return refuse(SiteEntryArmUnset, what+".entry", "%s (write_id %s): the entry oneof names neither agent_update nor session_update", what, e.GetWriteId())
	}
	return nil
}

// validateEntryBatch is the base validation of store.v1.EntryBatch: every
// entry's envelope well-formed, a cursor advance (when the producer sent one)
// that names a file, and AT LEAST ONE OF THE TWO.
//
// A CURSOR-ONLY BATCH IS LEGAL. A sidecar that read bytes yielding no entries —
// a partial line, a block of records it had already absorbed — must still make
// its file position durable, and refusing it would leave the reader re-reading
// the same bytes forever. What is empty is a batch carrying NEITHER entries nor
// a cursor advance: that states nothing at all, which is a defect the store
// names rather than acknowledging as durable.
func validateEntryBatch(b *storev1.EntryBatch, carriesShapes bool) *refusal {
	if b == nil {
		return refuse(SiteBatchMissing, "batch", "batch: the request carries no EntryBatch")
	}
	if len(b.GetEntries()) == 0 && b.GetCursorAdvance() == nil && !carriesShapes {
		return refuse(SiteBatchEmpty, "batch", "batch: the EntryBatch carries neither entries nor a cursor advance, and the request carries no shape observation either")
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
		return refuse(SiteProducerEmpty, "producer", "producer: the write names no producer, so nothing can be attributed")
	}
	if ref := validateEntryBatch(req.GetBatch(), len(req.GetShapes()) > 0); ref != nil {
		return ref
	}
	// THE CATALOG IS PART OF THE WRITE, so a malformed observation refuses the
	// whole request here rather than reaching a transaction the records share.
	for i, shape := range req.GetShapes() {
		if ref := validateShapeObservation(shape, i); ref != nil {
			return ref
		}
	}
	return nil
}

// validateShapeObservation refuses a catalog observation that names no shape.
//
// EVERY FIELD BUT THE EXAMPLE IS REQUIRED. The example alone is optional
// because a line can legitimately be empty bytes; a hash, a kind, a rendering
// and an observation instant are what make a row matchable, filterable,
// readable and orderable, and a row missing any of them is one nobody can use.
func validateShapeObservation(s *storev1.ShapeObservation, index int) *refusal {
	what := fmt.Sprintf("shapes[%d]", index)
	if s == nil {
		return refuse(SiteShapeUnset, what, "%s: the shape observation is unset", what)
	}
	if s.GetShapeHash() == "" {
		return refuse(SiteShapeHashEmpty, what+".shape_hash", "%s: shape_hash is empty, and it is the catalog's primary key", what)
	}
	if s.GetKind() == "" {
		return refuse(SiteShapeKindEmpty, what+".kind", "%s (shape %s): kind is empty, so the observation names no residue kind", what, s.GetShapeHash())
	}
	if s.GetKeyStructure() == "" {
		return refuse(SiteShapeStructureEmpty, what+".key_structure", "%s (shape %s): key_structure is empty, so nothing says what was hashed", what, s.GetShapeHash())
	}
	if s.GetSeenMs() <= 0 {
		return refuse(SiteShapeSeenMsUnset, what+".seen_ms", "%s (shape %s): seen_ms is %d, not a positive unix millis; first_seen and last_seen come from the observer's clock", what, s.GetShapeHash(), s.GetSeenMs())
	}
	return nil
}

// validateListResidueShapesRequest refuses a catalog listing whose kind filter
// is an empty string standing in for absence.
func validateListResidueShapesRequest(req *storev1.ListResidueShapesRequest) *refusal {
	if req.Kind != nil && req.GetKind() == "" {
		return refuse(SiteShapeKindFilterEmpty, "kind", "kind: present but empty; omit the field to ask for every kind")
	}
	return nil
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

// validateWatchBashRunRequest is the use site for WatchBashRun: the run's unit
// identity is the whole address, so an empty one names no run.
func validateWatchBashRunRequest(req *storev1.WatchBashRunRequest) *refusal {
	if req.GetRun() == nil || req.GetRun().GetValue() == "" {
		return refuse(SiteRunEmpty, "run", "run: an AgentActivityId with no value addresses no run")
	}
	return nil
}

// validateGetLiveWorkRequest is the use site for GetLiveWork: `session` is
// REQUIRED. The store is shared by every session on the host, and the shim
// closes every obligation its own vendor does not hold, so an unscoped answer
// would make one session's start close another session's running work.
func validateGetLiveWorkRequest(req *storev1.GetLiveWorkRequest) *refusal {
	if req.GetSession() == nil || req.GetSession().GetValue() == "" {
		return refuse(SiteSessionEmpty, "session", "session: GetLiveWork names no session, and the store never answers it unscoped")
	}
	return nil
}

func validateGetSidecarCursorsRequest(req *storev1.GetSidecarCursorsRequest) *refusal {
	// file_id is OPTIONAL: absent asks for every cursor. Present but empty is
	// again a sentinel standing in for absence.
	if req.FileId != nil && req.GetFileId() == "" {
		return refuse(SiteFileIDEmpty, "file_id", "file_id: present but empty; omit the field to ask for every cursor")
	}
	return nil
}
