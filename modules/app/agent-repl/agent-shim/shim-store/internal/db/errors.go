package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
)

// The store's three refusal classes. Each is errors.Is-able and each wraps a
// human detail the server turns into a typed failure arm's `detail` string.
//
// THREE AND NOT MORE, deliberately. A caller does not act on a finer taxonomy
// than this: it either sent something illegal (ErrInvalid), named a position
// that is not in the book it asked about (ErrStalePointer), or hit the
// database itself (ErrStorage). The `detail` is what a human reads; nothing
// switches on it.
var (
	// ErrInvalid is every VALIDATION refusal: an unset non-optional field, an
	// unset oneof, an empty identifier, a pointer that is not a store-minted
	// pointer at all.
	ErrInvalid = errors.New("invalid request")

	// ErrStalePointer is a well-formed, store-minted pointer that names no row
	// of the book it was presented against.
	ErrStalePointer = errors.New("stale pointer")

	// ErrStorage is the database itself failing. It always wraps the driver's
	// own error so the cause survives to the log.
	ErrStorage = errors.New("storage failure")

	// ErrUnknownAgent is a well-formed agent id that names no book of this
	// store — no agent row was ever recorded for it.
	//
	// IT IS NOT ErrInvalid: the request is perfectly well formed and the caller
	// cannot fix it by respelling anything. It is its own class because it is
	// its own wire arm (OpenAgentSessionFailure.unknown_agent), which the shim
	// maps to NotFound.
	ErrUnknownAgent = errors.New("unknown agent")
)

// The refusal SITES this package can produce.
//
// THEY LIVE HERE, NOT IN THE SERVER, because this is the layer that decides
// them: the server opens only the write ENVELOPE, so every refusal about what
// is INSIDE a frame is this package's to name. internal/server aliases these
// constants rather than restating the strings, so the vocabulary has one
// spelling and cannot drift between the layer that emits a site and the layer
// that logs it.
const (
	// SiteStoreRefusedRequest is the storage layer refusing the request on its
	// own validation, with no finer site to name.
	SiteStoreRefusedRequest = "store_refused_request"
	// SiteStalePointer is a well-formed pointer naming no row of its book.
	SiteStalePointer = "stale_pointer"
	// SiteUpsertChangesIdentity is a write that would move an existing row to
	// another book or another kind. Identity per thing.
	SiteUpsertChangesIdentity = "upsert_changes_identity"
	// SitePageBookMismatch is a page line whose envelope names a different
	// agent than the frame inside it does.
	SitePageBookMismatch = "page_book_mismatch"
	// SiteResidueRawUnset is residue that carries no verbatim record, which is
	// the only thing it exists to carry.
	SiteResidueRawUnset = "residue_raw_unset"
	// SiteUnknownAgent is a well-formed agent id the store holds no agent row
	// for. An agent the store HAS heard of but that has said nothing yet is not
	// this: it is a legal, empty book.
	SiteUnknownAgent = "unknown_agent"
	// SiteSessionEmpty is a live-work read naming no session. The store is
	// shared by every session on the host, so an unscoped answer would hand
	// one session every other session's obligations; it is refused instead.
	SiteSessionEmpty = "session_empty"
)

// refusal is one refusal's STRUCTURED detail: the site, the store's own name
// for the field at fault, and the class sentinel it answers to.
//
// THE FIELD NAME CROSSES THE LAYER BOUNDARY because the failure arm carries it
// on the wire. The server names the fields IT validates; this package names the
// fields inside a frame, which the server never opens — so without carrying the
// name up, every db-side refusal would reach the producer as an unnamed
// "invalid request" and the producer's own logs could not say what it sent
// wrong.
type refusal struct {
	site   string
	field  string
	detail string
	class  error
}

func (r *refusal) Error() string { return r.class.Error() + ": " + r.detail }

// Unwrap keeps errors.Is(err, ErrInvalid) and errors.Is(err, ErrStalePointer)
// working, so no caller has to learn this type to classify a refusal.
func (r *refusal) Unwrap() error { return r.class }

// RefusalSite reports the site a refusal names, or "" for an error that is not
// one of this package's structured refusals.
func RefusalSite(err error) string {
	var target *refusal
	if errors.As(err, &target) {
		return target.site
	}
	return ""
}

// RefusalField reports the store's own name for the field a refusal blames, or
// "" when the refusal blames no single field.
func RefusalField(err error) string {
	var target *refusal
	if errors.As(err, &target) {
		return target.field
	}
	return ""
}

// invalidf builds an ErrInvalid that blames no single field.
func invalidf(format string, args ...any) error {
	return &refusal{site: SiteStoreRefusedRequest, detail: fmt.Sprintf(format, args...), class: ErrInvalid}
}

// invalidFieldf builds an ErrInvalid naming the field at fault.
func invalidFieldf(field, format string, args ...any) error {
	return &refusal{site: SiteStoreRefusedRequest, field: field, detail: fmt.Sprintf(format, args...), class: ErrInvalid}
}

// invalidSitef builds an ErrInvalid at a NAMED site — one of the specific
// refusals this package owns, rather than the generic one.
func invalidSitef(site, field, format string, args ...any) error {
	return &refusal{site: site, field: field, detail: fmt.Sprintf(format, args...), class: ErrInvalid}
}

// unknownAgentf builds an ErrUnknownAgent naming the field that addressed the
// book nobody kept.
func unknownAgentf(field, format string, args ...any) error {
	return &refusal{site: SiteUnknownAgent, field: field, detail: fmt.Sprintf(format, args...), class: ErrUnknownAgent}
}

// stalePointerf builds an ErrStalePointer with the detail a human reads.
func stalePointerf(field, format string, args ...any) error {
	return &refusal{site: SiteStalePointer, field: field, detail: fmt.Sprintf(format, args...), class: ErrStalePointer}
}

// storagef builds an ErrStorage that keeps `cause` reachable through
// errors.Is/As, so the driver's own classification is never lost.
func storagef(cause error, format string, args ...any) error {
	return fmt.Errorf("%w: %s: %w", ErrStorage, fmt.Sprintf(format, args...), cause)
}

// errNotAPageLine is the cause of a row indexed as a page line whose stored
// frame carries none — a corruption of this store's own invariant, never
// anything a caller did.
var errNotAPageLine = errors.New("entry row indexed as a page line carries no serveable frame")

// errNotABashRow is the cause of a row indexed as a bash row whose stored
// frame carries none — a corruption of this store's own invariant.
var errNotABashRow = errors.New("entry row indexed as a bash row carries no bash frame")

// isContextError reports the caller's own cancellation or deadline, which is
// nobody's fault and never a storage failure.
func isContextError(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}

// isNoRows reports the driver's empty-result signal.
func isNoRows(err error) bool { return errors.Is(err, sql.ErrNoRows) }
