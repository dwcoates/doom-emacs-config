package db

import (
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
	// unset oneof, an empty identifier, a zero page size, a pointer that is
	// not a store-minted pointer at all.
	ErrInvalid = errors.New("invalid request")

	// ErrStalePointer is a well-formed, store-minted pointer that names no row
	// of the book it was presented against.
	ErrStalePointer = errors.New("stale pointer")

	// ErrStorage is the database itself failing. It always wraps the driver's
	// own error so the cause survives to the log.
	ErrStorage = errors.New("storage failure")
)

// invalidf builds an ErrInvalid with the detail a human reads.
func invalidf(format string, args ...any) error {
	return fmt.Errorf("%w: %s", ErrInvalid, fmt.Sprintf(format, args...))
}

// stalePointerf builds an ErrStalePointer with the detail a human reads.
func stalePointerf(format string, args ...any) error {
	return fmt.Errorf("%w: %s", ErrStalePointer, fmt.Sprintf(format, args...))
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

// isNoRows reports the driver's empty-result signal.
func isNoRows(err error) bool { return errors.Is(err, sql.ErrNoRows) }
