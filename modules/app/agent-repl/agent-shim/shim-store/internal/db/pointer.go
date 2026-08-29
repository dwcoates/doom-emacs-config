package db

import (
	"strconv"
	"strings"

	storev1 "agentrepl/proto/store/v1"
)

// pointerPrefix marks a value as one THIS store minted, in THIS encoding.
//
// A PREFIX RATHER THAN A BARE NUMBER because the two failures a caller can
// commit are different answers: a value that was never a pointer at all is an
// ILLEGAL request (ErrInvalid), while a well-formed pointer naming no row of
// the book asked about is a STALE one (ErrStalePointer). Without a marker the
// store could not tell "the caller sent a session token by mistake" from "the
// caller is walking a book that was nuked underneath it".
const pointerPrefix = "sip1-"

// encodePointer mints the opaque pointer for one entry position.
//
// The caller never parses it. It encodes `position` — the FIRST-INSERT order —
// which is exactly why a pointer survives an upsert of the row it names.
func encodePointer(position int64) *storev1.StoreItemPointer {
	return &storev1.StoreItemPointer{Value: pointerPrefix + strconv.FormatInt(position, 36)}
}

// decodePointer recovers the position a pointer names, refusing anything this
// store did not mint. The refusal is ErrInvalid: a malformed pointer is a
// malformed REQUEST, and reporting it as merely stale would tell the caller to
// re-open when what it must do is stop sending garbage.
func decodePointer(p *storev1.StoreItemPointer, field string) (int64, error) {
	if p == nil {
		return 0, invalidf("%s is unset", field)
	}
	value := p.GetValue()
	if value == "" {
		return 0, invalidf("%s.value is empty", field)
	}
	rest, ok := strings.CutPrefix(value, pointerPrefix)
	if !ok {
		return 0, invalidf("%s.value %q is not a store-minted item pointer", field, value)
	}
	position, err := strconv.ParseInt(rest, 36, 64)
	if err != nil {
		return 0, invalidf("%s.value %q is not a store-minted item pointer", field, value)
	}
	if position <= 0 {
		return 0, invalidf("%s.value %q names position %d, which this store never assigns", field, value, position)
	}
	return position, nil
}
