package convert

// A FORK'S INHERITED RECORDS ARE NEVER STORED IN THE FORK'S BOOK.
//
// The vendor's session fork copies the parent's records into the fork's
// transcript with their content unchanged and stamps each copy with
// `forkedFrom: {sessionId, messageUuid}`, naming the parent record it came from
// (shim.v1 ForkSession, proto/src/shim/v1/endpoint_fork_session.proto). Those
// records are already filed in the PARENT's book, and the fork's feed reads its
// inherited past from there. Storing them again would file the same facts under
// two books: the store keys rows by what the record says (a tool_use id, a
// message id), so the second write is refused as a row moving between books.
//
// THE STAMP IS THE WHOLE TEST. It is the vendor's own mark, present on exactly
// the records it copied and on nothing the fork goes on to say, so the skip is
// exact without counting lines or reading the parent. Like a keep-alive's
// records, an inherited record is still converted — every join it opens or
// settles stays as warm as any other's — and only its entries are withheld.

// inheritedByFork reports whether `record` is one the vendor's fork copied
// from the parent conversation.
func inheritedByFork(record map[string]any) bool {
	stamp, ok := record["forkedFrom"].(map[string]any)
	return ok && stamp != nil
}
