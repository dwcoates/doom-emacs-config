package frontend

import (
	"context"
	"fmt"
)

// WHO IS READING, and why the answer is the CONNECTION.
//
// conversation-history.proto's position is per READER per workspace: two tabs
// scrolled to different depths of the same conversation must not read each
// other's place, and one tab open on two workspaces keeps two places. A reader
// is therefore a connected frontend, and the only identity a connected frontend
// has here is the connection itself.
//
// IT IS NOT A WIRE FIELD, deliberately. A client-supplied reader id is a value
// the client authors, and a client that authored someone else's id would read
// someone else's position — the same category of mistake as authoring a seq.
// The daemon mints it at accept, where it cannot be forged.
//
// It travels on the CONTEXT rather than through every CommandHandler method
// because exactly two commands have any use for it, and widening thirty
// unrelated signatures to carry an identity they must ignore is how a parameter
// becomes decoration. The two that do use it take it EXPLICITLY, so a handler
// can never silently forget to ask: an absent reader is a loud refusal at the
// dispatch arm, not a page filed under the empty key.

// readerContextKey is the private context key the reader identity travels on.
type readerContextKey struct{}

// ContextWithReader stamps the reading identity of one connection onto the
// context its commands are dispatched with.
func ContextWithReader(ctx context.Context, reader string) context.Context {
	return context.WithValue(ctx, readerContextKey{}, reader)
}

// ReaderFrom reports the reading identity the context carries, and whether it
// carries one at all. An absent identity is never defaulted: the caller refuses.
func ReaderFrom(ctx context.Context) (string, bool) {
	reader, ok := ctx.Value(readerContextKey{}).(string)
	if !ok || reader == "" {
		return "", false
	}
	return reader, true
}

// connectionReader names one connection as a reader.
//
// The id is minted per daemon RUN and no connection survives a bounce, which is
// why the SSM discards every stored position at Open — a surviving row would
// otherwise be inherited by an unrelated later connection that happened to be
// handed the same number.
func connectionReader(id uint64) string {
	return fmt.Sprintf("conn-%d", id)
}
