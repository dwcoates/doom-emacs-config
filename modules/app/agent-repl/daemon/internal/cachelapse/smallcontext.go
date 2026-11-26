package cachelapse

import (
	"fmt"

	"agent-repld/internal/ids"
)

// A SMALL CONTEXT IS NOT KEPT WARM (owner request, 2026-11-26). A keep-alive
// re-reads the whole context from the cache, and a cold resume rewrites it, so
// at a context this small both sides of the trade are trivial and the beats'
// own overhead (a restore, a turn, a park) costs more than the rewrite they
// protect against. A workspace whose context is at or below
// SmallContextTokens owes no keep-alive; its cache is left to lapse as any
// unkept one does, and nothing is marked cold, since the shim's cold gate asks
// only above its own floor.
//
// The figure is the one the topbar's context chip states (Deps.ContextTokens),
// and ZERO MEANS NONE REPORTED YET, which is not a small context: a workspace
// whose vendor has stated no usage is kept warm as before.

// SmallContextTokens is the largest context, in tokens, that is not kept warm.
const SmallContextTokens int64 = 80_000

// smallContext answers whether WS's context is reported and at or below
// SmallContextTokens, with the reason it owes no keep-alive when it is.
func (k *Keeper) smallContext(ws ids.WorkspaceID) (string, bool) {
	tokens := k.deps.ContextTokens(ws)
	if tokens <= 0 || tokens > SmallContextTokens {
		return "", false
	}
	return fmt.Sprintf("its context of %d tokens is at or below %d, too small for a keep-alive to save more than it costs", tokens, SmallContextTokens), true
}
