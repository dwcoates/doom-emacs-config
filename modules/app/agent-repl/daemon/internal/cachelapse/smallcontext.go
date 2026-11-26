package cachelapse

import (
	"fmt"
	"strconv"

	"agent-repld/internal/envc"
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
	if tokens <= 0 || tokens > k.deps.SmallContext {
		return "", false
	}
	return fmt.Sprintf("its context of %d tokens is at or below %d, too small for a keep-alive to save more than it costs", tokens, k.deps.SmallContext), true
}

// EnvTestSmallContext replaces SmallContextTokens in a TEST RUN: the keep-alive
// suites drive scenarios whose contexts are a few hundred tokens, which the
// production line would leave unkept, and their subject is the beat itself.
// Its value is a token count, 0 meaning no context is small. Honored only
// under the vendor guard, as every test-run seam is (IntervalFromEnv).
const EnvTestSmallContext = "AGENT_REPL_TEST_SMALL_CONTEXT_TOKENS"

// SmallContextFromEnv answers the largest context not kept warm in this
// process: SmallContextTokens, or EnvTestSmallContext's value in a test run.
func SmallContextFromEnv(contracts envc.Contracts, getenv func(string) string) (int64, error) {
	raw := getenv(EnvTestSmallContext)
	if raw == "" {
		return SmallContextTokens, nil
	}
	if err := contracts.RequireTestRun(EnvTestSmallContext+"="+raw, "a live daemon leaves contexts of "+strconv.FormatInt(SmallContextTokens, 10)+" tokens or fewer unkept"); err != nil {
		return 0, fmt.Errorf("cachelapse: %w", err)
	}
	tokens, err := strconv.ParseInt(raw, 10, 64)
	if err != nil {
		return 0, fmt.Errorf("cachelapse: %s=%q is not a token count: %w", EnvTestSmallContext, raw, err)
	}
	if tokens < 0 {
		return 0, fmt.Errorf("cachelapse: %s=%q is negative", EnvTestSmallContext, raw)
	}
	return tokens, nil
}
