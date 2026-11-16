package cachelapse

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A KEEP-ALIVE RUN STOPS AT ITS BREAK-EVEN (owner request, 2026-11-11). A
// keep-alive re-reads the context C from the cache, about CacheReadPrice x C.
// A cold resume rewrites it at CacheWritePrice x C, so it costs about
// (CacheWritePrice - CacheReadPrice) x C more than a warm one. After
// KeepaliveRunCap keep-alives with no turn between them, the rent spent equals
// the one rewrite it protects against, whatever the size of C: the rent-or-buy
// rule, which bounds the worst case at about twice the optimum. Of the 47 runs
// from 2026-11-01 to 11-02, 19 ended with the workspace never working again,
// and e2e-scenario-consolidation and webhost-page-boot-silent each spent about
// 40 keep-alives on a ~370k context.
//
// The run is counted on the workspace's row (wsm.Workspace.KeepaliveRunBeats),
// so a daemon restart, frequent as they are, never starts it over; a turn's
// close ends it. A capped workspace's cache is marked cold (coldmark.go), so
// the cold gate greets the user's return. A WORKSPACE WITH A MERGE QUEUED IS
// NEVER CAPPED (owner ruling, 2026-10-26, in beatPlanFor): its merge's prompts
// run on its session.

// The vendor's cache prices, in tenths of the base input price: a read at a
// tenth of it, a write at the one-hour tier at twice it. The shim's coverage
// rule names the same two prices (shim.engine.cold.CACHE_READ_PRICE and
// CACHE_WRITE_PRICE).
const (
	CacheReadPriceTenths  = 1
	CacheWritePriceTenths = 20
)

// KeepaliveRunCap is how many keep-alives a run spends before the rewrite it
// protects against is cheaper: ceil((write - read) / read), 19 at the
// vendor's prices (about 16.5 hours at KeepaliveInterval).
const KeepaliveRunCap = (CacheWritePriceTenths - CacheReadPriceTenths + CacheReadPriceTenths - 1) / CacheReadPriceTenths

// runCapped answers whether RECORD's keep-alive run has reached its
// break-even, and marks the cache it keeps cold the first time it finds it
// so. QUEUED exempts a workspace with a merge queued.
func (k *Keeper) runCapped(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, record wsm.Workspace, queued bool) (string, bool) {
	beats := record.KeepaliveRunBeats
	if beats < KeepaliveRunCap || queued {
		return "", false
	}
	why := fmt.Sprintf("its keep-alive run reached its break-even: %d keep-alives with no turn between them, against a cap of %d", beats, KeepaliveRunCap)
	if record.CacheClock() != nil && !record.CacheKnownCold() {
		log.Info(opDrive, "the workspace's keep-alive run reached its break-even; its cache is left to lapse and marked cold", dlog.Context{
			"run_beats": beats, "run_cap": KeepaliveRunCap,
			"read_price_tenths": CacheReadPriceTenths, "write_price_tenths": CacheWritePriceTenths,
		})
		// A mark that cannot be written is recorded at ERROR by markCold; the
		// run still owes no keep-alive, and the next reconcile marks again.
		_ = k.markCold(ctx, ws, why)
	}
	return why, true
}
