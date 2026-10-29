package cachelapse

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// This file is the cold mark (wsm.Workspace.CacheKnownCold): the turn clock at
// which a workspace's prompt cache was last known cold, on its row, so no
// keep-alive is owed until a real turn closes after it (owner ruling,
// 2026-10-29: passing the cold gate by any answer makes no LLM request of its
// own). A cold gate's raise and answer mark it, and so does a shim judging a
// beat's cache lapsed. The mark is durable, so a restart's beats honor it.

const opColdMark = "daemon.cachelapse.cold_mark"

// ColdGateAnswered is the user's answer on WS's cold gate, any answer, taken
// before the re-open it chose starts: the cache is marked cold, so the session
// the answer brings up owes no keep-alive until its next real turn ends. A
// cleared or compacted conversation's shim states no cache lifetime and would
// never refuse the beat, and a paid one's old lifetime may still cover it, so
// the shim's judgment cannot stand in for the mark.
//
// No session runs while the answer is taken, so no turn can close between the
// answer and the mark. A mark that cannot be written is answered, and the
// answer is refused: the re-open would owe a keep-alive at once.
func (k *Keeper) ColdGateAnswered(ctx context.Context, ws ids.WorkspaceID) error {
	if err := k.markCold(ctx, ws, "its cold gate was answered"); err != nil {
		return err
	}
	k.enqueue(edge{ws: ws, why: "its cold gate was answered"})
	return nil
}

// markCold marks WS's cache cold at its turn clock, WHY naming what knew it
// cold. It is THE ONE write of the mark (TestColdMarkWritesHaveOneHome), and
// records its own outcome: INFO on the mark, ERROR on a failure, which it
// answers.
func (k *Keeper) markCold(ctx context.Context, ws ids.WorkspaceID, why string) error {
	log := k.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	marked, err := k.deps.DB.MarkCacheCold(ctx, ws)
	if err != nil {
		if !leaving(ctx, log, opColdMark, "marking the cache cold", err) {
			log.Error(opColdMark, "could not mark the workspace's cache cold; a keep-alive may be owed for a cold cache", dlog.Context{
				"why": why, "cause": err.Error(),
			})
		}
		return fmt.Errorf("cachelapse: mark the cache of %s cold: %w", ws, err)
	}
	fields := dlog.Context{"why": why, "turn_clock": "none"}
	if marked != nil {
		fields["turn_clock"] = *marked
	}
	log.Info(opColdMark, "marked the workspace's cache cold; no keep-alive is owed until a turn closes after it", fields)
	return nil
}
