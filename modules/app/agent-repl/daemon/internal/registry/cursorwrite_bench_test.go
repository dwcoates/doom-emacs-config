package registry

import (
	"fmt"
	"testing"
)

// THE REPLAY-DRAIN CURSOR WRITE.
//
// shimclient.advanceDurableCursor calls SeqStore.SetLastSeq — which lands here
// as Registry.Update — for EVERY persistent event whose accounting transaction
// is settled. A transcript replayed after a redeploy therefore performs one
// registry mutation per event, and a mutation is a read-modify-write of the
// WHOLE registry. This benchmark pins what that costs on a steady-state
// registry, which is the size the cost scales with.
//
// benchRegistrySize is the retention limit plus a realistic live fleet: the
// registry keeps TerminalRetention terminal records and however many workspaces
// are open.
const benchRegistrySize = TerminalRetention + 24

// newBenchRegistry returns a prepared registry holding a steady-state fleet.
func newBenchRegistry(tb testing.TB) *Registry {
	tb.Helper()
	r := Open(testBenchPath(tb), discardLogf)
	tb.Cleanup(func() { _ = r.Close() })
	if err := r.Prepare(); err != nil {
		tb.Fatalf("prepare: %v", err)
	}
	for i := range benchRegistrySize {
		rec := Record{
			SessionID:       fmt.Sprintf("s_%04d", i),
			CWD:             fmt.Sprintf("/ws/workspace-%04d", i),
			Model:           "claude-opus-4",
			PermissionMode:  "auto",
			ConfigDir:       "/home/bench/.claude",
			ClaudeSessionID: fmt.Sprintf("uuid-%04d", i),
			CreatedAt:       "2026-07-12T00:00:00Z",
			BackfillState:   "done",
			// Most of a steady-state registry is terminal history the
			// retention cap holds onto.
			Terminal:   i < TerminalRetention,
			TerminalAt: "2026-07-12T01:00:00Z",
		}
		if err := r.Put(rec); err != nil {
			tb.Fatalf("put %s: %v", rec.SessionID, err)
		}
	}
	return r
}

// testBenchPath names a fresh state store for one benchmark.
func testBenchPath(tb testing.TB) string {
	tb.Helper()
	return tb.TempDir() + "/state.db"
}

// BenchmarkCursorWrite measures one durable last_seq advance — the per-event
// write a replayed transcript performs.
func BenchmarkCursorWrite(b *testing.B) {
	r := newBenchRegistry(b)
	target := fmt.Sprintf("s_%04d", benchRegistrySize-1)
	b.ReportAllocs()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		found, err := r.Update(target, func(rec *Record) { rec.LastSeq = uint64(i + 1) })
		if err != nil {
			b.Fatalf("update: %v", err)
		}
		if !found {
			b.Fatalf("update reported no record for %s", target)
		}
	}
	b.StopTimer()
	perWrite := float64(b.Elapsed().Nanoseconds()) / float64(b.N)
	b.ReportMetric(1e9/perWrite, "writes/s")
	b.ReportMetric(perWrite/1e6, "ms/write")
}
