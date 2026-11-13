package rollout

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func TestPreflightSpawn(t *testing.T) {
	tests := []struct {
		name string
		// incumbent is the address this daemon joined, empty for a daemon
		// that owns its state.
		incumbent    string
		preflightErr error
		wantErr      bool
		wantLevel    string
		wantFaults   int
	}{
		{name: "a fresh daemon that can spawn records it and opens no fault", wantLevel: dlog.LevelInfo},
		{name: "a joining successor that can spawn records it and opens no fault", incumbent: "127.0.0.1:7000", wantLevel: dlog.LevelInfo},
		{name: "a joining successor that cannot spawn refuses before it serves and opens no fault of its own", incumbent: "127.0.0.1:7000", preflightErr: errFake, wantErr: true, wantLevel: dlog.LevelError},
		{name: "a fresh daemon that cannot spawn serves with the successor fault open", preflightErr: errFake, wantLevel: dlog.LevelError, wantFaults: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, func(d *Deps) { d.IncumbentAddress = tt.incumbent })
			h.spawner.preflightErr = tt.preflightErr

			// Act
			err := h.c.PreflightSpawn(context.Background())

			// Assert
			if gotErr := err != nil; gotErr != tt.wantErr || (err != nil && !errors.Is(err, errFake)) {
				t.Fatalf("PreflightSpawn = %v, want an error wrapping the spawner's: %v", err, tt.wantErr)
			}
			if !slices.Equal(h.spawner.preflighted, []string{h.c.deps.SelfAddress}) {
				t.Fatalf("Preflight was told %v, want once this daemon's own address %q", h.spawner.preflighted, h.c.deps.SelfAddress)
			}
			rec, found := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: opSpawnPreflight})
			if !found || rec.Context["self_address"] != h.c.deps.SelfAddress || rec.Context["joining"] != (tt.incumbent != "") {
				t.Fatalf("records = %+v, want a %s %s record naming this daemon", h.log.Records(), tt.wantLevel, opSpawnPreflight)
			}
			if tt.preflightErr != nil && rec.Context["cause"] != tt.preflightErr.Error() {
				t.Fatalf("record = %+v, want the spawner's failure as its cause", rec)
			}
			open := successorFaults(t, h)
			if len(open) != tt.wantFaults {
				t.Fatalf("faults = %+v, want %d", open, tt.wantFaults)
			}
			if tt.wantFaults > 0 && (open[0].Workspace != nil || !strings.Contains(open[0].Detail, errFake.Error())) {
				t.Fatalf("fault = %+v, want it daemon-scoped and naming the cause", open[0])
			}
			if n := len(h.spawner.Told()); n != 0 {
				t.Fatalf("the preflight spawned %d successors, want none", n)
			}
		})
	}
}
