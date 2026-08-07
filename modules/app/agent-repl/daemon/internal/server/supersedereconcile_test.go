package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// The boot half: history a previous daemon left standing has no successor edge
// coming for it, so nothing but a boot pass will ever close it.

func TestBootReconciliationResolvesAStalePersistedSupersede(t *testing.T) {
	// Arrange — the history the fix has to heal: a record written before the
	// resolution existed, whose successor edge is never coming.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_ancient", "/w"))

	// Act.
	resolved := ReconcileSupersededDeaths(reg, func() int64 { return 4242 }, nil)

	// Assert.
	if resolved != 1 {
		t.Fatalf("reconciled %d records, want 1", resolved)
	}
	rec, _ := reg.Get("s_ancient")
	if rec.DeathResolvedAtMs != 4242 {
		t.Fatalf("death_resolved_at_ms = %d, want 4242", rec.DeathResolvedAtMs)
	}
}

func TestBootReconciliationLeavesOtherDeathReasonsOpen(t *testing.T) {
	// Arrange.
	tests := []struct {
		name   string
		reason string
	}{
		{name: "deleted", reason: errclass.DeathReasonDeleted},
		{name: "shim died", reason: errclass.DeathReasonShimDied},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			reg := openTestRegistry(t)
			seedRecords(t, reg,
				registry.Record{SessionID: "s_old", CWD: "/w", Terminal: true, DeathReason: tc.reason})

			// Act.
			resolved := ReconcileSupersededDeaths(reg, func() int64 { return 4242 }, nil)

			// Assert.
			if resolved != 0 {
				t.Fatalf("reconciled %d records for reason %q, want 0", resolved, tc.reason)
			}
			rec, _ := reg.Get("s_old")
			if rec.DeathResolvedAtMs != 0 {
				t.Fatalf("death_resolved_at_ms = %d for reason %q, want 0", rec.DeathResolvedAtMs, tc.reason)
			}
		})
	}
}

func TestBootReconciliationLeavesALiveSessionAlone(t *testing.T) {
	// Arrange — a non-terminal record has no death to resolve at all.
	reg := openTestRegistry(t)
	seedRecords(t, reg, registry.Record{SessionID: "s_live", CWD: "/w"})

	// Act.
	resolved := ReconcileSupersededDeaths(reg, func() int64 { return 4242 }, nil)

	// Assert.
	if resolved != 0 {
		t.Fatalf("reconciled %d live records, want 0", resolved)
	}
}

func TestBootReconciliationDoesNotRestampAnAlreadyResolvedSupersede(t *testing.T) {
	// Arrange — a record closed by a previous boot keeps the instant it was
	// actually closed at, so the history stays honest across restarts.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))
	ReconcileSupersededDeaths(reg, func() int64 { return 4242 }, nil)

	// Act — the next boot.
	resolved := ReconcileSupersededDeaths(reg, func() int64 { return 9900 }, nil)

	// Assert.
	if resolved != 0 {
		t.Fatalf("reconciled %d already-resolved records, want 0", resolved)
	}
	rec, _ := reg.Get("s_old")
	if rec.DeathResolvedAtMs != 4242 {
		t.Fatalf("death_resolved_at_ms = %d, want the original 4242", rec.DeathResolvedAtMs)
	}
}

func TestSessionViewCarriesTheResolvedSupersede(t *testing.T) {
	// Arrange — the whole point: the resolution has to reach the frontend on
	// the SessionView, which is the only place a death is ever rendered from.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))
	ReconcileSupersededDeaths(reg, func() int64 { return 4242 }, nil)
	rec, _ := reg.Get("s_old")

	// Act.
	view := SessionViewFromRecord(t.Logf, rec, nil, false)

	// Assert.
	if view.GetDeath().GetResolvedAtMs() != 4242 {
		t.Fatalf("view death resolved_at_ms = %d, want 4242", view.GetDeath().GetResolvedAtMs())
	}
}
