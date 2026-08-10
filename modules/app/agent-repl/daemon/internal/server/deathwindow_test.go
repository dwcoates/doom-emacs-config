package server

import (
	"testing"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// ---------------------------------------------------------------------------
// The shared close: one vocabulary of window-shaped deaths, one write that
// stamps them. Every edge that settles a death goes through both, so these pin
// the guarantee the edges themselves stop having to restate.
// ---------------------------------------------------------------------------

func TestStampDeathResolutionClosesAnOpenWindow(t *testing.T) {
	// Arrange.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))

	// Act.
	stamped, found, err := stampDeathResolution(reg, "s_old", 1700)

	// Assert.
	if err != nil {
		t.Fatalf("stampDeathResolution: %v", err)
	}
	if !stamped || !found {
		t.Fatalf("stamped=%v found=%v, want both true", stamped, found)
	}
	rec, _ := reg.Get("s_old")
	if rec.DeathResolvedAtMs != 1700 {
		t.Fatalf("death_resolved_at_ms = %d, want 1700", rec.DeathResolvedAtMs)
	}
}

func TestStampDeathResolutionRefusesToRestampASettledDeath(t *testing.T) {
	// Arrange — two recovery edges can legitimately race for one record, and
	// the loser must not overwrite the instant the winner recorded.
	reg := openTestRegistry(t)
	seedRecords(t, reg, superseded("s_old", "/w"))
	if _, _, err := stampDeathResolution(reg, "s_old", 1700); err != nil {
		t.Fatalf("first stamp: %v", err)
	}

	// Act.
	stamped, _, err := stampDeathResolution(reg, "s_old", 9900)

	// Assert.
	if err != nil {
		t.Fatalf("stampDeathResolution: %v", err)
	}
	if stamped {
		t.Fatal("stamped a death that was already settled")
	}
	rec, _ := reg.Get("s_old")
	if rec.DeathResolvedAtMs != 1700 {
		t.Fatalf("death_resolved_at_ms = %d, want the first close at 1700", rec.DeathResolvedAtMs)
	}
}

func TestStampDeathResolutionRefusesAnEventShapedDeath(t *testing.T) {
	// Arrange — "the agent process exited" stays true no matter what comes up
	// afterwards, so no edge may settle it.
	reg := openTestRegistry(t)
	seedRecords(t, reg, registry.Record{
		SessionID: "s_old", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonShimDied,
	})

	// Act.
	stamped, found, err := stampDeathResolution(reg, "s_old", 1700)

	// Assert.
	if err != nil {
		t.Fatalf("stampDeathResolution: %v", err)
	}
	if stamped {
		t.Fatal("stamped a shim death; it is event-shaped and has no closing edge")
	}
	if !found {
		t.Fatal("found = false for a record the registry holds")
	}
}

func TestStampDeathResolutionReportsAMissingRecord(t *testing.T) {
	// Arrange — a record can be pruned between the caller's read and this
	// write, and "gone" must be distinguishable from "someone else closed it".
	reg := openTestRegistry(t)

	// Act.
	stamped, found, err := stampDeathResolution(reg, "s_gone", 1700)

	// Assert.
	if err != nil {
		t.Fatalf("stampDeathResolution: %v", err)
	}
	if stamped || found {
		t.Fatalf("stamped=%v found=%v for an absent record, want both false", stamped, found)
	}
}

// TestEveryWindowDeathReasonClosesOnBothEdges is the shared-shape assertion. A
// reason added to the vocabulary is closed by BOTH resolvers because both ask
// resolvableDeath; a hand-rolled resolver that grew its own reason list would
// fail here rather than silently leaving half the vocabulary open.
func TestEveryWindowDeathReasonClosesOnBothEdges(t *testing.T) {
	for reason := range windowResolvableDeathReasons {
		t.Run(reason, func(t *testing.T) {
			// Arrange — the same record shape put to each edge in turn.
			open := func() *registry.Registry {
				reg := openTestRegistry(t)
				seedRecords(t, reg,
					registry.Record{SessionID: "s_old", CWD: "/w", Terminal: true, DeathReason: reason},
					registry.Record{SessionID: "s_new", CWD: "/w"},
				)
				return reg
			}

			// Act — the live edge.
			live := open()
			(&RegistryRegistrar{Reg: live, Now: func() int64 { return 1700 }}).SessionOperational("/w", "s_new")

			// Act — the boot edge.
			boot := open()
			ReconcileOpenDeaths(boot, func() int64 { return 1700 }, nil)

			// Assert.
			for name, reg := range map[string]*registry.Registry{"operational": live, "boot": boot} {
				rec, _ := reg.Get("s_old")
				if rec.DeathResolvedAtMs != 1700 {
					t.Fatalf("%s edge left reason %q open (death_resolved_at_ms = %d)", name, reason, rec.DeathResolvedAtMs)
				}
			}
		})
	}
}
