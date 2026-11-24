package pageheld

import "testing"

// counts records what a table counted.
type counts struct {
	loads   []LoadReason
	unloads []UnloadReason
}

func (c *counts) CountPageLoad(r LoadReason)     { c.loads = append(c.loads, r) }
func (c *counts) CountPageUnload(r UnloadReason) { c.unloads = append(c.unloads, r) }

func TestALoadIsHeldAndCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)

	// Act
	table.Loaded("a", LoadAwake)

	// Assert
	if got := table.Snapshot(); len(got) != 1 || got["a"] != LoadAwake || len(c.loads) != 1 || c.loads[0] != LoadAwake {
		t.Fatalf("pages %v, loads %v; want a held for awake, counted once", got, c.loads)
	}
}

func TestAnUnloadDropsThePageAndIsCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)
	table.Loaded("a", LoadShown)

	// Act
	table.Unloaded("a", UnloadDormant)

	// Assert
	if got := table.Snapshot(); len(got) != 0 || len(c.unloads) != 1 || c.unloads[0] != UnloadDormant {
		t.Fatalf("pages %v, unloads %v; want none held, one dormant unload", got, c.unloads)
	}
}

func TestARestatementIsHeldButNeverCounted(t *testing.T) {
	// Arrange
	c := &counts{}
	table := New(c)

	// Act
	table.Held("a", LoadSlot)
	table.Held("b", LoadShown)
	table.Forget("b")

	// Assert
	if got := table.Snapshot(); len(got) != 1 || got["a"] != LoadSlot || len(c.loads)+len(c.unloads) != 0 {
		t.Fatalf("pages %v, loads %v, unloads %v; want a held, nothing counted", got, c.loads, c.unloads)
	}
}

func TestTheSnapshotIsACopy(t *testing.T) {
	// Arrange
	table := New(&counts{})
	table.Held("a", LoadSlot)

	// Act
	snap := table.Snapshot()
	delete(snap, "a")

	// Assert
	if len(table.Snapshot()) != 1 {
		t.Fatal("changing the snapshot changed the table")
	}
}

func TestAnUndeclaredLoadReasonPanics(t *testing.T) {
	// Arrange
	table := New(&counts{})
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("an undeclared load reason did not panic")
		}
	}()

	// Act
	table.Loaded("a", "whim")
}

func TestATableWithoutTelemetryPanics(t *testing.T) {
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("New(nil) did not panic")
		}
	}()

	// Act
	New(nil)
}
