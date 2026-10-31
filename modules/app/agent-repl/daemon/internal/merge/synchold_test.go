package merge

import (
	"context"
	"errors"
	"testing"
)

func TestHoldTargetForSyncIsGrantedOnlyWithNoMergeInFlight(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(t *testing.T, h *harness)
		wantHeld bool
	}{
		{name: "an empty queue", arrange: func(*testing.T, *harness) {}, wantHeld: true},
		{name: "a merge in line", arrange: func(t *testing.T, h *harness) { enqueue(t, h) }},
		{name: "a merge running", arrange: func(t *testing.T, h *harness) { h.o.running[h.repoKey()] = &run{repo: h.repoKey()} }},
		{name: "another sync holding it", arrange: func(t *testing.T, h *harness) {
			if _, held, err := h.o.HoldTargetForSync(context.Background(), h.targetD); err != nil || !held {
				t.Fatalf("the first hold = %v, %v; want held", held, err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			tt.arrange(t, h)

			// Act.
			release, held, err := h.o.HoldTargetForSync(context.Background(), h.targetD)

			// Assert.
			if err != nil || held != tt.wantHeld || (release != nil) != tt.wantHeld {
				t.Fatalf("HoldTargetForSync = %v, %v; want held %v", held, err, tt.wantHeld)
			}
		})
	}
}

func TestAMergeWaitsOutASyncAndIsAdmittedOnItsRelease(t *testing.T) {
	// Arrange: a sync holds the target, then a merge is put in line.
	h := newHarness(t)
	release, held, err := h.o.HoldTargetForSync(context.Background(), h.targetD)
	if err != nil || !held {
		t.Fatalf("HoldTargetForSync = %v, %v", held, err)
	}
	enqueue(t, h)

	// Act.
	_, _, admittedHeld, errHeld := h.o.admitFront(context.Background(), h.repoKey())
	release()
	_, _, admittedFree, errFree := h.o.admitFront(context.Background(), h.repoKey())

	// Assert.
	if errHeld != nil || admittedHeld {
		t.Fatalf("admitFront under the hold = %v, %v; want nothing admitted", admittedHeld, errHeld)
	}
	if errFree != nil || !admittedFree {
		t.Fatalf("admitFront after the release = %v, %v; want the merge admitted", admittedFree, errFree)
	}
}

func TestHoldTargetForSyncOfAnUnreadableRepositoryIsItsError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.commonDirErr = errors.New("not a repository")

	// Act.
	_, held, err := h.o.HoldTargetForSync(context.Background(), h.targetD)

	// Assert.
	if err == nil || held {
		t.Fatalf("HoldTargetForSync = %v, %v; want the error and nothing held", held, err)
	}
	h.o.mu.Lock()
	defer h.o.mu.Unlock()
	if len(h.o.syncing) != 0 {
		t.Fatalf("syncing = %v, want nothing marked", h.o.syncing)
	}
}
