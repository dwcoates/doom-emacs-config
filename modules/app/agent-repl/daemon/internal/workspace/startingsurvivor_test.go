package workspace

import (
	"context"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimsocket"
)

// TestASpawnRecordsItsPidAtTheFork pins the invariant every other test here
// rests on: the pid of a shim this bring-up forked is durable in the registry
// from the instant of the fork, not from a started session. A registered
// workspace has no session row at all when its first shim is forked, so
// nothing else in the state root could hold it.
func TestASpawnRecordsItsPidAtTheFork(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	record, err := f.db.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if record.SpawnedShimPID == nil || *record.SpawnedShimPID != f.client.pid {
		t.Fatalf("recorded spawn pid = %v, want the fork's %d", record.SpawnedShimPID, f.client.pid)
	}
}

// TestAFailedStartKeepsItsRecordedSpawn: a failed start's shim stays held and
// alive, so the pid made durable at its fork still names it, and a successor
// WAITS for that shim instead of spawning over it.
func TestAFailedStartKeepsItsRecordedSpawn(t *testing.T) {
	// Arrange.
	f := arrangeFailedStart(t)
	ws := f.workspace("w1")
	f.client.response = vendorStartFailed()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err == nil {
		t.Fatal("Start = nil, want the arranged start failure")
	}

	// Assert.
	record, err := f.db.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	// The held shim is alive and this daemon's: the record still names it,
	// so a successor waits for it rather than spawning over it.
	if record.SpawnedShimPID == nil || *record.SpawnedShimPID != f.client.pid {
		t.Fatalf("recorded spawn pid = %v, want the held shim's %d", record.SpawnedShimPID, f.client.pid)
	}
}

// TestABringUpWaitsForAShimAPredecessorSpawned is the bring-up's half of the
// starting-shim invariant, and the three outcomes the recorded pid can have.
//
// A free lock with nothing listening is what a client-less workspace looks
// like, and it is ALSO what a shim forked tens of milliseconds ago looks like:
// the shim takes its conversation locks inside StartSession and Node binds its
// socket later still. A bring-up that read the second as the first spawned a
// SECOND shim onto one session socket, which the shim refuses at bind.
func TestABringUpWaitsForAShimAPredecessorSpawned(t *testing.T) {
	tests := []struct {
		name string
		// arrange
		recordPID bool
		alive     bool
		// bindsAfter is how many socket probes read absent before the shim
		// announces itself; -1 never binds.
		bindsAfter int
		// assert
		wantAdopted bool
		wantSpawned bool
		wantErr     bool
	}{
		{
			name:        "a live recorded spawn that announces itself is adopted, not spawned over",
			recordPID:   true,
			alive:       true,
			bindsAfter:  2,
			wantAdopted: true,
		},
		{
			name:        "a recorded spawn whose process is dead is spawned over",
			recordPID:   true,
			alive:       false,
			bindsAfter:  -1,
			wantSpawned: true,
		},
		{
			name:        "no recorded spawn at all is the ordinary spawn",
			recordPID:   false,
			alive:       true,
			bindsAfter:  -1,
			wantSpawned: true,
		},
		{
			name:       "a live recorded spawn that never announces itself refuses rather than spawning",
			recordPID:  true,
			alive:      true,
			bindsAfter: -1,
			wantErr:    true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			const recordedPID = 4242
			f := newFleetFixtureBoundedAt(t, 200*time.Millisecond)
			f.probeState = sessionlock.StateFree
			f.socketState = shimsocket.StateAbsent
			f.shimAlive = func(int) bool { return tt.alive }
			probes := 0
			f.onSocketProbe = func(string) {
				probes++
				if tt.bindsAfter >= 0 && probes > tt.bindsAfter {
					f.socketState = shimsocket.StateLive
				}
			}
			ws := f.workspace("w1")
			if tt.recordPID {
				pid := recordedPID
				if err := f.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
					t.Fatalf("SetSpawnedShimPID: %v", err)
				}
			}

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if (err != nil) != tt.wantErr {
				t.Fatalf("Start = %v, want an error: %v", err, tt.wantErr)
			}
			if got := len(f.supervisor.adopts) > 0; got != tt.wantAdopted {
				t.Fatalf("adoptions = %v, want %v (adopted paths %v)", got, tt.wantAdopted, f.supervisor.adopts)
			}
			if got := len(f.supervisor.spawns) > 0; got != tt.wantSpawned {
				t.Fatalf("spawns = %v, want %v", got, tt.wantSpawned)
			}
		})
	}
}

// TestAStartingShimThatNeverAnnouncedItselfIsRecordedAtError pins the one loud
// record of the bring-up's wait. The other outcomes are the ordinary course of
// a bring-up and stay at INFO.
func TestAStartingShimThatNeverAnnouncedItselfIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 200*time.Millisecond)
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateAbsent
	f.shimAlive = func(int) bool { return true }
	ws := f.workspace("w1")
	pid := 4242
	if err := f.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err == nil {
		t.Fatal("Start = nil, want the undetermined starting shim to refuse the bring-up")
	}

	// Assert.
	var loud bool
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" && strings.Contains(r.Message, "never announced itself") {
			loud = true
		}
	}
	if !loud {
		t.Fatalf("no error record named the starting shim that never announced itself: %v", f.log.logger.Records())
	}
}

// TestAWaitedStartingShimStatesBothHalvesAtInfo pins the two INFO records the
// wait owes a reader: that it is waiting, with the pid and the bound, and that
// the shim announced itself. Without them an adoption that paid the bound
// reads on disk as an unexplained pause.
func TestAWaitedStartingShimStatesBothHalvesAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 200*time.Millisecond)
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateAbsent
	f.shimAlive = func(int) bool { return true }
	probes := 0
	f.onSocketProbe = func(string) {
		probes++
		if probes > 1 {
			f.socketState = shimsocket.StateLive
		}
	}
	ws := f.workspace("w1")
	pid := 4242
	if err := f.db.SetSpawnedShimPID(context.Background(), ws.ID, &pid); err != nil {
		t.Fatalf("SetSpawnedShimPID: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	var waited, announced bool
	for _, r := range f.log.logger.Records() {
		if r.Level != "info" {
			continue
		}
		if strings.Contains(r.Message, "still starting") {
			waited = true
		}
		if strings.Contains(r.Message, "announced itself") {
			announced = true
		}
	}
	if !waited || !announced {
		t.Fatalf("waited=%v announced=%v, want both stated at info: %v", waited, announced, f.log.logger.Records())
	}
}
