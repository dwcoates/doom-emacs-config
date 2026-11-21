package workspace

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/shimsocket"
)

// TestASpawnRecordsItsIdentityAtTheForkAndRetiresItAtTheAnnouncement pins
// the invariant every other test here rests on: the identity of a shim this
// bring-up forked is durable in the registry from the instant of the fork
// until the shim announces itself, and no longer. A registered workspace has
// no session row at all when its first shim is forked, so nothing else in the
// state root could hold it; a record left past the announcement outlives the
// shim and names whatever process the kernel hands its pid to next.
func TestASpawnRecordsItsIdentityAtTheForkAndRetiresItAtTheAnnouncement(t *testing.T) {
	tests := []struct {
		name string
		// arrange
		startFails bool
		// assert
		wantWrites []string
	}{
		{name: "a started session's spawn is recorded at the fork and retired at the announcement",
			wantWrites: []string{"recorded 4242", "retired 4242"}},
		{name: "a start that fails after its shim announced itself still retires the record: the live socket is its trace",
			startFails: true, wantWrites: []string{"recorded 4242", "retired 4242"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			if tt.startFails {
				f = arrangeFailedStart(t)
				f.client.response = vendorStartFailed()
			}
			f.client.pid = 4242
			ws := f.workspace("w1")

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if (err != nil) != tt.startFails {
				t.Fatalf("Start = %v, want an error: %v", err, tt.startFails)
			}
			if !slices.Equal(f.db.spawnWrites, tt.wantWrites) {
				t.Fatalf("spawn writes = %v, want %v", f.db.spawnWrites, tt.wantWrites)
			}
			record, err := f.db.Workspace(context.Background(), ws.ID)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}
			if record.SpawnedShim != nil {
				t.Fatalf("recorded spawn = %v, want none once the shim announced itself", *record.SpawnedShim)
			}
		})
	}
}

// TestASpawnRecordThatCannotBeRetiredIsRecordedAtError pins the failed
// retirement's one loud record: the stale record would wait out the next
// bring-up's bound for the shim, so the failure must be on the shared log.
func TestASpawnRecordThatCannotBeRetiredIsRecordedAtError(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	f.client.pid = 4242
	f.db.retireErr = errors.New("database is locked")
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{
		Level: "error", Operation: opBringUp, Message: "could not retire the record of a spawned shim that announced itself"})
	if !ok {
		t.Fatalf("no error record of the failed retirement: %v", f.log.logger.Records())
	}
	if r.Context["shim_pid"] != 4242 || r.Context["cause"] == nil {
		t.Fatalf("retirement record context = %v, want the pid and the cause", r.Context)
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
			f.shimAlive = func(recorded ids.ProcessIdentity) bool {
				if recorded != fakeSpawn(recordedPID) {
					t.Fatalf("liveness asked about %v, want the recorded %v", recorded, fakeSpawn(recordedPID))
				}
				return tt.alive
			}
			// Each bind the test models is a change of the socket's
			// directory, which the wait probes again on.
			if tt.bindsAfter >= 0 {
				// The adoption bound is held, so the wait ends on the bind.
				f.clock.held = map[time.Duration]bool{200 * time.Millisecond: true}
				f.socketChanges = make(chan error, tt.bindsAfter+1)
				for range tt.bindsAfter + 1 {
					f.socketChanges <- nil
				}
			}
			probes := 0
			f.onSocketProbe = func(string) {
				probes++
				if tt.bindsAfter >= 0 && probes > tt.bindsAfter {
					f.socketState = shimsocket.StateLive
				}
			}
			ws := f.workspace("w1")
			if tt.recordPID {
				spawn := fakeSpawn(recordedPID)
				if err := f.db.SetSpawnedShim(context.Background(), ws.ID, &spawn); err != nil {
					t.Fatalf("SetSpawnedShim: %v", err)
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
	f.shimAlive = func(ids.ProcessIdentity) bool { return true }
	ws := f.workspace("w1")
	spawn := fakeSpawn(4242)
	if err := f.db.SetSpawnedShim(context.Background(), ws.ID, &spawn); err != nil {
		t.Fatalf("SetSpawnedShim: %v", err)
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
	f.shimAlive = func(ids.ProcessIdentity) bool { return true }
	probes := 0
	f.onSocketProbe = func(string) {
		probes++
		if probes > 1 {
			f.socketState = shimsocket.StateLive
		}
	}
	ws := f.workspace("w1")
	spawn := fakeSpawn(4242)
	if err := f.db.SetSpawnedShim(context.Background(), ws.ID, &spawn); err != nil {
		t.Fatalf("SetSpawnedShim: %v", err)
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

// TestARecordedSpawnWhoseLivenessCannotBeReadLeavesTheWorkspaceUndetermined
// pins the unreadable answer: a pid the kernel will not stamp is neither the
// recorded shim nor gone, so the bring-up spawns nothing and says why at
// ERROR.
func TestARecordedSpawnWhoseLivenessCannotBeReadLeavesTheWorkspaceUndetermined(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 200*time.Millisecond)
	f.probeState = sessionlock.StateFree
	f.socketState = shimsocket.StateAbsent
	f.shimAliveErr = errors.New("sysctl refused")
	ws := f.workspace("w1")
	spawn := fakeSpawn(4242)
	if err := f.db.SetSpawnedShim(context.Background(), ws.ID, &spawn); err != nil {
		t.Fatalf("SetSpawnedShim: %v", err)
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "sysctl refused") {
		t.Fatalf("Start = %v, want the unreadable liveness", err)
	}
	if len(f.supervisor.spawns) != 0 {
		t.Fatalf("spawns = %d, want none onto an undetermined workspace", len(f.supervisor.spawns))
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{
		Level: "error", Operation: opBringUp, Message: "could not read whether the recorded spawn's process still runs; the workspace is left undetermined"})
	if !ok || r.Context["shim_pid"] != 4242 || r.Context["cause"] != "sysctl refused" {
		t.Fatalf("no error record naming the pid and cause: %v", f.log.logger.Records())
	}
}
