package workspace

import (
	"context"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// recordedGate is the gate a shim judged for vendor-1, as recorded.
func recordedGate() wsm.ColdGateRecord {
	return wsm.ColdGateRecord{
		VendorSessionID: "vendor-1",
		Cold:            coldResponse().GetFailure().GetCold(),
		ConfigDir:       "/accounts/work",
	}
}

func TestAColdRefusalRecordsTheGate(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	rec, ok := f.db.gates[ws]
	if !ok || rec.VendorSessionID != "vendor-1" || !proto.Equal(rec.Cold, coldResponse().GetFailure().GetCold()) {
		t.Fatalf("recorded gate = %+v (%v), want the shim's gate on vendor-1", rec, ok)
	}
}

func TestAColdRefusalStandsTheShimDownAndKeepsTheGate(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if f.fleet.Held(ws) || len(f.client.kills) != 1 || !strings.Contains(f.client.kills[0].Reason, "cold gate") {
		t.Fatalf("held %v after kills %+v, want the gate's shim killed", f.fleet.Held(ws), f.client.kills)
	}
	if served, ok := f.fleet.ColdGate(ws); !ok || served.VendorSessionID != "vendor-1" {
		t.Fatalf("ColdGate = %+v, %v; want the gate standing on vendor-1", served, ok)
	}
	if !f.fleet.Live(ws) {
		t.Fatal("Live = false, want a standing gate live with no shim")
	}
}

func TestAColdGateWhoseRecordWillNotWriteStillStands(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.client.response = coldResponse()
	f.db.setGateErr = errFake

	// Act.
	_ = f.fleet.Start(context.Background(), ws)

	// Assert.
	if _, ok := f.fleet.ColdGate(ws); !ok {
		t.Fatal("no gate stands, want it standing for this daemon")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opColdGate,
		Message: "could not record the cold gate; it stands for this daemon only, and a later start asks the shim again"}); !ok {
		t.Fatalf("records = %+v, want the failed record at ERROR", f.log.logger.Records())
	}
}

func TestAStartOfARecordedGateRaisesItAndSpawnsNothing(t *testing.T) {
	// Arrange: a gate recorded before this daemon, with no shim behind it.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gates[ws] = recordedGate()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 0 || f.supervisor.spawnAttempts != 0 {
		t.Fatalf("spawn attempts = %d, want none for a recorded gate", f.supervisor.spawnAttempts)
	}
	if served, ok := f.fleet.ColdGate(ws); !ok || served.VendorSessionID != "vendor-1" {
		t.Fatalf("ColdGate = %+v, %v; want the recorded gate standing", served, ok)
	}
	if !f.footer.coldGates[ws].Standing {
		t.Fatal("the footer draws no gate, want the recorded gate drawn")
	}
}

func TestAStartWhoseRecordedGateCannotBeReadSpawnsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gateErr = errFake

	// Act.
	err := f.fleet.Start(context.Background(), ws)

	// Assert.
	if err == nil || f.supervisor.spawnAttempts != 0 {
		t.Fatalf("Start = %v after %d spawn attempts, want the failed read surfaced and nothing spawned", err, f.supervisor.spawnAttempts)
	}
}

func TestAReboundStartRetiresTheRecordedGateAndStarts(t *testing.T) {
	// Arrange: the workspace is pointed at another conversation.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gates[ws] = recordedGate()

	// Act.
	if err := f.fleet.StartRebound(context.Background(), ws); err != nil {
		t.Fatalf("StartRebound: %v", err)
	}

	// Assert.
	if _, ok := f.db.gates[ws]; ok {
		t.Fatal("the recorded gate stands, want it retired")
	}
	if len(f.supervisor.spawns) != 1 {
		t.Fatalf("spawns = %d, want the rebound session started", len(f.supervisor.spawns))
	}
}

func TestAnAnsweredGatesSessionRetiresItsRecord(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	if _, ok := f.db.gates[ws.ID]; !ok {
		t.Fatal("no gate was recorded, so this test could not tell the retirement apart")
	}

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if _, ok := f.db.gates[ws.ID]; ok {
		t.Fatal("the recorded gate stands, want it retired once the session came up")
	}
}

func TestResumeColdOfAnotherConversationsGateIsRefused(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	spawned := len(f.supervisor.spawns)

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-other", Remediation: payRemediation()})

	// Assert.
	asRefusal(t, err, ArmNoSession)
	if len(f.supervisor.spawns) != spawned {
		t.Fatal("a shim was spawned for a gate that does not stand")
	}
}

// heldAtGate brings W1 to a standing gate whose shim is still held: the state
// between the gate's raise and its stand-down.
func heldAtGate(t *testing.T, f *fleetFixture) ids.WorkspaceID {
	t.Helper()
	ws := resumable(f)
	f.fleet.remember(ws, &live{client: f.client, hostSessionID: "host-1"})
	f.fleet.raiseColdGate(ws, "vendor-1", coldResponse().GetFailure().GetCold(), "/accounts/work", true)
	return ws
}

func TestStandDownAtGate(t *testing.T) {
	tests := []struct {
		name     string
		arrange  func(f *fleetFixture, ws ids.WorkspaceID)
		wantHeld bool
	}{
		{name: "stands an unanswered gate's idle shim down", arrange: func(*fleetFixture, ids.WorkspaceID) {}},
		{name: "leaves an answered gate's shim for its resume", arrange: func(f *fleetFixture, ws ids.WorkspaceID) {
			f.fleet.TakeColdGate(ws, "vendor-1")
		}, wantHeld: true},
		{name: "leaves a shim a session runs on", arrange: func(f *fleetFixture, ws ids.WorkspaceID) {
			f.fleet.noteSessionStarted(ws)
		}, wantHeld: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := heldAtGate(t, f)
			tt.arrange(f, ws)

			// Act.
			f.fleet.standDownAtGate(context.Background(), ws)

			// Assert.
			if f.fleet.Held(ws) != tt.wantHeld {
				t.Fatalf("held = %v, want %v", f.fleet.Held(ws), tt.wantHeld)
			}
		})
	}
}

func TestAFailedAnswersGateStandsAgainWithNoShim(t *testing.T) {
	// Arrange: an answer spawned its shim, and its resume failed.
	f := newFleetFixture(t)
	ws := heldAtGate(t, f)
	f.fleet.TakeColdGate(ws, "vendor-1")

	// Act.
	stood := f.fleet.ReraiseColdGate(ws, "vendor-1")
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the gate's stand-down did not finish")
	}

	// Assert.
	if !stood || f.fleet.Held(ws) {
		t.Fatalf("re-raised %v, held %v; want the gate standing and its shim stood down", stood, f.fleet.Held(ws))
	}
}

func TestAStandingGateIsAQuietSessionWithNoShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gates[ws] = recordedGate()
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	running, live := f.fleet.Running(ws)
	exists, serving := f.fleet.Health(ws)

	// Assert.
	if !live || running.Turn != nil || !exists || serving {
		t.Fatalf("Running = %+v, %v; Health = %v, %v; want a quiet session that exists and does not serve", running, live, exists, serving)
	}
}
