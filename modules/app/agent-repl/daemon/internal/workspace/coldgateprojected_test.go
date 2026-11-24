package workspace

import (
	"context"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// projectedCold is the gate a shim projected for vendor-1, with no model.
func projectedCold() *conversationv1.SessionCold {
	return &conversationv1.SessionCold{
		ContextTokens: 120_000, LastRequestAtMs: 1_800_000,
		Reason: &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: 3_600_000}},
	}
}

func TestARaisedProjectedGateStandsRecordedWithTheSessionsModel(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	session := f.db.sessions[ws]
	session.Model = "claude-opus-5-5"
	f.db.sessions[ws] = session
	var told []ColdGateRaise
	f.fleet.deps.ColdGateRaised = func(_ ids.WorkspaceID, raise ColdGateRaise) { told = append(told, raise) }

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	want := proto.Clone(projectedCold()).(*conversationv1.SessionCold)
	want.RequestedModel = &conversationv1.AgentModel{Name: "claude-opus-5-5"}
	if err != nil || !raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want the gate raised", raised, err)
	}
	if rec, ok := f.db.gates[ws]; !ok || rec.VendorSessionID != "vendor-1" || !proto.Equal(rec.Cold, want) {
		t.Fatalf("recorded gate = %+v (%v), want vendor-1 with the projected facts and the session's model", rec, ok)
	}
	if served, ok := f.fleet.ColdGate(ws); !ok || served.VendorSessionID != "vendor-1" {
		t.Fatalf("ColdGate = %+v, %v; want the gate standing", served, ok)
	}
	if !f.footer.coldGates[ws].Standing {
		t.Fatal("the footer draws no gate, want the raised gate drawn")
	}
	if len(told) != 1 || !told[0].Judged() {
		t.Fatalf("raises told to the sinks = %v, want one judged raise", told)
	}
}

func TestARaisedProjectedGateSpawnsNoShim(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)

	// Act
	_, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	if err != nil || f.supervisor.spawnAttempts != 0 {
		t.Fatalf("RaiseProjectedGate = %v after %d spawn attempts, want a raise with nothing spawned", err, f.supervisor.spawnAttempts)
	}
}

func TestAProjectedGateIsNotRaisedOverAStandingGate(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := parkAtColdGate(t, f)
	before := f.db.gates[ws]

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	if err != nil || raised || !proto.Equal(f.db.gates[ws].Cold, before.Cold) {
		t.Fatalf("RaiseProjectedGate = %v, %v; want no raise and the standing gate untouched", raised, err)
	}
}

func TestAProjectedGateIsNotRaisedWithNoConversationToResume(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture) ids.WorkspaceID
	}{
		{name: "no session recorded", arrange: func(f *fleetFixture) ids.WorkspaceID { return f.workspace("w1").ID }},
		{name: "a session with no conversation", arrange: func(f *fleetFixture) ids.WorkspaceID {
			ws := f.workspace("w1")
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}
			return ws.ID
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := tt.arrange(f)

			// Act
			raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

			// Assert
			if err != nil || raised {
				t.Fatalf("RaiseProjectedGate = %v, %v; want no raise and no error", raised, err)
			}
			if _, ok := f.db.gates[ws]; ok {
				t.Fatal("a gate was recorded, want none")
			}
		})
	}
}

func TestAProjectedGateWhoseReadsFailIsRefusedAndLogged(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture)
		message string
	}{
		{name: "the session read", arrange: func(f *fleetFixture) { f.db.sessionErr = errFake },
			message: "could not read the session a projected cold gate stands over; the gate is not raised"},
		{name: "the workspace read", arrange: func(f *fleetFixture) { f.db.workspaceErr = errFake },
			message: "could not read the workspace a projected cold gate stands over; the gate is not raised"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := resumable(f)
			tt.arrange(f)

			// Act
			raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

			// Assert
			if err == nil || raised {
				t.Fatalf("RaiseProjectedGate = %v, %v; want the failed read surfaced", raised, err)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "error", Operation: opColdGate, Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want the failed read at ERROR", f.log.logger.Records())
			}
			if _, ok := f.fleet.ColdGate(ws); ok {
				t.Fatal("a gate stands, want none after a failed read")
			}
		})
	}
}

// projectedPayment is the payment for the read projectedCold names on
// vendor-1, priced for MODEL.
func projectedPayment(model string) wsm.ColdReadPayment {
	return wsm.ColdReadPayment{VendorSessionID: "vendor-1", LastRequestAtMs: projectedCold().GetLastRequestAtMs(), Model: model, PaidAt: fixedNow}
}

func TestAProjectedGateIsNotRaisedForTheReadTheUserPaidFor(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	session := f.db.sessions[ws]
	session.Model = "opus"
	f.db.sessions[ws] = session
	if err := f.db.PutColdReadPayment(context.Background(), ws, projectedPayment("opus")); err != nil {
		t.Fatalf("seed the payment: %v", err)
	}

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	if err != nil || raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want no raise for a paid read", raised, err)
	}
	if _, ok := f.fleet.ColdGate(ws); ok {
		t.Fatal("a gate stands over the read the user already paid for")
	}
	if _, ok := f.db.gates[ws]; ok {
		t.Fatal("a gate was recorded over the read the user already paid for")
	}
	if got, ok := f.db.coldPayment(ws); !ok || got != projectedPayment("opus") {
		t.Fatalf("payment = %+v (found=%v), want it kept for the read it pays for", got, ok)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opColdGate,
		Message: "the projected cold read is the one the user already paid for; no gate is raised"})
	if !ok || record.Context["vendor_session_id"] != "vendor-1" || record.Context["last_request_at_ms"] != projectedCold().GetLastRequestAtMs() {
		t.Fatalf("records = %+v, want the declined raise naming the paid read", f.log.logger.Records())
	}
}

func TestAProjectedGateIsRaisedOverAPaymentForADifferentRead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	session := f.db.sessions[ws]
	session.Model = "opus"
	f.db.sessions[ws] = session
	stale := projectedPayment("opus")
	stale.LastRequestAtMs--
	if err := f.db.PutColdReadPayment(context.Background(), ws, stale); err != nil {
		t.Fatalf("seed the payment: %v", err)
	}

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	if err != nil || !raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want the gate raised for a read nobody paid for", raised, err)
	}
	if _, ok := f.db.coldPayment(ws); ok {
		t.Fatal("the spent payment still stands, want it dropped")
	}
}

func TestAProjectedGateWhosePaymentCannotBeReadIsRefused(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.coldPaymentOfErr = errFake

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())

	// Assert
	if err == nil || raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want the failed payment read surfaced", raised, err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opColdReadPayment,
		Message: "the cold read payment could not be read; the bring-up is refused"}); !ok {
		t.Fatalf("records = %+v, want the failed payment read recorded", f.log.logger.Records())
	}
	if _, ok := f.fleet.ColdGate(ws); ok {
		t.Fatal("a gate stands, want none after a failed payment read")
	}
}

func TestAProjectedGateInRemoteControlWithNoShimSpawnsTheShimThatHoldsItsBridge(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())
	f.client.bridgeFrames <- openedFrame("bridge-1")
	settled(t, f)

	// Assert
	if err != nil || !raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want the gate raised", raised, err)
	}
	if len(f.supervisor.spawns) != 1 || len(f.client.bridges) != 1 || len(f.client.requests) != 0 {
		t.Fatalf("spawns %d, bridges %d, starts %d; want one shim spawned for the bridge and no session started",
			len(f.supervisor.spawns), len(f.client.bridges), len(f.client.requests))
	}
}

func TestAProjectedGateOverARunningSessionInRemoteControlLeavesTheBridgeToItsPark(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	spawned := len(f.supervisor.spawns)

	// Act
	raised, err := f.fleet.RaiseProjectedGate(context.Background(), ws, projectedCold())
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the fleet's detached work did not finish")
	}

	// Assert
	if err != nil || !raised {
		t.Fatalf("RaiseProjectedGate = %v, %v; want the gate raised", raised, err)
	}
	if len(f.supervisor.spawns) != spawned || len(f.client.bridges) != 0 {
		t.Fatalf("spawns %d (were %d), bridges %d; want no shim spawned beside the running session", len(f.supervisor.spawns), spawned, len(f.client.bridges))
	}
	if _, ok := bridgeRecord(f, "info", "the gate stands over a session still running; its park bridges the gate"); !ok {
		t.Fatalf("records = %+v, want the bridge left to the park", f.log.logger.Records())
	}
	if _, ok := bridgeRecord(f, "error", "could not spawn the shim that holds the parked workspace's bridge"); ok {
		t.Fatal("the bridge was attempted beside the running session")
	}
}
