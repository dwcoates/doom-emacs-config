package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/netstate"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// remoteControlFailure is a shim refusal carrying CAUSE.
func remoteControlFailure(failure *shimv1.SetSessionRemoteControlFailure) *shimv1.SetSessionRemoteControlResponse {
	return &shimv1.SetSessionRemoteControlResponse{Result: &shimv1.SetSessionRemoteControlResponse_Failure{Failure: failure}}
}

// vendorRefusedRemoteControl is the vendor's refusal of the bridge.
func vendorRefusedRemoteControl() *shimv1.SetSessionRemoteControlResponse {
	return remoteControlFailure(&shimv1.SetSessionRemoteControlFailure{
		Cause:  &shimv1.SetSessionRemoteControlFailure_VendorRefused{VendorRefused: &shimv1.SetSessionRemoteControlVendorRefused{}},
		Detail: "remote control is disabled by policy",
	})
}

// startedFleet is the fleet fixture with workspace w1's session up.
func startedFleet(t *testing.T) (*fleetFixture, wsm.Workspace) {
	t.Helper()
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	return f, ws
}

func TestRemoteControlArmNamesEachShimRefusal(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.SetSessionRemoteControlFailure
		want    string
	}{
		{"no_session", &shimv1.SetSessionRemoteControlFailure{Cause: &shimv1.SetSessionRemoteControlFailure_NoSession{NoSession: &shimv1.SetSessionRemoteControlNoSession{}}}, ArmShimNoSession},
		{"vendor_refused", &shimv1.SetSessionRemoteControlFailure{Cause: &shimv1.SetSessionRemoteControlFailure_VendorRefused{VendorRefused: &shimv1.SetSessionRemoteControlVendorRefused{}}}, ArmShimVendorRefused},
		{"unset", &shimv1.SetSessionRemoteControlFailure{}, ArmShimUnspecified},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table. Act.
			got := remoteControlArm(tt.failure)

			// Assert.
			if got != tt.want {
				t.Errorf("remoteControlArm = %q, want %q", got, tt.want)
			}
		})
	}
}

// ---- Fleet.SetRemoteControl: a change ----

func TestFleetSetRemoteControlRecordsTheModeWithNoShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	on, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	var refusal *ShimRefusal
	if !on || !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
		t.Fatalf("SetRemoteControl = (%v, %v), want on and the no_session refusal", on, err)
	}
	if got := f.db.remoteOf(ws.ID); !got.On || !got.Recorded {
		t.Fatalf("recorded = %+v, want on", got)
	}
}

func TestFleetSetRemoteControlStatesTheModeToTheTopbar(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	_, _ = f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlToggle)

	// Assert.
	if len(f.topbarRemote) != 1 || !f.topbarRemote[0] {
		t.Fatalf("topbar modes = %v, want the toggled on", f.topbarRemote)
	}
}

func TestFleetSetRemoteControlTurnsTheLiveShimsBridgeOn(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)

	// Act.
	on, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if err != nil || !on {
		t.Fatalf("SetRemoteControl = (%v, %v), want on", on, err)
	}
	if n := len(f.client.remoteControls); n != 1 {
		t.Fatalf("SetSessionRemoteControl asks = %v, want one", f.client.remoteControls)
	}
	if req := f.client.remoteControls[0]; !req.GetEnabled() || req.GetName() != ws.Name || req.ReattachBridgeSessionId != nil {
		t.Fatalf("ask = %v, want on under the workspace's name, re-binding nothing", req)
	}
}

func TestFleetSetRemoteControlKeepsTheBridgeSession(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)

	// Act.
	_, _ = f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if got := f.db.remoteOf(ws.ID).BridgeSessionID; got != "bridge-new" {
		t.Fatalf("kept bridge = %q, want the shim's bridge-new", got)
	}
}

func TestFleetSetRemoteControlReBindsTheKeptBridgeSession(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.setRemote(ws.ID, wsm.RemoteControl{BridgeSessionID: "bridge-kept"})

	// Act.
	_, _ = f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if got := f.client.remoteControls[0].GetReattachBridgeSessionId(); got != "bridge-kept" {
		t.Fatalf("re-bind = %q, want the kept bridge", got)
	}
}

func TestFleetSetRemoteControlTurnsTheLiveShimsBridgeOff(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.setRemote(ws.ID, wsm.RemoteControl{BridgeSessionID: "bridge-kept"})

	// Act.
	on, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOff)

	// Assert.
	if err != nil || on {
		t.Fatalf("SetRemoteControl = (%v, %v), want off", on, err)
	}
	if req := f.client.remoteControls[0]; req.GetEnabled() || req.GetName() != "" || req.ReattachBridgeSessionId != nil {
		t.Fatalf("ask = %v, want a bare off", req)
	}
	if got := f.db.remoteOf(ws.ID).BridgeSessionID; got != "bridge-kept" {
		t.Fatalf("kept bridge = %q, want it kept for a later on", got)
	}
}

func TestFleetSetRemoteControlCarriesTheVendorsRefusal(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	on, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	var refusal *ShimRefusal
	if !on || !errors.As(err, &refusal) || refusal.Arm != ArmShimVendorRefused || refusal.Detail != "remote control is disabled by policy" {
		t.Fatalf("SetRemoteControl = (%v, %v), want on and the vendor's refusal with its detail", on, err)
	}
}

func TestFleetSetRemoteControlKeepsTheModeTheVendorRefused(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	_, _ = f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if got := f.db.remoteOf(ws.ID); !got.On || !got.Recorded {
		t.Fatalf("recorded = %+v, want on recorded as asked", got)
	}
}

func TestFleetSetRemoteControlStatesTheVendorsRefusalLoudly(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	_, _ = f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "disabled by policy") {
		t.Errorf("warnings = %v, want one carrying the vendor's detail", f.topbarWarnings)
	}
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 1 {
		t.Errorf("error records = %v, want one", records)
	}
}

func TestFleetSetRemoteControlLeavesAShimWithNoSessionToItsStart(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlResponse = remoteControlFailure(&shimv1.SetSessionRemoteControlFailure{
		Cause: &shimv1.SetSessionRemoteControlFailure_NoSession{NoSession: &shimv1.SetSessionRemoteControlNoSession{}},
	})

	// Act.
	_, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession || len(f.topbarWarnings) != 0 {
		t.Fatalf("SetRemoteControl = %v, warnings %v; want no_session and no warning", err, f.topbarWarnings)
	}
}

func TestFleetSetRemoteControlSendsNothingWhenTheModeCannotBeRecorded(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.remoteSetErr = errors.New("disk full")

	// Act.
	_, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	if err == nil || len(f.client.remoteControls) != 0 || len(f.topbarRemote) != 0 {
		t.Fatalf("SetRemoteControl = %v, asks %v, topbar %v; want the failure and nothing sent or drawn", err, f.client.remoteControls, f.topbarRemote)
	}
}

// ---- applyRemoteControl: the shim's answer ----

func TestApplyRemoteControlRefusesABrokenSuccess(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.SetSessionRemoteControlResponse
		want     string
	}{
		{"neither arm", &shimv1.SetSessionRemoteControlResponse{}, "neither a success nor a failure"},
		{"the bridge left off", &shimv1.SetSessionRemoteControlResponse{Result: &shimv1.SetSessionRemoteControlResponse_Success{
			Success: &shimv1.SetSessionRemoteControlSuccess{Enabled: false},
		}}, "asked on"},
		{"no bridge session", &shimv1.SetSessionRemoteControlResponse{Result: &shimv1.SetSessionRemoteControlResponse_Success{
			Success: &shimv1.SetSessionRemoteControlSuccess{Enabled: true},
		}}, "no bridge session"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f, ws := startedFleet(t)
			f.client.remoteControlResponse = tt.response

			// Act.
			err := f.fleet.applyRemoteControl(context.Background(), f.log.logger, ws.ID, f.client, true)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q named", err, tt.want)
			}
		})
	}
}

func TestApplyRemoteControlSurfacesATransportFailure(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlErr = errors.New("socket closed")

	// Act.
	err := f.fleet.applyRemoteControl(context.Background(), f.log.logger, ws.ID, f.client, true)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "socket closed") {
		t.Fatalf("err = %v, want the transport failure", err)
	}
}

func TestApplyRemoteControlOutlivesItsCallersCancel(t *testing.T) {
	// Arrange: a bring-up under an rpc whose context already ended.
	f, ws := startedFleet(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	var seen error
	f.client.remoteControlErr = nil
	probe := &cancelProbe{fakeClient: f.client, seen: &seen}

	// Act.
	err := f.fleet.applyRemoteControl(ctx, f.log.logger, ws.ID, probe, true)

	// Assert.
	if err != nil || seen != nil {
		t.Fatalf("applyRemoteControl = %v, the shim call saw %v; want it bounded but not cut by the caller", err, seen)
	}
}

// cancelProbe records the context error SetSessionRemoteControl was called
// under.
type cancelProbe struct {
	*fakeClient
	seen *error
}

func (p *cancelProbe) SetSessionRemoteControl(ctx context.Context, req *shimv1.SetSessionRemoteControlRequest) (*shimv1.SetSessionRemoteControlResponse, error) {
	*p.seen = ctx.Err()
	return p.fakeClient.SetSessionRemoteControl(ctx, req)
}

func TestABridgeSessionThatCannotBeKeptIsNoFailedBridge(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.bridgeErr = errors.New("disk full")

	// Act.
	err := f.fleet.applyRemoteControl(context.Background(), f.log.logger, ws.ID, f.client, true)

	// Assert.
	if err != nil {
		t.Fatalf("err = %v, want the bridge on whatever the record did", err)
	}
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 1 || records[0]["cause"] != "disk full" {
		t.Errorf("error records = %v, want one carrying the cause", records)
	}
}

// ---- reapplyRemoteControl: every bring-up ----

func TestANewShimIsPutInRemoteControlWhenTheModeIsOn(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true, BridgeSessionID: "bridge-kept"})

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.remoteControls) != 1 || !f.client.remoteControls[0].GetEnabled() ||
		f.client.remoteControls[0].GetReattachBridgeSessionId() != "bridge-kept" {
		t.Fatalf("SetSessionRemoteControl asks = %v, want one on re-binding the kept bridge", f.client.remoteControls)
	}
}

func TestARemoteControlPutBackOutlivesItsCallersCancel(t *testing.T) {
	// Arrange: the bring-up's caller is gone by the time the put-back runs
	// (2026-11-17 13:43, store-peer-messages-timeout).
	f, ws := startedFleet(t)
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.client.remoteControls = nil
	var seen error
	probe := &cancelProbe{fakeClient: f.client, seen: &seen}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	f.fleet.reapplyRemoteControl(ctx, f.log.logger, ws.ID, probe)

	// Assert.
	f.db.remoteMu.Lock()
	reads := slices.Clone(f.db.remoteReadCtxErr)
	f.db.remoteMu.Unlock()
	if len(reads) == 0 || reads[len(reads)-1] != nil {
		t.Fatalf("mode reads saw %v, want the last one uncut by the caller", reads)
	}
	if seen != nil || len(f.client.remoteControls) != 1 {
		t.Fatalf("the shim call saw %v with asks %v, want one uncut put-back", seen, f.client.remoteControls)
	}
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 0 {
		t.Errorf("error records = %v, want none", records)
	}
}

func TestANewShimIsAskedNothingWhenTheModeIsOff(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.remoteControls) != 0 {
		t.Fatalf("SetSessionRemoteControl asks = %v, want none with the mode off", f.client.remoteControls)
	}
}

func TestANewShimIsPutInRemoteControlAfterTheSessionIsUp(t *testing.T) {
	// Arrange: a held prompt is never kept waiting on the bridge.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	upFirst := false
	f.client.remoteControlErr = nil
	f.client.onRemoteControl = func() { upFirst = len(f.sessionsUp) == 1 }

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !upFirst {
		t.Fatal("the bridge was asked for before the session was called up")
	}
}

func TestAFailedRemoteControlPutBackIsLoggedAndStatedOnTheStrip(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert: the session still comes up; the failure is loud, not fatal.
	if err != nil {
		t.Fatalf("Start: %v, want the session up", err)
	}
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "claude.ai") {
		t.Errorf("warnings = %v, want one naming claude.ai", f.topbarWarnings)
	}
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 1 {
		t.Errorf("error records = %v, want one", records)
	}
}

func TestABridgeTheNetworkRefusedIsOwedUntilTheNetworkIsRestored(t *testing.T) {
	// Arrange: the vendor refuses the new shim's bridge while the network is
	// down (2026-11-17 13:39:16, eval-cache-torch-human).
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.network.down(netstate.EvidenceFaultOpen)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()
	asked := make(chan struct{}, 2)
	calls := 0
	f.client.onRemoteControl = func() {
		// THE PUT-BACK AFTER THE RESTORATION IS ANSWERED: the vendor reaches
		// claude.ai again.
		if calls++; calls == 2 {
			f.client.remoteControlResponse = nil
		}
		asked <- struct{}{}
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	<-asked
	f.network.restore()
	select {
	case <-asked:
	case <-time.After(time.Minute):
		t.Fatal("the bridge was not asked again once the network was restored")
	}
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the owed put-back did not leave")
	}

	// Assert.
	t.Run("the refusal is recorded at INFO and marked transient", func(t *testing.T) {
		records := dlog.FilterRecords(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opRemoteControl,
			Message: "the vendor could not bridge the new shim while the network is down; it is asked again once the network is restored"})
		if len(records) != 1 || records[0].Context[dlog.TransientKey] != true || records[0].Context["evidence"] != string(netstate.EvidenceFaultOpen) {
			t.Fatalf("records = %+v, want one transient INFO naming its evidence", records)
		}
	})
	t.Run("nothing is recorded at ERROR or stated on the strip", func(t *testing.T) {
		if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 0 || len(f.topbarWarnings) != 0 {
			t.Fatalf("errors %v, warnings %v; want neither for a refusal the network caused", records, f.topbarWarnings)
		}
	})
	t.Run("the restoration puts the bridge in force", func(t *testing.T) {
		if len(f.client.remoteControls) != 2 || !f.client.remoteControls[1].GetEnabled() {
			t.Fatalf("asks = %v, want the refused one and its put-back", f.client.remoteControls)
		}
		if records := dlog.FilterRecords(f.log.logger.Records(), dlog.RecordQuery{Level: "info", Operation: opRemoteControl,
			Message: "put the new shim in remote control"}); len(records) != 1 {
			t.Fatalf("records = %+v, want the put-back recorded once", records)
		}
	})
}

func TestABridgeTheNetworkRefusedIsNotAskedAgainOnceTheDaemonStandsDown(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.network.down(netstate.EvidenceCLIWords)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	left := f.fleet.DrainStarts(time.Minute)

	// Assert.
	if !left || len(f.client.remoteControls) != 1 {
		t.Fatalf("left %v after asks %v, want the owed put-back gone with the daemon and nothing asked again", left, f.client.remoteControls)
	}
}

func TestABridgeRefusalThatWasNotTheNetworksIsAnError(t *testing.T) {
	// Arrange: the network is up, so the refusal is the vendor's own.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.network.classified) != 1 || !strings.Contains(f.network.classified[0], "remote control is disabled by policy") {
		t.Fatalf("classified %v, want the refusal's own words asked once", f.network.classified)
	}
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 1 {
		t.Fatalf("error records = %v, want the refusal at ERROR", records)
	}
}

func TestAShimGoneBeforeItsBridgeIsAskedIsNoFailure(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{"it died", fmt.Errorf("%w: unavailable", shimclient.ErrShimDied)},
		{"this daemon stood it down", shimclient.ErrStandDownOrdered},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
			f.client.remoteControlErr = tt.err

			// Act.
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert: the death is recorded where it is observed, once.
			if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 0 || len(f.topbarWarnings) != 0 {
				t.Fatalf("errors %v, warnings %v; want neither for a shim that went", records, f.topbarWarnings)
			}
		})
	}
}

func TestAShimWhoseSessionEndedBeforeItsBridgeIsAskedIsNoFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})
	f.client.remoteControlResponse = remoteControlFailure(&shimv1.SetSessionRemoteControlFailure{
		Cause: &shimv1.SetSessionRemoteControlFailure_NoSession{NoSession: &shimv1.SetSessionRemoteControlNoSession{}},
	})

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert: the session's end is recorded where it is observed, once.
	if records := recordsAt(f.log, opRemoteControl, "error"); len(records) != 0 || len(f.topbarWarnings) != 0 {
		t.Fatalf("errors %v, warnings %v; want neither for a session that ended", records, f.topbarWarnings)
	}
}

func TestAChangeWhoseShimDiedUnderItIsLeftToTheNextBringUp(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.client.remoteControlErr = fmt.Errorf("%w: unavailable", shimclient.ErrShimDied)

	// Act.
	_, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession || len(f.topbarWarnings) != 0 {
		t.Fatalf("SetRemoteControl = %v, warnings %v; want no_session and no warning", err, f.topbarWarnings)
	}
}

func TestAnUnreadableModeAtABringUpIsStatedOnTheStrip(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.remoteErr = errors.New("database is locked")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("Start: %v, want the session up", err)
	}
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "database is locked") {
		t.Errorf("warnings = %v, want one carrying the cause", f.topbarWarnings)
	}
}

func TestARelaunchedShimIsPutInRemoteControl(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true})

	// Act.
	if _, err := f.fleet.Resume(context.Background(), ws.ID, f.client); err != nil {
		t.Fatalf("Resume: %v", err)
	}

	// Assert.
	if len(f.client.remoteControls) != 1 || !f.client.remoteControls[0].GetEnabled() {
		t.Fatalf("SetSessionRemoteControl asks = %v, want one on", f.client.remoteControls)
	}
}

// ---- verbs.UpdateRemoteControlMode ----

func TestUpdateRemoteControlModeAsksTheFleetForTheChange(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.remoteOn = true

	// Act.
	on, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlToggle)

	// Assert.
	if err != nil || !on || len(f.fleet.remoteChanges) != 1 || f.fleet.remoteChanges[0] != wsm.RemoteControlToggle {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), fleet asks %v; want on from one toggle", on, err, f.fleet.remoteChanges)
	}
}

func TestUpdateRemoteControlModeTellsTheCacheKeeper(t *testing.T) {
	tests := []struct {
		name      string
		remoteErr error
	}{
		{name: "a mode the live shim took"},
		{name: "a mode recorded with no shim", remoteErr: &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.fleet.remoteErr = tt.remoteErr

			// Act.
			_, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlOff)

			// Assert.
			if err != nil || len(f.remoteEdges) != 1 || f.remoteEdges[0] != "w1" {
				t.Fatalf("err %v, remote-control edges %v; want one for w1", err, f.remoteEdges)
			}
		})
	}
}

func TestUpdateRemoteControlModeStartsAShimForAnOnWithNone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.remoteOn = true
	f.fleet.remoteErr = &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession}

	// Act.
	on, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlOn)

	// Assert.
	if err != nil || !on || len(f.queue.settingsRevivals) != 1 {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), revivals %v; want on and one shim started", on, err, f.queue.settingsRevivals)
	}
}

func TestUpdateRemoteControlModeRecordsAnOffWithNoShimAlone(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.remoteErr = &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession}

	// Act.
	on, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlOff)

	// Assert.
	if err != nil || on || len(f.queue.settingsRevivals) != 0 {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), revivals %v; want off and no shim started", on, err, f.queue.settingsRevivals)
	}
}

func TestUpdateRemoteControlModeSurfacesAFailedStart(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.remoteOn = true
	f.fleet.remoteErr = &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimNoSession}
	f.queue.reviveErr = errors.New("no revival wired")

	// Act.
	_, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlOn)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no revival wired") {
		t.Fatalf("err = %v, want the failed start", err)
	}
}

func TestUpdateRemoteControlModeSurfacesTheVendorsRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.remoteOn = true
	f.fleet.remoteErr = &ShimRefusal{Verb: "SetSessionRemoteControl", Arm: ArmShimVendorRefused, Detail: "no"}

	// Act.
	on, err := f.verbs.UpdateRemoteControlMode(context.Background(), "w1", wsm.RemoteControlOn)

	// Assert.
	var refusal *ShimRefusal
	if !on || !errors.As(err, &refusal) || refusal.Arm != ArmShimVendorRefused || len(f.queue.settingsRevivals) != 0 {
		t.Fatalf("UpdateRemoteControlMode = (%v, %v), revivals %v; want the vendor's refusal and no shim started", on, err, f.queue.settingsRevivals)
	}
}

func TestUpdateRemoteControlModeRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.UpdateRemoteControlMode(context.Background(), "nope", wsm.RemoteControlOn)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.fleet.remoteChanges) != 0 {
		t.Fatalf("fleet asks = %v, want none", f.fleet.remoteChanges)
	}
}

// ---- registration ----

func TestRegistrationStatesTheRemoteControlModeOnTheNaming(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.remoteUnscripted = wsm.RemoteControl{On: true}

	// Act.
	if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if n := len(f.topbarNamings); n == 0 || !f.topbarNamings[n-1].RemoteControl {
		t.Fatalf("namings = %+v, want the last carrying remote control on", f.topbarNamings)
	}
}

func TestRegistrationFailsOnAnUnreadableRemoteControlMode(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.remoteErr = errors.New("database is locked")

	// Act.
	_, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("Register = %v, want the read's failure", err)
	}
}

// ---- remoteControlRequest: the one builder of a bridge request ----

func TestRemoteControlRequest(t *testing.T) {
	kept := "bridge-kept"
	tests := []struct {
		name     string
		standing wsm.RemoteControl
		on       bool
		want     *shimv1.SetSessionRemoteControlRequest
	}{
		{"an on names the workspace and re-binds the kept bridge", wsm.RemoteControl{On: true, BridgeSessionID: kept}, true,
			&shimv1.SetSessionRemoteControlRequest{Enabled: true, Name: "name-w1", ReattachBridgeSessionId: &kept}},
		{"an on with no kept bridge re-binds none", wsm.RemoteControl{On: true}, true,
			&shimv1.SetSessionRemoteControlRequest{Enabled: true, Name: "name-w1"}},
		{"an off names nothing", wsm.RemoteControl{BridgeSessionID: kept}, false,
			&shimv1.SetSessionRemoteControlRequest{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.setRemote(ws.ID, tt.standing)

			// Act.
			got, err := f.fleet.remoteControlRequest(context.Background(), ws.ID, tt.on)

			// Assert.
			if err != nil || !proto.Equal(got, tt.want) {
				t.Fatalf("remoteControlRequest = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestRemoteControlRequestRefusesAnUnreadableStanding(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.remoteErr = errFake

	// Act.
	_, err := f.fleet.remoteControlRequest(context.Background(), ws.ID, true)

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("err = %v, want the store's failure", err)
	}
}

// TestALiveBridgeIsAskedTheBuiltRequest asserts a live session's bridge is
// asked exactly the builder's request.
func TestALiveBridgeIsAskedTheBuiltRequest(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.setRemote(ws.ID, wsm.RemoteControl{On: true, BridgeSessionID: "bridge-kept"})
	want, err := f.fleet.remoteControlRequest(context.Background(), ws.ID, true)
	if err != nil {
		t.Fatalf("remoteControlRequest: %v", err)
	}

	// Act.
	if _, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws.ID, wsm.RemoteControlOn); err != nil {
		t.Fatalf("SetRemoteControl: %v", err)
	}

	// Assert.
	if n := len(f.client.remoteControls); n == 0 || !proto.Equal(f.client.remoteControls[n-1], want) {
		t.Fatalf("SetSessionRemoteControl asks = %v, want the last to be %v", f.client.remoteControls, want)
	}
}

// ---- Fleet.RestateRemoteControl: a queue bypass transition ----

// lifted is the standing the store reads for a holder whose take lifted
// remote control.
var lifted = wsm.RemoteControl{On: true, LiftedByQueueBypass: true}

func TestFleetRestateRemoteControlPutsEachTransitionsModeInForce(t *testing.T) {
	tests := []struct {
		name       string
		transition string
		standing   wsm.RemoteControl
		wantOn     bool
	}{
		{name: "entering with remote control off turns the bridge on", transition: queueBypassEntered, standing: lifted, wantOn: true},
		{name: "entering with remote control already on keeps the bridge on", transition: queueBypassEntered, standing: wsm.RemoteControl{On: true, Recorded: true}, wantOn: true},
		{name: "leaving a lift turns the bridge off", transition: queueBypassLeft, standing: wsm.RemoteControl{}},
		{name: "leaving with remote control on before keeps the bridge on", transition: queueBypassLeft, standing: wsm.RemoteControl{On: true, Recorded: true}, wantOn: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f, ws := startedFleet(t)
			f.db.setRemote(ws.ID, tt.standing)

			// Act.
			err := f.fleet.RestateRemoteControl(context.Background(), f.log.logger, ws.ID, tt.transition)

			// Assert.
			if err != nil {
				t.Fatalf("RestateRemoteControl: %v", err)
			}
			if len(f.client.remoteControls) != 1 || f.client.remoteControls[0].GetEnabled() != tt.wantOn {
				t.Fatalf("SetSessionRemoteControl asks = %v, want one %s", f.client.remoteControls, onOff(tt.wantOn))
			}
			if len(f.topbarRemote) != 1 || f.topbarRemote[0] != tt.wantOn {
				t.Fatalf("topbar modes = %v, want %s", f.topbarRemote, onOff(tt.wantOn))
			}
		})
	}
}

func TestFleetRestateRemoteControlWithNoShimStatesTheModeAlone(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, lifted)

	// Act.
	err := f.fleet.RestateRemoteControl(context.Background(), f.log.logger, ws.ID, queueBypassEntered)

	// Assert.
	if err != nil || len(f.topbarRemote) != 1 || !f.topbarRemote[0] || len(f.topbarWarnings) != 0 {
		t.Fatalf("RestateRemoteControl = %v, topbar %v, warnings %v; want on stated, no failure", err, f.topbarRemote, f.topbarWarnings)
	}
}

func TestFleetRestateRemoteControlSurfacesAnUnreadableMode(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.remoteErr = errors.New("database is locked")

	// Act.
	err := f.fleet.RestateRemoteControl(context.Background(), f.log.logger, ws.ID, queueBypassEntered)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "database is locked") {
		t.Fatalf("RestateRemoteControl = %v, want the read's failure", err)
	}
	if len(f.client.remoteControls) != 0 || len(f.topbarRemote) != 0 {
		t.Fatalf("asks %v, topbar %v; want nothing sent or drawn", f.client.remoteControls, f.topbarRemote)
	}
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "database is locked") {
		t.Errorf("warnings = %v, want one carrying the cause", f.topbarWarnings)
	}
	requireTransitionRecord(t, f, "could not read the remote-control mode a queue bypass transition left", queueBypassEntered, "database is locked")
}

func TestFleetRestateRemoteControlSurfacesTheVendorsRefusal(t *testing.T) {
	// Arrange.
	f, ws := startedFleet(t)
	f.db.setRemote(ws.ID, lifted)
	f.client.remoteControlResponse = vendorRefusedRemoteControl()

	// Act.
	err := f.fleet.RestateRemoteControl(context.Background(), f.log.logger, ws.ID, queueBypassEntered)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimVendorRefused {
		t.Fatalf("RestateRemoteControl = %v, want the vendor's refusal", err)
	}
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "disabled by policy") {
		t.Errorf("warnings = %v, want one carrying the vendor's detail", f.topbarWarnings)
	}
	requireTransitionRecord(t, f, "the shim did not put the remote-control mode in force", queueBypassEntered, "disabled by policy")
}

// requireTransitionRecord fails unless one ERROR record under opRemoteControl
// carries MESSAGE, the TRANSITION and a cause containing CAUSE.
func requireTransitionRecord(t *testing.T, f *fleetFixture, message, transition, cause string) {
	t.Helper()
	records := dlog.FilterRecords(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRemoteControl})
	if len(records) != 1 || records[0].Message != message {
		t.Fatalf("error records = %+v, want one: %q", records, message)
	}
	if got := records[0].Context["transition"]; got != transition {
		t.Errorf("transition = %v, want %q", got, transition)
	}
	if got := fmt.Sprint(records[0].Context["cause"]); !strings.Contains(got, cause) {
		t.Errorf("cause = %q, want it to carry %q", got, cause)
	}
}

// TestEveryModeChangeSharesTheInForceTail pins the extraction: an explicit
// change and a queue bypass transition to the same mode ask the shim and the
// topbar exactly the same.
func TestEveryModeChangeSharesTheInForceTail(t *testing.T) {
	// Arrange.
	explicit, ws := startedFleet(t)
	transition, other := startedFleet(t)
	transition.db.setRemote(other.ID, lifted)

	// Act.
	_, errExplicit := explicit.fleet.SetRemoteControl(context.Background(), explicit.log.logger, ws.ID, wsm.RemoteControlOn)
	errTransition := transition.fleet.RestateRemoteControl(context.Background(), transition.log.logger, other.ID, queueBypassEntered)

	// Assert.
	if errExplicit != nil || errTransition != nil {
		t.Fatalf("errors = %v, %v; want none", errExplicit, errTransition)
	}
	if len(explicit.client.remoteControls) != 1 || len(transition.client.remoteControls) != 1 ||
		!proto.Equal(explicit.client.remoteControls[0], transition.client.remoteControls[0]) {
		t.Fatalf("asks = %v and %v, want the same one ask", explicit.client.remoteControls, transition.client.remoteControls)
	}
	if fmt.Sprint(explicit.topbarRemote) != fmt.Sprint(transition.topbarRemote) {
		t.Fatalf("topbar = %v and %v, want the same", explicit.topbarRemote, transition.topbarRemote)
	}
}

func TestARestartedShimOfAHolderIsPutInRemoteControl(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.setRemote(ws.ID, lifted)

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.remoteControls) != 1 || !f.client.remoteControls[0].GetEnabled() {
		t.Fatalf("SetSessionRemoteControl asks = %v, want one on for the lifted holder", f.client.remoteControls)
	}
}

func TestARegisteredHolderStatesItsLiftOnTheNaming(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.remoteUnscripted = lifted

	// Act.
	if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if n := len(f.topbarNamings); n == 0 || !f.topbarNamings[n-1].RemoteControl {
		t.Fatalf("namings = %+v, want the last carrying the lifted remote control on", f.topbarNamings)
	}
}
