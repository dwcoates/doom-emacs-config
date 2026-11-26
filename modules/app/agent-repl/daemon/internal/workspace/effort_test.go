package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/claudesettings"
	"agent-repld/internal/dlog"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

const (
	effortLow  = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_LOW
	effortHigh = conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH
)

// effortFailure is a shim refusal carrying CAUSE.
func effortFailure(failure *shimv1.SetSessionEffortFailure) *shimv1.SetSessionEffortResponse {
	return &shimv1.SetSessionEffortResponse{Result: &shimv1.SetSessionEffortResponse_Failure{Failure: failure}}
}

// recordsAt lists the fleet fixture's records under OPERATION at LEVEL.
func recordsAt(f *fakeSurfaces, operation, level string) []map[string]any {
	var out []map[string]any
	for _, record := range dlog.FilterRecords(f.logger.Records(), dlog.RecordQuery{Level: level, Operation: operation}) {
		out = append(out, record.Context)
	}
	return out
}

func TestSetEffortArmNamesEachShimRefusal(t *testing.T) {
	tests := []struct {
		name    string
		failure *shimv1.SetSessionEffortFailure
		want    string
	}{
		{"not_supported", &shimv1.SetSessionEffortFailure{Cause: &shimv1.SetSessionEffortFailure_NotSupported{NotSupported: &shimv1.SetSessionEffortNotSupported{}}}, ArmEffortNotSupported},
		{"no_session", &shimv1.SetSessionEffortFailure{Cause: &shimv1.SetSessionEffortFailure_NoSession{NoSession: &shimv1.SetSessionEffortNoSession{}}}, ArmShimNoSession},
		{"vendor_refused", &shimv1.SetSessionEffortFailure{Cause: &shimv1.SetSessionEffortFailure_VendorRefused{VendorRefused: &shimv1.SetSessionEffortVendorRefused{}}}, "vendor_refused"},
		{"unset", &shimv1.SetSessionEffortFailure{}, ArmShimUnspecified},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table. Act.
			got := setEffortArm(tt.failure)

			// Assert.
			if got != tt.want {
				t.Errorf("setEffortArm = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestApplyEffortAnswersTheLevelTheShimConfirmed(t *testing.T) {
	// Arrange.
	client := &fakeClient{}

	// Act.
	got, err := applyEffort(context.Background(), client, effortHigh)

	// Assert.
	if err != nil || got != effortHigh {
		t.Fatalf("applyEffort = (%v, %v), want (high, nil)", got, err)
	}
}

func TestApplyEffortCarriesTheShimsRefusalByArmAndDetail(t *testing.T) {
	// Arrange.
	client := &fakeClient{effortResponse: effortFailure(&shimv1.SetSessionEffortFailure{
		Detail: "the vendor said no",
		Cause:  &shimv1.SetSessionEffortFailure_VendorRefused{VendorRefused: &shimv1.SetSessionEffortVendorRefused{}},
	})}

	// Act.
	_, err := applyEffort(context.Background(), client, effortHigh)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "vendor_refused" || refusal.Detail != "the vendor said no" {
		t.Fatalf("err = %v, want the vendor_refused refusal with its detail", err)
	}
}

func TestApplyEffortRefusesASuccessStatingNoLevel(t *testing.T) {
	// Arrange.
	client := &fakeClient{effortResponse: &shimv1.SetSessionEffortResponse{
		Result: &shimv1.SetSessionEffortResponse_Success{Success: &shimv1.SetSessionEffortSuccess{}},
	}}

	// Act.
	_, err := applyEffort(context.Background(), client, effortHigh)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no level in effect") {
		t.Fatalf("err = %v, want the missing level named", err)
	}
}

func TestApplyEffortSurfacesATransportFailure(t *testing.T) {
	// Arrange.
	client := &fakeClient{effortErr: errors.New("socket closed")}

	// Act.
	_, err := applyEffort(context.Background(), client, effortHigh)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "socket closed") {
		t.Fatalf("err = %v, want the transport failure", err)
	}
}

func TestFleetSetEffortRefusesNoSessionWithNoLiveShim(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	err := f.fleet.SetEffort(context.Background(), f.log.logger, ws.ID, effortHigh)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
		t.Fatalf("err = %v, want the no_session refusal", err)
	}
}

func TestFleetSetEffortOnAShimWithNoSessionIsTheNoSessionRefusalAtInfo(t *testing.T) {
	// Arrange: the shim is up and has no session yet.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.effortResponse = effortFailure(&shimv1.SetSessionEffortFailure{
		Cause:  &shimv1.SetSessionEffortFailure_NoSession{NoSession: &shimv1.SetSessionEffortNoSession{}},
		Detail: "no session has been started on this shim",
	})

	// Act.
	err := f.fleet.SetEffort(context.Background(), f.log.logger, ws.ID, effortHigh)

	// Assert.
	t.Run("the caller gets the no_session refusal", func(t *testing.T) {
		var refusal *ShimRefusal
		if !errors.As(err, &refusal) || refusal.Arm != ArmShimNoSession {
			t.Fatalf("err = %v, want the no_session refusal", err)
		}
	})
	t.Run("it is recorded at INFO, never ERROR", func(t *testing.T) {
		if records := recordsAt(f.log, opSetEffort, "error"); len(records) != 0 {
			t.Fatalf("error records = %v, want none", records)
		}
		if records := recordsAt(f.log, opSetEffort, "info"); len(records) != 1 || records[0]["effort"] != effortHigh.String() {
			t.Fatalf("info records = %v, want one naming the level", records)
		}
	})
}

func TestFleetSetEffortStatesTheConfirmedLevelToTheTopbar(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	err := f.fleet.SetEffort(context.Background(), f.log.logger, ws.ID, effortHigh)

	// Assert.
	if err != nil || f.picked != effortHigh {
		t.Fatalf("SetEffort = %v, picked = %v; want nil and high", err, f.picked)
	}
}

func TestFleetSetEffortLeavesThePickAloneAndLogsWhenTheShimRefuses(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.effortResponse = effortFailure(&shimv1.SetSessionEffortFailure{
		Cause: &shimv1.SetSessionEffortFailure_NotSupported{NotSupported: &shimv1.SetSessionEffortNotSupported{}},
	})

	// Act.
	err := f.fleet.SetEffort(context.Background(), f.log.logger, ws.ID, effortHigh)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != ArmEffortNotSupported {
		t.Fatalf("err = %v, want the not_supported refusal", err)
	}
	if f.picked != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		t.Errorf("picked = %v, want no pick recorded for a refusal", f.picked)
	}
	if records := recordsAt(f.log, opSetEffort, "error"); len(records) != 1 || records[0]["effort"] != effortHigh.String() {
		t.Errorf("error records = %v, want one naming the level", records)
	}
}

func TestANewShimIsPutBackAtThePickedEffort(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.picked = effortLow

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.efforts) != 1 || f.client.efforts[0].GetEffort() != effortLow {
		t.Fatalf("SetSessionEffort asks = %v, want one for the picked low", f.client.efforts)
	}
}

func TestAPutBackRecordsHowLongTheShimTook(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		level string
	}{
		{name: "a put-back that took", level: "info"},
		{name: "a refused put-back", err: errors.New("socket closed"), level: "error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.picked = effortLow
			f.client.effortErr = tt.err

			// Act.
			_ = f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			records := recordsAt(f.log, opSetEffort, tt.level)
			if len(records) != 1 {
				t.Fatalf("records = %v, want one at %s", records, tt.level)
			}
			if _, ok := records[0]["elapsed_ms"]; !ok {
				t.Fatalf("record = %v, want elapsed_ms", records[0])
			}
		})
	}
}

func TestANewShimIsAskedNothingWithNoPick(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.client.efforts) != 0 {
		t.Fatalf("SetSessionEffort asks = %v, want none before any pick", f.client.efforts)
	}
}

func TestAFailedPutBackIsLogged(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.picked = effortLow
	f.client.effortErr = errors.New("socket closed")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert: the session still comes up; the failure is loud, not fatal.
	if err != nil {
		t.Fatalf("Start: %v, want the session up", err)
	}
	if records := recordsAt(f.log, opSetEffort, "error"); len(records) != 1 || records[0]["cause"] != "socket closed" {
		t.Errorf("error records = %v, want one carrying the cause", records)
	}
}

func TestSetEffortRefusesWhenTheSelectorServedNoLevels(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert.
	asRefusal(t, err, ArmEffortNotSupported)
	if len(f.fleet.efforts) != 0 {
		t.Errorf("fleet asks = %v, want none", f.fleet.efforts)
	}
}

func TestSetEffortRefusesALevelTheSelectorNeverOffered(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortLow}, true

	// Act.
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert.
	asRefusal(t, err, ArmEffortNotSupported)
	if len(f.fleet.efforts) != 0 {
		t.Errorf("fleet asks = %v, want none", f.fleet.efforts)
	}
}

func TestSetEffortAsksTheFleetForAServedLevel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortLow, effortHigh}, true

	// Act.
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert.
	if err != nil || len(f.fleet.efforts) != 1 || f.fleet.efforts[0].Level != effortHigh {
		t.Fatalf("SetEffort = %v, fleet asks = %v; want one ask for high", err, f.fleet.efforts)
	}
}

func TestSetEffortNeverQueuesAnAct(t *testing.T) {
	// Arrange: an effort change never puts a row in front of the reader.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true

	// Act.
	if err := f.verbs.SetEffort(context.Background(), "w1", effortHigh); err != nil {
		t.Fatalf("SetEffort: %v", err)
	}

	// Assert.
	if acts := f.queue.acts["w1"]; len(acts) != 0 {
		t.Fatalf("session acts = %+v, want none", acts)
	}
}

func TestSetEffortSurfacesTheFleetsFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true
	f.fleet.effortErr = &ShimRefusal{Verb: "SetSessionEffort", Arm: "vendor_refused", Detail: "no"}

	// Act.
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert.
	var refusal *ShimRefusal
	if !errors.As(err, &refusal) || refusal.Arm != "vendor_refused" {
		t.Fatalf("err = %v, want the fleet's refusal", err)
	}
}

func TestRegistrationReadsTheConfigRootsEffortSettings(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.account.configDir = "/config"
	f.effortSettings = claudesettings.Effort{Path: "/config/settings.json", Default: effortHigh}

	// Act.
	if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	if len(f.effortReads) != 1 || f.effortReads[0] != "/config" {
		t.Fatalf("effort reads = %v, want the session's root", f.effortReads)
	}
	if got := f.topbarEffortSettings; len(got) != 1 || got[0].Default != effortHigh {
		t.Fatalf("topbar effort settings = %+v, want the read installed", got)
	}
}

func TestAnUnreadableSettingsFileIsStatedAndTheRootNamesNoLevel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.account.configDir = "/config"
	f.effortSettings = claudesettings.Effort{Path: "/config/settings.json", Default: effortHigh}
	f.effortErr = errors.New("unexpected end of JSON input")

	// Act.
	if _, err := f.verbs.Register(context.Background(), f.git.mainWorktree, wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	got := f.topbarEffortSettings
	if len(got) != 1 || got[0].Default != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED || got[0].Path != "/config/settings.json" {
		t.Fatalf("topbar effort settings = %+v, want the path with no level", got)
	}
	if records := recordsAt(f.log, opEffortSettings, "error"); len(records) != 1 || records[0]["cause"] != "unexpected end of JSON input" {
		t.Errorf("error records = %v, want one carrying the cause", records)
	}
}

func TestSelectAccountRereadsTheEffortSettingsOfTheChosenRoot(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act.
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert.
	if n := len(f.effortReads); n == 0 || f.effortReads[n-1] != "/config-work" {
		t.Fatalf("effort reads = %v, want the chosen root read last", f.effortReads)
	}
}

func TestAnEffortPickWithNoShimKeepsThePickAndStartsTheShim(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true
	f.fleet.effortErr = &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession}

	// Act
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert
	if err != nil || f.topbarPicked != effortHigh || len(f.queue.settingsRevivals) != 1 || f.queue.settingsRevivals[0] != "w1" {
		t.Fatalf("err %v, picked %v, revivals %v: want the pick kept and one settings start", err, f.topbarPicked, f.queue.settingsRevivals)
	}
}

func TestAnEffortPickThatCannotStartTheShimFails(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.cards.efforts, f.cards.hasEfforts = []conversationv1.AgentEffortLevel{effortHigh}, true
	f.fleet.effortErr = &ShimRefusal{Verb: "SetSessionEffort", Arm: ArmShimNoSession}
	f.queue.reviveErr = errors.New("no revival wired")

	// Act
	err := f.verbs.SetEffort(context.Background(), "w1", effortHigh)

	// Assert
	if err == nil {
		t.Fatal("SetEffort answered success though the shim could not be started")
	}
}

// goneShimCause is the transport failure a call to a shim that is no more
// carries.
var goneShimCause = errors.New("unavailable: dial unix /sock/9e9ccb0bae024ebd.n21.sock: connect: no such file or directory")

func TestFleetSetEffortOnAShimGoneOrUnreachableIsNoSessionAtInfo(t *testing.T) {
	tests := []struct {
		name string
		err  error
	}{
		{name: "a shim this daemon stood down", err: fmt.Errorf("%w: %w", shimclient.ErrStandDownOrdered, goneShimCause)},
		{name: "a shim that died", err: fmt.Errorf("%w: %w", shimclient.ErrShimDied, goneShimCause)},
		{name: "a shim whose socket could not be dialed", err: fmt.Errorf("%w: %w", shimclient.ErrShimUnreachable, goneShimCause)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}
			f.client.effortErr = tt.err

			// Act.
			err := f.fleet.SetEffort(context.Background(), f.log.logger, ws.ID, effortLow)

			// Assert.
			if !switchFoundNoSession(err) || !errors.Is(err, tt.err) {
				t.Fatalf("err = %v, want the switch read as finding no session, the cause kept", err)
			}
			if records := recordsAt(f.log, opSetEffort, "error"); len(records) != 0 {
				t.Fatalf("error records = %v, want none", records)
			}
			if records := recordsAt(f.log, opSetEffort, "info"); len(records) != 1 || records[0]["cause"] != tt.err.Error() {
				t.Fatalf("info records = %v, want one carrying the cause", records)
			}
		})
	}
}

func TestFleetSetEffortAbandonedByItsCallerIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()
	f.client.effortErr = context.Canceled

	// Act.
	err := f.fleet.SetEffort(ctx, f.log.logger, ws.ID, effortLow)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("err = %v, want the cancellation answered", err)
	}
	if records := recordsAt(f.log, opSetEffort, "error"); len(records) != 0 {
		t.Fatalf("error records = %v, want none for a switch its caller abandoned", records)
	}
}

// awaitSwitch waits for the effort switch's outcome record MESSAGE at LEVEL,
// then joins the fleet's detached work, so every effect of the switch is in.
func awaitSwitch(t *testing.T, f *fleetFixture, level, message string) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	if _, err := f.log.logger.Await(ctx, dlog.RecordQuery{Level: level, Operation: opSetEffort, Message: message}); err != nil {
		t.Fatalf("records = %+v, want %q at %s: %v", f.log.logger.Records(), message, level, err)
	}
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the effort switch did not leave")
	}
}

func TestSwitchEffortSettlesEachOutcomeOffTheCallersGoroutine(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(f *fleetFixture)
		level      string
		message    string
		wantPicked conversationv1.AgentEffortLevel
		wantErrors int
	}{
		{name: "a switch the shim confirmed states the level", level: "info", message: "the effort switch landed", wantPicked: effortLow},
		{name: "a shim the rollout stood down leaves the pick standing", level: "info",
			message: "the effort switch found no session to take it; the pick stands for the session's next start",
			arrange: func(f *fleetFixture) {
				f.client.effortErr = fmt.Errorf("%w: %w", shimclient.ErrStandDownOrdered, goneShimCause)
			}},
		{name: "a level the vendor refused is an error", level: "error", message: "the shim did not switch the effort",
			wantErrors: 1,
			arrange: func(f *fleetFixture) {
				f.client.effortResponse = effortFailure(&shimv1.SetSessionEffortFailure{
					Detail: "the vendor said no",
					Cause:  &shimv1.SetSessionEffortFailure_VendorRefused{VendorRefused: &shimv1.SetSessionEffortVendorRefused{}},
				})
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}
			if tt.arrange != nil {
				tt.arrange(f)
			}

			// Act.
			f.fleet.SwitchEffort(f.log.logger, ws.ID, effortLow)
			awaitSwitch(t, f, tt.level, tt.message)

			// Assert.
			if f.picked != tt.wantPicked {
				t.Errorf("picked = %v, want %v", f.picked, tt.wantPicked)
			}
			if records := recordsAt(f.log, opSetEffort, "error"); len(records) != tt.wantErrors {
				t.Errorf("error records = %v, want %d", records, tt.wantErrors)
			}
		})
	}
}

func TestSwitchEffortNeverWaitsOnTheRunningTurn(t *testing.T) {
	// Arrange: the shim holds the switch until the running turn ends.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	turnEnds := make(chan struct{})
	f.client.effortHold = turnEnds

	// Act: SwitchEffort answers with the shim still holding the switch.
	f.fleet.SwitchEffort(f.log.logger, ws.ID, effortLow)

	// Assert: nothing has landed while the turn runs, and the switch lands
	// once it ends.
	if _, landed := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opSetEffort, Message: "the effort switch landed"}); landed {
		t.Fatal("the switch landed while the shim still held it")
	}
	close(turnEnds)
	awaitSwitch(t, f, "info", "the effort switch landed")
	if f.picked != effortLow {
		t.Fatalf("picked = %v, want low once the turn ended", f.picked)
	}
}
