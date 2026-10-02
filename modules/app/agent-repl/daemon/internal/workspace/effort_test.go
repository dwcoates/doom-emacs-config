package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/claudesettings"
	"claude-repld/internal/wsm"
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
	for _, record := range f.logger.Records() {
		if record.Operation == operation && record.Level == level {
			out = append(out, record.Context)
		}
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

func TestAFailedPutBackIsLoggedAndStatedOnTheStrip(t *testing.T) {
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
	if len(f.topbarWarnings) != 1 || !strings.Contains(f.topbarWarnings[0], "effort low") {
		t.Errorf("warnings = %v, want one naming the level", f.topbarWarnings)
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
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
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
	if _, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{}); err != nil {
		t.Fatalf("Register: %v", err)
	}

	// Assert.
	got := f.topbarEffortSettings
	if len(got) != 1 || got[0].Default != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED || got[0].Path != "/config/settings.json" {
		t.Fatalf("topbar effort settings = %+v, want the path with no level", got)
	}
	if len(f.topbarWarnings) != 1 || !strings.HasPrefix(f.topbarWarnings[0], effortSettingsWarning+": ") {
		t.Errorf("warnings = %v, want one keyed %s", f.topbarWarnings, effortSettingsWarning)
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
