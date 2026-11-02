package workspace

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// openedFrame is the bridge's opened frame under BRIDGE.
func openedFrame(bridge string) *shimv1.HoldParkedBridgeResponse {
	return &shimv1.HoldParkedBridgeResponse{Event: &shimv1.HoldParkedBridgeResponse_Opened{
		Opened: &shimv1.HoldParkedBridgeOpened{BridgeSessionId: bridge},
	}}
}

// remotePromptFrame is a prompt typed on claude.ai, saying TEXT.
func remotePromptFrame(text string) *shimv1.HoldParkedBridgeResponse {
	return &shimv1.HoldParkedBridgeResponse{Event: &shimv1.HoldParkedBridgeResponse_RemotePrompt{
		RemotePrompt: &shimv1.HoldParkedBridgeRemotePrompt{ClientUuid: "uuid-1", Said: SaidText(text)},
	}}
}

// refusedFrame is the shim's refusal to hold the bridge.
func refusedFrame() *shimv1.HoldParkedBridgeResponse {
	return &shimv1.HoldParkedBridgeResponse{Event: &shimv1.HoldParkedBridgeResponse_Refused{
		Refused: &shimv1.HoldParkedBridgeRefused{
			Cause:  &shimv1.HoldParkedBridgeRefused_VendorRefused{VendorRefused: &shimv1.HoldParkedBridgeVendorRefused{}},
			Detail: "Remote Control is disabled by policy",
		},
	}}
}

// parkedInRemoteControl starts w1 in remote control into its cold gate, with
// its bridge's frames ready to be sent.
func parkedInRemoteControl(t *testing.T) (*fleetFixture, ids.WorkspaceID) {
	t.Helper()
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	f.client.response = coldResponse()
	f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	return f, ws
}

// settled ends the bridge's stream and waits out the fleet's detached work,
// so everything the bridge's frames caused has happened.
func settled(t *testing.T, f *fleetFixture) {
	t.Helper()
	close(f.client.bridgeFrames)
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the fleet's detached work did not finish")
	}
}

// bridgeRecord finds the record in the fixture's log holds a record at LEVEL with
// MESSAGE on the parked bridge's operation.
func bridgeRecord(f *fleetFixture, level, message string) (dlog.Record, bool) {
	return dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: level, Operation: opParkedBridge, Message: message})
}

func TestAColdRefusalInRemoteControlHoldsTheGatesBridge(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)

	// Act.
	f.client.bridgeFrames <- openedFrame("bridge-1")
	settled(t, f)

	// Assert.
	if len(f.client.bridges) != 1 || len(f.client.kills) != 0 {
		t.Fatalf("bridges asked %d, kills %+v; want one bridge and the shim kept", len(f.client.bridges), f.client.kills)
	}
	if _, ok := f.fleet.ColdGate(ws); !ok {
		t.Fatal("no gate stands, want the gate standing over its bridge")
	}
}

func TestTheParkedBridgeResumesTheGatesConversationAsAStartWould(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)

	// Act.
	f.client.bridgeFrames <- openedFrame("bridge-1")
	settled(t, f)

	// Assert.
	start := f.client.bridges[0].GetStart()
	if !proto.Equal(start, f.client.requests[0]) {
		t.Fatalf("bridge's start = %v, want the start the gate parked %v", start, f.client.requests[0])
	}
	record, err := f.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	bridge := f.client.bridges[0].GetRemoteControl()
	if !bridge.GetEnabled() || bridge.GetName() != record.Name {
		t.Fatalf("bridge request = %v, want on under the workspace's name", bridge)
	}
}

func TestTheParkedBridgeKeepsItsClaudeSession(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)

	// Act.
	f.client.bridgeFrames <- openedFrame("bridge-9")
	settled(t, f)

	// Assert.
	if got := f.db.remoteOf(ws).BridgeSessionID; got != "bridge-9" {
		t.Fatalf("kept bridge session = %q, want bridge-9", got)
	}
	if _, ok := bridgeRecord(f, "info", "the parked workspace is reachable from claude.ai; a prompt typed there answers its cold gate pay"); !ok {
		t.Fatalf("records = %+v, want the bridge's opening at INFO", f.log.logger.Records())
	}
}

func TestARemotePromptAnswersTheGatePay(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)
	f.client.bridgeFrames <- openedFrame("bridge-1")

	// Act.
	f.client.bridgeFrames <- remotePromptFrame("fix the build")
	paid := <-f.paid
	settled(t, f)

	// Assert.
	if paid.ws != ws || paid.answer.GetPay() == nil {
		t.Fatalf("answered %+v, want the pay answer for %s", paid, ws)
	}
}

func TestARemotePromptsAnswerIsRecordedAtInfoNamingThePromptAndTheGate(t *testing.T) {
	// Arrange.
	f, _ := parkedInRemoteControl(t)
	f.client.bridgeFrames <- openedFrame("bridge-1")

	// Act.
	f.client.bridgeFrames <- remotePromptFrame("fix the build")
	<-f.paid
	settled(t, f)

	// Assert.
	record, ok := bridgeRecord(f, "info", "a prompt typed on claude.ai reached the parked workspace; its cold gate is answered pay")
	if !ok {
		t.Fatalf("records = %+v, want the automatic answer at INFO", f.log.logger.Records())
	}
	if record.Context["prompt"] != "fix the build" || record.Context["client_uuid"] != "uuid-1" ||
		record.Context["vendor_session_id"] != "vendor-1" || record.Context["gate"] == "" {
		t.Fatalf("context = %+v, want the prompt, its uuid and the gate named", record.Context)
	}
}

func TestARemotePromptWhoseAnswerIsRefusedIsAnError(t *testing.T) {
	// Arrange.
	f, _ := parkedInRemoteControl(t)
	f.payErr = errors.New("no cold gate stands")
	f.client.bridgeFrames <- openedFrame("bridge-1")

	// Act.
	f.client.bridgeFrames <- remotePromptFrame("fix the build")
	<-f.paid
	settled(t, f)

	// Assert.
	record, ok := bridgeRecord(f, "error", "the cold gate could not be answered pay for the prompt typed on claude.ai")
	if !ok || !strings.Contains(record.Context["cause"].(string), "no cold gate stands") {
		t.Fatalf("records = %+v, want the refused answer at ERROR with its cause", f.log.logger.Records())
	}
}

func TestABridgeThatCannotBeHeldStandsTheShimDownLoudly(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fleetFixture)
		act     func(f *fleetFixture)
		message string
	}{
		{
			name:    "the shim refuses the bridge",
			arrange: func(*fleetFixture) {},
			act:     func(f *fleetFixture) { f.client.bridgeFrames <- refusedFrame() },
			message: "the shim could not hold the parked workspace's bridge; claude.ai cannot reach the parked workspace",
		},
		{
			name:    "the bridge's stream breaks",
			arrange: func(f *fleetFixture) { f.client.bridgeEnd = errors.New("stream reset") },
			act:     func(f *fleetFixture) { f.client.bridgeFrames <- openedFrame("bridge-1") },
			message: "the parked bridge's stream broke; claude.ai cannot reach the parked workspace",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			f.db.setRemote(ws, wsm.RemoteControl{On: true})
			f.client.response = coldResponse()
			f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)
			tt.arrange(f)
			if err := f.fleet.Start(context.Background(), ws); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Act.
			tt.act(f)
			settled(t, f)

			// Assert.
			if _, ok := bridgeRecord(f, "error", tt.message); !ok {
				t.Fatalf("records = %+v, want %q at ERROR", f.log.logger.Records(), tt.message)
			}
			if f.fleet.Held(ws) || len(f.topbarWarnings) == 0 {
				t.Fatalf("held %v, warnings %v; want the shim stood down and the strip told", f.fleet.Held(ws), f.topbarWarnings)
			}
			if _, ok := f.fleet.ColdGate(ws); !ok {
				t.Fatal("no gate stands, want the gate standing with no bridge")
			}
		})
	}
}

func TestABridgeTheShimWillNotOpenStandsTheShimDownLoudly(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	f.client.response = coldResponse()
	f.client.bridgeErr = errFake

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the fleet's detached work did not finish")
	}

	// Assert.
	if _, ok := bridgeRecord(f, "error", "the parked bridge could not be asked of its shim; claude.ai cannot reach the parked workspace"); !ok || f.fleet.Held(ws) {
		t.Fatalf("held %v, records %+v; want the shim stood down and the failure at ERROR", f.fleet.Held(ws), f.log.logger.Records())
	}
}

func TestTheBridgesEndAfterThePayAnswerIsNoFailure(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)
	f.client.bridgeFrames <- openedFrame("bridge-1")

	// Act.
	settled(t, f)

	// Assert.
	if _, ok := bridgeRecord(f, "info", "the parked bridge's stream ended: the cold gate's pay answer took its session"); !ok {
		t.Fatalf("records = %+v, want the promotion's end at INFO", f.log.logger.Records())
	}
	if !f.fleet.Held(ws) {
		t.Fatal("the shim was stood down, want it kept for the session it now serves")
	}
}

func TestAnUnreadableModeAtAGateKeepsNoShimLoudly(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.remoteErr = errFake
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if _, ok := bridgeRecord(f, "error", "could not read the parked workspace's remote-control mode; its gate holds no bridge, and claude.ai cannot reach it"); !ok {
		t.Fatalf("records = %+v, want the unreadable mode at ERROR", f.log.logger.Records())
	}
	if f.fleet.Held(ws) || len(f.client.bridges) != 0 || len(f.topbarWarnings) == 0 {
		t.Fatalf("held %v, bridges %d, warnings %v; want no shim, no bridge and the strip told", f.fleet.Held(ws), len(f.client.bridges), f.topbarWarnings)
	}
}

func TestARecordedGateInRemoteControlSpawnsTheShimThatHoldsItsBridge(t *testing.T) {
	// Arrange: a gate recorded before this daemon, with no shim behind it.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gates[ws] = recordedGate()
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.bridgeFrames <- openedFrame("bridge-1")
	settled(t, f)

	// Assert.
	if len(f.supervisor.spawns) != 1 || len(f.client.bridges) != 1 || len(f.client.requests) != 0 {
		t.Fatalf("spawns %d, bridges %d, starts %d; want one shim spawned for the bridge and no session started",
			len(f.supervisor.spawns), len(f.client.bridges), len(f.client.requests))
	}
}

func TestARecordedGateOutOfRemoteControlSpawnsNothing(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.gates[ws] = recordedGate()

	// Act.
	if err := f.fleet.Start(context.Background(), ws); err != nil {
		t.Fatalf("Start: %v", err)
	}
	if !f.fleet.DrainStarts(time.Minute) {
		t.Fatal("the fleet's detached work did not finish")
	}

	// Assert.
	if f.supervisor.spawnAttempts != 0 || len(f.client.bridges) != 0 {
		t.Fatalf("spawn attempts %d, bridges %d; want neither", f.supervisor.spawnAttempts, len(f.client.bridges))
	}
}

func TestABridgedGateIsNotBridgedTwice(t *testing.T) {
	// Arrange.
	f, ws := parkedInRemoteControl(t)
	f.client.bridgeFrames <- openedFrame("bridge-1")

	// Act.
	f.fleet.holdBridgeAtGate(context.Background(), f.log.logger, ws)
	settled(t, f)

	// Assert.
	if len(f.client.bridges) != 1 {
		t.Fatalf("bridges asked %d, want the one already held", len(f.client.bridges))
	}
}

func TestAnAnsweredGateIsNotBridged(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := heldAtGate(t, f)
	f.db.setRemote(ws, wsm.RemoteControl{On: true})
	f.fleet.TakeColdGate(ws, "vendor-1")

	// Act.
	f.fleet.holdBridgeAtGate(context.Background(), f.log.logger, ws)

	// Assert.
	if len(f.client.bridges) != 0 {
		t.Fatalf("bridges asked %d, want none for an answered gate", len(f.client.bridges))
	}
}

func TestAGateAnswerOnABridgedShim(t *testing.T) {
	clear := &conversationv1.SessionColdRemediation{Remediation: &conversationv1.SessionColdRemediation_Clear{Clear: &conversationv1.SessionColdClear{}}}
	tests := []struct {
		name        string
		remediation *conversationv1.SessionColdRemediation
		wantKills   int
		wantSpawns  int
	}{
		{"a pay resumes through the bridge's shim", payRemediation(), 0, 0},
		{"any other answer stands the bridge's shim down and spawns another", clear, 1, 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f, ws := parkedInRemoteControl(t)
			f.client.bridgeFrames <- openedFrame("bridge-1")
			spawnsBefore := len(f.supervisor.spawns)
			f.fleet.TakeColdGate(ws, "vendor-1")
			record, err := f.db.Workspace(context.Background(), ws)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act.
			held, err := f.fleet.heldForAnswer(context.Background(), f.log.logger, ws, record, f.db.sessions[ws],
				ColdResume{VendorSessionID: "vendor-1", Remediation: tt.remediation})
			settled(t, f)

			// Assert.
			if err != nil || held == nil {
				t.Fatalf("heldForAnswer = %v, %v; want a shim", held, err)
			}
			if len(f.client.kills) != tt.wantKills || len(f.supervisor.spawns)-spawnsBefore != tt.wantSpawns {
				t.Fatalf("kills %d, spawns %d; want %d and %d", len(f.client.kills), len(f.supervisor.spawns)-spawnsBefore, tt.wantKills, tt.wantSpawns)
			}
		})
	}
}

func TestTheModeChangingAtAGateMovesItsBridge(t *testing.T) {
	tests := []struct {
		name        string
		bridged     bool
		change      wsm.RemoteControlChange
		wantBridges int
		wantHeld    bool
	}{
		{"an on bridges the standing gate", false, wsm.RemoteControlOn, 1, true},
		{"an off stands the bridge's shim down", true, wsm.RemoteControlOff, 1, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := heldAtGate(t, f)
			f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)
			if tt.bridged {
				f.db.setRemote(ws, wsm.RemoteControl{On: true})
				f.fleet.gateShim(context.Background(), ws, f.fleet.standDownAtGate)
				f.client.bridgeFrames <- openedFrame("bridge-1")
			}

			// Act.
			if _, err := f.fleet.SetRemoteControl(context.Background(), f.log.logger, ws, tt.change); err != nil {
				t.Fatalf("SetRemoteControl: %v", err)
			}
			if !tt.bridged {
				f.client.bridgeFrames <- openedFrame("bridge-1")
			}
			settled(t, f)

			// Assert.
			if len(f.client.bridges) != tt.wantBridges || len(f.client.remoteControls) != 0 || f.fleet.Held(ws) != tt.wantHeld {
				t.Fatalf("bridges %d, live bridge asks %d, held %v; want %d, 0, %v",
					len(f.client.bridges), len(f.client.remoteControls), f.fleet.Held(ws), tt.wantBridges, tt.wantHeld)
			}
		})
	}
}

func TestARelaunchTheShimAnsweredCold(t *testing.T) {
	tests := []struct {
		name        string
		on          bool
		wantBridges int
	}{
		{"in remote control holds the gate's bridge on the relaunched shim", true, 1},
		{"out of remote control leaves the relaunched shim as it was", false, 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := resumable(f)
			f.fleet.remember(ws, &live{client: f.client, hostSessionID: "host-1"})
			f.db.setRemote(ws, wsm.RemoteControl{On: tt.on})
			f.client.bridgeFrames = make(chan *shimv1.HoldParkedBridgeResponse)

			// Act.
			if err := f.fleet.RaiseColdGate(context.Background(), ws, coldResponse().GetFailure().GetCold()); err != nil {
				t.Fatalf("RaiseColdGate: %v", err)
			}
			if tt.on {
				f.client.bridgeFrames <- openedFrame("bridge-1")
			}
			settled(t, f)

			// Assert.
			if len(f.client.bridges) != tt.wantBridges || len(f.client.kills) != 0 {
				t.Fatalf("bridges %d, kills %d; want %d and none", len(f.client.bridges), len(f.client.kills), tt.wantBridges)
			}
		})
	}
}

func TestRefusedBridgeArmNamesEachRefusal(t *testing.T) {
	tests := []struct {
		name    string
		refused *shimv1.HoldParkedBridgeRefused
		want    string
	}{
		{"resume failed", &shimv1.HoldParkedBridgeRefused{Cause: &shimv1.HoldParkedBridgeRefused_ResumeFailed{ResumeFailed: &shimv1.HoldParkedBridgeResumeFailed{}}}, "resume_failed"},
		{"vendor refused", &shimv1.HoldParkedBridgeRefused{Cause: &shimv1.HoldParkedBridgeRefused_VendorRefused{VendorRefused: &shimv1.HoldParkedBridgeVendorRefused{}}}, "vendor_refused"},
		{"already started", &shimv1.HoldParkedBridgeRefused{Cause: &shimv1.HoldParkedBridgeRefused_AlreadyStarted{AlreadyStarted: &shimv1.HoldParkedBridgeAlreadyStarted{}}}, "already_started"},
		{"no cause", &shimv1.HoldParkedBridgeRefused{}, "unspecified"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := refusedBridgeArm(tt.refused)

			// Assert.
			if got != tt.want {
				t.Fatalf("refusedBridgeArm = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestRemediationName(t *testing.T) {
	tests := []struct {
		name        string
		remediation *conversationv1.SessionColdRemediation
		want        string
	}{
		{"pay", payRemediation(), "pay"},
		{"clear", &conversationv1.SessionColdRemediation{Remediation: &conversationv1.SessionColdRemediation_Clear{Clear: &conversationv1.SessionColdClear{}}}, "clear"},
		{"compact", &conversationv1.SessionColdRemediation{Remediation: &conversationv1.SessionColdRemediation_Compact{Compact: &conversationv1.SessionColdCompact{}}}, "compact"},
		{"none", nil, "none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act + Assert.
			if got := remediationName(tt.remediation); got != tt.want {
				t.Fatalf("remediationName = %q, want %q", got, tt.want)
			}
		})
	}
}
