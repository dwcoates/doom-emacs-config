package health

import (
	"context"
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// TestFaultFooterCellPartitionsEverySessionKind is the partition itself, one
// row per session-scoped fault kind. It is the test the owner's ruling asks
// for: every daemon fault kind reaches the footer, and this states where.
func TestFaultFooterCellPartitionsEverySessionKind(t *testing.T) {
	tests := []struct {
		name string
		kind string
		want FaultCell
	}{
		{"a shim that would not come up", KindShimStartFailed, FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed}},
		{"a resume the shim refused", KindResumeFailed, FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed}},
		{"the legacy relaunch spelling of the same class", KindRelaunchResumeFailed, FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed}},
		{"an adoption window nobody claimed", KindAdoptionWindowExpired, FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed}},
		{"a cold gate whose re-open failed", KindColdGateReopenFailed, FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed}},
		{"a shim that exited while serving", KindShimDied, FaultCell{FaultStatusDisconnected, FaultSubStatusDead}},
		{"a bounce whose replacement died", KindBounceDied, FaultCell{FaultStatusDisconnected, FaultSubStatusDead}},
		{"a workspace with no live session", KindSessionAbsent, FaultCell{FaultStatusDisconnected, FaultSubStatusDead}},
		{"a link that stopped serving", KindLinkSevered, FaultCell{FaultStatusDisconnected, FaultSubStatusSevered}},
		{"a watch open the shim refused", KindWatchOpenRefused, FaultCell{FaultStatusDisconnected, FaultSubStatusSevered}},
		{"a state client that will not answer", KindStateUnreadable, FaultCell{FaultStatusBlocked, FaultSubStatusDaemonImpaired}},
		{"a fault the shim reported about itself", KindShimReported, FaultCell{FaultStatusNone, ""}},
		{"a headless classifier run that failed", KindClassifierFailed, FaultCell{FaultStatusNone, ""}},
		{"a bounce disposition needing a human", KindBounceUnknown, FaultCell{FaultStatusNone, ""}},
		{"a conversation a fresh bring-up left behind", KindConversationAbandoned, FaultCell{FaultStatusNone, ""}},
		{"a turn whose answer did not land", KindFinalAnswerUnresolved, FaultCell{FaultStatusNone, ""}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got, ok := FaultFooterCell(tt.kind, false)

			// Assert
			if !ok {
				t.Fatalf("FaultFooterCell(%q) has no footer cell; every fault kind reaches the footer", tt.kind)
			}
			if got != tt.want {
				t.Fatalf("FaultFooterCell(%q) = %+v, want %+v", tt.kind, got, tt.want)
			}
		})
	}
}

// TestFaultFooterCellPartitionsEveryDaemonKind is the same table for the
// daemon-scoped kinds, which stand on every workspace's strip.
func TestFaultFooterCellPartitionsEveryDaemonKind(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{"an absent prompts directory", KindPromptsDirMissing},
		{"a state client that opened read-only", KindWsmReadOnly},
		{"a durable sink that cannot be written", KindLogSinkPoisoned},
		{"a handover whose successor would not start", KindSuccessorSpawnFailed},
		{"a state client that will not answer", KindStateUnreadable},
		{"a handover nobody claimed", KindAdoptionWindowExpired},
	}
	want := FaultCell{FaultStatusBlocked, FaultSubStatusDaemonImpaired}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got, ok := FaultFooterCell(tt.kind, true)

			// Assert
			if !ok {
				t.Fatalf("FaultFooterCell(%q, daemon) has no footer cell", tt.kind)
			}
			if got != want {
				t.Fatalf("FaultFooterCell(%q, daemon) = %+v, want %+v", tt.kind, got, want)
			}
		})
	}
}

// TestFaultFooterCellLeavesTheStatusToAFailedDeploy: the daemon that ran the
// deploy keeps serving, so the fault claims the activity line alone.
func TestFaultFooterCellLeavesTheStatusToAFailedDeploy(t *testing.T) {
	// Arrange, Act
	got, ok := FaultFooterCell(KindDeployFailed, true)

	// Assert
	if !ok {
		t.Fatalf("FaultFooterCell(%q, daemon) has no footer cell", KindDeployFailed)
	}
	if got != (FaultCell{}) {
		t.Fatalf("FaultFooterCell(%q, daemon) = %+v, want the non-escalating cell", KindDeployFailed, got)
	}
}

// TestFaultFooterCellRefusesTheAccountingKind is the one kind that draws
// nothing, and the reason it draws nothing.
func TestFaultFooterCellRefusesTheAccountingKind(t *testing.T) {
	// Arrange, Act
	_, ok := FaultFooterCell("bounce_disposition", false)

	// Assert
	if ok {
		t.Fatalf("bounce_disposition claims a footer cell; it is opened and closed in one breath and never stands")
	}
}

func TestFaultLineDetailReadsTheStartFailureFromItsEvidence(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: KindShimStartFailed, Detail: "prose", Evidence: map[string]string{
		"exit_code": "127", "stderr_tail": "one\ntwo\n",
	}}

	// Act
	got := FaultLineDetail(f)

	// Assert
	if want := "exit 127: two"; got != want {
		t.Fatalf("FaultLineDetail = %q, want %q", got, want)
	}
}

func TestFaultLineDetailNamesTheDeployStepThatFailed(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: KindDeployFailed, Detail: "prose", Evidence: DeployFailure{
		Step: DeployStepBuild, BuildStep: "webapp", Detail: "first\nerror TS2322\n",
	}.Evidence()}

	// Act
	got := FaultLineDetail(f)

	// Assert
	if want := "build webapp: error TS2322"; got != want {
		t.Fatalf("FaultLineDetail = %q, want %q", got, want)
	}
}

func TestFaultLineDetailPrefersTheRecordedCause(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: KindResumeFailed, Detail: "prose", Evidence: map[string]string{"cause": "the shim refused"}}

	// Act
	got := FaultLineDetail(f)

	// Assert
	if want := "the shim refused"; got != want {
		t.Fatalf("FaultLineDetail = %q, want %q", got, want)
	}
}

func TestFaultLineDetailFallsBackToTheRecordsOwnProse(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: KindBounceUnknown, Detail: "the bounce's outcome is unknown"}

	// Act
	got := FaultLineDetail(f)

	// Assert
	if want := "the bounce's outcome is unknown"; got != want {
		t.Fatalf("FaultLineDetail = %q, want %q", got, want)
	}
}

// recordingSink captures what the decorated state client told the footer.
type recordingSink struct {
	opened []struct {
		ws   ids.WorkspaceID
		line FaultLine
	}
	closed []struct {
		ws ids.WorkspaceID
		id ids.FaultID
	}
}

func (s *recordingSink) FaultOpened(ws ids.WorkspaceID, line FaultLine) {
	s.opened = append(s.opened, struct {
		ws   ids.WorkspaceID
		line FaultLine
	}{ws, line})
}

func (s *recordingSink) FaultClosed(ws ids.WorkspaceID, id ids.FaultID) {
	s.closed = append(s.closed, struct {
		ws ids.WorkspaceID
		id ids.FaultID
	}{ws, id})
}

func TestObserveFaultsTellsTheSinkWhereAnOpenedFaultLands(t *testing.T) {
	// Arrange
	ws := ids.WorkspaceID("ws-1")
	db := &stubDB{openedID: "fault-1"}
	sink := &recordingSink{}
	observed := ObserveFaults(db, sink, newStubSurfaces())

	// Act
	if _, err := observed.OpenFault(context.Background(), wsm.Fault{
		Workspace: &ws, Kind: KindResumeFailed, OpenedAt: fixedNow,
		Evidence: map[string]string{"cause": "the shim refused"},
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Assert
	if len(sink.opened) != 1 {
		t.Fatalf("sink saw %d opened faults, want 1", len(sink.opened))
	}
	got := sink.opened[0]
	want := FaultLine{
		ID: "fault-1", Kind: KindResumeFailed,
		Cell:   FaultCell{FaultStatusDisconnected, FaultSubStatusStartFailed},
		Detail: "the shim refused", At: fixedNow,
	}
	if got.ws != ws || got.line != want {
		t.Fatalf("sink saw (%q, %+v), want (%q, %+v)", got.ws, got.line, ws, want)
	}
}

func TestObserveFaultsDeliversADaemonScopedFaultWithNoWorkspace(t *testing.T) {
	// Arrange
	db := &stubDB{openedID: "fault-2"}
	sink := &recordingSink{}
	observed := ObserveFaults(db, sink, newStubSurfaces())

	// Act
	if _, err := observed.OpenFault(context.Background(), wsm.Fault{
		Kind: KindPromptsDirMissing, OpenedAt: fixedNow,
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Assert
	if len(sink.opened) != 1 || sink.opened[0].ws != "" {
		t.Fatalf("sink saw %+v, want one fault with an empty workspace", sink.opened)
	}
	if want := (FaultCell{FaultStatusBlocked, FaultSubStatusDaemonImpaired}); sink.opened[0].line.Cell != want {
		t.Fatalf("cell = %+v, want %+v", sink.opened[0].line.Cell, want)
	}
}

func TestObserveFaultsWithholdsTheKindThatDrawsNothing(t *testing.T) {
	// Arrange
	ws := ids.WorkspaceID("ws-1")
	db := &stubDB{openedID: "fault-3"}
	sink := &recordingSink{}
	observed := ObserveFaults(db, sink, newStubSurfaces())

	// Act
	if _, err := observed.OpenFault(context.Background(), wsm.Fault{
		Workspace: &ws, Kind: "bounce_disposition", OpenedAt: fixedNow,
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Assert
	if len(sink.opened) != 0 {
		t.Fatalf("sink saw %+v, want nothing: the kind never stands", sink.opened)
	}
}

func TestObserveFaultsRetractsAFaultItOpened(t *testing.T) {
	// Arrange
	ws := ids.WorkspaceID("ws-1")
	db := &stubDB{openedID: "fault-4"}
	sink := &recordingSink{}
	observed := ObserveFaults(db, sink, newStubSurfaces())
	if _, err := observed.OpenFault(context.Background(), wsm.Fault{
		Workspace: &ws, Kind: KindLinkSevered, OpenedAt: fixedNow,
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	if err := observed.CloseFault(context.Background(), "fault-4", fixedNow.Add(time.Minute)); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Assert
	if len(sink.closed) != 1 || sink.closed[0].ws != ws || sink.closed[0].id != "fault-4" {
		t.Fatalf("sink saw %+v, want one retraction of fault-4 on %q", sink.closed, ws)
	}
}

func TestObserveFaultsSaysNothingAboutAFaultThisProcessNeverOpened(t *testing.T) {
	// Arrange
	db := &stubDB{}
	sink := &recordingSink{}
	log := newStubSurfaces()
	observed := ObserveFaults(db, sink, log)

	// Act
	if err := observed.CloseFault(context.Background(), "from-a-previous-daemon", fixedNow); err != nil {
		t.Fatalf("CloseFault: %v", err)
	}

	// Assert
	if len(sink.closed) != 0 {
		t.Fatalf("sink saw %+v, want nothing: no strip is drawing that fault", sink.closed)
	}
	var recorded bool
	for _, r := range log.logger.Records() {
		if r.Operation == opCloseFault {
			recorded = true
		}
	}
	if !recorded {
		t.Fatalf("closing an unknown fault left no record")
	}
}

func TestObserveFaultsSaysNothingWhenTheWriteFailed(t *testing.T) {
	// Arrange
	ws := ids.WorkspaceID("ws-1")
	db := &stubDB{openErr: errStub}
	sink := &recordingSink{}
	observed := ObserveFaults(db, sink, newStubSurfaces())

	// Act
	_, err := observed.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: KindShimDied})

	// Assert
	if err == nil {
		t.Fatalf("OpenFault answered no error; the write failed")
	}
	if len(sink.opened) != 0 {
		t.Fatalf("sink saw %+v, want nothing: no fault was recorded", sink.opened)
	}
}
