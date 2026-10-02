package health

import (
	"errors"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/wsm"
)

func TestDaemonFaultFillsEveryTypedArm(t *testing.T) {
	tests := []struct {
		name  string
		fault wsm.Fault
	}{
		{name: "adoption window expired", fault: wsm.Fault{Kind: KindAdoptionWindowExpired}},
		{name: "log sink poisoned", fault: wsm.Fault{Kind: KindLogSinkPoisoned}},
		{name: "successor spawn failed", fault: wsm.Fault{Kind: KindSuccessorSpawnFailed}},
		{name: "prompts dir missing", fault: wsm.Fault{Kind: KindPromptsDirMissing}},
		{name: "wsm read only", fault: wsm.Fault{Kind: KindWsmReadOnly}},
		{name: "deploy failed", fault: wsm.Fault{Kind: KindDeployFailed, Evidence: DeployFailure{Step: DeployStepBuild}.Evidence()}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := daemonFault(tt.fault)
			// Assert.
			if got.GetKind() == nil {
				t.Fatalf("daemonFault(%q) left the kind oneof unset", tt.fault.Kind)
			}
		})
	}
}

func TestDaemonFaultCarriesThePoisonedSink(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindLogSinkPoisoned, Evidence: map[string]string{"sink": "/w/.claude/emacs/daemon.log"}}

	// Act.
	got := daemonFault(fault)

	// Assert.
	if got.GetLogSinkPoisoned().GetSink() != "/w/.claude/emacs/daemon.log" {
		t.Fatalf("sink = %q, want the recorded path", got.GetLogSinkPoisoned().GetSink())
	}
}

func TestDaemonFaultCarriesThePromptsPath(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindPromptsDirMissing, Evidence: map[string]string{"path": "/repo/prompts"}}

	// Act.
	got := daemonFault(fault)

	// Assert.
	if got.GetPromptsDirMissing().GetPath() != "/repo/prompts" {
		t.Fatalf("path = %q, want the recorded path", got.GetPromptsDirMissing().GetPath())
	}
}

func TestDaemonFaultCarriesTheExpiredWorkspaceRef(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindAdoptionWindowExpired,
		Evidence: map[string]string{"workspace": "w1", "workspace_dir": "/tree/w1"},
	}

	// Act.
	got := daemonFault(fault)

	// Assert.
	ref := got.GetAdoptionWindowExpired().GetWorkspace()
	if ref.GetId() != "w1" || ref.GetDir() != "/tree/w1" {
		t.Fatalf("workspace ref = %v, want the recorded id and dir", ref)
	}
}

func TestDaemonFaultCarriesTheFailedDeployStep(t *testing.T) {
	tests := []struct {
		name    string
		failure DeployFailure
		want    *agentreplv1.DaemonFaultDeployFailed
	}{
		{
			name:    "a failed build names its step, its output and its log",
			failure: DeployFailure{Step: DeployStepBuild, BuildStep: "webapp", Detail: "tsc: 1 error", Log: "/s/deploy/logs/build-1.log"},
			want: &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Build{
				Build: &agentreplv1.DeployBuildFailed{Step: "webapp", Detail: "tsc: 1 error", Log: "/s/deploy/logs/build-1.log"},
			}},
		},
		{
			name:    "a failed install names the component",
			failure: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "permission denied"},
			want: &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Install{
				Install: &agentreplv1.DeployInstallFailed{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "permission denied"},
			}},
		},
		{
			name:    "a failed service restart names the service",
			failure: DeployFailure{Step: DeployStepRestartServices, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "exit 5"},
			want: &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_RestartServices{
				RestartServices: &agentreplv1.DeployServiceRestartFailed{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Detail: "exit 5"},
			}},
		},
		{
			name:    "a failed rollback",
			failure: DeployFailure{Step: DeployStepRollback, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "EROFS"},
			want: &agentreplv1.DaemonFaultDeployFailed{Step: &agentreplv1.DaemonFaultDeployFailed_Rollback{
				Rollback: &agentreplv1.DeployRollbackFailed{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "EROFS"},
			}},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fault := wsm.Fault{Kind: KindDeployFailed, Detail: "prose", Evidence: tt.failure.Evidence()}

			// Act.
			got := daemonFault(fault).GetDeployFailed()

			// Assert.
			if !proto.Equal(got, tt.want) {
				t.Fatalf("deploy_failed arm = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDaemonFaultOfADeployStepNoArmSpellsKeepsItsDetailLine(t *testing.T) {
	// Arrange: a record whose step the oneof does not spell.
	fault := wsm.Fault{Kind: KindDeployFailed, Detail: "the evidence", Evidence: map[string]string{"step": "layout"}}

	// Act.
	got := daemonFault(fault)

	// Assert.
	if got.GetKind() != nil {
		t.Fatalf("kind = %v, want no typed arm for a step no arm spells", got.GetKind())
	}
	if got.GetDetail() != "deploy_failed: the evidence" {
		t.Fatalf("detail = %q, want the kind and the evidence", got.GetDetail())
	}
}

func TestDaemonFaultOfAnUnknownKindKeepsItsDetailLine(t *testing.T) {
	// Arrange: an unreportable fault is still a fault; it is never dropped.
	fault := wsm.Fault{Kind: "something_nobody_landed", Detail: "the evidence"}

	// Act.
	got := daemonFault(fault)

	// Assert.
	if got.GetKind() != nil {
		t.Fatalf("kind = %v, want no typed arm for an unknown kind", got.GetKind())
	}
	if got.GetDetail() != "something_nobody_landed: the evidence" {
		t.Fatalf("detail = %q, want the kind and the evidence", got.GetDetail())
	}
}

func TestSessionFaultFillsEveryTypedArm(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "shim start failed", kind: KindShimStartFailed},
		{name: "shim died", kind: KindShimDied},
		{name: "link severed", kind: KindLinkSevered},
		{name: "resume failed", kind: KindResumeFailed},
		{name: "bounce died", kind: KindBounceDied},
		{name: "bounce unknown", kind: KindBounceUnknown},
		{name: "classifier failed", kind: KindClassifierFailed},
		{name: "shim reported", kind: KindShimReported},
		{name: "network unreachable, carried as shim reported", kind: KindNetworkUnreachable},
		{name: "conversation abandoned", kind: KindConversationAbandoned},
		{name: "session absent", kind: KindSessionAbsent},
		{name: "watch open refused", kind: KindWatchOpenRefused},
		{name: "daemon state unreadable", kind: KindStateUnreadable},
		{name: "adoption window expired", kind: KindAdoptionWindowExpired},
		{name: "final answer unresolved", kind: KindFinalAnswerUnresolved},
		{name: "vendor start retrying", kind: KindVendorStartRetrying},
		{name: "vendor start rejected", kind: KindVendorStartRejected},
		{name: "vendor start failed", kind: KindVendorStartFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got, ok := sessionFault(wsm.Fault{Kind: tt.kind})
			// Assert.
			if !ok {
				t.Fatalf("sessionFault(%q) withheld a kind that has an arm", tt.kind)
			}
			if got.GetKind() == nil {
				t.Fatalf("sessionFault(%q) left the kind oneof unset", tt.kind)
			}
		})
	}
}

func TestSessionFaultCarriesTheShimStartEvidence(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindShimStartFailed,
		Evidence: map[string]string{"exit_code": "127", "stderr_tail": "node: not found"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatalf("sessionFault(%q) withheld a kind that has an arm", fault.Kind)
	}
	arm := got.GetShimStartFailed()
	if arm.GetExitCode() != 127 || arm.GetStderrTail() != "node: not found" {
		t.Fatalf("shim_start_failed = %v, want exit 127 and the stderr tail", arm)
	}
}

func TestSessionFaultCarriesTheResumeCause(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindResumeFailed, Evidence: map[string]string{"cause": "identity mismatch"}}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatalf("sessionFault(%q) withheld a kind that has an arm", fault.Kind)
	}
	if got.GetResumeFailed().GetCause() != "identity mismatch" {
		t.Fatalf("cause = %q, want the recorded cause", got.GetResumeFailed().GetCause())
	}
}

func TestSessionFaultFallsBackToTheProseDetailForACause(t *testing.T) {
	// Arrange: the record said something, so the arm is not left empty.
	fault := wsm.Fault{Kind: KindResumeFailed, Detail: "the shim refused the resume"}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatalf("sessionFault(%q) withheld a kind that has an arm", fault.Kind)
	}
	if got.GetResumeFailed().GetCause() != "the shim refused the resume" {
		t.Fatalf("cause = %q, want the prose detail", got.GetResumeFailed().GetCause())
	}
}

func TestSessionFaultRespellsAShimReportedFault(t *testing.T) {
	// Arrange: the shim's own component and kind are kept as evidence.
	fault := wsm.Fault{
		Kind:     KindShimReported,
		Evidence: map[string]string{"component": "store", "kind": "store_unreachable"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatalf("sessionFault(%q) withheld a kind that has an arm", fault.Kind)
	}
	arm := got.GetShimReported()
	if arm.GetComponent() != "store" || arm.GetKind() != "store_unreachable" {
		t.Fatalf("shim_reported = %v, want the shim's own component and kind", arm)
	}
}

func TestExitCodeOfAnUnparsableRecordIsZero(t *testing.T) {
	// Arrange: an absent exit code is reported through the detail line, never
	// invented as a number.
	fault := wsm.Fault{Kind: KindShimDied, Evidence: map[string]string{"exit_code": "not a number"}}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatalf("sessionFault(%q) withheld a kind that has an arm", fault.Kind)
	}
	if got.GetShimDied().GetExitCode() != 0 {
		t.Fatalf("exit code = %d, want zero", got.GetShimDied().GetExitCode())
	}
}

// TestSessionAbsentIsWithheldFromTheWire pins that the liveness probe's own
// answer, and it now has its OWN arm to say so with. It was withheld from the
// wire until 2026-09-12, because the oneof had no arm for it and an unset
// oneof is a contract breach the consumer refuses.
func TestSessionAbsentRendersItsOwnArm(t *testing.T) {
	// Arrange: it is the liveness probe's answer, not a fault anyone raised.
	// Act.
	got, ok := sessionFault(wsm.Fault{Kind: KindSessionAbsent, Detail: "no live session"})

	// Assert.
	if !ok || got.GetSessionAbsent() == nil {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the session_absent arm", KindSessionAbsent, got, ok)
	}
}

// TestSessionAbsentIsNoLongerArmless pins that the kind left the armless set
// in the same change that gave it an arm: the two must never disagree, or a
// rendered fault would still be reported as one the wire cannot carry.
func TestSessionAbsentIsNoLongerArmless(t *testing.T) {
	// Arrange. Act. Assert.
	if ArmlessSessionKind(KindSessionAbsent) {
		t.Fatalf("ArmlessSessionKind(%q) = true, want the kind no longer declared armless", KindSessionAbsent)
	}
}

// TestConversationAbandonedCarriesTheAbandonedVendorId pins the fault that
// actually broke a live WatchHostWorkspace push: it stands open for the life of
// a workspace that came up fresh, so the id it abandoned is the whole evidence.
func TestConversationAbandonedCarriesTheAbandonedVendorId(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindConversationAbandoned,
		Detail:   "no transcript",
		Evidence: map[string]string{"vendor_session_id": "sess-abc"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok || got.GetConversationAbandoned().GetVendorSessionId() != "sess-abc" {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the abandoned vendor id", KindConversationAbandoned, got, ok)
	}
}

// TestTheWorkspaceScopedAdoptionExpiryCarriesItsWindow pins the arm the ROLLOUT
// controller's record needs: it opens the expiry against the WORKSPACE, where
// the daemon-health filter drops it, so the session surfaces are the only ones
// that can spell it.
func TestTheWorkspaceScopedAdoptionExpiryCarriesItsWindow(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindAdoptionWindowExpired,
		Evidence: map[string]string{"adoption_window": "30s"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok || got.GetAdoptionWindowExpired().GetAdoptionWindow() != "30s" {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the recorded window", KindAdoptionWindowExpired, got, ok)
	}
}

// TestTheSessionScopedUnreadableStateCarriesItsCause pins that the reporter's
// own fault says WHY the state client refused, rather than only that the
// answer is incomplete.
func TestTheSessionScopedUnreadableStateCarriesItsCause(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindStateUnreadable, Detail: "database is locked"}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok || got.GetDaemonStateUnreadable().GetCause() != "database is locked" {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the refusal's cause", KindStateUnreadable, got, ok)
	}
}

// TestTheLegacyRelaunchSpellingRendersAsResumeFailed pins that a fault recorded
// under the rollout controller's old private spelling still reaches the
// `resume_failed' arm: it names the same class, and standing records carry it.
func TestTheLegacyRelaunchSpellingRendersAsResumeFailed(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindRelaunchResumeFailed, Detail: "the shim refused the resume"}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok || got.GetResumeFailed() == nil {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the resume_failed arm", KindRelaunchResumeFailed, got, ok)
	}
}

// TestAnUnknownKindIsWithheldFromTheWire pins that a kind nobody declared is
// withheld too: the renderer never puts an unset oneof on the wire, whatever
// the reason it could not classify the fault.
func TestAnUnknownKindIsWithheldFromTheWire(t *testing.T) {
	// Arrange. Act.
	got, ok := sessionFault(wsm.Fault{Kind: "something_nobody_landed", Detail: "the evidence"})

	// Assert.
	if ok || got != nil {
		t.Fatalf("sessionFault(unknown) = (%v, %v), want it withheld", got, ok)
	}
}

// TestWatchOpenRefusedIsNotASeveredLink pins that a refused watch open is its
// OWN kind: the shim answered the open, so nothing about the link was lost and
// the severed arm must not be what a reader sees.
func TestWatchOpenRefusedIsNotASeveredLink(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindWatchOpenRefused,
		Detail:   "no such agent",
		Evidence: map[string]string{"operation": "watch_agent", "handle": "sub-1"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert: it renders its OWN arm, so in particular it is not the severed one.
	if !ok || got.GetLinkSevered() != nil || got.GetWatchOpenRefused() == nil {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the watch_open_refused arm and no severed-link arm",
			KindWatchOpenRefused, got, ok)
	}
}

// TestWatchOpenRefusedCarriesTheRefusedHandle pins the evidence that tells one
// refused open from another: which operation asked, and for what handle.
func TestWatchOpenRefusedCarriesTheRefusedHandle(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{
		Kind:     KindWatchOpenRefused,
		Evidence: map[string]string{"operation": "watch_agent", "handle": "sub-1"},
	}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	arm := got.GetWatchOpenRefused()
	if !ok || arm.GetOperation() != "watch_agent" || arm.GetHandle() != "sub-1" {
		t.Fatalf("sessionFault(%q) = (%v, %v), want the refused operation and handle", KindWatchOpenRefused, got, ok)
	}
}

func TestSelfCheckFaultNamesTheUnreadableState(t *testing.T) {
	// Arrange. Act.
	got := selfCheckFault(errors.New("database is locked"))

	// Assert.
	if !strings.HasPrefix(got.GetDetail(), KindStateUnreadable) {
		t.Fatalf("detail = %q, want it to lead with %s", got.GetDetail(), KindStateUnreadable)
	}
}

// TestSelfCheckFaultCarriesItsTypedArm pins the end of the last site that put
// a fault on the wire with the `kind' oneof unset. The one fault the daemon
// can always detect about itself was the one no consumer would accept.
func TestSelfCheckFaultCarriesItsTypedArm(t *testing.T) {
	// Arrange. Act.
	got := selfCheckFault(errors.New("database is locked"))

	// Assert.
	if got.GetDaemonStateUnreadable().GetCause() != "database is locked" {
		t.Fatalf("kind = %v, want the daemon_state_unreadable arm carrying the cause", got.GetKind())
	}
}

// TestDaemonFaultRendersTheUnreadableStateArm pins that a RECORDED fault of the
// same kind reaches the same arm as the synthetic self-check one, so the two
// paths cannot spell one condition two ways.
func TestDaemonFaultRendersTheUnreadableStateArm(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindStateUnreadable, Detail: "database is locked"}

	// Act.
	got := daemonFault(fault)

	// Assert.
	if got.GetDaemonStateUnreadable().GetCause() != "database is locked" {
		t.Fatalf("kind = %v, want the daemon_state_unreadable arm carrying the cause", got.GetKind())
	}
}

func TestFaultDetailOmitsAnEmptyProseDetail(t *testing.T) {
	// Arrange. Act.
	got := faultDetail(wsm.Fault{Kind: KindLinkSevered})

	// Assert.
	if got != KindLinkSevered {
		t.Fatalf("faultDetail() = %q, want the bare kind", got)
	}
}

// The `detail` arms take the RECORDED evidence when there is one, exactly as
// the `cause` arms do — the fault's prose detail is the fallback, never the
// preference.
func TestDetailArmPrefersTheRecordedEvidenceOverTheProse(t *testing.T) {
	tests := []struct {
		name   string
		fault  wsm.Fault
		detail func(*agentreplv1.DaemonFault) string
		want   string
	}{
		{
			name: "successor spawn, evidence recorded",
			fault: wsm.Fault{
				Kind:     KindSuccessorSpawnFailed,
				Detail:   "the prose",
				Evidence: map[string]string{"detail": "fork failed"},
			},
			detail: func(f *agentreplv1.DaemonFault) string { return f.GetSuccessorSpawnFailed().GetDetail() },
			want:   "fork failed",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := daemonFault(tc.fault)

			// Assert.
			if tc.detail(got) != tc.want {
				t.Fatalf("detail = %q, want %q", tc.detail(got), tc.want)
			}
		})
	}
}

func TestStartFailedDetailComposesTheLineFromTheFaultsOwnEvidence(t *testing.T) {
	cases := []struct {
		name  string
		fault wsm.Fault
		want  string
	}{
		{
			name: "a failed spawn names the exit code and the tail's last line",
			fault: wsm.Fault{
				Kind:     KindShimStartFailed,
				Detail:   "the workspace's shim would not come up",
				Evidence: map[string]string{"exit_code": "1", "stderr_tail": "starting\nError: Cannot find module\n"},
			},
			want: "exit 1: Error: Cannot find module",
		},
		{
			name: "a spawn death with no stderr names the exit code alone",
			fault: wsm.Fault{
				Kind:     KindShimStartFailed,
				Detail:   "the workspace's shim would not come up",
				Evidence: map[string]string{"exit_code": "9", "stderr_tail": ""},
			},
			want: "exit 9",
		},
		{
			name: "a failed adoption names the refusal itself",
			fault: wsm.Fault{
				Kind:     KindShimStartFailed,
				Detail:   "the workspace's shim would not come up",
				Evidence: map[string]string{"stderr_tail": "the lock's owner is unreachable"},
			},
			want: "the lock's owner is unreachable",
		},
		{
			name: "a failure with no evidence falls back to the fault's own detail",
			fault: wsm.Fault{
				Kind:   KindShimStartFailed,
				Detail: "the workspace's shim would not come up",
			},
			want: "the workspace's shim would not come up",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act
			got := StartFailedDetail(tc.fault)

			// Assert
			if got != tc.want {
				t.Fatalf("StartFailedDetail = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestFinalAnswerUnresolvedCarriesTheTurnTheUnitAndTheWhy pins that the arm
// carries what the reader needs to tell the three cases apart: `why` IS the
// substatus for this kind, and the footer's substatus cell never holds it.
func TestFinalAnswerUnresolvedCarriesTheTurnTheUnitAndTheWhy(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindFinalAnswerUnresolved, Evidence: map[string]string{
		"turn": "turn-7", "unit": "msg_01:0", "why": "answer_row_unresolved",
	}}

	// Act.
	got, ok := sessionFault(fault)

	// Assert.
	if !ok {
		t.Fatal("sessionFault withheld final_answer_unresolved from the wire")
	}
	arm := got.GetFinalAnswerUnresolved()
	if arm == nil {
		t.Fatalf("sessionFault rendered %T, want the final-answer arm", got.GetKind())
	}
	if arm.GetTurn() != "turn-7" || arm.GetUnit() != "msg_01:0" || arm.GetWhy() != "answer_row_unresolved" {
		t.Fatalf("arm = %+v, want the recorded turn, unit and why", arm)
	}
}

func TestDaemonFaultOfIsTheHealthAnswersOwnRendering(t *testing.T) {
	// Arrange
	fault := wsm.Fault{
		Kind:     KindDeployFailed,
		Evidence: DeployFailure{Step: DeployStepInstall, Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "EACCES", Rollback: RollbackRestored}.Evidence(),
	}

	// Act
	got := DaemonFaultOf(fault)

	// Assert
	if want := daemonFault(fault); !proto.Equal(got, want) {
		t.Fatalf("DaemonFaultOf = %v, want DaemonHealth's own rendering %v", got, want)
	}
}

// vendorEvidence is a vendor-start fault's recorded evidence: the third failed
// attempt of a run that began at 1700000000000 ms.
func vendorEvidence(kind string) wsm.Fault {
	return wsm.Fault{Kind: kind, Evidence: map[string]string{
		EvidenceFailedAttempts: "3",
		EvidenceCause:          "supportedModels did not answer in 3s",
		EvidenceFailingSinceMs: "1700000000000",
	}}
}

func TestSessionFaultCarriesTheVendorRetryEvidence(t *testing.T) {
	// Arrange
	f := vendorEvidence(KindVendorStartRetrying)

	// Act
	got, _ := sessionFault(f)

	// Assert
	arm := got.GetVendorStartRetrying()
	if arm.GetFailedAttempts() != 3 || arm.GetCause() != "supportedModels did not answer in 3s" || arm.GetFailingSinceMs() != 1700000000000 {
		t.Fatalf("vendor_start_retrying = %+v, want attempts 3, the cause and the run's anchor", arm)
	}
}

func TestSessionFaultCarriesTheVendorRejectionCause(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: KindVendorStartRejected, Evidence: map[string]string{EvidenceCause: "invalid api key"}}

	// Act
	got, _ := sessionFault(f)

	// Assert
	if cause := got.GetVendorStartRejected().GetCause(); cause != "invalid api key" {
		t.Fatalf("vendor_start_rejected.cause = %q, want the shim's account", cause)
	}
}

func TestSessionFaultCarriesTheVendorFailedEvidenceAsTheLastCause(t *testing.T) {
	// Arrange
	f := vendorEvidence(KindVendorStartFailed)

	// Act
	got, _ := sessionFault(f)

	// Assert
	arm := got.GetVendorStartFailed()
	if arm.GetFailedAttempts() != 3 || arm.GetLastCause() != "supportedModels did not answer in 3s" || arm.GetFailingSinceMs() != 1700000000000 {
		t.Fatalf("vendor_start_failed = %+v, want attempts 3, the last cause and the run's anchor", arm)
	}
}

func TestVendorStartLineComposesEachSentence(t *testing.T) {
	tests := []struct {
		name string
		kind string
		want string
	}{
		{"retrying", KindVendorStartRetrying, "Claude SDK did not start (attempt 3): supportedModels did not answer in 3s · retrying"},
		{"rejected", KindVendorStartRejected, "Claude SDK refused to start: supportedModels did not answer in 3s · restart: SPC o C-c"},
		{"failed", KindVendorStartFailed, "Claude SDK failed to start · restart: SPC o C-c"},
		{"not a vendor kind", KindResumeFailed, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := vendorEvidence(tt.kind)

			// Act
			got := VendorStartLine(f)

			// Assert
			if got != tt.want {
				t.Fatalf("VendorStartLine = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestFaultLineDetailIsTheVendorStartLine(t *testing.T) {
	// Arrange
	f := vendorEvidence(KindVendorStartFailed)

	// Act
	got := FaultLineDetail(f)

	// Assert
	if want := "Claude SDK failed to start · restart: SPC o C-c"; got != want {
		t.Fatalf("FaultLineDetail = %q, want %q", got, want)
	}
}
