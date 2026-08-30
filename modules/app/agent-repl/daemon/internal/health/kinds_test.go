package health

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/wsm"
)

func TestDaemonFaultFillsEveryTypedArm(t *testing.T) {
	tests := []struct {
		name  string
		fault wsm.Fault
	}{
		{name: "adoption window expired", fault: wsm.Fault{Kind: KindAdoptionWindowExpired}},
		{name: "log sink poisoned", fault: wsm.Fault{Kind: KindLogSinkPoisoned}},
		{name: "deploy script failed", fault: wsm.Fault{Kind: KindDeployScriptFailed}},
		{name: "successor spawn failed", fault: wsm.Fault{Kind: KindSuccessorSpawnFailed}},
		{name: "prompts dir missing", fault: wsm.Fault{Kind: KindPromptsDirMissing}},
		{name: "wsm read only", fault: wsm.Fault{Kind: KindWsmReadOnly}},
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
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := sessionFault(wsm.Fault{Kind: tt.kind})
			// Assert.
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
	got := sessionFault(fault)

	// Assert.
	arm := got.GetShimStartFailed()
	if arm.GetExitCode() != 127 || arm.GetStderrTail() != "node: not found" {
		t.Fatalf("shim_start_failed = %v, want exit 127 and the stderr tail", arm)
	}
}

func TestSessionFaultCarriesTheResumeCause(t *testing.T) {
	// Arrange.
	fault := wsm.Fault{Kind: KindResumeFailed, Evidence: map[string]string{"cause": "identity mismatch"}}

	// Act.
	got := sessionFault(fault)

	// Assert.
	if got.GetResumeFailed().GetCause() != "identity mismatch" {
		t.Fatalf("cause = %q, want the recorded cause", got.GetResumeFailed().GetCause())
	}
}

func TestSessionFaultFallsBackToTheProseDetailForACause(t *testing.T) {
	// Arrange: the record said something, so the arm is not left empty.
	fault := wsm.Fault{Kind: KindResumeFailed, Detail: "the shim refused the resume"}

	// Act.
	got := sessionFault(fault)

	// Assert.
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
	got := sessionFault(fault)

	// Assert.
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
	got := sessionFault(fault)

	// Assert.
	if got.GetShimDied().GetExitCode() != 0 {
		t.Fatalf("exit code = %d, want zero", got.GetShimDied().GetExitCode())
	}
}

func TestSessionAbsentHasNoTypedArm(t *testing.T) {
	// Arrange: it is the liveness probe's answer, not a fault anyone raised.
	// Act.
	got := sessionFault(wsm.Fault{Kind: KindSessionAbsent, Detail: "no live session"})

	// Assert.
	if got.GetKind() != nil {
		t.Fatalf("kind = %v, want no typed arm", got.GetKind())
	}
	if !strings.HasPrefix(got.GetDetail(), KindSessionAbsent) {
		t.Fatalf("detail = %q, want it to lead with the kind", got.GetDetail())
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

func TestFaultDetailOmitsAnEmptyProseDetail(t *testing.T) {
	// Arrange. Act.
	got := faultDetail(wsm.Fault{Kind: KindLinkSevered})

	// Assert.
	if got != KindLinkSevered {
		t.Fatalf("faultDetail() = %q, want the bare kind", got)
	}
}
