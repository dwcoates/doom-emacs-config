package footer

import (
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// faultOf is the fault the health package would hand the footer for one kind,
// composed through the REAL partition rather than a table copied into the
// test — a copy would pass while the two drifted apart.
func faultOf(t *testing.T, id, kind string, daemonScope bool) Fault {
	t.Helper()
	cell, ok := health.FaultFooterCell(kind, daemonScope)
	if !ok {
		t.Fatalf("health.FaultFooterCell(%q) has no cell", kind)
	}
	record := wsm.Fault{Kind: kind, Detail: "the record's own prose"}
	return Fault{
		ID: id, Kind: kind, Status: string(cell.Status), SubStatus: cell.SubStatus,
		Detail: health.FaultLineDetail(record), At: instant,
	}
}

// disconnectedStep names the drawn disconnected substatus.
func disconnectedStep(status *frontendv1.FooterStatus) string {
	switch status.GetDisconnected().GetSubstatus().(type) {
	case *frontendv1.FooterStatusDisconnected_Starting:
		return "starting"
	case *frontendv1.FooterStatusDisconnected_Degraded:
		return "degraded"
	case *frontendv1.FooterStatusDisconnected_Severed:
		return "severed"
	case *frontendv1.FooterStatusDisconnected_Dead:
		return "dead"
	case *frontendv1.FooterStatusDisconnected_StartFailed:
		return "start_failed"
	default:
		return ""
	}
}

// TestEveryFaultKindReachesTheStrip is the owner's ruling as a table: one row
// per fault kind, asserting the status, the substatus and the activity kind
// the strip draws while it stands.
func TestEveryFaultKindReachesTheStrip(t *testing.T) {
	tests := []struct {
		name          string
		kind          string
		daemonScope   bool
		wantStatus    string
		wantSubStatus string
	}{
		{"a shim that would not come up", health.KindShimStartFailed, false, "disconnected", "start_failed"},
		{"a resume the shim refused", health.KindResumeFailed, false, "disconnected", "start_failed"},
		{"the legacy relaunch spelling", health.KindRelaunchResumeFailed, false, "disconnected", "start_failed"},
		{"an adoption window nobody claimed", health.KindAdoptionWindowExpired, false, "disconnected", "start_failed"},
		{"a cold gate whose re-open failed", health.KindColdGateReopenFailed, false, "disconnected", "start_failed"},
		{"a shim that exited while serving", health.KindShimDied, false, "disconnected", "dead"},
		{"a bounce whose replacement died", health.KindBounceDied, false, "disconnected", "dead"},
		{"a workspace with no live session", health.KindSessionAbsent, false, "disconnected", "dead"},
		{"a link that stopped serving", health.KindLinkSevered, false, "disconnected", "severed"},
		{"a watch open the shim refused", health.KindWatchOpenRefused, false, "disconnected", "severed"},
		{"a state client that will not answer", health.KindStateUnreadable, false, "blocked", "daemon_impaired"},
		{"an absent prompts directory", health.KindPromptsDirMissing, true, "blocked", "daemon_impaired"},
		{"a state client that opened read-only", health.KindWsmReadOnly, true, "blocked", "daemon_impaired"},
		{"a durable sink that cannot be written", health.KindLogSinkPoisoned, true, "blocked", "daemon_impaired"},
		{"a successor that would not start", health.KindSuccessorSpawnFailed, true, "blocked", "daemon_impaired"},
		{"a fault the shim reported about itself", health.KindShimReported, false, "idle", ""},
		{"a headless classifier run that failed", health.KindClassifierFailed, false, "idle", ""},
		{"a bounce disposition needing a human", health.KindBounceUnknown, false, "idle", ""},
		{"a conversation a fresh bring-up left behind", health.KindConversationAbandoned, false, "idle", ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			ws := ids.WorkspaceID("")
			if !tt.daemonScope {
				ws = testWS
			}

			// Act
			h.r.OpenFault(ws, faultOf(t, "fault-1", tt.kind, tt.daemonScope))

			// Assert
			view := h.view(t)
			if got := statusName(view.GetStrip().GetStatus()); got != tt.wantStatus {
				t.Fatalf("status = %q, want %q", got, tt.wantStatus)
			}
			switch tt.wantStatus {
			case "disconnected":
				if got := disconnectedStep(view.GetStrip().GetStatus()); got != tt.wantSubStatus {
					t.Fatalf("substatus = %q, want %q", got, tt.wantSubStatus)
				}
			case "blocked":
				if view.GetStrip().GetStatus().GetBlocked().GetDaemonImpaired() == nil {
					t.Fatalf("substatus = %+v, want daemon_impaired",
						view.GetStrip().GetStatus().GetBlocked().GetSubstatus())
				}
			}
		})
	}
}

// TestEveryFaultKindDrawsItsActivityLine is the third resolution level: the
// activity cell names the kind and carries its detail, under whatever status
// the fault landed on.
func TestEveryFaultKindDrawsItsActivityLine(t *testing.T) {
	tests := []struct {
		name        string
		kind        string
		daemonScope bool
		line        func(*frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault
	}{
		{"a resume the shim refused", health.KindResumeFailed, false,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetDisconnected().GetActivity().GetSalient().GetFault()
			}},
		{"a state client that will not answer", health.KindStateUnreadable, false,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetBlocked().GetActivity().GetSalient().GetFault()
			}},
		{"an absent prompts directory", health.KindPromptsDirMissing, true,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetBlocked().GetActivity().GetSalient().GetFault()
			}},
		{"a conversation a fresh bring-up left behind", health.KindConversationAbandoned, false,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetIdle().GetActivity().GetUnpinned().GetTransient().GetFault()
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			ws := ids.WorkspaceID("")
			if !tt.daemonScope {
				ws = testWS
			}

			// Act
			h.r.OpenFault(ws, faultOf(t, "fault-1", tt.kind, tt.daemonScope))

			// Assert
			line := tt.line(h.view(t).GetStrip().GetStatus())
			if line == nil {
				t.Fatalf("no fault activity was drawn for %q", tt.kind)
			}
			if line.GetKind() != tt.kind {
				t.Fatalf("activity kind = %q, want %q", line.GetKind(), tt.kind)
			}
			if want := "the record's own prose"; line.GetDetail() != want {
				t.Fatalf("activity detail = %q, want %q", line.GetDetail(), want)
			}
		})
	}
}

// TestClosingAFaultReturnsTheStripToTheSessionsOwnStatus is the other half of
// the ruling: a fault that is retracted stops being drawn, and the strip goes
// back to what the session itself was doing.
func TestClosingAFaultReturnsTheStripToTheSessionsOwnStatus(t *testing.T) {
	tests := []struct {
		name        string
		kind        string
		daemonScope bool
	}{
		{"a resume the shim refused", health.KindResumeFailed, false},
		{"a shim that exited while serving", health.KindShimDied, false},
		{"a link that stopped serving", health.KindLinkSevered, false},
		{"a state client that will not answer", health.KindStateUnreadable, false},
		{"an absent prompts directory", health.KindPromptsDirMissing, true},
		{"a conversation a fresh bring-up left behind", health.KindConversationAbandoned, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			ws := ids.WorkspaceID("")
			if !tt.daemonScope {
				ws = testWS
			}
			h.r.OpenFault(ws, faultOf(t, "fault-1", tt.kind, tt.daemonScope))

			// Act
			h.r.CloseFault(ws, "fault-1")

			// Assert
			view := h.view(t)
			if got := statusName(view.GetStrip().GetStatus()); got != "idle" {
				t.Fatalf("status after the fault closed = %q, want idle", got)
			}
			if view.GetStrip().GetStatus().GetIdle().GetActivity().GetSalient() != nil {
				t.Fatalf("a retracted fault is still drawn")
			}
		})
	}
}

func TestTheStrongestStandingFaultIsTheOneDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OpenFault(testWS, faultOf(t, "weak", health.KindClassifierFailed, false))

	// Act
	h.r.OpenFault(testWS, faultOf(t, "strong", health.KindShimDied, false))

	// Assert
	view := h.view(t)
	if got := disconnectedStep(view.GetStrip().GetStatus()); got != "dead" {
		t.Fatalf("substatus = %q, want dead: the stronger fault claims the strip", got)
	}
	if got := view.GetStrip().GetStatus().GetDisconnected().GetActivity().GetSalient().GetFault().GetKind(); got != health.KindShimDied {
		t.Fatalf("activity kind = %q, want %q", got, health.KindShimDied)
	}
}

func TestAmongEqualFaultsTheNewestIsDrawn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	older := faultOf(t, "older", health.KindShimDied, false)
	newer := faultOf(t, "newer", health.KindBounceDied, false)
	newer.At = instant.Add(time.Minute)
	h.r.OpenFault(testWS, older)

	// Act
	h.r.OpenFault(testWS, newer)

	// Assert
	got := h.view(t).GetStrip().GetStatus().GetDisconnected().GetActivity().GetSalient().GetFault().GetKind()
	if got != health.KindBounceDied {
		t.Fatalf("activity kind = %q, want %q: the newest evidence is the live one", got, health.KindBounceDied)
	}
}

func TestTheLinkStateOutranksAFaultForTheDisconnectedStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.OpenFault(testWS, faultOf(t, "fault-1", health.KindShimDied, false))

	// Act — the link is DIALING, which is the live truth about it
	h.r.OnLink(testWS, shimclient.LinkDialing)

	// Assert
	if got := disconnectedStep(h.view(t).GetStrip().GetStatus()); got != "starting" {
		t.Fatalf("substatus = %q, want starting: the link state is the live truth about the link", got)
	}
}

func TestADaemonScopedFaultStandsOnEveryWorkspacesStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	second := ids.WorkspaceID("ws-second")
	if err := h.r.SetWorkspaceDir(second, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.SetParticipants(second, true, true)

	// Act
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindPromptsDirMissing, true))

	// Assert
	for _, ws := range []ids.WorkspaceID{testWS, second} {
		view, ok := h.r.Topic(ws).Latest()
		if !ok {
			t.Fatalf("workspace %q published no view", ws)
		}
		if got := statusName(view.GetStrip().GetStatus()); got != "blocked" {
			t.Fatalf("workspace %q status = %q, want blocked", ws, got)
		}
	}
}

func TestAFaultReopenedUnderTheSameIdRefreshesItsLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	first := faultOf(t, "fault-1", health.KindResumeFailed, false)
	h.r.OpenFault(testWS, first)
	second := first
	second.Detail = "a second account of the same failure"

	// Act
	h.r.OpenFault(testWS, second)

	// Assert
	line := h.view(t).GetStrip().GetStatus().GetDisconnected().GetActivity().GetSalient().GetFault()
	if line.GetDetail() != second.Detail {
		t.Fatalf("detail = %q, want %q", line.GetDetail(), second.Detail)
	}
	if n := len(h.r.states[testWS].faults); n != 1 {
		t.Fatalf("%d faults stand, want 1: the same record must not stand twice", n)
	}
}

func TestANonEscalatingFaultIsAnnouncedAndNeverStands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OpenFault(testWS, faultOf(t, "fault-1", health.KindConversationAbandoned, false))

	// Assert
	if got := transientOf(t, h).GetFault().GetKind(); got != health.KindConversationAbandoned {
		t.Fatalf("transient fault kind = %q, want %q", got, health.KindConversationAbandoned)
	}
	if n := len(h.r.states[testWS].faults); n != 0 {
		t.Fatalf("%d faults stand, want none: a non-escalating fault is an event", n)
	}
}

func TestADaemonScopedNonEscalatingFaultIsAnnouncedOnEveryStrip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	second := ids.WorkspaceID("ws-second")
	if err := h.r.SetWorkspaceDir(second, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.SetParticipants(second, true, true)
	h.r.OnLink(second, shimclient.LinkConnected)

	// Act
	h.r.OpenFault("", faultOf(t, "fault-1", health.KindDeployFailed, true))

	// Assert
	for _, ws := range []ids.WorkspaceID{testWS, second} {
		view, _ := h.r.Topic(ws).Latest()
		if got := unpinnedOf(view.GetStrip().GetStatus()).GetTransient().GetFault().GetKind(); got != health.KindDeployFailed {
			t.Fatalf("workspace %q transient fault = %q, want %q", ws, got, health.KindDeployFailed)
		}
	}
	if n := len(h.r.daemonFaults); n != 0 {
		t.Fatalf("%d daemon faults stand, want none", n)
	}
}
