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

// faultStep names the drawn substatus of whichever fault-domain arm stands.
func faultStep(status *frontendv1.FooterStatus) string {
	switch status.GetVendorFault().GetSubstatus().(type) {
	case *frontendv1.FooterStatusVendorFault_VendorRetry:
		return "vendor_retry"
	case *frontendv1.FooterStatusVendorFault_VendorRejection:
		return "vendor_rejection"
	case *frontendv1.FooterStatusVendorFault_VendorFailed:
		return "vendor_failed"
	}
	if status.GetNetworkFault().GetOffline() != nil {
		return "offline"
	}
	switch status.GetAgentReplFault().GetSubstatus().(type) {
	case *frontendv1.FooterStatusAgentReplFault_DaemonImpaired:
		return "daemon_impaired"
	case *frontendv1.FooterStatusAgentReplFault_Starting:
		return "starting"
	case *frontendv1.FooterStatusAgentReplFault_Degraded:
		return "degraded"
	case *frontendv1.FooterStatusAgentReplFault_Severed:
		return "severed"
	case *frontendv1.FooterStatusAgentReplFault_Dead:
		return "dead"
	case *frontendv1.FooterStatusAgentReplFault_StartFailed:
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
		{"a shim that would not come up", health.KindShimStartFailed, false, "agent_repl_fault", "start_failed"},
		{"a resume the shim refused", health.KindResumeFailed, false, "agent_repl_fault", "start_failed"},
		{"the legacy relaunch spelling", health.KindRelaunchResumeFailed, false, "agent_repl_fault", "start_failed"},
		{"an adoption window nobody claimed", health.KindAdoptionWindowExpired, false, "agent_repl_fault", "start_failed"},
		{"a cold gate whose re-open failed", health.KindColdGateReopenFailed, false, "agent_repl_fault", "start_failed"},
		{"a vendor start being retried", health.KindVendorStartRetrying, false, "vendor_fault", "vendor_retry"},
		{"a vendor start the vendor refused", health.KindVendorStartRejected, false, "vendor_fault", "vendor_rejection"},
		{"a vendor start that failed for the window", health.KindVendorStartFailed, false, "vendor_fault", "vendor_failed"},
		{"the network the shim could not reach", health.KindNetworkUnreachable, false, "network_fault", "offline"},
		{"a shim that exited while serving", health.KindShimDied, false, "agent_repl_fault", "dead"},
		{"a bounce whose replacement died", health.KindBounceDied, false, "agent_repl_fault", "dead"},
		{"a workspace with no live session", health.KindSessionAbsent, false, "agent_repl_fault", "dead"},
		{"a link that stopped serving", health.KindLinkSevered, false, "agent_repl_fault", "severed"},
		{"a watch open the shim refused", health.KindWatchOpenRefused, false, "agent_repl_fault", "severed"},
		{"a state client that will not answer", health.KindStateUnreadable, false, "agent_repl_fault", "daemon_impaired"},
		{"an absent prompts directory", health.KindPromptsDirMissing, true, "agent_repl_fault", "daemon_impaired"},
		{"a state client that opened read-only", health.KindWsmReadOnly, true, "agent_repl_fault", "daemon_impaired"},
		{"a durable sink that cannot be written", health.KindLogSinkPoisoned, true, "agent_repl_fault", "daemon_impaired"},
		{"a successor that would not start", health.KindSuccessorSpawnFailed, true, "agent_repl_fault", "daemon_impaired"},
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
			if got := faultStep(view.GetStrip().GetStatus()); got != tt.wantSubStatus {
				t.Fatalf("substatus = %q, want %q", got, tt.wantSubStatus)
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
				return s.GetAgentReplFault().GetActivity().GetSalient().GetFault()
			}},
		{"a state client that will not answer", health.KindStateUnreadable, false,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetAgentReplFault().GetActivity().GetSalient().GetFault()
			}},
		{"an absent prompts directory", health.KindPromptsDirMissing, true,
			func(s *frontendv1.FooterStatus) *frontendv1.FooterStatusActivityFault {
				return s.GetAgentReplFault().GetActivity().GetSalient().GetFault()
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
	if got := faultStep(view.GetStrip().GetStatus()); got != "dead" {
		t.Fatalf("substatus = %q, want dead: the stronger fault claims the strip", got)
	}
	if got := view.GetStrip().GetStatus().GetAgentReplFault().GetActivity().GetSalient().GetFault().GetKind(); got != health.KindShimDied {
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
	got := h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetActivity().GetSalient().GetFault().GetKind()
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
	if got := faultStep(h.view(t).GetStrip().GetStatus()); got != "starting" {
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
		if got := statusName(view.GetStrip().GetStatus()); got != "agent_repl_fault" {
			t.Fatalf("workspace %q status = %q, want agent_repl_fault", ws, got)
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
	line := h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetActivity().GetSalient().GetFault()
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

// vendorFaultOf is a vendor-start fault as the health package hands it over:
// the line composed out of its evidence.
func vendorFaultOf(t *testing.T, id, kind string) Fault {
	t.Helper()
	cell, _ := health.FaultFooterCell(kind, false)
	record := wsm.Fault{Kind: kind, Evidence: map[string]string{
		health.EvidenceFailedAttempts: "3",
		health.EvidenceCause:          "supportedModels did not answer in 3s",
	}}
	return Fault{
		ID: id, Kind: kind, Status: string(cell.Status), SubStatus: cell.SubStatus,
		Detail: health.FaultLineDetail(record), At: instant,
	}
}

func TestTheVendorStartLineIsDrawnUnderEachVendorStep(t *testing.T) {
	tests := []struct {
		name string
		kind string
		want string
	}{
		{"retrying", health.KindVendorStartRetrying, "Claude SDK did not start (attempt 3): supportedModels did not answer in 3s · retrying"},
		{"rejected", health.KindVendorStartRejected, "Claude SDK refused to start: supportedModels did not answer in 3s · restart: SPC o C-c"},
		{"failed", health.KindVendorStartFailed, "Claude SDK failed to start · restart: SPC o C-c"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			h.r.OpenFault(testWS, vendorFaultOf(t, "fault-1", tt.kind))

			// Assert
			got := h.view(t).GetStrip().GetStatus().GetVendorFault().GetActivity().GetSalient().GetVendorStart().GetText()
			if got != tt.want {
				t.Fatalf("vendor_start text = %q, want %q", got, tt.want)
			}
		})
	}
}

// THE VENDOR STEP OUTRANKS A DEAD LINK THAT NEVER CONNECTED: a spawned shim
// stopped after its vendor failed is not a shim that would not start.
func TestAVendorFaultOutranksADeadLinkThatNeverConnected(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.OnLink(testWS, shimclient.LinkDead)

	// Act
	h.r.OpenFault(testWS, vendorFaultOf(t, "fault-1", health.KindVendorStartFailed))

	// Assert
	if got := faultStep(h.view(t).GetStrip().GetStatus()); got != "vendor_failed" {
		t.Fatalf("substatus = %q, want vendor_failed", got)
	}
}

// THE FAULT DOMAINS ARE RANKED agent_repl_fault > network_fault >
// vendor_fault (owner ruling, 2026-10-02): whichever was opened last, the
// stronger domain is the one the strip draws.
func TestTheFaultDomainsRankAgentReplThenNetworkThenVendor(t *testing.T) {
	tests := []struct {
		name   string
		faults []Fault
		want   string
	}{
		{name: "an agent-repl fault outranks a vendor fault opened after it",
			faults: []Fault{faultOf(t, "severed", health.KindLinkSevered, false), vendorFaultOf(t, "vendor", health.KindVendorStartRetrying)},
			want:   "agent_repl_fault"},
		{name: "an agent-repl fault outranks a network fault opened after it",
			faults: []Fault{faultOf(t, "severed", health.KindLinkSevered, false), faultOf(t, "net", health.KindNetworkUnreachable, false)},
			want:   "agent_repl_fault"},
		{name: "a network fault outranks a vendor fault opened after it",
			faults: []Fault{faultOf(t, "net", health.KindNetworkUnreachable, false), vendorFaultOf(t, "vendor", health.KindVendorStartRetrying)},
			want:   "network_fault"},
		{name: "a network fault outranks a vendor fault opened before it",
			faults: []Fault{vendorFaultOf(t, "vendor", health.KindVendorStartFailed), faultOf(t, "net", health.KindNetworkUnreachable, false)},
			want:   "network_fault"},
		{name: "a daemon-scoped agent-repl fault outranks a network fault",
			faults: []Fault{faultOf(t, "net", health.KindNetworkUnreachable, false), faultOf(t, "impaired", health.KindStateUnreadable, false)},
			want:   "agent_repl_fault"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)

			// Act
			for _, f := range tt.faults {
				h.r.OpenFault(testWS, f)
			}

			// Assert
			if got := statusName(h.view(t).GetStrip().GetStatus()); got != tt.want {
				t.Fatalf("status = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestClosingTheNetworkFaultUncoversTheVendorFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OpenFault(testWS, vendorFaultOf(t, "vendor", health.KindVendorStartRetrying))
	h.r.OpenFault(testWS, faultOf(t, "net", health.KindNetworkUnreachable, false))

	// Act
	h.r.CloseFault(testWS, "net")

	// Assert
	if got := faultStep(h.view(t).GetStrip().GetStatus()); got != "vendor_retry" {
		t.Fatalf("substatus = %q, want vendor_retry once the network is back", got)
	}
}

func TestTheNetworkFaultDrawsTheShimsObservation(t *testing.T) {
	tests := []struct {
		name   string
		detail string
		want   string
	}{
		{name: "the shim's own words", detail: "cannot reach api.anthropic.com: no route to host", want: "cannot reach api.anthropic.com: no route to host"},
		{name: "no words at all still draws a line", detail: "", want: "offline: this machine cannot reach the network"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			f := faultOf(t, "net", health.KindNetworkUnreachable, false)
			f.Detail = tt.detail

			// Act
			h.r.OpenFault(testWS, f)

			// Assert
			got := h.view(t).GetStrip().GetStatus().GetNetworkFault().GetActivity().GetSalient().GetOffline().GetText()
			if got != tt.want {
				t.Fatalf("offline line = %q, want %q", got, tt.want)
			}
		})
	}
}

// A VENDOR FAULT LEAVES A REDIALING LINK ITS CLAIM: a route being redialed is
// agent-repl's own fault, newer than any vendor start.
func TestARedialingLinkOutranksAStandingVendorFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OpenFault(testWS, vendorFaultOf(t, "vendor", health.KindVendorStartRetrying))

	// Act
	h.r.OnLink(testWS, shimclient.LinkRedialing)

	// Assert
	if got := faultStep(h.view(t).GetStrip().GetStatus()); got != "severed" {
		t.Fatalf("substatus = %q, want severed", got)
	}
}
