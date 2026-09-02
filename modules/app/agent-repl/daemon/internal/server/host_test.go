package server

import (
	"context"
	"errors"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// liveFacts is the arrangement a live session needs: an identity and a
// generation, without which no `existing` arm can be proven.
func liveFacts() HostFacts {
	return HostFacts{SessionID: "sess-1", Generation: "gen-1", ShimAttached: true}
}

// composeHost arranges a server and composes one host view for the test
// workspace, failing the test when the view was withheld.
func composeHost(t *testing.T, h *harness) *agentreplv1.HostWorkspace {
	t.Helper()
	surface := h.Server.(*server)
	log, err := surface.workspaceLog(context.Background(), "WatchHostWorkspace", testWorkspaceID)
	if err != nil {
		t.Fatalf("resolve the workspace log: %v", err)
	}
	view, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)
	if !ok {
		t.Fatal("the host view was withheld, want a composed view")
	}
	return view
}

// ---- the session arm ------------------------------------------------------

// TestAWorkspaceWithNoSessionComposesTheNoneArm pins the one session arm that
// needs no live facts at all: registered, and no session was ever created.
func TestAWorkspaceWithNoSessionComposesTheNoneArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetNone() == nil {
		t.Fatalf("session arm = %v, want none", view.GetSession())
	}
}

// TestALiveSessionComposesTheExistingLiveArm pins the whole live arm.
func TestALiveSessionComposesTheExistingLiveArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetLive() == nil {
		t.Fatalf("session arm = %v, want existing{live}", view.GetSession())
	}
}

// TestTheExistingArmCarriesTheMintedSessionIdentity pins that the identity
// Emacs correlates by comes from the party that minted it.
func TestTheExistingArmCarriesTheMintedSessionIdentity(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if got := view.GetExisting().GetId().GetValue(); got != "sess-1" {
		t.Fatalf("session id = %q, want the minted identity", got)
	}
}

// TestTheLiveArmCarriesTheControllerGeneration pins the generation, which
// scopes the fault windows below it.
func TestTheLiveArmCarriesTheControllerGeneration(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if got := view.GetExisting().GetLive().GetGeneration().GetValue(); got != "gen-1" {
		t.Fatalf("generation = %q, want the controller generation", got)
	}
}

// TestADetachedShimIsReportedAsUnattached pins the fact that tells "live but
// momentarily unwired" from "up".
func TestADetachedShimIsReportedAsUnattached(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	facts := liveFacts()
	facts.ShimAttached = false
	h.Facts.facts[testWorkspaceID] = facts

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetLive().GetShimAttached() {
		t.Fatal("shim_attached = true, want false while the daemon is between shim starts")
	}
}

// TestATerminalSessionComposesTheTerminalArm pins that the DURABLE record's
// death wins over whatever the fleet still holds: the record is the truth and
// the fleet's entry is what has not been reaped yet.
func TestATerminalSessionComposesTheTerminalArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {
		Workspace: testWorkspaceID,
		Terminal:  &wsm.SessionTerminal{Kind: "killed"},
	}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetTerminal() == nil {
		t.Fatalf("standing = %v, want terminal", view.GetExisting().GetStanding())
	}
}

// TestAKilledSessionIsRehydratable pins that an ordinary death leaves the
// vendor conversation resumable.
func TestAKilledSessionIsRehydratable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {
		Workspace:       testWorkspaceID,
		VendorSessionID: "vendor-1",
		Terminal:        &wsm.SessionTerminal{Kind: "killed"},
	}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if !view.GetExisting().GetTerminal().GetRehydratable() {
		t.Fatal("rehydratable = false for a killed session, want true")
	}
}

// TestADeletedSessionIsNotRehydratable pins the one death that refuses
// resurrection.
func TestADeletedSessionIsNotRehydratable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {
		Workspace:       testWorkspaceID,
		VendorSessionID: "vendor-1",
		Terminal:        &wsm.SessionTerminal{Kind: "deleted"},
	}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetTerminal().GetRehydratable() {
		t.Fatal("rehydratable = true for a deleted session, want false")
	}
}

// ---- withholding rather than inventing ------------------------------------

// TestASessionWithNoFactsWithholdsTheView pins the rule that matters most:
// HostSessionExisting.id is not optional, so a session whose identity this
// daemon does not know yields NO view rather than one with an empty id a
// client would correlate wrongly on.
func TestASessionWithNoFactsWithholdsTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)
	log, err := surface.workspaceLog(context.Background(), "WatchHostWorkspace", testWorkspaceID)
	if err != nil {
		t.Fatalf("resolve the workspace log: %v", err)
	}

	// Act.
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("a session with no facts composed a view, want it withheld")
	}
}

// TestFactsWithNoSessionIdentityWithholdTheView pins the same rule against a
// facts source that answered without minting one.
func TestFactsWithNoSessionIdentityWithholdTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	facts := liveFacts()
	facts.SessionID = ""
	h.Facts.facts[testWorkspaceID] = facts
	surface := h.Server.(*server)
	log, err := surface.workspaceLog(context.Background(), "WatchHostWorkspace", testWorkspaceID)
	if err != nil {
		t.Fatalf("resolve the workspace log: %v", err)
	}

	// Act.
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("facts with no session id composed a view, want it withheld")
	}
}

// TestFactsWithNoGenerationWithholdTheView pins the generation's own presence.
func TestFactsWithNoGenerationWithholdTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	facts := liveFacts()
	facts.Generation = ""
	h.Facts.facts[testWorkspaceID] = facts
	surface := h.Server.(*server)
	log, err := surface.workspaceLog(context.Background(), "WatchHostWorkspace", testWorkspaceID)
	if err != nil {
		t.Fatalf("resolve the workspace log: %v", err)
	}

	// Act.
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("facts with no generation composed a view, want it withheld")
	}
}

// TestAFailingLeaseReadWithholdsTheView pins that the composer never guesses
// the composer gate: Emacs gates its input on it.
func TestAFailingLeaseReadWithholdsTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.DB.leaseErr = errors.New("the state client will not read")
	surface := h.Server.(*server)
	log, err := surface.workspaceLog(context.Background(), "WatchHostWorkspace", testWorkspaceID)
	if err != nil {
		t.Fatalf("resolve the workspace log: %v", err)
	}

	// Act.
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("a failing lease read composed a view, want it withheld")
	}
}

// ---- the composer gate ----------------------------------------------------

// TestTheComposerGateFollowsTheOccupancyLease pins every holder's gate. The
// lease is the one place that knows a session is spoken for, so the gate and
// the prompt queue's refusal cannot disagree.
func TestTheComposerGateFollowsTheOccupancyLease(t *testing.T) {
	tests := []struct {
		name   string
		held   bool
		holder wsm.LeaseHolder
		policy wsm.LeasePolicy
		want   func(*agentreplv1.HostSessionLive) bool
	}{
		{
			name: "no lease is open",
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetOpen() != nil },
		},
		{
			name: "a merge refusing work is merging",
			held: true, holder: wsm.HolderMerge, policy: wsm.PolicyRefuse,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetMerging() != nil },
		},
		{
			name: "a merge parked for guidance is merge_parked",
			held: true, holder: wsm.HolderMerge, policy: wsm.PolicyParked,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetMergeParked() != nil },
		},
		{
			name: "a restart is restarting",
			held: true, holder: wsm.HolderRestart, policy: wsm.PolicyHold,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetRestarting() != nil },
		},
		{
			name: "a drain is draining",
			held: true, holder: wsm.HolderDrain, policy: wsm.PolicyHold,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetDraining() != nil },
		},
		{
			name: "a hibernation is draining",
			held: true, holder: wsm.HolderHibernate, policy: wsm.PolicyHold,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetDraining() != nil },
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
			h.Facts.facts[testWorkspaceID] = liveFacts()
			if test.held {
				h.DB.leases = map[ids.WorkspaceID]wsm.Lease{testWorkspaceID: {
					Workspace: testWorkspaceID, Holder: test.holder, Policy: test.policy,
				}}
			}

			// Act.
			view := composeHost(t, h)

			// Assert.
			if !test.want(view.GetExisting().GetLive()) {
				t.Fatalf("composer = %v, want the %s arm", view.GetExisting().GetLive().GetComposer(), test.name)
			}
		})
	}
}

// ---- naming ---------------------------------------------------------------

// TestNamingCarriesTheDerivedSlug pins what Emacs names buffers from.
func TestNamingCarriesTheDerivedSlug(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	view := composeHost(t, h)

	// Assert.
	if got := view.GetNaming().GetSlug(); got != "test" {
		t.Fatalf("naming.slug = %q, want the workspace's derived name", got)
	}
}

// TestAnUnnamedWorkspaceLeavesTheSlugUnset pins presence over sentinels: the
// slug is OPTIONAL and is unset until the daemon has derived one, never blank.
func TestAnUnnamedWorkspaceLeavesTheSlugUnset(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.workspaces[testWorkspaceID] = wsm.Workspace{ID: testWorkspaceID, Dir: testWorkspaceDir}

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetNaming().Slug != nil {
		t.Fatalf("naming.slug = %v, want it unset rather than blank", view.GetNaming().Slug)
	}
}

// TestTheTitleStaysUnsetUntilTheVendorSuppliesOne pins the other optional.
func TestTheTitleStaysUnsetUntilTheVendorSuppliesOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetNaming().Title != nil {
		t.Fatalf("naming.title = %v, want it unset", view.GetNaming().Title)
	}
}

// ---- the vendor arm -------------------------------------------------------

// TestTheVendorArmCarriesTheConversation pins the identifiers transcripts and
// support reports are correlated by.
func TestTheVendorArmCarriesTheConversation(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {
		Workspace: testWorkspaceID, VendorSessionID: "vendor-1", ConfigDir: "/accounts/a",
	}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	claude := composeHost(t, h).GetExisting().GetLive().GetClaude()

	// Assert.
	if claude.GetSessionId() != "vendor-1" || claude.GetConfigDir() != "/accounts/a" {
		t.Fatalf("vendor arm = %v, want the conversation and its account", claude)
	}
}

// TestTheVendorArmIsUnsetBeforeAConversationExists pins that the arm stays
// unset rather than carrying an empty conversation id.
func TestTheVendorArmIsUnsetBeforeAConversationExists(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetLive().GetVendorInfo() != nil {
		t.Fatalf("vendor_info = %v, want it unset before a conversation exists", view.GetExisting().GetLive().GetVendorInfo())
	}
}

// ---- backfill -------------------------------------------------------------

// TestBackfillRendersEveryState pins the never-blue signal's four arms.
func TestBackfillRendersEveryState(t *testing.T) {
	tests := []struct {
		name  string
		state BackfillState
		want  func(*agentreplv1.HostBackfill) bool
	}{
		{"none", BackfillNone, func(b *agentreplv1.HostBackfill) bool { return b.GetNone() != nil }},
		{"pending", BackfillPending, func(b *agentreplv1.HostBackfill) bool { return b.GetPending() != nil }},
		{"done", BackfillDone, func(b *agentreplv1.HostBackfill) bool { return b.GetDone() != nil }},
		{"failed", BackfillFailed, func(b *agentreplv1.HostBackfill) bool { return b.GetFailed() != nil }},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
			facts := liveFacts()
			facts.Backfill = test.state
			h.Facts.facts[testWorkspaceID] = facts

			// Act.
			view := composeHost(t, h)

			// Assert.
			if !test.want(view.GetExisting().GetLive().GetBackfill()) {
				t.Fatalf("backfill = %v, want %s", view.GetExisting().GetLive().GetBackfill(), test.name)
			}
		})
	}
}

// TestAFailedBackfillCarriesTheSidecarsAccount pins the one arm with a field.
func TestAFailedBackfillCarriesTheSidecarsAccount(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	facts := liveFacts()
	facts.Backfill = BackfillFailed
	facts.BackfillDetail = "line 12 would not parse"
	h.Facts.facts[testWorkspaceID] = facts

	// Act.
	view := composeHost(t, h)

	// Assert.
	if got := view.GetExisting().GetLive().GetBackfill().GetFailed().GetDetail(); got != "line 12 would not parse" {
		t.Fatalf("failed.detail = %q, want the sidecar's account", got)
	}
}

// ---- faults ---------------------------------------------------------------

// TestTheLiveArmCarriesTheWorkspacesOpenFaults pins that the host stream
// reports the same fault classes SessionHealth does, through the same arms.
func TestTheLiveArmCarriesTheWorkspacesOpenFaults(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{{
		Kind:     health.KindLinkSevered,
		Detail:   "the link stopped serving",
		OpenedAt: time.UnixMilli(1700000000000),
	}}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if len(faults) != 1 || faults[0].GetLinkSevered() == nil {
		t.Fatalf("faults = %v, want one link_severed", faults)
	}
}

// TestAFaultCarriesItsOpeningInstant pins the window's start, which is what
// scopes it to the generation above it.
func TestAFaultCarriesItsOpeningInstant(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{{
		Kind: health.KindShimDied, Detail: "exited", OpenedAt: time.UnixMilli(1700000000000),
	}}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if faults[0].GetOpenedAtMs() != 1700000000000 {
		t.Fatalf("opened_at_ms = %d, want the window's start", faults[0].GetOpenedAtMs())
	}
}

// TestAFaultKindWithNoTypedArmKeepsItsDetail pins that an unreportable fault
// is still a fault, exactly as the health reporter treats one.
func TestAFaultKindWithNoTypedArmKeepsItsDetail(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{{Kind: health.KindSessionAbsent, Detail: "no live session"}}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if len(faults) != 1 || faults[0].GetKind() != nil || faults[0].GetDetail() != "no live session" {
		t.Fatalf("faults = %v, want one untyped fault keeping its detail", faults)
	}
}

// TestAFailingFaultReadStillComposesTheView pins the one read whose failure
// does NOT withhold the view: the faults supplement the standing, and a host
// that cannot see them is better off than a host that sees nothing.
func TestAFailingFaultReadStillComposesTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faultsErr = errors.New("the state client will not read")

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetExisting().GetLive() == nil {
		t.Fatalf("view = %v, want the live arm despite the failed fault read", view)
	}
}

// ---- publishing -----------------------------------------------------------

// TestPublishingPutsTheViewOnTheStateTopic pins that the state reaches the
// topic a subscriber replays from.
func TestPublishingPutsTheViewOnTheStateTopic(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)

	// Act.
	surface.PublishHostWorkspace(context.Background(), testWorkspaceID)

	// Assert.
	view, ok := surface.hostStateTopic(testWorkspaceID).Latest()
	if !ok || view.GetNone() == nil {
		t.Fatalf("the state topic's latest = (%v, %v), want the composed view", view, ok)
	}
}

// TestAWithheldViewPublishesNothing pins that the topic is never left holding
// a view the composer refused to build.
func TestAWithheldViewPublishesNothing(t *testing.T) {
	// Arrange: a session record with no facts is the withholding case.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)

	// Act.
	surface.PublishHostWorkspace(context.Background(), testWorkspaceID)

	// Assert.
	if _, ok := surface.hostStateTopic(testWorkspaceID).Latest(); ok {
		t.Fatal("a withheld view reached the state topic")
	}
}

// TestRepublishingAnUnchangedViewIsDropped pins that the edges can call the
// publisher freely: the topic's proto.Equal dedupe means no caller has to
// decide whether its edge actually moved the view.
func TestRepublishingAnUnchangedViewIsDropped(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	surface.PublishHostWorkspace(ctx, testWorkspaceID)
	views := surface.hostStateTopic(testWorkspaceID).Subscribe(ctx)
	<-views

	// Act.
	surface.PublishHostWorkspace(ctx, testWorkspaceID)

	// Assert.
	select {
	case extra := <-views:
		t.Fatalf("an unchanged re-render reached the wire: %v", extra)
	case <-time.After(50 * time.Millisecond):
	}
}

// TestAChangedViewIsPublishedAgain pins the other half: a view that really
// moved does reach the subscriber.
func TestAChangedViewIsPublishedAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	surface := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	surface.PublishHostWorkspace(ctx, testWorkspaceID)
	views := surface.hostStateTopic(testWorkspaceID).Subscribe(ctx)
	<-views

	// Act: the workspace acquires a session.
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	surface.PublishHostWorkspace(ctx, testWorkspaceID)

	// Assert.
	select {
	case got := <-views:
		if got.GetExisting().GetLive() == nil {
			t.Fatalf("the second push = %v, want existing{live}", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("the changed view never reached the subscriber")
	}
}

// TestAHibernatedSessionStaysLiveWithTheShimUnattached pins the one session
// mark that is NOT a death: the idle sweep's park. A prompt brings the session
// straight back, so the host keeps the live arm and only the shim's attachment
// changes — the frontend must not be able to tell a parked workspace from an
// idle one.
func TestAHibernatedSessionStaysLiveWithTheShimUnattached(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {
		Workspace:       testWorkspaceID,
		HostSessionID:   "host-1",
		VendorSessionID: "vendor-1",
		Terminal:        &wsm.SessionTerminal{Kind: "hibernated"},
	}}

	// Act.
	view := composeHost(t, h)

	// Assert.
	live := view.GetExisting().GetLive()
	if live == nil {
		t.Fatalf("standing = %v, want the live arm for a parked session", view.GetExisting().GetStanding())
	}
	if live.GetShimAttached() {
		t.Fatalf("shim_attached = true, want false while the session is parked")
	}
}
