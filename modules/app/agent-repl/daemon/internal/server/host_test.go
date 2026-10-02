package server

import (
	"context"
	"errors"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
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

// TestAFaultKindWithNoTypedArmIsWithheldFromTheView pins that a fault the
// oneof spells no arm for does not reach the view at all.
//
// This test previously pinned the opposite -- the fault carried on the wire
// with its detail and the `kind' oneof left unset. That is a CONTRACT BREACH
// Emacs refuses, and it refuses the WHOLE push with it, so the one untyped
// fault cost the editor every host view of the workspace. The invariant
// supersedes the old pin: a fault the wire cannot carry is withheld, and it
// stays recorded, loudly, at the site that opened it.
//
// The kind it is driven with is a RECONCILED BOUNCE DISPOSITION, which is
// armless BY DESIGN -- per-session accounting, never a standing condition a
// host view should draw. It used to be driven with `session_absent', which
// landed its own arm on 2026-09-12.
func TestAFaultKindWithNoTypedArmIsWithheldFromTheView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{{Kind: "bounce_disposition", Detail: "reconciled"}}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if len(faults) != 0 {
		t.Fatalf("faults = %v, want the armless fault withheld", faults)
	}
}

// TestAnArmlessFaultDoesNotWithholdTheArmedOnesBesideIt pins that withholding
// is per fault: the one the wire can carry still reaches the view.
func TestAnArmlessFaultDoesNotWithholdTheArmedOnesBesideIt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{
		{Kind: "bounce_disposition", Detail: "reconciled"},
		{Kind: health.KindShimDied, Detail: "exited"},
	}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if len(faults) != 1 || faults[0].GetShimDied() == nil {
		t.Fatalf("faults = %v, want only the shim-died fault", faults)
	}
}

// TestAnAbandonedConversationNoLongerCostsTheWholeView pins the end of the
// failure this whole batch exists for: the fault a fresh bring-up leaves
// standing now reaches the view through its own arm, beside every other fault.
func TestAnAbandonedConversationNoLongerCostsTheWholeView(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	h.Health.faults = []wsm.Fault{
		{Kind: health.KindConversationAbandoned, Detail: "no transcript",
			Evidence: map[string]string{"vendor_session_id": "sess-abc"}},
	}

	// Act.
	faults := composeHost(t, h).GetExisting().GetLive().GetFaults()

	// Assert.
	if len(faults) != 1 || faults[0].GetConversationAbandoned().GetVendorSessionId() != "sess-abc" {
		t.Fatalf("faults = %v, want the abandoned conversation carried with its vendor id", faults)
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

// ---- the cancelled publish -----------------------------------------------

// TestPublishHostWorkspaceIsQuietWhenTheStreamsContextIsCancelled pins the
// publish's cancellation arm. The host publish runs under the host watch's own
// context, and an orderly exit cancels it while a publish is still in flight;
// the view is simply not published, and no ERROR is recorded. Any other resolve
// failure still records ERROR.
func TestPublishHostWorkspaceIsQuietWhenTheStreamsContextIsCancelled(t *testing.T) {
	tests := []struct {
		name      string
		resolve   error
		wantQuiet bool
	}{
		{name: "cancelled", resolve: context.Canceled, wantQuiet: true},
		{name: "deadline exceeded", resolve: context.DeadlineExceeded, wantQuiet: true},
		{name: "a genuine workspace-read failure", resolve: errors.New("the state is gone"), wantQuiet: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the WORKSPACE READ is what can refuse now -- resolving a
			// named workspace's SINK is total.
			log := &recordingLogger{}
			h := newHarness(t, func(d *Deps) {
				d.Log = &fakeSurfaces{global: log}
			})
			h.DB.workspaceErr = tc.resolve

			// Act.
			h.Server.(*server).PublishHostWorkspace(context.Background(), testWorkspaceID)

			// Assert.
			errs := log.at("ERROR")
			if tc.wantQuiet {
				if len(errs) != 0 {
					t.Fatalf("recorded %v at ERROR, want none", errs)
				}
				info := log.at("INFO")
				if len(info) != 1 || info[0].Context["stream"] != "WatchHostWorkspace" {
					t.Fatalf("INFO records = %v, want exactly one naming the stream", info)
				}
				return
			}
			if len(errs) != 1 {
				t.Fatalf("ERROR records = %v, want exactly one", errs)
			}
		})
	}
}

// TestComposeHostWorkspaceIsQuietWhenTheStreamsContextIsCancelled pins the same
// distinction one layer down, where the composer's own reads run: a cancelled
// read withholds the view without an ERROR, a broken one still records it.
func TestComposeHostWorkspaceIsQuietWhenTheStreamsContextIsCancelled(t *testing.T) {
	tests := []struct {
		name      string
		read      error
		wantQuiet bool
	}{
		{name: "cancelled", read: context.Canceled, wantQuiet: true},
		{name: "deadline exceeded", read: context.DeadlineExceeded, wantQuiet: true},
		{name: "a genuine read failure", read: errors.New("the database is gone"), wantQuiet: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.DB.sessionErr = tc.read
			log := &recordingLogger{}

			// Act.
			_, ok := h.Server.(*server).composeHostWorkspace(
				context.Background(), log, testWorkspaceID)

			// Assert.
			if ok {
				t.Fatal("the host view was composed from a failed read")
			}
			errs := log.at("ERROR")
			if tc.wantQuiet {
				if len(errs) != 0 {
					t.Fatalf("recorded %v at ERROR, want none", errs)
				}
				info := log.at("INFO")
				if len(info) != 1 || info[0].Context["stream"] != "WatchHostWorkspace" {
					t.Fatalf("INFO records = %v, want exactly one naming the stream", info)
				}
				return
			}
			if len(errs) != 1 {
				t.Fatalf("ERROR records = %v, want exactly one", errs)
			}
		})
	}
}

// ---- the missing-host-identity startup transient --------------------------

// awaitingIdentityDebug answers the compose_host_workspace DEBUG record that
// narrates a withheld view awaiting host identity, or nil when none was logged.
func awaitingIdentityDebug(log *recordingLogger) *logRecord {
	for i, rec := range log.records {
		if rec.Level == "DEBUG" && rec.Context["reason"] == "host session id not yet minted" {
			return &log.records[i]
		}
	}
	return nil
}

// A session record with no host identity, right after WatchHostWorkspace
// subscribes, is a STARTUP TRANSIENT: the shim has not described the session
// yet, so the FIRST withholding is DEBUG rather than ERROR.
func TestAMissingHostIdentityIsATransientOnTheFirstWithholding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)
	log := &recordingLogger{}

	// Act.
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("a session with no host identity composed a view, want it withheld")
	}
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("the first withholding was recorded at ERROR: %v", errs)
	}
	if awaitingIdentityDebug(log) == nil {
		t.Fatalf("the first withholding did not narrate the transient at DEBUG: %v", log.records)
	}
}

// The same missing identity escalates to ERROR once it has PERSISTED past the
// describe bound: a session that is still identity-less that long after
// subscription is the genuine mint defect the ERROR names.
func TestAPersistentMissingHostIdentityEscalatesToError(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)
	current := time.Unix(1_700_000_000, 0)
	surface.now = func() time.Time { return current }

	// Act: a first withholding opens the transient window, then time passes
	// beyond the bound before a second withholding.
	surface.composeHostWorkspace(context.Background(), &recordingLogger{}, testWorkspaceID)
	current = current.Add(hostIdentityDescribeBound + time.Second)
	log := &recordingLogger{}
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("a persistently identity-less session composed a view, want it withheld")
	}
	errs := log.at("ERROR")
	if len(errs) != 1 || errs[0].Context["invariant_violation"] != "a session record exists with no host session id" {
		t.Fatalf("a persistent missing identity did not escalate to the mint-defect ERROR: %v", log.records)
	}
	if awaitingIdentityDebug(log) != nil {
		t.Fatalf("an escalated withholding also narrated a transient DEBUG: %v", log.records)
	}
}

// A missing identity that stays within the bound is STILL a transient on later
// withholdings, not only the first: escalation is about persistence, not count.
func TestAMissingHostIdentityWithinTheBoundStaysATransient(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)
	current := time.Unix(1_700_000_000, 0)
	surface.now = func() time.Time { return current }

	// Act.
	surface.composeHostWorkspace(context.Background(), &recordingLogger{}, testWorkspaceID)
	current = current.Add(hostIdentityDescribeBound / 2)
	log := &recordingLogger{}
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert.
	if ok {
		t.Fatal("a still-identity-less session composed a view, want it withheld")
	}
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("a withholding still inside the bound escalated to ERROR: %v", errs)
	}
	if awaitingIdentityDebug(log) == nil {
		t.Fatalf("a withholding inside the bound did not narrate the transient: %v", log.records)
	}
}

// A host view that composes WITH an identity clears the transient window, so a
// later missing identity opens a fresh one rather than inheriting a stale bound.
func TestComposingAHostIdentityClearsTheTransientWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	surface := h.Server.(*server)
	current := time.Unix(1_700_000_000, 0)
	surface.now = func() time.Time { return current }

	// Act: withhold once (opening the window), then compose a view with a live
	// identity long after the bound would have elapsed, then withhold again.
	surface.composeHostWorkspace(context.Background(), &recordingLogger{}, testWorkspaceID)
	current = current.Add(hostIdentityDescribeBound + time.Second)
	h.Facts.facts[testWorkspaceID] = liveFacts()
	if _, ok := surface.composeHostWorkspace(context.Background(), &recordingLogger{}, testWorkspaceID); !ok {
		t.Fatal("a live session did not compose a view")
	}
	delete(h.Facts.facts, testWorkspaceID)
	log := &recordingLogger{}
	_, ok := surface.composeHostWorkspace(context.Background(), log, testWorkspaceID)

	// Assert: the fresh window makes this withholding a transient again.
	if ok {
		t.Fatal("a session with no host identity composed a view, want it withheld")
	}
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("a withholding after a clear escalated as if the old window survived: %v", errs)
	}
	if awaitingIdentityDebug(log) == nil {
		t.Fatalf("a withholding after a clear did not narrate a fresh transient: %v", log.records)
	}
}

// ---------------------------------------------------------------------------
// HostFault: the arm is the fault class.
// ---------------------------------------------------------------------------

// TestHostFaultFillsEveryTypedArm pins that every kind the oneof spells an arm
// for is rendered through it, so no classifiable fault reaches the wire unset.
func TestHostFaultFillsEveryTypedArm(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "shim start failed", kind: health.KindShimStartFailed},
		{name: "shim died", kind: health.KindShimDied},
		{name: "link severed", kind: health.KindLinkSevered},
		{name: "resume failed", kind: health.KindResumeFailed},
		{name: "the legacy relaunch spelling", kind: health.KindRelaunchResumeFailed},
		{name: "bounce died", kind: health.KindBounceDied},
		{name: "bounce unknown", kind: health.KindBounceUnknown},
		{name: "classifier failed", kind: health.KindClassifierFailed},
		{name: "shim reported", kind: health.KindShimReported},
		{name: "conversation abandoned", kind: health.KindConversationAbandoned},
		{name: "session absent", kind: health.KindSessionAbsent},
		{name: "watch open refused", kind: health.KindWatchOpenRefused},
		{name: "daemon state unreadable", kind: health.KindStateUnreadable},
		{name: "adoption window expired", kind: health.KindAdoptionWindowExpired},
		{name: "final answer unresolved", kind: health.KindFinalAnswerUnresolved},
		{name: "vendor start retrying", kind: health.KindVendorStartRetrying},
		{name: "vendor start rejected", kind: health.KindVendorStartRejected},
		{name: "vendor start failed", kind: health.KindVendorStartFailed},
	}
	for _, tt := range tests {
		tt := tt
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange in the table. Act.
			got, ok := hostFault(wsm.Fault{Kind: tt.kind})
			// Assert.
			if !ok {
				t.Fatalf("hostFault(%q) withheld a kind that has an arm", tt.kind)
			}
			if got.GetKind() == nil {
				t.Fatalf("hostFault(%q) left the kind oneof unset", tt.kind)
			}
		})
	}
}

// TestAnAbandonedConversationReachesTheHostView pins the fault that actually
// broke a live push: it stands open for the whole life of a workspace that
// came up fresh, and rendered with an unset `kind' oneof Emacs refused the
// WHOLE WatchHostWorkspace push, losing the host view with it. It has its own
// arm as of 2026-09-12, and the id it abandoned is the evidence it carries.
func TestAnAbandonedConversationReachesTheHostView(t *testing.T) {
	// Arrange. Act.
	got, ok := hostFault(wsm.Fault{
		Kind:     health.KindConversationAbandoned,
		Detail:   "the recorded conversation had no transcript; the session came up fresh",
		Evidence: map[string]string{"vendor_session_id": "sess-abc"},
	})

	// Assert.
	if !ok || got.GetConversationAbandoned().GetVendorSessionId() != "sess-abc" {
		t.Fatalf("hostFault(%q) = (%v, %v), want the abandoned vendor id",
			health.KindConversationAbandoned, got, ok)
	}
}

// TestARefusedWatchOpenReachesTheHostViewWithItsHandle pins that the host
// stream carries the same evidence SessionHealth does: which operation asked,
// and for what handle.
func TestARefusedWatchOpenReachesTheHostViewWithItsHandle(t *testing.T) {
	// Arrange. Act.
	got, ok := hostFault(wsm.Fault{
		Kind:     health.KindWatchOpenRefused,
		Evidence: map[string]string{"operation": "watch_agent", "handle": "sub-1"},
	})

	// Assert.
	arm := got.GetWatchOpenRefused()
	if !ok || arm.GetOperation() != "watch_agent" || arm.GetHandle() != "sub-1" {
		t.Fatalf("hostFault(%q) = (%v, %v), want the refused operation and handle",
			health.KindWatchOpenRefused, got, ok)
	}
}

// TestAnExpiredAdoptionWindowReachesTheHostViewWithItsWindow pins the arm the
// ROLLOUT controller's record needs: it opens the expiry against the WORKSPACE,
// so the host stream is one of the two surfaces that can spell it at all.
func TestAnExpiredAdoptionWindowReachesTheHostViewWithItsWindow(t *testing.T) {
	// Arrange. Act.
	got, ok := hostFault(wsm.Fault{
		Kind:     health.KindAdoptionWindowExpired,
		Evidence: map[string]string{"adoption_window": "30s"},
	})

	// Assert.
	if !ok || got.GetAdoptionWindowExpired().GetAdoptionWindow() != "30s" {
		t.Fatalf("hostFault(%q) = (%v, %v), want the recorded window",
			health.KindAdoptionWindowExpired, got, ok)
	}
}

// TestAnUnknownFaultKindIsWithheldFromTheHostView pins that a kind nobody
// declared is withheld too: the renderer never puts an unset oneof on the wire.
func TestAnUnknownFaultKindIsWithheldFromTheHostView(t *testing.T) {
	// Arrange. Act.
	got, ok := hostFault(wsm.Fault{Kind: "something_nobody_landed", Detail: "the evidence"})

	// Assert.
	if ok || got != nil {
		t.Fatalf("hostFault(unknown) = (%v, %v), want it withheld from the wire", got, ok)
	}
}

// TestPublishHostWorkspaceSurvivesAWorkspaceThatOwnsNoLogSink pins that
// RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION on the serving path
// too: a directory that cannot host a durable sink no longer WITHHOLDS the
// workspace's host view, and it records no error beside it.
func TestPublishHostWorkspaceSurvivesAWorkspaceThatOwnsNoLogSink(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(d *Deps) {
		d.Log = &fakeSurfaces{global: log, workspaceErr: errors.New("the workspace owns no sink")}
	})
	h.DB.sessions = map[ids.WorkspaceID]wsm.Session{testWorkspaceID: {Workspace: testWorkspaceID}}
	h.Facts.facts[testWorkspaceID] = liveFacts()
	surface := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	view := surface.hostStateTopic(testWorkspaceID).Subscribe(ctx)

	// Act.
	surface.PublishHostWorkspace(context.Background(), testWorkspaceID)

	// Assert.
	select {
	case got := <-view:
		if got.GetExisting() == nil {
			t.Fatalf("published %v, want the existing arm", got)
		}
	case <-time.After(time.Second):
		t.Fatal("no host view was published; an unroutable sink must not withhold one")
	}
	if errs := log.at("ERROR"); len(errs) != 0 {
		t.Fatalf("recorded %v at ERROR, want none: an unroutable sink is an ordinary outcome", errs)
	}
}

// TestAnUnresolvedFinalAnswerReachesTheHostView pins that the host stream
// carries the turn, the unit and the WHY, so a reader of the host view can tell
// a terminal that named nothing from one whose answer resolved to no row.
func TestAnUnresolvedFinalAnswerReachesTheHostView(t *testing.T) {
	// Arrange, Act.
	got, ok := hostFault(wsm.Fault{
		Kind: health.KindFinalAnswerUnresolved,
		Evidence: map[string]string{
			"turn": "turn-7", "unit": "msg_01:0", "why": "stalled",
		},
	})

	// Assert.
	if !ok {
		t.Fatal("hostFault withheld final_answer_unresolved from the host stream")
	}
	arm := got.GetFinalAnswerUnresolved()
	if arm == nil {
		t.Fatalf("hostFault rendered %T, want the final-answer arm", got.GetKind())
	}
	if arm.GetTurn() != "turn-7" || arm.GetUnit() != "msg_01:0" || arm.GetWhy() != "stalled" {
		t.Fatalf("arm = %+v, want the recorded turn, unit and why", arm)
	}
}

// ---- the standing held-prompt edit ------------------------------------------

func TestTheHostViewCarriesTheStandingEdit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.edit = &promptqueue.Edit{Turn: "turn-1", Said: &conversationv1.UserSaid{}, ID: 7}

	// Act.
	view := composeHost(t, h)

	// Assert.
	edit := view.GetHeldPromptEdit()
	if edit.GetTurn().GetValue() != "turn-1" || edit.GetEdit() != 7 || edit.GetSaid() == nil {
		t.Fatalf("held_prompt_edit = %v, want turn-1's edit 7 with its content", edit)
	}
}

func TestTheHostViewCarriesNoEditWhenNoneStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	view := composeHost(t, h)

	// Assert.
	if view.GetHeldPromptEdit() != nil {
		t.Fatalf("held_prompt_edit = %v, want absent", view.GetHeldPromptEdit())
	}
}

func TestTheComposerGateFollowsTheOccupancyLeaseWithNothingParked(t *testing.T) {
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
			name: "a merge holding work is open",
			held: true, holder: wsm.HolderMerge, policy: wsm.PolicyHold,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetOpen() != nil },
		},
		{
			name: "a merge refusing work is merging",
			held: true, holder: wsm.HolderMerge, policy: wsm.PolicyRefuse,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetMerging() != nil },
		},
		{
			name: "a merge lease an older build parked is merging",
			held: true, holder: wsm.HolderMerge, policy: wsm.PolicyParked,
			want: func(l *agentreplv1.HostSessionLive) bool { return l.GetMerging() != nil },
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

func TestHostFaultCarriesTheVendorRetryEvidence(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: health.KindVendorStartRetrying, Evidence: map[string]string{
		health.EvidenceFailedAttempts: "2", health.EvidenceCause: "stream ended before ready",
		health.EvidenceFailingSinceMs: "1700000000000",
	}}

	// Act
	got, _ := hostFault(f)

	// Assert
	arm := got.GetVendorStartRetrying()
	if arm.GetFailedAttempts() != 2 || arm.GetCause() != "stream ended before ready" || arm.GetFailingSinceMs() != 1700000000000 {
		t.Fatalf("vendor_start_retrying = %+v, want the recorded evidence", arm)
	}
}

func TestHostFaultCarriesTheVendorRejectionCause(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: health.KindVendorStartRejected, Evidence: map[string]string{health.EvidenceCause: "model missing"}}

	// Act
	got, _ := hostFault(f)

	// Assert
	if cause := got.GetVendorStartRejected().GetCause(); cause != "model missing" {
		t.Fatalf("vendor_start_rejected.cause = %q, want the recorded cause", cause)
	}
}

func TestHostFaultCarriesTheVendorFailedLastCause(t *testing.T) {
	// Arrange
	f := wsm.Fault{Kind: health.KindVendorStartFailed, Evidence: map[string]string{
		health.EvidenceFailedAttempts: "40", health.EvidenceCause: "liveness timeout",
		health.EvidenceFailingSinceMs: "1700000000000",
	}}

	// Act
	got, _ := hostFault(f)

	// Assert
	arm := got.GetVendorStartFailed()
	if arm.GetFailedAttempts() != 40 || arm.GetLastCause() != "liveness timeout" || arm.GetFailingSinceMs() != 1700000000000 {
		t.Fatalf("vendor_start_failed = %+v, want the recorded evidence", arm)
	}
}
