package server

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/rollout"
	"agent-repld/internal/workspace"
)

// TestAdoptWebAnswersNoTransferAnnouncedOnAnOrdinaryBoot pins the project-lead
// ruling: the web side never redials, so EVERY non-handover page boot calls
// AdoptWebWorkspace and gets this answer. It is an answer, not a fault.
func TestAdoptWebAnswersNoTransferAnnouncedOnAnOrdinaryBoot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = rollout.ErrNoTransferAnnounced

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNoTransferAnnounced() == nil {
		t.Fatalf("result = %v, want no_transfer_announced", resp.Msg.GetResult())
	}
}

// TestAdoptWebAnswersNotYetAdoptedWhileAdoptionRuns pins the retry answer the
// page backs off on.
func TestAdoptWebAnswersNotYetAdoptedWhileAdoptionRuns(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = rollout.ErrNotYetAdopted

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetNotYetAdopted() == nil {
		t.Fatalf("result = %v, want not_yet_adopted", resp.Msg.GetResult())
	}
}

// TestAdoptHostAnswersParticipantNotExpected pins the third rollout refusal: a
// caller whose stream was not open at announcement.
func TestAdoptHostAnswersParticipantNotExpected(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptHostErr = rollout.ErrParticipantNotExpected

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetParticipantNotExpected() == nil {
		t.Fatalf("result = %v, want participant_not_expected", resp.Msg.GetResult())
	}
}

// TestAdoptionIsReachableOnAJoiningDaemon pins that the adoption calls do NOT
// refuse on `not_yet_adopted` standing: that is exactly the state they exist to
// leave, and refusing would make the rendezvous unreachable.
func TestAdoptionIsReachableOnAJoiningDaemon(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Ownership.standing = workspace.StandingNotYetAdopted

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success on a joining daemon", resp.Msg.GetResult())
	}
}

// TestAdoptionRefusesAnUnknownWorkspace pins that the rendezvous still keys on a
// registered workspace.
func TestAdoptionRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{
			Workspace: &workspacev1.WorkspaceRef{Id: "ws-nope"},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("result = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// adoptBoth calls one adoption verb for ws and answers its error arm (nil on
// success) and its transport error.
func adoptBoth(h *harness, web bool, ws *workspacev1.WorkspaceRef) (unknown bool, err error) {
	if web {
		resp, err := h.Client.AdoptWebWorkspace(context.Background(),
			connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ws}))
		if err != nil {
			return false, err
		}
		return resp.Msg.GetError().GetUnknownWorkspace() != nil, nil
	}
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ws}))
	if err != nil {
		return false, err
	}
	return resp.Msg.GetError().GetUnknownWorkspace() != nil, nil
}

// TestAnAdoptionWhoseRegistryReadBreaksFailsRatherThanAnsweringUnknown pins
// the 2026-10-06 handover: the successor's registry read for queen-model's
// adopting page met `sql: database is closed`, and the adoption resolver
// answered EVERY read error as unknown_workspace. The page took that for a
// workspace the daemon does not know and failed its boot. A broken read is a
// failure -- internal, recorded at ERROR under the rpc -- and only a read that
// found nothing is unknown_workspace.
func TestAnAdoptionWhoseRegistryReadBreaksFailsRatherThanAnsweringUnknown(t *testing.T) {
	tests := []struct {
		name string
		web  bool
		rpc  string
	}{
		{name: "the page's adoption", web: true, rpc: "AdoptWebWorkspace"},
		{name: "the host's adoption", web: false, rpc: "AdoptHostWorkspace"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			// A JOINING daemon: the boundary does not read an unadopted
			// workspace, so the adoption resolver's read is the only one, as
			// it was live.
			h.Ownership.standing = workspace.StandingNotYetAdopted
			h.DB.workspaceErr = errors.New("sql: database is closed")

			// Act.
			unknown, err := adoptBoth(h, tc.web, ref())

			// Assert.
			if unknown {
				t.Fatalf("%s on a broken registry read = unknown_workspace, want a failure", tc.rpc)
			}
			if connect.CodeOf(err) != connect.CodeInternal {
				t.Fatalf("%s on a broken registry read = %v, want internal", tc.rpc, err)
			}
			errs := log.at("ERROR")
			if len(errs) == 0 || errs[0].Operation != tc.rpc {
				t.Fatalf("%s recorded ERRORs %+v, want one under the rpc's own operation", tc.rpc, errs)
			}
		})
	}
}

// TestAnAdoptionOfAnUnknownWorkspaceIsRecordedAtInfo pins that the daemon's
// own unknown_workspace answer to an adoption is visible: an adopt call names
// a workspace a transfer announced, so the registry not knowing it is worth a
// line at the default verbosity. On 2026-10-06 the page recorded the refusal
// and the daemon recorded nothing above DEBUG.
func TestAnAdoptionOfAnUnknownWorkspaceIsRecordedAtInfo(t *testing.T) {
	tests := []struct {
		name string
		web  bool
		rpc  string
	}{
		{name: "the page's adoption", web: true, rpc: "AdoptWebWorkspace"},
		{name: "the host's adoption", web: false, rpc: "AdoptHostWorkspace"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })

			// Act.
			unknown, err := adoptBoth(h, tc.web, &workspacev1.WorkspaceRef{Id: "ws-nope"})

			// Assert.
			if err != nil || !unknown {
				t.Fatalf("%s(ws-nope) = (unknown=%v, %v), want unknown_workspace", tc.rpc, unknown, err)
			}
			var seen bool
			for _, rec := range log.at("INFO") {
				if rec.Operation == tc.rpc && rec.Context["arm"] == workspace.ArmUnknownWorkspace {
					seen = true
				}
			}
			if !seen {
				t.Fatalf("%s(ws-nope) recorded %+v, want an INFO naming the unknown_workspace arm", tc.rpc, log.records)
			}
		})
	}
}

// TestAdoptHostAnswersTransferDeferredForARefusedMidWorkAdoption pins that a
// refused mid-work adoption reaches Emacs as its own arm rather than as the
// not_yet_adopted it wraps, which Emacs retries on a timer.
func TestAdoptHostAnswersTransferDeferredForARefusedMidWorkAdoption(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptHostErr = fmt.Errorf("rollout: adopt %q: %w", "w1", rollout.ErrMidWorkRefused)

	// Act.
	resp, err := h.Client.AdoptHostWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptHostWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetTransferDeferred() == nil {
		t.Fatalf("result = %v, want transfer_deferred", resp.Msg.GetResult())
	}
}

// TestAdoptWebAnswersTransferDeferredForARefusedMidWorkAdoption is the page's
// half of the same arm.
func TestAdoptWebAnswersTransferDeferredForARefusedMidWorkAdoption(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = fmt.Errorf("rollout: adopt %q: %w", "w1", rollout.ErrMidWorkRefused)

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	if resp.Msg.GetError().GetTransferDeferred() == nil {
		t.Fatalf("result = %v, want transfer_deferred", resp.Msg.GetResult())
	}
}

// TestAdoptWebOnTheDaemonThatHandedTheWorkspaceAwayPointsAtTheSuccessor pins
// the lagging page's answer: a page that booted against the departing daemon
// is told where its workspace went, with the successor's address, instead of
// waiting on a rendezvous only the successor completes.
func TestAdoptWebOnTheDaemonThatHandedTheWorkspaceAwayPointsAtTheSuccessor(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Rollout.adoptWebErr = rollout.ErrTransferredAway

	// Act.
	resp, err := h.Client.AdoptWebWorkspace(context.Background(),
		connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("AdoptWebWorkspace: %v", err)
	}
	away := resp.Msg.GetError().GetTransferringAway()
	if away == nil {
		t.Fatalf("result = %v, want transferring_away", resp.Msg.GetResult())
	}
	if away.GetAddress() != "127.0.0.1:9999" {
		t.Fatalf("address = %q, want the successor's 127.0.0.1:9999", away.GetAddress())
	}
}

// TestARolloutRefusalOfAnAdoptionIsRecordedAtInfo pins that the rollout's own
// refusals of an adopt call leave a line at the default verbosity: on
// 2026-10-11 hundreds of the host's not_yet_adopted answers were recorded only
// at DEBUG, so no log said what Emacs kept being told.
func TestARolloutRefusalOfAnAdoptionIsRecordedAtInfo(t *testing.T) {
	tests := []struct {
		name string
		web  bool
		rpc  string
	}{
		{name: "the page's adoption", web: true, rpc: "AdoptWebWorkspace"},
		{name: "the host's adoption", web: false, rpc: "AdoptHostWorkspace"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := &recordingLogger{}
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			h.Rollout.adoptHostErr = rollout.ErrNotYetAdopted
			h.Rollout.adoptWebErr = rollout.ErrNotYetAdopted

			// Act.
			if _, err := adoptBoth(h, tc.web, ref()); err != nil {
				t.Fatalf("%s: %v", tc.rpc, err)
			}

			// Assert.
			var seen bool
			for _, rec := range log.at("INFO") {
				if rec.Operation == tc.rpc && rec.Context["arm"] == "not_yet_adopted" {
					seen = true
				}
			}
			if !seen {
				t.Fatalf("%s recorded %+v, want an INFO naming the not_yet_adopted arm", tc.rpc, log.records)
			}
		})
	}
}
