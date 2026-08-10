package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	frontend "claude-repld/internal/frontend"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// AN UNWIRED WORKSPACE IS CLASSIFIED BY CALLER.
//
// Emacs fans background machinery — resyncs, kept health probes, sweep passes —
// across every workspace it knows about, and after a daemon bounce most of them
// are legitimately unwired. Reporting each of those refusals as a failure put
// dozens of error-shaped lines and nacks in front of a user with nothing to act
// on. Direct user-initiated commands keep the loud nack, because there the
// refusal IS the feedback.
// ---------------------------------------------------------------------------

// noPages supplies the Resyncer half these fakes do not exercise.
//
// Embedded rather than repeated per fake: every one of them is about how a
// REPLAY refusal is classified, and a page they never serve is noise in each
// of them. One copy also means a Resyncer that grows another method fails to
// compile in exactly one place.
type noPages struct{}

func (noPages) ConversationPage(context.Context, string, string, string, sessioncontroller.PageAnchor) (*frontendv1.ConversationPage, error) {
	return nil, errors.New("this fake serves no conversation pages")
}

// unwiredResyncer refuses with the no-live-controller sentinel, exactly as
// sessioncontroller does for a workspace that has not been brought up.
type unwiredResyncer struct{ noPages }

func (unwiredResyncer) ResyncForGeneration(string, string, string, uint64) error {
	return errclass.ErrNoLiveSessionController
}

// brokenResyncer fails for a reason that is nobody's routine expectation.
type brokenResyncer struct{ noPages }

func (brokenResyncer) ResyncForGeneration(string, string, string, uint64) error {
	return errors.New("the retained ring is corrupt")
}

// newResyncHandler builds a handler over one Resyncer, capturing the log.
func newResyncHandler(t *testing.T, r Resyncer, lines *[]string) *commandHandler {
	t.Helper()
	h, err := newCommandHandler(&fakePrompts{}, &fakeMerges{}, &fakeLifecycle{}, r, &fakeSessionCmds{}, nil, nil,
		func(f string, a ...any) { *lines = append(*lines, fmt.Sprintf(f, a...)) })
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	return h
}

// An unwired workspace is NO LONGER a resync exception. Its conversation is
// served from durable store history (sessioncontroller/durablereplay.go), so a
// resyncer that still reports "no live session controller" is reporting that
// the durable path itself could not answer — a failure, not a routine state.
func TestResyncThatCannotServeAnUnwiredWorkspaceNacks(t *testing.T) {
	// Arrange.
	var logged []string
	h := newResyncHandler(t, unwiredResyncer{}, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert — silence would be indistinguishable from an empty conversation.
	if err == nil {
		t.Fatal("Resync that could not serve an unwired workspace returned no error")
	}
}

func TestResyncWithNoResyncerWiredNacks(t *testing.T) {
	// Arrange — the command exists, so something must answer it.
	var logged []string
	h := newResyncHandler(t, nil, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	if err == nil {
		t.Fatal("Resync with no resyncer wired returned no error")
	}
}

func TestAFailedResyncIsLoggedWithItsCause(t *testing.T) {
	// Arrange.
	var logged []string
	h := newResyncHandler(t, brokenResyncer{}, &logged)

	// Act.
	h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	for _, l := range logged {
		if strings.Contains(l, "resync ws=/w request_id=r1 session= generation= from_seq=1 FAILED") && strings.Contains(l, "the retained ring is corrupt") {
			return
		}
	}
	t.Fatalf("no failed-resync line carrying the cause; lines=%v", logged)
}

func TestAGenuineResyncFailureStaysLoud(t *testing.T) {
	// Arrange — nothing here weakens the loud path for a real caller error.
	var logged []string
	h := newResyncHandler(t, brokenResyncer{}, &logged)

	// Act.
	err := h.Resync(context.Background(), "/w", "r1", &frontendv1.ResyncCmd{FromSeq: 1})

	// Assert.
	if err == nil {
		t.Fatal("a genuine resync failure was quieted")
	}
}

type supersededResyncer struct{ noPages }

func (supersededResyncer) ResyncForGeneration(string, string, string, uint64) error {
	return errclass.ErrSessionSuperseded
}

// TestStaleResyncCommandAckIsClassifiedSuperseded pins the ack a stale-fence
// resync carries.
//
// reconnect_superseded is the MORE SPECIFIC TRUE STATEMENT and the one the
// contract defines for exactly this case: a view whose replay would have come
// from a generation it never saw. The resync is also the only command that
// echoes a fence at all, so it is the only place a stale fence refuses
// anything; workspace_not_live is minted from ErrNotLiveSession, which is a
// session-identity mismatch rather than a fence comparison.
func TestStaleResyncCommandAckIsClassifiedSuperseded(t *testing.T) {
	// Arrange.
	var logged []string
	h := newResyncHandler(t, supersededResyncer{}, &logged)
	// Act.
	ack := frontend.Dispatch(context.Background(), func(string, ...any) {}, h, nil, &frontendv1.FrontendCommand{
		RequestId: "r-stale", Workspace: "/w",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{Fence: ssm.Fence("retired", "old")}},
	})
	// Assert.
	if ack.GetOk() {
		t.Fatalf("a stale-fence resync acked ok")
	}
	if errclass.KindName(ack.GetFailure()) != string(errclass.TypeSessionReconnectSuperseded) {
		t.Fatalf("failure type = %q, want %q", errclass.KindName(ack.GetFailure()), errclass.TypeSessionReconnectSuperseded)
	}
}

func TestAStaleResyncOffersTheRemedyItsViewNeeds(t *testing.T) {
	// Arrange — a resync is a view's ONLY recovery mechanism, so a refused one
	// strands the view unless the card tells it to remount.
	var logged []string
	h := newResyncHandler(t, supersededResyncer{}, &logged)
	// Act.
	ack := frontend.Dispatch(context.Background(), func(string, ...any) {}, h, nil, &frontendv1.FrontendCommand{
		RequestId: "r-stale", Workspace: "/w",
		Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{Fence: ssm.Fence("retired", "old")}},
	})
	// Assert.
	if ack.GetFailure().GetReconnectSuperseded().GetRemedy() == "" {
		t.Fatalf("a refused resync offered no remedy")
	}
}

func TestASupersededErrorWithNoRemedyOffersNone(t *testing.T) {
	// Arrange — the same sentinel from a caller that is not a resync has no
	// action to offer, and an invented one would send the user somewhere
	// arbitrary to look useful.
	// Act.
	card := errclass.Command(func(string, ...any) {}, errclass.ErrSessionSuperseded)
	// Assert.
	if got := card.GetKind().GetReconnectSuperseded().GetRemedy(); got != "" {
		t.Fatalf("remedy = %q, want none", got)
	}
}

func TestInterruptOnAnUnwiredWorkspaceStaysLoud(t *testing.T) {
	// Arrange — a DIRECT user action. The user pressed stop; a silent no-op
	// would leave a pressed control doing nothing with no explanation.
	p := &fakePrompts{err: errclass.ErrNoLiveSessionController}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil)
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	err = h.Interrupt(context.Background(), "/w", "r1", &frontendv1.InterruptCmd{ConfirmAgents: true})

	// Assert.
	if err == nil {
		t.Fatal("a user-commanded interrupt on an unwired workspace returned no error")
	}
}

// servingResyncer records what reached the eligibility ladder, and accepts
// everything. It is what makes "refused BEFORE the ladder" provable.
type servingResyncer struct{ entered int }

func (r *servingResyncer) ResyncForGeneration(string, string, string, uint64) error {
	r.entered++
	return nil
}

// TestAResyncEchoingAnUnmintableFenceIsRefusedBeforeTheLadder covers the
// distinction the ladder CANNOT make. An unmintable token and an absent one
// both split to two empty identities; only the token itself says which
// arrived, and a delayed request must not silently rebind itself to the
// current generation.
func TestAResyncEchoingAnUnmintableFenceIsRefusedBeforeTheLadder(t *testing.T) {
	// Arrange
	var logged []string
	resyncer := &servingResyncer{}
	h := newResyncHandler(t, resyncer, &logged)

	// Act
	err := h.Resync(context.Background(), "/w", "r-unmintable", &frontendv1.ResyncCmd{Fence: "no-workspace-ever-held-this"})

	// Assert
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("error = %v, want session superseded", err)
	}
	if resyncer.entered != 0 {
		t.Fatalf("the eligibility ladder ran %d time(s) for an unmintable fence, want 0", resyncer.entered)
	}
}

// TestAResyncCarryingNoFenceAtAllReachesTheLadder is the other half of that
// boundary: a client holding no fence predates fenced chrome, has no identity
// to be stale about, and is served under whatever identity is current.
func TestAResyncCarryingNoFenceAtAllReachesTheLadder(t *testing.T) {
	// Arrange
	var logged []string
	resyncer := &servingResyncer{}
	h := newResyncHandler(t, resyncer, &logged)

	// Act
	err := h.Resync(context.Background(), "/w", "r-fenceless", &frontendv1.ResyncCmd{})

	// Assert
	if err != nil {
		t.Fatalf("fenceless resync: %v", err)
	}
	if resyncer.entered != 1 {
		t.Fatalf("the eligibility ladder ran %d time(s) for a fenceless resync, want 1", resyncer.entered)
	}
}
