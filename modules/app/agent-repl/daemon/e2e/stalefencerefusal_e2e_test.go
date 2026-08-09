// A STALE FENCE is REFUSED, and refused before anything is replayed.
//
// ResyncCmd.fence is the fence ECHO — "the exact fence the client held when it
// decided to ask for this replay". The proto states what the daemon owes in
// return: "The daemon compares it against the workspace's live fence and
// REFUSES the command before replaying anything when they differ."
//
// Both halves matter, and each covers a different way of getting this wrong.
// Accepting a stale echo rebinds a delayed request to a newer generation and
// serves a replay nobody asked for. Nacking but replaying anyway is worse: the
// client has been told the answer is invalid and has already been handed it, so
// the conversation it renders is a splice of two generations.
package e2e

import (
	"fmt"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAResyncCarryingAStaleFenceIsRefusedWithoutReplay covers the
// STALE-PUSH REJECTION edge.
func TestE2EAResyncCarryingAStaleFenceIsRefusedWithoutReplay(t *testing.T) {
	// Arrange — a real session with real conversation, so a replay would have
	// something to serve and its absence is a decision rather than an empty
	// history.
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	const history = "e2e-stale-fence: conversation a wrongly-served replay would carry"
	store.write(sidecarUserLineEvent(t, vendorID, "e2e-stale-fence-history", history))
	awaitItem(t, conn, cwd, "the history item", func(it *frontendv1.ConversationItem) bool {
		return it.GetUserMessage().GetContentString() == history
	})

	// A fence the workspace does not hold. Byte-different from any live token
	// and structurally unlike one, because the daemon is the ONLY thing allowed
	// to read meaning into a fence — a test that built a plausible-looking one
	// would be encoding the composition the contract keeps free to change.
	const staleFence = "e2e-stale-fence-echo-no-workspace-ever-held-this"
	const requestID = "e2e-stale-resync"

	// Act
	writeCmd(t, conn, fmt.Sprintf(`{"requestId":%q,"resync":{"fromSeq":"0","fence":%q}}`, requestID, staleFence))

	// Assert
	var (
		ack      *frontendv1.CommandAck
		replayed []*frontendv1.ConversationItem
	)
	deadline := time.Now().Add(frameTimeout)
	for ack == nil && time.Now().Before(deadline) {
		frame := readFrame(t, conn)
		if got, ok := frame.GetFrame().(*frontendv1.FrontendFrame_CommandAck); ok && got.CommandAck.GetRequestId() == requestID {
			ack = got.CommandAck
			break
		}
		replayed = append(replayed, deltaItems(frame, cwd)...)
	}
	if ack == nil {
		t.Fatalf("no CommandAck for resync %s arrived before the deadline: a refusal the client never hears is indistinguishable from a hang", requestID)
	}
	if ack.GetOk() {
		t.Fatalf("the daemon ACCEPTED a resync carrying the stale fence %q: a delayed request has silently rebound itself to the current generation and asked for a replay nobody wanted", staleFence)
	}
	// THE ARM IS NAMED, not merely counted. Per the coordinator's ruling, a
	// stale-fence refusal classifies as reconnect_superseded — the specific
	// "this view is behind: the replay it asked for would have come from a
	// generation it never saw" arm (errors.proto 94-95) — and NOT as
	// workspace_not_live, which says the workspace has no live session at all.
	// The two resolve differently for the reader: one asks the client to catch
	// up, the other says there is nothing to catch up to. Asserting only that
	// SOME arm is set would let them be swapped silently.
	if ack.GetFailure() == nil {
		t.Fatalf("the stale-fence refusal arrived with no classified failure: CommandAck.failure is the reader-facing account, and a bare error string reaches the webapp as nothing at all")
	}
	if ack.GetFailure().GetReconnectSuperseded() == nil {
		t.Errorf("the stale-fence refusal classified as %T, want the reconnect_superseded arm: the client is told its workspace is unusable instead of being told to catch up",
			ack.GetFailure().GetKind())
	}
	if len(replayed) > 0 {
		t.Errorf("the daemon replayed %d conversation items alongside its refusal of the stale fence %q: the proto states it must refuse BEFORE replaying anything, or the client renders a splice of two generations",
			len(replayed), staleFence)
	}
}
