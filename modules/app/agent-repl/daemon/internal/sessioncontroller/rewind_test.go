package sessioncontroller

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/session"
)

// fakeVendorSessions records the rewind's one atomic write: the uuid flip and
// the lineage that accounts for it.
type fakeVendorSessions struct {
	adopted  []string
	lineages []RewindLineage
	refuse   bool
}

func (f *fakeVendorSessions) AdoptRewoundVendorSessionID(sessionID, claudeSessionID string, lineage RewindLineage) (bool, string, bool) {
	if f.refuse {
		return false, "old-session", false
	}
	f.adopted = append(f.adopted, claudeSessionID)
	f.lineages = append(f.lineages, lineage)
	return true, "old-session", true
}

// transcriptLine renders one JSONL transcript record.
func transcriptLine(t *testing.T, fields map[string]any) string {
	t.Helper()
	b, err := json.Marshal(fields)
	if err != nil {
		t.Fatalf("marshal transcript record: %v", err)
	}
	return string(b)
}

// writeRewindableTranscript lays a real turn followed by a keep-alive ping turn
// under configDir, which is the shape the cut is decided on.
func writeRewindableTranscript(t *testing.T, configDir, workspace, vendorSessionID string) {
	t.Helper()
	dir := session.ProjectDir(configDir, workspace)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir project dir: %v", err)
	}
	lines := []string{
		transcriptLine(t, map[string]any{
			"type": "user", "uuid": "u1", "parentUuid": nil, "sessionId": vendorSessionID,
			"message": map[string]any{"role": "user", "content": "real work"},
		}),
		transcriptLine(t, map[string]any{
			"type": "assistant", "uuid": "a1", "parentUuid": "u1", "sessionId": vendorSessionID,
			"message": map[string]any{"role": "assistant", "content": []any{
				map[string]any{"type": "text", "text": "done"},
			}},
		}),
		transcriptLine(t, map[string]any{
			"type": "user", "uuid": "u2", "parentUuid": "a1", "sessionId": vendorSessionID,
			"message": map[string]any{"role": "user", "content": keepalive.PingText},
		}),
		transcriptLine(t, map[string]any{
			"type": "assistant", "uuid": "a2", "parentUuid": "u2", "sessionId": vendorSessionID,
			"message": map[string]any{"role": "assistant", "content": []any{
				map[string]any{"type": "text", "text": "."},
			}},
		}),
	}
	path := filepath.Join(dir, vendorSessionID+".jsonl")
	if err := os.WriteFile(path, []byte(strings.Join(lines, "\n")+"\n"), 0o644); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
}

// rewindRig is a keep-alive rig whose session names a rewindable transcript.
func rewindRig(t *testing.T) (*Manager, *fakeApplier, *fakeVendorSessions) {
	t.Helper()
	m, applier, _ := keepAliveRig(t)
	configDir := t.TempDir()
	writeRewindableTranscript(t, configDir, "ws", "old-session")
	vendors := &fakeVendorSessions{}
	m.cfg.VendorSessions = vendors
	m.cfg.SessionConfigDir = func(string) string { return configDir }
	m.cfg.VendorSessionOf = func(string) (string, bool) { return "old-session", true }
	return m, applier, vendors
}

// THE REWIND'S STOP IS GUARDED. The keep-alive hold is supposed to make a live
// turn impossible at this point, so one found running is evidence the hold has
// a hole — and stopping anyway would SIGTERM an acked user turn and truncate it
// out of the transcript with nothing told to the user.
func TestRewindRefusesToStopAWorkspaceThatIsNotSettled(t *testing.T) {
	// Arrange.
	m, applier, _ := rewindRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_READY,
		TurnActive: true,
	})

	// Act.
	_, err := m.rewindKeepAliveTurns(context.Background(), "ws", "s1", []string{"ka_1"})

	// Assert.
	if !errors.Is(err, ErrNotSettled) {
		t.Fatalf("rewindKeepAliveTurns against a live turn = %v, want ErrNotSettled", err)
	}
}

// THE REFUSAL LEAVES THE SESSION ALONE. The whole point of refusing is that the
// turn that slipped through keeps running; a stop taken anyway is the failure.
func TestRewindRefusalLeavesTheSessionControllerLive(t *testing.T) {
	// Arrange.
	m, applier, _ := rewindRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_READY,
		TurnActive: true,
	})

	// Act.
	_, _ = m.rewindKeepAliveTurns(context.Background(), "ws", "s1", []string{"ka_1"})

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	if _, live := m.byWS["ws"]; !live {
		t.Fatal("the refused rewind stopped the session controller anyway; the live turn it refused over was killed")
	}
}

// THE REFUSAL WRITES NOTHING. It travels the established DEGRADED channel — the
// held prompt is submitted WITHOUT the rewind — so the registry must still name
// the original conversation.
func TestRewindRefusalTakesNoRegistryFlip(t *testing.T) {
	// Arrange.
	m, applier, vendors := rewindRig(t)
	applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_READY,
		TurnActive: true,
	})

	// Act.
	_, _ = m.rewindKeepAliveTurns(context.Background(), "ws", "s1", []string{"ka_1"})

	// Assert.
	if len(vendors.adopted) != 0 {
		t.Fatalf("a refused rewind flipped the registry to %v", vendors.adopted)
	}
}

// THE LINEAGE RIDES THE FLIP. It is the only account of what the flip dropped,
// so the two are one write: a crash between them used to leave a record naming
// a truncated conversation with nothing left to say it had been truncated.
func TestRewindWritesTheLineageWithTheFlip(t *testing.T) {
	// Arrange.
	m, _, vendors := rewindRig(t)

	// Act.
	if _, err := m.rewindKeepAliveTurns(context.Background(), "ws", "s1", []string{"ka_1", "ka_2"}); err != nil {
		t.Fatalf("rewindKeepAliveTurns: %v", err)
	}

	// Assert.
	if len(vendors.lineages) != 1 {
		t.Fatalf("%d lineage writes, want exactly the one that carried the flip", len(vendors.lineages))
	}
	got := vendors.lineages[0]
	if got.PreviousVendorSessionID != "old-session" || got.RetainedLeafUUID == "" || got.DroppedTurnIDs != "ka_1,ka_2" {
		t.Fatalf("lineage = %+v, want the truncated uuid, a retained leaf and the dropped turns in submission order", got)
	}
}

// A REFUSED FLIP ARMS NOTHING, which is what removes the stale-arm defect: the
// lineage cannot exist without the flip it accounts for.
func TestRewindRefusedFlipLeavesNoLineageBehind(t *testing.T) {
	// Arrange.
	m, _, vendors := rewindRig(t)
	vendors.refuse = true

	// Act.
	_, err := m.rewindKeepAliveTurns(context.Background(), "ws", "s1", []string{"ka_1"})

	// Assert.
	if err == nil {
		t.Fatal("rewindKeepAliveTurns = nil against a registry that refused the flip")
	}
	if len(vendors.lineages) != 0 {
		t.Fatalf("a refused flip stored lineage %+v", vendors.lineages)
	}
}

// ---------------------------------------------------------------------------
// The rewind's ownership of the queue
// ---------------------------------------------------------------------------

// THE ORCHESTRATOR OWNS THE QUEUE ACROSS THE BOUNCE. The entries are taken out
// from under the dying controller before its exit tail can reach them, so the
// tail's unconditional drain finds nothing to drop.
func TestTakeQueueForRewindEmptiesTheControllersQueue(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	d.queue.add(&queueEntry{id: "q1", text: "real work", keepAliveHoldTurnID: "ka_1"})
	m.mu.Unlock()

	// Act.
	owned := m.takeQueueForRewind(d)

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	if len(owned) != 1 || owned[0].id != "q1" {
		t.Fatalf("takeQueueForRewind returned %+v, want the one held entry", owned)
	}
	if len(d.queue.entries) != 0 {
		t.Fatalf("%d entr(ies) left in the retired queue; the exit tail can still drop them", len(d.queue.entries))
	}
}

// THE MIGRATION FLAG IS WHAT THE EXIT TAIL READS. Without it the tail would
// drain a queue it does not own and persist nil over the durable record of
// prompts the orchestrator is still holding.
func TestTakeQueueForRewindMarksTheControllerMigrating(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()

	// Act.
	m.takeQueueForRewind(d)

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	if !d.queueMigrating {
		t.Fatal("the controller was not marked migrating; its exit tail would drain and persist over the orchestrator's entries")
	}
}

// A MIGRATING CONTROLLER'S EXIT TAIL DROPS NOTHING. This is the arbitration:
// the tail structurally finds an empty queue because ownership moved before it
// ran, rather than losing a race with it.
func TestExitTailDropsNothingFromAMigratingController(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.takeQueueForRewindLocked(d)
	d.queue.add(&queueEntry{id: "q1", text: "arrived during the bounce"})
	m.mu.Unlock()

	// Act.
	m.mu.Lock()
	defer m.mu.Unlock()
	dropped := m.drainQueueForExitLocked(d)

	// Assert.
	if len(dropped) != 0 {
		t.Fatalf("the exit tail dropped %d entr(ies) of a migrating controller, want none", len(dropped))
	}
	if len(d.queue.entries) != 1 {
		t.Fatalf("%d entr(ies) survived the exit tail, want the one the orchestrator will re-park", len(d.queue.entries))
	}
}

// A CONTROLLER THAT IS NOT MIGRATING KEEPS THE OLD BEHAVIOR: a dead session's
// prompts can never be delivered, so the queue is emptied and the empty view
// pushed.
func TestExitTailStillDrainsAnOrdinaryController(t *testing.T) {
	// Arrange.
	m, _, _ := keepAliveRig(t)
	m.mu.Lock()
	d := m.byWS["ws"]
	d.queue.add(&queueEntry{id: "q1", text: "real work"})
	m.mu.Unlock()

	// Act.
	m.mu.Lock()
	dropped := m.drainQueueForExitLocked(d)
	m.mu.Unlock()

	// Assert.
	if len(dropped) != 1 {
		t.Fatalf("the exit tail dropped %d entr(ies) of an ordinary controller, want 1", len(dropped))
	}
}

// THE RE-PARK PRESERVES ORDER. Entries that arrived during the bounce were
// typed AFTER the ones the orchestrator carried across, so the carried ones go
// back in front of them.
func TestRepositionedRewindEntriesKeepTheirPlaceAheadOfLaterArrivals(t *testing.T) {
	// Arrange.
	q := &promptQueue{}
	q.add(&queueEntry{id: "later"})

	// Act.
	q.pushFrontAll([]*queueEntry{{id: "earlier-1"}, {id: "earlier-2"}})

	// Assert.
	got := []string{q.entries[0].id, q.entries[1].id, q.entries[2].id}
	want := []string{"earlier-1", "earlier-2", "later"}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("queue order = %v, want %v", got, want)
		}
	}
}

// THE HELD PROMPT SURVIVES THE WHOLE BOUNCE. This is the failure the ownership
// transfer exists for: the exit tail of the controller the rewind stops used to
// drain the queue and persist nil before the migration ran, so the prompt the
// user typed vanished together with the record of it.
func TestKeepAliveAftermathKeepsTheHeldPromptAcrossTheBounce(t *testing.T) {
	// Arrange.
	m, pingTurn, d, heldIDs := aftermathRig(t)

	// Act.
	m.releaseKeepAliveHolds(d, pingTurn, heldIDs)

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	live, ok := m.byWS["ws"]
	if !ok {
		t.Fatal("the workspace has no controller after the aftermath")
	}
	// The FIRST held prompt is handed straight to delivery, so the second is
	// what proves the queue itself came across rather than being drained.
	var second *queueEntry
	for _, e := range live.queue.entries {
		if e.text == "real work 2" {
			second = e
		}
	}
	if second == nil {
		t.Fatalf("the second held prompt was dropped by the bounce; queue=%+v", live.queue.entries)
	}
	if second.keepAliveHeld() {
		t.Fatal("the carried-across prompt is still held behind a ping that has ended")
	}
}

// THE REWIND'S CLAIM IS RELEASED BY ITS OWN TAIL. A claim that outlived the
// aftermath would hold every later prompt on the workspace forever.
func TestKeepAliveAftermathReleasesTheRewindClaim(t *testing.T) {
	// Arrange.
	m, pingTurn, d, heldIDs := aftermathRig(t)

	// Act.
	m.releaseKeepAliveHolds(d, pingTurn, heldIDs)

	// Assert.
	m.mu.Lock()
	defer m.mu.Unlock()
	if got, claimed := m.keepAliveRewinds["ws"]; claimed {
		t.Fatalf("rewind claim = %q after the aftermath finished, want it released", got)
	}
}

// aftermathRig is a rewind rig standing exactly where the ping's turn boundary
// leaves it: the ping's own claim cleared, the rewind's claim taken, and one
// real prompt held behind it.
func aftermathRig(t *testing.T) (m *Manager, pingTurnID string, d *sessionController, heldIDs []string) {
	t.Helper()
	m, _, _ = rewindRig(t)
	pingTurnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	for _, text := range []string{"real work", "real work 2"} {
		if err := m.SubmitPrompt(context.Background(), "ws", "req-"+text, text, "",
			corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
			t.Fatalf("SubmitPrompt %q: %v", text, err)
		}
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	d = m.byWS["ws"]
	d.noteKeepAliveTurnEndedLocked(pingTurnID)
	m.claimKeepAliveRewindLocked("ws", pingTurnID)
	return m, pingTurnID, d, d.queue.keepAliveHeldIDs(pingTurnID)
}
