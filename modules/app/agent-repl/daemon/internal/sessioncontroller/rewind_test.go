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

	"claude-repld/internal/keepalive"
	"claude-repld/internal/session"
)

// fakeVendorSessions records the rewind's atomic registry flip.
type fakeVendorSessions struct {
	adopted []string
	refuse  bool
}

func (f *fakeVendorSessions) AdoptVendorSessionID(sessionID, claudeSessionID string) (bool, string, bool) {
	if f.refuse {
		return false, "old-session", false
	}
	f.adopted = append(f.adopted, claudeSessionID)
	return true, "old-session", true
}

// fakeRewindArmer records the one-shot lineage armed for the next spawn.
type fakeRewindArmer struct{ armed []RewindLineage }

func (f *fakeRewindArmer) ArmRewindLineage(sessionID string, lineage RewindLineage) error {
	f.armed = append(f.armed, lineage)
	return nil
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
	m.cfg.RewindLineages = &fakeRewindArmer{}
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
