package sessiondrv

import (
	"context"
	"path/filepath"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
)

// VENDOR_BLOCKED IS A REPORT, NOT A GATE.
//
// The color arms are pinned in isolation elsewhere (ssm/fivecolor_test.go and
// faultinject_test.go both drive an errored TurnEnded to purple and a clean one
// back off it). What neither can show is the link the purple is most likely to
// be misread as: that a blocked workspace still TAKES the user's next prompt.
// Nothing in the daemon gates submission on the render state, and this composes
// the whole sequence through a live Manager so a latch introduced anywhere
// between the state axis and the shim would fail here.

const (
	vendorBlockedWorkspace = "/ws/vendor-blocked"
	vendorBlockedSessionID = "s_vendor_blocked"
)

// vendorBlockedResolver binds the session id to its workspace for the SSM.
type vendorBlockedResolver map[string]string

func (r vendorBlockedResolver) Workspace(sessionID string) (string, bool) {
	ws, ok := r[sessionID]
	return ws, ok
}

// vendorBlockedRig is a live Manager over a REAL ssm.Manager, so the render
// state under assertion is the resolved one a footer frame carries rather than
// a fake's echo.
type vendorBlockedRig struct {
	t      *testing.T
	m      *Manager
	mgr    *ssm.Manager
	client *fakeClient
	seq    uint64
}

func newVendorBlockedRig(t *testing.T) *vendorBlockedRig {
	t.Helper()
	mgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "ssm.db"),
		Resolver: vendorBlockedResolver{vendorBlockedSessionID: vendorBlockedWorkspace},
		Logf:     t.Logf,
	})
	if err != nil {
		t.Fatalf("open ssm: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })

	var mu sync.Mutex
	var client *fakeClient
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               mgr,
		Progress:          &fakeProgress{},
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{vendorBlockedWorkspace: vendorBlockedSessionID}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		ProtocolVersion:   "1",
		Source:            stubSource{},
		newClient: func(cfg shimclient.Config) sessionClient {
			fc := &fakeClient{cfg: cfg}
			mu.Lock()
			client = fc
			mu.Unlock()
			return fc
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure(vendorBlockedWorkspace); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	mu.Lock()
	rig := &vendorBlockedRig{t: t, m: m, mgr: mgr, client: client}
	mu.Unlock()
	return rig
}

// apply feeds one lifecycle event in at the seam a shim's demux uses, stamping
// the next store seq.
func (r *vendorBlockedRig) apply(payload any) {
	r.t.Helper()
	r.seq++
	ev := &corev1.Event{SessionId: vendorBlockedSessionID, Seq: r.seq}
	switch p := payload.(type) {
	case *corev1.SessionStarted:
		ev.Payload = &corev1.Event_SessionStarted{SessionStarted: p}
	case *corev1.TurnStarted:
		ev.Payload = &corev1.Event_TurnStarted{TurnStarted: p}
	case *corev1.TurnEnded:
		ev.Payload = &corev1.Event_TurnEnded{TurnEnded: p}
	default:
		r.t.Fatalf("vendorBlockedRig.apply: unsupported payload %T", payload)
	}
	d, err := r.m.existing(vendorBlockedWorkspace)
	if err != nil {
		r.t.Fatalf("existing: %v", err)
	}
	d.consumer.Apply(ev)
}

// state is the workspace's resolved render state — the value a footer frame
// reports.
func (r *vendorBlockedRig) state() frontendv1.RenderState {
	r.t.Helper()
	ws, found, err := r.mgr.Current(vendorBlockedWorkspace)
	if err != nil {
		r.t.Fatalf("resolve current state: %v", err)
	}
	if !found {
		return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED
	}
	return ws.GetState()
}

// settleReady brings the workspace up to a resolved, non-blue baseline.
func (r *vendorBlockedRig) settleReady() {
	r.t.Helper()
	r.apply(&corev1.SessionStarted{Model: "test-model", Cwd: vendorBlockedWorkspace})
	if err := r.mgr.ApplyBackfillState(vendorBlockedWorkspace, BackfillDone); err != nil {
		r.t.Fatalf("apply backfill done: %v", err)
	}
	if err := r.mgr.ApplyPaintAck(vendorBlockedWorkspace, 0); err != nil {
		r.t.Fatalf("apply paint ack: %v", err)
	}
}

func TestAVendorBlockedWorkspaceStillAcceptsTheNextPrompt(t *testing.T) {
	// Arrange — a turn the vendor refused.
	rig := newVendorBlockedRig(t)
	rig.settleReady()
	rig.apply(&corev1.TurnStarted{})

	// Act — the block is reported, the user retries anyway, and the retry
	// succeeds.
	rig.apply(&corev1.TurnEnded{StopReason: "authentication_failed", IsError: true})
	blocked := rig.state()
	submitErr := rig.m.SubmitPrompt(context.Background(), vendorBlockedWorkspace, "", "try again", "")
	delivered := rig.client.promptTexts()
	rig.apply(&corev1.TurnStarted{})
	rig.apply(&corev1.TurnEnded{StopReason: "end_turn"})

	// Assert — purple was reported, it never gated the prompt, and the clean
	// turn superseded it wholesale.
	if blocked != frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED {
		t.Fatalf("state after the refused turn = %s, want VENDOR_BLOCKED", blocked)
	}
	if submitErr != nil {
		t.Fatalf("SubmitPrompt while vendor blocked = %v, want nil — the purple is a report, not a latch", submitErr)
	}
	if len(delivered) != 1 || delivered[0] != "try again" {
		t.Fatalf("prompts reaching the shim = %v, want one %q", delivered, "try again")
	}
	if got := rig.state(); got != frontendv1.RenderState_RENDER_STATE_DONE {
		t.Fatalf("state after the clean turn = %s, want DONE", got)
	}
}
