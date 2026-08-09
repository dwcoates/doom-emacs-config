package sessioncontroller

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/session"
)

// keepaliveresidue_test.go — WHAT THE MODEL SEES.
//
// Every assertion here is about the transcript the vendor is RESUMED against,
// never about a rendering. The display exclusion was already solid; the defect
// was that the keep-alive turns stayed in the model's context, and the only
// evidence about that is the file the CLI reads.

// modelFacingTranscript reads the transcript the workspace's session would now
// be resumed from — the rewound copy when a rewind landed, the original
// otherwise — and returns its raw bytes.
func modelFacingTranscript(t *testing.T, configDir, workspace, vendorSessionID string) string {
	t.Helper()
	path := filepath.Join(session.ProjectDir(configDir, workspace), vendorSessionID+".jsonl")
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the model-facing transcript %s: %v", path, err)
	}
	return string(raw)
}

// residueRig is the rewind rig with the config dir handed back, so a test can
// read the transcript the vendor would actually be resumed against.
func residueRig(t *testing.T) (*Manager, *fakeVendorSessions, string) {
	t.Helper()
	m, _, _ := keepAliveRig(t)
	configDir := t.TempDir()
	writeRewindableTranscript(t, configDir, "ws", "old-session")
	vendors := &fakeVendorSessions{}
	m.cfg.VendorSessions = vendors
	m.cfg.SessionConfigDir = func(string) string { return configDir }
	m.cfg.VendorSessionOf = func(string) (string, bool) { return "old-session", true }
	return m, vendors, configDir
}

// endOnePingWithNothingWaiting runs one whole ping — submit, then its own turn
// end — with no prompt held behind it. This is the shape that used to leave the
// ping in the model's context forever.
func endOnePingWithNothingWaiting(t *testing.T, m *Manager) string {
	t.Helper()
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()
	m.onTurnBoundary(d, false, 1_700_000_000_123)
	return turnID
}

func TestAPingThatEndedWithNothingWaitingRecordsTheRewindDebt(t *testing.T) {
	// Arrange: the ping's turns are now in the transcript and no aftermath ran,
	// so something has to remember that they are owed a rewind.
	m, _, _ := residueRig(t)

	// Act.
	turnID := endOnePingWithNothingWaiting(t, m)

	// Assert.
	owed := m.KeepAliveResidue("ws")
	if len(owed) != 1 || owed[0] != turnID {
		t.Fatalf("residue = %v, want exactly the ping turn %s that was left in the transcript", owed, turnID)
	}
}

func TestSettlingTheDebtRemovesThePingFromTheModelFacingTranscript(t *testing.T) {
	// Arrange: a completed ping standing at the transcript tail — the exact
	// state in which the next submission used to be answered with the ping's
	// own text in context.
	m, vendors, configDir := residueRig(t)
	endOnePingWithNothingWaiting(t, m)

	// Act.
	if !m.settleKeepAliveResidue(context.Background(), "ws", "test") {
		t.Fatal("settleKeepAliveResidue reported the debt unsettled")
	}

	// Assert: the conversation the vendor would now be resumed against.
	if len(vendors.adopted) != 1 {
		t.Fatalf("registry flips = %d, want exactly one rewound conversation adopted", len(vendors.adopted))
	}
	got := modelFacingTranscript(t, configDir, "ws", vendors.adopted[0])
	if strings.Contains(got, keepalive.PingText) {
		t.Fatalf("the model-facing transcript still carries the keep-alive prompt:\n%s", got)
	}
}

func TestSettlingTheDebtKeepsTheLastRealTurnInTheModelFacingTranscript(t *testing.T) {
	// Arrange: the cut must land at a turn boundary, so the real work in front
	// of the ping survives it intact.
	m, vendors, configDir := residueRig(t)
	endOnePingWithNothingWaiting(t, m)

	// Act.
	m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	got := modelFacingTranscript(t, configDir, "ws", vendors.adopted[0])
	if !strings.Contains(got, "real work") {
		t.Fatalf("the rewind cut away the last real turn:\n%s", got)
	}
}

func TestASettledDebtIsDischarged(t *testing.T) {
	// Arrange: a debt kept after a landed rewind would make every later
	// submission re-run a rewind against a clean transcript forever.
	m, _, _ := residueRig(t)
	endOnePingWithNothingWaiting(t, m)

	// Act.
	m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	if owed := m.KeepAliveResidue("ws"); len(owed) != 0 {
		t.Fatalf("residue = %v after a landed rewind, want it discharged", owed)
	}
}

func TestAUserPromptSettlesTheDebtBeforeItIsSubmitted(t *testing.T) {
	// Arrange: THE GAP. A user prompt arriving with no ping in flight but
	// completed ping turns at the transcript tail. Nothing used to rewind here.
	m, vendors, configDir := residueRig(t)
	endOnePingWithNothingWaiting(t, m)

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req-1", "what did I ask you?", "",
		corev1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert: the context the prompt was submitted against.
	if len(vendors.adopted) != 1 {
		t.Fatalf("registry flips = %d, want the prompt to have rewound the ping out first", len(vendors.adopted))
	}
	got := modelFacingTranscript(t, configDir, "ws", vendors.adopted[0])
	if strings.Contains(got, keepalive.PingText) {
		t.Fatalf("the user's prompt was submitted on top of the keep-alive turns:\n%s", got)
	}
}

func TestACompactFirstRevivalSettlesTheDebtBeforeCompacting(t *testing.T) {
	// Arrange: THE SHARPEST GAP. A compaction reads the whole conversation and
	// writes back a SUMMARY of it, so a ping folded in here is permanent — no
	// later rewind can reach it. A hibernated session's transcript tail is very
	// often exactly this shape.
	m, vendors, configDir := residueRig(t)
	endOnePingWithNothingWaiting(t, m)

	// Act.
	m.settleKeepAliveResidue(context.Background(), "ws", "revive:compact-first")

	// Assert.
	got := modelFacingTranscript(t, configDir, "ws", vendors.adopted[0])
	if strings.Contains(got, keepalive.PingText) {
		t.Fatalf("the compaction would have summarized the keep-alive turns into the conversation:\n%s", got)
	}
}

func TestTheHeldPromptAftermathDischargesTheDebtToo(t *testing.T) {
	// Arrange: the pre-existing rewind path and the new one settle ONE ledger.
	// A debt left standing by the aftermath would make the very next submission
	// bounce the shim again for nothing.
	m, _, _ := residueRig(t)
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	m.mu.Lock()
	d := m.byWS["ws"]
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()

	// Act.
	m.releaseKeepAliveHolds(d, turnID, []string{"q_1"})

	// Assert.
	if owed := m.KeepAliveResidue("ws"); len(owed) != 0 {
		t.Fatalf("residue = %v after the held-prompt aftermath rewound, want it discharged", owed)
	}
}

func TestASettleWithNoVendorTranscriptRefusesRatherThanInventingOne(t *testing.T) {
	// Arrange: the refusal semantics are UNCHANGED. A session that names no
	// vendor conversation cannot be rewound, and the daemon must not synthesize
	// one — it proceeds without rewinding and keeps the debt for a later path.
	m, vendors, _ := residueRig(t)
	endOnePingWithNothingWaiting(t, m)
	m.cfg.VendorSessionOf = func(string) (string, bool) { return "", false }

	// Act.
	settled := m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	if settled {
		t.Fatal("settleKeepAliveResidue reported success with no vendor transcript to rewind")
	}
	if len(vendors.adopted) != 0 {
		t.Fatalf("a refused settle flipped the registry to %v; it invented a conversation", vendors.adopted)
	}
}

func TestARefusedSettleKeepsTheDebtForALaterPath(t *testing.T) {
	// Arrange: dropping the debt on a refusal would silently give up on ever
	// removing the ping, which is the failure this whole ledger exists to end.
	m, _, _ := residueRig(t)
	turnID := endOnePingWithNothingWaiting(t, m)
	m.cfg.VendorSessionOf = func(string) (string, bool) { return "", false }

	// Act.
	m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	owed := m.KeepAliveResidue("ws")
	if len(owed) != 1 || owed[0] != turnID {
		t.Fatalf("residue = %v after a refused settle, want the debt kept", owed)
	}
}

func TestASettleDefersToAPingThatIsStillInFlight(t *testing.T) {
	// Arrange: a ping in flight owns its own aftermath and holds the queue
	// behind it. A second rewind racing it would stop the shim the first one is
	// bringing up.
	m, vendors, _ := residueRig(t)
	endOnePingWithNothingWaiting(t, m)
	if _, err := m.SubmitKeepAlivePing(context.Background(), "ws"); err != nil {
		t.Fatalf("second SubmitKeepAlivePing: %v", err)
	}

	// Act.
	settled := m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	if settled {
		t.Fatal("settleKeepAliveResidue ran while a ping was in flight")
	}
	if len(vendors.adopted) != 0 {
		t.Fatalf("a deferred settle flipped the registry to %v", vendors.adopted)
	}
}

func TestASettleWithNoDebtDoesNothingAtAll(t *testing.T) {
	// Arrange: the overwhelmingly common case is a workspace that has never
	// been pinged, and it must not pay a shim bounce on every prompt.
	m, vendors, _ := residueRig(t)

	// Act.
	settled := m.settleKeepAliveResidue(context.Background(), "ws", "test")

	// Assert.
	if !settled {
		t.Fatal("settleKeepAliveResidue reported a debt on a workspace that was never pinged")
	}
	if len(vendors.adopted) != 0 {
		t.Fatalf("a workspace with no debt was bounced anyway: %v", vendors.adopted)
	}
}
