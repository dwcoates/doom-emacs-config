//go:build integration

package integration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/encoding/protojson"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// heldingress_test.go exercises the held-prompt ingress
// ($AGENT_REPL_STATE_DIR/held-prompts/held_*.json, ARCHITECTURE.md
// "heldingress"): the durable place a client leaves a prompt it could not hand
// to a live daemon. Every entry is submitted through SubmitPrompt's own body
// under its own idempotency key, so these tests assert the rpc's own visible
// effects -- the held tray, StartTurn, the duplicate answer.

// heldWrite drops one held-prompt entry under its contracted name, through a
// dot-prefixed temporary name and a rename, exactly as a producer writes it.
func heldWrite(t *testing.T, stateDir, name, projectDir, key, prompt string) string {
	t.Helper()
	saidJSON, err := protojson.Marshal(said(prompt))
	if err != nil {
		t.Fatalf("encode said: %v", err)
	}
	body, err := json.Marshal(map[string]any{
		"version":         1,
		"project_dir":     projectDir,
		"idempotency_key": key,
		"origin":          conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT.String(),
		"said":            json.RawMessage(saidJSON),
		"queued_at":       "2026-09-28T12:00:00Z",
	})
	if err != nil {
		t.Fatalf("encode the entry: %v", err)
	}
	dir := filepath.Join(stateDir, "held-prompts")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir the held-prompt ingress: %v", err)
	}
	path := filepath.Join(dir, name)
	tmp := filepath.Join(dir, "."+name+".tmp")
	if err := os.WriteFile(tmp, body, 0o644); err != nil {
		t.Fatalf("write %s: %v", tmp, err)
	}
	if err := os.Rename(tmp, path); err != nil {
		t.Fatalf("rename %s: %v", path, err)
	}
	return path
}

// heldTexts lists the tray's held prompts' words, in display order.
func heldTexts(tray *frontendv1.DaemonHoldTray) []string {
	var out []string
	for _, item := range tray.GetItems() {
		if p := item.GetPrompt(); p != nil {
			out = append(out, text(p.GetSaid()))
		}
	}
	return out
}

// awaitHeldRecord waits for the ingress's record of one entry on the
// workspace's own daemon sink.
func awaitHeldRecord(t *testing.T, d *harness.Daemon, dir, operation, key string) {
	t.Helper()
	d.AwaitLogRecord(harness.WorkspaceLogPath(dir, "daemon"), operation+" for "+key, func(r harness.LogRecord) bool {
		return r.Operation == operation && r.Level == "info" && r.Context["idempotency_key"] == key
	})
}

// assertIngressEmpty fails when any entry is still in the ingress.
func assertIngressEmpty(t *testing.T, stateDir string) {
	t.Helper()
	left, err := filepath.Glob(filepath.Join(stateDir, "held-prompts", "held_*.json"))
	if err != nil {
		t.Fatal(err)
	}
	if len(left) != 0 {
		t.Fatalf("entries left in the ingress = %v, want every ingested entry removed", left)
	}
}

func TestIngestedHeldPromptsWaitBehindTheRunningTurnInOrderInTheHeldTray(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	holds := f.d.WatchHolds(f.ws)

	// Act
	heldWrite(t, f.d.StateDir, "held_20260928T120001.000000001_aaaaaaaa_k-h1.json", f.repo.Dir, "k-h1", "the first held prompt")
	heldWrite(t, f.d.StateDir, "held_20260928T120001.000000002_aaaaaaaa_k-h2.json", f.repo.Dir, "k-h2", "the second held prompt")

	// Assert: the tray shows both, in the order they were written.
	got := awaitView(t, f, holds, "both ingested prompts in the held tray", func(tray *frontendv1.DaemonHoldTray) bool {
		return len(heldTexts(tray)) == 2
	})
	if texts := heldTexts(got); texts[0] != "the first held prompt" || texts[1] != "the second held prompt" {
		t.Fatalf("held tray = %v, want the two prompts in written order", texts)
	}
	awaitHeldRecord(t, f.d, f.repo.Dir, "daemon.heldingress.ingest", "k-h2")
	assertIngressEmpty(t, f.d.StateDir)

	// Act: the running turn ends.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the first-written prompt is the first delivered.
	if st := f.shim.ExpectStartTurn(); text(st.GetSaid()) != "the first held prompt" {
		t.Fatalf("first delivered = %q, want the first-written held prompt", text(st.GetSaid()))
	}
}

func TestADaemonStartIngestsHeldPromptsWrittenWhileNoDaemonServed(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon is gone, and the client writes two prompts.
	f := newOpened(t, harness.Opts{})
	f.d.Stop()
	heldWrite(t, f.d.StateDir, "held_20260928T120001.000000001_aaaaaaaa_k-d1.json", f.repo.Dir, "k-d1", "written while down, first")
	heldWrite(t, f.d.StateDir, "held_20260928T120001.000000002_aaaaaaaa_k-d2.json", f.repo.Dir, "k-d2", "written while down, second")

	// Act
	nd := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	f.d = nd

	// Assert: the first is delivered to the adopted session, the second is
	// held behind it, and the ingress is empty.
	holds := nd.WatchHolds(f.ws)
	if st := nd.Shim(f.ws).ExpectStartTurn(); text(st.GetSaid()) != "written while down, first" {
		t.Fatalf("first delivered = %q, want the first-written prompt", text(st.GetSaid()))
	}
	got := awaitView(t, f, holds, "the second prompt held behind the first", func(tray *frontendv1.DaemonHoldTray) bool {
		return len(heldTexts(tray)) == 1
	})
	if texts := heldTexts(got); texts[0] != "written while down, second" {
		t.Fatalf("held tray = %v, want the second-written prompt", texts)
	}
	awaitHeldRecord(t, nd, f.repo.Dir, "daemon.heldingress.ingest", "k-d2")
	assertIngressEmpty(t, nd.StateDir)
}

func TestAHeldPromptWhoseKeyTheDaemonAlreadyAcceptedIsNotDeliveredTwice(t *testing.T) {
	t.Parallel()
	// Arrange: the submission the client gave up on DID land.
	f := newOpened(t, harness.Opts{})
	f.submit("do the thing", "k-landed", origin)
	f.shim.ExpectStartTurn()

	// Act
	heldWrite(t, f.d.StateDir, "held_20260928T120001.000000001_aaaaaaaa_k-landed.json", f.repo.Dir, "k-landed", "do the thing")

	// Assert: answered as the duplicate it is, at INFO, and never started.
	awaitHeldRecord(t, f.d, f.repo.Dir, "daemon.heldingress.dedupe", "k-landed")
	assertIngressEmpty(t, f.d.StateDir)
	if got := f.shim.Count(harness.RPCStartTurn); got != 1 {
		t.Fatalf("StartTurn count = %d, want exactly 1 (the held copy is never delivered)", got)
	}
}

func TestAMalformedHeldPromptEntryIsQuarantinedWithAWarning(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.heldingress.quarantine")
	dir := filepath.Join(f.d.StateDir, "held-prompts")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	if err := os.WriteFile(filepath.Join(dir, "held_20260928T120001.000000001_aaaaaaaa_k-bad.json"), []byte(`{"version":7}`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Assert
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the quarantine warning", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.heldingress.quarantine" && r.Level == "warn"
	})
	if _, err := os.Stat(filepath.Join(dir, "quarantine", "held_20260928T120001.000000001_aaaaaaaa_k-bad.json")); err != nil {
		t.Fatalf("the quarantined entry: %v, want it kept where a person can read it", err)
	}
	if got := f.shim.Count(harness.RPCStartTurn); got != 0 {
		t.Fatalf("StartTurn count = %d, want 0", got)
	}
}

// TestDuringAHandoverOnlyTheServingDaemonIngestsAHeldPrompt pins the ingress's
// place in a handover. The ingress submits through the prompt handler directly,
// so the server's transferring_away / not_yet_adopted refusals never reach it:
// ONLY THE DAEMON THAT SERVES may take it. An entry written after the transfer
// and before the adoption is taken by neither daemon -- the incumbent's handover
// has begun, the successor is still joining -- and once the successor serves it
// takes the entry, exactly once, and delivers it as the held prompt it is.
func TestDuringAHandoverOnlyTheServingDaemonIngestsAHeldPrompt(t *testing.T) {
	t.Parallel()
	// Arrange: a running turn, so the move carries it and the ingested prompt
	// is held behind it on the successor.
	selfRepo, d := drainSelfRepoDaemon(t)
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	f.shim.ExpectWatchSession()
	host := d.WatchHost(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	if first := f.submit("first", "k-handover-running", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT); first.GetSuccess() == nil {
		t.Fatalf("SubmitPrompt(first) = %v, want the turn accepted", first)
	}
	f.shim.ExpectStartTurn()
	daemonStream := d.WatchDaemonStream()
	drainTriggerDeploy(t, d, selfRepo, harness.DeployStaleDaemon)
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	d.AwaitLogRecord(d.RunLogPath(), "the incumbent leaving the intake", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.heldingress.gate" && r.Level == "info" &&
			strings.Contains(r.Message, "does not serve")
	})

	// Act: the client writes a prompt while neither daemon serves the workspace.
	path := heldWrite(t, d.StateDir, "held_20260928T120001.000000001_aaaaaaaa_k-handover.json", f.repo.Dir, "k-handover", "written mid-handover")
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read the entry: %v", err)
	}

	// Assert: neither daemon takes it.
	d.ExpectFileUnchanged(path, string(body), harness.ProbeWindow)

	// Act: the participants adopt, and the successor serves.
	successor := drainDial(announced.GetAddress())
	if resp, err := successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptHostWorkspace = (%v, %v), want a success", resp, err)
	}
	if resp, err := successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws})); err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AdoptWebWorkspace = (%v, %v), want a success", resp, err)
	}

	// Assert: the successor, and only the successor, ingested it.
	ingested := d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the successor's ingest", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.heldingress.ingest" && r.Level == "info" && r.Context["idempotency_key"] == "k-handover"
	})
	if ingested.PID == d.PID() {
		t.Fatalf("the incumbent (pid %d) ingested the entry after its handover began; only the serving daemon may", d.PID())
	}
	assertIngressEmpty(t, d.StateDir)

	// Act: the carried turn ends on the successor's watch.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: delivered once, as its own turn.
	if st := f.shim.ExpectStartTurn(); text(st.GetSaid()) != "written mid-handover" {
		t.Fatalf("delivered = %q, want the prompt written mid-handover", text(st.GetSaid()))
	}
	expectRPCCount(t, f.shim, harness.RPCStartTurn, 2, harness.ProbeWindow)
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the incumbent's exit code = %d, want an orderly 0", code)
	}
}
