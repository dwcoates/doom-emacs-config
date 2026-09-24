//go:build integration

package integration

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/stateroot"

	"connectrpc.com/connect"
)

// prelaunchControlSocket names the control socket a relaunch's Nth prelaunch
// binds, matching internal/workspace/fleet_rollout.go's freshSocketPath
// naming (the base per-workspace socket, ".sock" trimmed, ".n<gen>.sock"
// appended) plus the ".ctl" control suffix every fake shim serves. gen is 1
// for a workspace's first bounce, 2 for its second, and so on.
func prelaunchControlSocket(d *harness.Daemon, ws *workspacev1.WorkspaceRef, gen int) string {
	base := strings.TrimSuffix(d.SocketPath(ws), ".sock")
	return base + ".n" + strconv.Itoa(gen) + ".sock.ctl"
}

// restartGracefulInFlight opens a workspace, starts a long-running turn, and
// issues a graceful RestartWorkspace, answering the fixture once the bounce
// registry has REGISTERED the bounce behind the running turn. The turn is left
// running: the caller ends it (or not) to drive the bounce.
func restartGracefulInFlight(t *testing.T, key string) *fixture {
	t.Helper()
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.submit("long running work", key, conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()

	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws, Force: false}))
	if err != nil {
		t.Fatalf("RestartWorkspace{force:false} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want a success", resp.Msg)
	}
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the bounce registered behind the running turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.bounce" && r.Message == "the workspace has work in flight; registered the bounce for when it ends"
	})
	return f
}

// expectNoFile asserts a path does not appear within the probe window. It is
// a negative assertion, so it necessarily waits out a bound rather than
// synchronizing on an event, mirroring expectNoRPC.
func expectNoFile(t *testing.T, path string, probe time.Duration) {
	t.Helper()
	deadline := time.NewTimer(probe)
	defer deadline.Stop()
	ticker := time.NewTicker(5 * time.Millisecond)
	defer ticker.Stop()
	for {
		if _, err := os.Stat(path); err == nil {
			t.Fatalf("%s appeared inside the %s probe window, want it absent", path, probe)
		}
		select {
		case <-ticker.C:
		case <-deadline.C:
			return
		}
	}
}

// expectNoRPC asserts a shim received no request for `rpc` within the probe
// window. It is a negative assertion, so it necessarily waits out a bound
// rather than synchronizing on an event, mirroring harness.ExpectNoPush and
// Daemon.ExpectFileUnchanged.
func expectNoRPC(t *testing.T, s *harness.ShimControl, rpc string, probe time.Duration) {
	t.Helper()
	expectRPCCount(t, s, rpc, 0, probe)
}

// expectRPCCount asserts a shim's total receipt count for rpc stays at want
// throughout the probe window. Unlike expectNoRPC, it can observe that a
// successor did not add a second stream to one the incumbent already opened.
func expectRPCCount(t *testing.T, s *harness.ShimControl, rpc string, want int, probe time.Duration) {
	t.Helper()
	deadline := time.Now().Add(probe)
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for time.Now().Before(deadline) {
		if got := s.Count(rpc); got != want {
			t.Fatalf("%s count = %d, want %d throughout the probe window", rpc, got, want)
		}
		<-ticker.C
	}
}

// writeIntentManifest files a stand-down intent manifest directly under a
// daemon's state root, as the fixture data a genuine outgoing daemon would
// have left behind. It is harness-side fixture data, not production code: no
// code under internal/ or cmd/ is touched by writing it.
func writeIntentManifest(t *testing.T, d *harness.Daemon, sessions ...rollout.ManifestSession) {
	t.Helper()
	layout, err := stateroot.Root(d.StateDir, "")
	if err != nil {
		t.Fatalf("harness: resolve the state root layout for %q: %v", d.StateDir, err)
	}
	m := rollout.Manifest{
		Daemon:    ids.InstanceID("test-outgoing"),
		Successor: "",
		WrittenAt: time.Now(),
		Sessions:  sessions,
	}
	writeManifestFile(t, layout.IntentManifest(), m)
}

// writeManifestFile encodes and installs one manifest, creating its
// directory if the daemon under test has not already.
func writeManifestFile(t *testing.T, path string, m rollout.Manifest) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("harness: create the intent directory %s: %v", filepath.Dir(path), err)
	}
	body, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		t.Fatalf("harness: encode the intent manifest: %v", err)
	}
	if err := os.WriteFile(path, body, 0o644); err != nil {
		t.Fatalf("harness: write the intent manifest %s: %v", path, err)
	}
}

// awaitHostFault waits for a workspace's host stream to carry an open fault
// satisfying the predicate.
func awaitHostFault(t *testing.T, d *harness.Daemon, host *harness.Stream[*agentreplv1.WatchHostWorkspaceResponse], what string, pred func(*agentreplv1.HostFault) bool) *agentreplv1.HostFault {
	t.Helper()
	var found *agentreplv1.HostFault
	ctx, cancel := d.WaitCtx()
	defer cancel()
	harness.AwaitView(t, ctx, host, what, func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		for _, f := range r.GetHost().GetExisting().GetLive().GetFaults() {
			if pred(f) {
				found = f
				return true
			}
		}
		return false
	})
	return found
}

// shortTimeout bounds a structural "is it already true" probe to a small
// window, so a violated invariant fails fast rather than after the whole
// test's deadline.
func shortTimeout(t *testing.T, parent context.Context, d time.Duration) context.Context {
	t.Helper()
	ctx, cancel := context.WithTimeout(parent, d)
	t.Cleanup(cancel)
	return ctx
}
