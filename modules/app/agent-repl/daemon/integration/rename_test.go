//go:build integration

package integration

import (
	"context"
	"errors"
	"os"
	"os/exec"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// runWorkspaceCLI runs `agent-repld workspace ARGS...` against D, answering
// its combined output and its error.
func runWorkspaceCLI(t *testing.T, ctx context.Context, d *harness.Daemon, args ...string) (string, error) {
	t.Helper()
	cli := exec.CommandContext(ctx, harness.DaemonBinary(t), append(append([]string{"workspace"}, args...), "-state-dir", d.StateDir)...)
	cli.Env = append(os.Environ(), "AGENT_REPL_STATE_DIR="+d.StateDir, "AGENT_REPL_FORBID_VENDOR_CALLS=1")
	out, err := cli.CombinedOutput()
	return string(out), err
}

// TestTheCLIRenamesAWorkspace is the rename's one wiring test: the CLI
// binary's `workspace set -name` reaches the daemon process, which republishes
// the roster, and a later CLI call resolves the new name and no longer the
// old one. The verb's refusals and republishes are the workspace package's
// unit tests; what only the processes show is the CLI's roster resolution
// following the rename.
func TestTheCLIRenamesAWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	roster := f.d.WatchRoster()
	before := awaitRoster(t, f.d, roster, "the workspace's row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()) != nil
	})
	oldName := rosterRow(before, f.ws.GetId()).GetName().GetText()
	ctx, cancel := f.d.WaitCtx()
	defer cancel()

	// Act
	out, err := runWorkspaceCLI(t, ctx, f.d, "set", f.ws.GetDir(), "-name", "better-name")

	// Assert: the rename answered its name, the roster shows it, the new
	// name resolves, and the old name no longer does.
	if err != nil || !strings.Contains(out, "better-name") {
		t.Fatalf("workspace set -name = (%v, %q), want success naming better-name", err, out)
	}
	awaitRoster(t, f.d, roster, "the renamed row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, f.ws.GetId()).GetName().GetText() == "better-name"
	})
	if out, err := runWorkspaceCLI(t, ctx, f.d, "status", "better-name"); err != nil || !strings.Contains(out, f.ws.GetId()) {
		t.Fatalf("workspace status better-name = (%v, %q), want the renamed workspace", err, out)
	}
	out, err = runWorkspaceCLI(t, ctx, f.d, "status", oldName)
	var exit *exec.ExitError
	if !errors.As(err, &exit) || !strings.Contains(out, "holds no workspace named") {
		t.Fatalf("workspace status %s = (%v, %q), want the old name to resolve nowhere", oldName, err, out)
	}
}
