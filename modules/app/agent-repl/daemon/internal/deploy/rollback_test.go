package deploy

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/ids"
)

// liveTree answers every file under the harness's live locations — the
// checkout (shim bundle, webapp dist, daemon binary, their stamps) and the
// cache bin — by path, with its bytes.
func liveTree(t *testing.T, h *harness) map[string]string {
	t.Helper()
	out := map[string]string{}
	for _, root := range []string{h.live.ModuleRoot, h.live.CacheBin} {
		err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
			if err != nil || entry.IsDir() {
				return err
			}
			out[path] = readFile(t, path)
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}
	return out
}

// seedPreviousStamp puts a stamp beside the installed shim, which the fresh
// build's own stamp replaces, so a rollback has a previous stamp to restore.
func seedPreviousStamp(t *testing.T, h *harness) {
	t.Helper()
	writeFile(t, filepath.Join(filepath.Dir(h.live.ShimMain()), ".built-sha"), "sha-"+theOld.shim)
}

// staleDaemon makes the running daemon older than the fresh build, so the
// deploy installs a daemon and decides to hand it over.
func staleDaemon(t *testing.T, h *harness) {
	t.Helper()
	h.d.deps.DaemonBuild = hashOf(t, theOld.daemon)
}

func TestAFailureAfterTheInstallBeganLeavesThePreviousBuild(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		// restarts are every service restart the deploy and its rollback made.
		restarts []string
	}{
		{"an install that fails part way", func(t *testing.T, h *harness) {
			failInstall(t, h)
		}, nil},
		{"a store restart that fails", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			h.services.storeErr = errors.New("launchctl refused")
			h.services.recovers = true
		}, []string{"store", "store"}},
		{"a sidecar restart that fails", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			h.services.sidecarErr = errors.New("kickstart refused")
			h.services.recovers = true
		}, []string{"sidecar", "sidecar"}},
		{"a refused handover after the store restarted", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
			staleDaemon(t, h)
			h.rollout.handErr = errors.New("a handover is in flight")
		}, []string{"store", "store"}},
		{"a refused handover after the sidecar restarted", func(t *testing.T, h *harness) {
			h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))
			staleDaemon(t, h)
			h.rollout.handErr = errors.New("a handover is in flight")
		}, []string{"sidecar", "sidecar"}},
		{"a refused restart across a layout change", func(t *testing.T, h *harness) {
			staleDaemon(t, h)
			h.freshLayout = runningLayout + 1
			h.rollout.restartErr = errors.New("a handover is in flight")
		}, nil},
		{"a fresh daemon whose layout cannot be read", func(t *testing.T, h *harness) {
			staleDaemon(t, h)
			h.layoutErr = errors.New("exec format error")
		}, nil},
		{"a workspace set that cannot be listed", func(t *testing.T, h *harness) {
			h.d.deps.Workspaces = func(context.Context) ([]ids.WorkspaceID, error) { return nil, errors.New("wsm closed") }
		}, nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			seedPreviousStamp(t, h)
			tc.arrange(t, h)
			before := liveTree(t, h)

			// Act
			_, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err == nil {
				t.Fatalf("Deploy succeeded, want the failure")
			}
			if after := liveTree(t, h); !reflect.DeepEqual(after, before) {
				t.Fatalf("live artifacts after the failed deploy =\n%v\nwant the previous build exactly =\n%v", after, before)
			}
			if got := h.services.Calls(); !reflect.DeepEqual(got, tc.restarts) {
				t.Fatalf("service restarts = %v, want %v", got, tc.restarts)
			}
			if !logged(h.log, "info", opRollback, "rolled back: the previous build is installed and running") {
				t.Fatalf("records = %+v, want the rollback at INFO", records(h.log, opRollback))
			}
		})
	}
}

func TestAFailureBeforeTheInstallRollsNothingBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.builder.fail = &BuildFailed{Step: "webapp", Detail: "tsc"}

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the build failure")
	}
	if got := records(h.log, opRollback); len(got) != 0 {
		t.Fatalf("rollback records = %+v, want none: nothing was installed", got)
	}
}

func TestASuccessfulDeployRollsNothingBack(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := h.d.Deploy(context.Background(), false); err != nil {
		t.Fatalf("Deploy: %v", err)
	}

	// Assert
	if got := records(h.log, opRollback); len(got) != 0 {
		t.Fatalf("rollback records = %+v, want none", got)
	}
	if got := readFile(t, h.live.DaemonBin()); got != theFresh.daemon {
		t.Fatalf("installed daemon = %q, want the fresh build", got)
	}
}

func TestAnArtifactThatStoodNowhereIsRemovedByTheRollback(t *testing.T) {
	// Arrange: no shim-lock was installed before this deploy.
	h := newHarness(t)
	if err := os.Remove(h.live.CacheBinPath("shim-lock")); err != nil {
		t.Fatal(err)
	}
	h.services.sidecarErr = errors.New("kickstart refused")
	h.report(t, buildreport.ServiceSidecar, 102, hashOf(t, theOld.sidecar))

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the failure")
	}
	if _, err := os.Stat(h.live.CacheBinPath("shim-lock")); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("shim-lock after the rollback: stat = %v, want it absent as it was", err)
	}
}

func TestARollbackThatCannotRestoreAnArtifactSaysSoAtError(t *testing.T) {
	// Arrange: the handover is refused after the daemon's directory became
	// unwritable, so the previous daemon cannot be put back.
	h := newHarness(t)
	staleDaemon(t, h)
	h.rollout.handErr = errors.New("a handover is in flight")
	h.rollout.onHandOver = func() { failInstall(t, h) }

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the refused handover")
	}
	if !logged(h.log, "error", opRollback, "the rollback did NOT restore the previous build") {
		t.Fatalf("records = %+v, want the failed rollback at ERROR", records(h.log, opRollback))
	}
	if got := readFile(t, h.live.ShimMain()); got != theOld.shim {
		t.Fatalf("installed shim = %q, want the previous build restored past the daemon's failure", got)
	}
}

func TestARollbackWhoseServiceWillNotRestartSaysSoAtError(t *testing.T) {
	// Arrange: the store restart fails, and fails again onto the restored build.
	h := newHarness(t)
	h.report(t, buildreport.ServiceStore, 101, hashOf(t, theOld.store))
	h.services.storeErr = errors.New("launchctl refused")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err == nil {
		t.Fatalf("Deploy succeeded, want the failure")
	}
	if !logged(h.log, "error", opRollback, "could not restart the store and the sidecar onto the restored build") {
		t.Fatalf("records = %+v, want the failed restart at ERROR", records(h.log, opRollback))
	}
}
