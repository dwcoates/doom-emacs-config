package main

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/buildid"
	"claude-repld/internal/deploy"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

func envOf(values map[string]string) func(string) string {
	return func(key string) string { return values[key] }
}

func TestTheDeployPathsHonorTheOperatorOverrides(t *testing.T) {
	tests := []struct {
		name         string
		env          map[string]string
		wantPlistDir string
		wantReport   string
	}{
		{name: "the plist directory override", env: map[string]string{envLaunchAgentsDir: "/agents", "AGENT_REPL_LOCK_DIR": "/run"},
			wantPlistDir: "/agents", wantReport: "/run"},
		{name: "the default plist directory", env: map[string]string{"AGENT_REPL_LOCK_DIR": "/run"},
			wantPlistDir: "Library/LaunchAgents", wantReport: "/run"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := resolveDeployPaths(envOf(tc.env))

			// Assert
			if err != nil {
				t.Fatalf("resolveDeployPaths: %v", err)
			}
			if !strings.HasSuffix(got.plistDir, tc.wantPlistDir) {
				t.Fatalf("plist dir = %q, want it to end %q", got.plistDir, tc.wantPlistDir)
			}
			if got.reportDir != tc.wantReport {
				t.Fatalf("report dir = %q, want %q", got.reportDir, tc.wantReport)
			}
			if !strings.HasSuffix(got.cacheBin, filepath.Join(".cache", "agent-repl", "bin")) {
				t.Fatalf("cache bin = %q, want the services' install directory", got.cacheBin)
			}
		})
	}
}

func deployerParamsFor(t *testing.T, selfExe string) deployerParams {
	t.Helper()
	return deployerParams{
		Surfaces:  dlog.NewTestSurfaces(),
		Checkout:  t.TempDir(),
		StateDir:  t.TempDir(),
		SelfExe:   selfExe,
		Bundle:    buildid.NewShimBundle(filepath.Join(t.TempDir(), "main.js"), "fake-build"),
		Rollout:   nil,
		Clients:   &deployClientsForwarder{},
		Runner:    nil,
		Store:     "/sock/store.sock",
		Workspace: func() []ids.WorkspaceID { return nil },
		Getenv:    envOf(map[string]string{"AGENT_REPL_LOCK_DIR": t.TempDir()}),
	}
}

func TestADeployerIsNotBuiltWithoutItsOwnBuild(t *testing.T) {
	// Arrange: the binary this daemon claims to run from does not exist.
	p := deployerParamsFor(t, filepath.Join(t.TempDir(), "gone"))

	// Act
	_, err := buildDeployer(context.Background(), p)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "hash this daemon's own binary") {
		t.Fatalf("buildDeployer = %v, want the refusal naming its own binary", err)
	}
}

func TestADeployerIsNotBuiltWithoutARollout(t *testing.T) {
	// Arrange
	exe := filepath.Join(t.TempDir(), "claude-repld")
	if err := os.WriteFile(exe, []byte("binary"), 0o755); err != nil {
		t.Fatalf("write: %v", err)
	}
	p := deployerParamsFor(t, exe)

	// Act
	_, err := buildDeployer(context.Background(), p)

	// Assert: the deploy's own constructor refuses the missing collaborator.
	if err == nil || !strings.Contains(err.Error(), "rollout controller is required") {
		t.Fatalf("buildDeployer = %v, want the deploy's refusal", err)
	}
}

// fakeClients records what a bound forwarder hands on.
type fakeClients struct {
	pushedElisp  []string
	pushedWebapp []ids.WorkspaceID
}

func (f *fakeClients) EmacsBuilds() []deploy.EmacsClient {
	return []deploy.EmacsClient{{ID: "s1", Build: "old"}}
}

func (f *fakeClients) PushReloadElisp(streams []string, _, _ string) int {
	f.pushedElisp = append(f.pushedElisp, streams...)
	return len(streams)
}

func (f *fakeClients) WebviewBuilds() map[ids.WorkspaceID][]string {
	return map[ids.WorkspaceID][]string{"w1": {"old"}}
}

func (f *fakeClients) PushReloadWebapp(ws ids.WorkspaceID) {
	f.pushedWebapp = append(f.pushedWebapp, ws)
}

func TestAnUnboundClientsForwarderAnswersNoClient(t *testing.T) {
	// Arrange
	f := &deployClientsForwarder{}

	// Act
	emacs, webviews, reached := f.EmacsBuilds(), f.WebviewBuilds(), f.PushReloadElisp([]string{"s1"}, "/r", "b")
	f.PushReloadWebapp("w1")

	// Assert
	if len(emacs) != 0 || len(webviews) != 0 || reached != 0 {
		t.Fatalf("unbound = (%v, %v, %d), want no client and no push reached", emacs, webviews, reached)
	}
}

func TestABoundClientsForwarderHandsEverythingOn(t *testing.T) {
	// Arrange
	target := &fakeClients{}
	f := &deployClientsForwarder{}
	f.bind(target)

	// Act
	emacs, webviews := f.EmacsBuilds(), f.WebviewBuilds()
	reached := f.PushReloadElisp([]string{"s1"}, "/r", "b")
	f.PushReloadWebapp("w1")

	// Assert
	if len(emacs) != 1 || len(webviews["w1"]) != 1 || reached != 1 {
		t.Fatalf("bound = (%v, %v, %d), want the server's answers", emacs, webviews, reached)
	}
	if len(target.pushedElisp) != 1 || len(target.pushedWebapp) != 1 {
		t.Fatalf("pushes = (%v, %v), want both handed on", target.pushedElisp, target.pushedWebapp)
	}
}
