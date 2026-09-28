package stateroot_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/stateroot"
)

func TestRootPrecedence(t *testing.T) {
	home, err := os.UserHomeDir()
	if err != nil {
		t.Skipf("no home directory: %v", err)
	}
	tests := []struct {
		name     string
		override string
		fromEnv  string
		want     string
	}{
		{name: "override wins", override: "/a/flag", fromEnv: "/b/env", want: "/a/flag"},
		{name: "environment when no override", fromEnv: "/b/env", want: "/b/env"},
		{name: "default when neither", want: filepath.Join(home, stateroot.DefaultDirName)},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, err := stateroot.Root(tc.override, tc.fromEnv)

			// Assert.
			if err != nil {
				t.Fatalf("Root() error = %v", err)
			}
			if got.Dir() != tc.want {
				t.Fatalf("Dir() = %q, want %q", got.Dir(), tc.want)
			}
		})
	}
}

func TestRootCleansAndAbsolutizes(t *testing.T) {
	// Arrange, Act.
	got, err := stateroot.Root("/a/b/../c/", "")

	// Assert.
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	if got.Dir() != "/a/c" {
		t.Fatalf("Dir() = %q, want %q", got.Dir(), "/a/c")
	}
}

func TestLayoutPaths(t *testing.T) {
	// Arrange.
	l, err := stateroot.Root("/state", "")
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	tests := []struct {
		name string
		got  string
		want string
	}{
		{name: "daemon addr", got: l.DaemonAddr(), want: "/state/daemon.addr"},
		{name: "database", got: l.DB(), want: "/state/wsm.db"},
		{name: "logs dir", got: l.LogsDir(), want: "/state/logs"},
		{name: "run log", got: l.RunLog(), want: "/state/logs/daemon.run.log"},
		{name: "sock dir", got: l.SockDir(), want: "/state/sock"},
		{name: "shim socket", got: l.ShimSocket("abcdef0123456789"), want: "/state/sock/abcdef0123456789.sock"},
		{name: "intent dir", got: l.IntentDir(), want: "/state/intent"},
		{name: "intent manifest", got: l.IntentManifest(), want: "/state/intent/manifest.json"},
		{name: "output dir", got: l.OutputDir(), want: "/state/output"},
		{name: "command file glob", got: l.CommandFileGlob(), want: "/state/output/workspace_commands_*.json"},
		{name: "held prompt dir", got: l.HeldPromptDir(), want: "/state/held-prompts"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert.
			if tc.got != tc.want {
				t.Fatalf("got %q, want %q", tc.got, tc.want)
			}
		})
	}
}

func TestCheckSocketPathBudgetFits(t *testing.T) {
	// Arrange.
	l, err := stateroot.Root("/state", "")
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}

	// Act.
	got := l.CheckSocketPathBudget()

	// Assert.
	if got != nil {
		t.Fatalf("CheckSocketPathBudget() = %v, want nil", got)
	}
}

func TestCheckSocketPathBudgetOverflows(t *testing.T) {
	// Arrange.
	l, err := stateroot.Root("/"+strings.Repeat("d", 90), "")
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}

	// Act.
	got := l.CheckSocketPathBudget()

	// Assert.
	if got == nil {
		t.Fatal("CheckSocketPathBudget() = nil, want an overflow refusal")
	}
}

func TestDirs(t *testing.T) {
	// Arrange.
	l, err := stateroot.Root("/state", "")
	if err != nil {
		t.Fatalf("Root() error = %v", err)
	}
	want := []string{"/state", "/state/logs", "/state/sock", "/state/intent", "/state/output"}

	// Act.
	got := l.Dirs()

	// Assert.
	if len(got) != len(want) {
		t.Fatalf("Dirs() = %v, want %v", got, want)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("Dirs()[%d] = %q, want %q", i, got[i], want[i])
		}
	}
}
