package reposcans

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestEveryRestatementOfTheLogRetentionMatchesTheLibrary pins the three days
// every log is kept (agentrepl/logging DefaultRetention) to the two places that
// cannot import it: the log reader is one standalone file built without the
// module, and Emacs is another language. A change to one must change all three.
func TestEveryRestatementOfTheLogRetentionMatchesTheLibrary(t *testing.T) {
	cases := []struct {
		file string
		want string
	}{
		{file: "agent-shim/logging/go/retention.go", want: "const DefaultRetention = 72 * time.Hour"},
		{file: "bin/logs-reader.go", want: "const sinkRetention = 72 * time.Hour"},
		{file: "lisp/core.el", want: "(defcustom agent-repl-log-retention-seconds (* 3 86400)"},
	}
	root := moduleRootDir(t)
	for _, tc := range cases {
		t.Run(tc.file, func(t *testing.T) {
			// Arrange.
			source, err := os.ReadFile(filepath.Join(root, tc.file))
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act / Assert.
			if !strings.Contains(string(source), tc.want) {
				t.Fatalf("%s no longer states the three-day log retention as %q; update every restatement together", tc.file, tc.want)
			}
		})
	}
}

// TestEveryRestatementOfTheStorageSweepWindowsMatchesThePackage pins the
// storage sweep's windows (daemon/internal/storagesweep) to the prose that
// restates them: the daemon's AGENTS.md, the runtime skill and the
// PruneStorage contract's comments. A change to a window must change them all.
func TestEveryRestatementOfTheStorageSweepWindowsMatchesThePackage(t *testing.T) {
	cases := []struct {
		name string
		file string
		want string
	}{
		{name: "the landed window", file: "daemon/internal/storagesweep/sweep.go", want: "DefaultLandedAfter = 3 * 24 * time.Hour"},
		{name: "the closed window", file: "daemon/internal/storagesweep/sweep.go", want: "DefaultClosedAfter = 5 * 24 * time.Hour"},
		{name: "the aged window", file: "daemon/internal/storagesweep/sweep.go", want: "DefaultAgedAfter = 5 * 24 * time.Hour"},
		{name: "the logs window", file: "daemon/internal/storagesweep/sweep.go", want: "DefaultLogsAfter = 5 * 24 * time.Hour"},
		{name: "AGENTS.md, the landed window", file: "daemon/AGENTS.md", want: "the whole directory, 3\n  days after the close (`landed`)"},
		{name: "AGENTS.md, the closed window", file: "daemon/AGENTS.md", want: "only `backups/`, 5 days after the close"},
		{name: "AGENTS.md, the aged window", file: "daemon/AGENTS.md", want: "own modification time is over 5 days old is deleted (`aged`)"},
		{name: "AGENTS.md, the logs window", file: "daemon/AGENTS.md", want: "is over 5 days old loses every\ndaemon-written log file"},
		{name: "the runtime skill, the closed window", file: "skills/agent-repl-runtime/SKILL.md", want: "the transcript backups of workspaces closed over five days"},
		{name: "the runtime skill, the aged window", file: "skills/agent-repl-runtime/SKILL.md", want: "profiling sessions and test-reuse records over five days old"},
		{name: "the runtime skill, the logs window", file: "skills/agent-repl-runtime/SKILL.md", want: "daemon-written logs of workspaces nobody touched for five days"},
		{name: "the contract, the landed window", file: "proto/src/agentrepl/v1/endpoint_prune_storage.proto", want: "more than three days ago: the whole directory"},
		{name: "the contract, the closed window", file: "proto/src/agentrepl/v1/endpoint_prune_storage.proto", want: "every workspace closed more than five\n  // days ago"},
		{name: "the contract, the aged window", file: "proto/src/agentrepl/v1/endpoint_prune_storage.proto", want: "A cache directory's entry older than five days."},
		{name: "the contract, the logs window", file: "proto/src/agentrepl/v1/endpoint_prune_storage.proto", want: "of a workspace nobody touched for five days"},
	}
	root := moduleRootDir(t)
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			source, err := os.ReadFile(filepath.Join(root, tc.file))
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act / Assert.
			if !strings.Contains(string(source), tc.want) {
				t.Fatalf("%s no longer states the storage sweep's window as %q; update every restatement together", tc.file, tc.want)
			}
		})
	}
}

// TestEveryRestatementOfTheClosedWorkspaceExpiryMatchesTheReaper pins the
// three days a closed workspace's worktree may go untouched
// (daemon/internal/worktreereap) to the prose that restates it.
func TestEveryRestatementOfTheClosedWorkspaceExpiryMatchesTheReaper(t *testing.T) {
	cases := []struct {
		name string
		file string
		want string
	}{
		{name: "the window", file: "daemon/internal/worktreereap/reaper.go", want: "DefaultClosedExpireAfter = 3 * 24 * time.Hour"},
		{name: "AGENTS.md", file: "daemon/AGENTS.md", want: "`AGENT_REPL_WORKTREE_REAP_CLOSED_EXPIRE` (default 3 days"},
		{name: "the runtime skill", file: "skills/agent-repl-runtime/SKILL.md", want: "the worktree of every closed workspace nobody\n  touched for three days"},
		{name: "the contract", file: "proto/src/agentrepl/v1/endpoint_reap_worktrees.proto", want: "closed and nobody has touched it for three days"},
	}
	root := moduleRootDir(t)
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			source, err := os.ReadFile(filepath.Join(root, tc.file))
			if err != nil {
				t.Fatalf("read %s: %v", tc.file, err)
			}

			// Act / Assert.
			if !strings.Contains(string(source), tc.want) {
				t.Fatalf("%s no longer states the closed-workspace expiry as %q; update every restatement together", tc.file, tc.want)
			}
		})
	}
}
