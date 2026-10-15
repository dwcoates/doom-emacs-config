package harness

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"time"
)

// ArtifactsEnv names the directory a FAILING integration test copies its
// daemon's structured logs into. Unset, MainAt points it at a fresh run
// directory under the user cache dir (defaultArtifacts), so a red always
// keeps its WHOLE logs: a bounded tail of a verbose run log routinely ends
// before the record that explains the failure (2026-10-12: a handover's
// landing deploy was nowhere in the 64 KiB a failing test printed).
//
// WHY: the state root is a per-test temp dir the testing package deletes on
// the way out, so a red in a full run used to leave only its one failure line
// ("waiting for the merge's terminal: context deadline exceeded") and no
// record of what the daemon did. The e2e layer has had the same rule
// (AGENT_REPL_E2E_ARTIFACTS) since its own reds went unreadable.
const ArtifactsEnv = "AGENT_REPL_INTEGRATION_ARTIFACTS"

// artifactTailBytes bounds how much of one log a failing test prints when no
// artifacts directory is configured: the whole of an ordinary single-test run
// log, never an unbounded file in a full run's transcript.
const artifactTailBytes = 64 << 10

// preserveFailureLogs keeps every `.log` under logsDir past the test: copied
// into dest when dest is non-empty, otherwise logged as a bounded tail.
// Every step it cannot take is said through logf rather than dropped.
func preserveFailureLogs(logf func(format string, args ...any), logsDir, dest string) {
	entries, err := os.ReadDir(logsDir)
	if err != nil {
		logf("integration artifacts: no logs to preserve from %s: %v", logsDir, err)
		return
	}
	if dest != "" {
		if err := os.MkdirAll(dest, 0o755); err != nil {
			logf("integration artifacts: cannot create %s (%v); falling back to log tails", dest, err)
			dest = ""
		}
	}
	for _, entry := range entries {
		if entry.IsDir() || filepath.Ext(entry.Name()) != ".log" {
			continue
		}
		src := filepath.Join(logsDir, entry.Name())
		body, err := os.ReadFile(src)
		if err != nil {
			logf("integration artifacts: read %s: %v", src, err)
			continue
		}
		if dest != "" {
			if err := os.WriteFile(filepath.Join(dest, entry.Name()), body, 0o644); err != nil {
				logf("integration artifacts: write %s: %v", filepath.Join(dest, entry.Name()), err)
			}
			continue
		}
		logf("integration artifacts: %s (last %d bytes of %d):\n%s",
			entry.Name(), min(len(body), artifactTailBytes), len(body), tailBytes(body, artifactTailBytes))
	}
	if dest != "" {
		logf("integration artifacts: the daemon's structured logs are preserved under %s", dest)
	}
}

// tailBytes answers the last limit bytes of body, cut forward to the next
// newline so the tail never opens mid-record.
func tailBytes(body []byte, limit int) string {
	if len(body) <= limit {
		return string(body)
	}
	tail := body[len(body)-limit:]
	if i := bytes.IndexByte(tail, '\n'); i >= 0 {
		tail = tail[i+1:]
	}
	return string(tail)
}

// artifactDirName turns a Go test name into one path segment.
func artifactDirName(name string) string {
	return strings.NewReplacer("/", "_", string(filepath.Separator), "_").Replace(name)
}

// keptArtifactRuns is how many runs' preserved logs the default artifacts
// root keeps: the newest, so a red from a recent run is always there to read
// and the directory never grows without bound.
const keptArtifactRuns = 10

// defaultArtifacts creates this run's directory under root, named for its
// start instant and pid so runs sort by age and never collide, and removes
// every run directory but the newest keptArtifactRuns (this one included).
func defaultArtifacts(root string, now time.Time, pid int) (string, error) {
	dir := filepath.Join(root, fmt.Sprintf("%s-%d", now.UTC().Format("20060102T150405.000000000Z"), pid))
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("harness: create the run's artifacts directory: %w", err)
	}
	entries, err := os.ReadDir(root)
	if err != nil {
		return "", fmt.Errorf("harness: list the artifacts root %s: %w", root, err)
	}
	var runs []string
	for _, e := range entries {
		if e.IsDir() {
			runs = append(runs, e.Name())
		}
	}
	slices.Sort(runs)
	for _, old := range runs[:max(0, len(runs)-keptArtifactRuns)] {
		if err := os.RemoveAll(filepath.Join(root, old)); err != nil {
			return "", fmt.Errorf("harness: remove the old artifacts run %s: %w", old, err)
		}
	}
	return dir, nil
}

// useDefaultArtifacts points ArtifactsEnv at a fresh run directory under the
// user cache dir when the run did not name one.
func useDefaultArtifacts() error {
	if os.Getenv(ArtifactsEnv) != "" {
		return nil
	}
	cache, err := os.UserCacheDir()
	if err != nil {
		return fmt.Errorf("harness: locate the user cache dir for test artifacts: %w", err)
	}
	dir, err := defaultArtifacts(filepath.Join(cache, "agent-repl", "integration-artifacts"), time.Now(), os.Getpid())
	if err != nil {
		return err
	}
	return os.Setenv(ArtifactsEnv, dir)
}

// cleanupT is what registerLogChecks needs of a test: *testing.T's cleanup
// registration and its verdict.
type cleanupT interface {
	Cleanup(func())
	Failed() bool
}

// registerLogChecks registers a daemon's log sweeps and, AHEAD of them, KEEP,
// which runs once the test has failed. t.Cleanup unwinds last-registered
// first, so KEEP runs AFTER every sweep and a test failed by a sweep alone
// keeps its logs too. Registered after the sweeps, KEEP read only the body's
// verdict, and an unexpected warning record failed a full run with nothing
// kept to say what the daemon did around it (2026-10-15,
// TestUpdateMergeQueuePauseWithNoRepositoryPausesEveryRepositoryWithAQueue:
// daemon.feed.pin_refused). The state root KEEP reads is a temp dir the
// testing package removes only after every cleanup.
func registerLogChecks(t cleanupT, keep func(), sweeps ...func()) {
	t.Cleanup(func() {
		if t.Failed() {
			keep()
		}
	})
	for _, sweep := range sweeps {
		t.Cleanup(sweep)
	}
}
