package harness

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
)

// ArtifactsEnv names the directory a FAILING integration test copies its
// daemon's structured logs into. Unset, the logs are emitted as bounded tails
// into the test's own output instead -- never nothing.
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
