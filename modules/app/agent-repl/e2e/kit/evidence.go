package kit

import (
	"bytes"
	"fmt"
	"os"
)

// ArtifactsEnv names the directory a failing test copies its structured logs
// into. Unset (the default) means the logs are emitted as bounded tails into
// the test's own t.Log output instead — never nothing.
const ArtifactsEnv = "AGENT_REPL_E2E_ARTIFACTS"

// TailBytes answers the last limit bytes of body, cut forward to the next
// newline so the tail never opens mid-record and read as a broken JSONL line.
func TailBytes(body []byte, limit int) string {
	if len(body) <= limit {
		return string(body)
	}
	tail := body[len(body)-limit:]
	if i := bytes.IndexByte(tail, '\n'); i >= 0 {
		tail = tail[i+1:]
	}
	return string(tail)
}

// StopLogTailBytes bounds the log a failed stop prints: enough for the last
// few dozen records, which span the stop.
const StopLogTailBytes = 8 << 10

// LogTailOf answers the end of a child's log file, or why it cannot.
func LogTailOf(path string) string {
	body, err := os.ReadFile(path)
	if err != nil {
		return fmt.Sprintf("(cannot read %s: %v)", path, err)
	}
	return TailBytes(body, StopLogTailBytes)
}
