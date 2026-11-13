package kit

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
)

// CgroupCPUStatPath is the sandbox container's own CPU accounting (cgroup
// v2). It is EVIDENCE, not the verdict: the probe's bound is already kept on
// the granted clock, which charges no time the container spent starved, and
// this says on top what the container's quota did across a probe that still
// went unanswered.
const CgroupCPUStatPath = "/sys/fs/cgroup/cpu.stat"

// ReadCgroupCPUStat reads a cgroup v2 cpu.stat file into its counters.
func ReadCgroupCPUStat(path string) (map[string]int64, error) {
	body, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	out := map[string]int64{}
	for _, line := range strings.Split(strings.TrimSpace(string(body)), "\n") {
		fields := strings.Fields(line)
		if len(fields) != 2 {
			continue
		}
		n, perr := strconv.ParseInt(fields[1], 10, 64)
		if perr != nil {
			return nil, fmt.Errorf("%s: %q: %w", path, line, perr)
		}
		out[fields[0]] = n
	}
	return out, nil
}

// CpuOverProbe says what the container's CPU did across an unanswered probe
// that began with BEFORE and lasted TOOK: how much CPU the container used
// against what its quota allowed, and how often and how long it was
// throttled. Every way it cannot be said is said instead.
func CpuOverProbe(before map[string]int64, beforeErr error, took time.Duration) string {
	if beforeErr != nil {
		return fmt.Sprintf("\n  (the container's CPU accounting could not be read before the probe: %v)", beforeErr)
	}
	after, err := ReadCgroupCPUStat(CgroupCPUStatPath)
	if err != nil {
		return fmt.Sprintf("\n  (the container's CPU accounting could not be read after the probe: %v)", err)
	}
	delta := func(key string) int64 { return after[key] - before[key] }
	return fmt.Sprintf("\n  the container's CPU across the %s probe: used %dms, throttled in %d of %d periods for %dms",
		took.Round(time.Millisecond), delta("usage_usec")/1000, delta("nr_throttled"), delta("nr_periods"), delta("throttled_usec")/1000)
}

// MessagesFile is where that tail is filed with the other evidence.
const MessagesFile = "emacs.messages.log"

// ArtifactTailFileBytes bounds one log's tail when the artifacts are printed
// rather than kept, and ArtifactTailBudget bounds them all together: enough
// for the last few hundred records of every sink a scenario writes, never an
// unbounded dump into a full run's transcript. A log past the budget is still
// NAMED, with its size, so its absence from the output is never silent.
const (
	ArtifactTailFileBytes = 16 << 10
	ArtifactTailBudget    = 512 << 10
)

// TailArtifacts prints a bounded tail of every log collected under root:
// Emacs's *Messages* and pty output first, then every other `.log` and the
// native backtrace, in path order.
func TailArtifacts(logf func(format string, args ...any), root string) {
	var files []string
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			logf("emacs artifacts: walk %s: %v", path, err)
			return nil
		}
		if entry.Type().IsRegular() && (filepath.Ext(path) == ".log" || filepath.Base(path) == NativeBacktraceFile) {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		logf("emacs artifacts: walk %s: %v", root, err)
	}
	sort.SliceStable(files, func(i, j int) bool {
		return ArtifactTailRank(files[i]) < ArtifactTailRank(files[j])
	})
	budget := ArtifactTailBudget
	for _, path := range files {
		rel, relErr := filepath.Rel(root, path)
		if relErr != nil {
			rel = path
		}
		body, readErr := os.ReadFile(path)
		if readErr != nil {
			logf("emacs artifacts: read %s: %v", rel, readErr)
			continue
		}
		// A WHOLE TAIL OR NONE: a remainder smaller than one tail would print
		// a sliver cut forward to the next record, which may be nothing.
		if budget < ArtifactTailFileBytes {
			logf("emacs artifacts: %s (%d bytes) not printed: the tail budget of %d bytes is spent (set %s to keep every log whole)",
				rel, len(body), ArtifactTailBudget, ArtifactsEnv)
			continue
		}
		tail := TailBytes(body, ArtifactTailFileBytes)
		budget -= len(tail)
		logf("emacs artifacts: %s (last %d bytes of %d):\n%s", rel, len(tail), len(body), tail)
	}
	logf("emacs artifacts: printed tails of %d log(s); set %s to keep them whole", len(files), ArtifactsEnv)
}

// ArtifactTailRank orders the printed tails: Emacs's own words first.
func ArtifactTailRank(path string) int {
	switch filepath.Base(path) {
	case MessagesFile:
		return 0
	case "emacs.pty.log":
		return 1
	case NativeBacktraceFile:
		return 2
	}
	return 3
}

// NativeBacktraceFile is where the full capture lands in the failure
// artifacts. The error message carries a trimmed head; the file carries every
// frame of every thread.
const NativeBacktraceFile = "emacs.native-backtrace.txt"

// NativeBacktraceHeadFrames is how much of the capture the failure message
// quotes inline BEYOND the main thread, which is never trimmed. Enough to
// name the loop without burying the failure.
const NativeBacktraceHeadFrames = 40

// NativeBacktraceMainThread names the thread whose stack is the answer. Emacs
// runs lisp on ONE thread and gdb numbers it 1; the rest are glib's helpers,
// which are asleep in `poll` in every capture ever taken here and say nothing
// about why the editor stopped.
const NativeBacktraceMainThread = "Thread 1 "

// NativeBacktraceExcerpt is what the failure message quotes inline.
//
// WHAT IT GUARANTEES, and the reason it is not a plain head: THE MAIN
// THREAD'S STACK APPEARS IN FULL, FIRST, whatever else is trimmed. gdb's own
// output puts it last and a head-of-N excerpt therefore dropped exactly the
// frames the failure turns on. Everything before the first thread section --
// gdb's preamble and its `info threads` table -- is kept as the index to what
// follows, and the remaining threads fill whatever budget is left.
func NativeBacktraceExcerpt(text string) []string {
	lines := strings.Split(text, "\n")
	preamble, sections := NativeBacktraceSections(lines)

	out := append([]string{}, preamble...)
	var rest [][]string
	seenMain := false
	for _, section := range sections {
		if !seenMain && strings.HasPrefix(section[0], NativeBacktraceMainThread) {
			// The main thread, in full, immediately after the index. gdb
			// prints it twice (once for `thread apply 1 bt`, once inside the
			// `all` walk); only the first copy is quoted.
			seenMain = true
			out = append(out, section...)
			continue
		}
		if seenMain && strings.HasPrefix(section[0], NativeBacktraceMainThread) {
			continue
		}
		rest = append(rest, section)
	}

	budget := NativeBacktraceHeadFrames
	dropped := 0
	for _, section := range rest {
		if budget-len(section) < 0 {
			dropped += len(section)
			continue
		}
		budget -= len(section)
		out = append(out, section...)
	}
	if dropped > 0 {
		out = append(out, fmt.Sprintf("... %d more lines in %s", dropped, NativeBacktraceFile))
	}
	return out
}

// NativeBacktraceSections splits a capture into gdb's preamble and one group
// of lines per thread. A thread's group runs from its `Thread N (...)` header
// to the line before the next one, so a stack is never cut in half.
func NativeBacktraceSections(lines []string) (preamble []string, sections [][]string) {
	current := -1
	for _, line := range lines {
		if strings.HasPrefix(line, "Thread ") && strings.Contains(line, "(") {
			sections = append(sections, []string{line})
			current = len(sections) - 1
			continue
		}
		if current < 0 {
			preamble = append(preamble, line)
			continue
		}
		sections[current] = append(sections[current], line)
	}
	return preamble, sections
}

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
