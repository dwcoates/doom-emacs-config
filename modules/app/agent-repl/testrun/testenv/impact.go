package testenv

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"
	"syscall"
)

// ImpactOut names the file a coverage-recorded Go unit's harness appends the
// files each top-level test executed to (AppendImpactWindow), one line per
// window; the runner reads it back (ReadImpactWindows) to build the test's
// dependency record (testrun/internal/impact). Unset, nothing is recorded,
// and the unit runs the ordinary programs.
const ImpactOut = "AGENT_REPL_TEST_IMPACT_OUT"

// ImpactProgramSuffix is appended to the daemon program's name for the build
// a recorded unit runs: the same package, built `-cover -covermode=atomic`,
// the one build whose counters can be cut into windows (runtime/coverage's
// ClearCounters and WriteCountersDir refuse any other mode).
const ImpactProgramSuffix = "-impact"

// ImpactProgram is the roster name of the cover-built build of the program
// name, which a unit run under ImpactOut runs in its place.
func ImpactProgram(name string) string { return name + ImpactProgramSuffix }

// ImpactWindow is one window's reach: the files one top-level test (or the
// daemon's boot, credited to every test of its process) executed. Executed
// names each file as the coverage tools do, "<import path>/<file>.go".
type ImpactWindow struct {
	// Test is the top-level test's name; "" is the processes' shared boot.
	Test     string   `json:"test"`
	Executed []string `json:"executed"`
	// CPUSeconds is what cutting and reducing the window cost the harness
	// (the covdata processes' own CPU), the recording's overhead.
	CPUSeconds float64 `json:"cpu_seconds"`
	// Incomplete says why the window cannot name everything the test
	// executed (a daemon that was killed writes no counters); "" when it
	// can. A test with an incomplete window is never recorded.
	Incomplete string `json:"incomplete,omitempty"`
}

// AppendImpactWindow appends w to the run's ImpactOut file as one
// append-mode write, so the tests of one process never interleave theirs. It
// does nothing outside a run that asked.
func AppendImpactWindow(getenv func(string) string, w ImpactWindow) error {
	path := getenv(ImpactOut)
	if path == "" {
		return nil
	}
	return appendJSONLine(path, "impact windows", w)
}

// ImpactReach is one test's windows, merged: the union of every file any of
// its windows executed, and what they cost.
type ImpactReach struct {
	Executed   []string
	CPUSeconds float64
	Windows    int
	// Incomplete is each distinct reason a window gave (ImpactWindow.Incomplete).
	Incomplete []string
}

// ReadImpactWindows answers an ImpactOut file's windows merged by test (the
// shared boot under ""). A file no harness wrote answers none; a line that is
// not a window is an error.
func ReadImpactWindows(path string) (map[string]*ImpactReach, error) {
	sets := map[string]map[string]bool{}
	out := map[string]*ImpactReach{}
	ok, err := readJSONLines(path, "impact windows", func(n int, line []byte) error {
		var w ImpactWindow
		if err := json.Unmarshal(line, &w); err != nil {
			return fmt.Errorf("testenv: %s line %d is not an impact window: %w", path, n, err)
		}
		r := out[w.Test]
		if r == nil {
			r = &ImpactReach{}
			out[w.Test] = r
			sets[w.Test] = map[string]bool{}
		}
		for _, e := range w.Executed {
			sets[w.Test][e] = true
		}
		r.CPUSeconds += w.CPUSeconds
		r.Windows++
		if w.Incomplete != "" && !slices.Contains(r.Incomplete, w.Incomplete) {
			r.Incomplete = append(r.Incomplete, w.Incomplete)
		}
		return nil
	})
	if err != nil || !ok {
		return nil, err
	}
	for test, set := range sets {
		for e := range set {
			out[test].Executed = append(out[test].Executed, e)
		}
		sort.Strings(out[test].Executed)
	}
	return out, nil
}

// ParseCovTextfmt reads `covdata textfmt` output and answers the files with
// any block that ran, sorted: "<import path>/<file>.go" as the tool names
// them.
func ParseCovTextfmt(text []byte) ([]string, error) {
	seen := map[string]bool{}
	for n, line := range strings.Split(string(text), "\n") {
		line = strings.TrimSpace(line)
		if line == "" || strings.HasPrefix(line, "mode:") {
			continue
		}
		colon := strings.LastIndex(line, ":")
		fields := strings.Fields(line)
		if colon < 0 || len(fields) != 3 {
			return nil, fmt.Errorf("testenv: coverage text line %d, %q, is not <file>:<range> <statements> <count>", n+1, line)
		}
		var count int
		if _, err := fmt.Sscanf(fields[2], "%d", &count); err != nil {
			return nil, fmt.Errorf("testenv: coverage text line %d, %q, has no count: %w", n+1, line, err)
		}
		if count > 0 {
			seen[line[:colon]] = true
		}
	}
	files := make([]string, 0, len(seen))
	for f := range seen {
		files = append(files, f)
	}
	sort.Strings(files)
	return files, nil
}

// CovReducer reduces coverage directories to the files they executed, by
// `go tool covdata textfmt`. Bin is the covdata binary.
type CovReducer struct {
	Bin string
}

var (
	covdataOnce sync.Once
	covdataBin  string
	covdataErr  error
)

// DefaultCovReducer answers the reducer of this machine's Go toolchain,
// found once: running the covdata binary directly spares each reduction the
// go command's own start.
func DefaultCovReducer() (CovReducer, error) {
	covdataOnce.Do(func() {
		out, err := exec.Command("go", "env", "GOTOOLDIR").Output()
		if err != nil {
			covdataErr = fmt.Errorf("testenv: ask the go command for its tool directory: %w", err)
			return
		}
		covdataBin = filepath.Join(strings.TrimSpace(string(out)), "covdata")
	})
	return CovReducer{Bin: covdataBin}, covdataErr
}

// Reduce answers the files executed by the counters in counterDirs, read
// against the meta-data in metaDir, and the CPU seconds the reduction cost.
// The directories are only read.
func (r CovReducer) Reduce(metaDir string, counterDirs ...string) (files []string, cpu float64, err error) {
	dirs := append([]string{metaDir}, counterDirs...)
	tmp, err := os.MkdirTemp("", "impact-reduce-")
	if err != nil {
		return nil, 0, fmt.Errorf("testenv: make a directory for the reduction: %w", err)
	}
	defer os.RemoveAll(tmp)
	out := filepath.Join(tmp, "textfmt")
	var stderr bytes.Buffer
	cmd := exec.Command(r.Bin, "textfmt", "-i="+strings.Join(dirs, ","), "-o="+out)
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, 0, fmt.Errorf("testenv: covdata textfmt -i=%s failed: %w: %s", strings.Join(dirs, ","), err, strings.TrimSpace(stderr.String()))
	}
	if ru, ok := cmd.ProcessState.SysUsage().(*syscall.Rusage); ok {
		cpu = float64(ru.Utime.Sec+ru.Stime.Sec) + float64(ru.Utime.Usec+ru.Stime.Usec)/1e6
	}
	text, err := os.ReadFile(out)
	if err != nil {
		return nil, 0, fmt.Errorf("testenv: read the reduction %s: %w", out, err)
	}
	files, err = ParseCovTextfmt(text)
	return files, cpu, err
}
