package cover

// The shim's e2e reach: what the shims the e2e suite spawned ran of the
// shim's own source, in a coverage run.
//
// Every shim the suite spawns under coverage writes v8 profiles (the daemon
// hands it NODE_V8_COVERAGE) into a ShimReachDir directory beside the Go
// counters, with the bundle it ran staged in the directory above. c8 remaps
// the bundle's coverage through its source map, so the report carries one
// entry per original file: ours under agent-shim/**/src, and every dependency
// esbuild inlined. Only ours is summed.
//
// IT IS REPORTED, NOT MERGED. c8's statements are derived from v8's ranges,
// not instrumented by istanbul, so they are not the unit report's statements
// and the baseline's statement merge (istanbulStatements) cannot take them;
// the shim's baseline line stays provisional for it (unmergedReach).

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"agentrepl/testrun/internal/command"
)

// ShimReachDir is the directory each spawned shim writes its v8 profiles to,
// under its coverage root (daemon/integration/harness.NodeCoverageDirName).
const ShimReachDir = "shim"

// Exec runs name with args in dir and returns its stdout.
type Exec func(dir, name string, args ...string) ([]byte, error)

// Command is the real Exec.
func Command(dir, name string, args ...string) ([]byte, error) {
	cmd := exec.Command(name, args...)
	cmd.Dir = dir
	return command.Output(cmd)
}

// ShimE2EReach renders every shim profile directory under coverRoot with c8
// (the shim's own pinned devDependency, at the path c8) into its own
// directory under reportRoot, and answers one summary line per directory. A
// run whose e2e suite did not run under coverage holds none, and answers
// none.
func ShimE2EReach(run Exec, c8, coverRoot, reportRoot string) ([]string, error) {
	dirs, err := shimProfileDirs(coverRoot)
	if err != nil {
		return nil, err
	}
	if len(dirs) == 0 {
		return nil, nil
	}
	if _, err := os.Stat(c8); err != nil {
		return nil, fmt.Errorf("cover: the shim's c8 is not installed at %s (npm ci in agent-shim/claude/shim): %w", c8, err)
	}
	var lines []string
	for i, dir := range dirs {
		out := filepath.Join(reportRoot, strconv.Itoa(i))
		// c8 runs FROM THE DIRECTORY ABOVE THE PROFILES: its default filter
		// keeps only scripts beneath its working directory, and the bundle
		// the profiles name is staged there.
		if _, err := run(filepath.Dir(dir), c8, "report", "--temp-directory="+dir, "--reports-dir="+out,
			"--reporter=json-summary", "--reporter=html"); err != nil {
			return nil, fmt.Errorf("cover: render the shim's e2e reach in %s (raw v8 profiles remain there): %w", dir, err)
		}
		line, err := shimSummary(filepath.Join(out, "coverage-summary.json"))
		if err != nil {
			return nil, err
		}
		lines = append(lines, fmt.Sprintf("%s (report %s)", line, out))
	}
	return lines, nil
}

// shimProfileDirs is every ShimReachDir under root holding a v8 profile.
func shimProfileDirs(root string) ([]string, error) {
	var out []string
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() || d.Name() != ShimReachDir {
			return nil
		}
		profiles, err := filepath.Glob(filepath.Join(path, "*.json"))
		if err != nil {
			return err
		}
		if len(profiles) > 0 {
			out = append(out, path)
		}
		return filepath.SkipDir
	})
	if err != nil {
		return nil, fmt.Errorf("cover: find the shim's v8 profiles under %s: %w", root, err)
	}
	sort.Strings(out)
	return out, nil
}

// shimSummary sums the shim's own sources' statements out of a c8
// json-summary report.
func shimSummary(path string) (string, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("cover: read the shim's c8 summary: %w", err)
	}
	var summary map[string]struct {
		Statements struct{ Covered, Total int } `json:"statements"`
	}
	if err := json.Unmarshal(data, &summary); err != nil {
		return "", fmt.Errorf("cover: decode the shim's c8 summary %s: %w", path, err)
	}
	covered, total, files := 0, 0, 0
	for file, entry := range summary {
		if file == "total" || !strings.Contains(file, "/agent-shim/") || strings.Contains(file, "/node_modules/") {
			continue
		}
		files++
		covered += entry.Statements.Covered
		total += entry.Statements.Total
	}
	if total == 0 {
		return "", errors.New("cover: no agent-shim source survived c8's remap: the bundle's source map is the only thing that attributes the bundle back to src/**/*.ts")
	}
	return fmt.Sprintf("%.1f%% (%d/%d statements over %d files)", float64(covered)*100/float64(total), covered, total, files), nil
}
