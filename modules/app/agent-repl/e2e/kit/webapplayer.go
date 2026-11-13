package kit

import (
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

// WlBootBudget mirrors BOOT_BUDGET_MS in `webapp/test/webapp-layer/drive.ts`:
// the child's own budget for its page's mount once vitest has collected it.
const WlBootBudget = 5 * time.Second

// WlWebappWritable answers whether the webapp package directory can hold the
// cache vite and vitest write there, by actually creating and removing one —
// a stat of the mode bits would answer for the wrong uid on a mount that
// disagrees with them.
//
// A DIRECTORY, not a file: what vite creates is a directory, and a filesystem
// can refuse that while a file write would have succeeded.
func WlWebappWritable(webappDir string) error {
	probe, err := os.MkdirTemp(webappDir, ".vite-cache-writeprobe-")
	if err != nil {
		return fmt.Errorf("%s is not writable: %w", webappDir, err)
	}
	if err := os.RemoveAll(probe); err != nil {
		return fmt.Errorf("could not remove the write probe %s: %w", probe, err)
	}
	return nil
}

// WlParseLiveGroupMembers reads ps's `pid pgid stat` lines and answers the
// pids in group PGID whose state is not a zombie (Z).
func WlParseLiveGroupMembers(out string, pgid int) ([]int, error) {
	var live []int
	for _, line := range strings.Split(out, "\n") {
		fields := strings.Fields(line)
		if len(fields) == 0 {
			continue
		}
		if len(fields) < 3 {
			return nil, fmt.Errorf("ps line %q: want pid, pgid and stat", line)
		}
		pid, pidErr := strconv.Atoi(fields[0])
		group, groupErr := strconv.Atoi(fields[1])
		if pidErr != nil || groupErr != nil {
			return nil, fmt.Errorf("ps line %q: pid or pgid is not a number", line)
		}
		if group == pgid && !strings.HasPrefix(fields[2], "Z") {
			live = append(live, pid)
		}
	}
	return live, nil
}

// WebappLayerCoverageEnv names the directory the webapp layer's vitest driver
// writes its istanbul reports into (webapp/test/webapp-layer/layer-server.mjs).
const WebappLayerCoverageEnv = "AGENT_REPL_WEBAPP_LAYER_COVERAGE"

// DriverCoverageEnv answers the assignment handing one layer driver a
// directory of its own under root's webapp-layer directory (testrun's
// baseline merges every one of them into the webapp's coverage), or nil when
// root is empty and the run collects no coverage.
func DriverCoverageEnv(root string) ([]string, error) {
	if root == "" {
		return nil, nil
	}
	parent := filepath.Join(root, "webapp-layer")
	if err := os.MkdirAll(parent, 0o755); err != nil {
		return nil, fmt.Errorf("make the webapp layer's coverage directory: %w", err)
	}
	dir, err := os.MkdirTemp(parent, "driver-")
	if err != nil {
		return nil, fmt.Errorf("make the webapp layer driver's coverage directory: %w", err)
	}
	return []string{WebappLayerCoverageEnv + "=" + dir}, nil
}
