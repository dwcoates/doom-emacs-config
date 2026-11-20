// Package profile is a profiling run's (testrun.v1.TestRunRequest.profile)
// instrumentation of its units: every unit gets its own directory under the
// run's profiles directory, beside the run's log, named by its unit id
// (testenv.ProfileName); a Go test binary writes its CPU profile there
// (`-test.cpuprofile`), node and every node process it starts write theirs
// (`--cpu-prof`), and every unit is told the directory (testenv.ProfileDir),
// so the harnesses point the production processes their tests start at it
// too.
//
// A RUN THAT DOES NOT PROFILE IS UNTOUCHED: Apply is called only for a run
// that asked, and nothing else in the runner reads a unit's Profiler.
package profile

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/testenv"
)

// GoProfile is the file a Go test binary writes its CPU profile to, in its
// unit's directory.
const GoProfile = "test.cpu.pprof"

// Dir is the profiles directory of the run whose log is RUNLOG: <log
// dir>/profiles, beside the log as the run's failure artifacts are
// (cli.RunArtifactsEnv), so they live and are pruned with the run's log. A
// run with no log (testrun started directly) has nowhere a reader would look,
// so it cannot profile.
func Dir(runLog string) (string, error) {
	if runLog == "" {
		return "", fmt.Errorf("profile: a profiling run puts its profiles beside its log, and this run has none (testrun was started directly)")
	}
	return filepath.Join(filepath.Dir(runLog), "profiles"), nil
}

// Apply answers SPECS each made to write its profiles into its own directory
// under DIR, which it creates: the directory named in the unit's environment
// (testenv.ProfileDir), and the unit's own process profiled as its Profiler
// says. GETENV reads the runner's environment, whose NODE_OPTIONS a node
// unit's profiling flags are added to rather than replace.
func Apply(specs []run.Spec, dir string, getenv func(string) string) ([]run.Spec, error) {
	out := make([]run.Spec, len(specs))
	for i, s := range specs {
		unitDir := filepath.Join(dir, testenv.ProfileName(s.ID))
		if strings.ContainsRune(unitDir, '"') {
			return nil, fmt.Errorf("profile: the profiles directory %q holds a double quote, which NODE_OPTIONS cannot carry", unitDir)
		}
		if err := os.MkdirAll(unitDir, 0o755); err != nil {
			return nil, fmt.Errorf("profile: create the profiles directory of unit %s: %w", s.ID, err)
		}
		s.Argv = append([]string(nil), s.Argv...)
		s.Env = append(append([]string(nil), s.Env...), testenv.ProfileDir+"="+unitDir)
		switch s.Profiler {
		case run.NoProfiler:
		case run.GoTestProfiler:
			s.Argv = append(s.Argv, "-test.cpuprofile="+filepath.Join(unitDir, GoProfile))
		case run.NodeProfiler:
			s.Env = append(s.Env, "NODE_OPTIONS="+nodeOptions(s.Env, getenv, unitDir))
		default:
			panic(fmt.Sprintf("profile: unit %s has a Profiler the runner does not define: %d", s.ID, s.Profiler))
		}
		out[i] = s
	}
	return out, nil
}

// nodeOptions is the unit's NODE_OPTIONS with node's CPU profiler turned on
// into dir: the unit's own binding when it has one (the last wins, as in the
// process it starts), else the runner's.
func nodeOptions(env []string, getenv func(string) string, dir string) string {
	base := getenv("NODE_OPTIONS")
	for _, kv := range env {
		if v, ok := strings.CutPrefix(kv, "NODE_OPTIONS="); ok {
			base = v
		}
	}
	// Quoted, as node's NODE_OPTIONS parser reads a quoted argument, so a
	// directory with a space stays one argument.
	return strings.TrimSpace(base + ` --cpu-prof --cpu-prof-dir="` + dir + `"`)
}
