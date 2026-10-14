package cli

import "path/filepath"

// The artifact directories a failing integration or e2e test copies its whole
// logs into. Named here so every unit of one run shares them.
const (
	IntegrationArtifactsEnv = "AGENT_REPL_INTEGRATION_ARTIFACTS"
	E2EArtifactsEnv         = "AGENT_REPL_E2E_ARTIFACTS"
)

// RunArtifactsEnv answers the environment that points every unit's failure
// artifacts beside the run's own log: <log dir>/artifacts/{integration,e2e}.
// They then live and are pruned with the run log (the newest 100 runs), and
// no unit of the run can evict another's, which the harness's own default
// (one directory per process, the newest ten kept) did within a single run.
// A run with no log (testrun started directly) answers nothing, leaving the
// harness defaults in force; a variable the caller already set is kept.
func RunArtifactsEnv(runLog string, getenv func(string) string) map[string]string {
	if runLog == "" {
		return nil
	}
	root := filepath.Join(filepath.Dir(runLog), "artifacts")
	env := map[string]string{}
	for name, sub := range map[string]string{IntegrationArtifactsEnv: "integration", E2EArtifactsEnv: "e2e"} {
		if getenv(name) == "" {
			env[name] = filepath.Join(root, sub)
		}
	}
	return env
}
