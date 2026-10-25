package harness

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/deployconfig"
)

// EVERY FIXTURE REPOSITORY STATES ITS DEPLOY. A repository with no
// `.agent-repl/deploy.yaml` refuses every deploy (no_deploy_config), so a
// fixture repository carries one exactly as a real repository's author would
// write one: HooksOnlyDeploy by default, AgentReplDeploy once a test makes the
// repository agent-repl's own (DeploysAsAgentRepl).

// HooksOnlyDeploy is a deploy that runs its request's hooks and nothing else:
// no local merge and no deploy hook.
const HooksOnlyDeploy = "pre_deploy: {}\n"

// DeployLandingScript is the path, relative to a repository's main checkout
// root, of the deploy hook AgentReplDeploy names.
const DeployLandingScript = "bin/deploy-landing.sh"

// AgentReplDeploy is agent-repl's own deploy, as its repository commits it:
// the local merge, tested and version-tagged, then a deploy hook that asks
// the daemon to roll out what landed on main.
const AgentReplDeploy = `pre_deploy:
  local_merge:
    tests:
      agent_repl_suites:
        script_path: bin/test-all.sh
    version_tag: {}
deploy:
  hook:
    script:
      path: ` + DeployLandingScript + `
      timeout_seconds: 60
  runs_for:
    main: {}
`

// WriteDeployConfig replaces the repository's deploy config with BODY.
func (r *Repo) WriteDeployConfig(body string) {
	r.t.Helper()
	writeFile(r.t, deployconfig.Path(r.Dir), body)
}

// RemoveDeployConfig leaves the repository stating no deploy.
func (r *Repo) RemoveDeployConfig() {
	r.t.Helper()
	if err := os.Remove(deployconfig.Path(r.Dir)); err != nil {
		r.t.Fatalf("harness: remove %s: %v", deployconfig.Path(r.Dir), err)
	}
}

// DeploysAsAgentRepl makes the repository deploy as agent-repl's own does
// (AgentReplDeploy), with agent-repl's REAL deploy hook, bin/deploy-landing.sh,
// copied in verbatim: the front door it execs is linked to the daemon binary
// under test, and the hook inherits the daemon's AGENT_REPL_STATE_DIR and the
// deploy's environment, so it calls back the daemon exactly as in production.
func (r *Repo) DeploysAsAgentRepl() {
	r.t.Helper()
	r.WriteDeployConfig(AgentReplDeploy)
	source := filepath.Join(RepoRoot(r.t), DeployLandingScript)
	script, err := os.ReadFile(source)
	if err != nil {
		r.t.Fatalf("harness: read %s: %v", source, err)
	}
	writeExecutable(r.t, filepath.Join(r.Dir, DeployLandingScript), string(script))
	door := filepath.Join(r.Dir, frontDoorOf(r.t, string(script)))
	if err := os.MkdirAll(filepath.Dir(door), 0o755); err != nil {
		r.t.Fatalf("harness: mkdir %s: %v", filepath.Dir(door), err)
	}
	if err := os.Symlink(DaemonBinary(r.t), door); err != nil {
		r.t.Fatalf("harness: link the front door %s: %v", door, err)
	}
}

// frontDoorOf answers the path, relative to the repository root, SCRIPT's
// `FRONT_DOOR=` line names: read off the script, so the harness never restates
// it.
func frontDoorOf(t testing.TB, script string) string {
	t.Helper()
	for _, line := range strings.Split(script, "\n") {
		if value, ok := strings.CutPrefix(line, "FRONT_DOOR="); ok {
			return strings.Trim(value, `"`)
		}
	}
	t.Fatalf("harness: %s names no FRONT_DOOR", DeployLandingScript)
	return ""
}
