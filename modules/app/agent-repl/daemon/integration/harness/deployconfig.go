package harness

import (
	"os"
	"path/filepath"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/shellword"
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
// (AgentReplDeploy), with a deploy hook that calls back into the daemon under
// test exactly as agent-repl's bin/deploy-landing.sh calls the deployed
// build's front door: the hook inherits the daemon's AGENT_REPL_STATE_DIR, so
// the CLI dials that daemon.
func (r *Repo) DeploysAsAgentRepl() {
	r.t.Helper()
	r.WriteDeployConfig(AgentReplDeploy)
	writeExecutable(r.t, filepath.Join(r.Dir, DeployLandingScript), deployLandingScript(DaemonBinary(r.t)))
}

// deployLandingScript is the fixture's deploy hook body, calling the daemon
// binary at BINARY.
func deployLandingScript(binary string) string {
	cli := shellword.Quote(binary)
	return `#!/bin/sh
set -eu
if [ -n "${AGENT_REPL_DEPLOY_LEDGER:-}" ]; then
  exec ` + cli + ` runtime roll-out-landing -deploy "$AGENT_REPL_DEPLOY_LEDGER"
fi
exec ` + cli + ` runtime roll-out-landing
`
}
