package harness

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agentrepl/logging/buildreport"

	"claude-repld/internal/buildid"
)

// THE DEPLOY'S FAKE BUILD.
//
// The daemon's deploy runs AGENT_REPL_DEPLOY_BUILDER as `<exe> --out
// <staging>` in place of the real build. A harness never builds: the fake
// records the invocation and writes the staging layout build-frontend.sh's
// --out mode writes, with every component equal to what is already running
// (DeployCurrent, the default), so a deploy a landing runs decides every
// component up to date and changes nothing. The harness states the store's
// and the sidecar's build reports as the staged binaries, from the daemon's
// own pid (which is alive), so the deploy never reaches launchd.
//
// A test that needs a deploy to CHANGE something — a handover, a webview
// reload — or to FAIL stages it with Daemon.StageDeployBuild first.

// DeployBuild is what a staged deploy build changes.
type DeployBuild string

// The staged builds.
const (
	// DeployCurrent stages exactly what is running: every component is up to
	// date. It is every daemon's default.
	DeployCurrent DeployBuild = "current"
	// DeployFails fails the build, so the deploy installs and restarts
	// nothing, and says so loudly.
	DeployFails DeployBuild = "fails"
	// DeployStaleDaemon stages a daemon binary that is not the running one,
	// so the deploy hands the daemon over.
	DeployStaleDaemon DeployBuild = "stale-daemon"
	// DeployStaleWebapp stages a webapp whose entry bundle is not the one the
	// webviews report, so the deploy pushes them the reload.
	DeployStaleWebapp DeployBuild = "stale-webapp"
)

// FakeWebappEntry is the entry bundle hash the harness's served webapp dist
// names (and so the build every served page runs).
const FakeWebappEntry = "harness"

// stagedWebappEntry is the entry a DeployStaleWebapp build stages.
const stagedWebappEntry = "harnessfresh"

// fakeServiceBinary is the staged content of a cache-bin service binary.
func fakeServiceBinary(name string) string { return "harness " + name + "\n" }

// newFakeDeployBuilder writes the fake build, bound to the bundle and the dist
// this daemon runs.
func newFakeDeployBuilder(t *testing.T, dir, shimMain, webappDist string) *Recorder {
	t.Helper()
	r := NewRecorderExecutable(t, dir, "deploy-build")
	body, err := os.ReadFile(r.Path)
	if err != nil {
		t.Fatalf("harness: read %s: %v", r.Path, err)
	}
	// The staging arm runs AFTER the invocation is recorded and BEFORE the
	// scripted exit, so a staged build still counts as one invocation.
	staging := `if [ -f "$control.staged" ]; then
  set -e
  out="$2"
  mode="$(cat "$control.staged")"
  mkdir -p "$out/agent-shim/claude/shim/dist" "$out/webapp/dist" "$out/daemon/bin" "$out/cache-bin"
  cp ` + shellQuote(shimMain) + ` "$out/agent-shim/claude/shim/dist/main.js"
  cp -R ` + shellQuote(webappDist) + `/. "$out/webapp/dist/"
  if [ "$mode" = "` + string(DeployStaleDaemon) + `" ]; then
    printf 'a harness daemon build\n' > "$out/daemon/bin/claude-repld"
  else
    cp ` + shellQuote(daemonBinary) + ` "$out/daemon/bin/claude-repld"
  fi
  if [ "$mode" = "` + string(DeployStaleWebapp) + `" ]; then
    printf '<script src="/assets/index-` + stagedWebappEntry + `.js"></script>' > "$out/webapp/dist/index.html"
    printf '// fresh\n' > "$out/webapp/dist/assets/index-` + stagedWebappEntry + `.js"
  fi
  for f in built-sha source-tree; do
    printf 'harness\n' > "$out/agent-shim/claude/shim/dist/.$f"
    printf 'harness\n' > "$out/daemon/bin/.$f"
  done
  for n in shim-store shim-claude-sidecar shim-lock; do
    printf 'harness %s\n' "$n" > "$out/cache-bin/$n"
    for f in built-sha source-tree; do printf 'harness\n' > "$out/cache-bin/.$n.$f"; done
  done
  exit 0
fi
`
	script := strings.Replace(string(body), `if [ -f "$control.stdout" ]`, staging+`if [ -f "$control.stdout" ]`, 1)
	if err := os.WriteFile(r.Path, []byte(script), 0o755); err != nil {
		t.Fatalf("harness: write %s: %v", r.Path, err)
	}
	r.SetStdout(FakeDeployBuildRefusal + "\n")
	r.SetExitCode(1)
	return r
}

// StageDeployBuild makes the next deploys' build the named one, and states
// the store's and the sidecar's builds as the staged ones.
func (d *Daemon) StageDeployBuild(build DeployBuild) {
	d.t.Helper()
	if build == DeployFails {
		if err := os.Remove(d.Deploy.Control + ".staged"); err != nil && !os.IsNotExist(err) {
			d.t.Fatalf("harness: unstage the deploy build: %v", err)
		}
		d.Deploy.SetExitCode(1)
		return
	}
	for _, service := range []string{buildreport.ServiceStore, buildreport.ServiceSidecar} {
		sum := sha256.Sum256([]byte(fakeServiceBinary(service)))
		if err := buildreport.Write(d.LockDir, service, buildreport.Report{PID: d.PID(), Build: hex.EncodeToString(sum[:])}); err != nil {
			d.t.Fatalf("harness: state the %s build report: %v", service, err)
		}
	}
	if err := os.WriteFile(d.Deploy.Control+".staged", []byte(build), 0o644); err != nil {
		d.t.Fatalf("harness: stage the deploy build: %v", err)
	}
	d.Deploy.SetExitCode(0)
}

// pinnedConfigEl is the elisp loader the pinned checkout carries: the deploy
// hashes the checkout's elisp by it. It names one module with no file, which
// contributes nothing, exactly as a module absent from a checkout does.
const pinnedConfigEl = "(agent-repl--load-module \"core\")\n"

// PinnedElispBuild is the pinned checkout's elisp build, which every Emacs
// stream the harness opens reports: an Emacs running the checkout's elisp is
// one a deploy leaves alone.
var PinnedElispBuild string

// writePinnedConfig lays the loader into the pinned checkout and states its
// elisp build.
func writePinnedConfig(dir string) error {
	if err := os.WriteFile(filepath.Join(dir, "config.el"), []byte(pinnedConfigEl), 0o644); err != nil {
		return err
	}
	build, err := buildid.Elisp(dir)
	if err != nil {
		return err
	}
	PinnedElispBuild = build
	return nil
}
