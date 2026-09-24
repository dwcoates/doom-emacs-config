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
// component up to date and changes nothing. A harness daemon runs no store or
// sidecar, so the harness states their build reports as the staged binaries,
// from the daemon's own pid (which is alive), and the deploy never reaches
// launchd. A world that runs the REAL services names their binaries
// (Opts.ServiceBinaries, DeploySources.Store/Sidecar): the staged cache-bin
// then carries copies of them, and the processes' own reports are the ones
// the deploy reads.
//
// A test that needs a deploy to CHANGE something — a handover, a webview
// reload — or to FAIL stages it with Daemon.StageDeployBuild (or
// DeployBuilder.Stage) first.

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

// DeploySources are what a fake deploy build stages as the CURRENT build:
// the artifacts the stack under test really runs.
type DeploySources struct {
	// ShimMain is the shim bundle every spawn runs (--shim-main).
	ShimMain string
	// WebappDist is the dist the daemon serves (--webapp-dist).
	WebappDist string
	// Store and Sidecar, when set, are the shim-store and shim-claude-sidecar
	// binaries the world really runs. The staged cache-bin carries copies of
	// them, so a deploy judges those processes — which report their own builds
	// into the lock dir — up to date, and nothing is restarted. Unset, the
	// staged cache-bin carries placeholder bytes whose reports the harness
	// states itself (Daemon.StageDeployBuild). Both or neither.
	Store, Sidecar string
}

// realServices reports whether the staged services are real binaries.
func (s DeploySources) realServices() bool { return s.Store != "" }

// DeployBuilder is the deploy's fake build: a Recorder, so a test can count
// and read the deploys' builds, that stages one DeployBuild.
type DeployBuilder struct {
	*Recorder
	sources DeploySources
}

// NewFakeDeployBuilder writes the fake build into dir, bound to the artifacts
// src names and to the harness's own daemon binary. Until Stage is called it
// fails, as a harness that never builds does; StartDaemon stages
// DeployCurrent at once. A world that starts its daemon some other way (the
// Emacs layer's launcher) points AGENT_REPL_DEPLOY_BUILDER at Path itself.
func NewFakeDeployBuilder(t *testing.T, dir string, src DeploySources) *DeployBuilder {
	t.Helper()
	if (src.Store == "") != (src.Sidecar == "") {
		t.Fatalf("harness: the deploy's real services are both or neither, got store %q and sidecar %q", src.Store, src.Sidecar)
	}
	r := NewRecorderExecutable(t, dir, "deploy-build")
	body, err := os.ReadFile(r.Path)
	if err != nil {
		t.Fatalf("harness: read %s: %v", r.Path, err)
	}
	service := func(name, real string) string {
		if real == "" {
			return `printf 'harness %s\n' ` + shellQuote(name) + ` > "$out/cache-bin/` + name + `"`
		}
		return `cp ` + shellQuote(real) + ` "$out/cache-bin/` + name + `"`
	}
	// The staging arm runs AFTER the invocation is recorded and BEFORE the
	// scripted exit, so a staged build still counts as one invocation.
	staging := `if [ -f "$control.staged" ]; then
  set -e
  out="$2"
  mode="$(cat "$control.staged")"
  mkdir -p "$out/agent-shim/claude/shim/dist" "$out/webapp/dist" "$out/daemon/bin" "$out/cache-bin"
  cp ` + shellQuote(src.ShimMain) + ` "$out/agent-shim/claude/shim/dist/main.js"
  cp -R ` + shellQuote(src.WebappDist) + `/. "$out/webapp/dist/"
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
  ` + service(buildreport.ServiceStore, src.Store) + `
  ` + service(buildreport.ServiceSidecar, src.Sidecar) + `
  printf 'harness shim-lock\n' > "$out/cache-bin/shim-lock"
  for n in shim-store shim-claude-sidecar shim-lock; do
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
	return &DeployBuilder{Recorder: r, sources: src}
}

// Stage makes the next deploys' build the named one. It states no service
// build report; Daemon.StageDeployBuild does, for a daemon whose world runs
// no real services.
func (b *DeployBuilder) Stage(build DeployBuild) {
	b.t.Helper()
	if build == DeployFails {
		if err := os.Remove(b.Control + ".staged"); err != nil && !os.IsNotExist(err) {
			b.t.Fatalf("harness: unstage the deploy build: %v", err)
		}
		b.SetExitCode(1)
		return
	}
	if err := os.WriteFile(b.Control+".staged", []byte(build), 0o644); err != nil {
		b.t.Fatalf("harness: stage the deploy build: %v", err)
	}
	b.SetExitCode(0)
}

// StageDeployBuild makes the next deploys' build the named one. When this
// daemon's world runs no real store or sidecar, it states their builds as the
// staged placeholders, from the daemon's own pid; real services state their
// own.
func (d *Daemon) StageDeployBuild(build DeployBuild) {
	d.t.Helper()
	d.Deploy.Stage(build)
	if build == DeployFails || d.Deploy.sources.realServices() {
		return
	}
	for _, service := range []string{buildreport.ServiceStore, buildreport.ServiceSidecar} {
		sum := sha256.Sum256([]byte(fakeServiceBinary(service)))
		if err := buildreport.Write(d.LockDir, service, buildreport.Report{PID: d.PID(), Build: hex.EncodeToString(sum[:])}); err != nil {
			d.t.Fatalf("harness: state the %s build report: %v", service, err)
		}
	}
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
