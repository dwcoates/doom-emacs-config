package deploy

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"agent-repld/internal/dlog"
	"agent-repld/internal/outputtext"
	"agent-repld/internal/persistentwifi"
	"agent-repld/internal/restarttiming"
)

const opBuild = "daemon.deploy.build"

// Builder builds every component into a staging directory. A failure is a
// *BuildFailed naming the step: nothing is installed and nothing restarted.
type Builder interface {
	Build(ctx context.Context, staging string) error
}

// BuildFailed is a build step that failed. NOTHING WAS DEPLOYED.
type BuildFailed struct {
	// Step names the step: `proto`, or a build-frontend target.
	Step string
	// Detail is the step's own words: the tail of its output.
	Detail string
	// Log is where the whole build output is archived.
	Log string
}

func (e *BuildFailed) Error() string {
	return fmt.Sprintf("deploy: the %s build failed (output in %s): %s", e.Step, e.Log, e.Detail)
}

// Targets are build-frontend.sh's targets a deploy stages, in build order. lock
// rides with the shim — the shim spawns shim-lock for every kernel claim — so
// it is built and installed, but it is not a running process with a build of
// its own to compare.
var Targets = []string{"shim", "webapp", "daemon", "store", "sidecar", "zettel", "tests", "lock"}

// BuildObserver is told each build step as it begins and ends: the hot
// reload's card draws every service's build from it (hotreload.Tracker).
type BuildObserver interface {
	BuildStepBegan(step string)
	BuildStepEnded(step string, err error)
}

// LauncherTarget is build-frontend.sh's target for the launcher, staged after
// Targets on a platform that has one (ScriptBuilder.Launcher).
const LauncherTarget = "launcher"

// ScriptBuilder is the production Builder.
//
// THE ONE BUILD STEP IS bin/build-frontend.sh. It is kept — and not folded
// into Go — because Emacs's cold start has to build the daemon before any
// daemon exists, so a daemon-only builder could not be the only one; two
// builders would drift. The daemon therefore drives that same script, in its
// staging mode (`--out DIR`), one target at a time so a failure names its
// step, after regenerating the protobufs the way every build must.
type ScriptBuilder struct {
	// ModuleRoot is the agent-repl module root the daemon was deployed from.
	ModuleRoot string
	// LogDir is where each build's whole output is archived.
	LogDir string
	// Override, when set, is ONE executable run as `<Override> --out
	// <staging>` in place of every step: a harness's stand-in for the whole
	// build, so no test ever regenerates protobufs or runs a real build.
	Override string
	// WifiInstalled is the installed Wi-Fi helper app (Live.WifiApp), on the
	// platform that has one; empty on one that has none. The build stages the
	// helper (wifi-helper/build.sh) and hands it this app to reuse: an app
	// built from the same sources is copied rather than rebuilt, so codesign
	// (and the signing key it unlocks) runs only when the sources changed.
	WifiInstalled string
	Runner        Runner
	Clock         Clock
	Log           dlog.Logger
	// Restarts records how long each build step took, by its name.
	Restarts restarttiming.Recorder
	// Observer is told every step as it begins and ends. REQUIRED.
	Observer BuildObserver
	// Launcher stages the launcher too: macOS, where Zettel.app posts the
	// desktop banners.
	Launcher bool
}

// buildStep is one command of a build.
type buildStep struct {
	name string
	argv []string
}

// Build implements Builder.
func (b *ScriptBuilder) Build(ctx context.Context, staging string) error {
	if err := os.MkdirAll(b.LogDir, 0o755); err != nil {
		b.Log.Error(opBuild, "could not create the build-log directory; nothing was built", dlog.Context{"dir": b.LogDir, "cause": err.Error()})
		return &BuildFailed{Step: "setup", Detail: err.Error(), Log: b.LogDir}
	}
	logPath := filepath.Join(b.LogDir, fmt.Sprintf("build-%d.log", b.Clock.Now().UnixNano()))
	steps := b.steps(staging)
	var archive strings.Builder
	for _, step := range steps {
		fields := dlog.Context{"step": step.name, "argv": step.argv, "log": logPath}
		b.Log.Info(opBuild, "building", fields)
		began := b.Clock.Now()
		b.Observer.BuildStepBegan(step.name)
		out, code, err := b.Runner.Run(ctx, b.ModuleRoot, step.argv)
		b.Restarts.ObserveRestart(restarttiming.OfBuildStep(step.name, restarttiming.OutcomeOf(stepErr(code, err)), b.Clock.Now().Sub(began)))
		archive.WriteString("==== " + step.name + ": " + strings.Join(step.argv, " ") + "\n" + out + "\n")
		if stepErr(code, err) == nil {
			b.Log.Debug(opBuild, "the step built", fields)
			b.Observer.BuildStepEnded(step.name, nil)
			continue
		}
		detail := outputtext.TailLines(out, 20)
		if err != nil {
			detail = err.Error()
		} else {
			fields["exit_code"] = code
		}
		b.archive(logPath, archive.String())
		b.Log.Error(opBuild, "a build step failed; nothing is deployed", dlog.Merge(fields, dlog.Context{"detail": detail}))
		failed := &BuildFailed{Step: step.name, Detail: detail, Log: logPath}
		b.Observer.BuildStepEnded(step.name, failed)
		return failed
	}
	b.archive(logPath, archive.String())
	b.Log.Info(opBuild, "every component built into staging", dlog.Context{"staging": staging, "log": logPath})
	return nil
}

// steps are the build's commands, in order: the protobufs, then every
// build-frontend target into staging — or the Override alone.
func (b *ScriptBuilder) steps(staging string) []buildStep {
	if b.Override != "" {
		return []buildStep{{name: "build", argv: []string{b.Override, "--out", staging}}}
	}
	steps := []buildStep{{name: "proto", argv: []string{"make", "-C", filepath.Join(b.ModuleRoot, "proto"), "all"}}}
	targets := Targets
	if b.Launcher {
		targets = append(append([]string(nil), Targets...), LauncherTarget)
	}
	for _, target := range targets {
		steps = append(steps, buildStep{
			name: target,
			argv: []string{"bash", filepath.Join(b.ModuleRoot, "bin", "build-frontend.sh"), "--out", staging, target},
		})
	}
	if b.WifiInstalled != "" {
		// THE HELPER'S OWN BUILD, not a build-frontend target: build.sh is the
		// one build of the app, which the installer runs too
		// (bin/requirements.sh, install_app_build).
		steps = append(steps, buildStep{
			name: "wifi",
			argv: []string{"bash", filepath.Join(b.ModuleRoot, "wifi-helper", "build.sh"), Staged{Dir: staging}.WifiApp(), "--reuse", b.WifiInstalled},
		})
	}
	return steps
}

// stepErr answers a step's failure: the error that kept it from running, or
// its non-zero exit.
func stepErr(code int, err error) error {
	if err != nil {
		return err
	}
	if code != 0 {
		return fmt.Errorf("exit status %d", code)
	}
	return nil
}

// archive writes a build's output. A failure to archive is loud but does not
// fail the build it describes.
func (b *ScriptBuilder) archive(path, output string) {
	if err := os.WriteFile(path, []byte(output), 0o644); err != nil {
		b.Log.Error(opBuild, "could not archive the build output", dlog.Context{"log": path, "cause": err.Error()})
	}
}

// Staged names the artifacts inside one staging directory, in the layout
// build-frontend.sh's staging mode writes.
type Staged struct{ Dir string }

// ShimMain is the staged shim bundle.
func (s Staged) ShimMain() string {
	return filepath.Join(s.Dir, "agent-shim", "claude", "shim", "dist", "main.js")
}

// WebappDist is the staged webapp dist directory.
func (s Staged) WebappDist() string { return filepath.Join(s.Dir, "webapp", "dist") }

// DaemonBin is the staged daemon binary.
func (s Staged) DaemonBin() string { return filepath.Join(s.Dir, "daemon", "bin", "agent-repld") }

// WifiApp is the staged Wi-Fi helper app.
func (s Staged) WifiApp() string {
	return filepath.Join(s.Dir, "wifi-helper", persistentwifi.WifiAppName)
}

// wifiSourceHash names a Wi-Fi helper app by the hash of the sources it was
// built from, which wifi-helper/build.sh writes inside it. The sources, not
// the bytes, are its identity: a rebuild re-signs the app, and macOS may
// forget the Location Services permission it gave the last signature, so an
// app built from the same sources is never replaced.
func wifiSourceHash(app string) (string, error) {
	raw, err := os.ReadFile(filepath.Join(app, "Contents", "Resources", "source-sha256"))
	if err != nil {
		return "", fmt.Errorf("read the Wi-Fi helper's source hash: %w", err)
	}
	hash := strings.TrimSpace(string(raw))
	if hash == "" {
		return "", fmt.Errorf("the Wi-Fi helper %s names no source hash", app)
	}
	return hash, nil
}

// CacheBin is a staged cache-bin artifact (shim-store, shim-claude-sidecar,
// shim-lock).
func (s Staged) CacheBin(name string) string { return filepath.Join(s.Dir, "cache-bin", name) }
