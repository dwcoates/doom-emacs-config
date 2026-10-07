package deploy

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/outputtext"
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
var Targets = []string{"shim", "webapp", "daemon", "store", "sidecar", "lock"}

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
	Runner   Runner
	Clock    Clock
	Log      dlog.Logger
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
		out, code, err := b.Runner.Run(ctx, b.ModuleRoot, step.argv)
		archive.WriteString("==== " + step.name + ": " + strings.Join(step.argv, " ") + "\n" + out + "\n")
		if err == nil && code == 0 {
			b.Log.Debug(opBuild, "the step built", fields)
			continue
		}
		detail := outputtext.TailLines(out, 20)
		if err != nil {
			detail = err.Error()
		} else {
			fields["exit_code"] = code
		}
		b.archive(logPath, archive.String())
		b.Log.Error(opBuild, "a build step failed; nothing is deployed", merge(fields, dlog.Context{"detail": detail}))
		return &BuildFailed{Step: step.name, Detail: detail, Log: logPath}
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
	for _, target := range Targets {
		steps = append(steps, buildStep{
			name: target,
			argv: []string{"bash", filepath.Join(b.ModuleRoot, "bin", "build-frontend.sh"), "--out", staging, target},
		})
	}
	return steps
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
func (s Staged) DaemonBin() string { return filepath.Join(s.Dir, "daemon", "bin", "claude-repld") }

// CacheBin is a staged cache-bin artifact (shim-store, shim-claude-sidecar,
// shim-lock).
func (s Staged) CacheBin(name string) string { return filepath.Join(s.Dir, "cache-bin", name) }
