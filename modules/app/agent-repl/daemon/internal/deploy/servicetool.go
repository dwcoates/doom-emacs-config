package deploy

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
)

// Runner runs one command and reports its combined output with its exit code;
// scriptrunner.Runner is the production one.
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (output string, exitCode int, err error)
}

// ErrServiceNotLoaded is a Stop's answer that the service manager does not
// hold the label: the service had already left, which is the state the stop
// was asked to reach.
var ErrServiceNotLoaded = errors.New("deploy: the service is not loaded by the service manager")

// runTool runs a service manager's command, binary with args, in dir, and
// answers its output and exit code. A command that could not run at all is an
// error naming it.
func runTool(ctx context.Context, r Runner, dir, binary string, args ...string) (string, int, error) {
	argv := append([]string{binary}, args...)
	out, code, err := r.Run(ctx, dir, argv)
	if err != nil {
		return "", 0, fmt.Errorf("deploy: run %s: %w", strings.Join(argv, " "), err)
	}
	return out, code, nil
}

// mustTool is runTool for a verb whose non-zero exit is a failure, which it
// answers with the command's own words.
func mustTool(ctx context.Context, r Runner, dir, binary string, args ...string) error {
	out, code, err := runTool(ctx, r, dir, binary, args...)
	if err != nil {
		return err
	}
	if code != 0 {
		return toolExited(binary, args, code, out)
	}
	return nil
}

// toolExited is the error for a service manager's command that exited code,
// naming the tool by its base name.
func toolExited(binary string, args []string, code int, out string) error {
	return fmt.Errorf("deploy: %s %s exited %d: %s", filepath.Base(binary), strings.Join(args, " "), code, strings.TrimSpace(out))
}

// ScriptRequirements runs bin/requirements.sh in ModuleRoot for what a deploy
// brings up from its requirements entry: the one renderer and installer
// install uses, so a deploy and an install can never write different
// definitions. Restarter.Define is its Define, and the holder ensure's
// install (holderensure.Ensurer.Install) is its EnsureHolder.
//
// THE ROOTS IT RENDERS ARE THE ROOTS THIS DAEMON ROUTES SESSIONS TO. The
// script names every Claude config root a session runs under
// (`{claude-config-roots}`, the sidecar's --config-roots) from
// MULTI_REPO_CONFIG_DIR, and the daemon's own environment never carries it:
// Emacs hands the work account's root to the daemon as a flag. Inherited, the
// variable was empty, so every deploy rendered the sidecar watching
// ~/.claude alone, and from 2026-11-21 no transcript of a session on the work
// account was read: its shells' claims were never attributed, their spools
// never read, their terminals never written, and their sessions held busy
// for hours (2026-11-23). So every run states the variable from
// WorkConfigRoot, and an inherited value can never decide the roots.
type ScriptRequirements struct {
	// ModuleRoot is the checkout holding bin/requirements.sh.
	ModuleRoot string
	// WorkConfigRoot is the work account's config root this daemon routes
	// the workspaces under the multi-repo root to (its
	// -multi-repo-config-dir, account.Roots.MultiRepo), handed to every run
	// as MULTI_REPO_CONFIG_DIR; empty when the daemon routes none there.
	WorkConfigRoot string
	// Script runs in place of ModuleRoot's bin/requirements.sh when set: a
	// harness's stand-in, so no test writes or loads a real definition.
	Script string
	Runner Runner
}

// Define writes label's definition (define-service); a non-zero exit is a
// failure in the script's own words.
func (d ScriptRequirements) Define(ctx context.Context, label string) error {
	return d.run(ctx, "define-service", label)
}

// EnsureHolder installs the socket holder id (ensure-holder), leaving a
// loaded, current one alone; a non-zero exit is a failure in the script's
// own words.
func (d ScriptRequirements) EnsureHolder(ctx context.Context, id string) error {
	return d.run(ctx, "ensure-holder", id)
}

// envBinary states a run's environment ahead of the script it runs.
const envBinary = "/usr/bin/env"

// workConfigRootEnv is the script's name for the work account's config root
// (bin/requirements.sh claude_config_roots).
const workConfigRootEnv = "MULTI_REPO_CONFIG_DIR"

// run runs `requirements.sh VERB ARG` in ModuleRoot, with
// MULTI_REPO_CONFIG_DIR stated as WorkConfigRoot.
func (d ScriptRequirements) run(ctx context.Context, verb, arg string) error {
	script := d.Script
	if script == "" {
		script = filepath.Join(d.ModuleRoot, "bin", "requirements.sh")
	}
	return mustTool(ctx, d.Runner, d.ModuleRoot, envBinary, workConfigRootEnv+"="+d.WorkConfigRoot, script, verb, arg)
}
