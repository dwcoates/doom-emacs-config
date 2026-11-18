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
type ScriptRequirements struct {
	// ModuleRoot is the checkout holding bin/requirements.sh.
	ModuleRoot string
	Runner     Runner
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

// run runs `requirements.sh VERB ARG` in ModuleRoot.
func (d ScriptRequirements) run(ctx context.Context, verb, arg string) error {
	return mustTool(ctx, d.Runner, d.ModuleRoot, filepath.Join(d.ModuleRoot, "bin", "requirements.sh"), verb, arg)
}
