//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// THE EDITOR MUST NOT DISTURB THE OWNER. A realtest launches Emacs and the
// owner keeps typing wherever they were: focus never moves, and no picture is
// ever taken, so Emacs is never brought frontmost either.
//
// Two launch methods are implemented and BOTH are measured, because which one
// actually leaves focus alone on this machine is a question about macOS, not a
// question about this code, and the answer belongs in the run's report rather
// than in a comment:
//
//	LaunchOpenBackground   `open -g -a Emacs`, where -g ("--background") asks
//	                       LaunchServices not to bring the application
//	                       forward. It is the documented way to do this and it
//	                       is tried FIRST because it asks the window server
//	                       for the behavior instead of correcting for it.
//
//	LaunchDirectRestoring  the bundle's own executable, spawned directly, with
//	                       the frontmost application captured before and
//	                       reactivated the moment the frame maps. This one
//	                       CORRECTS a focus steal rather than preventing it,
//	                       so the owner may see a flicker; it is the fallback.
//
// Focus is measured, not assumed: FrontmostApp reads the frontmost process from
// System Events before and after, and the test reports which method left it
// alone. A method that moved focus is reported as having moved it even if
// everything else about the run was clean.

// FrontmostApp is the name of the application currently frontmost.
//
// Read through System Events, which is the only interface that answers this
// without activating anything. An empty answer with no error means System
// Events replied with nothing, which happens when no application is frontmost
// (a locked screen, a login window) — reported as such rather than as a name.
func FrontmostApp(ctx context.Context) (string, error) {
	const script = `tell application "System Events" to get name of first application process whose frontmost is true`
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "osascript", "-e", script).CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("ask System Events which application is frontmost: %w; it said: %s",
			err, strings.TrimSpace(string(out)))
	}
	return strings.TrimSpace(string(out)), nil
}

// ActivateApp brings a named application forward.
//
// It exists for exactly one purpose: putting the owner's application back after
// a launch method took focus away from it. Nothing else in a realtest may call
// it, because a realtest that activates Emacs has stopped being unobtrusive.
func ActivateApp(ctx context.Context, name string) error {
	if name == "" {
		return fmt.Errorf("no application was named to reactivate")
	}
	script := fmt.Sprintf(`tell application %q to activate`, name)
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "osascript", "-e", script).CombinedOutput()
	if err != nil {
		return fmt.Errorf("reactivate %q: %w; osascript said: %s", name, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// LaunchMethod names how an Emacs was started, for the report.
type LaunchMethod string

const (
	MethodOpenBackground LaunchMethod = "open -g -a Emacs"
	MethodDirectRestore  LaunchMethod = "Emacs.app binary, frontmost app reactivated"
)

// Launch is one launch attempt's whole account.
type Launch struct {
	Method LaunchMethod
	// SpawnedAt is when the launcher started the process. It is the T0 every
	// phase in phases.go is measured from, and it is taken as late as
	// possible before the exec so it does not carry this package's own
	// bookkeeping.
	SpawnedAt time.Time
	// FrontBefore and FrontAfter are the frontmost application either side of
	// the launch, and DisturbedOwner is whether they differ.
	FrontBefore    string
	FrontAfter     string
	DisturbedOwner bool
	// Reactivated is whether this method had to put the owner's application
	// back.
	Reactivated bool
}

// vendorGuardEnv is the ONE substitution a realtest makes: no real Claude call
// can occur. It is exported onto the Emacs process, and inherited from there by
// everything Emacs spawns — the daemon's launcher passes `process-environment`
// through (lisp/daemon.el, `agent-repl-daemon--environment`, which strips only
// AGENT_REPL_STATE_DIR and MULTI_REPO_ROOT to restate them), and the daemon
// passes `os.Environ()` through to each shim and sets the variable explicitly
// on top when its own contracts carry it
// (daemon/internal/shimclient/supervisor.go, `spawnEnv`).
const vendorGuardEnv = "AGENT_REPL_FORBID_VENDOR_CALLS"

// LaunchOpenBackground starts Emacs.app without bringing it forward.
//
// `open -g` returns as soon as LaunchServices has accepted the request, not
// when Emacs is up, which is correct here: SpawnedAt is the moment the process
// was asked for, and every phase after it is read off the log.
//
// `--env` rather than an inherited environment: `open` hands the application to
// launchd, which does NOT pass this process's environment along, so a variable
// merely exported here would never reach Emacs. Getting that wrong would mean a
// realtest that believes the vendor is forbidden while the real SDK is one
// prompt away, which is why it is stated on the command line where it can be
// read back off the process.
func LaunchOpenBackground(ctx context.Context) (Launch, error) {
	result := Launch{Method: MethodOpenBackground}

	before, err := FrontmostApp(ctx)
	if err != nil {
		return result, err
	}
	result.FrontBefore = before

	callCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
	defer cancel()
	cmd := exec.CommandContext(callCtx, "open", "-g", "-a", "/Applications/Emacs.app",
		"--env", vendorGuardEnv+"=1")
	result.SpawnedAt = time.Now()
	if out, err := cmd.CombinedOutput(); err != nil {
		return result, fmt.Errorf("launch Emacs in the background: %w; open said: %s",
			err, strings.TrimSpace(string(out)))
	}
	return result, nil
}

// LaunchDirectRestoring spawns the bundle's executable and puts the owner's
// application back.
//
// The child is deliberately NOT waited on and NOT parented to the test: an
// Emacs whose parent exits keeps running, which is the point — the owner's
// editor is left standing after the run.
func LaunchDirectRestoring(ctx context.Context) (Launch, error) {
	result := Launch{Method: MethodDirectRestore}

	before, err := FrontmostApp(ctx)
	if err != nil {
		return result, err
	}
	result.FrontBefore = before

	cmd := exec.Command(EmacsAppBinary)
	cmd.Env = append(os.Environ(), vendorGuardEnv+"=1")
	// The process must outlive this one, so its standard streams go nowhere
	// rather than to a pipe this side will close.
	cmd.Stdin, cmd.Stdout, cmd.Stderr = nil, nil, nil
	result.SpawnedAt = time.Now()
	if err := cmd.Start(); err != nil {
		return result, fmt.Errorf("spawn %s: %w", EmacsAppBinary, err)
	}
	if err := cmd.Process.Release(); err != nil {
		return result, fmt.Errorf("release the spawned Emacs so it outlives this run: %w", err)
	}

	// Putting focus back is the whole reason this method exists, and it is
	// done as soon as the process is spawned rather than after the frame maps:
	// waiting for the frame means waiting through the steal.
	if before != "" {
		if err := ActivateApp(ctx, before); err != nil {
			return result, err
		}
		result.Reactivated = true
	}
	return result, nil
}

// ObserveFocus fills in a launch's after-state.
func (l *Launch) ObserveFocus(ctx context.Context) error {
	after, err := FrontmostApp(ctx)
	if err != nil {
		return err
	}
	l.FrontAfter = after
	l.DisturbedOwner = l.FrontBefore != "" && after != l.FrontBefore
	return nil
}

// ProcessEnvironment reads a running process's environment through `ps -Eww`.
//
// This is how the vendor guard is VERIFIED rather than trusted. The launcher
// states the variable, the module's own launcher passes it on, and the daemon's
// supervisor passes it on again — all of which is readable in the source, and
// none of which proves the process standing in front of you actually has it.
// The kernel's copy does.
func ProcessEnvironment(ctx context.Context, pid int) (map[string]string, error) {
	callCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
	defer cancel()
	out, err := exec.CommandContext(callCtx, "ps", "-Eww", "-o", "command=", "-p", fmt.Sprint(pid)).Output()
	if err != nil {
		return nil, fmt.Errorf("read process %d's environment: %w", pid, err)
	}
	env := make(map[string]string)
	for _, field := range strings.Fields(string(out)) {
		name, value, ok := strings.Cut(field, "=")
		if !ok {
			continue
		}
		env[name] = value
	}
	return env, nil
}
