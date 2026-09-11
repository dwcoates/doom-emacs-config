//go:build realtest

package realtest

import (
	"context"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// THE EDITOR MUST NOT DISTURB THE OWNER. A realtest launches Emacs and the
// owner keeps typing wherever they were: focus never moves, and no picture is
// ever taken, so Emacs is never brought frontmost either.
//
// There is exactly one launch method:
//
//	LaunchOpenBackground   `open -gj -a Emacs`. -g ("--background") asks
//	                       LaunchServices not to bring the application forward,
//	                       and -j ("--hide") launches it hidden. -g alone still
//	                       ACTIVATED Emacs on run 3's cold launch: a GUI app's
//	                       first launch activates despite -g, and moved focus
//	                       from Chrome to Emacs. -j launches the app hidden, so
//	                       there is no window for the window server to bring
//	                       forward and nothing to steal focus with. It asks the
//	                       window server for the behavior instead of correcting
//	                       for it (docs/REALTEST-JUDGEMENT-CALLS.md, realtest 1,
//	                       row 29).
//
// A second method used to exist here: the bundle's own executable spawned
// directly, with the frontmost application captured before and reactivated
// once the frame mapped, correcting a focus steal rather than preventing it.
// It was removed (row 24) once run 1's focus move was traced to this module's
// own webview pre-creation on link-up (commit 3db3d6271), which macOS answers
// by activating Emacs regardless of how it was launched.
//
// Focus is still measured, not assumed: FrontmostApp reads the frontmost
// process from System Events before and after, and the test reports whether
// the launch left it alone. That detection is what proves the invariant and it
// stays whatever the launch flags are.

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

// LaunchMethod names how an Emacs was started, for the report.
type LaunchMethod string

const (
	MethodOpenBackground LaunchMethod = "open -gj -a Emacs"
)

// emacsBundle is the application bundle a realtest launches.
const emacsBundle = "/Applications/Emacs.app"

// openBackgroundArgs is the argv `open` is invoked with, factored out so a test
// can assert the flags without launching anything.
//
// -g keeps LaunchServices from bringing the app forward, and -j launches it
// HIDDEN so there is no window for the window server to activate — the pair is
// what keeps a GUI app's first launch from stealing focus, which -g alone did
// not on run 3 (row 29). --env states the vendor guard on the command line
// because `open` hands the app to launchd, which does not pass this process's
// environment along.
func openBackgroundArgs() []string {
	return []string{"-gj", "-a", emacsBundle, "--env", vendorGuardEnv + "=1"}
}

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

// LaunchOpenBackground starts Emacs.app hidden, without bringing it forward.
//
// `open -gj` returns as soon as LaunchServices has accepted the request, not
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
	cmd := exec.CommandContext(callCtx, "open", openBackgroundArgs()...)
	result.SpawnedAt = time.Now()
	if out, err := cmd.CombinedOutput(); err != nil {
		return result, fmt.Errorf("launch Emacs in the background: %w; open said: %s",
			err, strings.TrimSpace(string(out)))
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
