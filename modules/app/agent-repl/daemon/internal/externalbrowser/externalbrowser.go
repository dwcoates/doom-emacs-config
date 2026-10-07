package externalbrowser

import (
	"context"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/usersetup"
)

const (
	// DefaultBinary is the browser executable a url is handed to when nothing
	// overrides it.
	//
	// Chrome's own executable is invoked directly rather than through `open`:
	// macOS `open -a Foo --args …` DROPS the arguments whenever Foo is already
	// running, so `--profile-directory` would be honored on a cold launch and
	// silently ignored on every link after it. Invoked directly, Chrome hands
	// the url to the running browser over its singleton socket TOGETHER with
	// the requested profile, which is the only invocation that lands the tab in
	// a specific profile's window reliably.
	DefaultBinary = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

	// DefaultApp is the application name used to raise the browser BEFORE the
	// url is handed over. See openDefault for why the activation goes first.
	DefaultApp = "Google Chrome"

	// activateBinary raises an application on macOS.
	activateBinary = "osascript"
)

// DefaultLaunchWindow is how long a launcher is given to exit before it is
// presumed to have become the browser itself.
const DefaultLaunchWindow = 2 * time.Second

// opener is the Opener.
type opener struct {
	launcherCmd    string
	defaultBin     string
	activateBin    string
	localStatePath string
	launchWindow   time.Duration
	log            dlog.Logger
}

// newOpener resolves every default and builds the opener.
func newOpener(cfg Config) *opener {
	o := &opener{
		launcherCmd:    cfg.LauncherCmd,
		defaultBin:     cfg.DefaultLauncherBin,
		activateBin:    cfg.ActivateBin,
		localStatePath: cfg.LocalStatePath,
		launchWindow:   cfg.LaunchWindow,
		log:            cfg.Logger,
	}
	if o.launcherCmd == "" {
		o.launcherCmd = os.Getenv(EnvBrowserCmd)
	}
	if o.defaultBin == "" {
		o.defaultBin = DefaultBinary
	}
	if o.activateBin == "" {
		o.activateBin = activateBinary
	}
	if o.localStatePath == "" {
		o.localStatePath = DefaultLocalStatePath()
	}
	if o.launchWindow <= 0 {
		o.launchWindow = DefaultLaunchWindow
	}
	o.log.Debug("daemon.externalbrowser.new", "external browser opener built", dlog.Context{
		"launcher":      o.launcherName(),
		"overridden":    o.launcherCmd != "",
		"launch_window": o.launchWindow.String(),
	})
	return o
}

// launcherName is the command a url will actually be handed to.
func (o *opener) launcherName() string {
	if o.launcherCmd != "" {
		return o.launcherCmd
	}
	return o.defaultBin
}

// Validate reports whether url is something worth handing to a browser command
// line. Restricted to http/https for the same reason the webapp's markdown
// renderer restricts link targets: no other scheme belongs in an argv
// assembled from model output.
func Validate(url string) error {
	if url == "" {
		return errors.New("externalbrowser: url is required")
	}
	if !strings.HasPrefix(url, "http://") && !strings.HasPrefix(url, "https://") {
		return fmt.Errorf("externalbrowser: url must be http or https, got %q", url)
	}
	if strings.ContainsAny(url, " \t\r\n") {
		return fmt.Errorf("externalbrowser: url must not contain whitespace, got %q", url)
	}
	return nil
}

// LaunchArgv is the argument list that opens url on the default path: in the
// named profile, or with no profile flag at all when profile is empty, which
// leaves the choice of window to the browser.
func LaunchArgv(profile, url string) []string {
	if profile == "" {
		return []string{url}
	}
	return []string{"--profile-directory=" + profile, url}
}

// ActivateArgv is the osascript argument list that raises the browser.
func ActivateArgv(app string) []string {
	return []string{"-e", fmt.Sprintf("tell application %q to activate", app)}
}

// Open implements Opener.
func (o *opener) Open(ctx context.Context, url, accountEmail string) error {
	if err := ctx.Err(); err != nil {
		return err
	}
	if err := Validate(url); err != nil {
		o.log.Error("daemon.externalbrowser.open", "external link refused", dlog.Context{
			"url":    url,
			"branch": "invalid-url",
			"error":  err.Error(),
		})
		return err
	}
	if o.launcherCmd != "" {
		return o.openOverridden(ctx, url)
	}
	profile, err := o.profileFor(url, accountEmail)
	if err != nil {
		return err
	}
	return o.openDefault(ctx, url, profile)
}

// profileFor resolves the Chrome profile directory accountEmail signs in as,
// reading Chrome's own Local State.
//
// THERE IS NO PINNED DEFAULT. A blank email is the logged-out state, an answer
// rather than a fault, and it answers no profile at all: the url goes to the
// browser with no profile flag. A non-empty email whose profile cannot be
// found is an error naming the email, because the account is real and a link
// that quietly landed in some other profile's window is exactly what the
// routing exists to prevent.
func (o *opener) profileFor(url, email string) (string, error) {
	if strings.TrimSpace(email) == "" {
		o.log.Debug("daemon.externalbrowser.profile_for_account", "no account email; opening with no profile flag", dlog.Context{
			"url":    url,
			"branch": "logged-out",
		})
		return "", nil
	}
	fail := func(branch string, err error, ctx dlog.Context) (string, error) {
		ctx["url"], ctx["email"], ctx["branch"], ctx["error"] = url, email, branch, err.Error()
		o.log.Error("daemon.externalbrowser.profile_for_account", "could not route the account to its Chrome profile", ctx)
		return "", err
	}
	if o.localStatePath == "" {
		return fail("no-local-state-path",
			usersetup.Errorf("externalbrowser: no Chrome Local State path to find the profile %s signs in as", email),
			dlog.Context{})
	}
	data, err := os.ReadFile(o.localStatePath) //nolint:gosec // daemon-derived Chrome path, never client input
	if err != nil {
		return fail("local-state-unreadable",
			usersetup.Errorf("externalbrowser: reading Chrome Local State to find the profile %s signs in as: %w", email, err),
			dlog.Context{"local_state_path": o.localStatePath})
	}
	profile, matched := ProfileForEmail(data, email)
	if !matched {
		return fail("no-profile-match",
			usersetup.Errorf("externalbrowser: no Chrome profile is signed in as %s", email),
			dlog.Context{"local_state_path": o.localStatePath})
	}
	o.log.Debug("daemon.externalbrowser.profile_for_account", "routed the account to its Chrome profile", dlog.Context{
		"url":     url,
		"email":   email,
		"profile": profile,
		"branch":  "matched",
	})
	return profile, nil
}

// openOverridden hands the url to the configured launcher and nothing else. An
// override names the whole launch, so neither the profile flag nor the
// activation applies: an operator pointing this at a different browser did not
// ask for Chrome's argv.
func (o *opener) openOverridden(ctx context.Context, url string) error {
	if err := o.run(ctx, o.launcherCmd, []string{url}); err != nil {
		o.log.Error("daemon.externalbrowser.open", "external link launch failed", dlog.Context{
			"url":      url,
			"launcher": o.launcherCmd,
			"branch":   "override-launch-failed",
			"error":    err.Error(),
		})
		return err
	}
	o.log.Debug("daemon.externalbrowser.open", "external link handed to the configured launcher", dlog.Context{
		"url":      url,
		"launcher": o.launcherCmd,
		"branch":   "override",
	})
	return nil
}

// openDefault raises the browser and then hands url to the routed profile.
//
// ORDER MATTERS, and it is the reason focus lands on the right WINDOW. Chrome
// raises the profile window it puts the new tab in, but it does not bring
// itself to the front; activating afterwards would instead restore whichever
// window was frontmost before, which is routinely a window of the OTHER
// profile. Activating FIRST makes the outcome independent of how long the
// browser takes to process the hand-off.
//
// A failed raise and a failed hand-off are distinct errors: a link the user
// clicked that silently went nowhere is worse than a loud failure.
func (o *opener) openDefault(ctx context.Context, url, profile string) error {
	if err := o.run(ctx, o.activateBin, ActivateArgv(DefaultApp)); err != nil {
		wrapped := fmt.Errorf("externalbrowser: raising %q before opening %s: %w", DefaultApp, url, err)
		o.log.Error("daemon.externalbrowser.open", "could not raise the external browser", dlog.Context{
			"url":    url,
			"app":    DefaultApp,
			"branch": "activate-failed",
			"error":  wrapped.Error(),
		})
		return wrapped
	}
	if err := o.run(ctx, o.defaultBin, LaunchArgv(profile, url)); err != nil {
		wrapped := fmt.Errorf("externalbrowser: opening %s in profile %q: %w", url, profile, err)
		o.log.Error("daemon.externalbrowser.open", "external link launch failed", dlog.Context{
			"url":     url,
			"profile": profile,
			"branch":  "launch-failed",
			"error":   wrapped.Error(),
		})
		return wrapped
	}
	o.log.Debug("daemon.externalbrowser.open", "external link handed to the routed profile", dlog.Context{
		"url":     url,
		"profile": profile,
		"branch":  "default",
	})
	return nil
}

// run starts one launcher and gives it the launch window to finish.
//
// IT NEVER BLOCKS ON THE BROWSER. A launcher that hands the url to an already
// running browser exits at once and its status is the answer; a launcher that
// BECOMES the browser (a cold Chrome start runs the browser in the invoked
// process) never exits at all, so a still-running launcher at the end of the
// window is a success, and the reaper keeps waiting so nothing is left a
// zombie. A launch that cannot start — a missing or non-executable command —
// fails synchronously and loudly, which is the case an operator most needs to
// see.
func (o *opener) run(ctx context.Context, name string, args []string) error {
	cmd := exec.Command(name, args...) //nolint:gosec // daemon-configured launcher, url validated above
	// The launcher outlives this call; a pipe to the daemon's own stdio would
	// keep a descriptor open for the life of a browser.
	cmd.Stdin, cmd.Stdout, cmd.Stderr = nil, nil, nil

	if err := cmd.Start(); err != nil {
		return fmt.Errorf("externalbrowser: starting %s: %w", name, err)
	}

	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()

	timer := time.NewTimer(o.launchWindow)
	defer timer.Stop()

	select {
	case err := <-exited:
		if err != nil {
			return fmt.Errorf("externalbrowser: %s exited with an error: %w", name, err)
		}
		return nil
	case <-timer.C:
		o.log.Debug("daemon.externalbrowser.open", "launcher is still running at the end of its window; it became the browser", dlog.Context{
			"launcher":      name,
			"launch_window": o.launchWindow.String(),
			"branch":        "still-running",
		})
		return nil
	case <-ctx.Done():
		o.log.Warn("daemon.externalbrowser.open", "launch abandoned by the caller's context", dlog.Context{
			"launcher": name,
			"branch":   "context-done",
			"error":    ctx.Err().Error(),
		})
		return ctx.Err()
	}
}
