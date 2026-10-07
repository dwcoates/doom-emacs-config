// Package externalbrowser opens a link in the external browser profile the
// session's account signs in as.
//
// It is the OpenExternal verb's whole implementation: a clicked link never
// navigates the webview, and the browser profile is routed by account so a link
// does not land in whatever window happened to be frontmost.
//
// WHY THE DAEMON OPENS LINKS AT ALL. The gui frontend is the webapp mounted
// inside Emacs as an xwidget. A markdown link in a response bubble renders as
// an anchor, and clicking it makes WebKit navigate the webview — the page the
// user was reading disappears behind a web page in an editor buffer. The
// webapp therefore cancels the click and calls OpenExternal, and the daemon is
// the nearest process that can actually launch a browser.
package externalbrowser

import (
	"context"
	"errors"
	"os"
	"time"

	"claude-repld/internal/dlog"
)

// EnvBrowserCmd overrides the launcher command. It is the operator/test knob
// AGENTS.md pins: the whole launch becomes `<cmd> <url>`, with no profile flag
// and no activation, so a test can point it at a script and an operator can
// point it at a different browser.
const EnvBrowserCmd = "AGENT_REPL_BROWSER_CMD"

// DefaultLauncherConfigured reports whether the pinned DEFAULT launcher is
// actually present on this machine.
//
// It exists so the composition root can tell "the operator configured no
// browser at all" from "the browser refused the link": a daemon on a host with
// neither $AGENT_REPL_BROWSER_CMD nor the pinned binary has nothing to hand a
// url to, and OpenExternal answers no_browser_configured instead of pretending
// a launch and failing.
func DefaultLauncherConfigured() bool {
	return DefaultLauncherConfiguredAt(DefaultBinary)
}

// DefaultLauncherConfiguredAt is DefaultLauncherConfigured against a named
// path. The pinned default is an absolute macOS application path, so the
// question "is this launcher present" has no seam at all otherwise; this is
// that seam, and DefaultLauncherConfigured is exactly this function applied to
// DefaultBinary.
func DefaultLauncherConfiguredAt(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir()
}

// Opener opens links externally.
type Opener interface {
	// Open launches url in the Chrome profile accountEmail signs in as,
	// resolved from Chrome's own Local State. A blank email opens with no
	// profile flag. An email no profile is signed in as, a refused or
	// malformed url, and a launcher that will not run are all errors the
	// caller surfaces; nothing is opened silently or in a guessed profile.
	// A configured launcher override skips the routing entirely: it names the
	// whole launch.
	Open(ctx context.Context, url, accountEmail string) error
}

// Config assembles an Opener.
type Config struct {
	// LauncherCmd is the command a url is handed to. Empty takes
	// $AGENT_REPL_BROWSER_CMD, and an unset environment takes DefaultBinary
	// (with the routed-profile argv and the activation step). A command from
	// either override is invoked as `<cmd> <url>` and nothing else.
	LauncherCmd string
	// LocalStatePath is Chrome's Local State document, read on the default
	// path to route an account email to its on-disk profile directory. Empty
	// takes DefaultLocalStatePath; a path that cannot be read fails the open
	// of any link whose account has an email.
	LocalStatePath string
	// DefaultLauncherBin is the browser executable the DEFAULT path hands a
	// url to. Empty takes DefaultBinary. It exists for the same reason
	// LauncherCmd does — the launcher's spelling is injected rather than
	// hardcoded — and it is the only way a test drives the default path,
	// whose binary is an absolute macOS application path.
	DefaultLauncherBin string
	// ActivateBin is the command that raises the browser before the url is
	// handed over on the DEFAULT path. Empty takes osascript.
	ActivateBin string
	// LaunchWindow is how long the launcher is given to hand the url off and
	// exit before it is presumed to have BECOME the browser (a cold Chrome
	// launch runs the browser in the invoked process, which never exits).
	// Empty takes DefaultLaunchWindow.
	LaunchWindow time.Duration
	// Logger is required; every branch of a launch is recorded.
	Logger dlog.Logger
}

// New builds the opener.
//
// ARCHITECTURE.md does not fix the launcher's spelling, so it is injected
// rather than hardcoded: the constructor takes the command, falls back to
// $AGENT_REPL_BROWSER_CMD, and only then to Chrome with the routed profile.
func New(cfg Config) (Opener, error) {
	if cfg.Logger == nil {
		return nil, errors.New("externalbrowser: a logger is required")
	}
	return newOpener(cfg), nil
}
