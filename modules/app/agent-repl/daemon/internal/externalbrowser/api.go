// Package externalbrowser opens a link in the pinned external browser profile.
//
// It is the OpenExternal verb's whole implementation: a clicked link never
// navigates the webview, and the browser profile is pinned so a link does not
// land in whatever window happened to be frontmost.
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
	"time"

	"claude-repld/internal/dlog"
)

// EnvBrowserCmd overrides the launcher command. It is the operator/test knob
// AGENTS.md pins: the whole launch becomes `<cmd> <url>`, with no profile flag
// and no activation, so a test can point it at a script and an operator can
// point it at a different browser.
const EnvBrowserCmd = "AGENT_REPL_BROWSER_CMD"

// Opener opens links externally.
type Opener interface {
	// Open launches url in the pinned profile. A refused or malformed url is
	// an error the caller surfaces; nothing is opened silently.
	Open(ctx context.Context, url string) error
}

// Config assembles an Opener.
type Config struct {
	// LauncherCmd is the command a url is handed to. Empty takes
	// $AGENT_REPL_BROWSER_CMD, and an unset environment takes DefaultBinary
	// (with the pinned-profile argv and the activation step). A command from
	// either override is invoked as `<cmd> <url>` and nothing else.
	LauncherCmd string
	// Profile is the pinned browser profile directory, used only on the
	// default path. Empty takes DefaultProfileDirectory.
	Profile string
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
// $AGENT_REPL_BROWSER_CMD, and only then to the pinned-profile default.
func New(cfg Config) (Opener, error) {
	if cfg.Logger == nil {
		return nil, errors.New("externalbrowser: a logger is required")
	}
	return newOpener(cfg), nil
}
