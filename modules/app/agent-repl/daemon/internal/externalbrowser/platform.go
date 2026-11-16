package externalbrowser

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
)

// Platform is where one platform keeps what the default path launches: the
// Chrome executable, how the browser is raised, Chrome's Local State, and the
// program that hands a Slack deep link to the Slack app.
type Platform struct {
	// Binary is the Chrome executable a url is handed to: an absolute path,
	// or a name looked up on PATH.
	Binary string
	// ActivateBin raises the browser before the hand-off with ActivateArgv
	// (openDefault says why the raise goes first). Empty is a platform whose
	// browser raises the window it puts the tab in itself, so there is no
	// raise to make.
	ActivateBin  string
	ActivateArgv []string
	// LocalState is Chrome's Local State document, relative to the home
	// directory.
	LocalState string
	// SlackOpen hands a `slack://` url to the application registered for the
	// scheme.
	SlackOpen string
}

// Darwin is macOS. Chrome's own executable is invoked directly rather than
// through `open`: macOS `open -a Foo --args …` DROPS the arguments whenever
// Foo is already running, so `--profile-directory` would be honored on a cold
// launch and silently ignored on every link after it. Invoked directly, Chrome
// hands the url to the running browser over its singleton socket TOGETHER
// with the requested profile, which is the only invocation that lands the tab
// in a specific profile's window reliably. Chrome does not bring itself to the
// front, so osascript raises it.
var Darwin = Platform{
	Binary:       "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
	ActivateBin:  "osascript",
	ActivateArgv: ActivateArgv("Google Chrome"),
	LocalState:   filepath.Join("Library", "Application Support", "Google", "Chrome", "Local State"),
	SlackOpen:    "/usr/bin/open",
}

// Linux is a freedesktop host. Chrome's launcher, google-chrome, hands the url
// and its profile to the running browser over the same singleton socket, and
// the browser activates the window it opens the tab in, so there is no raise
// of its own to make. Slack registers its scheme with xdg-open. (Untested on a
// Linux desktop: the routing is macOS's, unchanged.)
var Linux = Platform{
	Binary:     "google-chrome",
	LocalState: filepath.Join(".config", "google-chrome", "Local State"),
	SlackOpen:  "xdg-open",
}

// PlatformFor answers the platform of goos, refusing one agent-repl does not
// run on.
func PlatformFor(goos string) (Platform, error) {
	switch goos {
	case "darwin":
		return Darwin, nil
	case "linux":
		return Linux, nil
	default:
		return Platform{}, fmt.Errorf("externalbrowser: no browser platform for %s", goos)
	}
}

// Host is this host's platform. The daemon runs only where PlatformFor
// answers, so a host it refuses is a build for a platform agent-repl does not
// support, and fails at start.
var Host = func() Platform {
	p, err := PlatformFor(runtime.GOOS)
	if err != nil {
		panic(err)
	}
	return p
}()

// localStatePath answers the platform's Local State under the home
// directory; empty when there is no home (the opener then fails any link
// whose account must be routed, as a missing file does).
func (p Platform) localStatePath() string {
	home, err := os.UserHomeDir()
	if err != nil || home == "" {
		return ""
	}
	return filepath.Join(home, p.LocalState)
}
