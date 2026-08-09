// Package externalbrowser opens a hyperlink in the one browser window every
// link belongs in: the dodge@chess.com Google Chrome profile, brought to the
// front.
//
// WHY THE DAEMON OPENS LINKS AT ALL. The gui frontend is the webapp mounted
// inside Emacs as an xwidget. A markdown link in a response bubble renders as
// an anchor, and clicking it makes WebKit navigate the webview (or ask Emacs
// for a second one) — the page the user was reading disappears behind a web
// page in an editor buffer. The webapp therefore cancels the click before it
// can become a navigation and POSTs the URL to this daemon, which is the
// nearest process that can actually launch a browser.
//
// PROFILE SELECTION. macOS `open -a Foo --args ...` DROPS the arguments
// whenever Foo is already running, so `--profile-directory` would be honored
// on a cold launch and silently ignored on every link after it. Chrome's own
// executable is invoked directly instead: it hands the URL to the running
// browser over Chrome's singleton socket TOGETHER with the requested profile,
// which is the only invocation that lands the tab in a specific profile's
// window reliably.
//
// The Emacs half of the same policy lives in lisp/external-browser.el, which
// covers `browse-url` (org links, magit, help buttons). The two must name the
// same profile.
package externalbrowser

import (
	"fmt"
	"os/exec"
	"strings"
)

const (
	// Binary is the Chrome executable a URL is handed to. Invoked directly
	// rather than through `open` for the reason in the package comment.
	Binary = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"

	// ProfileDirectory is the Chrome profile every hyperlink opens in.
	// "Profile 6" is the dodge@chess.com login window; the
	// dodge.w.coates@gmail.com login window is "Profile 7". The value is the
	// on-disk directory name under Chrome's user-data dir, which is what
	// --profile-directory takes — not the account address.
	ProfileDirectory = "Profile 6"

	// App is the application name used to raise the browser BEFORE the URL is
	// handed over. Chrome raises the profile window it puts a new tab in, but
	// it never brings itself to the front, so the browser is activated
	// explicitly — see Open for why the activation goes first.
	App = "Google Chrome"
)

// Runner runs one external command to completion. Injected so tests exercise
// the argv and the sequencing without launching a browser.
type Runner func(name string, args ...string) error

// Exec is the production Runner: it runs the command and reports its failure.
func Exec(name string, args ...string) error {
	out, err := exec.Command(name, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("externalbrowser: %s: %w (output: %s)",
			name, err, strings.TrimSpace(string(out)))
	}
	return nil
}

// Validate reports whether url is something worth handing to a browser command
// line. Restricted to http/https for the same reason the webapp's markdown
// renderer restricts link targets: no other scheme belongs in an argv assembled
// from model output.
func Validate(url string) error {
	if url == "" {
		return fmt.Errorf("externalbrowser: url is required")
	}
	if !strings.HasPrefix(url, "http://") && !strings.HasPrefix(url, "https://") {
		return fmt.Errorf("externalbrowser: url must be http or https, got %q", url)
	}
	if strings.ContainsAny(url, " \t\r\n") {
		return fmt.Errorf("externalbrowser: url must not contain whitespace, got %q", url)
	}
	return nil
}

// LaunchArgv is the Chrome argument list that opens url in the pinned profile.
func LaunchArgv(url string) []string {
	return []string{"--profile-directory=" + ProfileDirectory, url}
}

// ActivateArgv is the osascript argument list that raises the browser.
func ActivateArgv() []string {
	return []string{"-e", fmt.Sprintf("tell application %q to activate", App)}
}

// Open raises the browser through run and then hands url to the pinned Chrome
// profile. A refused URL, a failed raise, and a failed hand-off are three
// distinct errors: a link the user clicked that silently went nowhere is worse
// than a loud failure, and the caller logs which of the three happened.
//
// ORDER MATTERS, and it is the reason focus lands on the right WINDOW. Chrome
// raises the profile window it puts the new tab in, but it does not bring
// itself to the front; activating afterwards would instead restore whichever
// window was frontmost before, which is routinely a window of the other
// profile. Activating FIRST makes the outcome independent of how long Chrome
// takes to process the hand-off: the app is already frontmost, so the window
// Chrome raises for the tab is the window the user ends up looking at.
func Open(url string, run Runner) error {
	if err := Validate(url); err != nil {
		return err
	}
	if err := run("osascript", ActivateArgv()...); err != nil {
		return fmt.Errorf("externalbrowser: raising %q before opening %s: %w",
			App, url, err)
	}
	if err := run(Binary, LaunchArgv(url)...); err != nil {
		return fmt.Errorf("externalbrowser: opening %s in profile %q: %w",
			url, ProfileDirectory, err)
	}
	return nil
}
