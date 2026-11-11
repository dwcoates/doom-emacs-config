// Package appfreeze FREEZES, never quits, a configured list of applications
// while persistent wifi's lid-closed quiet state stands: SIGSTOP to every
// process of each listed application (the application and its helpers) when
// the lid closes, SIGCONT when it opens. A frozen process keeps its memory
// and windows, uses no CPU, and continues exactly where it stopped.
//
// THE FROZEN SET IS KEPT DURABLY (Record), and its lifetime is the structural
// guarantee: it is written BEFORE the first SIGSTOP and cleared only AFTER
// the last SIGCONT, so whatever exits the daemon (a crash, a kill, a
// handover) between the two, the record names every process that may still
// be stopped. A later daemon adopts it (never freezing twice) or thaws it,
// and the thaw guard (guard.go) thaws it when the last daemon holding it is
// gone without a successor.
//
// An application launched while the lid is closed is not chased: the set is
// the processes running at the moment the state began.
package appfreeze

import (
	"fmt"
	"strings"
)

// EnvApps names the applications to freeze, comma-separated by their bundle
// names ("Slack,Google Chrome"). Unset is DefaultApps; `none` freezes
// nothing. It is per-user configuration, set in the shell profile like the
// other persistent-wifi variables.
const EnvApps = "AGENT_REPL_PERSISTENT_WIFI_FREEZE_APPS"

// NoApps is EnvApps' value that freezes nothing.
const NoApps = "none"

// DefaultApps is the list frozen when EnvApps is unset: the applications that
// draw for a screen nobody sees with the lid closed. Docker is not on it: the
// e2e-emacs gate suite needs it while the lid is closed.
var DefaultApps = []string{"Slack", "Google Chrome", "Spotify", "WhatsApp", "Activity Monitor"}

// neverFrozen are the bundles no entry may name: the editor agent-repl runs
// in, every terminal, and agent-repl's own applications. A process of one is
// never frozen, so an entry naming one is refused outright.
var neverFrozen = []string{
	"Emacs", "Zettel",
	"Terminal", "iTerm", "iTerm2", "Ghostty", "WezTerm", "Alacritty", "kitty", "Warp", "Hyper", "Tabby",
}

// AppsFromEnv resolves the configured list. Every entry is trimmed; an empty
// entry, a duplicate, and an entry naming a bundle that is never frozen
// (neverFrozen) are REFUSED, so a configuration that would freeze the editor
// or a terminal never starts.
func AppsFromEnv(getenv func(string) string) ([]string, error) {
	raw := getenv(EnvApps)
	if raw == "" {
		return append([]string(nil), DefaultApps...), nil
	}
	if strings.TrimSpace(raw) == NoApps {
		return nil, nil
	}
	seen := map[string]bool{}
	var apps []string
	for entry := range strings.SplitSeq(raw, ",") {
		name := strings.TrimSpace(entry)
		switch {
		case name == "":
			return nil, fmt.Errorf("appfreeze: %s=%q has an empty entry", EnvApps, raw)
		case strings.ContainsAny(name, "/"):
			return nil, fmt.Errorf("appfreeze: %s entry %q is a path; name the application's bundle (\"Google Chrome\")", EnvApps, name)
		case seen[strings.ToLower(name)]:
			return nil, fmt.Errorf("appfreeze: %s names %q twice", EnvApps, name)
		case isNeverFrozen(name):
			return nil, fmt.Errorf("appfreeze: %s entry %q names the editor, a terminal or agent-repl itself, which is never frozen", EnvApps, name)
		}
		seen[strings.ToLower(name)] = true
		apps = append(apps, name)
	}
	return apps, nil
}

func isNeverFrozen(name string) bool {
	for _, n := range neverFrozen {
		if strings.EqualFold(n, name) {
			return true
		}
	}
	return false
}
