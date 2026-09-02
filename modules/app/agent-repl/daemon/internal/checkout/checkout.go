// Package checkout resolves the agent-repl checkout the daemon binary was
// deployed from, which is what the --shim-main, --webapp-dist and
// --prompts-dir defaults are relative to.
package checkout

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
)

// Env is the environment override for the checkout root.
const Env = "AGENT_REPL_CHECKOUT"

// marker is the module-root directory name, relative to a repository root,
// that identifies a checkout of agent-repl.
const marker = "modules/app/agent-repl"

// Root resolves the checkout root: $AGENT_REPL_CHECKOUT when set, otherwise
// the nearest ancestor of exePath that contains the marker
// "modules/app/agent-repl". The answer is the module root, i.e. the
// "modules/app/agent-repl" directory itself.
//
// When the environment variable is set, it is used verbatim (cleaned with
// filepath.Clean) and is not validated against the marker, because an
// operator naming a root means it.
//
// Absent the environment variable, exePath's symlinks are resolved first (a
// failure to resolve them falls back to the given path rather than failing),
// and the search walks upward from the executable's directory. At each
// ancestor, that ancestor is the root if its path ends with the marker, or
// if it contains the marker as a subdirectory, in which case
// "<ancestor>/modules/app/agent-repl" is the root; whichever is found first
// walking up wins.
//
// If neither the environment variable nor the walk yields a root, Root
// returns a loud error naming exePath and Env rather than a guessed or
// relative path.
func Root(exePath string) (string, error) {
	if env, ok := os.LookupEnv(Env); ok {
		return filepath.Clean(env), nil
	}

	resolved, err := filepath.EvalSymlinks(exePath)
	if err != nil {
		resolved = exePath
	}

	dir := filepath.Dir(resolved)
	for {
		if hasMarkerSuffix(dir) {
			return dir, nil
		}
		candidate := filepath.Join(dir, marker)
		if info, err := os.Stat(candidate); err == nil && info.IsDir() {
			return candidate, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}

	// THE BINARY WAS NOT DEPLOYED INTO THE CHECKOUT. `go build -o <tmp>` puts
	// it outside the tree, which is what every build from a test harness or a
	// scratch directory does. The path this source file was COMPILED from is
	// still inside the checkout, so it names the tree the binary was built
	// from — the same tree a deployed binary would have been copied out of.
	if root, ok := compiledRoot(); ok {
		return root, nil
	}

	return "", fmt.Errorf(
		"resolve agent-repl checkout: no ancestor of %q contains %q, and the compiled-in source path names none either; set %s to override",
		exePath, marker, Env)
}

// compiledRoot answers the checkout this package was COMPILED from, walking up
// from the source path the compiler recorded for this file. It is the last
// resort, after the environment and after the executable's own location,
// because a checkout that moved after it was built leaves a path that no
// longer exists — which is exactly what the existence check below catches.
func compiledRoot() (string, bool) {
	_, file, _, ok := runtime.Caller(0)
	if !ok {
		return "", false
	}
	dir := filepath.Dir(file)
	for {
		if hasMarkerSuffix(dir) {
			if info, err := os.Stat(dir); err == nil && info.IsDir() {
				return dir, true
			}
			return "", false
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", false
		}
		dir = parent
	}
}

// VocabDir is the shared render vocabulary beneath root: the render-colors and
// paint-class tables the resolvers refuse to serve an unpainted state without.
func VocabDir(root string) string {
	return filepath.Join(root, "proto", "vocab")
}

// hasMarkerSuffix reports whether dir's path ends with the marker's
// segments, i.e. dir is itself the "modules/app/agent-repl" directory.
func hasMarkerSuffix(dir string) bool {
	markerSegments := strings.Split(marker, "/")
	dirSegments := strings.Split(filepath.ToSlash(dir), "/")
	if len(dirSegments) < len(markerSegments) {
		return false
	}
	tail := dirSegments[len(dirSegments)-len(markerSegments):]
	return strings.Join(tail, "/") == marker
}

// ShimMain is the compiled shim entry point beneath root.
func ShimMain(root string) string {
	return filepath.Join(root, "agent-shim", "claude", "shim", "dist", "main.js")
}

// WebappDist is the built webapp's static assets beneath root.
func WebappDist(root string) string {
	return filepath.Join(root, "webapp", "dist")
}

// PromptsDir is the prompt corpus beneath root.
func PromptsDir(root string) string {
	return filepath.Join(root, "prompts")
}
