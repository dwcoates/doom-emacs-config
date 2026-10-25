// Package checkout resolves the agent-repl checkout the daemon binary was
// deployed from, which is what the --shim-main, --webapp-dist and
// --prompts-dir defaults are relative to.
package checkout

import (
	"fmt"
	"os"
	"path/filepath"
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
	return resolveRoot(exePath, builtFrom)
}

// builtFrom is the module root this binary was built from, stamped at link
// time (BuiltFromLink) by bin/build-frontend.sh. It is empty in a binary
// nobody stamped (a test harness's or a hand-run `go build`), which then
// resolves through the environment or its own location, or is refused.
//
// A STAMP, NOT THE COMPILED-IN SOURCE PATH. Every Go build here runs with
// -trimpath, so worktrees share one build cache instead of each caching its
// own copy of every package; -trimpath removes the absolute source paths
// runtime.Caller would have answered, so the checkout is named explicitly.
var builtFrom string

// BuiltFromLink is the -ldflags -X binding that stamps root as the module
// root a binary was built from.
func BuiltFromLink(root string) string {
	return builtFromSymbol + "=" + root
}

// builtFromSymbol is builtFrom's linker name.
const builtFromSymbol = "agent-repld/internal/checkout.builtFrom"

// resolveRoot is Root with its LAST RESORT injected. The stamp is the one
// input the process cannot change, so the refusal that follows a walk finding
// nothing is unreachable without a seam here; production passes builtFrom and
// behaves exactly as Root's documentation says.
func resolveRoot(exePath, stamp string) (string, error) {
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

	// THE BINARY WAS NOT DEPLOYED INTO THE CHECKOUT. A staged deploy build
	// lands outside the tree, so the module root stamped into it at build
	// time names the tree it was built from: the same tree a deployed binary
	// would have been copied out of.
	if stamp == "" {
		return "", fmt.Errorf(
			"resolve agent-repl checkout: no ancestor of %q contains %q, and the binary carries no %s stamp; set %s to override",
			exePath, marker, builtFromSymbol, Env)
	}
	root := filepath.Clean(stamp)
	if !hasMarkerSuffix(root) {
		return "", fmt.Errorf(
			"resolve agent-repl checkout: no ancestor of %q contains %q, and the stamped root %q is not a %q module root; set %s to override",
			exePath, marker, stamp, marker, Env)
	}
	// A checkout that moved after the binary was built leaves a stamp that no
	// longer exists.
	info, err := os.Stat(root)
	if err != nil {
		return "", fmt.Errorf(
			"resolve agent-repl checkout: no ancestor of %q contains %q, and the stamped root is unusable: %w; set %s to override",
			exePath, marker, err, Env)
	}
	if !info.IsDir() {
		return "", fmt.Errorf(
			"resolve agent-repl checkout: no ancestor of %q contains %q, and the stamped root %q is not a directory; set %s to override",
			exePath, marker, stamp, Env)
	}
	return root, nil
}

// RepoRoot answers the repository root that contains the module root root:
// root with its trailing "modules/app/agent-repl" segments removed. A root that
// does not end in the marker (an unvalidated $AGENT_REPL_CHECKOUT) names no
// repository, so RepoRoot refuses it loudly rather than guessing one.
func RepoRoot(root string) (string, error) {
	clean := filepath.Clean(root)
	if !hasMarkerSuffix(clean) {
		return "", fmt.Errorf("checkout: %q is not a %q module root, so it names no repository root", root, marker)
	}
	repo := clean
	for range strings.Split(marker, "/") {
		repo = filepath.Dir(repo)
	}
	return repo, nil
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

// ShimBuildStamp is the shim bundle's build stamp beneath root: the file the
// shim's build chain writes next to its compiled entry point. It is the
// production source of SHIM_BUILD_SHA.
func ShimBuildStamp(root string) string {
	return filepath.Join(root, "agent-shim", "claude", "shim", "dist", ".built-sha")
}
