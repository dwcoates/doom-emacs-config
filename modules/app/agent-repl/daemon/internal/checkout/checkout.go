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
	return resolveRoot(exePath, compiledRoot)
}

// resolveRoot is Root with its LAST RESORT injected. The compiled-in source
// path is the one input the caller cannot supply and the process cannot
// change, so the refusal that follows a walk finding nothing is unreachable
// without a seam here; production passes compiledRoot and behaves exactly as
// Root's documentation says.
func resolveRoot(exePath string, compiled func() (string, bool)) (string, error) {
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
	if root, ok := compiled(); ok {
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
	return compiledRootFrom(file, ok)
}

// compiledRootFrom answers the checkout containing sourceFile, walking up from
// its directory. known is runtime.Caller's own ok: an unknown caller frame
// yields no root rather than a walk from an empty path. It is separated from
// compiledRoot so the two answers a MISSING compiled path can give — a
// checkout that moved after it was built, and a source path in no checkout at
// all — are reachable without moving this package's own source.
func compiledRootFrom(sourceFile string, known bool) (string, bool) {
	if !known {
		return "", false
	}
	dir := filepath.Dir(sourceFile)
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
