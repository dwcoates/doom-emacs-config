// Package typecheck runs a TypeScript package's typecheck incrementally,
// seeding each worktree's incremental state from the host's newest passing
// typecheck of the same package (`testrun typecheck`).
//
// INCREMENTAL, WITH DECLARATIONS. Each package's `npm run typecheck` is
// `tsc --incremental` emitting declarations only, its state under CacheDir.
// Measured 2026-11-20 (webapp, one run each, warm): a full `tsc --noEmit`
// costs 19-20 CPU-seconds; incremental after a body edit to a file with one
// importer 5.7, to a file sixteen files import 5.0, and after an exported-API
// edit to the module 221 files import 21.7. Without declaration emit, a body
// edit to that sixteen-importer file cost 26.1, MORE than the full check:
// under --noEmit tsc keys a file's signature on its text, so every edit
// rechecks everything that imports it, transitively. The shim: full 16.3-16.9,
// a body edit 3.9, an exported-API edit to its most imported module 14.2.
//
// SEEDED HOST-WIDE. A gate typechecks in its workspace's worktree, which may
// never have typechecked before. tsc records every path relative to its
// build-info file, so the same file at the same place in another worktree is
// a valid start: before the typecheck, a worktree with no state of its own
// takes the host's seed (Seed), and after a typecheck that passed, its state
// becomes the seed (Publish). tsc compares every source file by the hash of
// its text, so a seed from another checkout makes the typecheck cheaper,
// never different.
package typecheck

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"

	"agentrepl/testrun/internal/atomicfile"
)

// CacheDir is where a package's incremental typecheck keeps its state, under
// the package's directory; git ignores it, and its suite declares it
// generated (roster.Reuse.Generated).
const CacheDir = ".typecheck-cache"

// BuildInfoFile is tsc's incremental state under CacheDir
// (--tsBuildInfoFile).
const BuildInfoFile = "tsbuildinfo"

// SeedsEnv overrides DefaultSeedDir, for tests.
const SeedsEnv = "AGENT_REPL_TYPECHECK_SEEDS"

// BuildInfoPath is package pkgDir's own incremental state.
func BuildInfoPath(pkgDir string) string {
	return filepath.Join(pkgDir, CacheDir, BuildInfoFile)
}

// DefaultSeedDir is where the host keeps every package's seed.
func DefaultSeedDir() (string, error) {
	if p := os.Getenv(SeedsEnv); p != "" {
		return p, nil
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return "", fmt.Errorf("typecheck: resolve the home directory: %w", err)
	}
	return filepath.Join(home, ".cache", "agent-repl", "typecheck"), nil
}

var seedName = regexp.MustCompile(`^[A-Za-z0-9_-]+$`)

// SeedPath is the seed of the package its suite names name, under seedDir.
func SeedPath(seedDir, name string) (string, error) {
	if !seedName.MatchString(name) {
		return "", fmt.Errorf("typecheck: %q is no suite name a seed can be kept under", name)
	}
	return filepath.Join(seedDir, name+".tsbuildinfo"), nil
}

// Seed copies seed to the package's own state when the package has none and
// the seed exists, and answers whether it did.
func Seed(pkgDir, seed string) (bool, error) {
	local := BuildInfoPath(pkgDir)
	if _, err := os.Stat(local); err == nil {
		return false, nil
	} else if !errors.Is(err, fs.ErrNotExist) {
		return false, fmt.Errorf("typecheck: read %s: %w", local, err)
	}
	if _, err := os.Stat(seed); errors.Is(err, fs.ErrNotExist) {
		return false, nil
	} else if err != nil {
		return false, fmt.Errorf("typecheck: read the seed %s: %w", seed, err)
	}
	if err := copyAtomic(seed, local); err != nil {
		return false, err
	}
	return true, nil
}

// Publish makes the package's own state, after a typecheck that passed, the
// host's seed. Two worktrees publishing at once each replace the seed whole.
func Publish(pkgDir, seed string) error {
	return copyAtomic(BuildInfoPath(pkgDir), seed)
}

// copyAtomic copies src over dst through atomicfile.Replace, so a reader sees
// the old dst or the new one, never part of one.
func copyAtomic(src, dst string) error {
	data, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("typecheck: read %s: %w", src, err)
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return fmt.Errorf("typecheck: create %s: %w", filepath.Dir(dst), err)
	}
	if err := atomicfile.Replace(dst, filepath.Base(dst)+".*", data); err != nil {
		return fmt.Errorf("typecheck: %w", err)
	}
	return nil
}

// Logger is the run's canonical log (run.Log).
type Logger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Run seeds the package in pkgDir, runs its typecheck (run, which answers
// the command's error), and publishes its state when the typecheck passed.
// A failed typecheck publishes nothing, so the seed is always a passing
// typecheck's.
func Run(pkgDir, name string, seedDir string, run func() error, log Logger) error {
	seed, err := SeedPath(seedDir, name)
	if err != nil {
		return err
	}
	seeded, err := Seed(pkgDir, seed)
	if err != nil {
		return err
	}
	if seeded {
		log.Infof("typecheck %s: seeded its incremental state from %s", name, seed)
	}
	if err := run(); err != nil {
		return fmt.Errorf("typecheck %s: %w", name, err)
	}
	if err := Publish(pkgDir, seed); err != nil {
		return err
	}
	log.Infof("typecheck %s: published its incremental state to %s", name, seed)
	return nil
}
