// Package tempdirs is the ONE answer to "does this directory lie inside a
// temporary directory", which is what the registry refuses to register (owner
// ruling, 2026-10-06).
//
// WHY. On 2026-09-02 a real-vendor capture run had the model drive the
// create-or-update-workspace skill from a scratch folder under
// `/private/var/folders/.../T/`, and the live daemon registered that folder as
// a repository ("cwd") with a workspace under it. A temporary folder is
// somebody's scratch space: the OS purges it, a test run deletes it, and a
// roster row for it is a row for a directory that is about to vanish. So the
// registry refuses one outright, with a sentence that names the directory and
// the temporary root it lies under.
//
// THE ROOTS (Roots):
//
//   - os.TempDir(): this process's own temporary directory. On Unix it IS
//     `$TMPDIR` when set and `/tmp` otherwise, so `$TMPDIR` needs no entry of
//     its own.
//   - `/tmp`: the POSIX temporary directory, whatever `$TMPDIR` says. On macOS
//     it is a symlink to `/private/tmp`.
//   - `/var/tmp`: the other POSIX temporary directory (preserved across
//     reboots, but temporary by definition). On macOS, `/private/var/tmp`.
//   - `/var/folders`: macOS's per-user temporary and cache roots
//     (`/var/folders/<xx>/<hash>/{T,C,0}`). `$TMPDIR` of every macOS user
//     process lives under it, including the process that wrote the
//     2026-09-02 registration, and the launchd-started daemon's own
//     `$TMPDIR` may differ from that process's, so the whole tree is listed
//     rather than this process's one leaf. Every sibling of `T/` is a
//     system-managed purgeable cache, never a place a repository lives.
//
// Every root is CANONICAL (dirpath.Canonical): symlinks resolved and in the
// volume's own case, so `/tmp/x`, `/private/tmp/x` and `/PRIVATE/tmp/x` are one
// answer. A root that does not exist on this host (`/var/folders` off macOS)
// canonicalizes to its cleaned self and simply matches nothing.
//
// INSIDE means EQUAL TO a root or BENEATH it by whole path components:
// `/tmpfoo` is not inside `/tmp`.
package tempdirs

import (
	"errors"
	"fmt"
	"path/filepath"
	"sort"
	"strings"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/envc"
)

// FixedRoots are the temporary roots every host is checked against, in the
// spelling a person types. Roots canonicalizes them.
var FixedRoots = []string{"/tmp", "/var/tmp", "/var/folders"}

// EnvTestRoot names the ONE directory beneath which a TEST-RUN daemon may
// register directories that lie inside a temporary root. It is the harness's
// seam, and it is honored only together with envc.EnvForbidVendorCalls (see
// FromEnv).
const EnvTestRoot = "AGENT_REPL_TEMPORARY_REGISTRATION_TEST_ROOT"

// InsideError is the refusal: Dir lies inside the temporary root Root. Both are
// canonical. It is an ANSWER to a request, never a fault.
type InsideError struct {
	// Dir is the refused directory, canonical.
	Dir string
	// Root is the temporary root Dir lies inside, canonical.
	Root string
}

// Error is the sentence the user and the log read.
func (e *InsideError) Error() string {
	return fmt.Sprintf("%s is inside the temporary directory %s; agent-repl does not register temporary folders", e.Dir, e.Root)
}

// AsInside reports whether err is (or wraps) the temporary-directory refusal.
func AsInside(err error) (*InsideError, bool) {
	var inside *InsideError
	if errors.As(err, &inside) {
		return inside, true
	}
	return nil, false
}

// Roots answers the canonical temporary roots for a process whose own
// temporary directory is tmpdir (os.TempDir()), sorted and without duplicates.
//
// A root that canonicalizes to the filesystem root is REFUSED: it would make
// every directory on the host temporary, and that is a misconfigured
// environment (`TMPDIR=/`) to surface, never a policy to apply.
func Roots(tmpdir string) ([]string, error) {
	if tmpdir == "" {
		return nil, errors.New("tempdirs: the process's temporary directory is empty")
	}
	spelled := append([]string{tmpdir}, FixedRoots...)
	seen := make(map[string]bool, len(spelled))
	out := make([]string, 0, len(spelled))
	for _, root := range spelled {
		canonical, err := dirpath.Canonical(root)
		if err != nil {
			return nil, fmt.Errorf("tempdirs: canonicalize the temporary root %q: %w", root, err)
		}
		if canonical == string(filepath.Separator) {
			return nil, fmt.Errorf("tempdirs: the temporary root %q is the filesystem root, which would make every directory temporary", root)
		}
		if !seen[canonical] {
			seen[canonical] = true
			out = append(out, canonical)
		}
	}
	sort.Strings(out)
	return out, nil
}

// Guard is the check every registration path runs. Build it with New or
// FromEnv; the zero Guard holds no roots and panics when used, because a guard
// that refuses nothing is not a guard.
type Guard struct {
	roots []string
	// testRoot is the canonical directory beneath which a test-run daemon may
	// register temporary directories; empty in every daemon but a test run's.
	testRoot string
}

// New builds the guard for a process whose temporary directory is tmpdir.
//
// testRoot is the TEST-RUN seam and is empty everywhere else. When set it must
// be absolute and must lie STRICTLY BENEATH a temporary root: an exemption
// outside every root exempts nothing and is a misconfiguration, and one that
// IS a root (all of `/tmp`) would reopen the very hole this guard closes.
func New(tmpdir, testRoot string) (Guard, error) {
	roots, err := Roots(tmpdir)
	if err != nil {
		return Guard{}, err
	}
	g := Guard{roots: roots}
	if testRoot == "" {
		return g, nil
	}
	canonical, err := dirpath.Canonical(testRoot)
	if err != nil {
		return Guard{}, fmt.Errorf("tempdirs: the test registration root %q: %w", testRoot, err)
	}
	root, inside := g.rootOf(canonical)
	switch {
	case !inside:
		return Guard{}, fmt.Errorf("tempdirs: the test registration root %s lies inside no temporary root, so it exempts nothing", canonical)
	case root == canonical:
		return Guard{}, fmt.Errorf("tempdirs: the test registration root %s IS the temporary root, which would exempt every temporary folder", canonical)
	}
	g.testRoot = canonical
	return g, nil
}

// FromEnv builds the guard from the daemon's environment: tmpdir is
// os.TempDir(), and getenv reads EnvTestRoot.
//
// THE TEST ROOT IS HONORED ONLY ON A DAEMON THAT MAY NOT CALL THE VENDOR.
// envc.EnvForbidVendorCalls is what every test run sets and what no daemon
// serving a person can run under, so a stray EnvTestRoot on a live daemon is a
// BOOT REFUSAL here rather than a silent exemption.
func FromEnv(contracts envc.Contracts, tmpdir string, getenv func(string) string) (Guard, error) {
	testRoot := getenv(EnvTestRoot)
	if testRoot != "" && !contracts.ForbidVendorCalls() {
		return Guard{}, fmt.Errorf("tempdirs: %s=%q is a test-run seam and is honored only with %s set", EnvTestRoot, testRoot, envc.EnvForbidVendorCalls)
	}
	return New(tmpdir, testRoot)
}

// Check refuses dir when it lies inside a temporary root: it answers an
// *InsideError naming the canonical dir and the root. dir must be absolute,
// and is canonicalized HERE, so no caller can slip a symlinked or case-folded
// spelling past the comparison. A dir beneath the test root is allowed.
func (g Guard) Check(dir string) error {
	if len(g.roots) == 0 {
		panic("tempdirs: Check on a Guard that was never built (use New or FromEnv)")
	}
	canonical, err := dirpath.Canonical(dir)
	if err != nil {
		return fmt.Errorf("tempdirs: canonicalize %q: %w", dir, err)
	}
	root, inside := g.rootOf(canonical)
	if !inside {
		return nil
	}
	if g.testRoot != "" && within(canonical, g.testRoot) {
		return nil
	}
	return &InsideError{Dir: canonical, Root: root}
}

// Built reports whether the guard was built by New or FromEnv. The zero Guard
// is not, and a holder that finds one builds the production guard instead.
func (g Guard) Built() bool { return len(g.roots) > 0 }

// Roots answers the guard's canonical roots, for a record that names them.
func (g Guard) Roots() []string { return append([]string(nil), g.roots...) }

// rootOf answers the temporary root canonical lies inside, if any.
func (g Guard) rootOf(canonical string) (string, bool) {
	for _, root := range g.roots {
		if within(canonical, root) {
			return root, true
		}
	}
	return "", false
}

// within reports whether path equals root or lies beneath it by whole
// components. Both are canonical.
func within(path, root string) bool {
	return path == root || strings.HasPrefix(path, root+string(filepath.Separator))
}
