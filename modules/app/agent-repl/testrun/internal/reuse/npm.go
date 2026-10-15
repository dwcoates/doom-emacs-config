package reuse

import (
	"encoding/json"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"sort"
)

// A node_modules tree is generated: the run installs it (bin/ensure-deps.sh)
// and reads it, and the key covers what decides it, the package's
// package-lock.json and the node toolchain. That holds only when the tree was
// installed FROM that lock. npm writes what it installed to
// node_modules/.package-lock.json, so after a run the two are compared, and a
// suite whose tree disagrees with its lock passed on packages its key does not
// name: it records nothing.

// npmLock is the part of a lockfile the comparison reads.
type npmLock struct {
	Packages map[string]npmPackage `json:"packages"`
}

type npmPackage struct {
	Version   string `json:"version"`
	Resolved  string `json:"resolved"`
	Integrity string `json:"integrity"`
	Optional  bool   `json:"optional"`
	Dev       bool   `json:"dev"`
	Link      bool   `json:"link"`
}

// NodeModulesMismatch compares the installed tree at nodeModules
// (repository-relative, its base name node_modules) against its package's
// package-lock.json, and names the first package that differs, or "" when the
// tree is the lock's. A package the lock marks optional may be absent (npm
// skips another platform's binaries), but never different.
func NodeModulesMismatch(repo, nodeModules string) (string, error) {
	pkgDir := path.Dir(nodeModules)
	lock, err := readNPMLock(filepath.Join(repo, filepath.FromSlash(pkgDir), "package-lock.json"))
	if err != nil {
		return "", err
	}
	installed, err := readNPMLock(filepath.Join(repo, filepath.FromSlash(nodeModules), ".package-lock.json"))
	if err != nil {
		return "", err
	}
	var names []string
	for name := range lock.Packages {
		names = append(names, name)
	}
	for name := range installed.Packages {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		if name == "" {
			// The package's own root entry is not installed.
			continue
		}
		want, inLock := lock.Packages[name]
		got, inTree := installed.Packages[name]
		switch {
		case !inLock:
			return fmt.Sprintf("%s is installed but not in package-lock.json", name), nil
		case !inTree && !want.Optional:
			return fmt.Sprintf("%s is in package-lock.json but not installed", name), nil
		case inTree && (got.Version != want.Version || got.Integrity != want.Integrity || got.Resolved != want.Resolved):
			return fmt.Sprintf("%s is installed at %s but package-lock.json names %s", name, got.Version, want.Version), nil
		}
	}
	return "", nil
}

func readNPMLock(p string) (npmLock, error) {
	data, err := os.ReadFile(p)
	if err != nil {
		return npmLock{}, fmt.Errorf("reuse: read %s: %w", p, err)
	}
	var l npmLock
	if err := json.Unmarshal(data, &l); err != nil {
		return npmLock{}, fmt.Errorf("reuse: %s is not an npm lockfile: %w", p, err)
	}
	return l, nil
}
