// Package buildid names every component's BUILD by content hash — the one
// identity a deploy compares a running process against (owner design,
// 2026-09-23: staleness is decided by a content hash per component, never by
// "did this build change the file").
//
//   - A binary or a bundle is the lowercase hex SHA-256 of its bytes
//     (buildreport.HashFile, shared with the services that report their own).
//   - The webapp is the content hash Vite gives its entry bundle: the <hash> of
//     `assets/index-<hash>.js` as index.html names it. A webview reports the
//     same value from its own entry tag.
//   - Elisp is the module-set hash WatchDaemonEmacs.elisp_build states: the
//     SHA-256 of `<module>\t<sha256 of lisp/<module>.el>\n` lines in config.el's
//     load order. proto/vocab/elisp-build.json holds Emacs and this package to
//     one answer.
//
// The ShimBundle guard is here too, because the shim's build is the one a
// SPAWN states: the daemon hashes the bundle it is about to run and the shim
// reports that value back, so the hash and the process must be the same bytes.
package buildid

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"

	"agentrepl/logging/buildreport"
)

// File answers a file's content hash.
func File(path string) (string, error) { return buildreport.HashFile(path) }

// webappEntry is the entry tag's pattern: the same one bin/build-frontend.sh's
// write_webapp_build_id and the webapp's own reader use.
var webappEntry = regexp.MustCompile(`src="/assets/index-([A-Za-z0-9_-]+)\.js"`)

// Webapp answers the webapp build a built dist serves: the entry bundle's
// content hash, read from its index.html. An index naming no entry bundle is
// an error, never an empty build.
func Webapp(dist string) (string, error) {
	index := filepath.Join(dist, "index.html")
	raw, err := os.ReadFile(index)
	if err != nil {
		return "", fmt.Errorf("buildid: read the webapp entry %s: %w", index, err)
	}
	match := webappEntry.FindSubmatch(raw)
	if match == nil {
		return "", fmt.Errorf("buildid: %s names no assets/index-<hash>.js entry bundle", index)
	}
	return string(match[1]), nil
}

// loadModule is config.el's loader form, anchored at the start of a line
// exactly as the loader's own top-level calls are written.
var loadModule = regexp.MustCompile(`^\(agent-repl--load-module "([^"]+)"\)`)

// ElispModules answers the module names config.el loads, in load order.
func ElispModules(moduleRoot string) ([]string, error) {
	config := filepath.Join(moduleRoot, "config.el")
	raw, err := os.ReadFile(config)
	if err != nil {
		return nil, fmt.Errorf("buildid: read the elisp loader %s: %w", config, err)
	}
	var modules []string
	scanner := bufio.NewScanner(bytes.NewReader(raw))
	for scanner.Scan() {
		if match := loadModule.FindStringSubmatch(scanner.Text()); match != nil {
			modules = append(modules, match[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("buildid: scan %s: %w", config, err)
	}
	if len(modules) == 0 {
		return nil, fmt.Errorf("buildid: %s loads no module through agent-repl--load-module", config)
	}
	return modules, nil
}

// Module is one module's name and its file's content hash.
type Module struct {
	Name string
	Hash string
}

// ElispOf is the module-set hash of modules already hashed, in load order.
func ElispOf(modules []Module) string {
	var lines strings.Builder
	for _, m := range modules {
		lines.WriteString(m.Name + "\t" + m.Hash + "\n")
	}
	sum := sha256.Sum256([]byte(lines.String()))
	return hex.EncodeToString(sum[:])
}

// Elisp answers the checkout's elisp build: every module config.el loads,
// hashed from lisp/<module>.el, in load order. A module the loader names whose
// file is absent contributes nothing — exactly as it contributes nothing to
// what Emacs loads.
func Elisp(moduleRoot string) (string, error) {
	names, err := ElispModules(moduleRoot)
	if err != nil {
		return "", err
	}
	modules := make([]Module, 0, len(names))
	for _, name := range names {
		path := filepath.Join(moduleRoot, "lisp", name+".el")
		hash, err := File(path)
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return "", err
		}
		modules = append(modules, Module{Name: name, Hash: hash})
	}
	return ElispOf(modules), nil
}

// EnvShimBuild is the shim build stated for a checkout with NO bundle on disk:
// every test harness, whose fake shim has no bundle at all. A bundle on disk
// always wins; with neither, the build is unresolvable and a spawn refuses.
const EnvShimBuild = "SHIM_BUILD_SHA"

// ShimBundle is the installed shim bundle, guarded so a spawn and an install
// can never interleave.
//
// THE BUILD A SHIM REPORTS IS THE BUILD ITS SPAWNER HASHED, so the bytes node
// loads must be the bytes that were hashed. A spawn HOLDS the bundle from the
// hash until the shim has answered (by which time node has read the file),
// and an install REPLACES it only under the exclusive side of the same lock.
type ShimBundle struct {
	path     string
	override string
	mu       sync.RWMutex
}

// NewShimBundle guards the bundle at path. override is EnvShimBuild's value,
// used only while no bundle exists at path.
func NewShimBundle(path, override string) *ShimBundle {
	return &ShimBundle{path: path, override: strings.TrimSpace(override)}
}

// Path answers the guarded bundle's path.
func (b *ShimBundle) Path() string { return b.path }

// Build answers the installed bundle's build without holding it.
func (b *ShimBundle) Build() (string, error) {
	b.mu.RLock()
	defer b.mu.RUnlock()
	return b.buildLocked()
}

// Hold answers the installed bundle's build and HOLDS it installed until
// release is called. The caller spawns from it and releases once the spawned
// shim has answered. release is always safe to call exactly once, error or not.
func (b *ShimBundle) Hold() (build string, release func(), err error) {
	b.mu.RLock()
	var once sync.Once
	release = func() { once.Do(b.mu.RUnlock) }
	build, err = b.buildLocked()
	if err != nil {
		release()
		return "", func() {}, err
	}
	return build, release, nil
}

// Replace runs install with the bundle held EXCLUSIVELY, so no spawn hashes
// one set of bytes and runs another.
func (b *ShimBundle) Replace(install func() error) error {
	b.mu.Lock()
	defer b.mu.Unlock()
	return install()
}

func (b *ShimBundle) buildLocked() (string, error) {
	build, err := File(b.path)
	if err == nil {
		return build, nil
	}
	if errors.Is(err, fs.ErrNotExist) && b.override != "" {
		return b.override, nil
	}
	if errors.Is(err, fs.ErrNotExist) {
		return "", fmt.Errorf("buildid: the shim build is unresolvable: %s does not exist and %s is unset", b.path, EnvShimBuild)
	}
	return "", err
}
