package reuse

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"sort"
)

// ERT LOADS. Every ERT file loads the test helpers first, and the helpers
// load the module's config.el, which loads every lisp module it names; a
// file may load a sibling beside them. The scan reads those names; the
// file's profile refuses whatever else it reads, so a miss fails loudly.

// ertHelpers is the file every ERT unit loads before its own
// (testrun/ert/driver.el).
const ertHelpers = "test-helpers.el"

// loadModulePattern finds config.el's `(agent-repl--load-module "NAME")`.
var loadModulePattern = regexp.MustCompile(`\(agent-repl--load-module\s+"([^"]+)"\)`)

// elFileLiteral finds a quoted lisp file name.
var elFileLiteral = regexp.MustCompile(`"([A-Za-z0-9_.-]+\.el)"`)

// ERTModuleSources is what every ERT file of the suite in lispRel
// (repository-relative) loads before its own: the helpers, the module's
// config.el beside lispRel, and every module config.el loads, sorted.
func ERTModuleSources(repo, lispRel string) ([]string, error) {
	config := path.Join(path.Dir(lispRel), "config.el")
	src, err := os.ReadFile(filepath.Join(repo, filepath.FromSlash(config)))
	if err != nil {
		return nil, fmt.Errorf("reuse: read %s: %w", config, err)
	}
	out := []string{config}
	for _, m := range loadModulePattern.FindAllSubmatch(src, -1) {
		f := path.Join(lispRel, string(m[1])+".el")
		if _, err := os.Stat(filepath.Join(repo, filepath.FromSlash(f))); err != nil {
			return nil, fmt.Errorf("reuse: %s loads the module %s: %w", config, m[1], err)
		}
		out = append(out, f)
	}
	helpers, err := ERTFileLoads(repo, lispRel, ertHelpers)
	if err != nil {
		return nil, err
	}
	out = append(out, helpers...)
	sort.Strings(out)
	return dedupe(out), nil
}

// ERTFileLoads is file (a name in lispRel) and every lisp file of lispRel it
// names, sorted.
func ERTFileLoads(repo, lispRel, file string) ([]string, error) {
	p := path.Join(lispRel, file)
	src, err := os.ReadFile(filepath.Join(repo, filepath.FromSlash(p)))
	if err != nil {
		return nil, fmt.Errorf("reuse: read %s: %w", p, err)
	}
	out := []string{p}
	for _, m := range elFileLiteral.FindAllSubmatch(src, -1) {
		f := path.Join(lispRel, string(m[1]))
		if info, err := os.Stat(filepath.Join(repo, filepath.FromSlash(f))); err == nil && info.Mode().IsRegular() {
			out = append(out, f)
		}
	}
	sort.Strings(out)
	return dedupe(out), nil
}

// dedupe drops repeats from a sorted list.
func dedupe(sorted []string) []string {
	var out []string
	for i, s := range sorted {
		if i == 0 || s != sorted[i-1] {
			out = append(out, s)
		}
	}
	return out
}
