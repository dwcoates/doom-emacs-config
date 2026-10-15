package reuse

import (
	"fmt"
	"path"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// Profile is the seatbelt profile (sandbox-exec(1)) a keyed suite's units run
// under: every read beneath each of denyRoots is refused, except the suite's
// input set.
//
// denyRoots are absolute, symlink-free paths: the checkout the suite tests and
// every other checkout of the same repository on the host, so a test cannot
// reach around its own tree to a sibling's copy of a file it did not declare.
// repo is the checkout the input set is relative to, also symlink-free (the
// kernel matches the resolved path, so /tmp must already be /private/tmp).
//
// What the profile allows, beyond the denied roots, is everything: the
// toolchain, the module and build caches, the run's temp directories and the
// host's slot locks all live outside the checkouts.
//
// Rules are matched last-wins, so the allowances come after the denials.
func Profile(repo string, denyRoots []string, set *InputSet) (string, error) {
	for _, p := range append([]string{repo}, denyRoots...) {
		if !filepath.IsAbs(p) {
			return "", fmt.Errorf("reuse: the profile root %q is not absolute", p)
		}
	}
	var b strings.Builder
	b.WriteString("(version 1)\n(allow default)\n")
	for _, tool := range unsandboxedTools {
		fmt.Fprintf(&b, "(allow process-exec (literal %s) (with no-sandbox))\n", quote(tool))
	}
	roots := append([]string(nil), denyRoots...)
	sort.Strings(roots)
	for _, r := range roots {
		fmt.Fprintf(&b, "(deny file-read* (subpath %s))\n", quote(r))
	}
	abs := func(p string) string { return filepath.Join(repo, filepath.FromSlash(p)) }
	ancestors := map[string]bool{".": true}
	note := func(p string) {
		for d := path.Dir(p); d != "."; d = path.Dir(d) {
			ancestors[d] = true
		}
	}
	for _, p := range set.Files() {
		fmt.Fprintf(&b, "(allow file-read* (literal %s))\n", quote(abs(p)))
		note(p)
	}
	// A flat directory is ONE pattern for every entry in it, not one literal
	// per file: a profile of a few thousand literals takes the kernel about a
	// second to compile on every unit start, a few hundred patterns ~50ms.
	// The pattern also matches the directory's subdirectories, so a second
	// rule refuses reading those as directories again; a placed subtree
	// beneath one (its testdata) is allowed after, and the last match wins.
	for _, p := range set.FlatDirs() {
		entries, err := entryPattern(abs(p))
		if err != nil {
			return "", err
		}
		// The directory itself may be a subdirectory of a flat directory
		// before it (FlatDirs is sorted, so a parent comes first), whose
		// refusal names file-read-data: this allowance names it too.
		fmt.Fprintf(&b, "(allow file-read* (literal %s))\n", quote(abs(p)))
		fmt.Fprintf(&b, "(allow file-read-data (literal %s))\n", quote(abs(p)))
		fmt.Fprintf(&b, "(allow file-read* (regex %s))\n", entries)
		fmt.Fprintf(&b, "(deny file-read-data (require-all (regex %s) (vnode-type DIRECTORY)))\n", entries)
		note(p)
	}
	// A listed directory or a subtree may sit directly in a flat directory (a
	// module root whose subdirectory holds only packages, a package's
	// testdata), where the refusal above names file-read-data. The kernel lets
	// a rule naming an operation outrank one naming file-read* whatever their
	// order, so these allowances name file-read-data too.
	for _, p := range set.Dirs() {
		fmt.Fprintf(&b, "(allow file-read* (literal %s))\n", quote(abs(p)))
		fmt.Fprintf(&b, "(allow file-read-data (literal %s))\n", quote(abs(p)))
		note(p)
	}
	for _, p := range append(set.Trees(), set.Generated()...) {
		fmt.Fprintf(&b, "(allow file-read* (subpath %s))\n", quote(abs(p)))
		fmt.Fprintf(&b, "(allow file-read-data (subpath %s))\n", quote(abs(p)))
		note(p)
	}
	// Reaching a placed path means looking up every directory above it, which
	// needs their metadata and nothing more: their listings stay refused.
	var dirs []string
	for d := range ancestors {
		dirs = append(dirs, d)
	}
	sort.Strings(dirs)
	for _, d := range dirs {
		fmt.Fprintf(&b, "(allow file-read-metadata (literal %s))\n", quote(abs(d)))
	}
	// The ancestors of the checkout itself are outside every denied root
	// unless one checkout sits inside another, so they are allowed too.
	for d := filepath.Dir(repo); ; d = filepath.Dir(d) {
		fmt.Fprintf(&b, "(allow file-read-metadata (literal %s))\n", quote(d))
		if d == filepath.Dir(d) {
			break
		}
	}
	return b.String(), nil
}

// unsandboxedTools are the setuid programs a confined test may still run. The
// kernel refuses to exec a setuid binary inside a sandbox, so each runs
// outside it, and each is listed only because it reads no file of a checkout
// on its caller's behalf: ps lists processes (the integration harness reaps
// its runs with it).
var unsandboxedTools = []string{"/bin/ps"}

// entryPattern is the profile regex matching every direct entry of dir. The
// pattern is a raw #"..." literal, which cannot hold a double quote, so a
// directory whose path has one is refused rather than mis-spelled.
func entryPattern(dir string) (string, error) {
	if strings.ContainsRune(dir, '"') {
		return "", fmt.Errorf("reuse: %q has a double quote, which a profile pattern cannot spell", dir)
	}
	return `#"^` + regexp.QuoteMeta(dir) + `/[^/]+$"`, nil
}

// quote spells s as a profile string literal.
func quote(s string) string {
	r := strings.NewReplacer(`\`, `\\`, `"`, `\"`)
	return `"` + r.Replace(s) + `"`
}
