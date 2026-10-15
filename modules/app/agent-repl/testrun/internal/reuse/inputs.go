// Package reuse lets a run skip a suite whose every input is byte-identical to
// one an earlier run passed whole.
//
// A system's unit and integration suites run every OTHER system as a mock
// (AGENTS.md, "A system's integration tests run that system alone"), so what
// they can observe is exactly what they read: their own sources, the code they
// import, the files they open, the environment they are given and the
// toolchain that builds and runs them. The suite's KEY is a hash of all of it.
// A pass record stored under that key says "these bytes passed", which stays
// true on any branch, in any worktree, after any rebase and in any queue
// order, so reusing it needs no walk of history.
//
// THE INPUT SET IS ENFORCED, NOT TRUSTED. Every unit of a keyed suite runs
// under a seatbelt profile (Profile) that denies every file read under the
// checkout except the suite's input set, in the unit's whole process tree, and
// under an environment holding only the variables the key covers (Confine). A
// test that reads anything else fails, so an input left off the set is a loud
// failure, never a stale pass.
package reuse

import (
	"fmt"
	"path"
	"sort"
	"strings"
)

// InputSet is what one suite reads from the checkout, every path relative to
// the repository root, slash-separated.
type InputSet struct {
	// files are regular files the suite reads; their contents are hashed.
	files map[string]bool
	// dirs are directories the suite lists; the names and kinds of their
	// direct entries are hashed, and the entries themselves are not readable
	// unless placed some other way.
	dirs map[string]bool
	// flats are directories the suite reads FLAT: their listing and every
	// file directly in them, whatever it is named, but none of their
	// subdirectories. A Go package directory is one.
	flats map[string]bool
	// trees are subtrees the suite may read whole; every entry beneath one is
	// hashed.
	trees map[string]bool
	// generated are subtrees the suite's own run builds or installs before it
	// reads them. They are readable and never hashed: what is in them is a
	// function of hashed inputs (see roster.Reuse.Generated).
	generated map[string]bool
}

// NewInputSet is an empty set.
func NewInputSet() *InputSet {
	return &InputSet{files: map[string]bool{}, dirs: map[string]bool{}, flats: map[string]bool{}, trees: map[string]bool{}, generated: map[string]bool{}}
}

// clean checks one repository-relative path and normalizes it. A path that
// escapes the checkout or is absolute is a caller's bug.
func clean(p string) (string, error) {
	c := path.Clean(p)
	if p == "" || path.IsAbs(c) || c == ".." || strings.HasPrefix(c, "../") {
		return "", fmt.Errorf("reuse: %q is not a path inside the checkout", p)
	}
	return c, nil
}

// AddFile places one file.
func (s *InputSet) AddFile(p string) error { return s.add(s.files, p) }

// AddDir places one directory's listing.
func (s *InputSet) AddDir(p string) error { return s.add(s.dirs, p) }

// AddFlatDir places a directory's listing and every file directly in it.
func (s *InputSet) AddFlatDir(p string) error { return s.add(s.flats, p) }

// AddTree places a whole subtree.
func (s *InputSet) AddTree(p string) error { return s.add(s.trees, p) }

// AddGenerated places a subtree the suite's own run produces.
func (s *InputSet) AddGenerated(p string) error { return s.add(s.generated, p) }

func (s *InputSet) add(m map[string]bool, p string) error {
	c, err := clean(p)
	if err != nil {
		return err
	}
	m[c] = true
	return nil
}

// under reports whether p is root or beneath it.
func under(p, root string) bool {
	return root == "." || p == root || strings.HasPrefix(p, root+"/")
}

// coveredBy reports whether p sits in one of the subtrees of m.
func coveredBy(p string, m map[string]bool) bool {
	for root := range m {
		if under(p, root) {
			return true
		}
	}
	return false
}

// Contains reports whether the suite may read the file at p: it is placed
// itself, or it sits in a placed subtree.
func (s *InputSet) Contains(p string) bool {
	return s.files[p] || s.flats[path.Dir(p)] || coveredBy(p, s.trees) || coveredBy(p, s.generated)
}

// Size is how many paths the set places, before subtrees are expanded.
func (s *InputSet) Size() int {
	return len(s.files) + len(s.dirs) + len(s.flats) + len(s.trees) + len(s.generated)
}

// Files, Dirs, FlatDirs, Trees and Generated list each kind of placement,
// sorted, with anything a broader placement already covers left out.
func (s *InputSet) Files() []string {
	var out []string
	for _, f := range s.sortedOutsideTrees(s.files) {
		if !s.flats[path.Dir(f)] {
			out = append(out, f)
		}
	}
	return out
}

// Dirs lists the directories placed for their listing alone.
func (s *InputSet) Dirs() []string {
	var out []string
	for _, d := range s.sortedOutsideTrees(s.dirs) {
		if !s.flats[d] {
			out = append(out, d)
		}
	}
	return out
}

// FlatDirs lists the directories placed flat.
func (s *InputSet) FlatDirs() []string { return s.sortedOutsideTrees(s.flats) }

// Trees lists the hashed subtrees that no other placed subtree contains.
func (s *InputSet) Trees() []string { return outermost(s.trees, s.generated) }

// Generated lists the unhashed subtrees that no hashed subtree contains.
func (s *InputSet) Generated() []string { return outermost(s.generated, nil) }

func (s *InputSet) sortedOutsideTrees(m map[string]bool) []string {
	var out []string
	for p := range m {
		if !coveredBy(p, s.trees) && !coveredBy(p, s.generated) {
			out = append(out, p)
		}
	}
	sort.Strings(out)
	return out
}

// outermost lists the roots of m that no OTHER root of m, and no root of
// shadow, contains.
func outermost(m, shadow map[string]bool) []string {
	var out []string
	for p := range m {
		inner := false
		for q := range m {
			if q != p && under(p, q) {
				inner = true
				break
			}
		}
		if !inner && !coveredBy(p, shadow) {
			out = append(out, p)
		}
	}
	sort.Strings(out)
	return out
}

// Overlap names a hashed placement inside a generated subtree: a path cannot
// be both something the run produces and something the key fixes, so such a
// set is a roster bug. (A generated subtree INSIDE a hashed one is fine: the
// hash walk skips it.)
func (s *InputSet) Overlap() error {
	for g := range s.generated {
		for _, m := range []map[string]bool{s.files, s.dirs, s.flats, s.trees} {
			for p := range m {
				if under(p, g) {
					return fmt.Errorf("reuse: %s is both generated by the run and a hashed input", p)
				}
			}
		}
	}
	return nil
}
