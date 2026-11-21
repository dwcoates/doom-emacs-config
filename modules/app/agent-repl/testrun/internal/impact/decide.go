package impact

import (
	"fmt"
	"strings"
)

// Tree answers the blob hash of a repository-relative path in the tree under
// test, and whether the path exists there.
type Tree func(path string) (hash string, exists bool, err error)

// Decision is what selection WOULD do with one test. In shadow mode it is
// logged and measured, never acted on.
type Decision struct {
	Key       TestKey
	WouldSkip bool
	// Reason says why: the record that matched, or what stood in the way.
	Reason string
}

// maxChangedNamed bounds how many changed dependencies a reason names.
const maxChangedNamed = 3

// Decide says whether key WOULD be skipped on tree: only when a PASSING record
// exists whose facts equal facts and whose every dependency has, in tree,
// the blob hash it had when the record was written. A test with no record,
// with only failing records, or whose newest passing record has a changed
// dependency would run.
func (s Store) Decide(key TestKey, facts map[string]string, tree Tree) (Decision, error) {
	return s.decideWith(snapshots{}, key, facts, tree)
}

func (s Store) decideWith(cache snapshots, key TestKey, facts map[string]string, tree Tree) (Decision, error) {
	records, err := s.load(cache, key)
	if err != nil {
		return Decision{}, err
	}
	return decide(key, records, facts, tree)
}

func decide(key TestKey, records []Loaded, facts map[string]string, tree Tree) (Decision, error) {
	if len(records) == 0 {
		return Decision{Key: key, Reason: "no record: a new test, or one no run has recorded"}, nil
	}
	want := FactsDigest(facts)
	var closest []string
	passes, factsChanged := 0, 0
	for _, l := range records {
		if l.Record.Outcome != Pass {
			continue
		}
		passes++
		if FactsDigest(l.Record.Facts) != want {
			factsChanged++
			continue
		}
		paths, hashes := l.Deps()
		var changed []string
		for i, p := range paths {
			h, exists, err := tree(p)
			if err != nil {
				return Decision{}, fmt.Errorf("impact: hash %s for %s: %w", p, key, err)
			}
			switch {
			case !exists:
				changed = append(changed, p+" (gone)")
			case h != hashes[i]:
				changed = append(changed, p)
			}
		}
		if len(changed) == 0 {
			return Decision{Key: key, WouldSkip: true, Reason: fmt.Sprintf("passed at %s on this exact set of %d dependencies", l.Record.At.UTC().Format("2006-01-02T15:04:05Z"), len(paths))}, nil
		}
		if closest == nil || len(changed) < len(closest) {
			closest = changed
		}
	}
	switch {
	case passes == 0:
		return Decision{Key: key, Reason: "no passing record: it last failed"}, nil
	case closest == nil:
		return Decision{Key: key, Reason: fmt.Sprintf("its passing records' facts (toolchain, tags) differ from this run's in %d of %d", factsChanged, passes)}, nil
	}
	named := closest
	more := ""
	if len(named) > maxChangedNamed {
		more = fmt.Sprintf(" and %d more", len(named)-maxChangedNamed)
		named = named[:maxChangedNamed]
	}
	return Decision{Key: key, Reason: "a dependency changed since its nearest passing record: " + strings.Join(named, ", ") + more}, nil
}

// Decider decides many tests against one store and one tree, reading each
// snapshot once.
type Decider struct {
	Store Store
	Facts map[string]string
	Tree  Tree
	cache snapshots
}

// NewDecider is a Decider over store, facts and tree.
func NewDecider(store Store, facts map[string]string, tree Tree) *Decider {
	return &Decider{Store: store, Facts: facts, Tree: tree, cache: snapshots{}}
}

// Decide is Store.Decide for key.
func (d *Decider) Decide(key TestKey) (Decision, error) {
	return d.Store.decideWith(d.cache, key, d.Facts, d.Tree)
}

// DecideEntry decides one entry a run listed by NAME. A name that is no
// top-level test (a subtest path, an empty name, a package or suite level
// failure entry) is always a test that WOULD run: nothing records it.
func (d *Decider) DecideEntry(suite, pkg, tags, name string) (Decision, error) {
	key := TestKey{Suite: suite, Package: pkg, Tags: tags, Test: name}
	if name == "" || strings.Contains(name, "/") {
		return Decision{Key: key, Reason: "not a top-level test, so no record covers it"}, nil
	}
	return d.Decide(key)
}
