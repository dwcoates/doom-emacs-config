package suites

import (
	"fmt"
	"sort"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
)

// BuildSlots is a module build's width, the widest unit's
// (cores.FirstUnitSeats): the build's go command is pinned to it by
// WidePinnedEnv.
const BuildSlots = 2

// GoBuild is one Go module pass's build unit (internal/gobuild): every
// unit-test binary of the pass, built by one `go test` command. Its packages
// are those whose units remain in the suite's Units (Finish), so a narrowed
// or partly reused suite builds only what it runs.
type GoBuild struct {
	// ID is the build unit's ID: "<suite>:build", or "<suite>:build[<tags>]"
	// for a tagged pass.
	ID    string
	Suite string
	// Options is the build without its packages.
	Options gobuild.Options
	// Needs maps every atomic unit that runs a package's binary, by its ID,
	// to that package. A split names its own (Builds): a selection renames a
	// split's group (one subtest path's own group), so a group is never what
	// ties a split to its binary.
	Needs map[string]string
}

// Builds answers the package of sp whose binary b builds: sp's own Package
// when sp is one of b's pass (its suite and tags), and false for any other
// split (another pass's, or one that runs no Go binary).
func (b GoBuild) Builds(sp Split) (string, bool) {
	if sp.Package == "" || sp.Suite != b.Suite || sp.Tags != b.Options.Tags {
		return "", false
	}
	return sp.Package, true
}

// ModuleBuildID is a module pass's build unit ID.
func ModuleBuildID(suite, tags string) string {
	if tags != "" {
		return suite + ":build[" + tags + "]"
	}
	return suite + ":build"
}

// Spec is the build unit over the packages rels, held to its width.
func (b GoBuild) Spec(rels []string) run.Spec {
	o := b.Options
	o.Packages = rels
	return pinnedBuild(spec(b.ID, b.Suite, o.Module, append([]string{o.Self, "build-tests"}, o.Args()...)))
}

// pinnedBuild holds a unit whose go command builds and links (a module's
// test binaries, a roster program) to BuildSlots, pinned there by
// WidePinnedEnv. MEASURED (merge gates of 2026-11-20): a program build held
// to one slot used 1.17-1.45 cores, the go command compiling one package at
// a time while its compiler and linker ran on both of their GOMAXPROCS.
func pinnedBuild(sp run.Spec) run.Spec {
	sp.Env = WidePinnedEnv(BuildSlots)
	sp.Slots = BuildSlots
	return sp
}

// Finish places each of u's module builds over exactly the packages whose
// units u still holds, replacing any build unit placed before; a build none
// of whose packages remain is dropped. Every step that drops units from a
// suite (a selection's narrowing, per-package reuse) ends with Finish.
func (u Units) Finish() Units {
	ids := map[string]bool{}
	for _, b := range u.GoBuilds {
		ids[b.ID] = true
	}
	present := map[string]bool{}
	var atomic []run.Spec
	for _, a := range u.Atomic {
		if ids[a.ID] {
			continue
		}
		atomic = append(atomic, a)
		present[a.ID] = true
	}
	var builds []run.Spec
	for _, b := range u.GoBuilds {
		seen := map[string]bool{}
		var rels []string
		add := func(rel string) {
			if !seen[rel] {
				seen[rel] = true
				rels = append(rels, rel)
			}
		}
		for unit, rel := range b.Needs {
			if present[unit] {
				add(rel)
			}
		}
		for _, sp := range u.Splits {
			if rel, ok := b.Builds(sp); ok {
				add(rel)
			}
		}
		if len(rels) == 0 {
			continue
		}
		sort.Strings(rels)
		builds = append(builds, b.Spec(rels))
	}
	u.Atomic = append(atomic, builds...)
	return u
}

// need records that the atomic unit runs package rel's binary.
func (b *GoBuild) need(unit, rel string) {
	if b.Needs == nil {
		b.Needs = map[string]string{}
	}
	if old, ok := b.Needs[unit]; ok && old != rel {
		panic(fmt.Sprintf("suites: unit %s runs both %s's and %s's binary", unit, old, rel))
	}
	b.Needs[unit] = rel
}
