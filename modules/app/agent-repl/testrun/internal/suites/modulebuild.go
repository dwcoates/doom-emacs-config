package suites

import (
	"fmt"
	"sort"

	"agentrepl/testrun/internal/gobuild"
	"agentrepl/testrun/internal/run"
)

// BuildSlots is a module build's width, the widest unit the governor admits
// first (cores.FirstUnitSeats): the build's go command is pinned to it by
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
	// Needs maps every unit that runs a package's binary (a split's group,
	// or an atomic unit's ID) to that package.
	Needs map[string]string
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
	sp := spec(b.ID, b.Suite, o.Module, append([]string{o.Self, "build-tests"}, o.Args()...))
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
	for _, sp := range u.Splits {
		present[sp.Group] = true
	}
	var builds []run.Spec
	for _, b := range u.GoBuilds {
		seen := map[string]bool{}
		var rels []string
		for unit, rel := range b.Needs {
			if present[unit] && !seen[rel] {
				seen[rel] = true
				rels = append(rels, rel)
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

// need records that unit runs package rel's binary.
func (b *GoBuild) need(unit, rel string) {
	if b.Needs == nil {
		b.Needs = map[string]string{}
	}
	if old, ok := b.Needs[unit]; ok && old != rel {
		panic(fmt.Sprintf("suites: unit %s runs both %s's and %s's binary", unit, old, rel))
	}
	b.Needs[unit] = rel
}
