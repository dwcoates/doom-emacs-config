package suites

import (
	"fmt"
	"path/filepath"

	"agentrepl/testrun/internal/govet"
	"agentrepl/testrun/internal/run"
)

// GoVet is one Go module pass's vet (internal/govet): every package of the
// pass vetted by one `go vet` command, the vet unit, whose verdicts each
// package's own check unit reads. Its packages are those whose check units
// remain in the suite's Units (Finish), so a partly reused suite vets only
// the packages whose units run, as its build builds only theirs.
type GoVet struct {
	// ID is the vet unit's ID: "<suite>:vet", or "<suite>:vet[<tags>]" for a
	// tagged pass.
	ID    string
	Suite string
	// Self is the testrun binary the vet unit runs (`testrun vet-module`).
	Self string
	// Options is the vet without its packages.
	Options govet.Options
	// Needs maps every package's check unit, by its ID, to that package.
	Needs map[string]string
}

// ModuleVetID is a module pass's vet unit ID.
func ModuleVetID(suite, tags string) string {
	if tags != "" {
		return suite + ":vet[" + tags + "]"
	}
	return suite + ":vet"
}

// VetCheckID is the check unit of the package whose unit ID prefix is group
// ("<suite>:<rel>", with "[<tags>]" under a tagged pass).
func VetCheckID(group string) string { return group + ":vet" }

// moduleVet is the vet of one pass (tags) over the module in dir, its
// verdicts under the run's scratch directory.
func moduleVet(l Layout, suite, dir, tags string) GoVet {
	pass := suite
	if tags != "" {
		pass += "-" + tags
	}
	return GoVet{
		ID: ModuleVetID(suite, tags), Suite: suite, Self: l.Self,
		Options: govet.Options{Module: dir, ResultDir: filepath.Join(l.Work, "vet", pass), Tags: tags},
	}
}

// Spec is the vet unit over the packages rels.
func (v GoVet) Spec(rels []string) run.Spec {
	o := v.Options
	o.Packages = rels
	return spec(v.ID, v.Suite, o.Module, append([]string{v.Self, "vet-module"}, o.Args()...))
}

// check is the check unit of package rel, whose unit ID prefix is group: it
// waits for the vet and fails with the package's findings. The unit is the
// package's own, so the package's reuse unit owns its vet verdict.
func (v *GoVet) check(group, rel string) run.Spec {
	id := VetCheckID(group)
	if v.Needs == nil {
		v.Needs = map[string]string{}
	}
	if old, ok := v.Needs[id]; ok && old != rel {
		panic(fmt.Sprintf("suites: vet check %s is both %s's and %s's", id, old, rel))
	}
	v.Needs[id] = rel
	sp := spec(id, v.Suite, v.Options.Module, []string{v.Self, "vet-check", govet.ResultPath(v.Options.ResultDir, rel)})
	sp.Deps = []string{v.ID}
	return sp
}
