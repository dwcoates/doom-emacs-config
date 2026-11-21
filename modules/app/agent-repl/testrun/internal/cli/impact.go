package cli

import (
	"fmt"
	"path"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/impact"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/suites"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
)

// ImpactDeps are the seams of a run's shadow-mode impact recording
// (agentrepl/testrun/internal/impact).
type ImpactDeps struct {
	// Store is the host's record store, beside the run history.
	Store impact.Store
	// List lists a build's packages (impact.ExecLister).
	List impact.Lister
	// Toolchain answers the toolchain's facts (impact.ExecToolchain).
	Toolchain func() (map[string]string, error)
	// RepoFiles lists every file of the checkout, repository-relative, that
	// git tracks or does not ignore (reuse.ExecRepoFiles).
	RepoFiles func() ([]string, error)
	// Retention is what the run prunes the store to.
	Retention impact.Retention
}

// recordImpact records the files each top-level test of every recorded chunk
// executed, decides which tests selection WOULD have skipped, and logs every
// decision. It never fails the run: shadow mode must not stand between a
// change and its landing, so a failure is logged as an error and counted
// (testtimes.ImpactErrorsSeriesName) instead. The returned metrics hold one
// entry per suite that recorded, or whose recording failed.
func recordImpact(d Deps, layout suites.Layout, results []run.Result, now time.Time) []testtimes.Impact {
	if d.Impact == nil {
		return nil
	}
	log := d.Log
	passes, err := impactPasses(d, layout, results)
	if err != nil {
		log.Errorf("impact: planning the recording of this run failed: %v", err)
		return []testtimes.Impact{{Errors: 1, Suite: "unplanned"}}
	}
	if len(passes) == 0 {
		log.Infof("impact: no recorded chunk ran in this run (its units were reused or none was selected), so no test was recorded or decided")
		return nil
	}
	var out []testtimes.Impact
	for _, pass := range passes {
		f := impact.Finisher{Repo: layout.Repo, Store: d.Impact.Store, List: d.Impact.List, Now: now, Log: log, Retention: d.Impact.Retention}
		rep, err := f.Finish([]impact.PackagePass{pass})
		m := impactMetrics(pass, rep)
		if err != nil {
			log.Errorf("impact: recording %s:%s failed, so none of its tests was recorded: %v", pass.Suite, pass.Package, err)
			m.Errors = 1
		}
		ran := 0
		for _, c := range pass.Chunks {
			ran += len(c.Tests)
		}
		log.Infof("impact: %s:%s: %d tests ran, %d would be skipped (%.1f CPU-s), %d would run, %d misses, %d unnamed chunk failures, %d incomplete recordings, %d records written, %.1f CPU-s of recording overhead, pruned %d records and %d snapshots",
			pass.Suite, pass.Package, ran, rep.WouldSkip, rep.WouldSkipCPU, len(rep.Decisions)-rep.WouldSkip,
			len(rep.Misses), rep.UnnamedFailures, rep.Incomplete, rep.RecordsWritten, rep.OverheadCPU, rep.Pruned.Records, rep.Pruned.Snapshots)
		out = append(out, m)
	}
	return out
}

func impactMetrics(pass impact.PackagePass, rep impact.Report) testtimes.Impact {
	m := testtimes.Impact{
		Suite: pass.Suite, WouldSkip: rep.WouldSkip, WouldSkipCPU: rep.WouldSkipCPU, Misses: len(rep.Misses),
		RecordsWritten: rep.RecordsWritten, OverheadCPU: rep.OverheadCPU,
	}
	for key, share := range rep.Breadth {
		m.Breadth = append(m.Breadth, testtimes.ImpactBreadth{Test: key.Package + "." + key.Test, Share: share})
	}
	sort.Slice(m.Breadth, func(i, j int) bool { return m.Breadth[i].Test < m.Breadth[j].Test })
	return m
}

// impactPasses groups the recorded chunks among results by package pass and
// plans each pass's recording.
func impactPasses(d Deps, layout suites.Layout, results []run.Result) ([]impact.PackagePass, error) {
	type group struct {
		suite, pkg, tags string
		chunks           []impact.Chunk
	}
	var order []string
	groups := map[string]*group{}
	for _, r := range results {
		c := r.Spec.Impact
		if c == nil || r.Outcome == run.Cancelled {
			continue
		}
		key := c.Suite + "\x1f" + c.Package + "\x1f" + c.Tags
		g := groups[key]
		if g == nil {
			g = &group{suite: c.Suite, pkg: c.Package, tags: c.Tags}
			groups[key] = g
			order = append(order, key)
		}
		g.chunks = append(g.chunks, impactChunk(r))
	}
	sort.Strings(order)
	var passes []impact.PackagePass
	for _, key := range order {
		g := groups[key]
		pass, err := planImpactPass(d, layout, g.suite, g.pkg, g.tags)
		if err != nil {
			return nil, err
		}
		pass.Chunks = g.chunks
		passes = append(passes, pass)
	}
	return passes, nil
}

// impactChunk is one recorded unit's result: its top-level tests (a subtest
// folds into its test, whose own result line says how the whole ended).
func impactChunk(r run.Result) impact.Chunk {
	c := impact.Chunk{Windows: r.Spec.Impact.Windows}
	prefix := r.Spec.Impact.Package + "."
	failedNamed := false
	for _, t := range r.Tests {
		name, ok := strings.CutPrefix(t.Name, prefix)
		if !ok || strings.Contains(name, "/") {
			continue
		}
		c.Tests = append(c.Tests, impact.Test{Name: name, Outcome: t.Outcome, CPU: t.CPU, HasCPU: t.HasCPU})
		if t.Outcome == testtimes.Fail {
			failedNamed = true
		}
	}
	c.UnnamedFailure = r.Outcome == run.Failed && !failedNamed
	return c
}

// planImpactPass is the static plan of one package pass's recording: where
// its module and programs are, which packages coverage tracks, and the data
// the processes its tests boot read.
func planImpactPass(d Deps, layout suites.Layout, suite, pkg, tags string) (impact.PackagePass, error) {
	s, ok := d.Roster(suite)
	if !ok {
		return impact.PackagePass{}, fmt.Errorf("the roster holds no suite %s", suite)
	}
	moduleDir := filepath.Join(layout.Module, filepath.FromSlash(s.Path))
	var programPkgs []string
	for _, name := range s.Programs[pkg] {
		impactName, ok := roster.ImpactProgramOf(name)
		if !ok {
			continue
		}
		p, _ := roster.LookupProgram(impactName)
		if p.Module != s.Path {
			return impact.PackagePass{}, fmt.Errorf("the impact program %s is built in %s, outside the suite %s's module %s", impactName, p.Module, suite, s.Path)
		}
		programPkgs = append(programPkgs, p.Package)
	}
	if len(programPkgs) == 0 {
		return impact.PackagePass{}, fmt.Errorf("the suite %s's package %s runs no program with an impact build", suite, pkg)
	}
	module, required, err := suites.CoveredModules(layout, moduleDir)
	if err != nil {
		return impact.PackagePass{}, err
	}
	covered := append([]string{module}, required...)
	toolchain, err := d.Impact.Toolchain()
	if err != nil {
		return impact.PackagePass{}, err
	}
	facts := map[string]string{"tags": tags, "programs": strings.Join(programPkgs, ",")}
	for k, v := range toolchain {
		facts[k] = v
	}
	repoFiles, err := d.Impact.RepoFiles()
	if err != nil {
		return impact.PackagePass{}, fmt.Errorf("list the checkout's files: %w", err)
	}
	data, err := impactData(layout, s, moduleDir, pkg, tags, repoFiles)
	if err != nil {
		return impact.PackagePass{}, err
	}
	return impact.PackagePass{
		Suite: suite, Package: pkg, Tags: tags,
		ModuleDir: moduleDir, TestPackage: "./" + pkg, ProgramPackages: programPkgs,
		Instrumented: func(importPath string) bool {
			for _, m := range covered {
				if importPath == m || strings.HasPrefix(importPath, m+"/") {
					return true
				}
			}
			return false
		},
		HarnessPrefix: module + "/" + pkg + "/",
		Data:          data,
		Facts:         facts,
	}, nil
}

// impactData lists the repository files every test of the pass depends on
// besides its Go code: what the suite's roster entry says the pass reads
// (Reuse's unit declaration and shared inputs), since the processes the tests
// boot read them at run time and no counter sees it. The module's own Go
// files are left to coverage and the type analysis, and its documentation
// (markdown) is no input of a test's verdict.
func impactData(layout suites.Layout, s roster.Suite, moduleDir, pkg, tags string, repoFiles []string) ([]string, error) {
	if s.Reuse == nil {
		return nil, fmt.Errorf("the suite %s declares no Reuse inputs to take the pass's data from", s.Name)
	}
	unit := pkg
	if tags != "" {
		unit += "[" + tags + "]"
	}
	declared := append(append([]string(nil), s.Reuse.Units[unit]...), s.Reuse.Inputs...)
	moduleRel, err := filepath.Rel(layout.Repo, layout.Module)
	if err != nil {
		return nil, fmt.Errorf("place the module %s in the checkout %s: %w", layout.Module, layout.Repo, err)
	}
	moduleRel = filepath.ToSlash(moduleRel)
	sourceRel, err := filepath.Rel(layout.Repo, moduleDir)
	if err != nil {
		return nil, fmt.Errorf("place %s in the checkout %s: %w", moduleDir, layout.Repo, err)
	}
	sourceRel = filepath.ToSlash(sourceRel) + "/"
	var prefixes, exact []string
	for _, p := range declared {
		rel := path.Join(moduleRel, p)
		if strings.HasPrefix(p, "/") {
			rel = strings.TrimPrefix(p, "/")
		}
		if strings.HasSuffix(p, "/") {
			prefixes = append(prefixes, strings.TrimSuffix(rel, "/")+"/")
		} else {
			exact = append(exact, rel)
		}
	}
	seen := map[string]bool{}
	var out []string
	for _, f := range repoFiles {
		if seen[f] {
			continue
		}
		if strings.HasPrefix(f, sourceRel) && (strings.HasSuffix(f, ".go") || strings.HasSuffix(f, ".md")) {
			continue
		}
		match := false
		for _, e := range exact {
			match = match || f == e
		}
		for _, pre := range prefixes {
			match = match || strings.HasPrefix(f, pre)
		}
		if match {
			seen[f] = true
			out = append(out, f)
		}
	}
	sort.Strings(out)
	return out, nil
}

// impactLayoutOn is whether a run records (suites.Layout.Impact): a host with
// the impact seams does, except in a coverage run, whose programs are built
// for its own reports.
func impactLayoutOn(d Deps, a Args) bool { return d.Impact != nil && !a.Coverage }
