package suites

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/command"
	"agentrepl/testrun/internal/cover"
	"agentrepl/testrun/internal/gotest"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/testtimes"
	"agentrepl/testrun/roster"
)

// vitestFiles lists a package's unit test files under its default config, as
// paths relative to the package, after making its npm deps satisfy the
// lockfile through the one sanctioned path (bin/ensure-deps.sh).
func vitestFiles(module, dir string) ([]string, error) {
	return vitestFilesUnder(module, dir, "")
}

// vitestFilesUnder is vitestFiles under CONFIG ("" is the default config).
func vitestFilesUnder(module, dir, config string) ([]string, error) {
	if err := runEnsure(fmt.Sprintf("ensure the npm deps of %s", dir), filepath.Join(module, "bin", "ensure-deps.sh"), dir); err != nil {
		return nil, err
	}
	args := []string{"vitest", "list", "--filesOnly", "--json"}
	if config != "" {
		args = append(args, "--config", config)
	}
	list := exec.Command("npx", args...)
	list.Dir = dir
	list.Env = os.Environ()
	out, err := command.Output(list)
	if err != nil {
		return nil, fmt.Errorf("suites: list the vitest files: %w", err)
	}
	return parseVitestList(dir, out)
}

// parseVitestList reads `vitest list --filesOnly --json`: every test file,
// answered relative to the package dir and sorted.
func parseVitestList(dir string, out []byte) ([]string, error) {
	var entries []struct {
		File string `json:"file"`
	}
	if err := json.Unmarshal(out, &entries); err != nil {
		return nil, fmt.Errorf("suites: vitest list in %s printed no file list: %w", dir, err)
	}
	var files []string
	for _, e := range entries {
		rel, err := under(dir, e.File)
		if err != nil {
			return nil, err
		}
		files = append(files, rel)
	}
	sort.Strings(files)
	return files, nil
}

// TypecheckSlots is a vitest suite's typecheck width.
const TypecheckSlots = 2

// KnipSlots is a vitest suite's knip width. MEASURED (2026-11-14, Low Power,
// three runs over the webapp and the shim): user+sys over wall came to 1.5-1.7
// cores, node's compiler thread and GC as tsc's, so it holds two slots.
const KnipSlots = 2

// vitestUnits is the typecheck, knip and the test files split into chunks. Coverage
// adds chunk blobs and their merged report only when explicitly requested.
func vitestUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	files, err := vitestFiles(l.Module, dir)
	if err != nil {
		return Units{}, err
	}
	if len(files) == 0 {
		return Units{}, fmt.Errorf("suites: %s lists no test files under %s", s.Name, dir)
	}
	batches, err := vitestBatches(s, dir, files, "")
	if err != nil {
		return Units{}, err
	}
	u, err := vitestUnitsForBatches(l, s, dir, batches)
	if err != nil || s.IntegrationConfig == "" {
		return u, err
	}
	ifiles, err := vitestFilesUnder(l.Module, dir, s.IntegrationConfig)
	if err != nil {
		return Units{}, err
	}
	if len(ifiles) == 0 {
		return Units{}, fmt.Errorf("suites: %s lists no test files under %s's %s", s.Name, dir, s.IntegrationConfig)
	}
	// The integration batches' areas carry IntegrationAreaPrefix, so no batch
	// shares a unit name with a unit-config batch of the same area.
	ibatches, err := vitestBatches(s, dir, ifiles, IntegrationAreaPrefix)
	if err != nil {
		return Units{}, err
	}
	splits, err := vitestIntegrationSplits(l, s, dir, ibatches)
	if err != nil {
		return Units{}, err
	}
	if build, ok := vitestIntegrationBuild(s, dir); ok {
		u.Atomic = append(u.Atomic, build)
		for i := range splits {
			splits[i].Deps = append(splits[i].Deps, build.ID)
		}
	}
	u.Splits = append(u.Splits, splits...)
	return u, nil
}

// vitestIntegrationBuild is the unit that builds what the suite's integration
// files spawn, when the roster names one.
func vitestIntegrationBuild(s roster.Suite, dir string) (run.Spec, bool) {
	if len(s.IntegrationBuild) == 0 {
		return run.Spec{}, false
	}
	return spec(s.Name+":integration-build", s.Name, dir, s.IntegrationBuild), true
}

// vitestIntegrationSplit is the suite's integration files, run under its
// integration config one worker at a time, exactly as the unit files are. The
// typecheck already covers the package, so it is not repeated here. A
// coverage run has each chunk write istanbul JSON of the files it loaded
// (cover.VitestIntegrationCoverage), which the baseline merges into the suite's
// unit coverage.
func vitestIntegrationSplits(l Layout, s roster.Suite, dir string, batches []FileBatch) ([]Split, error) {
	work := filepath.Join(l.Work, "vitest", s.Name+"-integration")
	if err := os.MkdirAll(filepath.Join(work, "items"), 0o755); err != nil {
		return nil, fmt.Errorf("suites: create %s: %w", work, err)
	}
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	named := func(id string, items []string, byItem map[string][]string) run.Spec {
		names := FlatNames(items, byItem)
		tag := unitTag(id)
		itemsOut := filepath.Join(work, "items", tag+".json")
		argv := []string{
			"npx", "vitest", "run", "--config", s.IntegrationConfig,
			"--maxWorkers=1",
			"--reporter=default", "--reporter=" + reporter,
		}
		if l.Coverage {
			argv = append(argv,
				// --isolate as `npm run coverage` passes it: istanbul resets a
				// module's counters after each file, and un-isolated a module
				// shared across files reports nothing for the later ones
				// (webapp/vitest.config.ts's coverage note).
				"--isolate",
				"--coverage.enabled", "--coverage.provider=istanbul", "--coverage.reporter=json",
				"--coverage.reportsDirectory="+filepath.Join(cover.VitestIntegrationCoverage(l.Work, s.Name), tag),
			)
		}
		if len(names) > 0 {
			argv = append(argv, "-t", VitestNamePattern(names))
		}
		testsOut := filepath.Join(work, "items", tag+".tests.json")
		env := []string{VitestItemsEnv + "=" + itemsOut, testtimes.VitestTestsEnv + "=" + testsOut}
		if l.Coverage {
			// A chunk is confined to its own files' inputs, so it reports
			// only what it loaded; the merge adds the rest (the package's
			// vitest.config.ts coverage note).
			env = append(env, CoverageLoadedOnlyEnv+"=1")
		}
		sp := spec(id, s.Name, dir, append(argv, items...), env...)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		sp.Tests = func([]byte, bool) ([]testtimes.Test, error) { return testtimes.ParseVitest(s.Name, testsOut, dir) }
		sp.Profiler = run.NodeProfiler
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	return batchSplits(VitestIntegrationGroup(s.Name), s.Name, batches, chunk, named), nil
}

func vitestUnitsForBatches(l Layout, s roster.Suite, dir string, batches []FileBatch) (Units, error) {
	work := filepath.Join(l.Work, "vitest", s.Name)
	blobs := filepath.Join(work, "blobs")
	dirs := []string{filepath.Join(work, "items")}
	if l.Coverage {
		dirs = append(dirs, blobs)
	}
	for _, d := range dirs {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return Units{}, fmt.Errorf("suites: create %s: %w", d, err)
		}
	}
	// The typecheck is incremental, seeded from the host's newest passing
	// typecheck of the package (internal/typecheck).
	typecheck := spec(s.Name+":typecheck", s.Name, dir, []string{l.Self, "typecheck", "-name", s.Name, "--", "npm", "run", "typecheck"})
	// tsc cannot be pinned to one core: its compiler thread and node's GC and
	// worker threads measured 1.5-1.75 cores on average across four full
	// runs (webapp and shim alike), so the unit holds the two slots it uses.
	typecheck.Slots = TypecheckSlots
	// knip is the package's unused-export gate: an export, file or dependency
	// nothing uses fails the suite instead of accumulating (its knip.json).
	knip := spec(s.Name+":knip", s.Name, dir, []string{"npm", "run", "knip"})
	knip.Slots = KnipSlots
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	if len(batches) == 0 {
		return Units{}, fmt.Errorf("suites: %s has no batch of test files to run", s.Name)
	}
	allChunk := sched.ChunkID(VitestBatchGroup(s.Name, batches[0].Name), 0)
	named := func(id string, items []string, byItem map[string][]string) run.Spec {
		names := FlatNames(items, byItem)
		tag := unitTag(id)
		itemsOut := filepath.Join(work, "items", tag+".json")
		argv := []string{
			"npx", "vitest", "run",
			"--maxWorkers=1",
			"--reporter=default", "--reporter=" + reporter,
		}
		if l.Coverage {
			argv = append(argv,
				"--coverage",
				"--coverage.reportsDirectory="+filepath.Join(work, "chunk-coverage", tag),
				"--reporter=blob",
				"--outputFile.blob="+filepath.Join(blobs, tag+".json"),
			)
			// ONE chunk writes the config's reports; every other chunk writes
			// only the JSON the merge reads. Each reports only the files it
			// loaded (CoverageLoadedOnlyEnv), and the merge adds the rest.
			if id != allChunk {
				argv = append(argv, "--coverage.reporter=json")
			}
		}
		if len(names) > 0 {
			argv = append(argv, "-t", VitestNamePattern(names))
		}
		testsOut := filepath.Join(work, "items", tag+".tests.json")
		env := []string{VitestItemsEnv + "=" + itemsOut, testtimes.VitestTestsEnv + "=" + testsOut}
		if l.Coverage {
			// A chunk is confined to its own files' inputs, so it reports
			// only what it loaded; the merge adds the rest (the package's
			// vitest.config.ts coverage note).
			env = append(env, CoverageLoadedOnlyEnv+"=1")
		}
		sp := spec(id, s.Name, dir, append(argv, items...), env...)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		sp.Tests = func([]byte, bool) ([]testtimes.Test, error) { return testtimes.ParseVitest(s.Name, testsOut, dir) }
		sp.Profiler = run.NodeProfiler
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	splits := batchSplits(s.Name, s.Name, batches, chunk, named)
	atomic := []run.Spec{typecheck, knip}
	if l.Coverage {
		merge := spec(s.Name+":coverage", s.Name, dir, []string{
			"npx", "vitest", "run", "--merge-reports=" + blobs, "--coverage",
			"--coverage.reportsDirectory=" + filepath.Dir(cover.VitestCoverage(l.Work, s.Name)),
		})
		for _, sp := range splits {
			merge.Deps = append(merge.Deps, sp.Group)
		}
		atomic = append(atomic, merge)
	}
	return Units{Atomic: atomic, Splits: splits}, nil
}

// IntegrationAreaPrefix opens the area of every integration-config batch.
const IntegrationAreaPrefix = "integration-"

// vitestBatches is a vitest suite's files cut into batches by their areas
// (VitestArea, each opened by prefix), each file its roster never reuses in a
// batch of its own.
func vitestBatches(s roster.Suite, dir string, files []string, prefix string) ([]FileBatch, error) {
	areas := make(map[string]string, len(files))
	for _, f := range files {
		a, err := VitestArea(dir, f)
		if err != nil {
			return nil, err
		}
		areas[f] = prefix + a
	}
	return BatchFiles(files, func(f string) string { return areas[f] }, neverReused(s), prefix+PooledArea, s.Batching()), nil
}

// neverReused answers whether the roster names a file as a unit it never
// reuses, which therefore runs in a batch of its own.
func neverReused(s roster.Suite) func(string) bool {
	return func(f string) bool {
		if s.Reuse == nil {
			return false
		}
		_, never := s.Reuse.Never[f]
		return never
	}
}

// unitTag is a unit ID as one file name: its slashes spelled "%", so two
// files of one name in different directories never share a tag.
func unitTag(id string) string { return strings.ReplaceAll(id, "/", "%") }

// VitestBatchGroup is the split group of a vitest suite's unit-config batch.
func VitestBatchGroup(suite, batch string) string { return suite + "/" + batch }

// VitestIntegrationGroup is the group prefix of a vitest suite's integration
// files: "<suite>[integration]/<file>".
func VitestIntegrationGroup(suite string) string { return suite + "[integration]" }

// VitestBatchOfGroup is the batch a vitest suite's split group runs, and the
// config it runs under (relative to the package).
func VitestBatchOfGroup(s roster.Suite, group string) (batch, config string, err error) {
	if b, ok := strings.CutPrefix(group, VitestIntegrationGroup(s.Name)+"/"); ok && b != "" && s.IntegrationConfig != "" {
		return b, s.IntegrationConfig, nil
	}
	if b, ok := strings.CutPrefix(group, s.Name+"/"); ok && b != "" {
		return b, VitestConfig, nil
	}
	return "", "", fmt.Errorf("suites: %q is no batch group of %s", group, s.Name)
}

// VitestConfig is the config `vitest run` reads when given none.
const VitestConfig = "vitest.config.ts"

// VitestNamePattern is the `-t` value that runs exactly the tests whose full
// names (describe titles and the test's own, joined by single spaces) start
// with one of names: each name is matched literally, anchored at the start, and
// ends at a word boundary so "a test" does not also run "a tests".
func VitestNamePattern(names []string) string {
	return "^" + gotest.LiteralAlternation(names) + "( |$)"
}

// VitestItemsEnv names the file testrun/vitest/items-reporter.mjs writes.
const VitestItemsEnv = "AGENT_REPL_TESTRUN_ITEMS"

// CoverageLoadedOnlyEnv tells a vitest suite's config that its chunk reports
// coverage of only the files it loaded: a coverage chunk is confined to its
// own files' inputs, so its config leaves `coverage.include` unset, and the
// suite's one coverage merge applies it.
const CoverageLoadedOnlyEnv = "AGENT_REPL_COVERAGE_LOADED_ONLY"

// ParseVitestItems reads one chunk's items-reporter output: each test file's
// whole cost to its chunk, keyed by its path relative to the package.
func ParseVitestItems(path, dir string, want []string) (map[string]float64, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read the chunk's per-file costs: %w", err)
	}
	var costs map[string]float64
	if err := json.Unmarshal(data, &costs); err != nil {
		return nil, fmt.Errorf("%s is not an items-reporter file: %w", path, err)
	}
	got := map[string]float64{}
	for file, secs := range costs {
		rel, err := under(dir, file)
		if err != nil {
			return nil, err
		}
		if secs < 0 {
			return nil, fmt.Errorf("%s has a negative cost %v", rel, secs)
		}
		got[rel] = secs
	}
	return got, matchItems(got, want)
}
