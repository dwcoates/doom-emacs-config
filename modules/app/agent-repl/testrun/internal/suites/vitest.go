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

// vitestUnits is the typecheck and the test files split into chunks. Coverage
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
	u, err := vitestUnitsForFiles(l, s, dir, files)
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
	splits, err := vitestIntegrationSplits(l, s, dir, ifiles)
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
func vitestIntegrationSplits(l Layout, s roster.Suite, dir string, files []string) ([]Split, error) {
	work := filepath.Join(l.Work, "vitest", s.Name+"-integration")
	if err := os.MkdirAll(filepath.Join(work, "items"), 0o755); err != nil {
		return nil, fmt.Errorf("suites: create %s: %w", work, err)
	}
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	named := func(id string, items []string, names []string) run.Spec {
		tag := unitTag(id)
		itemsOut := filepath.Join(work, "items", tag+".json")
		argv := []string{
			"npx", "vitest", "run", "--config", s.IntegrationConfig,
			"--maxWorkers=1", "--minWorkers=1",
			"--reporter=default", "--reporter=" + reporter,
		}
		if l.Coverage {
			argv = append(argv,
				// --isolate as `npm run coverage` passes it: istanbul resets a
				// module's counters after each file, and un-isolated a module
				// shared across files reports nothing for the later ones
				// (webapp/vitest.config.ts's coverage note).
				"--isolate",
				"--coverage.enabled", "--coverage.provider=istanbul", "--coverage.reporter=json", "--coverage.all=false",
				"--coverage.reportsDirectory="+filepath.Join(cover.VitestIntegrationCoverage(l.Work, s.Name), tag),
			)
		}
		if len(names) > 0 {
			argv = append(argv, "-t", VitestNamePattern(names))
		}
		testsOut := filepath.Join(work, "items", tag+".tests.json")
		sp := spec(id, s.Name, dir, append(argv, items...), VitestItemsEnv+"="+itemsOut, testtimes.VitestTestsEnv+"="+testsOut)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		sp.Tests = func([]byte, bool) ([]testtimes.Test, error) { return testtimes.ParseVitest(s.Name, testsOut, dir) }
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	return fileSplits(VitestIntegrationGroup(s.Name), s.Name, files, chunk, named), nil
}

func vitestUnitsForFiles(l Layout, s roster.Suite, dir string, files []string) (Units, error) {
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
	typecheck := spec(s.Name+":typecheck", s.Name, dir, []string{"npm", "run", "typecheck"})
	// tsc cannot be pinned to one core: its compiler thread and node's GC and
	// worker threads measured 1.5-1.75 cores on average across four full
	// runs (webapp and shim alike), so the unit holds the two slots it uses.
	typecheck.Slots = TypecheckSlots
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	allChunk := sched.ChunkID(VitestFileGroup(s.Name, files[0]), 0)
	named := func(id string, items []string, names []string) run.Spec {
		tag := unitTag(id)
		itemsOut := filepath.Join(work, "items", tag+".json")
		argv := []string{
			"npx", "vitest", "run",
			"--maxWorkers=1", "--minWorkers=1",
			"--reporter=default", "--reporter=" + reporter,
		}
		if l.Coverage {
			argv = append(argv,
				"--coverage",
				"--coverage.reportsDirectory="+filepath.Join(work, "chunk-coverage", tag),
				"--reporter=blob",
				"--outputFile.blob="+filepath.Join(blobs, tag+".json"),
			)
			// ONE chunk carries the config's `all: true` pass. The merge sums
			// the counts, so every other chunk writes only its loaded files.
			if id != allChunk {
				argv = append(argv, "--coverage.all=false", "--coverage.reporter=json")
			}
		}
		if len(names) > 0 {
			argv = append(argv, "-t", VitestNamePattern(names))
		}
		testsOut := filepath.Join(work, "items", tag+".tests.json")
		sp := spec(id, s.Name, dir, append(argv, items...), VitestItemsEnv+"="+itemsOut, testtimes.VitestTestsEnv+"="+testsOut)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		sp.Tests = func([]byte, bool) ([]testtimes.Test, error) { return testtimes.ParseVitest(s.Name, testsOut, dir) }
		return sp
	}
	chunk := func(id string, items []string) run.Spec { return named(id, items, nil) }
	splits := fileSplits(s.Name, s.Name, files, chunk, named)
	atomic := []run.Spec{typecheck}
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

// ONE PROCESS PER TEST FILE. Each vitest and ERT test file is its own split,
// so its own process: the file is the unit its reuse records
// (agentrepl/testrun/internal/reuse), and a unit's profile confines exactly
// its own inputs only when no other file shares its process.

// fileSplits is one split per file under prefix ("<prefix>/<file>"), every
// one built by chunk and named.
func fileSplits(prefix, suite string, files []string, chunk func(id string, items []string) run.Spec, named func(id string, items, names []string) run.Spec) []Split {
	out := make([]Split, 0, len(files))
	for _, f := range files {
		out = append(out, Split{Group: prefix + "/" + f, Suite: suite, Items: []string{f}, Chunk: chunk, Named: named})
	}
	return out
}

// unitTag is a unit ID as one file name: its slashes spelled "%", so two
// files of one name in different directories never share a tag.
func unitTag(id string) string { return strings.ReplaceAll(id, "/", "%") }

// VitestFileGroup is the split group of a vitest suite's unit-config file.
func VitestFileGroup(suite, file string) string { return suite + "/" + file }

// VitestIntegrationGroup is the group prefix of a vitest suite's integration
// files: "<suite>[integration]/<file>".
func VitestIntegrationGroup(suite string) string { return suite + "[integration]" }

// VitestFileOfGroup is the file a vitest suite's split group runs, and the
// config it runs under (relative to the package).
func VitestFileOfGroup(s roster.Suite, group string) (file, config string, err error) {
	if f, ok := strings.CutPrefix(group, VitestIntegrationGroup(s.Name)+"/"); ok && f != "" && s.IntegrationConfig != "" {
		return f, s.IntegrationConfig, nil
	}
	if f, ok := strings.CutPrefix(group, s.Name+"/"); ok && f != "" {
		return f, VitestConfig, nil
	}
	return "", "", fmt.Errorf("suites: %q is no file group of %s", group, s.Name)
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
