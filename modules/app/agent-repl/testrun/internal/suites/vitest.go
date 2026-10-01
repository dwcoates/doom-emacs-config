package suites

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/roster"
)

// vitestFiles lists a package's unit test files under its default config, as
// paths relative to the package, after making its npm deps satisfy the
// lockfile through the one sanctioned path (bin/ensure-deps.sh).
func vitestFiles(module, dir string) ([]string, error) {
	ensure := exec.Command(filepath.Join(module, "bin", "ensure-deps.sh"), dir)
	ensure.Env = os.Environ()
	if out, err := ensure.CombinedOutput(); err != nil {
		return nil, fmt.Errorf("suites: ensure the npm deps of %s: %w\n%s", dir, err, out)
	}
	list := exec.Command("npx", "vitest", "list", "--filesOnly", "--json")
	list.Dir = dir
	list.Env = os.Environ()
	out, err := list.Output()
	if err != nil {
		var stderr string
		if ee, ok := err.(*exec.ExitError); ok {
			stderr = string(ee.Stderr)
		}
		return nil, fmt.Errorf("suites: vitest list in %s: %w\n%s", dir, err, stderr)
	}
	var entries []struct {
		File string `json:"file"`
	}
	if err := json.Unmarshal(out, &entries); err != nil {
		return nil, fmt.Errorf("suites: vitest list in %s printed no file list: %w", dir, err)
	}
	var files []string
	for _, e := range entries {
		rel, err := filepath.Rel(dir, e.File)
		if err != nil {
			return nil, fmt.Errorf("suites: %s is not under %s: %w", e.File, dir, err)
		}
		files = append(files, rel)
	}
	sort.Strings(files)
	return files, nil
}

// vitestUnits is the typecheck, the test files split into chunks (each
// leaving a blob report with its coverage), and the merge of those blobs into
// the package's one coverage report, exactly what `npm run coverage` produced
// when it ran whole.
func vitestUnits(l Layout, s roster.Suite) (Units, error) {
	dir := l.resolve(s.Path)
	files, err := vitestFiles(l.Module, dir)
	if err != nil {
		return Units{}, err
	}
	if len(files) == 0 {
		return Units{}, fmt.Errorf("suites: %s lists no test files under %s", s.Name, dir)
	}
	work := filepath.Join(l.Work, "vitest", s.Name)
	blobs := filepath.Join(work, "blobs")
	for _, d := range []string{blobs, filepath.Join(work, "items")} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return Units{}, fmt.Errorf("suites: create %s: %w", d, err)
		}
	}
	typecheck := spec(s.Name+":typecheck", s.Name, dir, []string{"npm", "run", "typecheck"})
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	chunk := func(id string, items []string) run.Spec {
		tag := filepath.Base(id)
		itemsOut := filepath.Join(work, "items", tag+".json")
		argv := []string{
			"npm", "run", "coverage", "--",
			"--maxWorkers=1", "--minWorkers=1",
			"--coverage.reportsDirectory=" + filepath.Join(work, "chunk-coverage", tag),
			"--reporter=default", "--reporter=blob", "--reporter=" + reporter,
			"--outputFile.blob=" + filepath.Join(blobs, tag+".json"),
		}
		// ONE chunk carries the config's `all: true` pass, which adds every
		// source file no test loaded (at zero) and is a fixed cost per process;
		// the merge sums the counts, so the merged report is the one a whole
		// run makes (measured: same files, same statement counts). The rest skip
		// that pass and write only the raw json the blob carries.
		if id != sched.ChunkID(s.Name, 0) {
			argv = append(argv, "--coverage.all=false", "--coverage.reporter=json")
		}
		sp := spec(id, s.Name, dir, append(argv, items...), VitestItemsEnv+"="+itemsOut)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		return sp
	}
	merge := spec(s.Name+":coverage", s.Name, dir, []string{
		"npx", "vitest", "run", "--merge-reports=" + blobs, "--coverage",
		"--coverage.reportsDirectory=" + filepath.Join(work, "coverage"),
	})
	merge.Deps = []string{s.Name}
	return Units{
		Atomic: []run.Spec{typecheck, merge},
		Splits: []Split{{Group: s.Name, Suite: s.Name, Items: files, Chunk: chunk}},
	}, nil
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
		rel, err := filepath.Rel(dir, file)
		if err != nil {
			return nil, fmt.Errorf("%s is not under %s: %w", file, dir, err)
		}
		if secs < 0 {
			return nil, fmt.Errorf("%s has a negative cost %v", rel, secs)
		}
		got[rel] = secs
	}
	return got, matchItems(got, want)
}
