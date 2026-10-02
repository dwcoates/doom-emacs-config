package suites

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"

	"agentrepl/testrun/internal/command"
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
	return vitestUnitsForFiles(l, s, dir, files)
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
	reporter := filepath.Join(l.Module, "testrun", "vitest", "items-reporter.mjs")
	chunk := func(id string, items []string) run.Spec {
		tag := filepath.Base(id)
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
			if id != sched.ChunkID(s.Name, 0) {
				argv = append(argv, "--coverage.all=false", "--coverage.reporter=json")
			}
		}
		sp := spec(id, s.Name, dir, append(argv, items...), VitestItemsEnv+"="+itemsOut)
		sp.Items = func([]byte) (map[string]float64, error) { return ParseVitestItems(itemsOut, dir, items) }
		return sp
	}
	atomic := []run.Spec{typecheck}
	if l.Coverage {
		merge := spec(s.Name+":coverage", s.Name, dir, []string{
			"npx", "vitest", "run", "--merge-reports=" + blobs, "--coverage",
			"--coverage.reportsDirectory=" + filepath.Join(work, "coverage"),
		})
		merge.Deps = []string{s.Name}
		atomic = append(atomic, merge)
	}
	return Units{
		Atomic: atomic,
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
