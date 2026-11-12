package suites

import (
	"fmt"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"sort"
	"strings"

	"agentrepl/testrun/internal/run"
)

// FILES RUN IN BATCHES. A vitest or ERT suite's test files are cut into
// batches, and each batch is one split: one reuse unit, run in as few
// processes as the scheduler's chunking gives it. One process per file paid a
// process start per file (a vitest worker or an Emacs, about 3s against the
// 0.3s its tests took), which made the webapp suite about four times slower
// and ERT about 2.6 times (2026-11-12). A batch is still confined to its
// files' inputs as a whole, so a missing input fails loudly; the cost is that
// a change to one file's input reruns its batch.
//
// A BATCH HOLDS FILES THAT READ THE SAME CODE, so a change reruns only the
// batches that read it: each file belongs to an AREA (what it tests: a vitest
// file's most imported source directory, an ERT file's module family), an area
// too large is cut into BatchMax-file batches, and areas too small to be worth
// a process are pooled. A file its suite never reuses (roster Reuse.Never) is a
// batch of its own, named by the file, so it does not take its batch-mates'
// reuse with it.

// BatchMax is the most files a batch holds; BatchMin is the fewest an area
// keeps as batches of its own before it is pooled with other small areas.
// A vitest process costs about 12s beyond its tests (transform, collect,
// setup, environment; 2026-11-12), so a batch must hold enough files to repay
// it: at 24 and 4 the webapp suite ran 28 processes and its batches spent 47%
// of their time outside their tests.
const (
	BatchMax = 48
	BatchMin = 12
)

// BatchingLabel names the batch sizes in force, for the test-time series a
// run pushes (testtimes.Report.Batching), so runs under different sizes
// compare: "max48-min12".
func BatchingLabel() string { return fmt.Sprintf("max%d-min%d", BatchMax, BatchMin) }

// PooledArea names the batches small areas are pooled into, opened by the
// suite's area prefix where it has one.
const PooledArea = "pooled"

// FileBatch is one batch: its name within its suite and its files, sorted.
type FileBatch struct {
	Name  string
	Files []string
}

// BatchFiles cuts files into batches by areaOf, small areas pooled under the
// area pool, every file solo answers true for in a batch of its own. The
// answer depends only on files and their areas, so a batch keeps its name and
// members until a file is added, removed or moves area. Batches are answered
// sorted by name.
func BatchFiles(files []string, areaOf func(string) string, solo func(string) bool, pool string) []FileBatch {
	areas := map[string][]string{}
	var out []FileBatch
	for _, f := range files {
		if solo != nil && solo(f) {
			out = append(out, FileBatch{Name: f, Files: []string{f}})
			continue
		}
		a := areaOf(f)
		areas[a] = append(areas[a], f)
	}
	names := make([]string, 0, len(areas))
	for a := range areas {
		names = append(names, a)
	}
	sort.Strings(names)
	var pooled []string
	for _, a := range names {
		members := areas[a]
		sort.Strings(members)
		if len(members) < BatchMin {
			pooled = append(pooled, members...)
			continue
		}
		out = append(out, cut(a, members)...)
	}
	out = append(out, cut(pool, pooled)...)
	sort.Slice(out, func(i, j int) bool { return out[i].Name < out[j].Name })
	return out
}

// cut is members as batches of at most BatchMax files: area itself when one
// batch holds them, otherwise "area.1", "area.2", ... of near-equal size.
func cut(area string, members []string) []FileBatch {
	if len(members) == 0 {
		return nil
	}
	n := (len(members) + BatchMax - 1) / BatchMax
	if n == 1 {
		return []FileBatch{{Name: area, Files: members}}
	}
	out := make([]FileBatch, 0, n)
	for i := range n {
		lo, hi := i*len(members)/n, (i+1)*len(members)/n
		out = append(out, FileBatch{Name: fmt.Sprintf("%s.%d", area, i+1), Files: append([]string(nil), members[lo:hi]...)})
	}
	return out
}

// batchSplits is one split per batch under prefix ("<prefix>/<batch>"), every
// one built by chunk and named.
func batchSplits(prefix, suite string, batches []FileBatch, chunk func(id string, items []string) run.Spec, named func(id string, items, names []string) run.Spec) []Split {
	out := make([]Split, 0, len(batches))
	for _, b := range batches {
		out = append(out, Split{Group: prefix + "/" + b.Name, Suite: suite, Items: b.Files, Chunk: chunk, Named: named})
	}
	return out
}

// vitestImport is a relative import or re-export specifier of a TypeScript or
// JavaScript test file: `from "./x"`, `import("../x")`, `import "./x"`.
var vitestImport = regexp.MustCompile(`(?:from|import)\s*\(?\s*["'](\.{1,2}/[^"']+)["']`)

// VitestArea is the area of test file FILE (relative to package directory
// DIR): the source directory under src/ its relative imports reach most often
// ("sidebar" for src/sidebar/...), or, for a file importing nothing under
// src/, its directory under the test tree. Ties go to the alphabetically
// first. An unreadable file is the caller's failure.
func VitestArea(dir, file string) (string, error) {
	data, err := os.ReadFile(filepath.Join(dir, file))
	if err != nil {
		return "", fmt.Errorf("suites: read %s to find its area: %w", file, err)
	}
	counts := map[string]int{}
	for _, m := range vitestImport.FindAllSubmatch(data, -1) {
		target := path.Clean(path.Join(path.Dir(filepath.ToSlash(file)), string(m[1])))
		rest, ok := strings.CutPrefix(target, "src/")
		if !ok {
			continue
		}
		area := strings.TrimSuffix(path.Base(rest), path.Ext(rest))
		if top, _, nested := strings.Cut(rest, "/"); nested {
			area = top
		}
		counts[area]++
	}
	best, bestN := "", 0
	for a, n := range counts {
		if n > bestN || n == bestN && a < best {
			best, bestN = a, n
		}
	}
	if best != "" {
		return best, nil
	}
	return testTreeArea(filepath.ToSlash(file)), nil
}

// testTreeArea is a test file's directory beneath its test root ("test/feed/x.test.ts"
// is "feed"), or "test" for a file at the root.
func testTreeArea(file string) string {
	parts := strings.Split(file, "/")
	if len(parts) > 2 {
		return parts[1]
	}
	return "test"
}

// ERTArea is an ERT file's module family: its name without "test-" and ".el",
// up to its first hyphen ("test-wire-verbs.el" is "wire").
func ERTArea(file string) string {
	stem := strings.TrimSuffix(strings.TrimPrefix(path.Base(file), "test-"), ".el")
	family, _, _ := strings.Cut(stem, "-")
	return family
}
