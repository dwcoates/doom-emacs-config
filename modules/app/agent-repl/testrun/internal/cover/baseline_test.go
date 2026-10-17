package cover

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"agentrepl/testrun/internal/redundancy"
)

// counterTree makes a cover root whose two Go counter directories sit at
// different depths, as a unit test's and a spawned system's do.
func counterTree(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	for _, d := range []string{"daemon/000", "spawned/daemon-002/agent-repld"} {
		dir := filepath.Join(root, d)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, "covmeta.abc"), nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

// profileTool answers covdata textfmt by writing profile to its -o path.
func profileTool(profile string, calls *[][]string) Runner {
	return func(dir string, args ...string) ([]byte, error) {
		*calls = append(*calls, args)
		out := strings.TrimPrefix(args[len(args)-1], "-o=")
		return nil, os.WriteFile(out, []byte(profile), 0o644)
	}
}

const mergedProfile = `mode: atomic
agent-repld/internal/a/a.go:1.1,2.1 6 1
agent-repld/internal/a/a.go:3.1,4.1 4 0
agentrepl/logging/log.go:1.1,2.1 5 0
agentrepl/e2e/harness/h.go:1.1,2.1 9 3
`

func TestBaselineMergesEveryTierPerSystem(t *testing.T) {
	// Arrange
	root := counterTree(t)
	var calls [][]string
	istanbul := filepath.Join(t.TempDir(), "coverage-final.json")
	if err := os.WriteFile(istanbul, []byte(`{"/w/src/a.ts":{"s":{"0":1,"1":0,"2":4,"3":2}}}`), 0o644); err != nil {
		t.Fatal(err)
	}
	in := BaselineInput{
		CoverRoot: root, Commit: "abc123", Now: time.Date(2026, 10, 15, 1, 2, 3, 0, time.UTC),
		GoSystems: map[string]string{"agent-repld": "daemon", "agentrepl/logging": "logging"},
		Vitest:    map[string]VitestReports{"webapp": {Unit: istanbul}},
	}

	// Act
	got, err := Baseline(profileTool(mergedProfile, &calls), in)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	wantIn := "-i=" + filepath.Join(root, "daemon/000") + "," + filepath.Join(root, "spawned/daemon-002/agent-repld")
	if len(calls) != 1 || calls[0][3] != wantIn {
		t.Fatalf("covdata calls = %v, want one over both directories", calls)
	}
	d := got.Systems["agent-repld"]
	if d.Percent != 60 || d.Statements != 10 || d.Floor != 55 || !strings.Contains(d.Tiers, "spawned") {
		t.Errorf("daemon = %+v, want 60%% of 10, floor 55", d)
	}
	if l := got.Systems["agentrepl/logging"]; l.Percent != 0 || l.Statements != 5 || l.Floor != 0 {
		t.Errorf("logging = %+v, want 0%% of 5, floor 0", l)
	}
	if w := got.Systems["webapp"]; w.Percent != 75 || w.Statements != 4 || w.Floor != 70 || w.Tiers != "vitest: unit, merged by statement" || w.Provisional {
		t.Errorf("webapp = %+v, want 75%% of 4", w)
	}
	if _, ok := got.Systems["agentrepl/e2e"]; ok || len(got.Systems) != 3 {
		t.Errorf("systems = %v: the e2e harness is no system", got.Systems)
	}
	if got.Commit != "abc123" || got.Measured != "2026-10-15T01:02:03Z" {
		t.Errorf("commit %q measured %q", got.Commit, got.Measured)
	}
}

func TestBaselineRefuses(t *testing.T) {
	tests := []struct {
		name    string
		root    func(t *testing.T) string
		tool    func(*[][]string) Runner
		systems map[string]string
		vitest  map[string]VitestReports
		want    string
	}{
		{
			name: "a run that wrote no counters",
			root: func(t *testing.T) string { return t.TempDir() },
			tool: func(c *[][]string) Runner { return profileTool(mergedProfile, c) },
			want: "holds no coverage counters",
		},
		{
			name: "a merge that fails",
			root: counterTree,
			tool: func(*[][]string) Runner {
				return func(string, ...string) ([]byte, error) { return nil, errors.New("covdata: bad meta") }
			},
			want: "merge the run's counters: covdata: bad meta",
		},
		{
			name:    "a system no counter covers",
			root:    counterTree,
			tool:    func(c *[][]string) Runner { return profileTool(mergedProfile, c) },
			systems: map[string]string{"agentrepl/shim-store": "store"},
			want:    "no counter of the run covers agentrepl/shim-store (suite store)",
		},
		{
			name:   "a vitest report that is missing",
			root:   counterTree,
			tool:   func(c *[][]string) Runner { return profileTool(mergedProfile, c) },
			vitest: map[string]VitestReports{"webapp": {Unit: "/gone/coverage-final.json"}},
			want:   "cover: webapp: read /gone/coverage-final.json",
		},
		{
			name: "a merged profile without its mode line",
			root: counterTree,
			tool: func(c *[][]string) Runner { return profileTool("agent-repld/a.go:1.1,2.1 1 1\n", c) },
			want: "the merged profile",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var calls [][]string
			systems := tt.systems
			if systems == nil {
				systems = map[string]string{"agent-repld": "daemon"}
			}
			in := BaselineInput{CoverRoot: tt.root(t), GoSystems: systems, Vitest: tt.vitest, Commit: "c"}

			// Act
			_, err := Baseline(tt.tool(&calls), in)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestIstanbulStatementsRefusesAnEmptyReport(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "coverage-final.json")
	if err := os.WriteFile(path, []byte(`{}`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := istanbulStatements(VitestReports{Unit: path})

	// Assert
	if err == nil || !strings.Contains(err.Error(), "holds no statements") {
		t.Fatalf("err = %v", err)
	}
}

func TestParseBaselineArgs(t *testing.T) {
	tests := []struct {
		name    string
		argv    []string
		wantErr bool
	}{
		{name: "every flag", argv: []string{"-module", "/m", "-work", "/w", "-commit", "c", "-out", "/o"}},
		{name: "no commit", argv: []string{"-module", "/m", "-work", "/w", "-out", "/o"}, wantErr: true},
		{name: "a stray argument", argv: []string{"-module", "/m", "-work", "/w", "-commit", "c", "-out", "/o", "x"}, wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			a, err := ParseBaselineArgs(tt.argv)

			// Assert
			if (err != nil) != tt.wantErr {
				t.Fatalf("err = %v, wantErr %t", err, tt.wantErr)
			}
			if !tt.wantErr && (a.Module != "/m" || a.Work != "/w" || a.Commit != "c" || a.Out != "/o") {
				t.Fatalf("args = %+v", a)
			}
		})
	}
}

func TestRunBaselineWritesTheMergedFile(t *testing.T) {
	// Arrange: a run's work directory with one counter directory and the
	// webapp's merged istanbul JSON where its merge unit leaves it.
	work := t.TempDir()
	dir := filepath.Join(work, "cover", "daemon", "000")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(dir, "covmeta.x"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	ist := VitestCoverage(work, "webapp")
	if err := os.MkdirAll(filepath.Dir(ist), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(ist, []byte(`{"a.ts":{"s":{"0":1}}}`), 0o644); err != nil {
		t.Fatal(err)
	}
	out := filepath.Join(t.TempDir(), "coverage-baseline.json")
	var calls [][]string
	a := BaselineArgs{Module: "/m", Work: work, Commit: "c", Out: out}

	// Act
	_, err := RunBaseline(profileTool(mergedProfile, &calls), a, map[string]string{"agent-repld": "daemon"}, []string{"webapp"}, time.Now())

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	got, err := redundancy.ReadBaseline(out)
	if err != nil {
		t.Fatal(err)
	}
	if len(got.Systems) != 2 || got.Systems["webapp"].Percent != 100 || got.Commit != "c" {
		t.Fatalf("baseline = %+v", got)
	}
	// Neither the integration tier nor the e2e layer wrote a report.
	if w := got.Systems["webapp"]; !w.Provisional || !strings.Contains(w.Reason, "no integration coverage") || !strings.Contains(w.Reason, "no e2e webapp layer") {
		t.Fatalf("webapp = %+v, want provisional for both missing tiers", w)
	}
}

// statementsJSON is one file's istanbul entry: three statements, the hits
// given.
func statementsJSON(file string, hits ...int) string {
	return fmt.Sprintf(`%q: {"s": {"0": %d, "1": %d, "2": %d}, "statementMap": {
	  "0": {"start": {"line": 1, "column": 0}, "end": {"line": 1, "column": 9}},
	  "1": {"start": {"line": 2, "column": 0}, "end": {"line": 2, "column": 9}},
	  "2": {"start": {"line": 3, "column": 0}, "end": {"line": 3, "column": 9}}}}`, file, hits[0], hits[1], hits[2])
}

func writeReport(t *testing.T, path string, entries ...string) string {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("{"+strings.Join(entries, ",")+"}"), 0o644); err != nil {
		t.Fatal(err)
	}
	return path
}

func TestIstanbulStatementsCountsAStatementAnyTierCovers(t *testing.T) {
	// Arrange: the unit tier covers statement 2, integration statement 0,
	// the layer statement 1; the layer also loaded a file outside the
	// unit config's include.
	dir := t.TempDir()
	r := VitestReports{
		Unit: writeReport(t, filepath.Join(dir, "unit.json"), statementsJSON("/w/src/a.ts", 0, 0, 1)),
		Tiers: map[string][]string{
			"integration":      {writeReport(t, filepath.Join(dir, "i.json"), statementsJSON("/w/src/a.ts", 2, 0, 0))},
			"e2e webapp layer": {writeReport(t, filepath.Join(dir, "l.json"), statementsJSON("/w/src/a.ts", 0, 5, 0), statementsJSON("/w/test/helper.ts", 1, 1, 1))},
		},
	}

	// Act
	got, err := istanbulStatements(r)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got.Percent != 100 || got.Statements != 3 || got.Tiers != "vitest: unit, e2e webapp layer, integration, merged by statement" || got.Provisional {
		t.Fatalf("baseline = %+v, want 100%% of the unit report's 3 statements", got)
	}
}

func TestIstanbulStatementsRefusesATierThatInstrumentedAFileDifferently(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	other := strings.Replace(statementsJSON("/w/src/a.ts", 1, 0, 0), `"line": 3, "column": 0`, `"line": 4, "column": 0`, 1)
	r := VitestReports{
		Unit:  writeReport(t, filepath.Join(dir, "unit.json"), statementsJSON("/w/src/a.ts", 0, 0, 1)),
		Tiers: map[string][]string{"integration": {writeReport(t, filepath.Join(dir, "i.json"), other)}},
	}

	// Act
	_, err := istanbulStatements(r)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the tiers instrumented it differently") {
		t.Fatalf("err = %v, want the mismatch refused", err)
	}
}

func TestIstanbulStatementsMarksAMissingTierProvisional(t *testing.T) {
	// Arrange
	r := VitestReports{Unit: writeReport(t, filepath.Join(t.TempDir(), "unit.json"), statementsJSON("/w/src/a.ts", 1, 0, 0)), Missing: "no layer"}

	// Act
	got, err := istanbulStatements(r)

	// Assert
	if err != nil || !got.Provisional || got.Reason != "no layer" {
		t.Fatalf("baseline = %+v, %v, want provisional for the missing tier", got, err)
	}
}

func TestVitestReportsFindsEveryTiersReports(t *testing.T) {
	tests := []struct {
		name        string
		system      string
		integration bool
		layer       bool
		missing     []string
	}{
		{name: "the webapp, every tier measured", system: "webapp", integration: true, layer: true},
		{name: "the webapp with no e2e layer", system: "webapp", integration: true, missing: []string{"no e2e webapp layer"}},
		{name: "the webapp with no integration", system: "webapp", layer: true, missing: []string{"no integration coverage"}},
		{name: "the shim, whose e2e reach is unmerged", system: "shim", integration: true, missing: []string{"the shim's e2e reach"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			work := t.TempDir()
			cover := filepath.Join(work, "cover")
			if err := os.MkdirAll(cover, 0o755); err != nil {
				t.Fatal(err)
			}
			var want []string
			if tt.integration {
				want = append(want, writeReport(t, filepath.Join(VitestIntegrationCoverage(work, tt.system), "c#00", "coverage-final.json")))
			}
			if tt.layer {
				want = append(want, writeReport(t, filepath.Join(cover, "spawned", "e2e-001", WebappLayerDir, "driver-1", "run-1-a.layer.test.ts.json")))
				writeReport(t, filepath.Join(cover, "spawned", "e2e-001", WebappLayerDir, "driver-1", "coverage-final.json"))
			}

			// Act
			r, err := vitestReports(work, cover, tt.system)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			var got []string
			for _, ps := range r.Tiers {
				got = append(got, ps...)
			}
			sort.Strings(got)
			sort.Strings(want)
			if !reflect.DeepEqual(got, want) || r.Unit != VitestCoverage(work, tt.system) {
				t.Fatalf("reports = %v (unit %s), want %v", got, r.Unit, want)
			}
			if (r.Missing == "") != (len(tt.missing) == 0) {
				t.Fatalf("missing = %q, want %v", r.Missing, tt.missing)
			}
			for _, m := range tt.missing {
				if !strings.Contains(r.Missing, m) {
					t.Errorf("missing = %q, want %q named", r.Missing, m)
				}
			}
		})
	}
}

// The e2e suite writes the webapp layer's reports where the baseline looks
// for them: the two modules cannot share a constant, so this reads the e2e
// side's source for the same names.
func TestTheE2ESuiteWritesTheWebappLayersReportsWhereTheBaselineReads(t *testing.T) {
	b, err := os.ReadFile(filepath.Join("..", "..", "..", "e2e", "coverage_test.go"))
	if err != nil {
		t.Fatal(err)
	}
	src := string(b)
	for _, want := range []string{`filepath.Join(root, "` + WebappLayerDir + `")`, `os.MkdirTemp(parent, "driver-")`} {
		if !strings.Contains(src, want) {
			t.Errorf("e2e/coverage_test.go no longer writes %s", want)
		}
	}
}
