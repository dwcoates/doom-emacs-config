package cover

import (
	"errors"
	"os"
	"path/filepath"
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
		Vitest:    map[string]string{"webapp": istanbul},
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
	if w := got.Systems["webapp"]; w.Percent != 75 || w.Statements != 4 || w.Floor != 70 {
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
		vitest  map[string]string
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
			vitest: map[string]string{"webapp": "/gone/coverage-final.json"},
			want:   "read /gone/coverage-final.json",
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
	_, err := istanbulStatements(path)

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
}
