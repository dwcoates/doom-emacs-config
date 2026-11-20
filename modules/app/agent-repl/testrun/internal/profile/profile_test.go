package profile

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/testenv"
)

func unit(id string, p run.Profiler, env ...string) run.Spec {
	return run.Spec{Unit: sched.Unit{ID: id}, Argv: []string{"prog", "-x"}, Env: env, Profiler: p}
}

func TestDir(t *testing.T) {
	tests := []struct {
		name, log, want, wantErr string
	}{
		{name: "beside the log, its extension replaced", log: "/logs/run-1.log", want: "/logs/run-1.profiles"},
		{name: "a log with no extension gets the suffix", log: "/logs/run-1", want: "/logs/run-1.profiles"},
		{name: "a run with no log cannot profile", log: "", wantErr: "has none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := Dir(tt.log)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("Dir(%q) = %q, %v; want %q", tt.log, got, err, tt.want)
			}
		})
	}
}

func TestApply(t *testing.T) {
	noEnv := func(string) string { return "" }
	tests := []struct {
		name     string
		spec     run.Spec
		getenv   func(string) string
		wantArgv []string
		wantEnv  []string
	}{
		{
			name:     "a Go test binary writes its profile into its unit's directory",
			spec:     unit("d:internal/x#0", run.GoTestProfiler, "A=1"),
			getenv:   noEnv,
			wantArgv: []string{"prog", "-x", "-test.cpuprofile=@D@/d_internal_x_0/" + GoProfile},
			wantEnv:  []string{"A=1", testenv.ProfileDir + "=@D@/d_internal_x_0"},
		},
		{
			name:     "a node unit turns on node's profiler",
			spec:     unit("w/feed#1", run.NodeProfiler),
			getenv:   noEnv,
			wantArgv: []string{"prog", "-x"},
			wantEnv:  []string{testenv.ProfileDir + "=@D@/w_feed_1", `NODE_OPTIONS=--cpu-prof --cpu-prof-dir="@D@/w_feed_1"`},
		},
		{
			name:     "a node unit keeps the runner's NODE_OPTIONS",
			spec:     unit("w", run.NodeProfiler),
			getenv:   func(k string) string { return map[string]string{"NODE_OPTIONS": "--max-old-space-size=99"}[k] },
			wantArgv: []string{"prog", "-x"},
			wantEnv:  []string{testenv.ProfileDir + "=@D@/w", `NODE_OPTIONS=--max-old-space-size=99 --cpu-prof --cpu-prof-dir="@D@/w"`},
		},
		{
			name:     "a node unit's own NODE_OPTIONS wins over the runner's",
			spec:     unit("w", run.NodeProfiler, "NODE_OPTIONS=--own"),
			getenv:   func(k string) string { return map[string]string{"NODE_OPTIONS": "--runner"}[k] },
			wantArgv: []string{"prog", "-x"},
			wantEnv:  []string{"NODE_OPTIONS=--own", testenv.ProfileDir + "=@D@/w", `NODE_OPTIONS=--own --cpu-prof --cpu-prof-dir="@D@/w"`},
		},
		{
			name:     "any other unit is only told its directory",
			spec:     unit("ert:x", run.NoProfiler),
			getenv:   noEnv,
			wantArgv: []string{"prog", "-x"},
			wantEnv:  []string{testenv.ProfileDir + "=@D@/ert_x"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			dir := t.TempDir()
			sub := func(xs []string) []string {
				out := make([]string, len(xs))
				for i, x := range xs {
					out[i] = strings.ReplaceAll(x, "@D@", dir)
				}
				return out
			}
			before := slices.Clone(tt.spec.Argv)

			// Act.
			got, err := Apply([]run.Spec{tt.spec}, dir, tt.getenv)

			// Assert.
			if err != nil {
				t.Fatal(err)
			}
			if !slices.Equal(got[0].Argv, sub(tt.wantArgv)) {
				t.Errorf("argv = %q, want %q", got[0].Argv, sub(tt.wantArgv))
			}
			if !slices.Equal(got[0].Env, sub(tt.wantEnv)) {
				t.Errorf("env = %q, want %q", got[0].Env, sub(tt.wantEnv))
			}
			if !slices.Equal(tt.spec.Argv, before) {
				t.Errorf("the input spec's argv changed to %q", tt.spec.Argv)
			}
			unitDir := filepath.Join(dir, testenv.ProfileName(tt.spec.ID))
			if info, err := os.Stat(unitDir); err != nil || !info.IsDir() {
				t.Errorf("the unit's directory %s was not created: %v", unitDir, err)
			}
		})
	}
}

func TestApplyFailsWhenAUnitsDirectoryCannotBeMade(t *testing.T) {
	// Arrange.
	blocked := filepath.Join(t.TempDir(), "a-file")
	if err := os.WriteFile(blocked, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act.
	got, err := Apply([]run.Spec{unit("u", run.NoProfiler)}, blocked, func(string) string { return "" })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "create the profiles directory of unit u") {
		t.Fatalf("err = %v, want the unit's directory named", err)
	}
	if got != nil {
		t.Fatalf("specs = %v, want none on failure", got)
	}
}

func TestApplyRefusesADirectoryNodeOptionsCannotCarry(t *testing.T) {
	// Act.
	_, err := Apply([]run.Spec{unit("u", run.NodeProfiler)}, filepath.Join(t.TempDir(), `a"b`), func(string) string { return "" })

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "double quote") {
		t.Fatalf("err = %v, want the double quote refused", err)
	}
}

func TestApplyPanicsOnAnUndefinedProfiler(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if r := recover(); r == nil || !strings.Contains(r.(string), "does not define") {
			t.Fatalf("recovered %v, want the undefined Profiler named", r)
		}
	}()

	// Act.
	_, _ = Apply([]run.Spec{unit("u", run.Profiler(99))}, t.TempDir(), func(string) string { return "" })
}
