package request

import (
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

	testrunv1 "agentrepl/proto/testrun/v1"

	"agentrepl/testrun/internal/redundancy/gomeasure"
	"agentrepl/testrun/internal/redundancy/vitestmeasure"
)

const repo = "/r"

func measurement() *testrunv1.RedundancyMeasurement {
	return &testrunv1.RedundancyMeasurement{OutDir: "/out"}
}

func goReq(g *testrunv1.RedundancyGo) *testrunv1.RedundancyRequest {
	if g.Measurement == nil {
		g.Measurement = measurement()
	}
	return &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: g}}
}

func pkg(p string) *testrunv1.RedundancyGo_Package {
	return &testrunv1.RedundancyGo_Package{Package: &testrunv1.RedundancyGoPackage{Package: p}}
}

func TestArgv(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.RedundancyRequest
		want []string
	}{
		{name: "a package with only what is required", req: goReq(&testrunv1.RedundancyGo{ModuleDir: "m/daemon", Subject: pkg("./internal/x")}),
			want: []string{"go", "-out", "/out", "-module", "/r/m/daemon", "-pkg", "./internal/x"}},
		{name: "a package with every field", req: goReq(&testrunv1.RedundancyGo{
			Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/out", Universe: testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_STRICT,
				MaxCoverageDrop: proto.Float64(0.5), Baseline: "m/testrun/coverage-baseline.json", Pins: []string{"TestA", "TestB"},
				OverheadUs: proto.Int64(250), KeepScratch: true, MaxLoad: proto.Float64(6), QuietWaitMs: proto.Int64(1500), Procs: proto.Int32(2)},
			ModuleDir: "m/daemon",
			Subject: &testrunv1.RedundancyGo_Package{Package: &testrunv1.RedundancyGoPackage{Package: "./internal/x",
				TestFiles: []string{"a_test.go", "b_test.go"}, Tags: "integration", Programs: []string{"git", "fakeshim"}}},
			Mutate: []string{"./internal/x", "./internal/y"}, Runs: proto.Int32(5), PinCalls: &testrunv1.RedundancyGoPinCalls{Calls: []string{"FindRecord"}},
			Estimate: true, HangBoundMs: proto.Int64(11000)}),
			want: []string{"go", "-out", "/out", "-universe", "strict", "-max-coverage-drop", "0.5", "-baseline", "/r/m/testrun/coverage-baseline.json",
				"-pin", "TestA,TestB", "-overhead", "250µs", "-keep-scratch", "-max-load", "6", "-quiet-wait", "1.5s", "-procs", "2",
				"-module", "/r/m/daemon", "-pkg", "./internal/x", "-tests", "a_test.go,b_test.go", "-tags", "integration", "-programs", "git,fakeshim",
				"-mutate", "./internal/x,./internal/y", "-runs", "5", "-pin-calls", "FindRecord", "-estimate", "-hang-bound", "11s"}},
		{name: "roster suites with the suite's own module", req: goReq(&testrunv1.RedundancyGo{Subject: &testrunv1.RedundancyGo_RosterSuites{RosterSuites: &testrunv1.RedundancyGoRosterSuites{Names: []string{"daemon"}}}}),
			want: []string{"go", "-out", "/out", "-roster-suite", "daemon"}},
		{name: "an empty pin-call list turns the default off", req: goReq(&testrunv1.RedundancyGo{ModuleDir: "m", Subject: pkg("."), PinCalls: &testrunv1.RedundancyGoPinCalls{}}),
			want: []string{"go", "-out", "/out", "-module", "/r/m", "-pkg", ".", "-pin-calls", ""}},
		{name: "a vitest measurement", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Vitest{Vitest: &testrunv1.RedundancyVitest{
			Measurement: measurement(), PackageDir: "m/webapp", Mutate: []string{"src/**/*.ts", "!src/fake/**"}, System: "webapp", Config: "vitest.config.ts", Runs: proto.Int32(2)}}},
			want: []string{"vitest", "-out", "/out", "-package", "/r/m/webapp", "-mutate", "src/**/*.ts,!src/fake/**", "-system", "webapp", "-config", "vitest.config.ts", "-runs", "2"}},
		{name: "a report", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{
			MeasurementFile: "/out/measurement.json", Universe: testrunv1.RedundancyUniverse_REDUNDANCY_UNIVERSE_KILLS_ONLY, OverheadUs: proto.Int64(100),
			MaxCoverageDrop: proto.Float64(1), Baseline: "m/b.json"}}},
			want: []string{"report", "-in", "/out/measurement.json", "-universe", "kills-only", "-overhead", "100µs", "-max-coverage-drop", "1", "-baseline", "/r/m/b.json"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := Argv(tc.req, repo, t.TempDir())

			// Assert
			if err != nil || !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Argv = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestArgvWritesTheFilesItsFlagsName(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	req := goReq(&testrunv1.RedundancyGo{
		Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/out", DeleteCases: []string{"TestA", "TestB/x"}},
		ModuleDir:   "m/daemon",
		Subject: &testrunv1.RedundancyGo_Suites{Suites: &testrunv1.RedundancyGoSuiteList{Suites: []*testrunv1.RedundancyGoSuite{
			{Package: "./internal/x", Tags: "integration", TestFiles: []string{"a_test.go"}, Programs: []string{"git"}, Kept: true, Module: "e2e"}}}},
	})

	// Act
	argv, err := Argv(req, repo, dir)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	deleted, err := os.ReadFile(filepath.Join(dir, "delete-cases.txt"))
	if err != nil || string(deleted) != "TestA\nTestB/x\n" || !strings.Contains(strings.Join(argv, " "), "-delete "+filepath.Join(dir, "delete-cases.txt")) {
		t.Fatalf("deletion set %q (%v), argv %q", deleted, err, argv)
	}
	suites, err := os.ReadFile(filepath.Join(dir, "suites.json"))
	if err != nil || !strings.Contains(string(suites), `"Package":"./internal/x"`) || !strings.Contains(string(suites), `"Kept":true`) || !strings.Contains(string(suites), `"Module":"e2e"`) {
		t.Fatalf("suite list %s (%v)", suites, err)
	}
}

// THE ADAPTERS' OWN PARSERS ACCEPT WHAT ARGV LOWERS A REQUEST ONTO, so the
// request and the flags can never drift into meaning different things.
func TestTheAdaptersParseWhatArgvWrites(t *testing.T) {
	cases := []struct {
		name  string
		req   *testrunv1.RedundancyRequest
		parse func(argv []string) error
	}{
		{name: "go", req: goReq(&testrunv1.RedundancyGo{ModuleDir: "m/daemon", Subject: pkg("./internal/x"), Runs: proto.Int32(2), HangBoundMs: proto.Int64(5000),
			Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/out", OverheadUs: proto.Int64(100), QuietWaitMs: proto.Int64(10), Procs: proto.Int32(1)}}),
			parse: func(argv []string) error { _, err := gomeasure.ParseArgs(argv, io.Discard); return err }},
		{name: "vitest", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Vitest{Vitest: &testrunv1.RedundancyVitest{
			Measurement: measurement(), PackageDir: "m/webapp", Mutate: []string{"src/a.ts"}, Runs: proto.Int32(1)}}},
			parse: func(argv []string) error { _, err := vitestmeasure.ParseArgs(argv, io.Discard); return err }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a repository holding the directories the request names,
			// and the agent-repl module root the vitest parser climbs to.
			root := t.TempDir()
			for _, d := range []string{"m/daemon/internal/x", "m/webapp", "m/testrun/roster"} {
				if err := os.MkdirAll(filepath.Join(root, d), 0o755); err != nil {
					t.Fatal(err)
				}
			}
			if err := os.WriteFile(filepath.Join(root, "m/testrun/roster/roster.go"), nil, 0o644); err != nil {
				t.Fatal(err)
			}
			argv, err := Argv(tc.req, root, t.TempDir())
			if err != nil {
				t.Fatal(err)
			}

			// Act
			err = tc.parse(argv[1:])

			// Assert
			if err != nil {
				t.Fatalf("the %s adapter refused %q: %v", tc.name, argv, err)
			}
		})
	}
}

func TestArgvRefuses(t *testing.T) {
	cases := []struct {
		name string
		req  *testrunv1.RedundancyRequest
		want string
	}{
		{name: "no command", req: &testrunv1.RedundancyRequest{}, want: "names no command"},
		{name: "no measurement settings", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Go{Go: &testrunv1.RedundancyGo{Subject: pkg(".")}}}, want: "names no measurement settings"},
		{name: "a relative out_dir", req: goReq(&testrunv1.RedundancyGo{Measurement: &testrunv1.RedundancyMeasurement{OutDir: "out"}, Subject: pkg(".")}), want: "out_dir \"out\" is not an absolute path"},
		{name: "a module outside the repository", req: goReq(&testrunv1.RedundancyGo{ModuleDir: "../x", Subject: pkg(".")}), want: "module_dir \"../x\" is not a path inside the repository"},
		{name: "an absolute baseline", req: goReq(&testrunv1.RedundancyGo{Measurement: &testrunv1.RedundancyMeasurement{OutDir: "/out", Baseline: "/b.json"}, Subject: pkg(".")}), want: "baseline \"/b.json\" is not a path inside the repository"},
		{name: "a package naming no package", req: goReq(&testrunv1.RedundancyGo{Subject: pkg("")}), want: "package names no package"},
		{name: "no subject", req: goReq(&testrunv1.RedundancyGo{}), want: "names no subject"},
		{name: "roster suites naming none", req: goReq(&testrunv1.RedundancyGo{Subject: &testrunv1.RedundancyGo_RosterSuites{RosterSuites: &testrunv1.RedundancyGoRosterSuites{}}}), want: "roster_suites names no suite"},
		{name: "a suite list naming none", req: goReq(&testrunv1.RedundancyGo{Subject: &testrunv1.RedundancyGo_Suites{Suites: &testrunv1.RedundancyGoSuiteList{}}}), want: "suites names no suite"},
		{name: "a vitest measurement mutating nothing", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Vitest{Vitest: &testrunv1.RedundancyVitest{Measurement: measurement(), PackageDir: "m/webapp"}}}, want: "mutate names no source glob"},
		{name: "a report of a relative file", req: &testrunv1.RedundancyRequest{Command: &testrunv1.RedundancyRequest_Report{Report: &testrunv1.RedundancyReport{MeasurementFile: "m.json"}}}, want: "is not an absolute path"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Argv(tc.req, repo, t.TempDir())

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Argv = %v, want an error containing %q", err, tc.want)
			}
		})
	}
}
