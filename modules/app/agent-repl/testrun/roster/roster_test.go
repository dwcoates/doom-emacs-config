package roster

import (
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/testrun/internal/cores"
	"agentrepl/testrun/internal/testdir"
)

func TestNamesAreUniqueAndInRosterOrder(t *testing.T) {
	// Act
	names := Names()

	// Assert
	seen := map[string]bool{}
	for i, n := range names {
		if seen[n] {
			t.Fatalf("suite %q is listed twice", n)
		}
		seen[n] = true
		if Suites[i].Name != n {
			t.Fatalf("Names()[%d] = %q, want roster order", i, n)
		}
	}
}

func TestLookup(t *testing.T) {
	tests := []struct {
		name   string
		wantOK bool
	}{
		{"ert", true},
		{"e2e", true},
		{"nope", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			s, ok := Lookup(tt.name)

			// Assert
			if ok != tt.wantOK || (ok && s.Name != tt.name) {
				t.Fatalf("Lookup(%q) = %+v, %v", tt.name, s, ok)
			}
		})
	}
}

func TestEveryHarnessTestsAScriptInTheModulesBin(t *testing.T) {
	// Arrange / Act
	names := Harnesses()

	// Assert
	if len(names) == 0 {
		t.Fatal("no suite is marked as a bin/ harness")
	}
	for _, n := range names {
		s, _ := Lookup(n)
		isHarnessKind := s.Kind == Script || s.Kind == SplitScript
		// A harness runs a bin/test- script, or is a lint gate in bin/ that
		// reads bin/'s scripts (shellcheck): either way a change under bin/
		// is in its blast radius.
		testsBin := strings.HasPrefix(s.Path, "bin/test-")
		lintsBin := strings.HasPrefix(s.Path, "bin/") && s.Reuse != nil && slices.Contains(s.Reuse.Inputs, "bin/")
		if !isHarnessKind || !(testsBin || lintsBin) {
			t.Errorf("%s is marked a bin/ harness but runs %q", n, s.Path)
		}
	}
}

func TestOnlyScriptSuitesHaveAWidth(t *testing.T) {
	// Every other kind is split into one-core units; a width there would be
	// silently ignored by its unit builder.
	for _, s := range Suites {
		if s.Slots != 0 && s.Kind != Script {
			t.Errorf("suite %s has width %d but is not a Script suite", s.Name, s.Slots)
		}
	}
}

func TestTestParallelNamesOnlyGoSuitesAtMoreThanOne(t *testing.T) {
	for _, s := range Suites {
		for pkg, par := range s.TestParallel {
			// Assert
			if s.Kind != GoModule && s.Kind != E2E {
				t.Errorf("%s names package %q in TestParallel, but only Go suites run -test.parallel", s.Name, pkg)
			}
			if par.Tests < 2 {
				t.Errorf("%s TestParallel[%q].Tests = %d; a package overlaps at least two tests or is not named", s.Name, pkg, par.Tests)
			}
			if par.Slots < 1 {
				t.Errorf("%s TestParallel[%q].Slots = %d; a chunk holds at least one core", s.Name, pkg, par.Slots)
			}
		}
	}
}

func TestTheWaitBoundPackagesOverlapTheirTests(t *testing.T) {
	want := map[string]string{"daemon": "integration", "sidecar": "integration", "store": "integration", "e2e": "."}
	for _, s := range Suites {
		pkg, named := want[s.Name]
		if !named {
			continue
		}
		// Assert
		if s.TestParallel[pkg].Tests < 2 {
			t.Errorf("%s TestParallel[%q].Tests = %d, want its wait-bound tests overlapped", s.Name, pkg, s.TestParallel[pkg].Tests)
		}
	}
}

// The widths the chunks hold, as measured (Suite.TestParallel's comment):
// the host's core budget counts them, so a chunk narrower than what it uses
// spends cores the budget never gave it.
func TestTheOverlappedPackagesHoldTheirMeasuredWidths(t *testing.T) {
	want := map[string]map[string]int{
		"daemon":  {"integration": 2},
		"sidecar": {"integration": 2},
		"store":   {"integration": 1},
		"e2e":     {".": 2},
	}
	for _, s := range Suites {
		for pkg, slots := range want[s.Name] {
			// Assert
			if got := s.TestParallel[pkg].Slots; got != slots {
				t.Errorf("%s TestParallel[%q].Slots = %d, want %d", s.Name, pkg, got, slots)
			}
		}
	}
}

func TestResolve(t *testing.T) {
	tests := []struct {
		name, path, want string
	}{
		{"a module-relative path", "daemon/go.mod", "/r/modules/app/agent-repl/daemon/go.mod"},
		{"a repository-relative path", "/.githooks/pre-commit", "/r/.githooks/pre-commit"},
		{"a subtree keeps no trailing slash", "prompts/", "/r/modules/app/agent-repl/prompts"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Resolve("/r", "/r/modules/app/agent-repl", tt.path)

			// Assert
			if got != tt.want {
				t.Fatalf("Resolve(%q) = %q, want %q", tt.path, got, tt.want)
			}
		})
	}
}

// TestRosterPathsResolveOnlyThroughResolve fails a runner file that reads the
// leading-"/" convention itself: every reading goes through Resolve.
func TestRosterPathsResolveOnlyThroughResolve(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	err := filepath.WalkDir("..", func(p string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(p, ".go") || strings.HasSuffix(p, "_test.go") || filepath.Base(p) == "roster.go" {
			return nil
		}
		body, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), `HasPrefix(p, "/")`) || strings.Contains(string(body), `HasPrefix(s.Path, "/")`) {
			offenders = append(offenders, p)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files read the roster's path convention themselves; call roster.Resolve: %v", offenders)
	}
}

func TestCoverageSystemsAreGoOrVitestSuites(t *testing.T) {
	for _, name := range CoverageSystems {
		s, ok := Lookup(name)
		if !ok {
			t.Errorf("coverage system %q is no roster suite", name)
			continue
		}
		if s.Kind != GoModule && s.Kind != Vitest {
			t.Errorf("coverage system %q is a %v suite; only Go modules and vitest packages have statement coverage", name, s.Kind)
		}
	}
}

func TestAProgramBuiltFromAnIntegrationDirectoryIsAFake(t *testing.T) {
	// The fakes live beside the integration suites that run them; a system
	// under test never does. Either way round, a mismatch would instrument a
	// fake into a system's coverage or leave a system uninstrumented.
	for _, p := range Programs {
		underIntegration := strings.Contains(p.Package, "/integration/")
		if underIntegration != p.Fake {
			t.Errorf("program %s (package %s): Fake = %t, but it is built from an integration directory: %t", p.Name, p.Package, p.Fake, underIntegration)
		}
	}
}

func TestEverySuiteThatReadsTheScenarioGoldensDeclaresThem(t *testing.T) {
	// A keyed suite runs confined to its declared inputs, and its reuse key is
	// taken over them: a suite reading the goldens undeclared is refused the
	// read, and a golden change would not rerun it.
	for _, name := range []string{"daemon", "sidecar"} {
		t.Run(name, func(t *testing.T) {
			// Arrange
			s, ok := Lookup(name)

			// Assert
			if !ok || s.Reuse == nil {
				t.Fatalf("%s declares no Reuse", name)
			}
			declared := slices.Contains(s.Reuse.Inputs, "testdata/scenario-goldens/")
			for _, paths := range s.Reuse.Units {
				declared = declared || slices.Contains(paths, "testdata/scenario-goldens/")
			}
			if !declared {
				t.Fatalf("%s's inputs = %+v, want testdata/scenario-goldens/ among them, for the suite or one of its units", name, s.Reuse)
			}
		})
	}
}

// TestNoSuiteHashesTheBuiltWebappDistItDoesNotRead pins that a suite whose
// keyed tree covers webapp/ but which reads no built dist declares the dist
// generated: a dist rebuilt after keying (2026-10-16, by the run itself) made
// a hashed dist refuse the suite's green record.
func TestNoSuiteHashesTheBuiltWebappDistItDoesNotRead(t *testing.T) {
	for _, s := range Suites {
		if s.Reuse == nil {
			continue
		}
		covers := s.Path == "webapp"
		for _, in := range s.Reuse.Inputs {
			covers = covers || in == "webapp/"
		}
		if !covers {
			continue
		}
		t.Run(s.Name, func(t *testing.T) {
			// Act
			generated := false
			for _, g := range s.Reuse.Generated {
				generated = generated || g == "webapp/dist/"
			}

			// Assert
			if !generated {
				t.Fatalf("%s hashes webapp/ but does not declare webapp/dist/ generated", s.Name)
			}
		})
	}
}

// TestNoUnitIsWiderThanTheGovernorsFirstUnit keeps every unit startable: for
// a waiter whose width it cannot read from the ledger (a run of an earlier
// testrun), the CPU governor lets the widest unit's seats go
// (cores.FirstUnitSeats), so a wider unit would wait there for good, and
// every unit behind it with it.
func TestNoUnitIsWiderThanTheGovernorsFirstUnit(t *testing.T) {
	for _, s := range Suites {
		t.Run(s.Name, func(t *testing.T) {
			// Assert
			if s.Slots > cores.FirstUnitSeats {
				t.Fatalf("suite %s holds %d slots, more than the governor's first unit (%d)", s.Name, s.Slots, cores.FirstUnitSeats)
			}
			for pkg, p := range s.TestParallel {
				if p.Slots > cores.FirstUnitSeats {
					t.Fatalf("suite %s package %s holds %d slots a chunk, more than the governor's first unit (%d)", s.Name, pkg, p.Slots, cores.FirstUnitSeats)
				}
			}
		})
	}
}

func TestCheck(t *testing.T) {
	tests := []struct {
		name  string
		suite Suite
		want  string
	}{
		{"a suite declaring its inputs", Suite{Name: "s", Reuse: &Reuse{}}, ""},
		{"a suite never reused with a reason", Suite{Name: "s", NeverReused: "it measures"}, ""},
		{"a suite declaring nothing", Suite{Name: "s"}, "s declares no Reuse and no NeverReused reason"},
		{"a blank reason", Suite{Name: "s", NeverReused: "  "}, "s declares no Reuse and no NeverReused reason"},
		{"both", Suite{Name: "s", Reuse: &Reuse{}, NeverReused: "x"}, "s declares both Reuse and NeverReused"},
		{"a never-reused unit without a reason", Suite{Name: "s", Reuse: &Reuse{Never: map[string]string{"u": ""}}}, `never reused without a reason`},
		{"a unit both declared and never reused", Suite{Name: "s", Reuse: &Reuse{Units: map[string][]string{"u": {"x/"}}, Never: map[string]string{"u": "why"}}},
			`both declares its unit "u"'s inputs and names it never reused`},
		{"a vitest suite's own batching", Suite{Name: "s", Kind: Vitest, Reuse: &Reuse{}, Batch: &Batching{Max: 24, Min: 6}}, ""},
		{"an ERT suite's own batching", Suite{Name: "s", Kind: ERT, Reuse: &Reuse{}, Batch: &Batching{Max: 1, Min: 1}}, ""},
		{"a batching on a suite that is not batched", Suite{Name: "s", Kind: GoModule, Reuse: &Reuse{}, Batch: &Batching{Max: 24, Min: 6}},
			"s declares a Batch, but only a vitest or ERT suite's files are batched"},
		{"a batching whose max is below one", Suite{Name: "s", Kind: Vitest, Reuse: &Reuse{}, Batch: &Batching{Max: 0, Min: 1}},
			"s declares Batch max 0 min 1"},
		{"a batching whose min is below one", Suite{Name: "s", Kind: Vitest, Reuse: &Reuse{}, Batch: &Batching{Max: 4, Min: 0}},
			"s declares Batch max 4 min 0"},
		{"a batching whose min is above its max", Suite{Name: "s", Kind: ERT, Reuse: &Reuse{}, Batch: &Batching{Max: 4, Min: 5}},
			"min no more than max"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := Check([]Suite{tt.suite})

			// Assert
			if tt.want == "" {
				if err != nil {
					t.Fatalf("err = %v, want none", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestSuiteBatching(t *testing.T) {
	tests := []struct {
		name  string
		suite Suite
		want  Batching
		label string
	}{
		{"a suite declaring none takes the default", Suite{Kind: Vitest}, DefaultBatching, "max48-min12"},
		{"a suite declaring its own takes it", Suite{Kind: ERT, Batch: &Batching{Max: 20, Min: 5}}, Batching{Max: 20, Min: 5}, "max20-min5"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.suite.Batching()

			// Assert
			if got != tt.want || got.Label() != tt.label {
				t.Fatalf("Batching() = %+v labeled %q, want %+v labeled %q", got, got.Label(), tt.want, tt.label)
			}
		})
	}
}

// TestEverySuiteDeclaresItsInputs is the roster's own standing check: the
// runner refuses a roster Check refuses, so this fails first.
func TestEverySuiteDeclaresItsInputs(t *testing.T) {
	// Act
	err := Check(Suites)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
}

// TestTheWifiHelperIsReadByNoSuite pins the Wi-Fi helper app's placement: no
// suite builds or runs it, so its sources sit in ReadByNoSuite and nowhere
// else. Unplaced, they failed every run's placement check (2026-11-14).
func TestTheWifiHelperIsReadByTheShellcheckGateAndItsOwnHarness(t *testing.T) {
	// Assert: shellcheck lints its build.sh and wifi-helper-harness tests its
	// standing.swift; no suite builds or runs the app.
	if slices.Contains(ReadByNoSuite, "wifi-helper/") {
		t.Fatalf("ReadByNoSuite = %v, but the shellcheck gate reads wifi-helper/", ReadByNoSuite)
	}
	var readers []string
	for _, s := range Suites {
		if s.Reuse != nil && slices.Contains(s.Reuse.Inputs, "wifi-helper/") {
			readers = append(readers, s.Name)
		}
	}
	if !slices.Equal(readers, []string{"wifi-helper-harness", "shellcheck"}) {
		t.Fatalf("suites reading wifi-helper/ = %v, want wifi-helper-harness and shellcheck", readers)
	}
}

// TestTheERTSuitesFakeDaemonModuleIsASuite pins that the fake daemon the ert
// suite builds from lisp/testsupport/fakedaemon has its Go tests run by a
// suite of their own: the ert suite only builds the binary.
func TestTheERTSuitesFakeDaemonModuleIsASuite(t *testing.T) {
	// Arrange
	ert, ok := Lookup("ert")
	if !ok {
		t.Fatal("the roster holds no ert suite")
	}

	// Act
	var found []Suite
	for _, s := range Suites {
		if s.Path == ert.Path+"/testsupport/fakedaemon" {
			found = append(found, s)
		}
	}

	// Assert
	if len(found) != 1 || found[0].Kind != GoModule {
		t.Fatalf("suites at %s/testsupport/fakedaemon = %+v, want one Go module suite", ert.Path, found)
	}
}

// TestMetapromptFilesAreEveryMetapromptOnDisk keeps metapromptFiles the
// module's metaprompt files: one added beside the others and left out would
// be unreadable to the shim's suite and missing from every run's regions.
func TestMetapromptFilesAreEveryMetapromptOnDisk(t *testing.T) {
	// Arrange
	paths, err := filepath.Glob(filepath.Join("..", "..", "metaprompt*.md"))
	if err != nil || len(paths) == 0 {
		t.Fatalf("glob the module's metaprompts: %v (%d found)", err, len(paths))
	}
	var onDisk []string
	for _, p := range paths {
		onDisk = append(onDisk, filepath.Base(p))
	}

	// Act
	listed := slices.Sorted(slices.Values(metapromptFiles))

	// Assert
	slices.Sort(onDisk)
	if !slices.Equal(listed, onDisk) {
		t.Fatalf("metapromptFiles = %v, want the metaprompts on disk %v", listed, onDisk)
	}
}

// TestEveryMetapromptIsAShimInputAndAnAlwaysRunRegion pins that both sites
// take the one list, so neither can drift from it.
func TestEveryMetapromptIsAShimInputAndAnAlwaysRunRegion(t *testing.T) {
	// Arrange
	var shim []string
	for _, s := range Suites {
		if s.Name == "shim" {
			shim = s.Reuse.Inputs
		}
	}

	// Act and Assert
	for _, f := range metapromptFiles {
		if !slices.Contains(shim, f) {
			t.Errorf("%s is not an input of the shim's suite", f)
		}
		if !slices.Contains(AlwaysRunRegions, f) {
			t.Errorf("%s is not an always-run region", f)
		}
	}
}

func TestCheckRefusesTwoSuitesWritingOneBuildDirectory(t *testing.T) {
	tests := []struct {
		name   string
		suites []Suite
		want   string
	}{
		{"two suites in directories of their own",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"a/.build/"}}, {Name: "b", NeverReused: "x", Builds: []string{"b/.build/"}}}, ""},
		{"one suite declaring a directory twice",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"a/.build/", "a/.build/"}}}, ""},
		{"directories sharing only a name prefix",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"a/.build/"}}, {Name: "b", NeverReused: "x", Builds: []string{"a/.build-swift/"}}}, ""},
		{"two suites in one directory",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"launcher/.build/"}}, {Name: "b", NeverReused: "x", Builds: []string{"launcher/.build/"}}},
			"a and b both write the build directory launcher/.build/"},
		{"a directory inside another suite's",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"launcher/.build/"}}, {Name: "b", NeverReused: "x", Builds: []string{"launcher/.build/release/"}}},
			"a and b both write the build directory launcher/.build/release/ (launcher/.build/)"},
		{"a directory holding another suite's",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"launcher/.build/release/"}}, {Name: "b", NeverReused: "x", Builds: []string{"launcher/"}}},
			"a and b both write the build directory launcher/ (launcher/.build/release/)"},
		{"a build directory not ending in a slash",
			[]Suite{{Name: "a", NeverReused: "x", Builds: []string{"launcher/.build"}}},
			`a declares the build directory "launcher/.build", which does not end in "/"`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := Check(tt.suites)

			// Assert
			if tt.want == "" {
				if err != nil {
					t.Fatalf("err = %v, want none", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

// TestTheProtoSuiteBuildsItsSwiftPluginOutsideTheLaunchersBuildDirectory pins
// the two Swift builds apart: proto's check-generated built protoc-gen-swift
// in launcher/.build while the launcher suite built there, and SwiftPM failed
// it on the locked build.db (2026-11-24). The Makefile's scratch path must be
// the build directory the proto suite declares, which Check keeps apart from
// the launcher suite's.
func TestTheProtoSuiteBuildsItsSwiftPluginOutsideTheLaunchersBuildDirectory(t *testing.T) {
	// Arrange
	module := testdir.ModuleRoot(t)
	proto, okProto := Lookup("proto")
	launcher, okLauncher := Lookup("launcher")
	if !okProto || !okLauncher {
		t.Fatal("the roster holds no proto or no launcher suite")
	}
	makefile, err := os.ReadFile(filepath.Join(module, "proto", "Makefile"))
	if err != nil {
		t.Fatal(err)
	}

	// Act
	var scratch string
	var recipe bool
	for _, line := range strings.Split(string(makefile), "\n") {
		if v, ok := strings.CutPrefix(line, "SWIFT_BUILD := "); ok {
			scratch = "proto/" + strings.TrimSpace(v) + "/"
		}
		if strings.HasPrefix(line, "\tswift build ") {
			recipe = strings.Contains(line, "--scratch-path $(SWIFT_BUILD) ") && strings.Contains(line, "--product protoc-gen-swift")
		}
	}

	// Assert
	if !recipe {
		t.Fatal("proto/Makefile builds protoc-gen-swift without --scratch-path $(SWIFT_BUILD)")
	}
	if !slices.Equal(proto.Builds, []string{scratch}) {
		t.Fatalf("proto suite Builds = %v, want the Makefile's scratch path %s", proto.Builds, scratch)
	}
	if !slices.Equal(launcher.Builds, []string{"launcher/.build/"}) {
		t.Fatalf("launcher suite Builds = %v, want SwiftPM's default launcher/.build/", launcher.Builds)
	}
	if err := Check([]Suite{proto, launcher}); err != nil {
		t.Fatal(err)
	}
}
