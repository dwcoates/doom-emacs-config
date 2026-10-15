package roster

import (
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
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
		{"e2e-emacs", true},
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

func TestOnlyTheSandboxSuiteMayDecline(t *testing.T) {
	// The exit-77 disposition exists for a precondition the run cannot meet
	// (the e2e sandbox image); any other suite exiting 77 is a failure.
	for _, s := range Suites {
		if s.MayDecline != (s.Name == "e2e-emacs") {
			t.Errorf("%s MayDecline = %v", s.Name, s.MayDecline)
		}
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
		if !isHarnessKind || len(s.Path) < len("bin/test-") || s.Path[:len("bin/test-")] != "bin/test-" {
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

func TestTheSandboxSuiteHoldsTheCoresItsContainerUses(t *testing.T) {
	// Arrange
	s, ok := Lookup("e2e-emacs")

	// Assert
	if !ok || s.Slots != 2 {
		t.Fatalf("e2e-emacs = %+v, want a width of 2: the CPUs its two Emacsen were re-measured on (EMACS-LAYER-SPEC.md)", s)
	}
}

func TestTestParallelNamesOnlyGoSuitesAtMoreThanOne(t *testing.T) {
	for _, s := range Suites {
		for pkg, n := range s.TestParallel {
			// Assert
			if s.Kind != GoModule && s.Kind != E2E {
				t.Errorf("%s names package %q in TestParallel, but only Go suites run -test.parallel", s.Name, pkg)
			}
			if n < 2 {
				t.Errorf("%s TestParallel[%q] = %d; a package overlaps at least two tests or is not named", s.Name, pkg, n)
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
		if s.TestParallel[pkg] < 2 {
			t.Errorf("%s TestParallel[%q] = %d, want its wait-bound tests overlapped", s.Name, pkg, s.TestParallel[pkg])
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

func TestOnlyTheSandboxSuiteReadsTheWebappDist(t *testing.T) {
	// The sandbox suite stages webapp/dist into its container; no other
	// suite reads it, and only a Script suite is handed the env that stops
	// it building the dist itself (suites.NoWebappBuildEnv).
	for _, s := range Suites {
		if s.NeedsWebappDist != (s.Name == "e2e-emacs") {
			t.Errorf("%s NeedsWebappDist = %v", s.Name, s.NeedsWebappDist)
		}
		if s.NeedsWebappDist && s.Kind != Script {
			t.Errorf("%s needs the webapp dist but is not a Script suite", s.Name)
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
			if !ok || s.Reuse == nil || !slices.Contains(s.Reuse.Inputs, "testdata/scenario-goldens/") {
				t.Fatalf("%s's inputs = %+v, want testdata/scenario-goldens/ among them", name, s.Reuse)
			}
		})
	}
}
