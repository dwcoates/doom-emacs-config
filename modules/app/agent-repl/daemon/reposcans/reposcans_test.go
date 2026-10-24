package reposcans

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/merge"
	"agentrepl/testrun/roster"
)

// moduleRoot is where the agent-repl module sits in the repository: a changed
// path, as the merge gate's selection reads it, is spelled from the
// repository root.
const moduleRoot = "modules/app/agent-repl/"

// moduleRootDir is the agent-repl module root on disk, two directories up:
// the tree every scan here reads.
func moduleRootDir(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}
	return root
}

// TestEveryGoSuiteIsSelectedByEveryLocalModuleItReplaces derives each Go
// suite's local dependencies from its own go.mod (`replace ... => ../...`) and
// checks a change in any of them selects the suite, so a new cross-module
// import can never leave a merge's gate blind to it.
func TestEveryGoSuiteIsSelectedByEveryLocalModuleItReplaces(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root := moduleRootDir(t)
	replace := regexp.MustCompile(`=>\s*(\.\.?/\S*)`)
	checked := 0
	for _, suite := range roster.Suites {
		if suite.Kind != roster.GoModule && suite.Kind != roster.E2E {
			continue
		}
		body, err := os.ReadFile(filepath.Join(root, suite.Path, "go.mod"))
		if err != nil {
			t.Fatalf("read %s's go.mod: %v", suite.Name, err)
		}
		for _, m := range replace.FindAllStringSubmatch(string(body), -1) {
			dep, err := filepath.Rel(root, filepath.Join(root, suite.Path, m[1]))
			if err != nil || strings.HasPrefix(dep, "..") {
				t.Fatalf("%s replaces %s outside the module: %v", suite.Name, m[1], err)
			}
			path := moduleRoot + filepath.ToSlash(dep) + "/x.go"
			checked++

			// Act.
			got := merge.SelectSuites([]string{path})

			// Assert.
			if !got.Full && !slices.Contains(got.Suites, suite.Name) {
				t.Errorf("a change at %s (a module %s replaces) selects %v, which leaves out %s", path, suite.Name, got.Suites, suite.Name)
			}
		}
	}
	if checked == 0 {
		t.Fatal("no go.mod replaced any local module; the walk read nothing")
	}
}

// TestEveryVitestSuiteIsSelectedByEveryTreeItImports walks each Vitest
// suite's sources and checks that every relative import reaching OUTSIDE the
// suite's own directory (the generated TS bindings, the shared logging
// module) selects the suite.
func TestEveryVitestSuiteIsSelectedByEveryTreeItImports(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root := moduleRootDir(t)
	importRe := regexp.MustCompile(`(?:from|import)\s*\(?\s*['"](\.\.?/[^'"]+)['"]`)
	checked := map[string]bool{}
	for _, suite := range roster.Suites {
		if suite.Kind != roster.Vitest {
			continue
		}
		suiteDir := filepath.Join(root, suite.Path)
		for _, sub := range []string{"src", "test"} {
			err := filepath.WalkDir(filepath.Join(suiteDir, sub), func(path string, d os.DirEntry, err error) error {
				if err != nil {
					return err
				}
				if d.IsDir() && d.Name() == "node_modules" {
					return filepath.SkipDir
				}
				if d.IsDir() || !strings.HasSuffix(path, ".ts") {
					return nil
				}
				body, err := os.ReadFile(path)
				if err != nil {
					return err
				}
				for _, m := range importRe.FindAllStringSubmatch(string(body), -1) {
					target := filepath.Join(filepath.Dir(path), m[1])
					if rel, _ := filepath.Rel(suiteDir, target); !strings.HasPrefix(rel, "..") {
						continue
					}
					dep, err := filepath.Rel(root, target)
					if err != nil || strings.HasPrefix(dep, "..") {
						return fmt.Errorf("%s imports %s outside the module", path, m[1])
					}
					changed := moduleRoot + filepath.ToSlash(dep) + ".ts"
					if checked[suite.Name+" "+changed] {
						continue
					}
					checked[suite.Name+" "+changed] = true

					// Act.
					got := merge.SelectSuites([]string{changed})

					// Assert.
					if !got.Full && !slices.Contains(got.Suites, suite.Name) {
						t.Errorf("a change at %s (imported by %s) selects %v, which leaves out %s", changed, suite.Name, got.Suites, suite.Name)
					}
				}
				return nil
			})
			if err != nil {
				t.Fatalf("walk %s/%s: %v", suite.Name, sub, err)
			}
		}
	}
	if len(checked) == 0 {
		t.Fatal("no Vitest suite imports anything outside itself; the walk read nothing")
	}
}

// TestNoSystemsIntegrationSuiteRunsAnotherSystem pins the rule the selection
// rests on (AGENTS.md, "A system's integration tests run that system alone"):
// no system's integration sources build, locate or start another system's
// real binary. Each entry names what running that other system would require
// a source to say; a line that is only a comment is not read.
func TestNoSystemsIntegrationSuiteRunsAnotherSystem(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root := moduleRootDir(t)
	tests := []struct {
		dir string
		// skip is a subdirectory that is not this system's own suite: the
		// daemon's integration harness is also the end-to-end suite's, and
		// stages real services for it.
		skip      string
		forbidden []*regexp.Regexp
	}{
		{dir: "daemon/integration", skip: "harness", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`ServiceBinaries\s*:`), regexp.MustCompile(`shim-store"`), regexp.MustCompile(`shim-sidecar"`)}},
		{dir: "agent-shim/claude/shim-sidecar/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`"shim-store"`), regexp.MustCompile(`"agent-repld"`), regexp.MustCompile(`storeBinary`)}},
		{dir: "agent-shim/shim-store/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`"shim-claude-sidecar"\)`), regexp.MustCompile(`shim-sidecar"`), regexp.MustCompile(`"agent-repld"`), regexp.MustCompile(`dist/main\.js`)}},
		{dir: "agent-shim/claude/shim/test/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`shim-store`), regexp.MustCompile(`shim-sidecar`), regexp.MustCompile(`agent-repld"`)}},
		{dir: "webapp/test/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`daemon/bin`), regexp.MustCompile(`cmd/agent-repld`), regexp.MustCompile(`shim-store`)}},
	}
	for _, tt := range tests {
		t.Run(tt.dir, func(t *testing.T) {
			// Act.
			err := filepath.WalkDir(filepath.Join(root, tt.dir), func(path string, d os.DirEntry, err error) error {
				if err != nil {
					return err
				}
				if d.IsDir() && tt.skip != "" && d.Name() == tt.skip {
					return filepath.SkipDir
				}
				if d.IsDir() || !(strings.HasSuffix(path, ".go") || strings.HasSuffix(path, ".ts")) {
					return nil
				}
				body, err := os.ReadFile(path)
				if err != nil {
					return err
				}
				for i, line := range strings.Split(string(body), "\n") {
					trimmed := strings.TrimSpace(line)
					if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "/*") {
						continue
					}
					// Assert.
					for _, re := range tt.forbidden {
						if re.MatchString(line) {
							t.Errorf("%s:%d runs another system for real (%s): %s", path, i+1, re, trimmed)
						}
					}
				}
				return nil
			})
			if err != nil {
				t.Fatalf("walk %s: %v", tt.dir, err)
			}
		})
	}
}

// TestRosterwalkIsTheOnlyChildWalk pins that no Go reader anywhere in
// agent-repl walks RosterRow.children by hand: a hand-rolled walk is exactly
// how one reader came to miss child workspaces while the others saw them.
func TestRosterwalkIsTheOnlyChildWalk(t *testing.T) {
	// Arrange: the agent-repl root, two directories up.
	root := moduleRootDir(t)
	if _, err := os.Stat(filepath.Join(root, "daemon", "go.mod")); err != nil {
		t.Fatalf("%s is not the agent-repl root: %v", root, err)
	}
	generated := filepath.Join(root, "proto", "gen")
	e2e := filepath.Join(root, "e2e") + string(filepath.Separator)
	// rosterwalk itself walks the children, and this suite spells the call
	// it looks for.
	self := filepath.Join(root, "proto", "helpers", "go", "rosterwalk")
	scans, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	var offenders []string

	// Act.
	err = filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if path == generated || path == self || path == scans || d.Name() == "node_modules" || d.Name() == ".git" {
				return filepath.SkipDir
			}
			return nil
		}
		// The e2e suite reads live rosters, so its tests are readers too; every
		// other _test.go builds rows rather than walking them.
		if !strings.HasSuffix(path, ".go") || (strings.HasSuffix(path, "_test.go") && !strings.HasPrefix(path, e2e)) {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "GetChildren()") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk agent-repl: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files walk RosterRow.children by hand; flatten through rosterwalk instead: %v", offenders)
	}
}

// TestTheStoreAndEveryFakeValidateThroughThisPackage pins the call sites: the
// store's server and every fake store a producer's suite runs against call
// ValidateWriteBatchRequest, and no other Go file restates its rules.
func TestTheStoreAndEveryFakeValidateThroughThisPackage(t *testing.T) {
	// Arrange: the agent-repl root, two directories up.
	root := moduleRootDir(t)
	callers := []string{
		"agent-shim/shim-store/internal/server/validate.go",
		"agent-shim/claude/shim-sidecar/integration/helpers_test.go",
	}
	// storewrite itself states the rule, and this suite spells it.
	self := filepath.Join(root, "proto", "helpers", "go", "storewrite")
	scans, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("resolve this package: %v", err)
	}
	rule := "carries neither entries nor a cursor advance nor an agent locator"

	// Act, Assert: each caller goes through this package.
	for _, rel := range callers {
		body, err := os.ReadFile(filepath.Join(root, rel))
		if err != nil {
			t.Fatalf("read %s: %v", rel, err)
		}
		if !strings.Contains(string(body), "storewrite.Validate") {
			t.Errorf("%s does not validate writes through storewrite", rel)
		}
	}
	// Assert: no other file restates the empty-batch rule.
	err = filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "node_modules" || d.Name() == ".git" || path == self || path == scans) {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), rule) {
			t.Errorf("%s restates the store's write validation; call storewrite instead", path)
		}
		return nil
	})
	if err != nil {
		t.Fatalf("walk agent-repl: %v", err)
	}
}

// TestTestsNameArmsOnlyThroughOneofarm pins the call sites: a test under e2e/
// or daemon/integration/ that reads WhichOneof by hand is a drifted copy of
// Name.
func TestTestsNameArmsOnlyThroughOneofarm(t *testing.T) {
	// Arrange: the agent-repl root, two directories up.
	root := moduleRootDir(t)
	var offenders []string

	// Act.
	for _, dir := range []string{"e2e", filepath.Join("daemon", "integration")} {
		err := filepath.WalkDir(filepath.Join(root, dir), func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(path, "_test.go") {
				return nil
			}
			body, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			if strings.Contains(string(body), "WhichOneof(") {
				offenders = append(offenders, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these tests read WhichOneof by hand; read the arm through oneofarm.Field or oneofarm.Name instead: %v", offenders)
	}
}

// TestNoSuiteSpellsTheServingPredicateItself pins that the integration and
// e2e suites wait on a serving session through SessionServes rather than a
// copy of its predicate.
func TestNoSuiteSpellsTheServingPredicateItself(t *testing.T) {
	// Arrange
	// The negated form (a shim DETACHED with the composer open) is a different
	// predicate and is not a copy.
	copy := regexp.MustCompile(`(^|[^!])live\.GetShimAttached\(\) && live\.GetOpen\(\) != nil`)
	var copies []string

	// Act
	// The daemon's integration tree and the e2e suite's, from the module root.
	for _, root := range []string{filepath.Join("..", "integration"), filepath.Join("..", "..", "e2e")} {
		err := filepath.WalkDir(root, func(path string, entry os.DirEntry, walkErr error) error {
			if walkErr != nil {
				return walkErr
			}
			if entry.IsDir() || !strings.HasSuffix(path, ".go") || filepath.Base(path) == "watch.go" || filepath.Base(path) == "sessionserves_test.go" {
				return nil
			}
			src, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			if copy.Match(src) {
				copies = append(copies, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walk %s: %v", root, err)
		}
	}

	// Assert
	if len(copies) != 0 {
		t.Fatalf("files spell the serving predicate themselves; use harness.SessionServes:\n%s", strings.Join(copies, "\n"))
	}
}

// TestSingletonServicesBindThroughTheSharedHelper pins the extraction of the
// socket binding (agent-shim/logging/go/singletonsocket): no launchd service
// (the store, the sidecar, zettel) binds a unix socket by hand, so the
// occupied/stale/non-socket/owner-only rules can never drift between them.
func TestSingletonServicesBindThroughTheSharedHelper(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	handRolled := regexp.MustCompile(`net\.Listen\("unix"`)
	scanned := 0
	for _, dir := range []string{"agent-shim/shim-store", "agent-shim/claude/shim-sidecar", "zettel"} {
		err := filepath.WalkDir(filepath.Join(root, dir), func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			scanned++

			// Act
			body, err := os.ReadFile(path)
			if err != nil {
				return err
			}

			// Assert
			if handRolled.Match(body) {
				t.Errorf("%s binds a unix socket by hand; bind it through singletonsocket.Listen or ListenReclaiming", path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("scan %s: %v", dir, err)
		}
	}
	if scanned == 0 {
		t.Fatal("scanned no source files")
	}
}
